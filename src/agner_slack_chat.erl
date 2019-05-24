-module(agner_slack_chat).

-export([start/0]).
-export([start_link/0]).
-export([connect_chat/0]).

start() ->
  spawn(?MODULE, connect_chat, []).

start_link() ->
  Pid = spawn_link(?MODULE, connect_chat, []),
  {ok, Pid}.

connect_chat() ->
  {ok, ConnPid} = connect(),
  monitor(process, ConnPid),
  upgrade(ConnPid).

connect() ->
  {ok, WssUrl} = agner_slack_rest:obtain_wss_url(),
  {_Scheme, Host, Path, _Query, _Fragment} = mochiweb_util:urlsplit(WssUrl),

  {ok, ConnPid} = gun:open(Host, 443),
  {ok, http} = gun:await_up(ConnPid),

  gun:ws_upgrade(ConnPid, Path),

  {ok, ConnPid}.

upgrade(ConnPid) ->
  receive
    {gun_upgrade, ConnPid, _StreamRef, _Protocols, _Headers} ->
      lager:info("chat upgrade success"),
      agner_player_server:chat_connected(self()),
      chat(ConnPid);
    {gun_response, ConnPid, _, _, Status, Headers} ->
      lager:info("chat gun_response"),
      exit({ws_upgrade_failed, Status, Headers});
    {gun_error, ConnPid, _StreamRef, Reason} ->
      lager:info("chat gun_error"),
      exit({ws_upgrade_failed, Reason});
    Else ->
      lager:info("upgrade_message_else ~p", [Else]),
      upgrade(ConnPid)
  after 5000 ->
    exit(slack_websocket_timeout)
  end.

chat(ConnPid) ->
  receive
    {gun_ws, ConnPid, _StreamRef, {text, Body}} ->
      case handle_frame(Body) of
        {message, Message} -> gun:ws_send(ConnPid, {text, Message});
        _ -> ok
      end,
      chat(ConnPid);
    {gun_down, Pid, _Protocol, Reason, _, _} ->
      exit({chat_connection_down, [{pid, Pid}, {reason, Reason}, {connPid, ConnPid}]});
    {shutdown, _Pid} ->
      lager:info("closing_slack_connection"),
      gun:close(ConnPid);
    {'DOWN', _Mref, process, ConnPid, Reason} ->
      lager:info("chat_down"),
      exit(Reason);
    {answer, Channel, Text} ->
      Response = jiffy:encode(#{
        <<"id">> => 1,
        <<"type">> => <<"message">>,
        <<"channel">> => Channel,
        <<"text">> => Text
      }),
      ok = gun:ws_send(ConnPid, {text, Response}),
      lager:info("Answered. MessageId: ~p Message: ~p", [Channel, Response]),
      chat(ConnPid);
    Else ->
      lager:info("chat message ~p", [Else]),
      chat(ConnPid)
  end.

handle_frame(Body) ->
  ParsedJson = jiffy:decode(Body, [return_maps]),
  handle_parsed_frame(ParsedJson).

handle_parsed_frame(#{<<"type">> := <<"message">>} = Message) ->
  handle_message(Message);
handle_parsed_frame(#{<<"type">> := Type} = _ParsedJson) ->
  {unsupported_type, Type};
handle_parsed_frame(#{<<"reply_to">> := Id} = _ParsedJson) ->
  {reply_to, Id}.

handle_message(#{<<"subtype">> := <<"message_changed">>} = Message) ->
  handle_sub_message(maps:get(<<"message">>, Message));
handle_message(#{<<"subtype">> := <<"channel_join">>} = Message) ->
  lager:info("Channel joined. Message: ~s", [Message]);
handle_message(#{<<"subtype">> := SubType} = Message) ->
  lager:info("Unsupported subtype: ~s. Message: ~s", [SubType, Message]);
handle_message(#{<<"text">> := Text} = Message) ->
  handle_text_message(Text, Message).

handle_sub_message(#{<<"attachments">> := Attachments, <<"user">> := UserId} = _SubMessage) ->
  handle_attachments(UserId, Attachments);
handle_sub_message(_SubMessage) ->
  no_attachments.

handle_attachments(UserId, [#{<<"video_html">> := _} = Attachment | Tail]) ->
  MovieId = get_movie_id(Attachment),
  Title = get_movie_title(Attachment),
  agner_player_server:add(MovieId, Title, UserId),
  handle_attachments(UserId, Tail);
handle_attachments(UserId, [_Attachment | Tail]) ->
  handle_attachments(UserId, Tail);
handle_attachments(_UserId, []) ->
  ok.

get_movie_id(#{<<"from_url">> := MovieUrl} = _Attachment) ->
  extract_movie_id(MovieUrl).

extract_movie_id(MovieUrl) when is_binary(MovieUrl) ->
  Decoded = cow_qs:urldecode(MovieUrl),
  extract_movie_id(mochiweb_util:urlsplit(erlang:binary_to_list(Decoded)));

extract_movie_id({_Scheme, "youtu.be", [_Slash | MovieId], _Query, _Fragment}) ->
  MovieId;
extract_movie_id({_Scheme, _Host, _Path, Query, _Fragment}) ->
  [MovieId] = [V || {"v", V} <- mochiweb_util:parse_qs(Query)],
  MovieId.

get_movie_title(#{<<"title">> := Title} = _Attachment) ->
  binary_to_list(Title).

handle_text_message(Text, Message) ->
  Intent = resolve_intent(Text),
  lager:info("Resolved intent: ~w", [Intent]),
  handle_intent(Intent, Message).

resolve_intent(Text) ->
  resolve_intent(Text, [
    {next, "^next$", []},
    {delete, "^delete$", []},
    {pause, "^pause", []},
    {previous, "^previous", []},
    {volume, "^volume$", []},
    {volume, "^volume (?<level>([0-9]|[1-9][0-9]|100))$", [{capture, ['level'], binary}]},
    {say, "^say (?<message>(.*))$", [{capture, ['message'], binary}]},
    {seek, "^seek (?<to>([0-9]*))$", [{capture, ['to'], binary}]},
    {now, "^now$", []}
  ]).

resolve_intent(Text, [{Intent, Regex, Options} | Rest]) ->
  case re:run(Text, Regex, [caseless | Options]) of
    {match, Captured} ->
      {Intent, Captured};
    nomatch ->
      resolve_intent(Text, Rest)
  end;
resolve_intent(<<"elo">>, []) ->
  {play, ["oXJ6wWy5wXU"]};
resolve_intent(Text, []) ->
  {nomatch, Text}.

handle_intent({next, _Captured}, _Message) ->
  agner_player_server:next();
handle_intent({volume, [Captured]}, #{<<"channel">> := Channel} = Message) when is_tuple(Captured) ->
  lager:info(jiffy:encode(Message)),
  agner_player_server:get_volume(Channel);
handle_intent({volume, [Level]}, _Message) ->
  agner_player_server:volume(Level);
handle_intent({say, [Text]}, _Message) ->
  agner_player_server:say(Text);
handle_intent({pause, _Captured}, _Message) ->
  agner_player_server:pause();
handle_intent({delete, _Captured}, _Message) ->
  agner_player_server:delete();
handle_intent({previous, _Captured}, _Message) ->
  agner_player_server:previous();
handle_intent({seek, [To]}, _Message) ->
  agner_player_server:seek(To);
handle_intent({nomatch, Text}, _Message) ->
  lager:info("Nomatch. Text: ~s", [Text]);
handle_intent({play, [MoveId]}, _Me0ssage) ->
  agner_player_server:play(MoveId);
handle_intent({now, _Captured}, #{<<"channel">> := Channel} = Message) ->
  lager:info(jiffy:encode(Message)),
  agner_player_server:now(Channel).
