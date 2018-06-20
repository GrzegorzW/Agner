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
  upgrade(ConnPid).

connect() ->
  WssUrl = agner_slack_rest:obtain_wss_url(),
  {_Scheme, Host, Path, _Query, _Fragment} = mochiweb_util:urlsplit(WssUrl),

  {ok, ConnPid} = gun:open(Host, 443),
  {ok, http} = gun:await_up(ConnPid),

  gun:ws_upgrade(ConnPid, Path),

  {ok, ConnPid}.

upgrade(ConnPid) ->
  receive
    {gun_upgrade, ConnPid, _StreamRef, _Protocols, _Headers} ->
      erlang:display(<<"chat upgrade success">>),
      chat(ConnPid);
    {gun_response, ConnPid, _, _, Status, Headers} ->
      erlang:display(<<"chat gun_response">>),
      exit({ws_upgrade_failed, Status, Headers});
    {gun_error, ConnPid, _StreamRef, Reason} ->
      erlang:display(<<"chat gun_error">>),
      exit({ws_upgrade_failed, Reason});
    Else ->
      erlang:display(upgrade_message_else),
      erlang:display(Else),
      upgrade(ConnPid)
  after 5000 ->
    exit(slack_websocket_timeout)
  end.

chat(ConnPid) ->
  receive
    {gun_ws, ConnPid, _StreamRef, {text, Body}} ->
      handle_frame(Body),
      chat(ConnPid);
    {gun_down, Pid, _Protocol, Reason, _, _} ->
      exit({chat_connection_down, [{pid, Pid}, {reason, Reason}, {connPid, ConnPid}]});
    Else ->
      erlang:display(chat_message_else),
      erlang:display(Else),
      chat(ConnPid)
  end.

handle_frame(Body) ->
  ParsedJson = jiffy:decode(Body, [return_maps]),
  handle_parsed_frame(ParsedJson).

handle_parsed_frame(#{<<"type">> := <<"message">>} = Message) ->
  handle_message(Message);
handle_parsed_frame(#{<<"type">> := Type} = _ParsedJson) ->
  {unsupported_type, Type}.

handle_message(#{<<"subtype">> := <<"message_changed">>} = Message) ->
  handle_sub_message(maps:get(<<"message">>, Message));
handle_message(#{<<"subtype">> := <<"channel_join">>} = Message) ->
  error_logger:info_msg("Channel joind. Message: ~s", [Message]);
handle_message(#{<<"subtype">> := SubType} = Message) ->
  error_logger:info_msg("Unsupported subtype: ~s. Message: ~s", [SubType, Message]);
handle_message(#{<<"text">> := Text} = _Message) ->
  handle_text_message(Text).

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

extract_movie_id(MovieUrl) ->
  Decoded = cow_qs:urldecode(MovieUrl),
  {_Scheme, _Host, _Path, Query, _Fragment} = mochiweb_util:urlsplit(binary_to_list(Decoded)),
  [MovieId] = [V || {"v", V} <- mochiweb_util:parse_qs(Query)],
  MovieId.

get_movie_title(#{<<"title">> := Title} = _Attachment) ->
  binary_to_list(Title).

handle_text_message(Text) ->
  Intent = resolve_intent(Text),
  error_logger:info_msg("Resolved intent: ~w", [Intent]),

  handle_intent(Intent).

resolve_intent(Text) ->
  resolve_intent(Text, [
    {next, "^next$", []},
    {delete, "^delete$", []},
    {pause, "^pause", []},
    {volume, "^volume (?<level>([0-9]|[1-9][0-9]|100))$", [{capture, ['level'], binary}]}
  ]).

resolve_intent(Text, [{Intent, Regex, Options} | Rest]) ->
  case re:run(Text, Regex, Options) of
    {match, Captured} ->
      {Intent, Captured};
    nomatch ->
      resolve_intent(Text, Rest)
  end;
resolve_intent(Text, []) ->
  {nomatch, Text}.

handle_intent({next, _Captured}) ->
  agner_player_server:next();
handle_intent({volume, [Level]}) ->
  agner_player_server:volume(Level);
handle_intent({pause, _Captured}) ->
  agner_player_server:pause();
handle_intent({delete, _Captured}) ->
  agner_player_server:delete();
handle_intent({nomatch, Text}) ->
  error_logger:info_msg("Nomatch. Text: ~s", [Text]).
