-module(agner_slack_chat).

-export([start/0]).
-export([chat/1]).

start() ->
  register(chat, spawn(?MODULE, chat, [false])).

chat(Connected) ->
  if
    Connected =:= false -> connect();
    true -> ok
  end,

  receive
    {gun_ws, _ConnPid, Frame} ->
      handle_frame(Frame),
      chat(true);
    {gun_ws_upgrade, _ConnPid, ok, _Headers} ->
      chat(true);
    {gun_response, _ConnPid, _, _, Status, Headers} ->
      exit({ws_upgrade_failed, Status, Headers});
    {gun_error, _ConnPid, _StreamRef, Reason} ->
      exit({ws_upgrade_failed, Reason})
  end.

connect() ->
  WssUrl = agner_slack_rest:obtain_wss_url(),
  {_Scheme, Host, Path, _Query, _Fragment} = mochiweb_util:urlsplit(binary_to_list(WssUrl)),

  {ok, ConnPid} = gun:open(Host, 443),
  {ok, http} = gun:await_up(ConnPid),
  gun:ws_upgrade(ConnPid, Path),
  {ok, ConnPid}.

%%
%%todo- handle reconnect_url
%%
handle_frame({text, Body}) ->
  ParsedJson = jiffy:decode(Body, [return_maps]),
  case maps:get(<<"type">>, ParsedJson) of
    <<"message">> ->
      handle_message(ParsedJson);
    _Type ->
      unsupported_type
  end.

handle_message(Message) ->
  case has_subtype(Message) of
    true ->
      case maps:get(<<"subtype">>, Message) of
        <<"message_changed">> ->
          handle_sub_message(maps:get(<<"message">>, Message));
        <<"channel_join">> ->
          erlang:display(<<"channel join">>),
          erlang:display(Message);
        _ ->
          erlang:display(<<"UNSUPPORTED SUBTYPE">>),
          erlang:display(Message)
      end;
    false ->
      handle_text_message(maps:get(<<"text">>, Message))
  end.

has_subtype(Message) ->
  maps:is_key(<<"subtype">>, Message).

handle_sub_message(SubMessage) ->
  case has_attachments(SubMessage) of
    true ->
      UserId = binary_to_list(maps:get(<<"user">>, SubMessage)),
      handle_attachments(UserId, maps:get(<<"attachments">>, SubMessage));
    false ->
      no_attachments
  end.

has_attachments(Message) ->
  maps:is_key(<<"attachments">>, Message).

handle_attachments(_UserId, []) ->
  ok;
handle_attachments(UserId, [Head | Tail]) ->
  case maps:is_key(<<"video_html">>, Head) of
    true ->
      MovieId = extract_movie_id(maps:get(<<"from_url">>, Head)),
      Title = binary_to_list(maps:get(<<"title">>, Head)),
      agner_playlist:add(MovieId, Title, UserId);
    false ->
      no_attachments
  end,

  handle_attachments(UserId, Tail).

extract_movie_id(MovieUrl) ->
  Decoded = cow_qs:urldecode(MovieUrl),
  {_Scheme, _Host, _Path, Query, _Fragment} = mochiweb_util:urlsplit(binary_to_list(Decoded)),
  [MovieId] = [V || {"v", V} <- mochiweb_util:parse_qs(Query)],
  MovieId.

handle_text_message(Text) ->
  case resolve_intent(Text) of
    {match, next, _Captured} ->
      agner_playlist:next();
    {match, volume, [Level]} ->
      agner_playlist:volume(Level);
    nomatch
      ->
      erlang:display(<<"nomatch">>),
      erlang:display(Text)
  end.

resolve_intent(Text) ->
  resolve_intent(Text, [
    {next, "^next$", []},
    {volume, "^volume (?<level>([0-9]|[1-9][0-9]|100))$", [{capture, ['level'], binary}]}
  ]).

resolve_intent(Text, [{Intent, Regex, Options} | Rest]) ->
  case re:run(Text, Regex, Options) of
    {match, Captured} ->
      {match, Intent, Captured};
    nomatch ->
      resolve_intent(Text, Rest)
  end;
resolve_intent(_Text, []) ->
  nomatch.