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
  {_Scheme, Host, Path, _Query, _Fragment} = mochiweb_util:urlsplit(WssUrl),

  {ok, ConnPid} = gun:open(Host, 443),
  {ok, http} = gun:await_up(ConnPid),
  gun:ws_upgrade(ConnPid, Path),
  {ok, ConnPid}.

%%
%%todo- handle reconnect_url
%%
handle_frame({text, Body}) ->
  ParsedJson = jiffy:decode(Body, [return_maps]),
  handle_parsed_frame(ParsedJson).

handle_parsed_frame(#{<<"type">> := <<"message">>} = ParsedJson) ->
  handle_message(ParsedJson);
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
  agner_playlist:add(MovieId, Title, UserId),
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
  handle_intent(Intent).

resolve_intent(Text) ->
  resolve_intent(Text, [
    {next, "^next$", []},
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
  agner_playlist:next();
handle_intent({volume, [Level]}) ->
  agner_playlist:volume(Level);
handle_intent({nomatch, Text}) ->
  error_logger:info_msg("Nomatch. Text: ~s", [Text]).