-module(agner_slack_rest).

-define(SLACK_HOST, "slack.com").
-define(SLACK_REST_URI, "/api/rtm.connect?token=").

-export([obtain_wss_url/0]).

obtain_wss_url() ->
  {ok, ConnPid} = gun:open(?SLACK_HOST, 443),
  {ok, _Protocol} = gun:await_up(ConnPid),
  {ok, SlackToken} = application:get_env(slack_token),

  Url = erlang:iolist_to_binary([?SLACK_REST_URI, SlackToken]),

  MRef = gun:get(ConnPid, Url),

  receive
    {gun_response, ConnPid, _StreamRef, fin, _Status, _Headers} ->
      no_data;
    {gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
      receive_data(ConnPid, MRef, StreamRef);
    {'DOWN', MRef, process, ConnPid, Reason} ->
      error_logger:error_msg("Oops!"),
      exit(Reason)
  after 1000 ->
    exit(timeout)
  end.

receive_data(ConnPid, MRef, StreamRef) ->
  receive
    {gun_data, ConnPid, StreamRef, _FinStatus, Data} ->
      ParsedJson = jiffy:decode(Data, [return_maps]),
      extract_url(ParsedJson);
    {'DOWN', MRef, process, ConnPid, Reason} ->
      error_logger:error_msg("Unable to obtain wss url"),
      exit(Reason);
    Else ->
      error_logger:info_msg("receive_data received Else ~w", [Else])
  after 1000 ->
    exit(timeout)
  end.

extract_url(#{<<"ok">> := true, <<"url">> := Url} = _ParsedJson) ->
  {ok, binary_to_list(Url)};
extract_url(#{<<"error">> := Error} = _ParsedJson) ->
  exit({error, Error});
extract_url(ParsedJson) ->
  exit({error, unable_to_extract_url, ParsedJson}).

