-module(agner_slack_rest).

-define(SLACK_HOST, "slack.com").
-define(SLACK_WS_URI, "/api/rtm.connect").
-define(SLACK_USER_PROFILE_URI, "/api/users.profile.get").

-export([
  obtain_wss_url/0,
  obtain_user_profile/1
]).

obtain_wss_url() ->
  {ok, SlackToken} = application:get_env(slack_token),
  Url = erlang:iolist_to_binary([?SLACK_WS_URI, "?token=", SlackToken]),
  get_data(Url).

obtain_user_profile(UserId) ->
  {ok, SlackToken} = application:get_env(slack_token),
  Url = erlang:iolist_to_binary([?SLACK_USER_PROFILE_URI, "?token=", SlackToken, "&user=", UserId]),
  get_data(Url).

get_data(Url) ->
  lager:info("getting data from url: ~p", [Url]),

  {ok, ConnPid} = gun:open(?SLACK_HOST, 443),
  {ok, _Protocol} = gun:await_up(ConnPid),

  MRef = gun:get(ConnPid, Url),

  receive
    {gun_response, ConnPid, _StreamRef, fin, _Status, _Headers} ->
      no_data;
    {gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
      receive_data(ConnPid, MRef, StreamRef);
    {'DOWN', MRef, process, ConnPid, Reason} ->
      exit({slack_connection_error, Reason})
  after 1000 ->
    exit(timeout)
  end.

receive_data(ConnPid, MRef, StreamRef) ->
  receive
    {gun_data, ConnPid, StreamRef, _FinStatus, Data} ->
      ParsedJson = jiffy:decode(Data, [return_maps]),
      extract_data(ParsedJson);
    {'DOWN', MRef, process, ConnPid, Reason} ->
      exit({slack_receive_data_error, Reason});
    Else ->
      lager:info("receive_data received Else ~w", [Else])
  after 1000 ->
    exit(timeout)
  end.

extract_data(#{<<"ok">> := true, <<"url">> := Url} = _ParsedJson) ->
  {ok, binary_to_list(Url)};
extract_data(#{<<"ok">> := true, <<"profile">> := Profile} = _ParsedJson) ->
  lager:info("profile: ~p", [Profile]),

  {ok};
extract_data(#{<<"error">> := _Error} = ParsedJson) ->
  exit({slack_error, ParsedJson});
extract_data(ParsedJson) ->
  exit({slack_error, unable_to_extract_data, ParsedJson}).
