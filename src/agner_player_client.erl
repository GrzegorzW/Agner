-module(agner_player_client).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  agner_player_server:subscribe(self()),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  JsonRequest = jiffy:decode(Msg, [return_maps]),
  case websocket_handle_message(JsonRequest) of
    {reply, Json} ->
      {reply, {text, Json}, State};
    noreply ->
      {ok, State}
  end.

websocket_handle_message(#{<<"action">> := <<"get">>} = _ParsedJson) ->
  ok = agner_player_server:get(),
  noreply;
websocket_handle_message(#{<<"action">> := <<"ping">>} = _ParsedJson) ->
  {reply, jiffy:encode(#{<<"action">> => <<"pong">>})};
websocket_handle_message(_ParsedJson) ->
  {reply, jiffy:encode(#{<<"action">> => <<"error">>, <<"reason">> => <<"unsupported_action">>})}.

websocket_info({timeout, _Ref, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info(added_to_empty_queue, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"added_to_empty_queue">>}),
  {reply, {text, Response}, State};
websocket_info(next, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"next">>}),
  {reply, {text, Response}, State};
websocket_info({volume, Level}, State) ->
  Response = jiffy:encode(#{
    <<"action">> => <<"volume">>,
    <<"level">> => Level
  }),
  {reply, {text, Response}, State};
websocket_info({get, MovieId, Source}, State) ->
  Response = jiffy:encode(#{
    <<"action">> => <<"play">>,
    <<"movieId">> => list_to_binary(MovieId),
    <<"source">> => Source
  }),
  {reply, {text, Response}, State};
websocket_info(stop, State) ->
  error_logger:info_msg(<<"websocket_info stop">>),
  {stop, State};
websocket_info(Info, State) ->
  error_logger:info_msg(<<"player_client unsupported message">>),
  error_logger:info_msg(Info),
  {ok, State}.
