-module(agner_player_slave).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  agner_server:subscribe_slave(self()),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  JsonRequest = jiffy:decode(Msg, [return_maps]),
  case websocket_handle_message(JsonRequest) of
    {reply, Json} ->
      {reply, {text, Json}, State};
    noreply ->
      {ok, State}
  end.

websocket_handle_message(#{<<"action">> := <<"ping">>} = _ParsedJson) ->
  {reply, jiffy:encode(#{<<"action">> => <<"pong">>})};

websocket_handle_message(_ParsedJson) ->
  {reply, jiffy:encode(#{<<"action">> => <<"error">>, <<"reason">> => <<"unsupported_action">>})}.

websocket_info({timeout, _Ref, Msg}, State) ->
  {reply, {text, Msg}, State};

websocket_info({say, Text}, State) ->
  Response = jiffy:encode(#{
    <<"action">> => <<"say">>,
    <<"text">> => Text
  }),
  {reply, {text, Response}, State};

websocket_info({seek, To}, State) ->
  Response = jiffy:encode(#{
    <<"action">> => <<"seek">>,
    <<"to">> => To
  }),
  {reply, {text, Response}, State};

websocket_info({play, MovieId, Source}, State) ->
  Response = jiffy:encode(#{
    <<"action">> => <<"play">>,
    <<"movieId">> => list_to_binary(MovieId),
    <<"source">> => Source
  }),
  {reply, {text, Response}, State};

websocket_info(pause, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"pause">>}),
  {reply, {text, Response}, State};

websocket_info(previous, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"previous">>}),
  {reply, {text, Response}, State};

websocket_info(stop, State) ->
  lager:info("websocket_info stop"),
  {stop, State};

websocket_info(Info, State) ->
  lager:info("Slave player unsupported message: ~w~n", [Info]),
  {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
  agner_server:unsubscribe_slave(self()),
  ok.
