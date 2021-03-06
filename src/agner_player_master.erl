-module(agner_player_master).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  agner_server:subscribe_master(self()),
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
  ok = agner_server:get(),
  noreply;

websocket_handle_message(#{<<"action">> := <<"ping">>} = _ParsedJson) ->
  {reply, jiffy:encode(#{<<"action">> => <<"pong">>})};

websocket_handle_message(#{<<"action">> := <<"delete">>, <<"movieId">> := MovieId} = _ParsedJson) ->
  ok = agner_server:delete(binary_to_list(MovieId)),
  noreply;

websocket_handle_message(#{<<"action">> := <<"reconnect_slack">>} = _ParsedJson) ->
  ok = agner_server:reconnect_slack(),
  noreply;

websocket_handle_message(#{<<"action">> := <<"answer">>, <<"messageId">> := MessageId, <<"answer">> := Answer} = _) ->
  ok = agner_server:answer(MessageId, Answer),
  noreply;

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

websocket_info(delete, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"delete">>}),
  {reply, {text, Response}, State};

websocket_info(previous, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"previous">>}),
  {reply, {text, Response}, State};

websocket_info({now, MessageId}, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"now">>, <<"messageId">> => MessageId}),
  {reply, {text, Response}, State};

websocket_info({get_volume, MessageId}, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"get_volume">>, <<"messageId">> => MessageId}),
  {reply, {text, Response}, State};

websocket_info({deleted, MovieId}, State) ->
  Response = jiffy:encode(#{
    <<"action">> => <<"deleted">>,
    <<"movieId">> => list_to_binary(MovieId)
  }),
  {reply, {text, Response}, State};

websocket_info(detach, State) ->
  lager:info("websocket_info detach"),
  {stop, State};

websocket_info(stop, State) ->
  lager:info("websocket_info stop"),
  {stop, State};

websocket_info(Info, State) ->
  lager:info("Master player unsupported message: ~w~n", [Info]),
  {ok, State}.
