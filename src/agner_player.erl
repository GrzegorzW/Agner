-module(agner_player).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  agner_playlist:subscribe(self()),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  ParsedJson = jiffy:decode(Msg, [return_maps]),

  case maps:get(<<"action">>, ParsedJson) of
    <<"get">> ->
      {MovieId, Source} = agner_playlist:get(),

      Response = jiffy:encode(#{
        <<"action">> => <<"play">>,
        <<"movieId">> => list_to_binary(MovieId),
        <<"source">> => Source
      }),
      {reply, {text, Response}, State};
    <<"ping">> ->
      Response = jiffy:encode(#{<<"action">> => <<"pong">>}),
      {reply, {text, Response}, State};
    _ ->
      Response = jiffy:encode(#{
        <<"action">> => <<"error">>,
        <<"reason">> => <<"unsupported_action">>
      }),
      {reply, {text, Response}, State}
  end.

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
websocket_info(Info, State) ->
  erlang:display(Info),
  {ok, State}.
