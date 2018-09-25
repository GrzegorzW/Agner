-module(agner_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  {ok, Port} = application:get_env(port),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, agner, "index.html"}},
      {"/static/[...]", cowboy_static, {priv_dir, agner, "static"}},
      {"/player", agner_player_client, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, Port}], #{env => #{dispatch => Dispatch}}),

  agner_sup:start_link().

stop(_State) ->
  ok.
