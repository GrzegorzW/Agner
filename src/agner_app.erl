-module(agner_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  ok = error_logger:logfile({open, "agner.log"}),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, agner, "index.html"}},
      {"/static/[...]", cowboy_static, {priv_dir, agner, "static"}},
      {"/player", agner_player, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 7777}], #{env => #{dispatch => Dispatch}}),

  agner_slack_chat:start(),
  agner_mnesia:start(),

  agner_sup:start_link().

stop(_State) ->
  ok.
