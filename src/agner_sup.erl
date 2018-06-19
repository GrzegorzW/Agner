-module(agner_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  ChildSpecs = [{
    slack_chat,
    {agner_slack_chat, start_link, []},
    permanent, 5000, worker, [agner_slack_chat]
  }, {
    player_server,
    {agner_player_server, start_link, []},
    permanent, 5000, worker, [agner_player_server]
  },
%%    {
%%    agner_mpv_client,
%%    {agner_player_mpv_client, start_link, []},
%%    transient, 5000, worker, [agner_player_mpv_client]
%%  },
    {
    agner_playlist,
    {agner_playlist, start_link, []},
    permanent, 5000, worker, [agner_playlist]
  }],
  {ok, {{one_for_one, 10, 60}, ChildSpecs}}.
