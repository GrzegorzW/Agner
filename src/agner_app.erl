-module(agner_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


%%-export([agner_mpv_player/1]).

start(_Type, _Args) ->
  ok = error_logger:logfile({open, "agner.log"}),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, agner, "index.html"}},
      {"/static/[...]", cowboy_static, {priv_dir, agner, "static"}},
      {"/player", agner_player_client, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 7777}], #{env => #{dispatch => Dispatch}}),

  agner_mnesia:start(),

%%  agner_player_mpv_client:start(),

  agner_sup:start_link().

stop(_State) ->
  ok.

%%play(Url) ->
%%
%%  erlang:display(<<"MASTER">>),
%%  erlang:display(self()),
%%
%%  spawn(?MODULE, agner_mpv_player, [Url]).
%%
%%agner_mpv_player(Url) ->
%%
%%  erlang:display(<<"MASTER">>),
%%
%%
%%  Command = lists:concat(["/usr/local/bin/mpv --no-video ", Url]),
%%
%%  erlang:display(Command),
%%
%%  Reason = my_exec(Command),
%%
%%  erlang:display(Reason),
%%  erlang:display(<<"===================================================">>),
%%  erlang:display(<<"===================================================">>),
%%  erlang:display(<<"===================================================">>),
%%  erlang:display(<<"===================================================">>),
%%  erlang:display(<<"===================================================">>),
%%  erlang:display(<<"===================================================">>).


%%my_exec(Command) ->
%%  Port = open_port({spawn, Command}, [stream, in, exit_status]),
%%  get_data(Port, []).
%%
%%get_data(Port, Sofar) ->
%%  receive
%%    {Port, {data, Bytes}} ->
%%
%%      erlang:display(<<"{Port, {data, Bytes}}">>),
%%
%%      get_data(Port, [Sofar | Bytes]);
%%    {Port, eof} ->
%%
%%      erlang:display(<<"{Port, eof}">>),
%%
%%
%%      Port ! {self(), close},
%%      receive
%%        {Port, closed} ->
%%          erlang:display(<<"{Port, closed}">>),
%%
%%
%%          true
%%      end,
%%      receive
%%        {'EXIT', Port, _} ->
%%          erlang:display(<<"{'EXIT',  Port,  _}">>),
%%
%%
%%          ok
%%      after 1 ->              % force context switch
%%
%%        erlang:display(<<" after 1">>),
%%
%%        ok
%%      end,
%%      ExitCode =
%%        receive
%%          {Port, {exit_status, Code}} ->
%%
%%            erlang:display(<<"{Port, {exit_status, Code}}">>),
%%
%%
%%            Code
%%        end,
%%      {ExitCode, lists:flatten(Sofar)}
%%  end

%%  agner_mpv_client ! {player_end, Reason}
%%.