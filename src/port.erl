-module(port).
%%
%%-export([start/1, stop/0, init/1]).
%%
%%start(MovieId) ->
%%  spawn(?MODULE, init, [MovieId]).
%%stop() ->
%%  ?MODULE ! stop.
%%
%%init(MovieId) ->
%%  Url = lists:concat(["https://www.youtube.com/watch?v=", MovieId]),
%%  Command = lists:concat(["/usr/local/bin/mpv --no-video ", Url]),
%%
%%  process_flag(trap_exit, true),
%%  Port = open_port({spawn, Command}, [stream, in, exit_status]),
%%  loop(Port).
%%
%%loop(Port) ->
%%  receive
%%    {call, Caller, Msg} ->
%%      Port ! {self(), {command, encode(Msg)}},
%%      receive
%%        {Port, {data, Data}} ->
%%          Caller ! {complex, decode(Data)}
%%      end,
%%      loop(Port);
%%    stop ->
%%      Port ! {self(), close},
%%      receive
%%        {Port, closed} ->
%%          exit(normal)
%%      end;
%%    {'EXIT', Port, Reason} ->
%%      erlang:display(<<"'EXIT'">>),
%%      erlang:display(Port),
%%      erlang:display(Reason),
%%
%%      exit(port_terminated)
%%  end.
%%
%%encode({foo, X}) -> [1, X].
%%decode([Int]) -> Int.
