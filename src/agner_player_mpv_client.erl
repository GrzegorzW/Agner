-module(agner_player_mpv_client).

-export([start/0, stop/0, start_link/0, init/0]).

%%todo remove after tests
-export([play/1]).

-define(PORT_NAME, agner_mpv_port).

start() ->
  spawn(?MODULE, init, []).

start_link() ->
  Pid = spawn_link(?MODULE, init, []),
  {ok, Pid}.

stop() ->
  agner_mpv_client ! stop.

init() ->
  process_flag(trap_exit, true),
  register(agner_mpv_client, self()),
  agner_player_server:subscribe(self()),
  loop().

loop() ->
  error_logger:info_msg("loop"),

  receive
    end_of_movie ->
      error_logger:info_msg("agner_player_mpv_client subscriber_added"),

      agner_player_server:get(),
      loop();
    stop ->
      stop_player();
    subscriber_added ->
      error_logger:info_msg("agner_player_mpv_client subscriber_added"),

      agner_player_server:get(),
      loop();
    next ->
      error_logger:info_msg("agner_player_mpv_client next"),

      agner_player_server:get(),
      loop();
    added_to_empty_queue ->
      error_logger:info_msg("agner_player_mpv_client added_to_empty_queue"),

      agner_player_server:get(),
      loop();
    {play, MovieId, _Source} ->
      error_logger:info_msg("agner_player_mpv_client play: ~s", [MovieId]),

      Url = lists:concat(["https://www.youtube.com/watch?v=", MovieId]),
      play(Url),
      loop();
    {error, Reason} ->
      error_logger:info_msg("error: ~s", [Reason]),
      loop();
    {'EXIT', FromPid, Reason} ->
      error_logger:info_msg("EXIT"),
      erlang:display(FromPid),
      erlang:display(Reason);
    Else ->
      error_logger:info_msg("else: ~s", [Else]),
      loop()
  end.

play(Url) ->
  Command = lists:concat(["/usr/local/bin/mpv --no-video ", Url]),
  Port = open_port({spawn, Command}, [stream, in, exit_status]),
  register(?PORT_NAME, Port),
  listen(Port).

stop_player() ->
  ?PORT_NAME ! stop.

listen(Port) ->
  receive
    {Port, {data, Data}} ->
      erlang:display(Data),
      listen(Port);
    stop ->
      error_logger:info_msg("stop"),
      Port ! {self(), close},
      receive
        {Port, closed} ->
          error_logger:info_msg("stop -> closed"),

          {os_pid, OsPid} = erlang:port_info(Port, os_pid),
          os:cmd(io_lib:format("kill -9 ~p", [OsPid])),

          unregister(?PORT_NAME),
          exit(normal)
      end;
    {Port, {exit_status, StatusCode}} ->
      Message = case StatusCode of
                  0 ->
                    end_of_movie;
                  4 ->
                    mpv_killed_remotely;
                  _Else ->
                    {error, {status_code, StatusCode}}
                end,
      error_logger:info_msg("agner_mpv_client ! ~s", [Message]),
      agner_mpv_client ! Message,

      exit(normal);
    Else ->
      error_logger:info_msg("ELSE"),
      error_logger:info_msg(Else)
  end.
