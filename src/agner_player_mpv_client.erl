-module(agner_player_mpv_client).

-export([start/0, stop/0, start_link/0, init/0, mpv_listen/1, start_player/1]).

-define(MPV, agner_mpv_connected_process).

-record(player_state, {mpv_pid}).

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

  State = #player_state{_ = '_'},

  loop(State).

loop(State = #player_state{mpv_pid = CurrentMpvPid}) ->
  receive
    detach ->
      stop_player(CurrentMpvPid);
    stop ->
      stop_player(CurrentMpvPid);
    subscriber_added ->
      agner_player_server:get(),
      loop(State);
    next ->
      agner_player_server:get(),
      loop(State);
    added_to_empty_queue ->
      agner_player_server:get(),
      loop(State);
    {play, MovieId, _Source} ->
      Url = lists:concat(["https://www.youtube.com/watch?v=", MovieId]),
      stop_player(CurrentMpvPid),
      MpvPid = spawn_link(?MODULE, start_player, [Url]),
      NewState = State#player_state{mpv_pid = MpvPid},
      loop(NewState);
    {error, _Reason} ->
      stop_player(CurrentMpvPid),
      loop(State);
    {'EXIT', _FromPid, Reason} ->
      case Reason of
        force_stop_player ->
          agner_player_server:get(),
          loop(State);
        normal ->
          agner_player_server:get(),
          loop(State);
        end_of_movie ->
          agner_player_server:get(),
          loop(State);
        mpv_killed_remotely ->
          stop_player(CurrentMpvPid);
        {error, {status_code, StatusCode}} ->
          stop_player(CurrentMpvPid),
          exit(StatusCode)
      end
  end.

start_player(_Url) ->
  Command = lists:concat(["tail -f /dev/null"]),
%%  Command = lists:concat(["/usr/local/bin/mpv --no-video ", Url]),
  Port = open_port({spawn, Command}, [stream, in, exit_status]),
  mpv_listen(Port).

stop_player(MpvPid) when is_pid(MpvPid) ->
  MpvPid ! stop;
stop_player(_MpvPid) ->
  ok.

mpv_listen(Port) ->
  receive
    {Port, {data, _Data}} ->
      mpv_listen(Port);
    stop ->
      kill_mpv(Port);
    {Port, {exit_status, StatusCode}} ->
      Reason = case StatusCode of
                 0 ->
                   end_of_movie;
                 4 ->
                   mpv_killed_remotely;
                 _Else ->
                   {error, {status_code, StatusCode}}
               end,
      exit(Reason);
    {'EXIT', _FromPid, Reason} ->
      kill_mpv(Port),
      exit(Reason);
    Else ->
      error_logger:info_msg("mpv_listen ELSE: ~w", [Else])
  end.

kill_mpv(Port) when is_port(Port) ->
  {os_pid, OsPid} = erlang:port_info(Port, os_pid),
  os:cmd(io_lib:format("kill -9 ~p", [OsPid])),
  error_logger:info_msg("PID killed ~p", [OsPid]),
  ok.
