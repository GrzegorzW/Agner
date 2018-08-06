-module(agner_player_mpv_client).

-export([start/0, stop/0, start_link/0, init/0, mpv_listen/1, start_player/1]).

-define(MPV, agner_mpv_connected_process).

-record(player_state, {mpv_pid, current_movie_id, current_movie_source}).

start() ->
  spawn(?MODULE, init, []).

start_link() ->
  Pid = spawn_link(?MODULE, init, []),
  {ok, Pid}.

stop() ->
  agner_mpv_client ! stop.

init() ->
  error_logger:info_msg("agner_mpv_client started: ~p", [self()]),
  process_flag(trap_exit, true),
  register(agner_mpv_client, self()),
  agner_player_server:subscribe(self()),

  State = #player_state{_ = '_'},

  loop(State).

loop(
    State = #player_state{
      mpv_pid = CurrentMpvPid,
      current_movie_id = CurrentMovieId,
      current_movie_source = CurrentSource}
) ->
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
    {deleted, _MovieId} ->
      agner_player_server:get(),
      loop(State);
    {volume, Level} ->
      set_volume(Level),
      loop(State);
    added_to_empty_queue when CurrentSource =:= random ->
      agner_player_server:get(),
      loop(State);
    {play, NewMovieId, Source} ->
      Url = lists:concat(["https://www.youtube.com/watch?v=", NewMovieId]),
      stop_player(CurrentMpvPid),
      MpvPid = spawn_link(?MODULE, start_player, [Url]),
      NewState = State#player_state{mpv_pid = MpvPid, current_movie_id = NewMovieId, current_movie_source = Source},
      loop(NewState);
    delete ->
      agner_player_server:delete(CurrentMovieId),
      loop(State);
    {'EXIT', _FromPid, Reason} ->
      error_logger:info_msg("mpv loop 'EXIT' reason: ~w", [Reason]),
      case Reason of
        normal ->
          loop(State);
        end_of_movie ->
          agner_player_server:get(),
          loop(State);
        mpv_killed_remotely ->
          stop_player(CurrentMpvPid);
        {error, Error} ->
          stop_player(CurrentMpvPid),
          exit(Error);
        Else ->
          error_logger:info_msg("Not handled 'EXIT' case clause: ~w", [Else])
      end;
    Else ->
      erlang:display(?MODULE),
      erlang:display(<<"ELSE">>),
      erlang:display(Else),
      loop(State)
  end.

start_player(Url) ->
  Command = lists:concat(["mpv --no-video --no-terminal --cache=no --osc=no ", Url]),
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
  error_logger:info_msg("mpv killed (PID: ~p)", [OsPid]),
  ok.

set_volume(Level) when is_binary(Level) ->
  set_volume(binary_to_integer(Level));
set_volume(Level) when is_integer(Level) ->
  {ok, AudioDevice} = application:get_env(audio_device),
  Command = lists:concat(["amixer sset ", AudioDevice, " ", Level, "%"]),
  os:cmd(Command).
