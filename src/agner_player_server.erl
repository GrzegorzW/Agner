-module(agner_player_server).

-behavior(gen_server).

-export([subscribe/1, volume/1, next/0, add/3, get/0, delete/0, pause/0, delete/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, subscriber_checker_alive_loop/2]).

-record(playlist_state, {queue, player_client, current_song}).

-define(CHECKER_TIMEOUT, 2000).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  error_logger:info_msg(<<"PLAYER SERVER INITIALIZED">>),
  State = #playlist_state{queue = queue:new()},
  {ok, State}.

subscribe(ClientPid) when is_pid(ClientPid) ->
  gen_server:call(?MODULE, {subscribe, ClientPid}).

volume(Level) ->
  gen_server:cast(?MODULE, {volume, Level}).

next() ->
  gen_server:cast(?MODULE, next).

add(MovieId, Title, User) ->
  gen_server:cast(?MODULE, {add, MovieId, Title, User}).

get() ->
  gen_server:cast(?MODULE, get).

pause() ->
  gen_server:cast(?MODULE, pause).

delete() ->
  gen_server:cast(?MODULE, delete).

delete(MovieId) ->
  gen_server:cast(?MODULE, {delete, MovieId}).

handle_cast({volume, Level}, State = #playlist_state{player_client = PlayerClient}) ->
  error_logger:info_msg("~p ! volume ~p", [PlayerClient, Level]),

  PlayerClient ! {volume, Level},
  {noreply, State};
handle_cast(next, State = #playlist_state{player_client = PlayerClient}) ->
  error_logger:info_msg("~p ! next", [PlayerClient]),

  PlayerClient ! next,
  {noreply, State};
handle_cast({add, MovieId, Title, User}, State = #playlist_state{queue = Queue, player_client = PlayerClient}) ->
  agner_mnesia:add_movie(MovieId, Title, User),
  case queue:is_empty(Queue) of
    true -> PlayerClient ! added_to_empty_queue;
    false -> ok
  end,
  NewState = State#playlist_state{queue = queue:in(MovieId, Queue)},
  {noreply, NewState};
handle_cast(get, State = #playlist_state{queue = Queue, player_client = PlayerClient}) ->
  error_logger:info_msg("~p ! play", [PlayerClient]),

  NewState = case queue:is_empty(Queue) of
               true ->
                 {ok, MovieId} = agner_mnesia:get_random_movie(),
                 PlayerClient ! {play, MovieId, random},
                 State;
               false ->
                 {{value, Item}, Q2} = queue:out(Queue),
                 PlayerClient ! {play, Item, queue},
                 State#playlist_state{queue = Q2}
             end,
  {noreply, NewState};

handle_cast(pause, State = #playlist_state{player_client = PlayerClient}) ->
  PlayerClient ! pause,
  {noreply, State};
handle_cast(delete, State = #playlist_state{player_client = PlayerClient}) ->
  PlayerClient ! delete,
  {noreply, State};
handle_cast({delete, MovieId}, State = #playlist_state{player_client = PlayerClient}) ->
  agner_mnesia:delete_movie(MovieId),

  PlayerClient ! {deleted, MovieId},

  {noreply, State}.

handle_call({subscribe, NewClient}, _From, State = #playlist_state{player_client = CurrentClient}) ->
  error_logger:info_msg("Client connected: ~p", [NewClient]),
  maybe_detach_client(CurrentClient),
  NewState = State#playlist_state{player_client = NewClient},
  start_subscriber_alive_checker(NewClient),
  NewClient ! subscriber_added,
  {reply, ok, NewState};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_info(Msg, State) ->
  error_logger:warning_msg("Unexpected message: ~w~n", [Msg]),
  {noreply, State}.

maybe_detach_client(ClientPid) when is_pid(ClientPid) ->
  ClientPid ! detach,
  ok;
maybe_detach_client(undefined) ->
  ok.

start_subscriber_alive_checker(ClientPid) when is_pid(ClientPid) ->
  ok.
%%  Pid = spawn_link(?MODULE, subscriber_checker_alive_loop, [ClientPid, ?CHECKER_TIMEOUT]),
%%  {ok, Pid}.

subscriber_checker_alive_loop(Pid, Timeout) ->
  receive
  after Timeout ->
    error_logger:warning_msg("subscriber_checker_alive_loop: ~p~n", [Pid]),
    case erlang:process_info(Pid) of
      undefined ->
        {ok, NewPid} = agner_player_mpv_client:start_link(),
        subscriber_checker_alive_loop(NewPid, Timeout);
      _Else ->
        subscriber_checker_alive_loop(Pid, Timeout)
    end
  end.
