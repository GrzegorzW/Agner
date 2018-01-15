-module(agner_player_server).

-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([subscribe/1, volume/1, next/0, add/3, get/0, delete/0, pause/0, delete/1, has_active_subscriber/0]).

-record(playlist_state, {queue, player_client, current_song}).

-define(CHECKER_TIMEOUT, 10000).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  error_logger:info_msg(<<"PLAYER SERVER INITIALIZED">>),
  State = #playlist_state{queue = queue:new()},
  {ok, State}.

subscribe(ClientPid) when is_pid(ClientPid) ->
  gen_server:cast(?MODULE, {subscribe, ClientPid}).

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

has_active_subscriber() ->
  gen_server:call(?MODULE, has_active_subscriber).

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
  {noreply, State};

handle_cast({subscribe, NewClient}, State = #playlist_state{player_client = CurrentClient}) ->
  error_logger:info_msg("Client connected: ~p", [NewClient]),
  maybe_detach_client(CurrentClient),
  NewState = State#playlist_state{player_client = NewClient},
  NewClient ! subscriber_added,
  {noreply, NewState}.

handle_call(has_active_subscriber, _From, State = #playlist_state{player_client = undefined}) ->
  {reply, false, State};

handle_call(has_active_subscriber, _From, State) ->
  {reply, true, State};

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
