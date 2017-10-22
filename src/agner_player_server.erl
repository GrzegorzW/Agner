-module(agner_player_server).

-behavior(gen_server).

-export([subscribe/1, volume/1, next/0, add/3, get/0, delete/0, delete/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(playlist_state, {queue, player_client, current_song}).

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

delete() ->
  gen_server:cast(?MODULE, delete).

delete(MovieId) ->
  gen_server:cast(?MODULE, {delete, MovieId}).

handle_cast({volume, Level}, State = #playlist_state{player_client = PlayerClient}) ->
  PlayerClient ! {volume, Level},
  {noreply, State};
handle_cast(next, State = #playlist_state{player_client = PlayerClient}) ->
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
  NewState = case queue:is_empty(Queue) of
               true ->
                 {ok, MovieId} = agner_mnesia:get_random_movie(),
                 PlayerClient ! {get, MovieId, random},
                 State;
               false ->
                 {{value, Item}, Q2} = queue:out(Queue),
                 PlayerClient ! {get, Item, queue},
                 State#playlist_state{queue = Q2}
             end,
  {noreply, NewState};
handle_cast(delete, State = #playlist_state{player_client = PlayerClient}) ->
  PlayerClient ! delete,
  {noreply, State};
handle_cast({delete, MovieId}, State = #playlist_state{player_client = PlayerClient}) ->
  agner_mnesia:delete_movie(MovieId),

  PlayerClient ! {deleted, MovieId},

  {noreply, State}.

handle_call({subscribe, ClientPid}, _From, State) ->
  NewState = State#playlist_state{player_client = ClientPid},
  {reply, ok, NewState};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  erlang:display(State),
  {noreply, State}.
