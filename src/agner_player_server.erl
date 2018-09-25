-module(agner_player_server).

-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
  subscribe/1,
  volume/1,
  next/0,
  add/3,
  get/0,
  delete/0,
  delete/1,
  previous/0,
  pause/0,
  seek/1,
  reconnect_slack/0,
  chat_connected/1,
  now/1,
  answer/2
]).

-record(playlist_state, {queue, player_client, current_song, chat}).

-define(CHECKER_TIMEOUT, 10000).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  lager:info("PLAYER SERVER INITIALIZED"),
  State = #playlist_state{queue = queue:new()},
  {ok, State}.

subscribe(ClientPid) when is_pid(ClientPid) ->
  gen_server:cast(?MODULE, {subscribe, ClientPid}).

volume(Level) ->
  gen_server:cast(?MODULE, {volume, Level}).

seek(To) ->
  gen_server:cast(?MODULE, {seek, To}).

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

previous() ->
  gen_server:cast(?MODULE, previous).

chat_connected(Pid) ->
  gen_server:cast(?MODULE, {chat_connected, Pid}).

reconnect_slack() ->
  gen_server:cast(?MODULE, reconnect_slack).

now(MessageId) ->
  gen_server:cast(?MODULE, {question, now, MessageId}).

answer(MessageId, Answer) ->
  gen_server:cast(?MODULE, {answer, MessageId, Answer}).

handle_cast({volume, Level}, State = #playlist_state{player_client = PlayerClient}) ->
  lager:info("~p ! volume ~p", [PlayerClient, Level]),
  PlayerClient ! {volume, Level},
  {noreply, State};

handle_cast({seek, To}, State = #playlist_state{player_client = PlayerClient}) ->
  lager:info("~p ! seek to ~p", [PlayerClient, To]),
  PlayerClient ! {seek, To},
  {noreply, State};

handle_cast(next, State = #playlist_state{player_client = PlayerClient}) ->
  lager:info("~p ! next", [PlayerClient]),
  PlayerClient ! next,
  {noreply, State};

handle_cast({add, MovieId, Title, User}, State = #playlist_state{queue = Queue, player_client = PlayerClient}) ->
  agner_playlist:add(MovieId, Title, User),
  case queue:is_empty(Queue) of
    true -> PlayerClient ! added_to_empty_queue;
    false -> ok
  end,
  NewState = State#playlist_state{queue = queue:in(MovieId, Queue)},
  {noreply, NewState};

handle_cast(get, State = #playlist_state{queue = Queue, player_client = PlayerClient}) ->
  lager:info("~p ! play", [PlayerClient]),
  NewState = case queue:is_empty(Queue) of
               true ->
                 {ok, MovieId} = agner_playlist:get(),
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

handle_cast(previous, State = #playlist_state{player_client = PlayerClient}) ->
  PlayerClient ! previous,
  {noreply, State};

handle_cast(delete, State = #playlist_state{player_client = PlayerClient}) ->
  PlayerClient ! delete,
  {noreply, State};

handle_cast({delete, MovieId}, State = #playlist_state{player_client = PlayerClient}) ->
  agner_playlist:delete(MovieId),
  PlayerClient ! {deleted, MovieId},
  {noreply, State};

handle_cast({chat_connected, Pid}, State) ->
  {noreply, State#playlist_state{chat = Pid}};

handle_cast(reconnect_slack, State = #playlist_state{chat = Pid}) when is_pid(Pid) ->
  Pid ! {shutdown, self()},
  {noreply, State};

handle_cast({answer, MessageId, Answer}, State = #playlist_state{chat = Chat}) when is_pid(Chat) ->
  lager:info("Answering. MessageId: ~p Answer: ~p", [MessageId, Answer]),

  Chat ! {answer, MessageId, Answer},
  {noreply, State};

handle_cast({subscribe, NewClient}, State = #playlist_state{player_client = CurrentClient}) ->
  lager:info("Client connected: ~p", [NewClient]),
  maybe_detach_client(CurrentClient),
  NewState = State#playlist_state{player_client = NewClient},
  NewClient ! subscriber_added,
  {noreply, NewState};

handle_cast({question, now, MessageId}, State = #playlist_state{player_client = Player}) ->
  Player ! {now, MessageId},
  {noreply, State}.

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_info(Msg, State) ->
  lager:warning("Unexpected message: ~w~n", [Msg]),
  {noreply, State}.

maybe_detach_client(ClientPid) when is_pid(ClientPid) ->
  ClientPid ! detach,
  ok;
maybe_detach_client(undefined) ->
  ok.

terminate(_, _) ->
  ok.

code_change(_, _, _) ->
  ok.
