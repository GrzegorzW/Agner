-module(agner_server).

-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
  subscribe_master/1,
  subscribe_slave/1,
  unsubscribe_slave/1,
  volume/1,
  say/1,
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
  answer/2,
  get_volume/1,
  play/1
]).

-record(playlist_state, {queue, master, slaves = [], current_song, chat}).

-define(CHECKER_TIMEOUT, 10000).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  lager:info("PLAYER SERVER INITIALIZED"),
  State = #playlist_state{queue = queue:new()},
  {ok, State}.

subscribe_master(MasterPlayer) when is_pid(MasterPlayer) ->
  gen_server:cast(?MODULE, {subscribe_master, MasterPlayer}).

subscribe_slave(SlavePlayer) when is_pid(SlavePlayer) ->
  gen_server:cast(?MODULE, {subscribe_slave, SlavePlayer}).

unsubscribe_slave(SlavePlayer) when is_pid(SlavePlayer) ->
  gen_server:cast(?MODULE, {unsubscribe_slave, SlavePlayer}).

volume(Level) ->
  gen_server:cast(?MODULE, {volume, Level}).

say(Text) ->
  gen_server:cast(?MODULE, {say, Text}).

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

get_volume(MessageId) ->
  gen_server:cast(?MODULE, {question, get_volume, MessageId}).

answer(MessageId, Answer) ->
  gen_server:cast(?MODULE, {answer, MessageId, Answer}).

play(MovieId) ->
  gen_server:cast(?MODULE, {play, MovieId}).

handle_cast({volume, Level}, State = #playlist_state{master = Player}) ->
  lager:info("~p ! volume ~p", [Player, Level]),
  Player ! {volume, Level},
  {noreply, State};

handle_cast({say, Message}, State) ->
  Trimmed = trim_message(Message, 80),
  notify_players({say, Trimmed}, State),
  {noreply, State};

handle_cast({seek, To}, State) ->
  notify_players( {seek, To}, State),
  {noreply, State};

handle_cast(next, State = #playlist_state{master = Player}) ->
  lager:info("~p ! next", [Player]),
  Player ! next,
  {noreply, State};

handle_cast({add, MovieId, Title, User}, State = #playlist_state{queue = Queue, master = Player}) ->
  agner_playlist:add(MovieId, Title, User),
  case queue:is_empty(Queue) of
    true -> Player ! added_to_empty_queue;
    false -> ok
  end,
  NewState = State#playlist_state{queue = queue:in(MovieId, Queue)},
  {noreply, NewState};

handle_cast(get, State = #playlist_state{queue = Queue, master = Player}) ->
  lager:info("~p ! get", [Player]),
  NewState = case queue:is_empty(Queue) of
               true ->
                 {ok, MovieId} = agner_playlist:get(),
                 notify_players({play, MovieId, random}, State),

                 State#playlist_state{current_song = MovieId};
               false ->
                 {{value, MovieId}, Q2} = queue:out(Queue),
                 notify_players({play, MovieId, queue}, State),
                 State#playlist_state{queue = Q2, current_song = MovieId}
             end,
  {noreply, NewState};

handle_cast(pause, State) ->
  notify_players(pause, State),
  {noreply, State};

handle_cast(previous, State) ->
  notify_players(previous, State),
  {noreply, State};

handle_cast(delete, State = #playlist_state{master = Player}) ->
  Player ! delete,
  {noreply, State};

handle_cast({delete, MovieId}, State = #playlist_state{master = Player}) ->
  agner_playlist:delete(MovieId),
  Player ! {deleted, MovieId},
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

handle_cast({subscribe_master, NewPlayer}, State = #playlist_state{master = CurrentPlayer}) ->
  lager:info("Master player connected: ~p", [NewPlayer]),
  maybe_detach_player(CurrentPlayer),
  NewState = State#playlist_state{master = NewPlayer},
  {noreply, NewState};

handle_cast({subscribe_slave, Player}, State = #playlist_state{slaves = Slaves, current_song = MovieId}) ->
  lager:info("Slave player connected: ~p", [Player]),
  NewState = State#playlist_state{slaves = [Player | Slaves]},
  Player ! {play, MovieId, enforced},
  {noreply, NewState};

handle_cast({unsubscribe_slave, Player}, State = #playlist_state{slaves = Slaves}) ->
  lager:info("Unsubscribing slave: ~p", [Player]),
  NewState = State#playlist_state{slaves = lists:filter(fun(Slave) -> Slave =/= Player end, Slaves)},
  {noreply, NewState};

handle_cast({question, now, MessageId}, State = #playlist_state{master = Player}) ->
  lager:info("~p ! {now, ~s}", [Player, MessageId]),
  Player ! {now, MessageId},
  {noreply, State};

handle_cast({question, get_volume, MessageId}, State = #playlist_state{master = Player}) ->
  Player ! {get_volume, MessageId},
  {noreply, State};

handle_cast({play, MovieId}, State) ->
  notify_players({play, MovieId, enforced}, State),
  {noreply, State}.

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_info(Msg, State) ->
  lager:warning("Unexpected message: ~w~n", [Msg]),
  {noreply, State}.

maybe_detach_player(Player) when is_pid(Player) ->
  Player ! detach,
  ok;
maybe_detach_player(undefined) ->
  ok.

notify_players(Message, #playlist_state{master = Master, slaves = Slaves}) ->
  do_notify_players(Message, [Master | Slaves]).

do_notify_players(_, []) ->
  ok;
do_notify_players(Message, [Player | Rest]) ->
  lager:info("Message: ~p ! ~p", [Player, Message]),
  Player ! Message,
  do_notify_players(Message, Rest).

terminate(_, _) ->
  ok.

code_change(_, _, _) ->
  ok.

trim_message(Message, Length) ->
  list_to_binary(lists:sublist(binary_to_list(Message), Length)).
