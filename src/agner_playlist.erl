-module(agner_playlist).

-export([start/0]).
-export([playlist/2]).
-export([get/0]).
-export([add/3]).
-export([next/0]).
-export([subscribe/1]).
-export([volume/1]).

start() ->
  Queue = queue:new(),
  Subscribers = [],
  register(playlist, spawn(?MODULE, playlist, [Queue, Subscribers])).

playlist(Queue, Subscribers) ->
  receive
    {From, {get}} ->
      case queue:is_empty(Queue) of
        true ->
          {ok, MovieId} = agner_mnesia:get_random_movie(),
          From ! {ok, MovieId, random},
          playlist(Queue, Subscribers);
        false ->
          {{value, Item}, Q2} = queue:out(Queue),
          From ! {ok, Item, queue},
          playlist(Q2, Subscribers)
      end;
    {From, {add, MovieId, Title, User}} ->
      From ! ok,
      agner_mnesia:add_movie(MovieId, Title, User),
      case queue:is_empty(Queue) of
        true ->
          dispatch(Subscribers, added_to_empty_queue);
        false ->
          ok
      end,
      playlist(queue:in(MovieId, Queue), Subscribers);
    {From, {next}} ->
      From ! ok,
      dispatch(Subscribers, next),
      playlist(Queue, Subscribers);
    {From, {volume, Level}} ->
      From ! ok,
      dispatch(Subscribers, {volume, Level}),
      playlist(Queue, Subscribers);
    {From, {subscribe, SubscriberPid}} ->
      From ! ok,
      detach(Subscribers),
      playlist(Queue, [SubscriberPid])
  end.

dispatch([SubscriberPid | Tail], Event) ->
  error_logger:info_msg("Sending '~s' event to pid: ~s", [SubscriberPid, Event]),
  SubscriberPid ! Event,
  dispatch(Tail, Event);
dispatch([], _EventType) ->
  ok.

subscribe(SubscriberPid) ->
  playlist ! {self(), {subscribe, SubscriberPid}},
  receive
    ok -> ok
  after 1000 ->
    timeout
  end.

detach([SubscriberPid | Tail]) ->
  SubscriberPid ! stop,
  detach(Tail);
detach([]) ->
  ok.

get() ->
  playlist ! {self(), {get}},
  receive
    {ok, MovieId, Source} ->
      {MovieId, Source}
  after 1000 ->
    timeout
  end.

add(MovieId, Title, User) ->
  playlist ! {self(), {add, MovieId, Title, User}},
  receive
    ok -> ok
  after 1000 ->
    timeout
  end.

next() ->
  playlist ! {self(), {next}},
  receive
    {ok} -> {ok}
  after 1000 ->
    timeout
  end.

volume(Level) ->
  playlist ! {self(), {volume, Level}},
  receive
    {ok} -> {ok}
  after 1000 ->
    timeout
  end.