-module(agner_playlist).

-export([start/1]).
-export([playlist/2]).
-export([get/0]).
-export([add/3]).
-export([next/0]).

start(Subscriber) ->
  case whereis(playlist) of
    undefined -> ok;
    Pid ->
      unregister(playlist),
      exit(Pid, kill)
  end,

  Queue = queue:new(),
  register(playlist, spawn(?MODULE, playlist, [Queue, Subscriber])).

playlist(Queue, Subscriber) ->
  receive
    {From, get} ->
      case queue:is_empty(Queue) of
        true ->
          {ok, MovieId} = agner_mnesia:get_random_movie(),
          From ! {ok, MovieId, random},
          playlist(Queue, Subscriber);
        false ->
          {{value, Item}, Q2} = queue:out(Queue),
          From ! {ok, Item, queue},
          playlist(Q2, Subscriber)
      end;
    {From, {add, MovieId, Title, User}} ->
      From ! ok,
      agner_mnesia:add_movie(MovieId, Title, User),
      case queue:is_empty(Queue) of
        true ->
          dispatch(Subscriber, added_to_empty_queue);
        false ->
          ok
      end,
      playlist(queue:in(MovieId, Queue), Subscriber);
    {From, next} ->
      From ! ok,
      dispatch(Subscriber, next),
      playlist(Queue, Subscriber)
  end.

dispatch(Subscriber, Event) ->
  error_logger:info_msg("Sending '~s' event to pid: ~s", [Subscriber, Event]),
  Subscriber ! Event.

get() ->
  playlist ! {self(), get},
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
  playlist ! {self(), next},
  receive
    ok -> ok
  after 1000 ->
    timeout
  end.
