-module(agner_playlist).

-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2, gc/1]).

-export([add/3, delete/1, get/0]).

-define(GC_INTERVAL, 1000).

-include("agner_playlist.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  mnesia:create_schema([node()]),
  mnesia:start(),

  ensure_mnesia_running(),
  ensure_mnesia_dir(),

  try
    mnesia:table_info(song, type)
  catch
    exit: _ ->
      {atomic, ok} = mnesia:create_table(song, [{attributes, record_info(fields, song)},
        {disc_copies, [node()]}])
  end,

  erlang:send_after(?GC_INTERVAL, self(), start_expiring_timer),

  {ok, []}.

ensure_mnesia_running() ->
  case mnesia:system_info(is_running) of
    yes ->
      ok;
    starting ->
      timer:sleep(1000),
      ensure_mnesia_running();
    Reason when Reason =:= no; Reason =:= stopping ->
      throw({error, mnesia_not_running})
  end.

ensure_mnesia_dir() ->
  MnesiaDir = mnesia:system_info(directory) ++ "/",
  case filelib:ensure_dir(MnesiaDir) of
    {error, Reason} ->
      throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
    ok ->
      ok
  end.

add(MovieId, Title, AuthorId) ->
  gen_server:cast(?MODULE, {add, MovieId, Title, AuthorId}).

delete(MovieId) ->
  gen_server:cast(?MODULE, {delete, MovieId}).

get() ->
  gen_server:call(?MODULE, {get_random}).

handle_cast({add, MovieId, Title, AuthorId}, RecentMovies) ->
  mnesia:wait_for_tables([song], 1000),

  Fun = fun() ->
    mnesia:write(#song{movie_id = MovieId, title = Title, author_id = AuthorId})
        end,
  {atomic, ok} = mnesia:transaction(Fun),
  {noreply, RecentMovies};

handle_cast({delete, MovieId}, RecentMovies) ->
  mnesia:wait_for_tables([song], 1000),

  Fun = fun() ->
    mnesia:delete({song, MovieId})
        end,
  {atomic, ok} = mnesia:transaction(Fun),
  {noreply, RecentMovies}.

handle_call({get_random}, _From, RecentMovies) ->
  RandomMovieId = randomize(RecentMovies),
  NewState = lists:append([{RandomMovieId, calculate_ttl()}], RecentMovies),
  {reply, {ok, RandomMovieId}, NewState}.

randomize(RecentMovies) ->
  {ok, MovieId} = get_random_movie(),
  case lists:keymember(MovieId, 1, RecentMovies) of
    true -> randomize(RecentMovies);
    false -> MovieId
  end.

expire_items(ExpiringItems) ->
  [ExpiringItem || ExpiringItem = {_Item, ExpiresAt} <- ExpiringItems, os:system_time() < ExpiresAt].

calculate_ttl() ->
  os:system_time() + get_songs_count() * 120 * 1000.

get_random_movie() ->
  error_logger:info_msg("mnesia:get_random_movie"),
  mnesia:wait_for_tables([song], 1000),

  Keys = mnesia:dirty_all_keys(song),
  get_random_movie(Keys).

get_random_movie([]) ->
  {error, no_songs};
get_random_movie(Keys) ->
  Key = lists:nth(rand:uniform(length(Keys)), Keys),
  [#song{movie_id = MovieId}] = mnesia:dirty_read({song, Key}),
  {ok, MovieId}.

get_songs_count() ->
  length(mnesia:dirty_all_keys(song)).

handle_info(start_expiring_timer, RecentMovies) ->
  spawn_gc(),
  {noreply, RecentMovies};

handle_info({expire}, RecentMovies) ->
  {noreply, expire_items(RecentMovies)}.

code_change(_, _, _) ->
  ok.

terminate(_, _) ->
  ok.

spawn_gc() ->
  spawn(?MODULE, gc, [self()]).

gc(Playlist) ->
  receive
  after 60000 ->
    Playlist ! {expire},
    gc(Playlist)
  end.
