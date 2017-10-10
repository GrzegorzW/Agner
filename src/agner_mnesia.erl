-module(agner_mnesia).

-export([start/0, add_movie/3, get_random_movie/0]).

-include("agner_playlist.hrl").

start() ->
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
  end.

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

add_movie(MovieId, Title, AuthorId) ->
  mnesia:wait_for_tables([song], 1000),

  Fun = fun() ->
    mnesia:write(#song{movie_id = MovieId, title = Title, author_id = AuthorId})
        end,
  {atomic, ok} = mnesia:transaction(Fun).

get_random_movie() ->
  Keys = mnesia:dirty_all_keys(song),
  get_random_movie(Keys).

%%
%%todo do it in efficient way
%%
get_random_movie([]) ->
  {error, no_songs};
get_random_movie(Keys) ->
  Key = lists:nth(rand:uniform(length(Keys)), Keys),
  [#song{movie_id = MovieId}] = mnesia:dirty_read({song, Key}),
  {ok, MovieId}.
