-module(mnesia_schema_transformer).
%%
%%-export([transform/0]).
%%
%%-include("agner_playlist.hrl").
%%
%%transform() ->
%%  Transformer =
%%    fun(X) ->
%%      #song{
%%        movie_id = X#new_song.movie_id,
%%        title = X#new_song.movie_id,
%%        author_id = X#new_song.author_id}
%%    end,
%%
%%
%%  {atomic, ok} = mnesia:transform_table(new_song, Transformer, record_info(fields, song), song),
%%
%%  erlang:display(<<"DONE!!">>),
%%
%%  erlang:exit(done),