-module(agner_health).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"ok">>,
    Req0),
  {ok, Req, State}.
