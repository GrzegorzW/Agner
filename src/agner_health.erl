-module(agner_health).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
  Res = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"ok">>,
    Req),
  {ok, Res, State}.
