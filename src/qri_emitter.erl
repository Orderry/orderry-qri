-module(qri_emitter).

-export([init/2, terminate/3]).

init(Req, Opts) ->
    Headers = [{<<"content-type">>, <<"text/plain">>}],

    Req2 = cowboy_req:reply(200, Headers, <<"">>, Req),
    {ok, Req2, Opts}.

terminate(_Reason, _Req, _State) ->
    ok.
