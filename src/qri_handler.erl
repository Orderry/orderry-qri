-module(qri_handler).

-export([init/2, info/3, terminate/3]).


init(Req, Opts) ->
    PID = self(),
    qri_peer:register(qri_peer:parse_qs(Req), PID),

    Headers = [
        {<<"content-type">>, <<"text/event-stream">>},
        {<<"Cache-Control">>, <<"no-cache">>},
        {<<"access-control-allow-origin">>, <<"*">>},
        {<<"access-control-allow-methods">>, <<"*">>}
    ],
    Req2 = cowboy_req:chunked_reply(200, Headers, Req),

    PID ! {message, "Tick"},

    {cowboy_loop, Req2, Opts}.

info({message, Msg}, Req, State) ->
    Chunk = [
        "id: ", qri_peer:generate_tick_id(),
        "\ndata: ", Msg,
        "\n\n"
    ],

    cowboy_req:chunk(Chunk, Req),

    {ok, Req, State}.

terminate(_Reason, Req, _State) ->
    qri_peer:remove(qri_peer:parse_qs(Req), self()),
    ok.
