-module(qri_emitter).

-export([init/2, info/3, terminate/3]).


init(Req, Opts) ->
    PID = qri_peer:register(qri_peer:parse_qs(Req), self()),

    Headers = [
        {<<"content-type">>, <<"text/event-stream">>},
        {<<"Cache-Control">>, <<"no-cache">>}
    ],
    Req2 = cowboy_req:chunked_reply(200, Headers, Req),

    PID ! {message, "Tick"},

    {cowboy_loop, Req2, Opts}.

info({message, Msg}, Req, State) ->
    PID = qri_peer:get_pid(qri_peer:parse_qs(Req)),
    Chunk = [
        "id: ", qri_peer:generate_tick_id(),
        "\ndata: ", Msg,
        "\n\n"
    ],

    case PID of
        undefined ->
            {stop, Req, State};

        _Else ->
            cowboy_req:chunk(Chunk, Req),
            Message = io_lib:format("Response ~p", [PID]),
            erlang:send_after(2000, PID, {message, Message}),
            {ok, Req, State}
    end.

terminate(_Reason, Req, _State) ->
    qri_peer:remove(qri_peer:parse_qs(Req)),
    ok.
