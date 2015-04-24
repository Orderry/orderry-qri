-module(qri_emitter).

-export([init/2, info/3, terminate/3]).

init(Req, _Opts) ->
    PID = self(),
    Registered = qri_peer:register(qri_peer:parse_qs(Req), PID),

    Headers = [
        {<<"content-type">>, <<"text/event-stream">>},
        {<<"Cache-Control">>, <<"no-cache">>}
    ],
    Req2 = cowboy_req:chunked_reply(200, Headers, Req),

    PID ! {message, "Tick"},

    {cowboy_loop, Req2, [Registered]}.

info({message, Msg}, Req, State) ->
    [PIDs] = State,
    Chunk = [
        "id: ", qri_peer:generate_tick_id(),
        "\ndata: ", Msg,
        "\n\n"
    ],

    cowboy_req:chunk(Chunk, Req),

    % Testing...
    % Message = io_lib:format("Response ~p", [PIDs]),
    % [erlang:send_after(2000, PID, {message, Message}) || PID <- PIDs],

    {ok, Req, State}.

terminate(_Reason, Req, _State) ->
    qri_peer:remove(qri_peer:parse_qs(Req)),
    ok.
