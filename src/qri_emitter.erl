-module(qri_emitter).

-export([init/2, info/3, terminate/2]).

init(Req, Opts) ->
    PID = self(),
    Peer = qri_peer:parse_id(Req),
    Checksum = 1,
    qri_peer:register({Peer, Checksum}, PID),

    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    Req2 = cowboy_req:chunked_reply(200, Headers, Req),

    PID ! {message, "Tick"},

    {cowboy_loop, Req2, Opts, 5000}.

info({message, Msg}, Req, State) ->
    Peer = qri_peer:parse_id(Req),
    Checksum = 1,
    PID = qri_peer:get_pid({Peer, Checksum}),

    Chunk = [
        "id: ", qri_peer:generate_tick_id(),
        "\ndata: ", Msg,
        "\n\n"
    ],

    cowboy_req:chunk(Chunk, Req),

    Message = io_lib:format("Response ~p", [PID]),
    erlang:send_after(1000, PID, {message, Message}),

    % erlang:send_after(2000, self(), {message, "Tick"}),
    {ok, Req, State}.

terminate(_, _) ->
    ok.
