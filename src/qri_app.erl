-module(qri_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-define(C_ACCEPTORS, 5000).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(ROUTES, [
    {'_', [
        {"/stream", qri_emitter, []}
    ]}
]).


http_port() ->
    {ok, Port} = application:get_env(http_port),
    Port.

socket_port() ->
    {ok, Port} = application:get_env(socket_port),
    Port.

start() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(qri).

start(_StartType, _StartArgs) ->
    Port = http_port(),
    Dispatch = cowboy_router:compile(?ROUTES),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],

    % Creating ETS table named peers for further mapping: Peer => PID.
    ets:new(peers, [set, public, named_table, {keypos, 1}]),

    cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    io:format("Starting at port ~p\n", [Port]),

    qri_sup:start_link(),

    listen().

stop(_State) ->
    ok.

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % Testing data.
            PIDs = qri_peer:get_pids({<<"123">>, 2286445522}),
            [PID ! {message, Data} || PID <- PIDs],
            io:format(">>> Data: ~p ~p\n", [Data, PIDs]),
            loop(Socket);
        {error, closed} ->
            ok
    end.

accept(LSock) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    spawn(fun() -> loop(Socket) end),
    accept(LSock).

% Listen socket connection and push notifications to the relative PIDs.
listen() ->
    {ok, LSock} = gen_tcp:listen(socket_port(), ?TCP_OPTIONS),
    accept(LSock).
