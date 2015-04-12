-module(qri_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-define(C_ACCEPTORS, 100).

get_port() ->
    {_, Port} = application:get_env(http_port),
    Port.

get_routes() ->
    [
        {'_', [
            {"/stream", qri_emitter, []}
        ]}
    ].

start() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(qri).

start(_StartType, _StartArgs) ->
    Port = get_port(),
    Routes = get_routes(),
    Dispatch = cowboy_router:compile(Routes),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],

    % Creating ETS table named peers for further mapping: Peer => PID.
    ets:new(peers, [set, public, named_table, {keypos, 1}]),

    cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    io:format("Starting at port ~p\n", [Port]),

    qri_sup:start_link().

stop(_State) ->
    ok.
