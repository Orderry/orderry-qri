-module(qri_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-define(C_ACCEPTORS, 100).

get_port() ->
    {_, Port} = application:get_env(http_port),
    Port.

start() ->
    lager:start(),

    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(qri).

start(_StartType, _StartArgs) ->
    Port = get_port(),
    Routes = routing:routes(),
    Dispatch = cowboy_router:compile(Routes),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],

    cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    lager:info("Starting at port ~p", [Port]),

    qri_sup:start_link().

stop(_State) ->
    ok.
