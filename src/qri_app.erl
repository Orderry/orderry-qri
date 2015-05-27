-module(qri_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-define(C_ACCEPTORS, 5000).
-define(ROUTES, [
    {'_', [
        {"/stream", qri_handler, []}
    ]}
]).

start() ->
    application:start(raven),
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(qri).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(http_port),

    Dispatch = cowboy_router:compile(?ROUTES),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],

    cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    io:format("Starting at port ~p\n", [Port]),

    qri_sup:start_link().

stop(_State) ->
    ok.
