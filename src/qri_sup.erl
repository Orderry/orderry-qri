-module(qri_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(ETS_OPTIONS, [set, public, named_table, {keypos, 1}]).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).


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

listen() ->
    {ok, SPort} = application:get_env(socket_port),

    % Listen socket connection and push notifications to the relative PIDs.
    spawn_link(fun() ->
        {ok, LSock} = gen_tcp:listen(SPort, ?TCP_OPTIONS),
        accept(LSock)
    end).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Creating ETS table named peers for further mapping: Peer => PID.
    ets:new(peers, ?ETS_OPTIONS),

    listen(),

    {ok, { {one_for_one, 2, 5}, []} }.
