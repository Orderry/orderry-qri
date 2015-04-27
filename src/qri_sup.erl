-module(qri_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%% ===================================================================
%% API functions
%% ===================================================================

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

start_link() ->
    {ok, SPort} = application:get_env(socket_port),

    % Listen socket connection and push notifications to the relative PIDs.
    spawn_link(fun() ->
        {ok, LSock} = gen_tcp:listen(SPort, ?TCP_OPTIONS),
        accept(LSock)
    end),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 2, 5}, []} }.
