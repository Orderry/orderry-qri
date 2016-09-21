-module(qri_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([ticks_emitter/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(ETS_OPTIONS, [set, public, named_table, {keypos, 1}]).
-define(TCP_OPTIONS, [binary, {packet, asn1}, {active, false}, {reuseaddr, true}]).
-define(TICK_INTERVAL, 30).


send_peers_ticks([]) -> ok;
send_peers_ticks([PeersPIDs|Rest]) ->
    [PID ! {message, "Tick"} || PID <- PeersPIDs],
    send_peers_ticks(Rest).

ticks_emitter() ->
    % Example: [[PID11, ..., PID1N], ..., [PIDM1, ..., PIDML]],
    % where PID11, ..., PID1N - PIDs of first peer,
    % PIDM1, ..., PIDML - another peers PIDs.
    PIDsByPeers = ets:select(peers, [{{'_','_', '$3'}, [], ['$3']}]),
    send_peers_ticks(PIDsByPeers).

emitter([Peer, Checksum, Message]) ->
    PIDs = qri_peer:get_pids({Peer, Checksum}),
    [PID ! {message, Message} || PID <- PIDs].

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {ok, {'Message', Peer, Checksum, Message}} = 'Ber':decode('Message', Data),
            emitter([Peer, Checksum, Message]),
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

    timer:apply_interval(timer:seconds(?TICK_INTERVAL), qri_sup, ticks_emitter, []),

    {ok, { {one_for_one, 5, 10}, []} }.
