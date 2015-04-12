-module(qri_peer).

-export([
    generate_tick_id/0,
    parse_id/1,
    has_peer/1,
    get_pid/1,
    remove/1,
    register/2
]).

-define(C_PEER, "peer").

is_valid_checksum(Checksum, Element) ->
    % Element should be a tuple of {Peer, PID, Checksum}.
    Checksum =:= element(3, Element).

generate_tick_id() ->
    {Mega, Sec, Micro} = erlang:now(),
    Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_list(Id, 16).

parse_id(Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    Tuple = lists:keyfind(<<?C_PEER>>, 1, QsVals),

    case Tuple of
        false ->
            undefined;

        {_, Peer} ->
            Peer
    end.

has_peer({Peer, Checksum}) ->
    {peer, {Peer, Checksum}}.

%% Return PID which is assigned to the given Peer, or returns undefined if Peer
%%  does not exists or has an invalid Checksum.
get_pid({Peer, Checksum}) ->
    [Element] = ets:lookup(peers, Peer),
    IsValidChecksum = is_valid_checksum(Checksum, Element),

    case IsValidChecksum of
        true ->
            element(2, Element);

        false ->
            undefined

    end.

remove({Peer, Checksum}) ->
    {peer, {Peer, Checksum}}.

register({Peer, Checksum}, PID) ->
    ets:insert(peers, {Peer, PID, Checksum}).
