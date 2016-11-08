-module(qri_peer).

-export([generate_tick_id/0]).
-export([parse_qs/1]).
-export([get_pids/1]).
-export([remove/2]).
-export([register/2]).

-define(C_KEY, "p").
-define(C_CHECKSUM, "s").
-define(C_POS_PEER, 1).
-define(C_POS_CHECKSUM, 2).
-define(C_POS_PID, 3).


is_valid_checksum(_Checksum, undefined) -> false;
is_valid_checksum(Checksum, Element) ->
    % Element should be a tuple of {Peer, Checksum, PID}.
    Checksum =:= element(?C_POS_CHECKSUM, Element).

generate_tick_id() ->
    {Mega, Sec, Micro} = erlang:now(),
    Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
    integer_to_list(Id, 16).

parse_qs(Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    Key = lists:keyfind(<<?C_KEY>>, 1, QsVals),
    Checksum = lists:keyfind(<<?C_CHECKSUM>>, 1, QsVals),

    if
        Key == false ->
            undefined;

        Checksum == false ->
            undefined;

        (Key /= false) and (Checksum /= false)->
            %% Get values of the query string.
            %% p=123&s=uniq => {"123", "uniq"}
            {element(2, Key), element(2, Checksum)}
    end.

%% Return PID which is assigned to the given Peer, or returns undefined if Peer
%%  does not exists or has an invalid Checksum.
get_pids(undefined) -> [];
get_pids('ALL_PEERS') ->
    case ets:select(peers, [{{'$1','$2','$3'},[],['$3']}]) of
        [] ->
            [];
        ListOfSomething when is_list(ListOfSomething) ->
            lists:flatten(ListOfSomething)
    end;
get_pids({Peer, Checksum}) ->
    Elements = ets:lookup(peers, Peer),

    case Elements of
        [] ->
            [];

        [Element] ->
            get_pids({Peer, Checksum}, Element)
    end.
get_pids({_Peer, Checksum}, Element) ->
    IsValidChecksum = is_valid_checksum(Checksum, Element),

    case IsValidChecksum of
        false ->
            [];

        true ->
            element(?C_POS_PID, Element)
    end.

remove(undefined, _PID) -> ok;
remove({Peer, Checksum}, PID) ->
    Stored = get_pids({Peer, Checksum}),
    Updated = lists:delete(PID, Stored),
    case Updated of
        [] ->
            ets:delete(peers, Peer);

        _Else ->
            ets:insert(peers, {Peer, Checksum, Updated})
    end.

register(undefined, PID) -> [PID];
register({Peer, Checksum}, PID) ->
    Stored = get_pids({Peer, Checksum}),
    Updated = lists:append(Stored, [PID]),
    ets:insert(peers, {Peer, Checksum, Updated}),
    Updated.
