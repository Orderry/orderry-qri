-module(herald).
-author("Bohdan Korniyenko <bohdan.korniyenko@gmail.com>").
-behavior(gen_server).

%% API
-export([start_link/0, call/1]).

-export([
  init/1,
  handle_info/2,
  handle_call/3
]).

-define(BROADCAST_TIMEOUT, 30000).

%% public

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

call(M) ->
  gen_server:call(?MODULE, M).

%% callbacks

init(_Args) ->
  {ok, maps:new()}.

handle_info(broadcast, State) when map_size(State) == 0 ->
  {noreply, State};
handle_info(broadcast, State) ->
  Self = self(),
  spawn_link(fun() ->
    AllPids = qri_peer:get_pids(all_peers),
    lists:foreach(fun(Message) ->
      [Pid ! {message, Message} || Pid <- AllPids]
    end, maps:values(State)),
    erlang:send_after(?BROADCAST_TIMEOUT, Self, broadcast)
  end),
  {noreply, State}.

handle_call({get, Type}, _From, State) ->
  {reply, maps:get(Type, State, false), State};

handle_call({set, Type, Message}, _From, State) when map_size(State) == 0 ->
  self() ! broadcast,
  {reply, ok, set(Type, Message, State)};
handle_call({set, Type, Message}, _From, State) ->
  {reply, ok, set(Type, Message, State)};

handle_call({delete, Type}, _From, State) ->
  AllPids = qri_peer:get_pids(all_peers),
  [Pid ! {message, <<Type/binary, "delete">>} || Pid <- AllPids],
  {reply, ok, maps:remove(Type, State)}.

%% private

set(Type, Message, State) ->
  Message1 = <<Type/binary, Message/binary>>,
  maps:put(Type, Message1, State).