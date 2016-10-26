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

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

call(M) ->
  gen_server:call(?MODULE, M).


-define(BROADCAST_TIMEOUT, 5000).

init(_Args) ->
  {ok, maps:new()}.

handle_info(broadcast, State) when map_size(State) == 0 ->
  {noreply, State};
handle_info(broadcast, State) ->
  AllPids = qri_peer:get_pids('ALL_PEERS'),
  lists:foreach(fun(Message) ->
    [Pid ! {message, Message} || Pid <- AllPids]
  end, maps:values(State)),
  erlang:send_after(?BROADCAST_TIMEOUT, self(), broadcast),
  {noreply, State}.

handle_call({get, Type}, _From, State) ->
  {reply, maps:get(Type, State, false), State};

handle_call({set, Type, Message}, _From, State) ->
  if map_size(State) == 0 ->
    self() ! broadcast;
    true -> okay end,
  Message1 = <<Type/binary, Message/binary>>,
  {reply, ok, maps:put(Type, Message1, State)};

handle_call({delete, Type}, _From, State) ->
  AllPids = qri_peer:get_pids('ALL_PEERS'),
  [Pid ! {message, <<Type/binary, "delete">>} || Pid <- AllPids],
  {reply, ok, maps:remove(Type, State)};

handle_call(_Comm, _From, State) ->
  {reply, ok, State}.