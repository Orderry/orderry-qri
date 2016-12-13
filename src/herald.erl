%% -----------------------------------------------------------------
%% Процесс, рассылающий системные, служебные уведомления всем
%% пользователям, находяшимся в данный момент онлайн.
%% Позволяет добавлять и удалять собщения, хранящиеся в словаре и
%% регулярно рассылающиеся.
%% -----------------------------------------------------------------

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
  State = maps:new(),
  {ok, State}.

handle_info(broadcast, State) when map_size(State) == 0 ->
  {noreply, State};
handle_info(broadcast, State) ->
  Self = self(),
  % Spawn a broadcaster worker
  spawn_link(fun() ->
    %% Get all online peers
    Pids = qri_peer:get_all_peers_pids(),

    %% The messages to be send to each peer
    Messages = maps:values(State),

    lists:foreach(fun(Pid) ->
      send_message(Pid, Messages)
    end, Pids),

    %% Broadcast again later
    erlang:send_after(?BROADCAST_TIMEOUT, Self, broadcast)
  end),
  {noreply, State}.


%% Set a message, notify the peers if there are any
handle_call({set, Type, Message}, _From, State) when map_size(State) == 0 ->
  self() ! broadcast,
  {reply, ok, set(Type, Message, State)};
handle_call({set, Type, Message}, _From, State) ->
  {reply, ok, set(Type, Message, State)};

%% Get a message by key
handle_call({get, Type}, _From, State) ->
  {reply, maps:get(Type, State, false), State};

%% Delete a message, and notify all the peers
handle_call({delete, Type}, _From, State) ->
  Pids = qri_peer:get_all_peers_pids(),
  MagicMessage = <<Type/binary, "delete">>,
  lists:foreach(fun(Pid) ->
    send_message(Pid, [MagicMessage])
  end, Pids),
  {reply, ok, maps:remove(Type, State)}.

%% private

set(Type, Message, State) ->
  Message1 = <<Type/binary, Message/binary>>,
  maps:put(Type, Message1, State).

send_message(_Pid, []) ->
  ok;
send_message(Pid, [Msg | MsgRest]) ->
  Pid ! {message, Msg},
  send_message(Pid, MsgRest).