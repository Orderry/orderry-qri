-module(backdoor_handler).

-export([
  init/2,
  allowed_methods/2,
  content_types_provided/2, to_json/2,
  content_types_accepted/2, from_plain/2,
  delete_resource/2
]).


init(Req, State) ->
  Headers = #{
    <<"Access-Control-Allow-Origin">> => <<"*">>,
    <<"Access-Control-Allow-Headers">> => <<"Content-Type, Access-Control-Allow-Headers">>,
    <<"Access-Control-Allow-Methods">> => <<"DELETE">>
  },
  Req1 = maps:fold(fun(K, V, AccR) -> cowboy_req:set_resp_header(K, V, AccR) end, Req, Headers),
  {cowboy_rest, Req1, State}.

allowed_methods(Req, State) ->
  {[<<"OPTIONS">>, <<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{ <<"text">>, <<"html">>, '*'}, to_json},
    {{ <<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{'*', from_plain}], Req, State}.

to_json(Req, State) ->
  #{type := Type} = cowboy_req:match_qs([type], Req),
  Resp = #{<<"type">> => Type},
  Resp1 =
    case herald:call({get, Type}) of
      false ->
        Resp#{<<"message">> => null};
      <<Type:3/binary, Message/binary>> ->
        Resp#{<<"message">> => Message}
    end,
  {jsone:encode(Resp1), Req, State}.
from_plain(Req, State) ->
  {ok, KeyValues, Req1} = cowboy_req:body_qs(Req),
  #{<<"type">> := Type, <<"message">> := Message} = maps:from_list(KeyValues),
  ok = herald:call({set, Type, Message}),
  {true, Req1, State}.

delete_resource(Req, State) ->
  {ok, KeyValues, _} = cowboy_req:body_qs(Req),
  #{<<"type">> := Type} = maps:from_list(KeyValues),
  ok = herald:call({delete, Type}),
  {true, Req, State}.