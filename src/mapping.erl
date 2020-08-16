%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 协议映射
%%%
%%% @end
%%% Created : 25. 7月 2020 10:47 下午
%%%-------------------------------------------------------------------
-module(mapping).
-author("huangzaoyi").

%% API
-export([do/1]).

-include("logs.hrl").

%% @doc 协议号映射
-spec do(pos_integer()) -> {ok, atom(), atom()} | {error, {bad_mapping, pos_integer()}}.
do(Code) ->
    case do_do(Code div 100) of
        {ok, ProtoMod, RpcMod} ->
            {ok, ProtoMod, RpcMod};
        {error, _Err} ->
            ?error("协议号映射错误, 协议号:~w", [Code]),
            {error, _Err}
    end.

do_do(100) ->
    {ok, proto_100, test_rpc};
do_do(_Code) ->
    {error, {bad_mapping, _Code}}.