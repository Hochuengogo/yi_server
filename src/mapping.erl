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
-spec do(pos_integer()) -> {ok, atom(), atom(), atom(), boolean()} | {error, {bad_mapping, pos_integer()}}.
do(Code) ->
    case do_do(Code div 100) of
        {ok, Parser, ProtoMod, RpcMod, IsLogin} ->
            {ok, Parser, ProtoMod, RpcMod, IsLogin};
        {error, Err} ->
            ?error("协议号映射错误, 协议号:~w", [Code]),
            {error, Err}
    end.

do_do(100) -> {ok, gateway, proto_100, gateway_rpc, false};
do_do(101) -> {ok, gateway, proto_101, login_rpc, true};
do_do(102) -> {ok, role, proto_102, notice_rpc, true};
do_do(Code) -> {error, {bad_mapping, Code}}.