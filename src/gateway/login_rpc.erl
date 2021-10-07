%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色注册、登录Rpc
%%% @end
%%% Created : 2021-08-17 22:37:44
%%%-------------------------------------------------------------------
-module(login_rpc).
-author("jiaoyinyi").

%% API
-export([handle/3]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

%% 注册角色
handle(10100, _Data, Gateway = #gateway{role_id = RoleId}) when is_pid(RoleId) ->
    {stop, normal, Gateway};
handle(10100, Data, Gateway) ->
    case gateway_handle:register(Gateway, Data) of
        {ok, RId, SrvId} ->
            {reply, {?true, RId, SrvId}};
        {false, Msg} ->
            notice:error(Msg),
            {reply, {?false, 0, <<>>}}
    end;

%% 登录角色
handle(10101, {_RId, _SrvId}, Gateway = #gateway{role_id = RoleId}) when is_pid(RoleId) ->
    {stop, normal, Gateway};
handle(10101, {RId, SrvId}, Gateway) ->
    case gateway_handle:login(Gateway, {RId, SrvId}) of
        {ok, NewGateway} ->
            {reply, {?true, RId, SrvId}, NewGateway};
        {false, Msg} ->
            notice:error(Msg),
            {reply, {?false, 0, <<>>}}
    end;

handle(Code, Data, _Role) ->
    ?error("错误的rpc处理，code：~w，数据：~w", [Code, Data]),
    {error, {bad_handle, Code}}.
