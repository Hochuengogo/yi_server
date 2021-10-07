%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 网关客户端Rpc
%%% @end
%%% Created : 2021-10-06 19:09:25
%%%-------------------------------------------------------------------
-module(t_gateway_client_rpc).
-author("jiaoyinyi").

%% API
-export([handle/3]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

%% 获取服务器角色信息
handle(10000, {?true, _Msg, []}, Client) -> %% 该账户没有角色
    t_gateway_client:pack_send(10100, []),
    {ok, Client#gateway_client{is_login = true}};
handle(10000, {?true, _Msg, [{RId, SrvId, _Name, _Lev, _Sex, _Career} | _]}, Client) -> %% 该账户有角色
    t_gateway_client:pack_send(10101, {RId, SrvId}),
    {ok, Client#gateway_client{is_login = true}};
handle(10000, {?false, _Msg, _RoleInfo}, _Client) -> %% 账号登录失败
    {stop, account_login_fail};

%% 请求服务器时间信息
handle(10001, {TimeStamp}, _Client) ->
    put(srv_timestamp, TimeStamp),
    erlang:send_after(10000, self(), heartbeat),
    ok;

%% 注册角色
handle(10100, {?true, RId, SrvId}, _Client) ->
    t_gateway_client:pack_send(10101, {RId, SrvId});
handle(10100, {?false, _RId, _SrvId}, _Client) ->
    {stop, register_fail};

%% 登录角色
handle(10101, {?true, _RId, _SrvId}, _Client) ->
    ok;
handle(10101, {?false, _RId, _SrvId}, _Client) ->
    {stop, login_fail};

handle(Code, Data, _Client) ->
    ?error("错误的rpc处理，code：~w，数据：~w", [Code, Data]),
    {error, {bad_handle, Code}}.
