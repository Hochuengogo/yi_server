%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 网关RPC
%%% @end
%%% Created : 2021-08-15 23:29:14
%%%-------------------------------------------------------------------
-module(gateway_rpc).
-author("jiaoyinyi").

%% API
-export([handle/3]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

%% 账号登录
handle(10000, _Data, #gateway{is_login = true}) ->
    ok;
handle(10000, Data, Gateway = #gateway{is_login = false}) ->
    case gateway_handle:account_login(Gateway, Data) of
        {ok, NewGateway, RoleInfo} ->
            {reply, {?true, <<>>, RoleInfo}, NewGateway};
        {false, Msg} ->
            {reply, {?false, Msg, []}}
    end;

%% 请求服务器时间信息
handle(10001, {}, _Gateway) ->
    Now = time_util:timestamp(),
    put(last_heartbeat, Now),
    {reply, {Now}};

handle(Code, Data, _Gateway) ->
    ?error("错误的rpc处理，code：~w，数据：~w", [Code, Data]),
    {error, {bad_handle, Code}}.
