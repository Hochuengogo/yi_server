%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色下线模块
%%% @end
%%% Created : 2021-10-06 18:01:12
%%%-------------------------------------------------------------------
-module(role_logout).
-author("jiaoyinyi").

%% API
-export([
    do/1
]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").

%% 下线执行列表
-define(logout_list, [

]).

do(Role) ->
    do(?logout_list, Role).
do([], Role) ->
    {ok, Role};
do([Do = {M, F, A} | List], Role) ->
    case catch util:apply(M, F, [Role | A]) of
        NRole = #role{} ->
            do(List, NRole);
        {ok, NRole = #role{}} ->
            do(List, NRole);
        ok ->
            do(List, Role);
        Err ->
            ?error("角色[~ts]~p下线失败，执行操作：~w，错误：~w", [Role#role.name, Role#role.id, Do, Err]),
            do(List, Role)
    end;
do([Do | List], Role) ->
    ?error("角色[~ts]~p下线失败，未匹配的执行操作：~w", [Role#role.name, Role#role.id, Do]),
    do(List, Role).