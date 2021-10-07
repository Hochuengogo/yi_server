%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色登录模块
%%% @end
%%% Created : 2021-09-26 23:35:17
%%%-------------------------------------------------------------------
-module(role_login).
-author("jiaoyinyi").

%% API
-export([
    do/1
]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").

%% 登录执行列表
-define(login_list, [

]).

do(Role) ->
    do(?login_list, Role).
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
            ?error("角色[~ts]~p登录失败，执行操作：~w，错误：~w", [Role#role.name, Role#role.id, Do, Err]),
            {error, Err}
    end;
do([Do | _List], Role) ->
    ?error("角色[~ts]~p登录失败，未匹配的执行操作：~w", [Role#role.name, Role#role.id, Do]),
    {error, {bad_login_func, Do}}.
