%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色库模块
%%% @end
%%% Created : 2021-09-07 19:48:01
%%%-------------------------------------------------------------------
-module(role_lib).
-author("jiaoyinyi").

%% API
-export([
    check_name/2
]).

-include("common.hrl").
-include("logs.hrl").

%% @doc 判断名字
-spec check_name(binary(), role_id()) -> true | {false, msg()}.
check_name(Name, RoleId) ->
    case check_name_valid(Name) of
        true ->
            check_name_used(Name, RoleId);
        {false, Msg} ->
            {false, Msg}
    end.

%% 判断名字是否合法
check_name_valid(Name) ->
    true.

%% 判断名字是否已使用
check_name_used(Name, RoleId) ->
    true.