%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色数据转换
%%% @end
%%% Created : 2021-09-18 23:51:17
%%%-------------------------------------------------------------------
-module(role_convert).
-author("jiaoyinyi").

%% API
-export([
    do/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").

%% @doc 角色数据转换其他数据结构
-spec do(atom(), #role{}) -> {ok, term()} | false.
do(save_role, Role = #role{}) ->
    SaveRole =
        Role#role{
            pid = undefined
            , m_gateway = undefined
        },
    {ok, SaveRole};
do(Flag, Role) ->
    ?error("角色数据转换失败，标识：~w，数据：~w", [Flag, Role]),
    false.