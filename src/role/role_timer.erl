%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色定时器
%%% @end
%%% Created : 2021-10-07 21:56:52
%%%-------------------------------------------------------------------
-module(role_timer).
-author("jiaoyinyi").

%% API
-export([
    add/2
    , del/2
    , set/2
    , tick/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").
-include("stimer.hrl").

%% @doc 增加定时器
-spec add(#role{}, #timer{}) -> #role{}.
add(Role = #role{s_timer = STimer}, Timer) ->
    NewSTimer = stimer:add(STimer, Timer),
    Role#role{s_timer = NewSTimer}.

%% @doc 删除定时器
-spec del(#role{}, term()) -> #role{}.
del(Role = #role{s_timer = STimer}, Id) ->
    NewSTimer = stimer:del(STimer, Id),
    Role#role{s_timer = NewSTimer}.

%% @doc 设置定时器
-spec set(#role{}, #timer{}) -> #role{}.
set(Role = #role{s_timer = STimer}, Timer) ->
    NewSTimer = stimer:del(STimer, Timer),
    Role#role{s_timer = NewSTimer}.

%% @doc 定时器触发
-spec tick(#role{}, reference()) -> #role{}.
tick(Role = #role{s_timer = STimer}, Ref) ->
    {NewSTimer, NRole} = stimer:tick(STimer, Ref, Role),
    NRole#role{s_timer = NewSTimer}.
