%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色触发器
%%% @end
%%% Created : 2021-10-07 21:56:31
%%%-------------------------------------------------------------------
-module(role_trigger).
-author("jiaoyinyi").

%% API
-export([
    register/2
    , unregister/2
    , unregister_event/2
    , async_fire/1
    , async_fire/2
    , fire/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").
-include("trigger.hrl").

%% @doc 注册触发器
%% 注册触发器，返回新的角色结构，和{事件，触发器ID} | [{事件，触发器ID}]
-spec register(#role{}, #trigger{}) -> {#role{}, {term(), pos_integer()}}.
register(Role = #role{s_trigger = STrigger}, Trigger) ->
    {NewSTrigger, Key} = strigger:register(STrigger, Trigger),
    {Role#role{s_trigger = NewSTrigger}, Key}.

%% @doc 取消注册触发器
%% 取消注册触发器，返回新的角色结构
-spec unregister(#role{}, {term(), pos_integer()}) -> #role{}.
unregister(Role = #role{s_trigger = STrigger}, Key) ->
    NewSTrigger = strigger:unregister(STrigger, Key),
    Role#role{s_trigger = NewSTrigger}.

%% @doc 取消注册事件
%% 取消注册事件，返回新的角色结构
-spec unregister_event(#role{}, term()) -> #role{}.
unregister_event(Role = #role{s_trigger = STrigger}, Event) ->
    NewSTrigger = strigger:unregister_event(STrigger, Event),
    Role#role{s_trigger = NewSTrigger}.

%% @doc 异步触发事件
%% 在对应的进程需要处理 {fire_trigger, EventTuple} 消息，执行方法使用 fire/3 即可
-spec async_fire(tuple()) -> ok.
async_fire(EventTuple) ->
    strigger:async_fire(EventTuple).
async_fire(Pid, EventTuple) ->
    strigger:async_fire(Pid, EventTuple).

%% @doc 触发事件
%% EventTuple ：是一个元组，第一项是事件名，后面自己定义
%% 触发事件中，出现闭环，或者执行回调方法失败，会返回错误，正常返回新的角色结构
fire(Role = #role{s_trigger = STrigger}, EventTuple) ->
    {NewSTrigger, NRole} = strigger:fire(STrigger, Role, EventTuple),
    NRole#role{s_trigger = NewSTrigger}.