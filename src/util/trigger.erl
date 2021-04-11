%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 进程内部触发器
%%%      尽量不要在触发回调方法里触发新的事件，避免在触发新事件时再次触发回旧事件，导致闭环
%%%      不要 事件A -> 事件B -> 事件A
%%% @end
%%% Created : 09. 4月 2021 10:11 下午
%%%-------------------------------------------------------------------
-module(trigger).
-author("huangzaoyi").

%% API
-export([
    register/2
    , registers/2
    , unregister/2
    , unregisters/2
    , unregister_event/2
    , unregister_events/2
    , fire/3
    , async_fire/1
    , async_fire/2
]).

-include("logs.hrl").
-include("trigger.hrl").

%% @doc 注册触发器
%% 注册触发器，返回新的总触发器结构，和{事件，触发器ID}
-spec register(#s_trigger{}, #trigger{}) -> {#s_trigger{}, {term(), pos_integer()}}.
register(STrigger = #s_trigger{triggers = TriggerList, next_id = NextId}, AddTrigger = #trigger{event = Event, callback = {_, _, _}}) ->
    NewAddTrigger = AddTrigger#trigger{id = NextId},
    NewTriggerList =
        case lists:keyfind(Event, 1, TriggerList) of
            {_, Triggers} ->
                NewTriggers = Triggers ++ [NewAddTrigger], %% 按照放入的顺序
                lists:keyreplace(Event, 1, TriggerList, {Event, NewTriggers});
            _ ->
                [{Event, [NewAddTrigger]} | TriggerList]
        end,
    NewSTrigger = STrigger#s_trigger{triggers = NewTriggerList, next_id = NextId + 1},
    {NewSTrigger, {Event, NextId}}.

%% @doc 注册多个触发器
%% 注册多个触发器，返回新的总触发器结构，和{事件，触发器ID}列表
-spec registers(#s_trigger{}, [#trigger{}]) -> {#s_trigger{}, [{term(), pos_integer()}]}.
registers(STrigger, Triggers) when is_list(Triggers) ->
    registers(STrigger, Triggers, []).
registers(STrigger, [], Acc) ->
    {STrigger, lists:reverse(Acc)};
registers(STrigger, [AddTrigger | AddTriggers], Acc) ->
    {NewSTrigger, Key} = ?MODULE:register(STrigger, AddTrigger),
    registers(NewSTrigger, AddTriggers, [Key | Acc]).

%% @doc 取消注册触发器
%% 取消注册触发器，返回新的总触发器结构
-spec unregister(#s_trigger{}, {term(), pos_integer()}) -> #s_trigger{}.
unregister(STrigger = #s_trigger{triggers = TriggerList}, {Event, Id}) ->
    case lists:keyfind(Event, 1, TriggerList) of
        {_, Triggers} ->
            NewTriggers = lists:keydelete(Id, #trigger.id, Triggers),
            NewTriggerList =
                case NewTriggers of
                    [] ->
                        lists:keydelete(Event, 1, TriggerList);
                    _ ->
                        lists:keyreplace(Event, 1, TriggerList, {Event, NewTriggers})
                end,
            STrigger#s_trigger{triggers = NewTriggerList};
        _ ->
            STrigger
    end.

%% @doc 取消注册多个触发器
%% 取消注册多个触发器，返回新的总触发器结构
-spec unregisters(#s_trigger{}, [{term(), pos_integer()}]) -> #s_trigger{}.
unregisters(STrigger, []) ->
    STrigger;
unregisters(STrigger, [Key | KeyList]) ->
    NewSTrigger = ?MODULE:unregister(STrigger, Key),
    unregisters(NewSTrigger, KeyList).

%% @doc 取消注册事件
%% 取消注册事件，返回新的总触发器结构
-spec unregister_event(#s_trigger{}, term()) -> #s_trigger{}.
unregister_event(STrigger = #s_trigger{triggers = TriggerList}, Event) ->
    NewTriggerList = lists:keydelete(Event, 1, TriggerList),
    STrigger#s_trigger{triggers = NewTriggerList}.

%% @doc 取消注册多个事件
%% 取消注册多个事件，返回新的总触发器结构
-spec unregister_events(#s_trigger{}, [term()]) -> #s_trigger{}.
unregister_events(STrigger, []) ->
    STrigger;
unregister_events(STrigger, [Event | Events]) ->
    NewSTrigger = unregister_event(STrigger, Event),
    unregister_events(NewSTrigger, Events).

%% @doc 异步触发事件
%% 在对应的进程需要处理 {fire_trigger, EventTuple} 消息，执行方法使用 fire/3 即可
-spec async_fire(tuple()) -> ok.
async_fire(EventTuple) ->
    async_fire(self(), EventTuple).
async_fire(Pid, EventTuple) ->
    Pid ! {fire_trigger, EventTuple}.

%% @doc 触发事件
%% EventTuple ：是一个元祖，第一项是事件名，后面自己定义
%% 触发事件中，出现闭环，或者执行回调方法失败，会返回错误，正常返回新的总触发器结构，和新的状态
-spec fire(#s_trigger{}, term(), tuple()) -> {#s_trigger{}, term()}.
fire(STrigger = #s_trigger{triggers = TriggerList}, State, EventTuple) ->
    Event = element(1, EventTuple),
    case lists:keyfind(Event, 1, TriggerList) of
        {_, Triggers} ->
            FireEvents = util:get('@trigger_fire_events', []),
            case lists:keymember(Event, 1, FireEvents) of
                false -> %% 不能在触发事件中再触发事件
                    put('@trigger_fire_events', [{Event, Triggers} | FireEvents]),
                    case catch do_fire(STrigger, State, EventTuple, Event) of
                        {NewSTrigger = #s_trigger{}, NewState} ->
                            put('@trigger_fire_events', lists:keydelete(Event, 1, util:get('@trigger_fire_events', []))),
                            {NewSTrigger, NewState};
                        _ ->
                            erase('@trigger_fire_events'),
                            erlang:error(fire_trigger_bad_return)
                    end;
                _ ->
                    ?error("触发事件中再触发事件：~w，已经存在的事件：~w", [EventTuple, FireEvents]),
                    erase('@trigger_fire_events'),
                    erlang:error(fire_same_trigger_error)
            end;
        _ ->
            {STrigger, State}
    end.

do_fire(STrigger, State, EventTuple, Event) ->
    FireEvents = util:get('@trigger_fire_events', []),
    case lists:keyfind(Event, 1, FireEvents) of
        {_, [Trigger = #trigger{id = Id, callback = {M, F, A}} | Triggers]} ->
            put('@trigger_fire_events', lists:keyreplace(Event, 1, FireEvents, {Event, Triggers})),
%%            ?debug("执行触发回调，触发器：~w，触发事件结构：~w", [Trigger, EventTuple]),
            case catch erlang:apply(M, F, [STrigger, State, EventTuple | A]) of
                ok ->
                    do_fire(STrigger, State, EventTuple, Event);
                {ok, NewSTrigger = #s_trigger{}} ->
                    do_fire(NewSTrigger, State, EventTuple, Event);
                {ok, NewSTrigger = #s_trigger{}, NewState} ->
                    do_fire(NewSTrigger, NewState, EventTuple, Event);
                remove -> %% 删除当前触发器
                    NewSTrigger = ?MODULE:unregister(STrigger, {Event, Id}),
                    do_fire(NewSTrigger, State, EventTuple, Event);
                {remove, NewSTrigger0 = #s_trigger{}} -> %% 删除当前触发器
                    NewSTrigger = ?MODULE:unregister(NewSTrigger0, {Event, Id}),
                    do_fire(NewSTrigger, State, EventTuple, Event);
                {remove, NewSTrigger0 = #s_trigger{}, NewState} -> %% 删除当前触发器
                    NewSTrigger = ?MODULE:unregister(NewSTrigger0, {Event, Id}),
                    do_fire(NewSTrigger, NewState, EventTuple, Event);
                {remove, {DelEvent, DelId}} -> %% 删除触发器
                    do_fire_unregister_trigger([{DelEvent, DelId}]),  %% 如果在后面则不让删除的触发器触发
                    NewSTrigger = ?MODULE:unregister(STrigger, {DelEvent, DelId}),
                    do_fire(NewSTrigger, State, EventTuple, Event);
                {remove, {DelEvent, DelId}, NewSTrigger0 = #s_trigger{}} -> %% 删除触发器
                    do_fire_unregister_trigger([{DelEvent, DelId}]),  %% 如果在后面则不让删除的触发器触发
                    NewSTrigger = ?MODULE:unregister(NewSTrigger0, {DelEvent, DelId}),
                    do_fire(NewSTrigger, State, EventTuple, Event);
                {remove, {DelEvent, DelId}, NewSTrigger0 = #s_trigger{}, NewState} -> %% 删除触发器
                    do_fire_unregister_trigger([{DelEvent, DelId}]),  %% 如果在后面则不让删除的触发器触发
                    NewSTrigger = ?MODULE:unregister(NewSTrigger0, {DelEvent, DelId}),
                    do_fire(NewSTrigger, NewState, EventTuple, Event);
                {remove, KeyList} when is_list(KeyList) -> %% 删除触发器
                    do_fire_unregister_trigger(KeyList),  %% 如果在后面则不让删除的触发器触发
                    NewSTrigger = unregisters(STrigger, KeyList),
                    do_fire(NewSTrigger, State, EventTuple, Event);
                {remove, KeyList, NewSTrigger0 = #s_trigger{}} when is_list(KeyList) -> %% 删除触发器
                    do_fire_unregister_trigger(KeyList),  %% 如果在后面则不让删除的触发器触发
                    NewSTrigger = unregisters(NewSTrigger0, KeyList),
                    do_fire(NewSTrigger, State, EventTuple, Event);
                {remove, KeyList, NewSTrigger0 = #s_trigger{}, NewState} when is_list(KeyList) -> %% 删除触发器
                    do_fire_unregister_trigger(KeyList),  %% 如果在后面则不让删除的触发器触发
                    NewSTrigger = unregisters(NewSTrigger0, KeyList),
                    do_fire(NewSTrigger, NewState, EventTuple, Event);
                _Err ->
                    ?error("触发器执行回调错误，触发器：~w，状态：~w，事件：~w，错误：~w", [Trigger, State, EventTuple, _Err]),
                    erlang:error(fire_trigger_error)
            end;
        {_, []} ->
            {STrigger, State};
        false ->
            {STrigger, State}
    end.

%% 在触发事件时，删除将要触发的触发器
do_fire_unregister_trigger(KeyList) ->
    do_fire_unregister_trigger2(KeyList, util:get('@trigger_fire_events', [])).
do_fire_unregister_trigger2([], FireEvents) ->
    put('@trigger_fire_events', FireEvents),
    ok;
do_fire_unregister_trigger2([{Event, Id} | KeyList], FireEvents) ->
    NewFireEvents =
        case lists:keyfind(Event, 1, FireEvents) of
            {_, Triggers} ->
                NewTriggers = lists:keydelete(Id, #trigger.id, Triggers),
                NewFireEvents0 = lists:keyreplace(Event, 1, FireEvents, {Event, NewTriggers}),
                NewFireEvents0;
            _ ->
                FireEvents
        end,
    do_fire_unregister_trigger2(KeyList, NewFireEvents).