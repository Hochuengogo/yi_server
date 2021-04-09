%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 触发器
%%%
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
]).

-include("logs.hrl").
-include("trigger.hrl").

%% @doc 注册触发器
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
-spec registers(#s_trigger{}, [#trigger{}]) -> {#s_trigger{}, [{term(), pos_integer()}]}.
registers(STrigger, Triggers) when is_list(Triggers) ->
    registers(STrigger, Triggers, []).
registers(STrigger, [], Acc) ->
    {STrigger, lists:reverse(Acc)};
registers(STrigger, [AddTrigger | AddTriggers], Acc) ->
    {NewSTrigger, Key} = ?MODULE:register(STrigger, AddTrigger),
    registers(NewSTrigger, AddTriggers, [Key | Acc]).

%% @doc 取消注册触发器
-spec unregister(#s_trigger{}, {term(), pos_integer()}) -> #s_trigger{}.
unregister(STrigger = #s_trigger{triggers = TriggerList}, {Event, Id}) ->
    case lists:keyfind(Event, 1, TriggerList) of
        {_, Triggers} ->
            NewTriggers = lists:keydelete(Id, #trigger.id, Triggers),
            NewTriggerList = lists:keyreplace(Event, 1, TriggerList, {Event, NewTriggers}),
            STrigger#s_trigger{triggers = NewTriggerList};
        _ ->
            STrigger
    end.

%% @doc 取消注册多个触发器
-spec unregisters(#s_trigger{}, [{term(), pos_integer()}]) -> #s_trigger{}.
unregisters(STrigger, []) ->
    STrigger;
unregisters(STrigger, [Key | KeyList]) ->
    NewSTrigger = ?MODULE:unregister(STrigger, Key),
    unregisters(NewSTrigger, KeyList).

%% @doc 取消注册事件
-spec unregister_event(#s_trigger{}, term()) -> #s_trigger{}.
unregister_event(STrigger = #s_trigger{triggers = TriggerList}, Event) ->
    NewTriggerList = lists:keydelete(Event, 1, TriggerList),
    STrigger#s_trigger{triggers = NewTriggerList}.

%% @doc 取消注册多个事件
-spec unregister_events(#s_trigger{}, [term()]) -> #s_trigger{}.
unregister_events(STrigger, []) ->
    STrigger;
unregister_events(STrigger, [Event | Events]) ->
    NewSTrigger = unregister_event(STrigger, Event),
    unregister_events(NewSTrigger, Events).

%% @doc 触发事件
-spec fire(#s_trigger{}, term(), tuple()) -> {#s_trigger{}, term()}.
fire(STrigger = #s_trigger{triggers = TriggerList}, State, EventTuple) ->
    Event = element(1, EventTuple),
    case lists:keyfind(Event, 1, TriggerList) of
        {_, Triggers} ->
            case util:get('@trigger_fire_events', []) of
                [] ->
                    put('@trigger_fire_events', [EventTuple]),
                    {NewSTrigger, NewState} = do_fire(STrigger, State, EventTuple, Triggers),
                    erase('@trigger_fire_events'),
                    {NewSTrigger, NewState};
                FireEvents -> %% 不能在触发事件中再触发事件
                    ?error("触发事件中再触发事件：~w，已经存在的事件：~w", [EventTuple, FireEvents]),
                    erase('@trigger_fire_events'),
                    {STrigger, State}
            end;
        _ ->
            {STrigger, State}
    end.

do_fire(STrigger, State, _EventTuple, []) ->
    {STrigger, State};
do_fire(STrigger, State, EventTuple, [Trigger = #trigger{event = Event, id = Id, callback = {M, F, A}} | Triggers]) ->
    case catch erlang:apply(M, F, [STrigger, State, EventTuple | A]) of
        ok ->
            do_fire(STrigger, State, EventTuple, Triggers);
        {ok, NewSTrigger = #s_trigger{}} ->
            do_fire(NewSTrigger, State, EventTuple, Triggers);
        {ok, NewSTrigger = #s_trigger{}, NewState} ->
            do_fire(NewSTrigger, NewState, EventTuple, Triggers);
        remove -> %% 删除当前触发器
            NewSTrigger = ?MODULE:unregister(STrigger, {Event, Id}),
            do_fire(NewSTrigger, State, EventTuple, Triggers);
        {remove, NewSTrigger0 = #s_trigger{}} -> %% 删除当前触发器
            NewSTrigger = ?MODULE:unregister(NewSTrigger0, {Event, Id}),
            do_fire(NewSTrigger, State, EventTuple, Triggers);
        {remove, NewSTrigger0 = #s_trigger{}, NewState} -> %% 删除当前触发器
            NewSTrigger = ?MODULE:unregister(NewSTrigger0, {Event, Id}),
            do_fire(NewSTrigger, NewState, EventTuple, Triggers);
        {remove, {DelEvent, DelId}} -> %% 删除触发器
            NewTriggers = lists:keydelete(DelId, #trigger.id, Triggers), %% 如果在后面则不让删除的触发器触发
            NewSTrigger = ?MODULE:unregister(STrigger, {DelEvent, DelId}),
            do_fire(NewSTrigger, State, EventTuple, NewTriggers);
        {remove, {DelEvent, DelId}, NewSTrigger0 = #s_trigger{}} -> %% 删除触发器
            NewTriggers = lists:keydelete(DelId, #trigger.id, Triggers), %% 如果在后面则不让删除的触发器触发
            NewSTrigger = ?MODULE:unregister(NewSTrigger0, {DelEvent, DelId}),
            do_fire(NewSTrigger, State, EventTuple, NewTriggers);
        {remove, {DelEvent, DelId}, NewSTrigger0 = #s_trigger{}, NewState} -> %% 删除触发器
            NewTriggers = lists:keydelete(DelId, #trigger.id, Triggers), %% 如果在后面则不让删除的触发器触发
            NewSTrigger = ?MODULE:unregister(NewSTrigger0, {DelEvent, DelId}),
            do_fire(NewSTrigger, NewState, EventTuple, NewTriggers);
        {remove, KeyList} when is_list(KeyList) -> %% 删除触发器
            NewTriggers = [Trigger0 || Trigger0 = #trigger{id = Id0} <- Triggers, not lists:keymember(Id0, 2, KeyList)], %% 如果在后面则不让删除的触发器触发
            NewSTrigger = unregisters(STrigger, KeyList),
            do_fire(NewSTrigger, State, EventTuple, NewTriggers);
        {remove, KeyList, NewSTrigger0 = #s_trigger{}} when is_list(KeyList) -> %% 删除触发器
            NewTriggers = [Trigger0 || Trigger0 = #trigger{id = Id0} <- Triggers, not lists:keymember(Id0, 2, KeyList)], %% 如果在后面则不让删除的触发器触发
            NewSTrigger = unregisters(NewSTrigger0, KeyList),
            do_fire(NewSTrigger, State, EventTuple, NewTriggers);
        {remove, KeyList, NewSTrigger0 = #s_trigger{}, NewState} when is_list(KeyList) -> %% 删除触发器
            NewTriggers = [Trigger0 || Trigger0 = #trigger{id = Id0} <- Triggers, not lists:keymember(Id0, 2, KeyList)], %% 如果在后面则不让删除的触发器触发
            NewSTrigger = unregisters(NewSTrigger0, KeyList),
            do_fire(NewSTrigger, NewState, EventTuple, NewTriggers);
        _Err ->
            ?error("触发器执行回调错误，触发器：~w，状态：~w，事件：~w，错误：~w", [Trigger, State, EventTuple, _Err]),
            do_fire(STrigger, State, EventTuple, Triggers)
    end.