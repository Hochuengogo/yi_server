%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 定时器
%%%
%%% @end
%%% Created : 11. 4月 2021 5:50 下午
%%%-------------------------------------------------------------------
-module(stimer).
-author("huangzaoyi").

%% API
-export([

]).

-define(sort_max_num, 500). %% 有序列表最大长度

-include("common.hrl").
-include("logs.hrl").
-include("stimer.hrl").

add(STimer = #s_timer{last_sort_timestamp = 0, ref_id = OldRefId}, Timer = #timer{timeout = Timeout}) ->
    Now = time_util:timestamp(ms),
    Timestamp = Now + Timeout,
    NewTimer = Timer#timer{timestamp = Timestamp},
    RefId = OldRefId + 1,
    Ref = erlang:send_after(max(1, Timeout), self(), {timer_tick, RefId}),
    STimer#s_timer{sort_timers = [NewTimer], unsort_timers = [], ref = Ref, ref_id = RefId, last_sort_timestamp = Timestamp};
add(STimer = #s_timer{sort_timers = [T | _] = SortTimers, unsort_timers = UnsortTimers, last_sort_timestamp = LastSortTimestamp, ref = OldRef, ref_id = OldRefId}, Timer = #timer{timeout = Timeout}) ->
    Now = time_util:timestamp(ms),
    Timestamp = Now + Timeout,
    NewTimer = Timer#timer{timestamp = Timestamp},
    case Timestamp > LastSortTimestamp of
        false ->
            NewTimer = Timer#timer{timestamp = Timestamp},
            NewSortTimers = list_util:add_keysort(NewTimer, SortTimers, [{asc, #timer.timestamp}]),
            case NewSortTimers of
                [T | _] -> %% 第一个定时器没有改变
                    #timer{timestamp = NewLastSortTimestamp} = lists:last(NewSortTimers),
                    STimer#s_timer{sort_timers = NewSortTimers, last_sort_timestamp = NewLastSortTimestamp};
                _ -> %% 第一个定时器改变了
                    #timer{timestamp = TS} = hd(NewSortTimers),
                    catch erlang:cancel_timer(OldRef),
                    RefId = OldRefId + 1,
                    Ref = erlang:send_after(max(1, TS - Now), self(), {timer_tick, RefId}),
                    STimer#s_timer{sort_timers = NewSortTimers, ref = Ref, ref_id = RefId}
            end;
        _ ->
            NewUnsortTimers = [NewTimer | UnsortTimers],
            STimer#s_timer{unsort_timers = NewUnsortTimers}
    end.

del(STimer = #s_timer{sort_timers = [], unsort_timers = []}, _Id) ->
    STimer;
del(STimer = #s_timer{sort_timers = [#timer{id = Id} | SortTimers], ref = OldRef, ref_id = OldRefId}, Id) ->
    catch erlang:cancel_timer(OldRef),
    case SortTimers of
        [#timer{timestamp = TS} | _] ->
            RefId = OldRefId + 1,
            Ref = erlang:send_after(max(1, TS - time_util:timestamp(ms)), self(), {timer_tick, RefId}),
            STimer#s_timer{sort_timers = SortTimers, ref = Ref, ref_id = RefId};
        _ ->
            STimer#s_timer{sort_timers = [], ref = undefined, ref_id = OldRefId + 1}
    end.

%% 加载有序定时器
load_sort_timers(SortTimers, UnsortTimers) ->
    Timers = lists:keysort(#timer.timestamp, SortTimers ++ UnsortTimers),
    {NewSortTimers, NewUnsortTimers} = lists:split(?sort_max_num, Timers),
    {NewSortTimers, NewUnsortTimers}.

set(STimer, Timer) ->
    todo.

tick(STimer, State, RefId) ->
    todo.