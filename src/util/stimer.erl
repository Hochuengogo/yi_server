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
    add/2
    , del/2
    , set/2
    , tick/3
]).

-include("common.hrl").
-include("logs.hrl").
-include("stimer.hrl").

%% @doc 增加定时器
-spec add(#s_timer{}, #timer{}) -> #s_timer{}.
add(STimer, []) ->
    STimer;
add(STimer = #s_timer{timers = []}, Timers) when is_list(Timers) ->
    Now = time_util:timestamp(ms),
    NewTimers0 = [Timer#timer{timestamp = Now + Timeout} || Timer = #timer{timeout = Timeout} <- Timers],
    NewTimers = list_util:keysort(#timer.timestamp, NewTimers0, asc),
    case NewTimers of
        [#timer{timestamp = Timestamp} | _] ->
            NewRef = util:start_timer(max(1, Timestamp - Now), self(), timer_tick),
            STimer#s_timer{timers = NewTimers, ref = NewRef};
        _ ->
            STimer#s_timer{timers = NewTimers}
    end;
add(STimer = #s_timer{timers = Timers = [Timer | _], ref = Ref}, AddTimers) when is_list(Timers) ->
    Now = time_util:timestamp(ms),
    NewAddTimers = [Timer0#timer{timestamp = Now + Timeout} || Timer0 = #timer{timeout = Timeout} <- AddTimers],
    NewTimers = list_util:keysort(#timer.timestamp, NewAddTimers ++ Timers, asc),
    case NewTimers of
        [Timer | _] ->
            STimer#s_timer{timers = NewTimers};
        [#timer{timestamp = Timestamp} | _] ->
            util:cancel_timer(Ref),
            NewRef = util:start_timer(max(1, Timestamp - Now), self(), timer_tick),
            STimer#s_timer{timers = NewTimers, ref = NewRef}
    end;
add(STimer = #s_timer{timers = []}, Timer = #timer{timeout = Timeout}) ->
    Now = time_util:timestamp(ms),
    Timestamp = Now + Timeout,
    NewTimer = Timer#timer{timestamp = Timestamp},
    Ref = util:start_timer(max(1, Timeout), self(), timer_tick),
    STimer#s_timer{timers = [NewTimer], ref = Ref};
add(STimer = #s_timer{timers = Timers, ref = Ref}, Timer = #timer{timeout = Timeout}) ->
    Now = time_util:timestamp(ms),
    Timestamp = Now + Timeout,
    NewTimer = Timer#timer{timestamp = Timestamp},
    NewTimers = list_util:add_keysort([{asc, #timer.timestamp}], NewTimer, Timers),
    case NewTimers of
        [NewTimer | _] ->
            util:cancel_timer(Ref),
            NewRef = util:start_timer(max(1, Timeout), self(), timer_tick),
            STimer#s_timer{timers = NewTimers, ref = NewRef};
        _ ->
            STimer#s_timer{timers = NewTimers}
    end.

%% @doc 删除定时器
-spec del(#s_timer{}, term()) -> #s_timer{}.
del(STimer, []) ->
    STimer;
del(STimer, Id) ->
    NewSTimer = do_del(STimer, Id),
    do_set(NewSTimer).

do_del(STimer, []) ->
    STimer;
do_del(STimer = #s_timer{timers = []}, _Id) ->
    STimer;
do_del(STimer = #s_timer{timers = [Timer | _] = Timers, ref = Ref}, Ids) when is_list(Ids) ->
    NewTimers = [Timer0 || Timer0 = #timer{id = Id} <- Timers, not lists:member(Id, Ids)],
    case NewTimers of
        [Timer | _] ->
            STimer#s_timer{timers = NewTimers};
        _ ->
            util:cancel_timer(Ref),
            STimer#s_timer{timers = NewTimers, ref = undefined}
    end;
do_del(STimer = #s_timer{timers = [#timer{id = Id}], ref = Ref}, Id) ->
    util:cancel_timer(Ref),
    STimer#s_timer{timers = [], ref = undefined};
do_del(STimer = #s_timer{timers = [#timer{id = Id} | Timers], ref = Ref}, Id) ->
    util:cancel_timer(Ref),
    STimer#s_timer{timers = Timers, ref = undefined};
do_del(STimer = #s_timer{timers = Timers}, Id) ->
    NewTimers = lists:keydelete(Id, #timer.id, Timers),
    STimer#s_timer{timers = NewTimers}.

%% @doc 设置定时器
-spec set(#s_timer{}, #timer{}) -> #s_timer{}.
set(STimer, []) ->
    STimer;
set(STimer, Timers) when is_list(Timers) ->
    Ids = [Id || #timer{id = Id} <- Timers],
    NewSTimer0 = do_del(STimer, Ids),
    NewSTimer = add(NewSTimer0, Timers),
    do_set(NewSTimer);
set(STimer, Timer = #timer{id = Id}) ->
    NewSTimer0 = do_del(STimer, Id),
    NewSTimer = add(NewSTimer0, Timer),
    do_set(NewSTimer).

do_set(STimer = #s_timer{timers = Timers, ref = Ref}) ->
    case Ref of
        undefined ->
            case Timers of
                [#timer{timestamp = Timestamp} | _] ->
                    Now = time_util:timestamp(ms),
                    Ref = util:start_timer(max(1, Timestamp - Now), self(), timer_tick),
                    STimer#s_timer{ref = Ref};
                _ ->
                    STimer
            end;
        _ ->
            STimer
    end.

%% @doc 定时器触发
-spec tick(#s_timer{}, reference(), tuple()) -> #s_timer{}.
tick(STimer = #s_timer{timers = [Timer = #timer{callback = {M, F, A}} | _], ref = Ref}, Ref, State) ->
    Ret =
        case catch util:apply(M, F, [State | A]) of
            ok ->
                {ok, STimer, State};
            {ok, NewState} ->
                {ok, STimer, NewState};
            remove ->
                {remove, STimer, State};
            {remove, NewState} ->
                {remove, STimer, NewState};
            Err -> %% 当做执行成功
                ?error("定时器执行失败，定时器：~w，状态：~w，错误：~w", [Timer, State, Err]),
                {ok, STimer, State}
        end,
    tick_after(Ret);
tick(STimer = #s_timer{timers = [#timer{timestamp = Timestamp} | _], ref = Ref}, _OldRef, State) ->
    ?error("定时器执行失败，错误的定时器，状态：~w", [State]),
    util:cancel_timer(Ref),
    Now = time_util:timestamp(ms),
    NewRef = util:start_timer(max(1000, Timestamp - Now), self(), timer_tick), %% 防止出错，一直发送消息
    NewSTimer = STimer#s_timer{ref = NewRef},
    {NewSTimer, State};
tick(STimer = #s_timer{timers = []}, _Ref, State) ->
    ?error("定时器执行失败，没有定时器，状态：~w", [State]),
    {STimer, State}.

tick_after({ok, STimer = #s_timer{timers = [Timer = #timer{time = Time, timeout = Timeout}]}, State}) ->
    case max(-1, Time - 1) of
        0 ->
            NewSTimer = STimer#s_timer{timers = [], ref = undefined},
            {NewSTimer, State};
        NewTime ->
            Now = time_util:timestamp(ms),
            NewTimer = Timer#timer{time = NewTime, timestamp = Now + Timeout},
            NewRef = util:start_timer(max(1, Timeout), self(), timer_tick),
            NewSTimer = STimer#s_timer{timers = [NewTimer], ref = NewRef},
            {NewSTimer, State}
    end;
tick_after({ok, STimer = #s_timer{timers = [Timer = #timer{time = Time, timeout = Timeout} | Timers]}, State}) ->
    Now = time_util:timestamp(ms),
    case max(-1, Time - 1) of
        0 ->
            #timer{timestamp = Timestamp} = hd(Timers),
            NewRef = util:start_timer(max(1, Timestamp - Now), self(), timer_tick),
            NewSTimer = STimer#s_timer{timers = Timers, ref = NewRef},
            {NewSTimer, State};
        NewTime ->
            NewTimer = Timer#timer{time = NewTime, timestamp = Now + Timeout},
            NewTimers = list_util:add_keysort([{asc, #timer.timestamp}], NewTimer, Timers),
            #timer{timestamp = Timestamp} = hd(Timers),
            NewRef = util:start_timer(max(1, Timestamp - Now), self(), timer_tick),
            NewSTimer = STimer#s_timer{timers = NewTimers, ref = NewRef},
            {NewSTimer, State}
    end;
tick_after({remove, STimer = #s_timer{timers = [_]}, State}) ->
    NewSTimer = STimer#s_timer{timers = [], ref = undefined},
    {NewSTimer, State};
tick_after({remove, STimer = #s_timer{timers = [_ | Timers]}, State}) ->
    Now = time_util:timestamp(ms),
    #timer{timestamp = Timestamp} = hd(Timers),
    NewRef = util:start_timer(max(1, Timestamp - Now), self(), timer_tick),
    NewSTimer = STimer#s_timer{timers = Timers, ref = NewRef},
    {NewSTimer, State}.