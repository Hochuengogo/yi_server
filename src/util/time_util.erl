%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 时间库
%%%
%%% @end
%%% Created : 07. 3月 2021 7:33 下午
%%%-------------------------------------------------------------------
-module(time_util).
-author("huangzaoyi").

%% API
-export([
    now/0
    , time/0
    , datetime/0
    , date/0
    , timestamp_to_datetime/1
    , timestamp/0
    , timestamp/1
    , next_diff/1
    , next_week_diff/1
    , next_month_diff/1
]).

-include("common.hrl").

%% @doc 当前erlang时间戳
-spec now() -> erlang:timestamp().
now() ->
    os:timestamp().

%% @doc 当前时间
-spec time() -> calendar:time().
time() ->
    erlang:time().

%% @doc 当前日期时间
-spec datetime() -> calendar:datetime().
datetime() ->
    erlang:localtime().

%% @doc 当前日期
-spec date() -> calendar:date().
date() ->
    erlang:date().

%% @doc unix时间戳转日期时间
-spec timestamp_to_datetime(pos_integer()) -> calendar:datetime().
timestamp_to_datetime(TS) ->
    calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(TS + ?secs_from_0_to_1970)).

%% @doc 当前时间unix时间戳
-spec timestamp() -> pos_integer().
timestamp() ->
    {MegaSecs, Secs, _} = time_util:now(),
    MegaSecs * 1000000 + Secs.
%% @doc unix时间戳
-spec timestamp(s | ms | zero | five | {zero, pos_integer()} | {five, pos_integer()}) -> pos_integer().
timestamp(s) ->
    timestamp();
timestamp(ms) ->
    {MegaSecs, Secs, MicroSecs} = time_util:now(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000;
timestamp(zero) ->
    T = {MegaSecs, Secs, _} = time_util:now(),
    {_, Time} = calendar:now_to_local_time(T),
    MegaSecs * 1000000 + Secs - calendar:time_to_seconds(Time);
timestamp(five) ->
    T = {MegaSecs, Secs, _} = time_util:now(),
    {_, Time} = calendar:now_to_local_time(T),
    case Time > {5, 0, 0} of
        true ->
            MegaSecs * 1000000 + Secs - calendar:time_to_seconds(Time) + ?hour_s(5);
        _ ->
            MegaSecs * 1000000 + Secs - calendar:time_to_seconds(Time) - ?day_s + ?hour_s(5)
    end;
timestamp({zero, TS}) ->
    {_, Time} = timestamp_to_datetime(TS),
    TS - calendar:time_to_seconds(Time);
timestamp({five, TS}) ->
    {_, Time} = timestamp_to_datetime(TS),
    case Time > {5, 0, 0} of
        true ->
            TS - calendar:time_to_seconds(Time) + ?hour_s(5);
        _ ->
            TS - calendar:time_to_seconds(Time) - ?day_s + ?hour_s(5)
    end.

%% @doc 距离下次时间点秒数
-spec next_diff(zero | five | calendar:time() | [calendar:time()]) -> pos_integer().
next_diff(zero) -> %% 距离下次0点秒数
    Time = time_util:time(),
    ?day_s - calendar:time_to_seconds(Time);
next_diff({0, 0, 0}) ->
    next_diff(zero);
next_diff(five) -> %% 距离下次5点秒数
    Time = time_util:time(),
    case Time >= {5, 0, 0} of
        true ->
            ?day_s - calendar:time_to_seconds(Time) + ?hour_s(5);
        _ ->
            ?hour_s(5) - calendar:time_to_seconds(Time)
    end;
next_diff({5, 0, 0}) ->
    next_diff(five);
next_diff(Ts = [_ | _]) ->
    lists:min([next_diff(T) || T <- Ts]);
next_diff(T = {_H, _Min, _S}) -> %% 距离下次任意时间点秒数
    Time = time_util:time(),
    case Time >= T of
        true ->
            ?day_s - calendar:time_to_seconds(Time) + calendar:time_to_seconds(T);
        _ ->
            calendar:time_to_seconds(T) - calendar:time_to_seconds(Time)
    end.

%% @doc 距离下次周几时间点秒数
-spec next_week_diff(zero | five | {calendar:daynum(), calendar:time()} | {[calendar:daynum()], calendar:time()}) -> pos_integer().
next_week_diff(zero) -> %% 距离下次周一0点秒数
    {Date, Time} = time_util:datetime(),
    WeekDay = calendar:day_of_the_week(Date),
    ?day_s(1) + ?week_s - (?day_s(WeekDay) + calendar:time_to_seconds(Time));
next_week_diff({1, {0, 0, 0}}) ->
    next_week_diff(zero);
next_week_diff(five) -> %% 距离下次周一5点秒数
    {Date, Time} = time_util:datetime(),
    WeekDay = calendar:day_of_the_week(Date),
    case {WeekDay, Time} >= {1, {5, 0, 0}} of
        true ->
            ?day_s(1) + ?hour_s(5) + ?week_s - (?day_s(WeekDay) + calendar:time_to_seconds(Time));
        _ ->
            ?hour_s(5) - calendar:time_to_seconds(Time)
    end;
next_week_diff({1, {5, 0, 0}}) ->
    next_week_diff(five);
next_week_diff({WeekDays = [_ | _], T = {_H, _Min, _S}}) -> %% 距离下次周几任意时间点秒数
    lists:min([next_week_diff({WeekDay, T}) || WeekDay <- WeekDays]);
next_week_diff({WD, T = {_H, _Min, _S}}) -> %% 距离下次周几任意时间点秒数
    {Date, Time} = time_util:datetime(),
    WeekDay = calendar:day_of_the_week(Date),
    case {WeekDay, Time} >= {WD, T} of
        true ->
            ?day_s(WD) + calendar:time_to_seconds(T) + ?week_s - (?day_s(WeekDay) + calendar:time_to_seconds(Time));
        _ ->
            ?day_s(WD) + calendar:time_to_seconds(T) - (?day_s(WeekDay) + calendar:time_to_seconds(Time))
    end.

%% @doc 距离下次几号时间点秒数
-spec next_month_diff(zero | five | {calendar:day(), calendar:time()} | {[calendar:day()], calendar:time()}) -> pos_integer().
next_month_diff(zero) -> %% 距离下次1号0点秒数
    {{Y, M, D}, Time} = time_util:datetime(),
    Day = calendar:last_day_of_the_month(Y, M),
    ?day_s(Day + 1 - D) - calendar:time_to_seconds(Time);
next_month_diff(five) -> %% 距离下次1号5点秒数
    {{Y, M, D}, Time} = time_util:datetime(),
    case {D, Time} >= {1, {5, 0, 0}} of
        true ->
            todo;
        _ ->
            ?hour_s(5) - calendar:time_to_seconds(Time)
    end;
next_month_diff({MonthDays = [_ | _], T = {_H, _Min, _S}}) -> %% 距离下次几号任意时间点秒数
    lists:min([next_month_diff({MonthDay, T}) || MonthDay <- MonthDays]);
next_month_diff({MonthDay, T = {_H, _Min, _S}}) -> %% 距离下次几号任意时间点秒数
    todo.