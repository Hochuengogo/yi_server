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
    , datetime_to_timestamp/1
    , timestamp/0
    , timestamp/1
    , next_diff/1
    , next_week_diff/1
    , next_month_diff/1
]).

-include("common.hrl").
-include("logs.hrl").

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

%% @doc unix时间戳转日期时间
-spec datetime_to_timestamp(calendar:datetime()) -> pos_integer().
datetime_to_timestamp(Datetime) ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time_to_universal_time(Datetime)) - ?secs_from_0_to_1970.

%% @doc 当前时间unix时间戳
-spec timestamp() -> pos_integer().
timestamp() ->
    {MegaSecs, Secs, _} = ?MODULE:now(),
    MegaSecs * 1000000 + Secs.
%% @doc unix时间戳
-spec timestamp(s | ms | zero | day_five | five | calendar:time() | {zero, pos_integer()} | {five, pos_integer()} | {pos_integer(), calendar:time()}) -> pos_integer().
timestamp(s) ->
    timestamp();
timestamp(ms) ->
    {MegaSecs, Secs, MicroSecs} = ?MODULE:now(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000;
timestamp(zero) -> %% 当天0点时间戳
    T = {MegaSecs, Secs, _} = ?MODULE:now(),
    {_, Time} = calendar:now_to_local_time(T),
    MegaSecs * 1000000 + Secs - calendar:time_to_seconds(Time);
timestamp(day_five) -> %% 当天5点时间戳
    timestamp(zero) + ?hour_s(5);
timestamp(five) -> %% 当前时间在5点或5点前，则为前一天5点时间戳；当前时间在5点后，则为当天5点时间戳
    Time = ?MODULE:time(),
    case Time > {5, 0, 0} of
        true ->
            timestamp(day_five);
        _ ->
            timestamp(day_five) - ?day_s
    end;
timestamp({0, 0, 0}) ->
    timestamp(zero);
timestamp({5, 0, 0}) ->
    timestamp(day_five);
timestamp(Time = {_H, _Min, _S}) -> %% 当天某个时间点的时间戳
    timestamp(zero) + calendar:time_to_seconds(Time);
timestamp({zero, TS}) -> %% 该时间戳的0点时间戳
    {_, Time} = ?MODULE:timestamp_to_datetime(TS),
    TS - calendar:time_to_seconds(Time);
timestamp({day_five, TS}) -> %% 该时间戳的5点时间戳
    timestamp({zero, TS}) + ?hour_s(5);
timestamp({five, TS}) -> %% 该时间戳在5点或5点前，则为前一天5点时间戳；当前时间在5点后，则为该时间戳5点时间戳
    {_, Time} = timestamp_to_datetime(TS),
    case Time > {5, 0, 0} of
        true ->
            TS - calendar:time_to_seconds(Time) + ?hour_s(5);
        _ ->
            TS - calendar:time_to_seconds(Time) + ?hour_s(5) - ?day_s
    end;
timestamp({TS, {0, 0, 0}}) ->
    timestamp({zero, TS});
timestamp({TS, {5, 0, 0}}) ->
    timestamp({day_five, TS});
timestamp({TS, Time = {_H, _Min, _S}}) -> %% 该时间戳当天某个时间点的时间戳
    timestamp({zero, TS}) + calendar:time_to_seconds(Time).

%% @doc 距离下次时间点秒数
-spec next_diff(zero | five | calendar:time() | [calendar:time()]) -> pos_integer().
next_diff(zero) -> %% 距离下次0点秒数
    Time = ?MODULE:time(),
    ?day_s - calendar:time_to_seconds(Time);
next_diff({0, 0, 0}) ->
    next_diff(zero);
next_diff(five) -> %% 距离下次5点秒数
    Time = ?MODULE:time(),
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
    Time = ?MODULE:time(),
    case Time >= T of
        true ->
            ?day_s - calendar:time_to_seconds(Time) + calendar:time_to_seconds(T);
        _ ->
            calendar:time_to_seconds(T) - calendar:time_to_seconds(Time)
    end.

%% @doc 距离下次周几时间点秒数
-spec next_week_diff(zero | five | {calendar:daynum(), calendar:time()} | {[calendar:daynum()], calendar:time()}) -> pos_integer().
next_week_diff(zero) -> %% 距离下次周一0点秒数
    {Date, Time} = ?MODULE:datetime(),
    WeekDay = calendar:day_of_the_week(Date),
    ?day_s(1) + ?week_s - (?day_s(WeekDay) + calendar:time_to_seconds(Time));
next_week_diff({1, {0, 0, 0}}) ->
    next_week_diff(zero);
next_week_diff(five) -> %% 距离下次周一5点秒数
    {Date, Time} = ?MODULE:datetime(),
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
    {Date, Time} = ?MODULE:datetime(),
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
    {{Y, M, D}, Time} = ?MODULE:datetime(),
    Day = calendar:last_day_of_the_month(Y, M),
    ?day_s(Day) + ?day_s - (?day_s(D) + calendar:time_to_seconds(Time));
next_month_diff(five) -> %% 距离下次1号5点秒数
    {{Y, M, D}, Time} = ?MODULE:datetime(),
    case {D, Time} >= {1, {5, 0, 0}} of
        true ->
            Day = calendar:last_day_of_the_month(Y, M),
            ?day_s(Day) + ?day_s - (?day_s(D) + calendar:time_to_seconds(Time)) + ?hour_s(5);
        _ ->
            ?hour_s(5) - calendar:time_to_seconds(Time)
    end;
next_month_diff({MonthDays = [_ | _], T = {_H, _Min, _S}}) -> %% 距离下次几号任意时间点秒数
    lists:min([next_month_diff({MonthDay, T}) || MonthDay <- MonthDays]);
next_month_diff({MonthDay, T = {_H, _Min, _S}}) when is_integer(MonthDay) andalso MonthDay >= 1 andalso MonthDay =< 31 -> %% 距离下次几号任意时间点秒数
    {{Y, M, D}, Time} = ?MODULE:datetime(),
    case {D, Time} >= {MonthDay, T} of
        true ->
            Month =
                case M == 12 of
                    true ->
                        next_month_day_month(Y + 1, 1, MonthDay);
                    _ ->
                        next_month_day_month(Y, M + 1, MonthDay)
                end,
            Timestamp = ?MODULE:timestamp(),
            case Month =< M of
                true ->
                    ?MODULE:datetime_to_timestamp({{Y + 1, Month, MonthDay}, T}) - Timestamp;
                _ ->
                    ?MODULE:datetime_to_timestamp({{Y, Month, MonthDay}, T}) - Timestamp
            end;
        _ ->
            ?day_s(MonthDay) + calendar:time_to_seconds(T) - (?day_s(D) + calendar:time_to_seconds(Time))
    end.

%% 下一次月几号在几月
next_month_day_month(Y, M, MonthDay) ->
    Day = calendar:last_day_of_the_month(Y, M),
    case Day >= MonthDay of
        true ->
            M;
        _ ->
            case M == 12 of
                true ->
                    next_month_day_month(Y + 1, 1, MonthDay);
                _ ->
                    next_month_day_month(Y, M + 1, MonthDay)
            end
    end.