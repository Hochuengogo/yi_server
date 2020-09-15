%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 时间库
%%% @end
%%% Created : 01. 十二月 2019 23:38
%%%-------------------------------------------------------------------
-module(time_lib).
-author("huangzaoyi").

%% API
-export([
    day_s/0, hour_s/0, min_s/0,
    day_s/1, hour_s/1, min_s/1,
    day_ms/0, hour_ms/0, min_ms/0, s_ms/0,
    day_ms/1, hour_ms/1, min_ms/1, s_ms/1,
    timestamp/0, now/0, mill_now/0, time/1,
    time_to_local_time/1,
    next_diff/1,
    next_week_diff/2,
    is_same_day/2,
    is_same_week/2,
    is_same_month/2,
    is_same_year/2,
    day_of_week/1,
    seconds/1,
    is_leap_year/1
]).

-define(SECONDS_FROM_0_TO_1970, 62167219200).

%% @doc 一天的秒数
-spec day_s() -> pos_integer().
day_s() ->
    86400.

%% @doc 一小时的秒数
-spec hour_s() -> pos_integer().
hour_s() ->
    3600.

%% @doc 一分钟的秒数
-spec min_s() -> pos_integer().
min_s() ->
    60.

%% @doc 一天的毫秒数
-spec day_ms() -> pos_integer().
day_ms() ->
    86400000.

%% @doc 一小时的毫秒数
-spec hour_ms() -> pos_integer().
hour_ms() ->
    3600000.

%% @doc 一分钟的毫秒数
-spec min_ms() -> pos_integer().
min_ms() ->
    60000.

%% @doc 一秒的毫秒数
-spec s_ms() -> pos_integer().
s_ms() ->
    1000.

%% @doc 获取多少天的秒数
-spec day_s(pos_integer()) -> pos_integer().
day_s(Day) ->
    Day * day_s().

%% @doc 获取多少小时的秒数
-spec hour_s(pos_integer()) -> pos_integer().
hour_s(Hour) ->
    Hour * hour_s().

%% @doc 获取多少分钟的秒数
-spec min_s(pos_integer()) -> pos_integer().
min_s(Min) ->
    Min * min_s().

%% @doc 获取多少天的毫秒数
-spec day_ms(pos_integer()) -> pos_integer().
day_ms(Day) ->
    Day * day_ms().

%% @doc 获取多少小时的毫秒数
-spec hour_ms(pos_integer()) -> pos_integer().
hour_ms(Hour) ->
    Hour * hour_ms().

%% @doc 获取多少分钟的毫秒数
-spec min_ms(pos_integer()) -> pos_integer().
min_ms(Min) ->
    Min * min_ms().

%% @doc 获取多少秒的毫秒数
-spec s_ms(pos_integer()) -> pos_integer().
s_ms(Sec) ->
    Sec * s_ms().

%% @doc 时间戳
-spec timestamp() -> {pos_integer(), pos_integer(), pos_integer()}.
timestamp() ->
    os:timestamp().

%% @doc 当前时间时间戳 10位
-spec now() -> pos_integer().
now() ->
    {M, S, _} = ?MODULE:timestamp(),
    M * 1000000 + S.
%% @doc 当前时间时间戳 13位
-spec mill_now() -> pos_integer().
mill_now() ->
    {M, S, Us} = ?MODULE:timestamp(),
    M * 1000000000 + S * 1000 + Us.

%% @doc 时间戳转成当地时间日期
-spec time_to_local_time(pos_integer()) -> calendar:datetime().
time_to_local_time(Time) ->
    calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(Time + ?SECONDS_FROM_0_TO_1970)).

%% @doc 时间戳
-spec time(Label | T) -> pos_integer() when
    Label :: s | ms,
    T :: calendar:time().
time(s) ->
    ?MODULE:now();
time(ms) ->
    mill_now();
%% 当天零点的时间戳
time({0, 0, 0}) ->
    Now = {M, S, _} = ?MODULE:timestamp(),
    {_, T} = calendar:now_to_local_time(Now),
    Sec = calendar:time_to_seconds(T),
    M * 1000000 + S - Sec;
%% 当天零点的时间戳
time(zero) ->
    time({0, 0, 0});
%% 当天某个时间点的时间戳
time({_H, _Min, _S} = T) ->
    time({0, 0, 0}) + calendar:time_to_seconds(T).

%% @doc 获取当前时间距离下一个时间点的时间间隔
-spec next_diff(calendar:time()) -> pos_integer().
next_diff({_H, _Min, _S} = T) ->
    Now = ?MODULE:now(),
    Time = ?MODULE:time(T),
    case Now >= Time of
        true ->
            Time - Now + day_s();
        _ ->
            Time - Now
    end.

%% @doc 获取当前时间到下一个星期几时间点的时间间隔
-spec next_week_diff([1..7], calendar:time()) -> pos_integer().
next_week_diff(WeekDays, {_H, _Min, _S} = T) when is_list(WeekDays) andalso WeekDays =/= [] ->
    {NowDate, NowT} = calendar:now_to_local_time(?MODULE:timestamp()),
    NowDay = calendar:day_of_the_week(NowDate),
    NowSec = calendar:time_to_seconds(NowT), %% 当前时间距离当天零时的秒数
    Sec = calendar:time_to_seconds(T), %% 时间点距离零时的秒数
    case next_week_diff2(WeekDays, NowDay, 0) of
        Day when Day =/= 0 -> %% 当有比当前星期大的星期数
            (Day - NowDay) * day_s() - NowSec + Sec;
        _ ->
            Day = lists:min(WeekDays),
            (7 - NowDay + Day) * day_s() - NowSec + Sec
    end.
%% 获取离日期时间最近且大的日期
next_week_diff2([], _WeekDay, AccDay) ->
    AccDay;
next_week_diff2([WeekDay | WeekDays], Day, 0) when WeekDay > Day ->
    next_week_diff2(WeekDays, Day, WeekDay);
next_week_diff2([WeekDay | WeekDays], Day, AccDay) when WeekDay > Day andalso WeekDay < AccDay ->
    next_week_diff2(WeekDays, Day, WeekDay);
next_week_diff2([_WeekDay | WeekDays], Day, AccDay) ->
    next_week_diff2(WeekDays, Day, AccDay).

%% @doc 判断两个时间戳是否为同一天
-spec is_same_day(pos_integer(), pos_integer()) -> boolean().
is_same_day(Time1, Time2) when Time1 > Time2 ->
    is_same_day(Time2, Time1);
is_same_day(Time1, Time2) when Time1 == Time2 ->
    true;
is_same_day(Time1, Time2) when Time2 - Time1 > 86400 -> %% 两个时间相差大1天
    false;
is_same_day(Time1, Time2) ->
    {Date1, _} = time_to_local_time(Time1),
    {Date2, _} = time_to_local_time(Time2),
    Date1 =:= Date2.

%% @doc 判断两个时间戳是否为同一星期
-spec is_same_week(pos_integer(), pos_integer()) -> boolean().
is_same_week(Time1, Time2) when Time1 > Time2 ->
    is_same_week(Time2, Time1);
is_same_week(Time1, Time2) when Time1 == Time2 ->
    true;
is_same_week(Time1, Time2) when Time2 - Time1 > 604800 -> %% 两个时间相差大于7天
    false;
is_same_week(Time1, Time2) ->
    calendar:iso_week_number(time_to_local_time(Time1)) =:= calendar:iso_week_number(time_to_local_time(Time2)).

%% @doc 判断两个时间戳是否为同一月份
-spec is_same_month(pos_integer(), pos_integer()) -> boolean().
is_same_month(Time1, Time2) when Time1 > Time2 ->
    is_same_month(Time2, Time1);
is_same_month(Time1, Time2) when Time1 == Time2 ->
    true;
is_same_month(Time1, Time2) when Time2 - Time1 > 2678400 -> %% 两个时间相差大于31天
    false;
is_same_month(Time1, Time2) ->
    {{Y1, M1, _}, _} = time_to_local_time(Time1),
    {{Y2, M2, _}, _} = time_to_local_time(Time2),
    Y1 =:= Y2 andalso M1 =:= M2.

%% @doc 判断两个时间戳是否为同一年
-spec is_same_year(pos_integer(), pos_integer()) -> boolean().
is_same_year(Time1, Time2) when Time1 > Time2 ->
    is_same_year(Time2, Time1);
is_same_year(Time1, Time2) when Time1 == Time2 ->
    true;
is_same_year(Time1, Time2) when Time2 - Time1 > 31622400 -> %% 两个时间相差大于366天
    false;
is_same_year(Time1, Time2) ->
    {{Y1, _, _}, _} = time_to_local_time(Time1),
    {{Y2, _, _}, _} = time_to_local_time(Time2),
    Y1 =:= Y2.

%% @doc 计算时间戳为星期几
-spec day_of_week(pos_integer()) -> 1..7.
day_of_week(Time) ->
    {Date, _} = time_to_local_time(Time),
    calendar:day_of_the_week(Date).

%% @doc 计算秒数
-spec seconds(Term) -> pos_integer() when
    Term ::
    {calendar:day(), calendar:hour(), calendar:minute(), calendar:second()} |
    calendar:time().
seconds({D, H, Min, S}) ->
    D * day_s() + H * hour_s() + Min * min_s() + S;
seconds({H, Min, S}) ->
    H * hour_s() + Min * min_s() + S.

%% @doc 判断一个年份是否是闰年
-spec is_leap_year(pos_integer()) -> boolean().
is_leap_year(Timestamp) ->
    {{Y, _, _}, _} = time_to_local_time(Timestamp),
    calendar:is_leap_year(Y).