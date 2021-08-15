%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 服务器时间管理进程 是否需要等待其他进程执行完0点更新或5点更新，由其他进程决定。可以使用异步，或者带超时的同步
%%% @end
%%%-------------------------------------------------------------------
-module(srv_time).

-behaviour(gen_server).

-export([
    get_time_cache/1
    , get_time_cache/2
    , get_datetime_cache/1
    , set_datetime_cache/2
]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-include("common.hrl").
-include("logs.hrl").

-record(state, {}).

%% 0点触发的事件 {M,F,A}
zero_fire_events(zone) ->
    [];
zero_fire_events(center) ->
    [].

%% 5点触发的事件 {M,F,A}
five_fire_events(zone) ->
    [];
five_fire_events(center) ->
    [].

%% 获取时间缓存时间戳
-spec get_time_cache(zero | day_five | date) -> term() | undefined.
get_time_cache(Key) ->
    case ets:lookup(srv_time_cache, Key) of
        [{_, Val}] ->
            Val;
        _ ->
            undefined
    end.
%% 获取时间缓存时间戳
-spec get_time_cache(zero | day_five | date, term()) -> term().
get_time_cache(Key, Default) ->
    case ets:lookup(srv_time_cache, Key) of
        [{_, Val}] ->
            Val;
        _ ->
            Default
    end.

%% 获取日期时间缓存时间戳
-spec get_datetime_cache(calendar:datetime()) -> pos_integer() | undefined.
get_datetime_cache(DateTime) ->
    case ets:lookup(srv_datetime_cache, DateTime) of
        [{_, Timestamp}] ->
            Timestamp;
        _ ->
            undefined
    end.

%% 获取日期时间缓存时间戳
-spec set_datetime_cache(calendar:datetime(), pos_integer()) -> true.
set_datetime_cache(DateTime, Timestamp) ->
    ets:insert(srv_datetime_cache, {DateTime, Timestamp}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info(?start_begin),
    process_flag(trap_exit, true),
    %% 时间变量缓存
    ets:new(srv_time_cache, [named_table, set, public, {keypos, 1}]),
    %% 日期时间缓存
    ets:new(srv_datetime_cache, [named_table, set, public, {keypos, 1}]),
    load_time_cache(),
    {NextHour, NextHourDiff} = get_next_hour_info(),
    util:start_timer(NextHourDiff * 1000, self(), {next_hour, NextHour}),
    ?info(?start_end),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    case catch do_handle_info(_Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("处理错误, 消息:~w, State:~w, Reason:~w", [_Info, State, _Err]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ?info(?stop_begin),
    ?info(?stop_end),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 0点更新
do_handle_info({timeout, _Ref, {next_hour, 0}}, State) ->
    ?info("0点更新"),
    {NextHour, NextHourDiff} = get_next_hour_info(),
    util:start_timer(NextHourDiff * 1000, self(), {next_hour, NextHour}),
    load_time_cache(),
    Zero = get_time_cache(zero),
    case Zero > srv_config:get(last_update_zero, 0) of
        true ->
            zero_flush(zero_fire_events(srv_lib:server_type()), get_time_cache(week_day)),
            srv_config:save(last_update_zero, Zero),
            ?info("0点更新执行完成");
        _ ->
            ?error("0点更新执行异常，0点时间戳：~w", [Zero])
    end,
    {noreply, State};

%% 5点更新
do_handle_info({timeout, _Ref, {next_hour, 5}}, State) ->
    ?info("5点更新"),
    {NextHour, NextHourDiff} = get_next_hour_info(),
    util:start_timer(NextHourDiff * 1000, self(), {next_hour, NextHour}),
    load_time_cache(),
    DayFive = get_time_cache(day_five),
    case DayFive > srv_config:get(last_update_day_five, 0) of
        true ->
            five_flush(five_fire_events(srv_lib:server_type()), get_time_cache(week_day)),
            srv_config:save(last_update_day_five, DayFive),
            ?info("5点更新执行完成");
        _ ->
            ?error("5点更新执行异常，5点时间戳：~w", [DayFive])
    end,
    {noreply, State};

%% 其他整点
do_handle_info({timeout, _Ref, {next_hour, Hour}}, State) ->
    ?info("~w点更新", [Hour]),
    {NextHour, NextHourDiff} = get_next_hour_info(),
    util:start_timer(NextHourDiff * 1000, self(), {next_hour, NextHour}),
    load_time_cache(),
    {noreply, State};

do_handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 加载时间缓存
load_time_cache() ->
    ets:insert(srv_time_cache, {zero, time_util:timestamp(zero)}),
    ets:insert(srv_time_cache, {day_five, time_util:timestamp(day_five)}),
    ets:insert(srv_time_cache, {date, time_util:date()}),
    ets:insert(srv_time_cache, {week_day, time_util:day_of_week()}).

%% 获取下一个小时信息
get_next_hour_info() ->
    {H, Min, Sec} = time_util:time(),
    NextHour =
        case H =:= 23 of
            true ->
                0;
            _ ->
                H + 1
        end,
    NextHourDiff = ?hour_s - ?min_s(Min) - Sec,
    {NextHour, NextHourDiff}.

%% 0点更新
zero_flush([], _WeekDay) ->
    ok;
zero_flush([Func | Funcs], WeekDay) ->
    zero_flush(Func, WeekDay),
    zero_flush(Funcs, WeekDay);
zero_flush({M, F, A}, WeekDay) ->
    case catch erlang:apply(M, F, [WeekDay | A]) of
        ok ->
            ok;
        _Err ->
            ?error("0点更新执行~w:~w:~w错误，原因：~w", [M, F, A, _Err])
    end.

%% 5点更新
five_flush([], _WeekDay) ->
    ok;
five_flush([Func | Funcs], WeekDay) ->
    five_flush(Func, WeekDay),
    five_flush(Funcs, WeekDay);
five_flush({M, F, A}, WeekDay) ->
    case catch erlang:apply(M, F, [WeekDay | A]) of
        ok ->
            ok;
        _Err ->
            ?error("5点更新执行~w:~w:~w错误，原因：~w", [M, F, A, _Err])
    end.