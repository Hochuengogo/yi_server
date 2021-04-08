%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 服务器时间管理进程
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

-define(SERVER, ?MODULE).

-record(state, {}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?info(?start_begin),
    process_flag(trap_exit, true),
    %% 时间变量缓存
    ets:new(srv_time_cache, [named_table, set, public, {keypos, 1}]),
    %% 日期时间缓存
    ets:new(srv_datetime_cache, [named_table, set, public, {keypos, 1}]),
    load_time_cache(),
    {NextHour, NextHourDiff} = get_next_hour_info(),
    erlang:send_after(NextHourDiff * 1000, self(), {next_hour, NextHour}),
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
do_handle_info({next_hour, 0}, State) ->
    ?info("0点更新"),
    load_time_cache(),
    srv_config:save(last_update_zero, get_time_cache(zero)),
    %% TODO 0点触发的事件
    {NextHour, NextHourDiff} = get_next_hour_info(),
    erlang:send_after(NextHourDiff * 1000, self(), {next_hour, NextHour}),
    {noreply, State};

%% 5点更新
do_handle_info({next_hour, 5}, State) ->
    ?info("5点更新"),
    srv_config:save(last_update_day_five, get_time_cache(day_five)),
    %% TODO 5点触发的事件
    {NextHour, NextHourDiff} = get_next_hour_info(),
    erlang:send_after(NextHourDiff * 1000, self(), {next_hour, NextHour}),
    {noreply, State};

%% 其他整点
do_handle_info({next_hour, Hour}, State) ->
    ?info("~w点更新", [Hour]),
    %% TODO x点触发的时间
    {NextHour, NextHourDiff} = get_next_hour_info(),
    erlang:send_after(NextHourDiff * 1000, self(), {next_hour, NextHour}),
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
    ets:insert(srv_time_cache, {date, time_util:date()}).

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
