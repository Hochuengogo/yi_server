%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 全局变量、锁处理
%%%
%%% @end
%%% Created : 08. 三月 2020 18:41
%%%-------------------------------------------------------------------
-module(config).
-author("huangzaoyi").

-behaviour(gen_server).

%% API
-export([get/1, get/2, set/2, save/2, lock/1, unlock/1]).
-export([start_link/0, call/1, cast/1, info/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("common.hrl").
-include("logs.hrl").

-define(config_filepath, "./setting.config").

-record(state, {}).

%% @doc 获取配置
-spec get(term()) -> term() | undefined.
get(Key) ->
    case ets:lookup(config_val, Key) of
        [{_, Val}] ->
            Val;
        _ ->
            undefined
    end.

%% @doc 获取配置
-spec get(term(), term()) -> term().
get(Key, Default) ->
    case ets:lookup(config_val, Key) of
        [{_, Val}] ->
            Val;
        _ ->
            Default
    end.

%% @doc 设置配置
-spec set(term(), term()) -> ok.
set(Key, Val) ->
    ets:insert(config_val, {Key, Val}),
    ok.

%% @doc 设置配置并保持到磁盘
-spec save(term(), term()) -> ok.
save(Key, Val) ->
    ets:insert(config_val, {Key, Val}),
    dets:insert(config_val, {Key, Val}),
    ok.

%% @doc 设置锁
-spec lock(term()) -> boolean().
lock(Key) ->
    case call({lock, Key}) of
        true ->
            true;
        _ ->
            false
    end.

%% @doc 解除锁
-spec unlock(term()) -> ok.
unlock(Key) ->
    info({unlock, Key}),
    ok.

call(Call) ->
    ?scall(?MODULE, Call).

cast(Cast) ->
    gen_server:cast(?MODULE, Cast).

info(Info) ->
    ?MODULE ! Info.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info(?start_begin),
    process_flag(trap_exit, true),
    %% 用于保存系统配置或者全局变量
    ets:new(config_val, [named_table, set, public, {keypos, 1}]),
    dets:open_file(config_val, [{file, "./dets/config_val.dets"}, {type, set}, {keypos, 1}]),
    dets:to_ets(config_val, config_val),
    %% 用于处理锁
    ets:new(config_lock, [named_table, set, {keypos, 1}]),
    load(),
    ?info(?start_end),
    {ok, #state{}}.

%% 加载配置
load() ->
    {ok, Terms} = file:consult(?config_filepath),
    Fun = fun({Key, Val}) -> set(Key, Val) end,
    lists:foreach(Fun, Terms).

%% 加锁
handle_call({lock, Key}, _From, State) ->
    case ets:insert_new(config_lock, {Key, true}) of
        true ->
            {reply, true, State};
        _ ->
            {reply, false, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% 释放锁
handle_info({unlock, Key}, State) ->
    ets:delete(config_lock, Key),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?info(?stop_begin),
    ?info(?stop_end),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



