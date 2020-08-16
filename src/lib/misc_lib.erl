%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 杂项库
%%% @end
%%% Created : 05. 一月 2020 13:08
%%%-------------------------------------------------------------------
-module(misc_lib).
-author("huangzaoyi").

%% API
-export([
    get/2,
    gc/0,
    set_timer/3, set_timer/4,
    set_ms_timer/3, set_ms_timer/4,
    unset_timer/1, has_timer/1,
    apply/3
]).

%% @doc 获取进程字典值
-spec get(term(), term()) -> term().
get(Key, Default) ->
    case get(Key) of
        undefined ->
            Default;
        Val ->
            Val
    end.

%% @doc 手动gc
-spec gc() -> ok.
gc() ->
    case erlang:process_info(self(), status) of
        {status, waiting} ->
            erlang:garbage_collect(self());
        _ ->
            ok
    end.

%% @doc 设置定时器
-spec set_timer(term(), pos_integer(), term()) -> term().
set_timer(Name, Secs, Msg) ->
    set_ms_timer(Name, Secs * 1000, self(), Msg).
-spec set_timer(term(), pos_integer(), pid(), term()) -> term().
set_timer(Name, Secs, To, Msg) ->
    set_ms_timer(Name, Secs * 1000, To, Msg).
-spec set_ms_timer(term(), pos_integer(), term()) -> term().
set_ms_timer(Name, MSecs, Msg) ->
    set_ms_timer(Name, MSecs, self(), Msg).
-spec set_ms_timer(term(), pos_integer(), pid(), term()) -> term().
set_ms_timer(Name, MSecs, To, Msg) ->
    Timers = get('@timers', []),
    case lists:keyfind(Name, 1, Timers) of
        {Name, Ref} ->
            catch erlang:cancel_timer(Ref);
        _ ->
            ok
    end,
    NewRef = erlang:send_after(MSecs, To, Msg),
    put('@timers', lists:keystore(Name, 1, Timers, {Name, NewRef})).

%% @doc 取消设置定时器
-spec unset_timer(term()) -> term().
unset_timer(Name) ->
    Timers = get('@timers', []),
    case lists:keyfind(Name, 1, Timers) of
        {Name, Ref} ->
            catch erlang:cancel_timer(Ref),
            put('@timers', lists:keydelete(Name, 1, Timers));
        _ ->
            ok
    end.

%% @doc 是否有某个定时器
-spec has_timer(term()) -> boolean().
has_timer(Name) ->
    lists:member(Name, get('@timers', [])).

%% @doc 执行方法
-spec apply(module() | undefined, atom() | function(), list()) -> term().
apply(undefined, F, A) ->
    erlang:apply(F, A);
apply(M, F, A) ->
    erlang:apply(M, F, A).