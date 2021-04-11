%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 3月 2021 7:32 下午
%%%-------------------------------------------------------------------
-module(util).
-author("huangzaoyi").

%% API
-export([
    get/2
    , set_timer/3
    , set_timer/4
    , set_ms_timer/3
    , set_ms_timer/4
    , unset_timer/1
    , have_timer/1
    , gc/0
    , gc/1
    , rand/1
    , rand/2
    , compress/1
    , uncompress/1
    , check_cd/2
    , term_to_list/1
    , list_to_term/1
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
    Timers = ?MODULE:get('@timers', []),
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
    Timers = ?MODULE:get('@timers', []),
    case lists:keyfind(Name, 1, Timers) of
        {Name, Ref} ->
            catch erlang:cancel_timer(Ref),
            put('@timers', lists:keydelete(Name, 1, Timers));
        _ ->
            ok
    end.

%% @doc 是否有某个定时器
-spec have_timer(term()) -> boolean().
have_timer(Name) ->
    lists:keymember(Name, 1, ?MODULE:get('@timers', [])).

%% @doc 手动gc
-spec gc() -> boolean().
gc() ->
    gc(self()).
-spec gc(pid()) -> boolean().
gc(Pid) ->
    case erlang:process_info(Pid, status) of
        {status, waiting} ->
            erlang:garbage_collect(Pid);
        _ ->
            false
    end.

%% @doc 压缩
-spec compress(binary()) -> binary().
compress(Bin) ->
    zlib:compress(Bin).

%% @doc 解压
-spec uncompress(binary()) -> binary().
uncompress(Bin) ->
    zlib:uncompress(Bin).

%% 生成随机数
-spec rand(pos_integer()) -> pos_integer().
rand(Max) when is_integer(Max) andalso Max > 0 ->
    rand(1, Max).
-spec rand(pos_integer(), pos_integer()) -> pos_integer().
rand(Max, Min) when Max > Min ->
    rand(Min, Max);
rand(Min, Max) when is_integer(Min) andalso Min > 0 andalso is_integer(Max) andalso Max > 0 ->
    rand:uniform(Max - Min + 1) + Min - 1.

%% @doc 检测cd
-spec check_cd(term(), pos_integer()) -> boolean().
check_cd(Key, Ms) ->
    Cd = ?MODULE:get({'@cd', Key}, 0),
    Now = time_util:timestamp(ms),
    case Now >= Cd of
        true ->
            put({'@cd', Key}, Now + Ms),
            true;
        _ ->
            false
    end.

%% @doc erlang结构转字符串
-spec term_to_list(term()) -> list().
term_to_list(Term) ->
    lists:flatten(io_lib:format("~w", [Term])).

%% @doc 字符串转erlang结构
-spec list_to_term(list()) -> term().
list_to_term(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.