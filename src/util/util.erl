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
    , set_timer/3, set_timer/4, set_ms_timer/3, set_ms_timer/4, unset_timer/1, clear_timer/1, have_timer/1
    , gc/0, gc/1
    , rand/1, rand/2
    , compress/1, uncompress/1
    , check_cd/2
    , term_to_string/1, term_to_string2/1, string_to_term/1
    , async_apply/4, handle_async_timeout/1, handle_async_return/2
    , apply/3
    , start_timer/3, cancel_timer/1
    , cn/1
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
set_timer(Name, Secs, Dest, Msg) ->
    set_ms_timer(Name, Secs * 1000, Dest, Msg).
-spec set_ms_timer(term(), pos_integer(), term()) -> term().
set_ms_timer(Name, MSecs, Msg) ->
    set_ms_timer(Name, MSecs, self(), Msg).
-spec set_ms_timer(term(), pos_integer(), pid(), term()) -> term().
set_ms_timer(Name, MSecs, Dest, Msg) ->
    case get('@timers') of
        undefined ->
            put('@timers', [{Name, start_timer(MSecs, Dest, Msg)}]);
        Timers ->
            case lists:keyfind(Name, 1, Timers) of
                {_, Ref} ->
                    cancel_timer(Ref),
                    put('@timers', lists:keyreplace(Name, 1, Timers, {Name, start_timer(MSecs, Dest, Msg)}));
                _ ->
                    put('@timers', [{Name, start_timer(MSecs, Dest, Msg)} | Timers])
            end
    end.

%% @doc 取消设置定时器
-spec unset_timer(term()) -> term().
unset_timer(Name) ->
    case get('@timers') of
        undefined ->
            ok;
        Timers ->
            case lists:keyfind(Name, 1, Timers) of
                {_, Ref} ->
                    cancel_timer(Ref),
                    put('@timers', lists:keydelete(Name, 1, Timers));
                _ ->
                    ok
            end
    end.

%% @doc 清除定时器，不管定时器是否已经触发
-spec clear_timer(term()) -> term().
clear_timer(Name) ->
    case get('@timers') of
        undefined ->
            ok;
        Timers ->
            put('@timers', lists:keydelete(Name, 1, Timers))
    end.

%% @doc 是否有某个定时器
-spec have_timer(term()) -> boolean().
have_timer(Name) ->
    case get('@timers') of
        undefined ->
            false;
        Timers ->
            lists:keymember(Name, 1, Timers)
    end.

%% @doc 手动gc
-spec gc() -> boolean().
gc() ->
    gc(self()).
-spec gc(pid()) -> boolean().
gc(Pid) ->
    erlang:garbage_collect(Pid).

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
-spec term_to_string(term()) -> list().
term_to_string(Term) ->
    io_lib:format("~w", [Term]).

%% @doc erlang结构转字符串，并将pid替换成undefined
-spec term_to_string2(term()) -> list().
term_to_string2(Term) ->
    Str = io_lib:format("~w", [Term]),
    re:replace(Str, "<[0-9]{1,4},[0-9]{1,4},[0-9]{1,4}>", "undefined").

%% @doc 字符串转erlang结构
-spec string_to_term(list()) -> term().
string_to_term(String) when is_binary(String) ->
    string_to_term(binary_to_list(String));
string_to_term(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

%% @doc 异步调用，有超时时间、回调
%% 执行该方法的进程需要处理 {'@async_apply_return', Idx, Ret} 和 {'@async_apply_timeout', Idx} 消息
-spec async_apply(pid(), mfa(), pos_integer(), mfa()) -> ok.
async_apply(Pid, MFA = {_, _, _}, Timeout, CallBack = {_, _, _}) ->
    Idx = util:get('@async_apply_idx', 1),
    put('@async_apply_idx', Idx + 1),
    Ref = erlang:send_after(Timeout, self(), {'@async_apply_timeout', Idx}),
    Pid ! {'@async_apply', MFA, self(), Idx},
    put({'@async_apply_data', Idx}, {Ref, MFA, CallBack}).

%% @doc 异步调用，超时处理
-spec handle_async_timeout(pos_integer()) -> ok.
handle_async_timeout(Idx) ->
    case erase({'@async_apply_data', Idx}) of
        {_Ref, _MFA, _CallBack = {M, F, A}} ->
            catch erlang:apply(M, F, [timeout | A]);
        _ ->
            skip
    end.

%% @doc 异步调用，正常返回处理
-spec handle_async_return(pos_integer(), term()) -> ok.
handle_async_return(Idx, Ret) ->
    case erase({'@async_apply_data', Idx}) of
        {Ref, _MFA, _CallBack = {M, F, A}} ->
            erlang:cancel_timer(Ref),
            catch erlang:apply(M, F, [Ret | A]);
        _ ->
            skip
    end.

%% @doc 执行方法
-spec apply(module(), atom(), list()) -> any().
apply(undefined, F, A) ->
    erlang:apply(F, A);
apply(M, F, A) ->
    erlang:apply(M, F, A).

%% @doc 启动定时器 超时消息格式{timeout, Ref, Msg}
-spec start_timer(pos_integer(), pid(), term()) -> reference().
start_timer(Time, Dest, Msg) ->
    erlang:start_timer(Time, Dest, Msg).

%% @doc 取消定时器
-spec cancel_timer(reference()) -> pos_integer() | false.
cancel_timer(Ref) when is_reference(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            receive {timeout, Ref, _Msg} ->
                0
            after 0 ->
                false
            end;
        Time ->
            Time
    end;
cancel_timer(_Ref) ->
    false.

%% @doc 在控制台输出中文
-spec cn(binary()) -> ok.
cn(Bin) ->
    io:format("~ts", [Bin]).