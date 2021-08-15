%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 网关管理进程
%%% @end
%%% Created : 2021-08-12 10:18:03
%%%-------------------------------------------------------------------
-module(gateway_mgr).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([call/1, cast/1, info/1, apply/2, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").

-record(state, {}).

call(Request) ->
    ?scall(?MODULE, Request).

cast(Request) ->
    gen_server:cast(?MODULE, Request).

info(Info) ->
    ?MODULE ! Info.

apply(sync, MFA = {_M, _F, _A}) ->
    call({apply, MFA});
apply(sync, FA = {_F, _A}) ->
    call({apply, FA});
apply(async, MFA = {_M, _F, _A}) ->
    info({apply, MFA});
apply(async, FA = {_F, _A}) ->
    info({apply, FA}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info("[~w]开始启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(gateway_socket, [named_table, public, set, {keypos, 1}]),
    ?info("[~w]启动完成", [?MODULE]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    case catch do_handle_call(Request, From, State) of
        {reply, Reply, NewState} ->
             {reply, Reply, NewState};
        _Err ->
            ?error("handle_call错误，消息:~w，State:~w，Reason:~w", [Request, State, _Err]),
            {reply, {error, handle_error}, State}
    end.

handle_cast(Request, State) ->
    case catch do_handle_cast(Request, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("handle_cast错误，消息:~w，State:~w，Reason:~w", [Request, State, _Err]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("handle_info错误，消息:~w，State:~w，Reason:~w", [Info, State, _Err]),
            {noreply, State}
    end.

terminate(Reason, _State) ->
    ?info("[~w]开始关闭，原因：~w", [?MODULE, Reason]),
    ?info("[~w]关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_call({apply, {M, F, A}}, _From, State) ->
    case catch util:apply({M, F, [State | A]}) of
        {ok, Reply} ->
            {reply, Reply, State};
        {reply, Reply} ->
            {reply, Reply, State};
        {ok, Reply, NewState} ->
            {reply, Reply, NewState};
        {reply, Reply, NewState} ->
            {reply, Reply, NewState};
        _Err ->
            ?error("同步执行~w:~w:~w错误，State:~w，Reason:~w", [M, F, A, State, _Err]),
            {reply, {error, apply_error}, State}
    end;
do_handle_call({apply, {F, A}}, _From, State) ->
    case catch util:apply({F, [State | A]}) of
        {ok, Reply} ->
            {reply, Reply, State};
        {reply, Reply} ->
            {reply, Reply, State};
        {ok, Reply, NewState} ->
            {reply, Reply, NewState};
        {reply, Reply, NewState} ->
            {reply, Reply, NewState};
        _Err ->
            ?error("同步执行~w:~w错误，State:~w，Reason:~w", [F, A, State, _Err]),
            {reply, {error, apply_error}, State}
    end;

do_handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

do_handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_info({apply, {M, F, A}}, State) ->
    case catch util:apply({M, F, [State | A]}) of
        ok ->
            {noreply, State};
        {ok, NewState} ->
            {noreply, NewState};
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("异步执行~w:~w:~w错误，State:~w，Reason:~w", [M, F, A, State, _Err]),
            {noreply, State}
    end;
do_handle_info({apply, {F, A}}, State) ->
    case catch util:apply({F, [State | A]}) of
        ok ->
            {noreply, State};
        {ok, NewState} ->
            {noreply, NewState};
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("异步执行~w:~w错误，State:~w，Reason:~w", [F, A, State, _Err]),
            {noreply, State}
    end;

%% 启动网关
do_handle_info({start_gateway, CSock}, State) ->
    start_gateway(CSock),
    {noreply, State};

%% 网关进程挂了
do_handle_info({'EXIT', Pid, _Reason}, State) ->
    ets:delete(gateway_socket, Pid),
    {noreply, State};

do_handle_info(_Info, State) ->
    {noreply, State}.

%% 启动网关
start_gateway(CSock) ->
    case ets:info(gateway_socket, size) < srv_config:get(gateway_num) of
        true ->
            Opts = srv_config:get(gateway_options),
            NewOpts =
                lists:filter(
                    fun({reuseaddr, _}) -> false;
                        ({backlog, _}) -> false;
                        (_Opt) -> true
                    end, Opts
                ),
            case inet:setopts(CSock, NewOpts) of
                ok ->
                    case gateway:start_link() of
                        {ok, Pid} ->
                            case gen_tcp:controlling_process(CSock, Pid) of
                                ok ->
                                    gateway:info(Pid, {start_gateway, CSock}),
                                    ets:insert(gateway_socket, {Pid, CSock});
                                _Err ->
                                    ?error("交接socket进程失败, socket:~w, pid:~w, 返回:~w", [CSock, Pid, _Err]),
                                    exit(Pid, normal),
                                    catch inet:close(CSock)
                            end;
                        _Err ->
                            ?error("启动worker进程失败，返回:~w", [_Err]),
                            catch inet:close(CSock)
                    end;
                _Err ->
                    ?error("设置socket参数失败, 返回:~w, socket:~w, 参数:~w", [_Err, CSock, NewOpts]),
                    catch inet:close(CSock)
            end;
        _ ->
            catch inet:close(CSock)
    end.