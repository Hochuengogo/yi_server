%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 三月 2020 01:38
%%%-------------------------------------------------------------------
-module(gateway_acceptor).
-author("huangzaoyi").

-behaviour(gen_server).

-export([start_link/2, start_acceptor/2]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

pro_name(Num) ->
    list_to_atom(lists:concat(["gateway_acceptor_", Num])).

start_acceptor(LSock, Num) ->
    Name = pro_name(Num),
    Acceptor = #{
        id => Name,
        start => {?MODULE, start_link, [LSock, Name]},
        restart => transient,
        shutdown => 10000,
        type => worker,
        modules => [?MODULE]
    },
    supervisor:start_child(gateway_acceptor_sup, Acceptor).

start_link(LSock, Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [LSock, Name], []).

init([LSock, Name]) ->
    ?info(?start_begin(Name)),
    process_flag(trap_exit, true),
    %% 异步监听客户端的连接
    {ok, Ref} = prim_inet:async_accept(LSock, -1),
    Acceptor = #gateway_acceptor{lsock = LSock, ref = Ref, name = Name},
    ?info(?start_end(Name)),
    {ok, Acceptor}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    case catch do_handle_info(_Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};
        _Err ->
            ?error("处理错误, 消息:~w, State:~w, Reason:~w", [_Info, State, _Err]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    Name = erase(acceptor_name),
    ?info(?stop_begin(Name)),
    ?info(?stop_end(Name)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 接收socket连接
do_handle_info(accept, Acceptor = #gateway_acceptor{lsock = LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, NewRef} ->
            NewAcceptor = Acceptor#gateway_acceptor{ref = NewRef},
            {noreply, NewAcceptor};
        {error, Reason} when Reason =:= emfile orelse Reason =:= enfile ->
            ?warn("网络接收连接处理错误, 原因:~w", [Reason]),
            erlang:send_after(1000, self(), accept),
            {noreply, Acceptor};
        {error, closed} ->
            {stop, normal, Acceptor};
        {error, Reason} ->
            {stop, Reason, Acceptor}
    end;

%% 正常接收到一个socket
do_handle_info({inet_async, LSock, Ref, {ok, CSock}}, Acceptor = #gateway_acceptor{lsock = LSock, ref = Ref}) ->
    {ok, Mod} = inet_db:lookup_socket(LSock),
    inet_db:register_socket(CSock, Mod),
    atk(CSock),
    self() ! accept,
    {noreply, Acceptor};
%% 异常接收到一个socket
do_handle_info({inet_async, _LSock, Ref, {ok, CSock}}, Acceptor) ->
    ?error("异常接收到socket建立连接，引用：~w，socket：~w，acceptor：~w", [Ref, CSock, Acceptor]),
    catch inet:close(CSock),
    self() ! accept,
    {noreply, Acceptor};

%% socket 关闭
do_handle_info({inet_async, LSock, _Ref, {error, closed}}, Acceptor = #gateway_acceptor{lsock = LSock}) ->
    {stop, normal, Acceptor};
%% socket资源不足
do_handle_info({inet_async, LSock, _Ref, {error, Reason}}, Acceptor = #gateway_acceptor{lsock = LSock}) when Reason =:= emfile orelse Reason =:= enfile ->
    ?warn("网络异步处理错误, 原因:~w", [Reason]),
    erlang:send_after(1000, self(), accept),
    {noreply, Acceptor};
%% socket 报错
do_handle_info({inet_async, LSock, _Ref, {error, Reason}}, Acceptor = #gateway_acceptor{lsock = LSock}) ->
    ?error("网络异步处理错误, 原因:~w", [Reason]),
    {stop, Reason, Acceptor};

do_handle_info(_Info, State) ->
    {noreply, State}.

%% 握手
atk(CSock) ->
    case gen_tcp:recv(CSock, 0, 1000) of
        {ok, ?game_name} ->
            start_gateway(CSock);
        _ ->
            catch inet:close(CSock)
    end.

%% 启动网关
start_gateway(CSock) ->
    case gateway:start() of
        {ok, Pid} ->
            case gen_tcp:controlling_process(CSock, Pid) of
                ok ->
                    Pid ! {start_gateway, CSock};
                _Err ->
                    ?error("交接socket进程失败, socket:~w, pid:~w, 返回:~w", [CSock, Pid, _Err]),
                    exit(Pid, normal),
                    catch inet:close(CSock)
            end;
        _Err ->
            ?error("启动worker进程失败，返回:~w", [_Err]),
            catch inet:close(CSock)
    end.