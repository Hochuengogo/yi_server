%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关监听器
%%%
%%% @end
%%% Created : 16. 三月 2020 00:49
%%%-------------------------------------------------------------------
-module(gateway_listener).
-author("huangzaoyi").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info(?start_begin),
    process_flag(trap_exit, true),
    Host = srv_config:get(gateway_host),
    {ok, IP} = inet:getaddr(Host, inet),
    Port = srv_config:get(gateway_port),
    Opts = srv_config:get(gateway_options),
    {ok, LSock} = gen_tcp:listen(Port, [{ip, IP}, {packet, 0}| Opts]),
    {ok, {Ip, _}} = inet:sockname(LSock),
    ?info("网关监听 IP:~s, 端口:~w", [inet:ntoa(Ip), Port]),

    Num = srv_config:get(acceptor_num),
    spawn_acceptor(LSock, Num),
    ?info(?start_end),
    {ok, #gateway_listener{lsock = LSock}}.

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
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_info(_Info, State) ->
    {noreply, State}.

%% 创建接收器
spawn_acceptor(_LSock, 0) -> ok;
spawn_acceptor(LSock, Num) ->
    {ok, _Pid} = gateway_acceptor:start_acceptor(LSock, Num),
    spawn_acceptor(LSock, Num - 1).