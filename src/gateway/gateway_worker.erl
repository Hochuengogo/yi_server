%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2019, <COMPANY>
%%% @doc 网关worker
%%%
%%% @end
%%% Created : 27. 三月 2019 08:36
%%%-------------------------------------------------------------------
-module(gateway_worker).
-author("huangzaoyi").

-behaviour(gen_server).

%% API
-export([start_worker/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

start_worker(CSock) ->
    Worker = #{
        id => ?MODULE,
        start => {?MODULE, start_link, [CSock]},
        restart => temporary,
        type => worker,
        shutdown => 10000
    },
    supervisor:start_child(gateway_worker_sup, [Worker]).

start_link(CSock) ->
    gen_server:start_link(?MODULE, [CSock], []).

init([Sock]) ->
    process_flag(trap_exit, true),
    Opts = config:get(gateway_options),
    ok = inet:setopts(Sock, [{packet, 2} | Opts]),
    ok = gen_tcp:controlling_process(Sock, self()),
    {ok, _} = prim_inet:async_recv(Sock, 0, -1),
    Gateway = #gateway{sock = Sock},
    put('@socket', Sock), %% 方便获取
    {ok, Gateway}.

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
            ?error("处理错误, 消息:~w, State:~w, Reason:~w, Stacktrace:~w", [_Info, State, _Err, erlang:get_stacktrace()]),
            {noreply, State}
    end.

terminate(_Reason, #gateway{sock = Sock}) ->
    catch inet:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 接收到数据
do_handle_info({inet_async, Sock, _Ref, {ok, <<Code:32, Bin/binary>>}}, Gateway = #gateway{sock = Sock}) ->
    Ret =
        case gateway:unpack(Code, req, Bin) of
            {ok, Term} ->
                gateway:route(Code, Term, Gateway);
            {error, _Err} -> %% 记录解包出错次数
                {noreply, Gateway}
        end,
    case Ret of
        {noreply, NewGateway} ->
            case prim_inet:async_recv(Sock, 0, -1) of
                {ok, _} ->
                    {noreply, NewGateway};
                {error, _Err} ->
                    ?error("Socket异步接受错误, 原因:~w", [_Err]),
                    {stop, normal, NewGateway}
            end;
        {stop, Reason, NewGateway} ->
            {stop, Reason, NewGateway}
    end;

do_handle_info({inet_async, _Sock, _Ref, {ok, Packet}}, Gateway) ->
    ?error("接收到错误的数据包, 数据:~w", [Packet]),
    {noreply, Gateway};

%% socket断开连接
do_handle_info({inet_async, Sock, _Ref, {error, closed}}, Gateway = #gateway{sock = Sock}) ->
    {ok, {IP, Port}} = inet:peername(Sock),
    StrIP = inet:ntoa(IP),
    ?info("Socket断开连接, IP:~s, Port:~w", [StrIP, Port]),
    {stop, normal, Gateway};

%% socket报错
do_handle_info({inet_async, Sock, _Ref, {error, Reason}}, Gateway = #gateway{sock = Sock}) ->
    ?error("网络异步处理错误, 原因:~w", [Reason]),
    {stop, normal, Gateway};

%% 发送数据
do_handle_info({send_data, Bin}, Gateway = #gateway{sock = Sock}) ->
    case gateway:sock_send(Sock, Bin) of
        {error, _Err} ->
            ?error("发送数据错误，原因：~w", [_Err]),
            {stop, normal, Gateway};
        _ ->
            {noreply, Gateway}
    end;

%% 延迟发送数据
do_handle_info(delay_send_data, Gateway = #gateway{sock = Sock}) ->
    ?debug("进入延迟发送数据处理"),
    erase(delay_send_ref),
    case gateway:sock_send(Sock, []) of
        {error, _Err} ->
            ?error("发送数据错误，原因：~w", [_Err]),
            {stop, normal, Gateway};
        _ ->
            {noreply, Gateway}
    end;

%% 发送数据回复成功
do_handle_info({inet_reply, _Sock, ok}, Gateway) ->
    {noreply, Gateway};

%% 发送数据回复失败
do_handle_info({inet_reply, _Sock, Status}, Gateway) ->
    ?error("发送数据回复失败，原因：~w", [Status]),
    {stop, normal, Gateway};

do_handle_info(_Info, State) ->
    {noreply, State}.

