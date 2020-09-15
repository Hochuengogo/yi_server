%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2019, <COMPANY>
%%% @doc 测试网关客户端
%%%
%%% @end
%%% Created : 27. 三月 2019 08:36
%%%-------------------------------------------------------------------
-module(t_gateway_client).
-author("huangzaoyi").

-behaviour(gen_server).

%% API
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

start() ->
    gen_server:start(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    ListenPort = config:get(gateway_port),
    Opts = config:get(gateway_options),
    {ok, Sock} = gen_tcp:connect("localhost", ListenPort, [{packet, 2} | Opts]),
    {ok, _} = prim_inet:async_recv(Sock, 0, -1),
    sock_print(Sock, connect),
    put(receiving, true), %% 将正在接收socket数据标识设置为true
    Gateway = #gateway{sock = Sock},
    put('@socket', Sock), %% 方便获取socket
    erlang:send_after(?heartbeat_interval * 1000, self(), check_heartbeat),
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
            ?error("处理错误, 消息:~w, State:~w, Reason:~w", [_Info, State, _Err]),
            {stop, normal, State}
    end.

terminate(_Reason, #gateway{sock = Sock}) ->
    sock_print(Sock, close),
    catch inet:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 接收到数据
do_handle_info({inet_async, Sock, _Ref, {ok, <<Code:16, Bin/binary>>}}, Gateway = #gateway{sock = Sock}) ->
    put(receiving, false), %% 将正在接收socket数据标识设置为false
    Ret =
        case gateway:unpack(Code, req, Bin) of
            {ok, Term} ->
                gateway:route(Code, Term, Gateway);
            {error, _Err} -> %% 记录解包出错次数
                ErrorTimes = misc_lib:get(unpack_error_times, 0),
                NewErrorTimes = ErrorTimes + 1,
                case NewErrorTimes >= ?max_unpack_error_times of
                    false ->
                        put(unpack_error_times, NewErrorTimes),
                        {noreply, Gateway};
                    _ ->
                        {stop, normal, Gateway}
                end
        end,
    case Ret of
        {noreply, NewGateway} ->
            case misc_lib:get(proto_msg_num, 0) < ?max_recv_proto_msg_num of
                true -> %% 小于未处理协议消息最大数量，接收socket数据
                    self() ! async_recv;
                _ ->
                    ok
            end,
            {noreply, NewGateway};
        {stop, Reason, NewGateway} ->
            {stop, Reason, NewGateway}
    end;

do_handle_info({inet_async, _Sock, _Ref, {ok, Packet}}, Gateway) ->
    ?error("接收到错误的数据包, 数据:~w", [Packet]),
    {stop, normal, Gateway};

%% socket断开连接
do_handle_info({inet_async, Sock, _Ref, {error, closed}}, Gateway = #gateway{sock = Sock}) ->
    {stop, normal, Gateway};

%% socket报错
do_handle_info({inet_async, Sock, _Ref, {error, Reason}}, Gateway = #gateway{sock = Sock}) ->
    ?error("网络异步处理错误, 原因:~w", [Reason]),
    {stop, normal, Gateway};

%% 异步接收数据
do_handle_info(async_recv, Gateway = #gateway{sock = Sock}) ->
    case prim_inet:async_recv(Sock, 0, -1) of
        {ok, _} ->
            put(receiving, true),
            {noreply, Gateway};
        {error, _Err} ->
            ?error("Socket异步接受错误, 原因:~w", [_Err]),
            {stop, normal, Gateway}
    end;

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
do_handle_info({inet_reply, _Sock, {error, closed}}, Gateway) ->
    {stop, normal, Gateway};

%% 发送数据回复失败
do_handle_info({inet_reply, _Sock, Status}, Gateway) ->
    ?error("发送数据回复失败，原因：~w", [Status]),
    {stop, normal, Gateway};

%% 读取下一条协议消息
do_handle_info(read_next, Gateway = #gateway{role_pid = RolePid}) ->
    put(read_next, true),
    case misc_lib:get(proto_msg_list, []) of
        [ProtoMsg | ProtoMsgList] ->
            RolePid ! ProtoMsg,
            put(proto_msg_list, ProtoMsgList),
            put(proto_msg_num, length(ProtoMsgList)),
            put(read_next, false);
        _ ->
            ok
    end,
    case misc_lib:get(proto_msg_num, 0) < ?max_recv_proto_msg_num of
        true -> %% 小于未处理协议消息最大数量，接收socket数据
            get(receiving) == false andalso self() ! async_recv;
        _ ->
            ok
    end,
    {noreply, Gateway};

%% socket的port挂了
do_handle_info({'EXIT', Sock, Reason}, Gateway = #gateway{sock = Sock}) ->
    {stop, Reason, Gateway};

%% 心跳检测
do_handle_info(check_heartbeat, Gateway) ->
    case time_lib:now() > misc_lib:get(heartbeat, 0) + ?heartbeat_interval of
        false ->
            erlang:send_after(?heartbeat_interval * 1000, self(), check_heartbeat),
            {noreply, Gateway};
        _ ->
            ?gate_debug("心跳检测超时"),
            {stop, normal, Gateway}
    end;

do_handle_info(_Info, State) ->
    {noreply, State}.

%% 打印信息
sock_print(Sock, connect) ->
    {ok, {IP, Port}} = inet:peername(Sock),
    StrIP = inet:ntoa(IP),
    put(ip, IP),
    put(port, Port),
    ?gate_debug("Socket连接，IP：~s，Port：~w", [StrIP, Port]);
sock_print(_Sock, close) ->
    IP = get(ip),
    StrIP = inet:ntoa(IP),
    Port = get(port),
    ?gate_debug("Socket断开连接，IP：~s，Port：~w", [StrIP, Port]).