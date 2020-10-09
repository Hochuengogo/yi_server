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
-export([start_many/1, start/1, stop_many/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

-define(client_opts, [
    binary,
    {active, false},
    {nodelay, true},
    {delay_send, true},
    {send_timeout, 30000},
    {send_timeout_close, true},
    {exit_on_close, false}
]).

start_many(N) when is_integer(N) andalso N > 0 ->
    start(N),
    start_many(N - 1);
start_many(_N) ->
    ok.

stop_many(N) when is_integer(N) andalso N > 0 ->
    stop(N),
    stop_many(N - 1);
stop_many(_N) ->
    ok.

start(N) ->
    gen_server:start({local, list_to_atom(lists:concat(["client_", N]))}, ?MODULE, [], []).

stop(N) ->
    case erlang:whereis(list_to_atom(lists:concat(["client_", N]))) of
        Pid when is_pid(Pid) ->
            exit(Pid, normal);
        _ ->
            ok
    end.

init([]) ->
    process_flag(trap_exit, true),
    ListenHost = config:get(gateway_host),
    {ok, ListenIP} = inet:getaddr(ListenHost, inet),
    ListenPort = config:get(gateway_port),
    {ok, Sock} = gen_tcp:connect(ListenIP, ListenPort, [{packet, 2} | ?client_opts]),
    {ok, _} = prim_inet:async_recv(Sock, 0, -1),
    sock_print(Sock, connect),
    put(receiving, true), %% 将正在接收socket数据标识设置为true
    Client = #gateway_client{sock = Sock},
    put('@socket', Sock), %% 方便获取socket
    self() ! heartbeat,
    {ok, Client}.

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

terminate(_Reason, #gateway_client{sock = Sock}) ->
    sock_print(Sock, close),
    catch inet:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 接收到数据
do_handle_info({inet_async, Sock, _Ref, {ok, <<Code:16, Bin/binary>>}}, Client = #gateway_client{sock = Sock}) ->
    put(receiving, false), %% 将正在接收socket数据标识设置为false
    Ret =
        case t_client:unpack(Code, res, Bin) of
            {ok, Term} ->
                t_client:route(Code, Term, Client);
            {error, _Err} -> %% 记录解包出错次数
                ErrorTimes = misc_lib:get(unpack_error_times, 0),
                NewErrorTimes = ErrorTimes + 1,
                case NewErrorTimes >= ?max_unpack_error_times of
                    false ->
                        put(unpack_error_times, NewErrorTimes),
                        {noreply, Client};
                    _ ->
                        {stop, normal, Client}
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

do_handle_info({inet_async, _Sock, _Ref, {ok, Packet}}, Client) ->
    ?error("接收到错误的数据包, 数据:~w", [Packet]),
    {stop, normal, Client};

%% socket断开连接
do_handle_info({inet_async, Sock, _Ref, {error, closed}}, Client = #gateway_client{sock = Sock}) ->
    {stop, normal, Client};

%% socket报错
do_handle_info({inet_async, Sock, _Ref, {error, Reason}}, Client = #gateway_client{sock = Sock}) ->
    ?error("网络异步处理错误, 原因:~w", [Reason]),
    {stop, normal, Client};

%% 异步接收数据
do_handle_info(async_recv, Client = #gateway_client{sock = Sock}) ->
    case prim_inet:async_recv(Sock, 0, -1) of
        {ok, _} ->
            put(receiving, true),
            {noreply, Client};
        {error, _Err} ->
            ?error("Socket异步接受错误, 原因:~w", [_Err]),
            {stop, normal, Client}
    end;

%% 发送数据
do_handle_info({send_data, Bin}, Client = #gateway_client{sock = Sock}) ->
    case t_client:sock_send(Sock, Bin) of
        {error, _Err} ->
            ?error("发送数据错误，原因：~w", [_Err]),
            {stop, normal, Client};
        _ ->
            {noreply, Client}
    end;

%% 延迟发送数据
do_handle_info(delay_send_data, Client = #gateway_client{sock = Sock}) ->
    ?debug("进入延迟发送数据处理"),
    erase(delay_send_ref),
    case t_client:sock_send(Sock, []) of
        {error, _Err} ->
            ?error("发送数据错误，原因：~w", [_Err]),
            {stop, normal, Client};
        _ ->
            {noreply, Client}
    end;

%% 发送数据回复成功
do_handle_info({inet_reply, _Sock, ok}, Client) ->
    {noreply, Client};

%% 发送数据回复失败
do_handle_info({inet_reply, _Sock, {error, closed}}, Client) ->
    {stop, normal, Client};

%% 发送数据回复失败
do_handle_info({inet_reply, _Sock, Status}, Client) ->
    ?error("发送数据回复失败，原因：~w", [Status]),
    {stop, normal, Client};

%% 读取下一条协议消息
do_handle_info(read_next, Client) ->
    put(read_next, true),
    case misc_lib:get(proto_msg_list, []) of
        [ProtoMsg | ProtoMsgList] ->
            self() ! ProtoMsg,
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
    {noreply, Client};

%%
do_handle_info({'EXIT', _Pid, Reason}, Client) ->
    {stop, Reason, Client};

%% 发送心跳
do_handle_info(heartbeat, Client = #gateway_client{sock = Sock}) ->
    t_client:sock_pack_send(Sock, 10000, {}),
    erlang:send_after(2000, self(), heartbeat),
    {noreply, Client};

do_handle_info(_Info, State) ->
    {noreply, State}.

%% 打印信息
sock_print(Sock, connect) ->
    {ok, {IP, Port}} = inet:peername(Sock),
    StrIP = inet:ntoa(IP),
    put(ip, IP),
    put(port, Port),
    ?gate_debug("Socket连接到网关，IP：~s，Port：~w", [StrIP, Port]);
sock_print(_Sock, close) ->
    IP = get(ip),
    StrIP = inet:ntoa(IP),
    Port = get(port),
    ?gate_debug("Socket断开连接网关，IP：~s，Port：~w", [StrIP, Port]).