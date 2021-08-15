%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 网关
%%% @end
%%% Created : 2021-08-13 00:10:37
%%%-------------------------------------------------------------------
-module(gateway).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([call/2, cast/2, info/2, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    send/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

call(Pid, Request) ->
    ?scall(Pid, Request).

cast(Pid, Request) ->
    gen_server:cast(Pid, Request).

info(Pid, Info) ->
    Pid ! Info.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    ?gate_debug("[~w]开始启动", [?MODULE]),
    process_flag(trap_exit, true),
    ?gate_debug("[~w]启动完成", [?MODULE]),
    {ok, #gateway{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, handle_error}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% 正式启动网关
handle_info({start_gateway, Sock}, Gateway) ->
    {ok, Ref} = prim_inet:async_recv(Sock, ?gateway_packet_head_byte, -1),
    {ok, {Ip, Port}} = inet:peername(Sock),
    NewGateway = Gateway#gateway{sock = Sock, ip = inet:ntoa(Ip), port = Port, ref = Ref, read_head = true},
    put('@socket', Sock),
    ?gate_debug("启动网关进程成功，socket：~w，ip：~s，port：~w", [Sock, inet:ntoa(Ip), Port]),
    {noreply, NewGateway};

%% 接收数据头部长度
handle_info({inet_async, Sock, Ref, {ok, <<DataSize:?gateway_packet_head_byte>>}}, Gateway = #gateway{sock = Sock, ref = Ref, read_head = true}) ->
    NewGateway = Gateway#gateway{read_head = false, data_size = DataSize},
    self() ! async_recv,
    {noreply, NewGateway};
%% 接收数据
handle_info({inet_async, Sock, Ref, {ok, Packet}}, Gateway = #gateway{sock = Sock, ref = Ref}) when byte_size(Packet) > ?gateway_packet_data_max_byte ->
    ?error("协议数据包字节超过上限，包大小：~w", [byte_size(Packet)]),
    NewGateway = Gateway#gateway{read_head = true, data_size = 0},
    self() ! async_recv,
    {noreply, NewGateway};
%% 接收数据
handle_info({inet_async, Sock, Ref, {ok, <<_:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, _>> = Packet}}, Gateway = #gateway{sock = Sock, ref = Ref, data_size = DataSize}) when byte_size(Packet) =:= DataSize ->
    %% 处理协议解包
    Ret =
        case gateway_lib:unpack(Packet, req) of
            {ok, Data} ->
                gateway_lib:route(Code, Data, Gateway);
            {error, _Err} ->
                {noreply, Gateway}
        end,
    case Ret of
        {noreply, NewGateway0} ->
            NewGateway = NewGateway0#gateway{read_head = true, data_size = 0},
            self() ! async_recv,
            {noreply, NewGateway};
        wait_role ->
            NewGateway = Gateway#gateway{read_head = true, data_size = 0},
            {noreply, NewGateway};
        {stop, Reason, NewGateway} ->
            {stop, Reason, NewGateway}
    end;
%% 接收到异常数据
handle_info({inet_async, _Sock, _Ref, {ok, Packet}}, Gateway) ->
    ?error("接收到错误的数据包, 数据:~w", [Packet]),
    {stop, normal, Gateway};

%% socket断开连接
handle_info({inet_async, Sock, _Ref, {error, closed}}, Gateway = #gateway{sock = Sock}) ->
    {stop, normal, Gateway};
%% socket报错
handle_info({inet_async, Sock, _Ref, {error, Reason}}, Gateway = #gateway{sock = Sock}) ->
    ?error("网络异步处理错误, 原因:~w", [Reason]),
    {stop, normal, Gateway};

%% 异步接收数据
handle_info(async_recv, Gateway = #gateway{sock = Sock, read_head = ReadHead, data_size = DataSize}) ->
    RecvSize = ?if_true(ReadHead, ?gateway_packet_head_byte, DataSize),
    case prim_inet:async_recv(Sock, RecvSize, -1) of
        {ok, Ref} ->
            NewGateway = Gateway#gateway{ref = Ref},
            {noreply, NewGateway};
        {error, _Err} ->
            ?error("Socket异步接受错误, 原因:~w", [_Err]),
            {stop, normal, Gateway}
    end;

%% 发送数据
handle_info({send_data, Bin}, Gateway = #gateway{sock = Sock}) ->
    case send(Sock, Bin) of
        ok ->
            {noreply, Gateway};
        {error, busy} ->
            ?gate_debug("socket发送数据繁忙"),
            {stop, normal, Gateway};
        {error, Reason} ->
            ?error("socket发送数据错误, 原因:~w", [Reason]),
            {stop, normal, Gateway}
    end;

%% 发送数据回复成功
handle_info({inet_reply, _Sock, ok}, Gateway) ->
    {noreply, Gateway};

%% 发送数据回复失败
handle_info({inet_reply, _Sock, {error, closed}}, Gateway) ->
    {stop, normal, Gateway};
%% 发送数据回复失败
handle_info({inet_reply, _Sock, {error, Reason}}, Gateway) ->
    ?error("发送数据回复失败，原因：~w", [Reason]),
    {stop, normal, Gateway};

%% socket挂了
handle_info({'EXIT', Sock, Reason}, Gateway = #gateway{sock = Sock}) ->
    ?error("socket挂了, 原因:~w", [Reason]),
    {stop, normal, Gateway};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #gateway{sock = CSock}) ->
    ?gate_debug("[~w]开始关闭，原因：~w", [?MODULE, Reason]),
    catch inet:close(CSock),
    ?gate_debug("[~w]关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc socket发送数据
send(Sock, Bin) ->
    try NewBin = pack_bin_head(Bin), erlang:port_command(Sock, NewBin, []) of
        false -> % Port busy and nosuspend option passed
            {error, busy};
        true ->
            ok
    catch
        error:_Error ->
            ?error("发送数据到socket报错，原因：~w", [_Error]),
            {error, einval}
    end.

%% @doc 数据加包头
pack_bin_head(Bins) when is_list(Bins) ->
    list_to_binary([pack_bin_head(Bin) || Bin <- Bins]);
pack_bin_head(Bin) ->
    <<(byte_size(Bin)):?gateway_packet_head_bits, Bin/binary>>.
