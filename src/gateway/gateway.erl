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
-export([call/2, cast/2, info/2, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    send/2
    , unpack/2
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

start() ->
    gen_server:start(?MODULE, [], []).

init([]) ->
%%    ?gate_debug("[~w]开始启动", [?MODULE]),
    process_flag(trap_exit, true),
%%    ?gate_debug("[~w]启动完成", [?MODULE]),
    {ok, #gateway{pid = self()}}.

handle_call(_Request, _From, State) ->
    {reply, {error, handle_error}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% 正式启动网关
handle_info({start_gateway, Sock}, Gateway) ->
    {ok, Ref} = prim_inet:async_recv(Sock, ?gateway_packet_head_byte, -1),
    {ok, {Ip, Port}} = inet:peername(Sock),
    NewGateway = Gateway#gateway{sock = Sock, ip = Ip, port = Port, ref = Ref, read_head = true},
    put('@socket', Sock),
    put(last_heartbeat, time_util:timestamp()),
    erlang:send_after(?gateway_heartbeat_interval * 1000, self(), check_heartbeat),
    ?gate_debug("启动网关进程成功，socket：~w，ip：~s，port：~w", [Sock, inet:ntoa(Ip), Port]),
    {noreply, NewGateway};

%% 接收数据头部长度
handle_info({inet_async, Sock, Ref, {ok, <<DataSize:?gateway_packet_head_bits>>}}, Gateway = #gateway{sock = Sock, ref = Ref, read_head = true}) ->
    NewGateway = Gateway#gateway{read_head = false, data_size = DataSize},
    self() ! async_recv,
    {noreply, NewGateway};
%% 接收数据
handle_info({inet_async, Sock, Ref, {ok, Packet}}, Gateway = #gateway{sock = Sock, ref = Ref, read_head = false}) when byte_size(Packet) > ?gateway_packet_data_max_byte ->
    ?error("协议数据包字节超过上限，包大小：~w", [byte_size(Packet)]),
    NewGateway = Gateway#gateway{read_head = true, data_size = 0},
    self() ! async_recv,
    {noreply, NewGateway};
%% 接收数据
handle_info({inet_async, Sock, Ref, {ok, <<Flag:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>> = Packet}}, Gateway = #gateway{sock = Sock, ref = Ref, read_head = false}) ->
    %% 处理协议解包
    Ret =
        case unpack(Packet, req) of
            {ok, Data} ->
                case get(proto_control) of
                    true ->
                        print_recv(Flag, Code, Data, Bin);
                    _ ->
                        skip
                end,
                route(Code, Data, Gateway);
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
handle_info({inet_async, Sock, Ref, {ok, Packet}}, Gateway) ->
    ?error("接收到错误的数据包, socket：~w，引用：~w，数据:~w，gateway：~w", [Sock, Ref, Packet, Gateway]),
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
        {error, Err} ->
            ?error("Socket异步接受错误, gateway：~w，原因:~w", [Gateway, Err]),
            {stop, normal, Gateway}
    end;

%% 发送数据
handle_info({send_data, Bin}, Gateway = #gateway{sock = Sock}) when is_binary(Bin) orelse is_list(Bin) ->
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

%% 心跳检测
handle_info(check_heartbeat, Gateway = #gateway{account = Account, ip = Ip, port = Port}) ->
    Now = time_util:timestamp(),
    case Now > get(last_heartbeat) + ?gateway_heartbeat_interval of
        false ->
            erlang:send_after(?gateway_heartbeat_interval * 1000, self(), check_heartbeat),
            {noreply, Gateway};
        _ ->
            ?gate_debug("账号：~ts，Ip：~w，端口：~w，网关心跳检测失败", [Account, inet:ntoa(Ip), Port]),
            {stop, normal, Gateway}
    end;

%% 关闭
handle_info({stop, Reason}, Gateway) ->
    {stop, Reason, Gateway};

%% 角色进程挂了
handle_info({'EXIT', _RolePid, Reason}, Gateway) ->
    ?error("角色进程挂了, 原因:~w", [Reason]),
    {stop, normal, Gateway};

%% 协议监控
handle_info({proto_control, Flag}, Gateway) ->
    put(proto_control, Flag),
    {noreply, Gateway};

handle_info(_Info, Gateway) ->
    {noreply, Gateway}.

terminate(Reason, #gateway{sock = CSock, account = Account, role_id = RoleId}) ->
    ?gate_debug("网关进程开始关闭，账号：~ts，角色Id：~p，原因：~w", [Account, RoleId, Reason]),
    catch inet:close(CSock),
    ?gate_debug("网关进程关闭完成，账号：~ts，角色Id：~p", [Account, RoleId]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc socket发送数据
send(Sock, Bin) ->
    try
        NewBin = pack_bin_head(Bin),
        case get(proto_control) of
            true ->
                print_send(Bin);
            _ ->
                skip
        end,
        erlang:port_command(Sock, NewBin, [])
    of
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
pack_bin_head(<<>>) ->
    <<>>;
pack_bin_head(Bin) ->
    <<(byte_size(Bin)):?gateway_packet_head_bits, Bin/binary>>.

%% @doc 解包协议数据
-spec unpack(binary(), req | res) -> {ok, term()} | {error, term()}.
unpack(<<0:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>>, Flag) ->
    gateway_lib:unpack(Code, Flag, Bin);
unpack(<<1:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>>, Flag) ->
    gateway_lib:unpack(Code, Flag, util:uncompress(Bin));
unpack(Packet, Flag) ->
    ?error("解包失败，错误的数据包：~w，标识：~w", [Packet, Flag]),
    {error, {bad_packet, Packet, Flag}}.

%% @doc 路由
route(Code, Data, Gateway = #gateway{role_pid = RolePid, is_login = IsLogin}) ->
    case mapping:do(Code) of
        {ok, gateway, _ProtoMod, RpcMod, false} ->
            Ret = (catch RpcMod:handle(Code, Data, Gateway)),
            handle_ret(Ret, Code, Gateway);
        {ok, gateway, _ProtoMod, RpcMod, true} when IsLogin ->
            Ret = (catch RpcMod:handle(Code, Data, Gateway)),
            handle_ret(Ret, Code, Gateway);
        {ok, role, _ProtoMod, RpcMod, true} when IsLogin andalso is_pid(RolePid) ->
            RolePid ! {rpc, RpcMod, Code, Data},
            wait_role;
        {error, _Reason} ->
            {noreply, Gateway};
        Err ->
            ?error("其他情况接收到协议，code：~w，is_login：~w，role_pid：~w，错误：~w", [Code, IsLogin, RolePid, Err]),
            {stop, normal, Gateway}
    end.

handle_ret(ok, _Code, Gateway) ->
    {noreply, Gateway};
handle_ret({ok, NewGateway = #gateway{}}, _Code, _Gateway) ->
    {noreply, NewGateway};
handle_ret({reply, Reply}, Code, Gateway) ->
    case gateway_lib:pack_send(Code, Reply) of
        ok ->
            {noreply, Gateway};
        {error, Reason} ->
            {stop, Reason, Gateway}
    end;
handle_ret({reply, Reply, NewGateway = #gateway{}}, Code, _Gateway) ->
    case gateway_lib:pack_send(Code, Reply) of
        ok ->
            {noreply, NewGateway};
        {error, Reason} ->
            {stop, Reason, NewGateway}
    end;
handle_ret({stop, Reason}, _Code, Gateway) ->
    {stop, Reason, Gateway};
handle_ret({stop, Reason, NewGateway = #gateway{}}, _Code, _Gateway) ->
    {stop, Reason, NewGateway};
handle_ret({error, {bad_handle, _Code}}, _Code, Gateway) ->
    {noreply, Gateway};
handle_ret({'EXIT', Err}, Code, Gateway) ->
    ?error("处理协议code：~w，遇到了未处理错误：~w", [Code, Err]),
    {stop, {error, Err}, Gateway};
handle_ret(Ret, Code, Gateway) ->
    ?error("未做处理的返回格式：~w，code：~w", [Ret, Code]),
    {stop, normal, Gateway}.

%% 打印协议接收输出
print_recv(0, Code, Data, Bin) ->
    ?print_line("协议接收，协议号：~w，协议数据：~w，二进制数据：~w", [Code, Data, Bin]);
print_recv(1, Code, Data, Bin) ->
    ?print_line("协议接收，协议号：~w，协议数据：~w，二进制数据：~w", [Code, Data, util:uncompress(Bin)]).

%% 打印协议发送输出
print_send([]) -> ok;
print_send([Bin | Bins]) ->
    print_send(Bin),
    print_send(Bins);
print_send(<<>>) ->
    ok;
print_send(<<0:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>>) ->
    {ok, Data} = gateway_lib:unpack(Code, res, Bin),
    ?print_line("协议发送，协议号：~w，协议数据：~w，二进制数据：~w", [Code, Data, Bin]);
print_send(<<1:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>>) ->
    NewBin = util:uncompress(Bin),
    {ok, Data} = gateway_lib:unpack(Code, res, NewBin),
    ?print_line("协议发送，协议号：~w，协议数据：~w，二进制数据：~w", [Code, Data, NewBin]).