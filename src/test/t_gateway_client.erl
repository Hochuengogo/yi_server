%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 网关客户端
%%% @end
%%% Created : 2021-10-06 18:18:41
%%%-------------------------------------------------------------------
-module(t_gateway_client).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([call/2, cast/2, info/2, start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    pack_send/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

%% 关闭
stop(Account) ->
    ProName = list_to_atom(lists:concat(["client_", binary_to_list(Account)])),
    catch info(ProName, {stop, normal}).

call(ProName, Request) ->
    ?scall(ProName, Request).

cast(ProName, Request) ->
    gen_server:cast(ProName, Request).

info(ProName, Info) ->
    ProName ! Info.

start(Type, Account) ->
    ProName = list_to_atom(lists:concat(["client_", binary_to_list(Account)])),
    gen_server:start({local, ProName}, ?MODULE, [Type, ProName, Account], []).

init([Type, ProName, Account]) ->
    ?info("[~w]开始启动", [ProName]),
    process_flag(trap_exit, true),
    Host = srv_config:get(gateway_host),
    {ok, IP} = inet:getaddr(Host, inet),
    Port = srv_config:get(gateway_port),
    Opts = srv_config:get(gateway_options),
    NewOpts0 = lists:keydelete(reuseaddr, 1, Opts),
    NewOpts = lists:keydelete(backlog, 1, NewOpts0),
    {ok, Sock} = gen_tcp:connect(IP, Port, NewOpts),
    ok = gen_tcp:send(Sock, ?game_name),
    put('@socket', Sock),
    {ok, Ref} = prim_inet:async_recv(Sock, ?gateway_packet_head_byte, -1),
    SecretKey = <<"x9%Dl%TnGX!oHKnNPN2SjluhT*Ei7OO5">>,
    Platform = srv_lib:platform(),
    Timestamp = integer_to_binary(time_util:timestamp()),
    Sign = crypto_util:md5_hex_string([Account, Platform, SecretKey, Timestamp]),
    AccountLoginData = [{<<"account">>, Account}, {<<"timestamp">>, Timestamp}, {<<"sign">>, Sign}],
    ok = pack_send(10000, AccountLoginData),
    self() ! heartbeat,
    ?info("[~w]启动完成", [ProName]),
    {ok, #gateway_client{action_type = Type, account = Account, sock = Sock, ref = Ref, read_head = true}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% 接收数据头部长度
handle_info({inet_async, Sock, Ref, {ok, <<DataSize:?gateway_packet_head_bits>>}}, Client = #gateway_client{sock = Sock, ref = Ref, read_head = true}) ->
    NewClient = Client#gateway_client{read_head = false, data_size = DataSize},
    self() ! async_recv,
    {noreply, NewClient};
%% 接收数据
handle_info({inet_async, Sock, Ref, {ok, Packet}}, Client = #gateway_client{sock = Sock, ref = Ref, read_head = false}) when byte_size(Packet) > ?gateway_packet_data_max_byte ->
    ?error("协议数据包字节超过上限，包大小：~w", [byte_size(Packet)]),
    NewClient = Client#gateway_client{read_head = true, data_size = 0},
    self() ! async_recv,
    {noreply, NewClient};
%% 接收数据
handle_info({inet_async, Sock, Ref, {ok, <<_:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, _/binary>> = Packet}}, Client = #gateway_client{sock = Sock, ref = Ref, read_head = false}) ->
    %% 处理协议解包
    Ret =
        case unpack(Packet, res) of
            {ok, Data} ->
                route(Code, Data, Client);
            {error, _Err} ->
                {noreply, Client}
        end,
    case Ret of
        {noreply, NewClient0} ->
            NewClient = NewClient0#gateway_client{read_head = true, data_size = 0},
            self() ! async_recv,
            {noreply, NewClient};
        {stop, Reason, NewClient} ->
            {stop, Reason, NewClient}
    end;
%% 接收到异常数据
handle_info({inet_async, _Sock, _Ref, {ok, Packet}}, Client) ->
    ?error("接收到错误的数据包, 数据:~w", [Packet]),
    {stop, normal, Client};

%% socket断开连接
handle_info({inet_async, Sock, _Ref, {error, closed}}, Client = #gateway_client{sock = Sock}) ->
    ?gate_debug("socket关闭, 原因:~w", [closed]),
    {stop, normal, Client};
%% socket报错
handle_info({inet_async, Sock, _Ref, {error, Reason}}, Client = #gateway_client{sock = Sock}) ->
    ?error("网络异步处理错误, 原因:~w", [Reason]),
    {stop, normal, Client};

%% 异步接收数据
handle_info(async_recv, Client = #gateway_client{sock = Sock, read_head = ReadHead, data_size = DataSize}) ->
    RecvSize = ?if_true(ReadHead, ?gateway_packet_head_byte, DataSize),
    case prim_inet:async_recv(Sock, RecvSize, -1) of
        {ok, Ref} ->
            NewClient = Client#gateway_client{ref = Ref},
            {noreply, NewClient};
        {error, _Err} ->
            ?error("Socket异步接受错误, 原因:~w", [_Err]),
            {stop, normal, Client}
    end;

%% 发送数据
handle_info({send_data, Bin}, Client = #gateway_client{sock = Sock}) when is_binary(Bin) orelse is_list(Bin) ->
    case send(Sock, Bin) of
        ok ->
            {noreply, Client};
        {error, busy} ->
            ?gate_debug("socket发送数据繁忙"),
            {stop, normal, Client};
        {error, Reason} ->
            ?error("socket发送数据错误, 原因:~w", [Reason]),
            {stop, normal, Client}
    end;

%% socket挂了
handle_info({'EXIT', Sock, Reason}, Client = #gateway_client{sock = Sock}) ->
    ?error("socket挂了, 原因:~w", [Reason]),
    {stop, normal, Client};

%% 心跳
handle_info(heartbeat, Client = #gateway_client{account = _Account}) ->
%%    ?gate_debug("账号：~ts执行心跳", [Account]),
    pack_send(10001, {}),
    {noreply, Client};

%% 关闭
handle_info({stop, Reason}, Client) ->
    ?gate_debug("关闭原因：~w", [Reason]),
    {stop, Reason, Client};

handle_info(_Info, Client) ->
    {noreply, Client}.

terminate(Reason, #gateway_client{account = Account, sock = Sock}) ->
    ProName = list_to_atom(lists:concat(["client_", binary_to_list(Account)])),
    ?info("[~w]开始关闭，原因：~w", [ProName, Reason]),
    catch inet:close(Sock),
    ?info("[~w]关闭完成", [ProName]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc socket发送数据
send(Sock, Bin) ->
    NewBin = pack_bin_head(Bin),
    gen_tcp:send(Sock, NewBin).

%% 打包数据并发送
pack_send(Code, Data) ->
    Bin = gateway_lib:pack(Code, req, Data),
    send(get('@socket'), Bin).

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
route(Code, Data, Client = #gateway_client{}) ->
    case mapping:do(Code) of
        {ok, _Parser, _ProtoMod, _RpcMod, _IsLogin} ->
            Ret = (catch t_gateway_client_rpc:handle(Code, Data, Client)),
            handle_ret(Ret, Code, Client);
        {error, _Reason} ->
            {noreply, Client};
        Err ->
            ?error("其他情况接收到协议，code：~w，错误：~w", [Code, Err]),
            {stop, normal, Client}
    end.

handle_ret(ok, _Code, Client) ->
    {noreply, Client};
handle_ret({ok, NewClient = #gateway_client{}}, _Code, _Client) ->
    {noreply, NewClient};
handle_ret({reply, Reply}, Code, Client) ->
    case pack_send(Code, Reply) of
        ok ->
            {noreply, Client};
        {error, Reason} ->
            {stop, Reason, Client}
    end;
handle_ret({reply, Reply, NewClient = #gateway_client{}}, Code, _Client) ->
    case pack_send(Code, Reply) of
        ok ->
            {noreply, NewClient};
        {error, Reason} ->
            {stop, Reason, NewClient}
    end;
handle_ret({stop, Reason}, _Code, Client) ->
    {stop, Reason, Client};
handle_ret({stop, Reason, NewClient = #gateway_client{}}, _Code, _Client) ->
    {stop, Reason, NewClient};
handle_ret({error, {bad_handle, _Code}}, _Code, Client) ->
    {noreply, Client};
handle_ret({'EXIT', Err}, Code, Client) ->
    ?error("处理协议code：~w，遇到了未处理错误：~w", [Code, Err]),
    {stop, {error, Err}, Client};
handle_ret(Ret, Code, Client) ->
    ?error("未做处理的返回格式：~w，code：~w", [Ret, Code]),
    {stop, normal, Client}.
