%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关接口
%%%
%%% @end
%%% Created : 25. 7月 2020 11:00 下午
%%%-------------------------------------------------------------------
-module(t_client).
-author("huangzaoyi").

%% API
-export([
    pack_send/3, send/2, sock_send/2, sock_pack_send/3,
    pack/3, unpack/3,
    route/3
]).

-include("logs.hrl").
-include("gateway.hrl").

%% @doc 打包并发送数据给网关进程
-spec pack_send(pid(), pos_integer(), term()) -> ok.
pack_send(ClientPid, Code, Term) when is_pid(ClientPid) ->
    {ok, Bin} = pack(Code, req, Term),
    ClientPid ! {send_data, Bin};
pack_send(_ClientPid, _Code, _Term) ->
    ok.

%% @doc 发送数据给网关进程
-spec send(pid(), binary()) -> ok.
send(ClientPid, Bin) when is_pid(ClientPid) andalso is_binary(Bin) ->
    ClientPid ! {send_data, Bin};
send(ClientPid, BinList) when is_pid(ClientPid) andalso is_list(BinList) ->
    ClientPid ! {send_data, BinList};
send(_ClientPid, _Bin) ->
    ok.

%% @doc socket发送数据
-spec sock_send(port(), binary() | list()) -> ok | {error, term()}.
sock_send(Sock, Bin) when is_binary(Bin) ->
    sock_send(Sock, [Bin]);
sock_send(Sock, List) ->
    case get(delay_send_ref) of
        Ref when is_reference(Ref) -> %% 如果之前socket已经busy，则直接放到延时发送列表中
            SendList = misc_lib:get(delay_send_list, []),
            NewSendList = SendList ++ List,
            put(delay_send_list, NewSendList),
            ok;
        _ ->
            SendList =
                case erase(delay_send_list) of
                    SendList0 when is_list(SendList0) ->
                        SendList0;
                    _ ->
                        []
                end,
            sock_send2(Sock, SendList ++ List)
    end.
%% 发送数据
sock_send2(_Sock, []) ->
    ok;
sock_send2(Sock, [Bin | List] = SendList) ->
    case sock_send2(Sock, Bin) of
        ok ->
            sock_send2(Sock, List);
        {error, busy} ->
            case length(SendList) >= ?max_send_proto_msg_num of %% 当发不出数据给socket，并且待发送协议数据数量达到上限
                false ->
                    put(delay_send_list, SendList),
                    Ref = erlang:send_after(10, self(), delay_send_data), %% 10ms后再尝试发送数据
                    put(delay_send_ref, Ref),
                    ok;
                _ ->
                    {error, busy}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
sock_send2(Sock, Bin) when is_port(Sock) andalso is_binary(Bin) ->
    try erlang:port_command(Sock, Bin, []) of
        false -> % Port busy and nosuspend option passed
            {error, busy};
        true ->
            ok
    catch
        error:_Error ->
            ?error("发送数据到socket报错，原因：~w", [_Error]),
            {error, einval}
    end;
sock_send2(_Sock, _BadData) ->
    ?error("发送数据错误，数据：~w", [_BadData]),
    ok.

%% @doc 打包数据并通过socket发送数据
-spec sock_pack_send(port(), pos_integer(), term()) -> ok | {error, term()}.
sock_pack_send(Sock, Code, Term) when is_port(Sock) ->
    case pack(Code, req, Term) of
        {ok, Bin} ->
            sock_send(Sock, Bin);
        _ ->
            ok
    end;
sock_pack_send(_Sock, _Code, _Term) ->
    ok.

%% @doc 打包数据
-spec pack(pos_integer(), atom(), term()) -> {ok, binary()} | {error, term()}.
pack(Code, Flag, Term) ->
    case mapping:do(Code) of
        {ok, ProtoMod, _RpcMod} ->
            case catch ProtoMod:pack(Code, Flag, Term) of
                {'EXIT', _Err} ->
                    ?error("打包数据错误, 原因:~w, 协议号:~w, 协议模块:~w, 标识:~w, 数据:~w", [_Err, Code, ProtoMod, Flag, Term]),
                    {error, {pack_error, Code}};
                Bin ->
                    Size = byte_size(Bin),
                    case Size =< ?max_packet_size of
                        true ->
                            NewBin = <<Code:16, Bin/binary>>,
                            {ok, NewBin};
                        _ ->
                            ?error("协议包长度异常，长度：~w，协议号：~w，数据：~w", [Size, Code, Term]),
                            {error, packet_size_error}
                    end
            end;
        {error, _Err} ->
            {error, _Err}
    end.

%% @doc 解包数据
-spec unpack(pos_integer(), atom(), binary()) -> {ok, term()} | {error, term()}.
unpack(Code, Flag, Bin) ->
    Size = byte_size(Bin),
    case Size =< ?max_packet_size of
        true ->
            case mapping:do(Code) of
                {ok, ProtoMod, _RpcMod} ->
                    case catch ProtoMod:unpack(Code, Flag, Bin) of
                        {'EXIT', _Err} ->
                            ?error("解包数据错误, 原因:~w, 协议号:~w, 协议模块:~w, 标识:~w, 数据:~w", [_Err, Code, ProtoMod, Flag, Bin]),
                            {error, {unpack_error, Code}};
                        Term ->
                            {ok, Term}
                    end;
                {error, _Err} ->
                    {error, _Err}
            end;
        _ ->
            ?error("协议包长度异常，长度：~w，协议号：~w，数据：~w", [Size, Code, Bin]),
            {error, packet_size_error}
    end.

%% @doc 路由
-spec route(pos_integer(), term(), #gateway_client{}) -> {noreply, #gateway_client{}} | {stop, term(), #gateway_client{}}.
route(Code, Term, Client = #gateway_client{is_login = IsLogin}) ->
    case mapping:do(Code) of
        {ok, _ProtoMod, gateway_rpc} -> %% 如果是网关协议处理
            case catch t_client_rpc:handle(Code, Term, Client) of
                Ret ->
                    handle_ret(Ret, Code, Client)
            end;
        {ok, _ProtoMod, RpcMod} ->
            case IsLogin of
                true ->
                    List = [ProtoMsg | ProtoMsgList] = misc_lib:get(proto_msg_list, []) ++ [{rpc, Code, Term, list_to_atom(lists:concat(["t_", RpcMod]))}],
                    case misc_lib:get(read_next, true) of
                        true ->
                            self() ! ProtoMsg,
                            put(proto_msg_list, ProtoMsgList),
                            put(proto_msg_num, length(ProtoMsgList)),
                            put(read_next, false);
                        _ ->
                            put(proto_msg_list, List),
                            put(proto_msg_num, length(List))
                    end,
                    {noreply, Client};
                _ ->
                    ?error("未登录就发非网关处理协议过来"),
                    {stop, normal, Client}
            end;
        {error, _Err} ->
            {noreply, Client}
    end.

%% 处理网关rpc返回
handle_ret(ok, _Code, Client) -> %% 正常返回
    {noreply, Client};
handle_ret({ok, NewClient = #gateway_client{}}, _Code, _CLient) -> %% 正常返回，并更新gateway
    {noreply, NewClient};
handle_ret({reply, Data}, Code, Client = #gateway_client{sock = Sock}) -> %% 回复消息
    case sock_pack_send(Sock, Code, Data) of
        ok ->
            {noreply, Client};
        {error, _Err} ->
            {stop, normal, Client}
    end;
handle_ret({reply, Data, NewClient = #gateway_client{sock = Sock}}, Code, _Client) -> %% 回复消息，并更新gateway
    case sock_pack_send(Sock, Code, Data) of
        ok ->
            {noreply, NewClient};
        {error, _Err} ->
            {stop, normal, NewClient}
    end;
handle_ret({error, Reason}, _Code, Client) -> %% 错误返回，并关闭进程
    {stop, Reason, Client};
handle_ret({false, Reason}, _Code, Client) -> %% 返回错误消息，并关闭进程
    %% todo 飘提示
    {stop, Reason, Client};
handle_ret({false, Reason, NewClient = #gateway_client{}}, _Code, _Client) -> %% 返回错误消息，更新gateway，并关闭进程
    %% todo 飘提示
    {stop, Reason, NewClient};
handle_ret({stop, Reason}, _Code, Client) -> %% 关闭进程
    {stop, Reason, Client};
handle_ret({stop, Reason, NewClient = #gateway_client{}}, _Code, _Client) -> %% 更新gateway，并关闭进程
    {stop, Reason, NewClient};
handle_ret({'EXIT', _Err}, Code, Client) -> %% 处理报错了
    ?error("网关rpc执行出错, 错误:~w, 代码:~w，gateway_client：~w", [_Err, Code, Client]),
    {noreply, Client};
handle_ret(Ret, Code, Client) -> %% 返回格式错误
    ?error("网关rpc错误返回格式，返回：~w，代码：~w，gateway_client：~w", [Ret, Code, Client]),
    {noreply, Client}.