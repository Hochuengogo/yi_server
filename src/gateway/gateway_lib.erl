%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 网关库
%%% @end
%%% Created : 2021-08-15 14:02:06
%%%-------------------------------------------------------------------
-module(gateway_lib).
-author("jiaoyinyi").

%% API
-export([
    pack_send/2, pack_send/3, send/2,
    pack/3, unpack/2, unpack/3,
    route/3
]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

%% @doc 打包发送数据给网关进程
-spec pack_send(pid(), pos_integer(), term()) -> void().
pack_send(GPid, Code, Data) ->
    {ok, Bin} = pack(Code, res, Data),
    GPid ! {send_data, Bin}.
%% @doc 打包发送数据 只能在网关进程使用
-spec pack_send(pos_integer(), term()) -> ok | {error, term()}.
pack_send(Code, Data) ->
    {ok, Bin} = pack(Code, res, Data),
    Sock = get('@socket'),
    gateway:send(Sock, Bin).

%% @doc 发送数据给网关进程
-spec send(pid(), binary()) -> void().
send(GPid, Bin) ->
    GPid ! {send_data, Bin}.

%% @doc 打包协议数据
-spec pack(pos_integer(), req | res, term()) -> {ok, binary()} | {error, term()}.
pack(Code, Flag, Data) ->
    case mapping:do(Code) of
        {ok, ProtoMod, _RpcMod, _IsLogin} ->
            case catch ProtoMod:pack(Code, Flag, Data) of
                Bin when is_binary(Bin) ->
                    NewBin =
                        case byte_size(Bin) > ?gateway_packet_compress_min_byte of
                            false ->
                                <<0:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>>;
                            _ ->
                                <<1:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, (util:compress(Bin))/binary>>
                        end,
                    {ok, NewBin};
                {error, Reason} ->
                    {error, Reason};
                {'EXIT', Err} ->
                    ?error("打包数据错误，code：~w，标识：~w，数据：~w，错误：~w", [Code, Flag, Data, Err]),
                    {error, {pack_error, {Code, Flag, Data, Err}}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 解包协议数据
-spec unpack(binary(), req | res) -> {ok, term()} | {error, term()}.
unpack(<<0:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>>, Flag) ->
    unpack(Code, Flag, Bin);
unpack(<<1:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>>, Flag) ->
    unpack(Code, Flag, util:uncompress(Bin));
unpack(Packet, Flag) ->
    ?error("解包失败，错误的数据包：~w，标识：~w", [Packet, Flag]),
    {error, {bad_packet, Packet, Flag}}.
%% @doc 解包协议数据
-spec unpack(pos_integer(), req | res, binary()) -> {ok, term()} | {error, term()}.
unpack(Code, Flag, Bin) ->
    case mapping:do(Code) of
        {ok, ProtoMod, _RpcMod, _IsLogin} ->
            case catch ProtoMod:unpack(Code, Flag, Bin) of
                {error, Reason} ->
                    {error, Reason};
                {'EXIT', Err} ->
                    ?error("解包数据错误，code：~w，标识：~w，数据：~w，错误：~w", [Code, Flag, Bin, Err]),
                    {error, {unpack_error, {Code, Flag, Bin, Err}}};
                Data ->
                    {ok, Data}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 路由
route(Code, Data, Gateway = #gateway{role_pid = RolePid, is_login = IsLogin}) ->
    case mapping:do(Code) of
        {ok, _ProtoMod, RpcMod, false} ->
            Ret = (catch RpcMod:handle(Code, Data, Gateway)),
            handle_ret(Ret, Code, Gateway);
        {ok, _ProtoMod, RpcMod, true} when IsLogin ->
            RolePid ! {rpc, RpcMod, Code, Data},
            wait_role;
        {ok, _ProtoMod, _RpcMod, true} ->
            ?error("在未登录时发送了code：~w协议过来", [Code]),
            {stop, normal, Gateway};
        {error, _Reason} ->
            {noreply, Gateway}
    end.

handle_ret(ok, _Code, Gateway) ->
    {noreply, Gateway};
handle_ret({ok, NewGateway = #gateway{}}, _Code, _Gateway) ->
    {noreply, NewGateway};
handle_ret({reply, Reply}, Code, Gateway) ->
    case gateway:send(Code, Reply) of
        ok ->
            {noreply, Gateway};
        {error, Reason} ->
            {stop, Reason, Gateway}
    end;
handle_ret({reply, Reply, NewGateway = #gateway{}}, Code, _Gateway) ->
    case gateway:send(Code, Reply) of
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