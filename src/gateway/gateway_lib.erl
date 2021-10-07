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
    pack/3, unpack/3,
    to_m_gateway/1
]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").

%% @doc 打包发送数据给网关进程
-spec pack_send(pid(), pos_integer(), term()) -> void().
pack_send(GatePid, Code, Data) ->
    Bin = pack(Code, res, Data),
    send(GatePid, Bin).
%% @doc 打包发送数据 只能在网关进程使用
-spec pack_send(pos_integer(), term()) -> ok | {error, term()}.
pack_send(Code, Data) ->
    Bin = pack(Code, res, Data),
    Sock = get('@socket'),
    gateway:send(Sock, Bin).

%% @doc 发送数据给网关进程
-spec send(pid(), binary()) -> void().
send(GatePid, Bin) ->
    GatePid ! {send_data, Bin}.

%% @doc 打包协议数据
-spec pack(pos_integer(), req | res, term()) -> binary().
pack(Code, Flag, Data) ->
    case mapping:do(Code) of
        {ok, _Parser, ProtoMod, _RpcMod, _IsLogin} ->
            case catch ProtoMod:pack(Code, Flag, Data) of
                Bin when is_binary(Bin) ->
                    case byte_size(Bin) > ?gateway_packet_compress_min_byte of
                        false ->
                            <<0:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, Bin/binary>>;
                        _ ->
                            <<1:?gateway_packet_compress_bits, Code:?gateway_packet_code_bits, (util:compress(Bin))/binary>>
                    end;
                {error, _Reason} ->
                    <<>>;
                {'EXIT', Err} ->
                    ?error("打包数据错误，code：~w，标识：~w，数据：~w，错误：~w", [Code, Flag, Data, Err]),
                    <<>>
            end;
        {error, _Reason} ->
            <<>>
    end.

%% @doc 解包协议数据
-spec unpack(pos_integer(), req | res, binary()) -> {ok, term()} | {error, term()}.
unpack(Code, Flag, Bin) ->
    case mapping:do(Code) of
        {ok, _Parser, ProtoMod, _RpcMod, _IsLogin} ->
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

%% 转成m_gateway结构
to_m_gateway(#gateway{pid = GatePid, sock = Sock, ip = Ip, port = Port}) ->
    #m_gateway{
        pid = GatePid
        , sock = Sock
        , ip = Ip
        , port = Port
    }.