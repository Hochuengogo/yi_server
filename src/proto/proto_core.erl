%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 协议核心部分
%%%
%%% @end
%%% Created : 14. 6月 2020 11:16 下午
%%%-------------------------------------------------------------------
-module(proto_core).
-author("huangzaoyi").

%% API
-export([
    pack_string/1
    , unpack_string/1
    , pack_bool/1
    , unpack_bool/1
    , unpack_int8/1
    , unpack_uint8/1
    , unpack_int16/1
    , unpack_uint16/1
    , unpack_int32/1
    , unpack_uint32/1
    , unpack_int64/1
    , unpack_uint64/1
    , unpack_array/2
]).

-define(uint16_size, 65535). %% 无符号16位最大值

%% @doc 打包字符串
%% 使用16位表示字符串长度，8位表示是否字符串结束
%% <<字符串长度:16,字符串是否结束:8,字符串内容,字符串长度:16,字符串是否结束:8,字符串内容>>
-spec pack_string(binary()) -> binary().
pack_string(String) ->
    Size = byte_size(String),
    pack_string(Size, String, <<>>).
pack_string(Size, String, Bin) when Size > ?uint16_size ->
    <<Str:Size/binary, SubStr/binary>> = String,
    NewSize = Size - ?uint16_size,
    NewAcc = <<Bin/binary, ?uint16_size:16, 1:8, Str/binary>>,
    pack_string(NewSize, SubStr, NewAcc);
pack_string(Size, String, Bin) ->
    <<Bin/binary, Size:16, 0:8, String/binary>>.

%% @doc 解包字符串
-spec unpack_string(binary()) -> {binary(), binary()}.
unpack_string(Bin) ->
    unpack_string(Bin, <<>>).
unpack_string(<<Size:16, 1:8, SubStr:Size/binary, Bin/binary>>, Str) ->
    NewStr = <<Str/binary, SubStr/binary>>,
    unpack_string(Bin, NewStr);
unpack_string(<<Size:16, 0:8, SubStr:Size/binary, Bin/binary>>, Str) ->
    NewStr = <<Str/binary, SubStr/binary>>,
    {NewStr, Bin}.

%% @doc 打包bool
-spec pack_bool(boolean()) -> binary().
pack_bool(true) ->
    <<1:8>>;
pack_bool(false) ->
    <<0:8>>.

%% @doc 解包bool
-spec unpack_bool(binary()) -> {boolean(), binary()}.
unpack_bool(<<1:8, Bin/binary>>) ->
    {true, Bin};
unpack_bool(<<0:8, Bin/binary>>) ->
    {false, Bin}.

%% @doc 解包int8
-spec unpack_int8(binary()) -> {pos_integer(), binary()}.
unpack_int8(<<Int:8/signed, Bin/binary>>) ->
    {Int, Bin}.

%% @doc 解包uint8
-spec unpack_uint8(binary()) -> {pos_integer(), binary()}.
unpack_uint8(<<Int:8, Bin/binary>>) ->
    {Int, Bin}.

%% @doc 解包int16
-spec unpack_int16(binary()) -> {pos_integer(), binary()}.
unpack_int16(<<Int:16/signed, Bin/binary>>) ->
    {Int, Bin}.

%% @doc 解包uint16
-spec unpack_uint16(binary()) -> {pos_integer(), binary()}.
unpack_uint16(<<Int:16, Bin/binary>>) ->
    {Int, Bin}.

%% @doc 解包int32
-spec unpack_int32(binary()) -> {pos_integer(), binary()}.
unpack_int32(<<Int:32/signed, Bin/binary>>) ->
    {Int, Bin}.

%% @doc 解包uint32
-spec unpack_uint32(binary()) -> {pos_integer(), binary()}.
unpack_uint32(<<Int:32, Bin/binary>>) ->
    {Int, Bin}.

%% @doc 解包int64
-spec unpack_int64(binary()) -> {pos_integer(), binary()}.
unpack_int64(<<Int:64/signed, Bin/binary>>) ->
    {Int, Bin}.

%% @doc 解包uint64
-spec unpack_uint64(binary()) -> {pos_integer(), binary()}.
unpack_uint64(<<Int:64, Bin/binary>>) ->
    {Int, Bin}.

%% @doc 解包列表
-spec unpack_array(binary(), function()) -> {list(), binary()}.
unpack_array(<<0:16, Bin/binary>>, Fun) when is_function(Fun, 1) ->
    {[], Bin};
unpack_array(<<Len:16, Bin/binary>>, Fun) when is_function(Fun, 1) ->
    unpack_array(Bin, Len, Fun, []).
unpack_array(Bin, Len, _Fun, Acc) when Len =< 0 ->
    {lists:reverse(Acc), Bin};
unpack_array(Bin, Len, Fun, Acc) ->
    {Data, NewBin} = Fun(Bin),
    unpack_array(NewBin, Len - 1, Fun, [Data | Acc]).