%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 加密算法库
%%%
%%% @end
%%% Created : 07. 3月 2021 7:34 下午
%%%-------------------------------------------------------------------
-module(crypto_util).
-author("huangzaoyi").

%% API
-export([
    md5/1
    , md5_hex_string/1
    , hex_string/1
]).

%% @doc md5加密
-spec md5(string()) -> binary().
md5(Data) ->
    erlang:md5(Data).

%% @doc md5加密并转成16进制字符串
-spec md5_hex_string(string()) -> list().
md5_hex_string(Data) ->
    hex_string(md5(Data)).

%% @doc 转成16进制字符串
-spec hex_string(binary()) -> list().
hex_string(Bin) when is_binary(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [V]) || <<V>> <= Bin]).