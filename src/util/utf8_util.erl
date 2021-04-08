%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc utf8库
%%%
%%% @end
%%% Created : 07. 3月 2021 7:33 下午
%%%-------------------------------------------------------------------
-module(utf8_util).
-author("huangzaoyi").

%% API
-export([
    utf8_to_chars/1
    , chars_to_utf8/1
    , char_num/1
]).

%% @doc utf8编码转字符串
-spec utf8_to_chars(binary()) -> list().
utf8_to_chars(Bin) when is_binary(Bin) ->
    unicode:characters_to_list(Bin, utf8).

%% @doc 字符串转uf8编码
-spec chars_to_utf8(list()) -> binary().
chars_to_utf8(Chars) when is_list(Chars) ->
    unicode:characters_to_binary(Chars, utf8).

%% @doc 获取字符数量
-spec char_num(binary() | list()) -> pos_integer().
char_num(Bin) when is_binary(Bin) ->
    char_num(utf8_to_chars(Bin));
char_num(Chars) when is_list(Chars) ->
    length(Chars).