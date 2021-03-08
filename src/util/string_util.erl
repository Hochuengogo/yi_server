%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 字符串处理库
%%%
%%% @end
%%% Created : 07. 3月 2021 7:33 下午
%%%-------------------------------------------------------------------
-module(string_util).
-author("huangzaoyi").

%% API
-export([
    spilt/2
]).

%% @doc 字符串切割
-spec spilt(list(), pos_integer()) -> [list()].
spilt(String, Char) ->
    spilt2(lists:reverse(String), Char, [], []).
spilt2([], _Char, Acc, SubStr) ->
    [SubStr | Acc];
spilt2([Char | String], Char, Acc, SubStr) ->
    spilt2(String, Char, [SubStr | Acc], []);
spilt2([C | String], Char, Acc, SubStr) ->
    spilt2(String, Char, Acc, [C | SubStr]).