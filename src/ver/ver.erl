%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 版本转换模块
%%% @end
%%% Created : 2021-09-17 23:23:16
%%%-------------------------------------------------------------------
-module(ver).
-author("jiaoyinyi").

%% API
-export([
    parse/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("ver.hrl").

%% 版本转换 传一个数据，根据给的版本转换模块去执行版本转换处理，返回最终版本转换后的结构
%% @doc 版本转换
-spec parse(tuple(), module()) -> {ok, tuple()} | false.
parse(Data, Mod) ->
    case catch do(Data, Mod) of
        {ok, NewData} ->
            {ok, NewData};
        false ->
            false;
        _Err ->
            ?error("数据[~w]版本转换失败，原因：~w", [element(1, Data), _Err]),
            false
    end.

do(Data, Mod) ->
    Ver = element(Mod:ver_index(), Data),
    do2(#ver_parser{data = Data, ver = Ver, mod = Mod}).
do2(VerParser = #ver_parser{mod = Mod}) ->
    case Mod:ver(VerParser) of
        {ok, #ver_parser{data = NewData}} -> %% 转换到最终版本成功
            VerList = Mod:ver_list(),
            do_sub(VerList, NewData);
        {continue, NewVerParser0 = #ver_parser{data = NewData0, opts = Opts}} -> %% 转换中
            NewData = do_ver(Opts, NewData0),
            NewVerParser = acc_ver(NewVerParser0#ver_parser{data = NewData, opts = []}),
            do2(NewVerParser);
        false ->
            false
    end.

%% 子字段版本转换
do_sub([], Data) ->
    {ok, Data};
do_sub([{Pos, Mod} | VerList], Data) ->
    SubData = element(Pos, Data),
    case do(SubData, Mod) of
        {ok, NewSubData} ->
            NewData = setelement(Pos, Data, NewSubData),
            do_sub(VerList, NewData);
        false ->
            false
    end.

%% 累加版本号
acc_ver(VerParser = #ver_parser{data = Data, ver = Ver, mod = Mod}) ->
    NewData = setelement(Mod:ver_index(), Data, Ver + 1),
    VerParser#ver_parser{data = NewData, ver = Ver + 1}.

%% 执行版本转换的实际操作
do_ver([], Data) ->
    Data;
do_ver([Opt | Opts], Data) ->
    NewData = do_ver(Opt, Data),
    do_ver(Opts, NewData);
do_ver({add, FiledList}, NewData) ->
    do_ver_add(FiledList, NewData);
do_ver({set, FiledList}, NewData) ->
    do_ver_set(FiledList, NewData).

do_ver_add([], Data) ->
    Data;
do_ver_add([Filed | FiledList], Data) ->
    NewData = erlang:append_element(Data, Filed),
    do_ver_add(FiledList, NewData).

do_ver_set([], Data) ->
    Data;
do_ver_set([{Pos, Filed} | FiledList], Data) ->
    NewData = setelement(Pos, Data, Filed),
    do_ver_set(FiledList, NewData).
