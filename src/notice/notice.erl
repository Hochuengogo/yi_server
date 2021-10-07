%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 飘字
%%% @end
%%% Created : 2021-08-16 00:33:49
%%%-------------------------------------------------------------------
-module(notice).
-author("jiaoyinyi").

%% API
-export([
    error/1, error/2
]).

-include("common.hrl").
-include("logs.hrl").

%% @doc 发送错误信息
-spec error(msg()) -> void().
error(Msg0) ->
    {Code, Args, Msg} = pack(Msg0),
    gateway_lib:pack_send(10200, {0, Code, Args, Msg}).
-spec error(pid(), msg()) -> void().
error(GPid, Msg0) ->
    {Code, Args, Msg} = pack(Msg0),
    gateway_lib:pack_send(GPid, 10200, {0, Code, Args, Msg}).

%% 消息打包
pack(Code) when is_integer(Code) ->
    {Code, [], <<>>};
pack(Msg) when is_binary(Msg) ->
    {0, [], Msg};
pack({Code, Args}) when is_integer(Code) andalso is_list(Args) ->
    {Code, format(Args), <<>>};
pack({Code, Args, Msg}) when is_integer(Code) andalso is_list(Args) andalso is_binary(Msg) ->
    {Code, format(Args), Msg}.

%% 参数格式转换
format(Args) when is_list(Args) ->
    [format2(Arg) || Arg <- Args];
format(Arg) ->
    format2(Arg).

%% 参数格式转换详细处理
format2(Arg) when is_integer(Arg) ->
    integer_to_binary(Arg);
format2(Arg) ->
    Arg.