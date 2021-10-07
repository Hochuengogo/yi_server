%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 客户端
%%% @end
%%% Created : 2021-10-07 00:28:36
%%%-------------------------------------------------------------------
-module(t_client).
-author("jiaoyinyi").

%% API
-export([
    start/2, start/3
    , stop/2, stop/3
]).

-include("common.hrl").
-include("logs.hrl").

%% @doc 启动客户端
-spec start(atom(), pos_integer()) -> ok.
start(Type, Num) ->
    start(Type, 1, Num).
-spec start(atom(), pos_integer(), pos_integer()) -> ok.
start(_Type, From, To) when From > To ->
    ok;
start(Type, From, To) ->
    t_gateway_client:start(Type, list_to_binary(lists:concat([Type, "_", From]))),
    start(Type, From + 1, To).

%% @doc 关闭客户端
-spec stop(atom(), pos_integer()) -> ok.
stop(Type, Num) ->
    stop(Type, 1, Num).
-spec stop(atom(), pos_integer(), pos_integer()) -> ok.
stop(_Type, From, To) when From > To ->
    ok;
stop(Type, From, To) ->
    t_gateway_client:stop(list_to_binary(lists:concat([Type, "_", From]))),
    stop(Type, From + 1, To).