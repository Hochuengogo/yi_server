%%%-------------------------------------------------------------------
%%% @author {author}
%%% @copyright (C) {year}, <COMPANY>
%%% @doc
%%% {desc}
%%% @end
%%% Created : {create_time}
%%%-------------------------------------------------------------------
-module({mod}).
-author("{author}").

%% API
-export([
    ver_index/0
    , ver/1
    , ver_list/0
]).

-include("common.hrl").
-include("logs.hrl").
-include("ver.hrl").

%% @doc 版本号位置
-spec ver_index() -> pos_integer().
ver_index() ->
    2.

%% @doc 版本转换
%% 版本转换处理到最新版本，返回{ok, #ver_parser{}}
%% 版本转换处理到中间版本，返回{continue, #ver_parser{}}
%% 版本转换处理失败，返回false
-spec ver(#ver_parser{}) -> {ok, #ver_parser{}} | {continue, #ver_parser{}} | false.
ver(VerParser) ->
    {ok, VerParser}.

%% @doc 子字段版本转换
-spec ver_list() -> [{pos_integer(), module()}].
ver_list() ->
    [].




