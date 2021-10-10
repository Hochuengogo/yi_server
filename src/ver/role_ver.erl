%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色版本转换模块
%%% @end
%%% Created : 2021-09-23 00:59:30
%%%-------------------------------------------------------------------
-module(role_ver).
-author("jiaoyinyi").

%% API
-export([
    ver_index/0
    , ver/1
    , ver_list/0
]).

-include("common.hrl").
-include("logs.hrl").
-include("ver.hrl").
-include("role.hrl").

%% @doc 版本号位置
-spec ver_index() -> pos_integer().
ver_index() ->
    2.

%% @doc 版本转换
%% 版本转换处理到最新版本，返回{ok, #ver_parser{}}
%% 版本转换处理到中间版本，返回{continue, #ver_parser{}}
%% 版本转换处理失败，返回false
-spec ver(#ver_parser{}) -> {ok, #ver_parser{}} | {continue, #ver_parser{}} | false.
ver(VerParser = #ver_parser{ver = ?role_ver}) ->
    {ok, VerParser};
ver(_VerParser = #ver_parser{ver = Ver}) ->
    ?error("role结构版本转换错误，当前版本号：~w", [Ver]),
    false.

%% @doc 子字段版本转换
-spec ver_list() -> [{pos_integer(), module()}].
ver_list() ->
    [].




