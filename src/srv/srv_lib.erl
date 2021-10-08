%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 服务器库模块
%%%
%%% @end
%%% Created : 01. 6月 2021 9:27 下午
%%%-------------------------------------------------------------------
-module(srv_lib).
-author("huangzaoyi").

%% API
-export([
    server_id/0
    , server_ids/0
    , server_type/0
    , platform/0
    , language/0
    , version/0
    , node/0
    , center_node/0
    , code_path/0
    , server_path/0
    , is_local/1
]).

-include("common.hrl").

%% @doc 服务器ID
-spec server_id() -> srv_id().
server_id() ->
    srv_config:get(server_id).

%% @doc 服务器ID列表
-spec server_ids() -> [srv_id()].
server_ids() ->
    srv_config:get(server_ids).

%% @doc 服务器类型
-spec server_type() -> zone | center.
server_type() ->
    srv_config:get(server_type).

%% @doc 平台
-spec platform() -> binary().
platform() ->
    srv_config:get(platform).

%% @doc 语言
-spec language() -> language().
language() ->
    srv_config:get(language).

%% @doc 服务器版本
-spec version() -> binary().
version() ->
    srv_config:get(version).

%% @doc 服务器节点
-spec node() -> node().
node() ->
    srv_config:get(node).

%% @doc 中央服节点
-spec center_node() -> node().
center_node() ->
    srv_config:get(center_node).

%% @doc 代码根路径
-spec code_path() -> string().
code_path() ->
    srv_config:get(code_path).

%% @doc 服务器根路径
-spec server_path() -> string().
server_path() ->
    {ok, SrvPath} = file:get_cwd(),
    SrvPath.

%% @doc 判断服务器ID是否为本服服务器ID
-spec is_local(srv_id()) -> boolean().
is_local(SrvId) ->
    lists:member(SrvId, server_ids()).


