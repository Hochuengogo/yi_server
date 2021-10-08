%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 跨服接口
%%% @end
%%% Created : 2021-10-07 22:30:49
%%%-------------------------------------------------------------------
-module(cluster_lib).
-author("jiaoyinyi").

%% API
-export([
    cast/4
    , call/4
    , is_link/0
    , is_link_ver/0
    , link/0
    , connect_center_fire/1
    , disconnect_center_fire/1
]).

-include("common.hrl").
-include("logs.hrl").

-define(cluster_call_timeout, 5000).

%% @doc 异步执行方法
-spec cast(center | center_ver | all | all_ver | srv_id() | node(), module(), atom(), list()) -> true | {error, term()}.
%% 中央服节点执行，并且要与中央服节点版本一致
cast(center, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            catch util:apply(M, F, A),
            true;
        zone ->
            case is_link() of
                true ->
                    rpc:cast(srv_lib:center_node(), M, F, A);
                _ ->
                    {error, not_link}
            end
    end;
%% 中央服节点执行
cast(center_ver, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            catch util:apply(M, F, A),
            true;
        zone ->
            case is_link_ver() of
                true ->
                    rpc:cast(srv_lib:center_node(), M, F, A);
                _ ->
                    {error, not_link}
            end
    end;
%% 所有和中央服节点版本一致的游戏服节点执行
cast(all, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            catch [rpc:cast(Node, M, F, A) || {_, Node} <- ets:tab2list(srv_id)],
            true;
        zone ->
            {error, not_center}
    end;
%% 所有游戏服节点执行
cast(all_ver, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            catch [rpc:cast(Node, M, F, A) || {_, Node} <- ets:tab2list(srv_id_ver)],
            true;
        zone ->
            {error, not_center}
    end;
%% 指定游戏服节点执行，本游戏服节点和目标游戏服节点与中央服节点版本一致
cast({srv, SrvId}, M, F, A) ->
    case srv_lib:is_local(SrvId) of
        true ->
            catch util:apply(M, F, A),
            true;
        _ ->
            case is_link() of
                true ->
                    case catch ets:lookup(srv_id, SrvId) of
                        [{_, Node}] ->
                            cast(Node, M, F, A);
                        _ ->
                            {error, srv_not_link}
                    end;
                _ ->
                    {error, not_link}
            end
    end;
%% 指定游戏服节点执行
cast({srv_ver, SrvId}, M, F, A) ->
    case srv_lib:is_local(SrvId) of
        true ->
            catch util:apply(M, F, A),
            true;
        _ ->
            case is_link_ver() of
                true ->
                    case catch ets:lookup(srv_id_ver, SrvId) of
                        [{_, Node}] ->
                            cast(Node, M, F, A);
                        _ ->
                            {error, srv_not_link}
                    end;
                _ ->
                    {error, not_link}
            end
    end;
%% 游戏节点执行
cast(Node, M, F, A) when Node =:= node() ->
    catch util:apply(M, F, A),
    true;
cast(Node, M, F, A) when is_atom(Node) ->
    case srv_lib:server_type() of
        center ->
            rpc:cast(Node, M, F, A);
        zone ->
            cast(center, ?MODULE, cast, [Node, M, F, A]) %% 交给中央服执行
    end;
cast(Flag, _M, _F, _A) ->
    ?error("跨服异步调用类型错误：~w", [Flag]),
    {error, {bad_flag, Flag}}.

%% @doc 同步执行方法
-spec call(center | center_ver | srv_id() | node(), module(), atom(), list()) -> true | {error, term()}.
%% 中央服节点执行，并且要与中央服节点版本一致
call(center, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            util:apply(M, F, A);
        zone ->
            case is_link() of
                true ->
                    rpc:call(srv_lib:center_node(), M, F, A, ?cluster_call_timeout);
                _ ->
                    {error, not_link}
            end
    end;
%% 中央服节点执行
call(center_ver, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            util:apply(M, F, A);
        zone ->
            case is_link_ver() of
                true ->
                    rpc:call(srv_lib:center_node(), M, F, A, ?cluster_call_timeout);
                _ ->
                    {error, not_link}
            end
    end;
%% 指定游戏服节点执行，本游戏服节点和目标游戏服节点与中央服节点版本一致
call({srv, SrvId}, M, F, A) ->
    case srv_lib:is_local(SrvId) of
        true ->
            util:apply(M, F, A);
        _ ->
            case is_link() of
                true ->
                    case catch ets:lookup(srv_id, SrvId) of
                        [{_, Node}] ->
                            call(Node, M, F, A);
                        _ ->
                            {error, srv_not_link}
                    end;
                _ ->
                    {error, not_link}
            end
    end;
%% 指定游戏服节点执行
call({srv_ver, SrvId}, M, F, A) ->
    case srv_lib:is_local(SrvId) of
        true ->
            util:apply(M, F, A);
        _ ->
            case is_link_ver() of
                true ->
                    case catch ets:lookup(srv_id_ver, SrvId) of
                        [{_, Node}] ->
                            call(Node, M, F, A);
                        _ ->
                            {error, srv_not_link}
                    end;
                _ ->
                    {error, not_link}
            end
    end;
%% 游戏节点执行
call(Node, M, F, A) when Node =:= node() ->
    util:apply(M, F, A);
call(Node, M, F, A) when is_atom(Node) ->
    case srv_lib:server_type() of
        center ->
            rpc:call(Node, M, F, A, ?cluster_call_timeout);
        zone ->
            call(center, ?MODULE, call, [Node, M, F, A]) %% 交给中央服执行
    end;
call(Flag, _M, _F, _A) ->
    ?error("跨服同步调用类型错误：~w", [Flag]),
    {error, {bad_flag, Flag}}.

%% @doc 是否连接到中央服节点，并且与中央服节点版本一致
-spec is_link() -> boolean().
is_link() ->
    is_link_ver() andalso srv_lib:version() =:= srv_config:get(center_version).

%% @doc 是否连接到中央服节点
-spec is_link_ver() -> boolean().
is_link_ver() ->
    srv_config:get(is_link) =:= true.

%% @doc 主动连接中央服节点
-spec link() -> void().
link() ->
    case is_link_ver() of
        true ->
            ignore;
        _ ->
            cluster_mgr:info(check_connect)
    end.

%% @doc 连接到中央服节点触发
-spec connect_center_fire(boolean()) -> ok.
connect_center_fire(true) ->
    ok;
connect_center_fire(_IsSameVer) ->
    ok.

%% @doc 断开与中央服节点连接触发
-spec disconnect_center_fire(boolean()) -> ok.
disconnect_center_fire(true) ->
    ok;
disconnect_center_fire(_IsSameVer) ->
    ok.