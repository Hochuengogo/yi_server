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
-include("cluster.hrl").

-define(cluster_call_timeout, 5000).

%% @doc 异步执行方法
-spec cast(center | all | {srv, srv_id()} | {node, node()}, module(), atom(), list()) -> true | {error, term()}.
%% 中央服节点执行
cast(center, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            do_cast(local, M, F, A);
        zone ->
            case is_link() of
                true ->
                    do_cast(srv_lib:center_node(), M, F, A);
                _ ->
                    {error, not_link}
            end
    end;
%% 所有和中央服节点版本一致的游戏服节点执行
cast(all, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            CenterVersion = srv_lib:version(),
            catch [do_cast(Node, M, F, A) || #srv_info{node = Node, version = Version} <- ets:tab2list(srv_info), CenterVersion =:= Version],
            true;
        zone ->
            {error, not_center}
    end;
%% 指定游戏服节点执行
cast({srv, SrvId}, M, F, A) ->
    case srv_lib:is_local(SrvId) of
        true ->
            do_cast(local, M, F, A);
        _ ->
            case is_link() of
                true ->
                    case catch ets:lookup(srv_id, SrvId) of
                        [{_, Node}] ->
                            cast({node, Node}, M, F, A);
                        _ ->
                            {error, srv_not_link}
                    end;
                _ ->
                    {error, not_link}
            end
    end;
%% 游戏节点执行
cast({node, Node}, M, F, A) when Node =:= node() ->
    do_cast(local, M, F, A);
cast({node, Node}, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            do_cast(Node, M, F, A);
        zone ->
            cast(center, ?MODULE, cast, [{node, Node}, M, F, A]) %% 交给中央服执行
    end;
%% 发送消息到进程
cast({msg, center, Name}, CastMod, CastFunc, Msg) ->
    case srv_lib:server_type() of
        center ->
            do_cast_msg(local, Name, CastMod, CastFunc, Msg);
        zone ->
            case is_link() of
                true ->
                    do_cast_msg(srv_lib:center_node(), Name, CastMod, CastFunc, Msg);
                _ ->
                    {error, not_link}
            end
    end;
cast({msg, all, Name}, CastMod, CastFunc, Msg) ->
    case srv_lib:server_type() of
        center ->
            CenterVersion = srv_lib:version(),
            catch [do_cast_msg(Node, Name, CastMod, CastFunc, Msg) || #srv_info{node = Node, version = Version} <- ets:tab2list(srv_info), CenterVersion =:= Version],
            true;
        zone ->
            {error, not_center}
    end;
cast({msg, {srv, SrvId}, Name}, CastMod, CastFunc, Msg) ->
    case srv_lib:is_local(SrvId) of
        true ->
            do_cast_msg(local, Name, CastMod, CastFunc, Msg);
        _ ->
            case is_link() of
                true ->
                    case catch ets:lookup(srv_id, SrvId) of
                        [{_, Node}] ->
                            cast({msg, {node, Node}, Name}, CastMod, CastFunc, Msg);
                        _ ->
                            {error, srv_not_link}
                    end;
                _ ->
                    {error, not_link}
            end
    end;
cast({msg, {node, Node}, Name}, CastMod, CastFunc, Msg) when Node =:= node() ->
    do_cast_msg(local, Name, CastMod, CastFunc, Msg);
cast({msg, {node, Node}, Name}, CastMod, CastFunc, Msg) ->
    case srv_lib:server_type() of
        center ->
            do_cast_msg(Node, Name, CastMod, CastFunc, Msg);
        zone ->
            cast(center, ?MODULE, cast, [{msg, {node, Node}, Name}, CastMod, CastFunc, Msg]) %% 交给中央服执行
    end;
cast(Flag, _M, _F, _A) ->
    ?error("跨服异步调用类型错误：~w", [Flag]),
    {error, {bad_flag, Flag}}.

do_cast(local, M, F, A) ->
    catch util:apply(M, F, A),
    true;
do_cast(Node, M, F, A) ->
    rpc:cast(Node, M, F, A).

do_cast_msg(local, Name, CastMod, CastFunc, Msg) ->
    catch CastMod:CastFunc(Name, Msg),
    true;
do_cast_msg(Node, Name, CastMod, CastFunc, Msg) ->
    catch CastMod:CastFunc({Name, Node}, Msg),
    true.

%% @doc 同步执行方法
-spec call(center | {srv, srv_id()} | {node, node()}, module(), atom(), list()) -> term() | {error, term()}.
%% 中央服节点执行
call(center, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            do_call(local, M, F, A);
        zone ->
            case is_link() of
                true ->
                    do_call(srv_lib:center_node(), M, F, A);
                _ ->
                    {error, not_link}
            end
    end;
%% 指定游戏服节点执行
call({srv, SrvId}, M, F, A) ->
    case srv_lib:is_local(SrvId) of
        true ->
            do_call(local, M, F, A);
        _ ->
            case is_link() of
                true ->
                    case catch ets:lookup(srv_id, SrvId) of
                        [{_, Node}] ->
                            call({node, Node}, M, F, A);
                        _ ->
                            {error, srv_not_link}
                    end;
                _ ->
                    {error, not_link}
            end
    end;
%% 游戏节点执行
call({node, Node}, M, F, A) when Node =:= node() ->
    do_call(local, M, F, A);
call({node, Node}, M, F, A) ->
    case srv_lib:server_type() of
        center ->
            do_call(Node, M, F, A);
        zone ->
            call(center, ?MODULE, call, [{node, Node}, M, F, A]) %% 交给中央服执行
    end;
call({msg, center, Name}, CallMod, CallFunc, Msg) ->
    case srv_lib:server_type() of
        center ->
            do_call_msg(local, Name, CallMod, CallFunc, Msg);
        zone ->
            case is_link() of
                true ->
                    do_call_msg(srv_lib:center_node(), Name, CallMod, CallFunc, Msg);
                _ ->
                    {error, not_link}
            end
    end;
call({msg, {srv, SrvId}, Name}, CallMod, CallFunc, Msg) ->
    case srv_lib:is_local(SrvId) of
        true ->
            do_call_msg(local, Name, CallMod, CallFunc, Msg);
        _ ->
            case is_link() of
                true ->
                    case catch ets:lookup(srv_id, SrvId) of
                        [{_, Node}] ->
                            call({msg, {node, Node}, Name}, CallMod, CallFunc, Msg);
                        _ ->
                            {error, srv_not_link}
                    end;
                _ ->
                    {error, not_link}
            end
    end;
call({msg, {node, Node}, Name}, CallMod, CallFunc, Msg) when Node =:= node() ->
    do_call_msg(local, Name, CallMod, CallFunc, Msg);
call({msg, {node, Node}, Name}, CallMod, CallFunc, Msg) ->
    case srv_lib:server_type() of
        center ->
            do_call_msg(Node, Name, CallMod, CallFunc, Msg);
        zone ->
            call(center, ?MODULE, call, [{msg, {node, Node}, Name}, CallMod, CallFunc, Msg]) %% 交给中央服执行
    end;
call(Flag, _M, _F, _A) ->
    ?error("跨服同步调用类型错误：~w", [Flag]),
    {error, {bad_flag, Flag}}.

do_call(local, M, F, A) ->
    util:apply(M, F, A);
do_call(Node, M, F, A) ->
    case rpc:call(Node, M, F, A, ?cluster_call_timeout) of
        {badrpc, Err} ->
            {error, {badrpc, Err}};
        Ret ->
            Ret
    end.

do_call_msg(local, Name, CallMod, CallFunc, Msg) ->
    CallMod:CallFunc(Name, Msg);
do_call_msg(Node, Name, CallMod, CallFunc, Msg) ->
    CallMod:CallFunc({Name, Node}, Msg).

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
    case srv_lib:server_type() of
        center ->
            ignore;
        zone ->
            case is_link_ver() of
                true ->
                    ignore;
                _ ->
                    cluster_mgr:info(check_connect)
            end
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