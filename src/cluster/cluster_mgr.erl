%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 跨服管理
%%% @end
%%% Created : 2021-10-07 22:33:08
%%%-------------------------------------------------------------------
-module(cluster_mgr).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([call/1, cast/1, info/1, apply/2, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").

-record(state, {}).

call(Request) ->
    ?scall(?MODULE, Request).

cast(Request) ->
    gen_server:cast(?MODULE, Request).

info(Info) ->
    ?MODULE ! Info.

apply(sync, MFA = {_M, _F, _A}) ->
    call({apply, MFA});
apply(sync, {F, A}) ->
    call({apply, {undefined, F, A}});
apply(async, MFA = {_M, _F, _A}) ->
    info({apply, MFA});
apply(async, {F, A}) ->
    info({apply, {undefined, F, A}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info("[~w]开始启动", [?MODULE]),
    process_flag(trap_exit, true),
    %% 服务器信息 和中央服同版本的服节点信息
    ets:new(srv_id, [named_table, public, set, {keypos, 1}]),
    %% 服务器信息 和中央服不同版本的服节点信息
    ets:new(srv_id_ver, [named_table, public, set, {keypos, 1}]),
    true = erlang:monitor_node(srv_lib:center_node(), true),
    erlang:send_after(util:rand(40000, 60000), self(), check_connect),
    ?info("[~w]启动完成", [?MODULE]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    case catch do_handle_call(Request, From, State) of
        {reply, Reply, NewState} ->
            {reply, Reply, NewState};
        _Err ->
            ?error("handle_call错误，消息:~w，State:~w，Reason:~w", [Request, State, _Err]),
            {reply, {error, handle_error}, State}
    end.

handle_cast(Request, State) ->
    case catch do_handle_cast(Request, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("handle_cast错误，消息:~w，State:~w，Reason:~w", [Request, State, _Err]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("handle_info错误，消息:~w，State:~w，Reason:~w", [Info, State, _Err]),
            {noreply, State}
    end.

terminate(Reason, _State) ->
    ?info("[~w]开始关闭，原因：~w", [?MODULE, Reason]),
    catch net_kernel:disconnect(srv_lib:center_node()), %% 主动断开和中央服节点连接
    ?info("[~w]关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_call({apply, {M, F, A}}, _From, State) ->
    case catch util:apply(M, F, [State | A]) of
        {ok, Reply} ->
            {reply, Reply, State};
        {reply, Reply} ->
            {reply, Reply, State};
        {ok, Reply, NewState} ->
            {reply, Reply, NewState};
        {reply, Reply, NewState} ->
            {reply, Reply, NewState};
        _Err ->
            ?error("同步执行~w:~w:~w错误，State:~w，Reason:~w", [M, F, A, State, _Err]),
            {reply, {error, apply_error}, State}
    end;

do_handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

do_handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_info({apply, {M, F, A}}, State) ->
    case catch util:apply(M, F, [State | A]) of
        ok ->
            {noreply, State};
        {ok, NewState} ->
            {noreply, NewState};
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("异步执行~w:~w:~w错误，State:~w，Reason:~w", [M, F, A, State, _Err]),
            {noreply, State}
    end;

%% 中央服断开连接
do_handle_info({nodedown, _Node}, State) ->
    srv_config:set(is_link, false),
    ets:delete_all_objects(srv_id),
    ets:delete_all_objects(srv_id_ver),
    ?info("中央服节点断开连接"),
    cluster_lib:disconnect_center_fire(srv_lib:version() =:= srv_config:get(center_version)),
    {noreply, State};

%% 检测是否和中央服连接
do_handle_info(check_connect, State) ->
    CenterNode = srv_lib:center_node(),
    case net_adm:ping(CenterNode) of
        pong ->
            case srv_config:get(is_link, false) of
                false ->
                    rpc:cast(CenterNode, c_cluster_mgr, info, [{srv_up, self(), node(), srv_lib:server_ids(), srv_lib:version()}]);
                _ ->
                    skip
            end;
        _ ->
            case srv_config:get(is_link, false) of
                true ->
                    srv_config:set(is_link, false),
                    ets:delete_all_objects(srv_id),
                    ets:delete_all_objects(srv_id_ver),
                    ?info("中央服节点断开连接");
                _ ->
                    ?error("连接中央服节点失败")
            end
    end,
    erlang:send_after(util:rand(60000, 90000), self(), check_connect),
    {noreply, State};

%% 连接到中央服
do_handle_info({center_info, CenterVersion, SrvIds, SrvIdsVer}, State) ->
    ets:delete_all_objects(srv_id),
    ets:delete_all_objects(srv_id_ver),
    ets:insert(srv_id, SrvIds),
    ets:insert(srv_id_ver, SrvIdsVer),
    srv_config:set(center_version, CenterVersion),
    srv_config:set(is_link, true),
    Version = srv_lib:version(),
    ?info("连接到中央服节点，中央服版本号：~ts，本服版本号：~ts", [CenterVersion, Version]),
    cluster_lib:connect_center_fire(Version =:= CenterVersion),
    {noreply, State};

%% 其他游戏服连接到中央服
do_handle_info({srv_up, Node, SameVersion, SrvIds}, State) ->
    ?debug("其他游戏服节点[~w]连接到中央服节点，srv_ids：~w", [Node, SrvIds]),
    ets:insert(srv_id_ver, SrvIds),
    case SameVersion of
        true ->
            ets:insert(srv_id, SrvIds);
        _ ->
            skip
    end,
    {noreply, State};

do_handle_info({srv_down, Node, SameVersion, SrvIds}, State) ->
    ?debug("其他游戏服节点[~w]断开与中央服节点连接，srv_ids：~w", [Node, SrvIds]),
    [ets:delete(srv_id_ver, SrvId) || SrvId <- SrvIds],
    case SameVersion of
        true ->
            [ets:delete(srv_id, SrvId) || SrvId <- SrvIds];
        _ ->
            skip
    end,
    {noreply, State};

do_handle_info(_Info, State) ->
    {noreply, State}.
