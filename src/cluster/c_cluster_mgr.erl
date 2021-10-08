%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 中央服跨服管理
%%% @end
%%% Created : 2021-10-07 22:32:44
%%%-------------------------------------------------------------------
-module(c_cluster_mgr).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([call/1, cast/1, info/1, apply/2, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").
-include("cluster.hrl").

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
    %% 服务器信息 多个服合服后，只会有表现出来的主服信息
    ets:new(srv_info, [named_table, public, set, {keypos, #srv_info.node}]),
    %% 服务器信息 同版本的服节点信息
    ets:new(srv_id, [named_table, public, set, {keypos, 1}]),
    %% 服务器信息 无论版本是否一致的服节点信息
    ets:new(srv_id_ver, [named_table, public, set, {keypos, 1}]),
    ok = net_kernel:monitor_nodes(true),
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
    catch [net_kernel:disconnect(Node) || #srv_info{node = Node} <- ets:tab2list(srv_info)], %% 主动断开和游戏服节点连接
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

%% 连接到节点
do_handle_info({nodeup, Node}, State) ->
    erlang:monitor_node(Node, true),
    {noreply, State};

%% 节点断开连接
do_handle_info({nodedown, Node}, State) ->
    erlang:monitor_node(Node, false),
    case ets:lookup(srv_info, Node) of
        [#srv_info{srv_list = SrvList, version = Version}] ->
            ets:delete(srv_info, Node),
            [ets:delete(srv_id_ver, SrvId) || SrvId <- SrvList],
            case srv_lib:version() =:= Version of
                true ->
                    [ets:delete(srv_id, SrvId) || SrvId <- SrvList],
                    [rpc:cast(Node0, cluster_mgr, info, [{srv_down, Node, true, SrvList}]) || #srv_info{node = Node0} <- ets:tab2list(srv_info)]; %% 广播游戏服断开中央服连接信息给其他已连接的节点
                _ ->
                    [rpc:cast(Node0, cluster_mgr, info, [{srv_down, Node, false, SrvList}]) || #srv_info{node = Node0} <- ets:tab2list(srv_info)] %% 广播游戏服断开中央服连接信息给其他已连接的节点
            end,
            ?info("游戏服节点[~w]断开连接", [Node]);
        _ ->
            skip
    end,
    {noreply, State};

%% 游戏服连接到中央服，上报服务器信息
do_handle_info({srv_up, BackPid, Node, SrvList, Version}, State) ->
    erlang:monitor_node(Node, true),
    SrvInfo = #srv_info{node = Node, srv_list = SrvList, version = Version},
    ets:insert(srv_info, SrvInfo),
    CenterVersion = srv_lib:version(),
    BackPid ! {center_info, CenterVersion, ets:tab2list(srv_id), ets:tab2list(srv_id_ver)}, %% 发送已连接到中央服的服务器信息，与中央服版本
    SrvIds = [{SrvId, Node} || SrvId <- SrvList],
    ets:insert(srv_id_ver, SrvIds),
    case CenterVersion =:= Version of
        true ->
            ets:insert(srv_id, SrvIds),
            [rpc:cast(Node0, cluster_mgr, info, [{srv_up, Node, true, SrvIds}]) || #srv_info{node = Node0} <- ets:tab2list(srv_info), Node0 =/= Node]; %% 广播该游戏服连接到中央服的信息给其他已连接的节点
        _ ->
            [rpc:cast(Node0, cluster_mgr, info, [{srv_up, Node, false, SrvIds}]) || #srv_info{node = Node0} <- ets:tab2list(srv_info), Node0 =/= Node] %% 广播该游戏服连接到中央服的信息给其他已连接的节点
    end,
    ?info("游戏服节点[~w]连接", [Node]),
    {noreply, State};

do_handle_info(_Info, State) ->
    {noreply, State}.




