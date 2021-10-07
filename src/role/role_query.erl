%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色查询模块
%%% @end
%%% Created : 2021-09-17 00:05:40
%%%-------------------------------------------------------------------
-module(role_query).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([
    gate_pid/2
    , role_pid/2
    , pack_send/3
    , send/2
    , apply/2
    , set_role_query/2
    , del_role_query/1
]).
-export([call/1, cast/1, info/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").
-include("gateway.hrl").

-record(state, {}).

%% @doc 获取在线玩家网关pid
-spec gate_pid(by_pid | by_name, role_id() | binary()) -> {ok, pid()} | {error, term()}.
gate_pid(Flag, Key) ->
    case get_role_query(Flag, Key) of
        {ok, #role_query{gate_pid = GatePid}} ->
            {ok, GatePid};
        _ ->
            {error, not_online}
    end.

%% @doc 获取在线玩家角色pid
-spec role_pid(by_pid | by_name, role_id() | binary()) -> {ok, pid()} | {error, term()}.
role_pid(Flag, Key) ->
    case get_role_query(Flag, Key) of
        {ok, #role_query{role_pid = RolePid}} ->
            {ok, RolePid};
        _ ->
            {error, not_online}
    end.

%% @doc 打包数据并发送给全服在线玩家
-spec pack_send(world, pos_integer(), term()) -> void().
pack_send(world, Code, Data) ->
    Bin = gateway_lib:pack(Code, res, Data),
    send(world, Bin).

%% @doc 发送数据给全服在线玩家
-spec send(world, binary()) -> void().
send(world, Bin) ->
    [gateway_lib:send(GatePid, Bin) || #role_query{gate_pid = GatePid} <- ets:tab2list(role_query_id)].

%% @doc 全服在线玩家调用方法
-spec apply(world, mfa()) -> void().
apply(world, MFA) ->
    [role:apply(async, RolePid, MFA) || #role_query{role_pid = RolePid} <- ets:tab2list(role_query_id)].

%% @doc 获取role_query结构
-spec get_role_query(by_id | by_name, role_id() | binary()) -> {ok, #role_query{}} | false.
get_role_query(by_id, RoleId) ->
    case catch ets:lookup(role_query_id, RoleId) of
        [RoleQuery] ->
            {ok, RoleQuery};
        _ ->
            false
    end;
get_role_query(by_name, Name) ->
    case catch ets:lookup(role_query_name, Name) of
        [RoleQuery] ->
            {ok, RoleQuery};
        _ ->
            false
    end.

%% @doc 设置role_query结构
-spec set_role_query(by_id | by_name, #role{}) -> true.
set_role_query(by_id, #role{id = RoleId, pid = RolePid, m_gateway = #m_gateway{pid = GatePid}}) ->
    ets:insert(role_query_id, #role_query{key = RoleId, gate_pid = GatePid, role_pid = RolePid});
set_role_query(by_name, #role{name = Name, pid = RolePid, m_gateway = #m_gateway{pid = GatePid}}) ->
    ets:insert(role_query_name, #role_query{key = Name, gate_pid = GatePid, role_pid = RolePid}).

%% @doc 删除role_query结构
-spec del_role_query(#role{}) -> true.
del_role_query(#role{id = RoleId, name = Name}) ->
    ets:delete(role_query_id, RoleId),
    ets:delete(role_query_name, Name).

call(Request) ->
    ?scall(?MODULE, Request).

cast(Request) ->
    gen_server:cast(?MODULE, Request).

info(Info) ->
    ?MODULE ! Info.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info("[~w]开始启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(role_query_id, [named_table, public, set, {keypos, #role_query.key}]),
    ets:new(role_query_name, [named_table, public, set, {keypos, #role_query.key}]),
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
    ?info("[~w]关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

do_handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_info(_Info, State) ->
    {noreply, State}.
