%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色模块
%%% @end
%%% Created : 2021-09-17 00:04:43
%%%-------------------------------------------------------------------
-module(role).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([call/2, cast/2, info/2, apply/3, start/3, switch/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    pack_send/2, pack_send/3
    , send/1, send/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").
-include("gateway.hrl").
-include("trigger.hrl").
-include("stimer.hrl").

%% @doc 打包发送数据
-spec pack_send(pos_integer(), term()) -> void().
pack_send(Code, Data) ->
    case get('@is_role_proc') of
        true ->
            Bin = gateway_lib:pack(Code, res, Data),
            gateway_lib:send(get('@gate_pid'), Bin);
        _ ->
            ?error("在非角色进程发送协议数据，协议号：~w，数据：~w", [Code, Data]),
            ok
    end.
%% @doc 打包异步发送数据
-spec pack_send(pid(), pos_integer(), term()) -> void().
pack_send(RolePid, Code, Data) ->
    Bin = gateway_lib:pack(Code, res, Data),
    send(RolePid, Bin).

%% @doc 发送数据
-spec send(binary()) -> void().
send(Bin) ->
    case get('@is_role_proc') of
        true ->
            gateway_lib:send(get('@gate_pid'), Bin);
        _ ->
            ?error("在非角色进程发送协议数据"),
            ok
    end.
%% @doc 异步发送数据
-spec send(pid(), binary()) -> void().
send(RolePid, Bin) ->
    RolePid ! {send_data, Bin}.

%% @doc 切换网关进程
switch(RolePid, MGateway, Args) ->
    call(RolePid, {switch, MGateway, Args}).

%% @doc 关闭角色进程
stop(RolePid) when is_pid(RolePid) ->
    info(RolePid, {stop, normal});
stop(RoleId) ->
    info(pro_name(RoleId), {stop, normal}).

call(RolePid, Request) ->
    ?scall(RolePid, Request).

cast(RolePid, Request) ->
    gen_server:cast(RolePid, Request).

info(RolePid, Info) ->
    RolePid ! Info.

apply(sync, RolePid, MFA = {_M, _F, _A}) ->
    call(RolePid, {apply, MFA});
apply(sync, RolePid, {F, A}) ->
    call(RolePid, {apply, {undefined, F, A}});
apply(async, RolePid, MFA = {_M, _F, _A}) ->
    info(RolePid, {apply, MFA});
apply(async, RolePid, {F, A}) ->
    info(RolePid, {apply, {undefined, F, A}}).

pro_name({RId, SrvId}) ->
    list_to_atom(lists:concat(["role_", RId, "_", binary_to_list(SrvId)])).

start(RoleId, MGateway, Args) ->
    gen_server:start({local, pro_name(RoleId)}, ?MODULE, [RoleId, MGateway, Args], []).

init([RoleId, MGateway = #m_gateway{pid = GatePid}, Args]) ->
    process_flag(trap_exit, true),
    Now = time_util:timestamp(),
    case role_data:load_role(RoleId) of
        {ok, _Flag, #role{banned_time = BannedTime}} when Now =< BannedTime ->
            {stop, banned};
        {ok, _Flag, #role{data_lock_time = DataLockTime}} when Now =< DataLockTime ->
            {stop, data_lock};
        {ok, _Flag, Role = #role{}} ->
            NRole0 = role_data:update_login_data(Role, Args),
            NRole = NRole0#role{pid = self(), m_gateway = MGateway},
            link(GatePid),
            put('@gate_pid', GatePid),
            put('@is_role_proc', true),
            self() ! init,
            {ok, NRole};
        {error, Err} ->
            {stop, Err}
    end.

handle_call(Request, From, Role) ->
    case catch do_handle_call(Request, From, Role) of
        {reply, Reply, NRole} ->
            {reply, Reply, NRole};
        _Err ->
            ?error("handle_call错误，消息:~w，State:~w，Reason:~w", [Request, Role, _Err]),
            {reply, {error, handle_error}, Role}
    end.

handle_cast(Request, Role) ->
    case catch do_handle_cast(Request, Role) of
        {noreply, NRole} ->
            {noreply, NRole};
        _Err ->
            ?error("handle_cast错误，消息:~w，State:~w，Reason:~w", [Request, Role, _Err]),
            {noreply, Role}
    end.

handle_info(Info, Role) ->
    case catch do_handle_info(Info, Role) of
        {noreply, NRole} ->
            {noreply, NRole};
        {stop, Reason, NRole} ->
            {stop, Reason, NRole};
        _Err ->
            ?error("handle_info错误，消息:~w，State:~w，Reason:~w", [Info, Role, _Err]),
            {noreply, Role}
    end.

terminate(Reason, Role) ->
    ?info("[~w]开始关闭，原因：~w", [?MODULE, Reason]),
    stop(Reason, Role),
    ?info("[~w]关闭完成", [?MODULE]),
    ok.

%% 关闭进程执行操作
stop(_Flag, Role = #role{id = RoleId}) ->
    StopRole =
        case get('@already_stop') of
            undefined ->
                {ok, NRole0} = role_logout:do(Role),
                Now = time_util:timestamp(),
                NRole = NRole0#role{is_online = ?false, logout_time = Now},
                role_query:del_role_query(NRole),
                case role_data:save(NRole) of
                    ok ->
                        role_data:del_cache(RoleId);
                    _ ->
                        role_data:save_cache(NRole),
                        role_data:info({save, RoleId})
                end,
                put('@already_stop', true),
                NRole;
            _ ->
                Role
        end,
    StopRole.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 方法调用处理
do_handle_call({apply, {M, F, A}}, _From, Role) ->
    case catch util:apply(M, F, [Role | A]) of
        {ok, Reply} ->
            {reply, Reply, Role};
        {reply, Reply} ->
            {reply, Reply, Role};
        {ok, Reply, NRole = #role{}} ->
            {reply, Reply, NRole};
        {reply, Reply, NRole = #role{}} ->
            {reply, Reply, NRole};
        _Err ->
            ?error("同步执行~w:~w:~w错误，State:~w，Reason:~w", [M, F, A, Role, _Err]),
            {reply, {error, apply_error}, Role}
    end;

%% 切换网关进程
do_handle_call({switch, MGateway = #m_gateway{pid = GatePid}, Args}, _From, Role = #role{m_gateway = #m_gateway{pid = OldGatePid}}) ->
    OldGatePid ! {stop, switch},
    unlink(OldGatePid),
    NRole0 = role_data:update_login_data(Role, Args),
    NRole = NRole0#role{m_gateway = MGateway, is_sync = ?true},
    link(GatePid),
    put('@gate_pid', GatePid),
%%    put('@login', true),
    util:unset_timer(check_login_timer),
    {reply, true, NRole};

do_handle_call(_Request, _From, Role) ->
    {reply, {error, bad_request}, Role}.

do_handle_cast(_Request, Role) ->
    {noreply, Role}.

%% 方法调用处理
do_handle_info({apply, {M, F, A}}, Role) ->
    case catch util:apply(M, F, [Role | A]) of
        ok ->
            {noreply, Role};
        {ok, NRole = #role{}} ->
            {noreply, NRole};
        {noreply, NRole = #role{}} ->
            {noreply, NRole};
        _Err ->
            ?error("异步执行~w:~w:~w错误，State:~w，Reason:~w", [M, F, A, Role, _Err]),
            {noreply, Role}
    end;

%% 异步触发事件
do_handle_info({fire_trigger, EventTuple}, Role = #role{id = RoleId, name = Name}) ->
    case catch role_trigger:fire(Role, EventTuple) of
        {NRole = #role{}} ->
            {noreply, NRole};
        Err ->
            ?error("角色[~ts]~p异步触发事件失败，事件：~w，错误：~w", [Name, RoleId, EventTuple, Err]),
            {noreply, Role}
    end;

%% 定时器超时
do_handle_info({timeout, Ref, timer_tick}, Role = #role{id = RoleId, name = Name}) ->
    case catch role_timer:tick(Role, Ref) of
        {NRole = #role{}} ->
            {noreply, NRole};
        Err ->
            ?error("角色[~ts]~p执行定时器事件失败，错误：~w", [Name, RoleId, Err]),
            {noreply, Role}
    end;

%% 初始化
do_handle_info(init, Role = #role{m_gateway = #m_gateway{ip = Ip}}) ->
    role_query:set_role_query(by_id, Role),
    Now = time_util:timestamp(),
    NRole0 = Role#role{login_time = Now, login_ip = inet:ntoa(Ip), is_online = ?true},
    case role_login:do(NRole0) of
        {ok, NRole1 = #role{}} ->
            NRole = NRole1#role{logout_time = 0, is_sync = ?true},
%%            put('@login', true), %% 已登录标识
            role_query:set_role_query(by_name, NRole),
            self() ! loop,
            {noreply, NRole};
        {error, _Err} ->
            {stop, normal, Role}
    end;

%% 循环检测
do_handle_info(loop, Role = #role{is_sync = IsSync, m_gateway = #m_gateway{pid = GatePid, sock = Sock}}) ->
    case erlang:process_info(GatePid, message_queue_len) of
        MsgLen when is_integer(MsgLen) andalso MsgLen >= 5000 -> %% 检测网关进程是否拥堵，拥堵则杀掉网关进程
            erlang:exit(GatePid, kill),
            catch inet:close(Sock),
            case util:have_timer(check_login_timer) of
                false ->
                    util:set_ms_timer(check_login_timer, ?min_ms(3), self(), check_login);
                _ ->
                    skip
            end;
        _ ->
            skip
    end,
    NRole =
        case IsSync of
            ?true ->
                self() ! save,
                Role#role{is_sync = ?false};
            _ ->
                Role
        end,
    erlang:send_after(util:rand(5000, 6000), self(), loop),
    {noreply, NRole};

%% 保存角色数据
do_handle_info(save, Role = #role{id = RoleId, name = Name}) ->
    Now = time_util:timestamp(),
    SaveDbTime = util:get('@save_db_time', 0),
    case Now >= SaveDbTime of
        true ->
            case role_data:save(Role) of
                ok ->
                    role_data:del_cache(RoleId),
                    ?debug("角色[~ts]~p保存角色数据到数据库成功", [Name, RoleId]);
                _ ->
                    role_data:save_cache(Role),
                    role_data:info({save, RoleId}),
                    ?debug("角色[~ts]~p保存角色数据到数据库失败，保存到dets", [Name, RoleId])
            end,
            put('@save_db_time', Now + ?min_s);
        _ ->
            role_data:save_cache(Role),
            ?debug("角色[~ts]~p保存角色数据到dets成功", [Name, RoleId])
    end,
    {noreply, Role};

%% 网关进程挂了
do_handle_info({'EXIT', GatePid, _Err}, Role = #role{id = RoleId, name = Name, m_gateway = #m_gateway{pid = GatePid}}) ->
%%    put('@login', false), %% 已登录标识
    ?gate_debug("角色[~ts]~p网关进程挂了，原因：~w", [Name, RoleId, _Err]),
    util:set_ms_timer(check_login_timer, ?min_ms(3), self(), check_login),
    {noreply, Role};

%% 检测是否已登录
do_handle_info({timeout, _Ref, check_login}, Role) ->
    {stop, normal, stop(normal, Role)};

%% Rpc处理
do_handle_info({rpc, RpcMod, Code, Data}, Role) ->
    Ret = (catch RpcMod:handle(Code, Data, Role)),
    case handle_ret(Ret, Code, Role) of
        {noreply, NRole} ->
            get('gate_pid') ! async_recv,
            {noreply, NRole};
        {stop, Reason, NRole} ->
            {stop, Reason, NRole}
    end;

%% 发送协议数据
do_handle_info({send_data, Bin}, Role) ->
    send(Bin),
    {noreply, Role};

%% 关闭角色进程
do_handle_info({stop, Reason}, Role) ->
    {stop, Reason, stop(Reason, Role)};

do_handle_info(_Info, Role) ->
    {noreply, Role}.

handle_ret(ok, _Code, Role) ->
    {noreply, Role};
handle_ret({ok, NRole = #role{}}, _Code, _Role) ->
    {noreply, NRole};
handle_ret({reply, Reply}, Code, Role) ->
    pack_send(Code, Reply),
    {noreply, Role};
handle_ret({reply, Reply, NRole = #role{}}, Code, _Role) ->
    pack_send(Code, Reply),
    {noreply, NRole};
handle_ret({stop, Reason}, _Code, Role) ->
    {stop, Reason, Role};
handle_ret({stop, Reason, NRole = #role{}}, _Code, _Role) ->
    {stop, Reason, NRole};
handle_ret({error, {bad_handle, _Code}}, _Code, Role) ->
    {noreply, Role};
handle_ret({'EXIT', Err}, Code, Role) ->
    ?error("处理协议code：~w，遇到了未处理错误：~w", [Code, Err]),
    {noreply, Role};
handle_ret(Ret, Code, Role) ->
    ?error("未做处理的返回格式：~w，code：~w", [Ret, Code]),
    {noreply, Role}.
