%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色数据模块
%%% @end
%%% Created : 2021-09-17 00:04:23
%%%-------------------------------------------------------------------
-module(role_data).
-author("jiaoyinyi").

-behaviour(gen_server).

-export([
    init_role/1
    , load_role/1
    , load_role_base/1
    , load_role_data/1
    , save/1
    , save_role_base/1
    , save_role_data/1
    , update_login_data/2
    , get_cache/1
    , save_cache/1
    , del_cache/1
]).
%% API
-export([call/1, cast/1, info/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").
-include("trigger.hrl").
-include("stimer.hrl").

-record(state, {queue}).
-define(role_data_num, 5).

%% @doc 初始化角色数据
-spec init_role(#role_base{}) -> #role{}.
init_role(#role_base{rid = RId, srv_id = SrvId, account = Account, name = Name, career = Career, type = Type, sex = Sex
    , face_id = FaceId, reg_channel = RegChannel, channel = Channel, reg_time = RegTime, reg_ip = RegIp, reg_idfa = RegIdfa
    , idfa = Idfa, reg_device_id = RegDeviceId, device_id = DeviceId, reg_os_name = RegOsName, os_name = OsName
    , reg_package_name = RegPackageName, package_name = PackageName, reg_package_version = RegPackageVersion
    , package_version = PackageVersion, platform = Platform
}) ->
    #role{
        ver = ?role_ver
        , id = {RId, SrvId}
        , account = Account
        , name = Name
        , career = Career
        , type = Type
        , sex = Sex
        , face_id = FaceId
        , reg_channel = RegChannel
        , channel = Channel
        , reg_time = RegTime
        , reg_ip = RegIp
        , reg_idfa = RegIdfa
        , idfa = Idfa
        , reg_device_id = RegDeviceId
        , device_id = DeviceId
        , reg_os_name = RegOsName
        , os_name = OsName
        , reg_package_name = RegPackageName
        , package_name = PackageName
        , reg_package_version = RegPackageVersion
        , package_version = PackageVersion
        , platform = Platform
        , s_trigger = #s_trigger{}
        , s_timer = #s_timer{}
    }.

%% @doc 加载角色数据
-spec load_role(role_id()) -> {ok, normal | init, #role{}} | {error, term()}.
load_role(RoleId) ->
    case load_role_base(RoleId) of
        {ok, RoleBase} ->
            case load_role_data(RoleId) of
                {ok, Data} ->
                    case ver:parse(Data, role_ver) of
                        {ok, Role = #role{}} ->
                            {ok, normal, Role#role{s_trigger = #s_trigger{}, s_timer = #s_timer{}}};
                        false ->
                            {error, data_ver_error}
                    end;
                {error, not_exist} ->
                    Role = init_role(RoleBase),
                    {ok, init, Role};
                {error, Err} ->
                    {error, Err}
            end;
        {error, Err} ->
            {error, Err}
    end.

%% @doc 加载基础数据
-spec load_role_base(role_id()) -> {ok, #role_base{}} | {error, term()}.
load_role_base({RId, SrvId}) ->
    Sql = <<"SELECT rid,srv_id,account,name,career,type,sex,face_id,lev,vip_lev,power,max_power,login_time,logout_time
            ,login_ip,is_online,banned_time,data_lock_time,identity,reg_channel,channel,reg_time,reg_ip,reg_idfa,idfa
            ,reg_device_id,device_id,reg_os_name,os_name,reg_package_name,package_name,reg_package_version,package_version
            ,platform FROM role_base WHERE rid=? AND srv_id=?;">>,
    case db:query(Sql, [RId, SrvId], 5000) of
        {ok, [{RId, SrvId, Account, Name, Career, Type, Sex, FaceId, Lev, VipLev, Power, MaxPower, LoginTime, LogoutTime
            , LoginIp, IsOnline, BannedTime, DataLockTime, Identity, RegChannel, Channel, RegTime, RegIp, RegIdfa, Idfa
            , RegDeviceId, DeviceId, RegOsName, OsName, RegPackageName, PackageName, RegPackageVersion, PackageVersion
            , Platform}]} ->
            {ok, #role_base{
                rid = RId
                , srv_id = SrvId
                , account = Account
                , name = Name
                , career = Career
                , type = Type
                , sex = Sex
                , face_id = FaceId
                , lev = Lev
                , vip_lev = VipLev
                , power = Power
                , max_power = MaxPower
                , login_time = LoginTime
                , logout_time = LogoutTime
                , login_ip = LoginIp
                , is_online = IsOnline
                , banned_time = BannedTime
                , data_lock_time = DataLockTime
                , identity = Identity
                , reg_channel = RegChannel
                , channel = Channel
                , reg_time = RegTime
                , reg_ip = RegIp
                , reg_idfa = RegIdfa
                , idfa = Idfa
                , reg_device_id = RegDeviceId
                , device_id = DeviceId
                , reg_os_name = RegOsName
                , os_name = OsName
                , reg_package_name = RegPackageName
                , package_name = PackageName
                , reg_package_version = RegPackageVersion
                , package_version = PackageVersion
                , platform = Platform
            }};
        {ok, []} ->
            {error, not_exist};
        {error, Err} ->
            ?error("获取角色基础数据失败，角色ID：~w，服务器ID：~w，错误：~w", [RId, SrvId, Err]),
            {error, Err};
        Err ->
            ?error("获取角色基础数据失败，角色ID：~w，服务器ID：~w，错误：~w", [RId, SrvId, Err]),
            {error, Err}
    end.

%% @doc 加载详细数据
-spec load_role_data(role_id()) -> {ok, term()} | {error, term()}.
load_role_data({RId, SrvId}) ->
    Sql = <<"SELECT data FROM role_data WHERE rid=? AND srv_id=?;">>,
    case db:query(Sql, [RId, SrvId], 5000) of
        {ok, [Data]} ->
            NewData = util:string_to_term(Data),
            {ok, NewData};
        {ok, []} ->
            {error, not_exist};
        {error, Err} ->
            ?error("获取角色详细数据失败，角色ID：~w，服务器ID：~w，错误：~w", [RId, SrvId, Err]),
            {error, Err};
        Err ->
            ?error("获取角色详细数据失败，角色ID：~w，服务器ID：~w，错误：~w", [RId, SrvId, Err]),
            {error, Err}
    end.

%% 保存数据
-spec save(#role{}) -> ok | {error, term()}.
save(Role) ->
    {ok, NRole} = role_convert:do(save_role, Role),
    save_role_base(NRole),
    case save_role_data(NRole) of
        ok ->
            ok;
        {error, Err} ->
            {error, Err}
    end.

%% @doc 保存基础数据
-spec save_role_base(#role{}) -> ok | {error, term()}.
save_role_base(#role{id = {RId, SrvId}, account = Account, name = Name, career = Career, type = Type, sex = Sex, face_id = FaceId
    , lev = Lev, vip_lev = VipLev, power = Power, max_power = MaxPower, login_time = LoginTime, logout_time = LogoutTime
    , login_ip = LoginIp, is_online = IsOnline, banned_time = BannedTime, data_lock_time = DataLockTime, identity = Identity
    , reg_channel = RegChannel, channel = Channel, reg_time = RegTime, reg_ip = RegIp, reg_idfa = RegIdfa, idfa = Idfa
    , reg_device_id = RegDeviceId, device_id = DeviceId, reg_os_name = RegOsName, os_name = OsName, reg_package_name = RegPackageName
    , package_name = PackageName, reg_package_version = RegPackageVersion, package_version = PackageVersion
    , platform = Platform
}) ->
    Sql = <<"REPLACE INTO role_base (rid,srv_id,account,name,career,type,sex,face_id,lev,vip_lev,power,max_power,login_time,logout_time
            ,login_ip,is_online,banned_time,data_lock_time,identity,reg_channel,channel,reg_time,reg_ip,reg_idfa,idfa
            ,reg_device_id,device_id,reg_os_name,os_name,reg_package_name,package_name,reg_package_version,package_version
            ,platform) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);">>,
    Args = [RId, SrvId, Account, Name, Career, Type, Sex, FaceId, Lev, VipLev, Power, MaxPower, LoginTime, LogoutTime
        , LoginIp, IsOnline, BannedTime, DataLockTime, Identity, RegChannel, Channel, RegTime, RegIp, RegIdfa, Idfa
        , RegDeviceId, DeviceId, RegOsName, OsName, RegPackageName, PackageName, RegPackageVersion, PackageVersion,
        Platform],
    case db:query(Sql, Args, 5000) of
        ok ->
            ok;
        {error, Err} ->
            ?error("角色[~ts]~p保存角色基础数据失败，原因：~w", [Name, {RId, SrvId}, Err]),
            {error, Err}
    end.

%% @doc 保存详细数据
-spec save_role_data(#role{}) -> ok | {error, term()}.
save_role_data(Role = #role{id = {RId, SrvId}, name = Name}) ->
    Data = util:term_to_string(Role),
    Sql = <<"REPLACE INTO role_data (rid,srv_id,data) VALUES (?,?,?);">>,
    case db:query(Sql, [RId, SrvId, Data], 5000) of
        ok ->
            ok;
        {error, Err} ->
            ?error("角色[~ts]~p保存角色详细数据失败，原因：~w", [Name, {RId, SrvId}, Err]),
            {error, Err}
    end.

%% 更新登录参数
update_login_data(Role = #role{}, Args) ->
    Role#role{
        channel = list_util:keyfind(channel, Args, <<>>)
        , idfa = list_util:keyfind(idfa, Args, <<>>)
        , device_id = list_util:keyfind(device_id, Args, <<>>)
        , os_name = list_util:keyfind(os_name, Args, <<>>)
        , package_name = list_util:keyfind(package_name, Args, <<>>)
        , package_version = list_util:keyfind(package_version, Args, <<>>)
    }.

%% @doc 获取dets中的角色数据
-spec get_cache(role_id()) -> {ok, term()} | {error, not_exist}.
get_cache(RoleId) ->
    case dets:lookup(tab(RoleId), RoleId) of
        [Data] ->
            {ok, Data};
        [] ->
            {error, not_exist}
    end.

%% @doc 保存角色数据到dets
-spec save_cache(#role{}) -> ok | {error, term()}.
save_cache(Role = #role{id = RoleId}) ->
    {ok, NRole} = role_convert:do(save_role, Role),
    dets:insert(tab(RoleId), NRole).

%% @doc 删除角色数据
-spec del_cache(#role{}) -> ok | {error, term()}.
del_cache(RoleId) ->
    dets:delete(tab(RoleId), RoleId).

%% 表名
tab({RId, _}) -> tab(RId);
tab(1) -> role_data_1;
tab(2) -> role_data_2;
tab(3) -> role_data_3;
tab(4) -> role_data_4;
tab(5) -> role_data_5;
tab(N) when N > ?role_data_num ->
    tab(N rem ?role_data_num + 1).

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
    open_dets(?role_data_num),
    save_db(?role_data_num),
    {ok, [MaxRoleId0]} = db:query(<<"SELECT MAX(rid) FROM role_base;">>, 5000),
    MaxRoleId = ?if_true(MaxRoleId0 =:= null, 0, MaxRoleId0),
    srv_config:save(max_role_id, MaxRoleId),
    {ok, [RoleNum]} = db:query(<<"SELECT COUNT(*) FROM role_base;">>, 5000),
    srv_config:save(role_num, RoleNum),
    State = #state{queue = queue:new()},
    ?info("[~w]启动完成", [?MODULE]),
    {ok, State}.

open_dets(N) when N =< 0 ->
    ok;
open_dets(N) ->
    TabName = tab(N),
    {ok, _} = dets:open_file(TabName, [{file, lists:concat(["./dets/", TabName, ".dets"])}, {type, set}, {keypos, #role.id}]),
    open_dets(N - 1).

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
    save_db(?role_data_num),
    ?info("[~w]关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

do_handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_info({save, RoleId}, State = #state{queue = Queue}) ->
    NewState =
        case queue:member(RoleId, Queue) of
            false ->
                case util:have_timer(save_timer) of
                    false ->
                        util:set_ms_timer(save_timer, util:rand(2000, 5000), do_save);
                    _ ->
                        skip
                end,
                State#state{queue = queue:in(RoleId, Queue)};
            _ ->
                State
        end,
    {noreply, NewState};

do_handle_info({timeout, _Ref, do_save}, State = #state{queue = Queue}) ->
    util:clear_timer(save_timer),
    NewQueue =
        case queue:out(Queue) of
            {{value, RoleId}, NewQueue0} ->
                case get_cache(RoleId) of
                    {ok, Data} ->
                        case ver:parse(Data, role_ver) of
                            {ok, Role = #role{id = RoleId}} ->
                                case save(Role) of
                                    ok ->
                                        dets:delete(tab(RoleId), RoleId),
                                        NewQueue0;
                                    _ ->
                                        Queue
                                end;
                            _ ->
                                Queue
                        end;
                    _ ->
                        NewQueue0
                end;
            {empty, NewQueue0} ->
                NewQueue0
        end,
    case queue:is_empty(NewQueue) of
        false ->
            util:set_ms_timer(save_timer, util:rand(500, 2000), do_save);
        _ ->
            skip
    end,
    NewState = State#state{queue = NewQueue},
    {noreply, NewState};

do_handle_info(_Info, State) ->
    {noreply, State}.

%% 保存数据到数据库中
save_db(N) when N =< 0 ->
    ok;
save_db(N) ->
    put(save_error_flag, false),
    put(del_role_ids, []),
    TabName = tab(N),
    dets:traverse(TabName,
        fun(Data) ->
            case ver:parse(Data, role_ver) of
                {ok, Role = #role{id = RoleId}} ->
                    case save(Role) of
                        ok ->
                            put(del_role_ids, [RoleId | get(del_role_ids)]);
                        _ ->
                            ?error("保存玩家~p详细数据失败", [RoleId]),
                            put(save_error_flag, true)
                    end;
                _ ->
                    put(save_error_flag, true)
            end,
            continue
        end
    ),
    case erase(save_error_flag) of
        false ->
            dets:delete_all_objects(TabName),
            ?info("[~w]所有角色数据保存成功", [TabName]);
        _ ->
            [dets:delete(TabName, RoleId) || RoleId <- erase(del_role_ids)],
            ?error("[~w]有角色数据保存失败", [TabName])
    end,
    save_db(N - 1).
