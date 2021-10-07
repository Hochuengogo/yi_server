%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 网关协议处理
%%% @end
%%% Created : 2021-08-15 23:35:15
%%%-------------------------------------------------------------------
-module(gateway_handle).
-author("jiaoyinyi").

%% API
-export([
    account_login/2
    , register/2
    , login/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("gateway.hrl").
-include("role.hrl").

%% @doc 账号登录
-spec account_login(#gateway{}, list()) -> {ok, #gateway{}, list()} | {false, binary()}.
account_login(Gateway, Data) ->
    case check_account_login([account, timestamp, sign], Data, #{}) of
        {true, #{account := Account}} ->
            case db:query(<<"SELECT rid,srv_id,name,lev,sex,career FROM role_base WHERE account = ?;">>, [Account], 5000) of
                {ok, RoleInfo} ->
                    put(account, Account),
                    put(role_ids, [{RId, SrvId} || {RId, SrvId, _, _, _, _} <- RoleInfo]),
                    set_data([channel, idfa, device_id, os_name, package_name, package_version], Data),
                    NewGateway = Gateway#gateway{is_login = true},
                    {ok, NewGateway, RoleInfo};
                {error, Reason} ->
                    ?error("获取服务器角色信息失败，原因：~w", [Reason]),
                    {false, ?str("获取服务器角色信息失败")}
            end;
        {false, Msg} ->
            {false, Msg}
    end.

%% 判断账号登录参数
check_account_login([], _Data, Cache) ->
    {true, Cache};
check_account_login([Cond | Conds], Data, Cache) ->
    case check_account_login(Cond, Data, Cache) of
        true ->
            check_account_login(Conds, Data, Cache);
        {true, NewCache} ->
            check_account_login(Conds, Data, NewCache);
        {false, Msg} ->
            {false, Msg}
    end;
%% 账号 账号详细检测未处理
check_account_login(account, Data, Cache) ->
    case lists:keyfind(<<"account">>, 1, Data) of
        {_, Account} when Account =/= <<>> ->
            {true, Cache#{account => Account}};
        _ ->
            {false, ?str("account error")}
    end;
%% 时间戳
check_account_login(timestamp, Data, Cache) ->
    case lists:keyfind(<<"timestamp">>, 1, Data) of
        {_, Timestamp} ->
            case catch binary_to_integer(Timestamp) of
                {'EXIT', _Err} ->
                    {false, ?str("timestamp error")};
                IntTimeStamp ->
                    Now = time_util:timestamp(),
                    case IntTimeStamp >= Now - ?hour_s andalso IntTimeStamp =< Now + ?hour_s of
                        true ->
                            {true, Cache#{timestamp => Timestamp}};
                        _ ->
                            {false, ?str("timestamp error")}
                    end
            end;
        _ ->
            {false, ?str("timestamp error")}
    end;
%% 令牌
check_account_login(sign, Data, #{account := Account, timestamp := Timestamp}) ->
    case lists:keyfind(<<"sign">>, 1, Data) of
        {_, Sign} when Sign =/= <<>> ->
            SecretKey = <<"x9%Dl%TnGX!oHKnNPN2SjluhT*Ei7OO5">>,
            Platform = srv_lib:platform(),
            case Sign =:= crypto_util:md5_hex_string([Account, Platform, SecretKey, Timestamp]) of
                true ->
                    true;
                _ ->
                    {false, ?str("sign error")}
            end;
        _ ->
            {false, ?str("sign error")}
    end;
check_account_login(_Cond, _Data, _Cache) ->
    {false, <<>>}.

%% 设置字段
set_data([], _Data) ->
    ok;
set_data([Cond | Conds], Data) ->
    put(Cond, list_util:keyfind(atom_to_binary(Cond), Data, <<>>)),
    set_data(Conds, Data).

%% @doc 注册角色
-spec register(#gateway{}, list()) -> {ok, pos_integer(), binary()} | {false, msg()}.
register(#gateway{ip = Ip}, Data) ->
    case length(get(role_ids)) < ?register_max_role_num of
        true ->
            case srv_config:get(role_num, 0) < ?max_role_num of
                true ->
                    case role_name:rand(?role_sex_male) of
                        {ok, Name} ->
                            case check_register([], Data, #{}) of
                                {true, #{}} ->
                                    RId = srv_config:update_counter(max_role_id, 1),
                                    Sql =
                                        <<"INSERT INTO role_base (rid,srv_id,account,name,career,type,sex,reg_channel,channel,
                                        reg_time,reg_ip,reg_idfa,idfa,reg_device_id,device_id,reg_os_name,os_name,
                                        reg_package_name,package_name,reg_package_version,package_version,platform)
                                        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);">>,
                                    SrvId = srv_lib:server_id(),
                                    Platform = srv_lib:platform(),
                                    Args = [RId, SrvId, get(account), Name, 0, 0, ?role_sex_male, get(channel), get(channel),
                                        time_util:timestamp(), inet:ntoa(Ip), get(idfa), get(idfa), get(device_id), get(device_id), get(os_name), get(os_name),
                                        get(package_name), get(package_name), get(package_version), get(package_version), Platform],
                                    case db:query(Sql, Args, 5000) of
                                        ok ->
                                            put(role_ids, [{RId, SrvId} | get(role_ids)]),
                                            catch srv_config:update_counter(role_num, 1),
                                            {ok, RId, SrvId};
                                        {error, Reason} ->
                                            ?error("注册角色失败，参数：~w，错误：~w", [Args, Reason]),
                                            {false, ?str("注册角色失败")}
                                    end;
                                {false, Msg} ->
                                    {false, Msg}
                            end;
                        _ ->
                            ?error("随机名字失败"),
                            {false, ?str("注册角色失败")}
                    end;
                _ ->
                    {false, ?str("已达到服务器角色注册上限")}
            end;
        _ ->
            {false, ?str("一个账号在同一个服只能注册~w个角色", [?register_max_role_num])}
    end.

%% 判断注册角色参数
check_register([], _Data, Cache) ->
    {true, Cache};
check_register([Cond | Conds], Data, Cache) ->
    case check_register(Cond, Data, Cache) of
        true ->
            check_register(Conds, Data, Cache);
        {true, NewCache} ->
            check_register(Conds, Data, NewCache);
        {false, Msg} ->
            {false, Msg}
    end;
%% 名字 判断名字是否重复，未处理
check_register(name, Data, Cache) ->
    case lists:keyfind(<<"name">>, 1, Data) of
        {_, Name} ->
            case role_lib:check_name(Name, {0, <<>>}) of
                true ->
                    {true, Cache#{name => Name}};
                {false, Msg} ->
                    {false, Msg}
            end;
        _ ->
            {false, ?str("名字错误")}
    end;
%% 职业
check_register(career, Data, Cache) ->
    case lists:keyfind(<<"career">>, 1, Data) of
        {_, Career} ->
            case catch binary_to_integer(Career) of
                NewCareer when is_integer(NewCareer) ->
                    case lists:member(NewCareer, ?role_career_list) of
                        true ->
                            {true, Cache#{career => NewCareer}};
                        _ ->
                            {false, ?str("不存在该职业")}
                    end;
                _ ->
                    {false, ?str("职业错误")}
            end;
        _ ->
            {false, ?str("职业错误")}
    end;
%% 性别
check_register(sex, Data, Cache) ->
    case lists:keyfind(<<"sex">>, 1, Data) of
        {_, Sex} ->
            case catch binary_to_integer(Sex) of
                NewSex when is_integer(NewSex) ->
                    case lists:member(NewSex, ?role_sex_list) of
                        true ->
                            {true, Cache#{sex => NewSex}};
                        _ ->
                            {false, ?str("不存在该性别")}
                    end;
                _ ->
                    {false, ?str("性别错误")}
            end;
        _ ->
            {false, ?str("性别错误")}
    end.

%% @doc 登录角色
-spec login(#gateway{}, role_id()) -> {ok, #gateway{}} | {false, msg()}.
login(Gateway, RoleId) ->
    case lists:member(RoleId, get(role_ids)) of
        true ->
            case role_query:role_pid(by_id, RoleId) of
                {ok, RolePid} ->
                    case role:switch(RolePid, gateway_lib:to_m_gateway(Gateway), login_args()) of
                        true ->
                            NewGateway = Gateway#gateway{role_pid = RolePid, role_id = RoleId},
                            {ok, NewGateway};
                        {error, Err} ->
                            ?error("角色~p切换登录失败，原因：~w", [RoleId, Err]),
                            {false, ?str("登录失败")}
                    end;
                _ ->
                    case role:start(RoleId, gateway_lib:to_m_gateway(Gateway), login_args()) of
                        {ok, RolePid} ->
                            NewGateway = Gateway#gateway{role_pid = RolePid, role_id = RoleId},
                            {ok, NewGateway};
                        {error, Err} ->
                            ?error("角色~p登录失败，原因：~w", [RoleId, Err]),
                            {false, ?str("登录失败")}
                    end
            end;
        _ ->
            {false, ?str("角色不存在")}
    end.

%% 登录时用到的参数
login_args() ->
    [
        {account, get(account)}, {channel, get(channel)}, {idfa, get(idfa)}, {device_id, get(device_id)}, {os_name, get(os_name)},
        {package_name, get(package_name)}, {package_version, get(package_version)}
    ].
