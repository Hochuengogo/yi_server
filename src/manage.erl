%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 三月 2020 17:08
%%%-------------------------------------------------------------------
-module(manage).
-author("huangzaoyi").

%% API
-export([start/0, start/1, stop/0, start_service/1]).

-include("common.hrl").
-include("logs.hrl").
-include("manager.hrl").

%% 要启动的应用
-define(start_apps, [
    logs, yi_server
]).

%% 要启动的进程
-define(start_zone_ids, [
    db, srv_code, srv_time, role_data, role_query, cluster_mgr, gateway_sup, gateway_mgr, gateway_acceptor_sup, gateway_listener
]).

-define(start_center_ids, [
    db, srv_code, srv_time, c_cluster_mgr
]).

%% @doc 启动系统
-spec start() -> ok | {error, term()}.
start() ->
    start_app(?start_apps).

%% @doc 关闭系统
-spec stop() -> ok | {error, term()}.
stop() ->
    stop_app(lists:reverse(?start_apps)).

%% @doc 启动应用
start_app([]) ->
    ok;
start_app([App | Apps]) ->
    {ok, _} = application:ensure_all_started(App),
    start_app(Apps).

%% @doc 关闭应用
stop_app([]) ->
    init:stop(),
    ok;
stop_app([yi_server = App| Apps]) ->
    ok = application:stop(App),
    stop_all_fire(),
    stop_app(Apps);
stop_app([App | Apps]) ->
    ok = application:stop(App),
    stop_app(Apps).

%% @doc 根据服务器类型启动服务
start(zone) ->
    case start_service(?start_zone_ids) of
        ok ->
            start_all_fire();
        _ ->
            ?error("启动服务器失败")
    end;
start(center) ->
    case start_service(?start_center_ids) of
        ok ->
            start_all_fire();
        _ ->
            ?error("启动服务器失败")
    end.

%% @doc 启动服务完成触发
start_all_fire() ->
    SrvStartTime = srv_config:get(server_start_time, 0),
    NewSrvStartTime = SrvStartTime + 1,
    srv_config:save(server_start_time, NewSrvStartTime),
    ?info("启动服务器第[~w]次成功", [NewSrvStartTime]),
    ok.

%% @doc 关闭服务完成触发
stop_all_fire() ->
    ?info("关闭服务器完成"),
    ok.

%% 启动服务
start_service([]) -> ok;
start_service([Id | Ids]) ->
    case catch start_service(Id) of
        {ok, _Pid} ->
            start_service(Ids);
        _Err ->
            ?error("启动服务失败, 原因:~w", [_Err]),
            false
    end;
start_service(Id) when is_atom(Id) ->
    start_service(get_service(Id));
start_service(Service = #service{depend_on = SupMod}) when SupMod =/= undefined ->
    supervisor:start_child(SupMod, child_spec(Service));
start_service(Service = #service{}) ->
    supervisor:start_child(yi_server_sup, child_spec(Service));
start_service(_Service) ->
    {error, service_not_exist}.

child_spec(#service{id = Id, start = Start, restart = Restart, type = Type, shutdown = ShutDown}) ->
    #{
        id => Id,
        start => Start,
        restart => Restart,
        shutdown => ShutDown,
        type => Type
    }.

%% 获取服务配置
get_service(srv_config) ->
    #service{
        id = srv_config,
        start = {srv_config, start_link, []},
        desc = "服务器配置管理进程"
    };
get_service(srv_code) ->
    #service{
        id = srv_code,
        start = {srv_code, start_link, []},
        desc = "模块代码更新管理进程"
    };
get_service(srv_time) ->
    #service{
        id = srv_time,
        start = {srv_time, start_link, []},
        desc = "服务器时间管理进程"
    };
get_service(db) ->
    #service{
        id = db,
        start = {db, start_link, []},
        desc = "mysql数据库连接池"
    };
get_service(gateway_sup) ->
    #service{
        id = gateway_sup,
        start = {gateway_sup, start_link, []},
        type = supervisor,
        shutdown = infinity,
        desc = "网关总supervisor"
    };
get_service(gateway_mgr) ->
    #service{
        id = gateway_mgr,
        start = {gateway_mgr, start_link, []},
        depend_on = gateway_sup,
        desc = "网关管理进程"
    };
get_service(gateway_acceptor_sup) ->
    #service{
        id = gateway_acceptor_sup,
        start = {gateway_acceptor_sup, start_link, []},
        type = supervisor,
        shutdown = infinity,
        depend_on = gateway_sup,
        desc = "网关接收进程supervisor"
    };
get_service(gateway_listener) ->
    #service{
        id = gateway_listener,
        start = {gateway_listener, start_link, []},
        depend_on = gateway_sup,
        desc = "网关监听进程"
    };
get_service(role_data) ->
    #service{
        id = role_data,
        start = {role_data, start_link, []},
        desc = "角色数据管理进程"
    };
get_service(role_query) ->
    #service{
        id = role_query,
        start = {role_query, start_link, []},
        desc = "角色查询进程"
    };
get_service(cluster_mgr) ->
    #service{
        id = cluster_mgr,
        start = {cluster_mgr, start_link, []},
        desc = "游戏服跨服管理进程"
    };
get_service(c_cluster_mgr) ->
    #service{
        id = c_cluster_mgr,
        start = {c_cluster_mgr, start_link, []},
        desc = "中央服跨服管理进程"
    }.