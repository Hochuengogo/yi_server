%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 三月 2020 17:08
%%%-------------------------------------------------------------------
-module(manager).
-author("huangzaoyi").

%% API
-export([start/0, start/1, stop/0]).

-include("common.hrl").
-include("logs.hrl").
-include("manager.hrl").

-define(start_normal_ids, [
    gateway_sup, gateway_acceptor_sup, gateway_worker_sup, gateway_listener
]).

-define(start_center_ids, [
    gateway_sup, gateway_acceptor_sup, gateway_worker_sup, gateway_listener
]).

%% @doc 启动系统
-spec start() -> ok | {error, term()}.
start() ->
    application:start(yi_server).

%% @doc 关闭系统
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(yi_server).

%% @doc 根据服务器类型启动服务
start(normal) ->
    case start_service(?start_normal_ids) of
        ok ->
            ?info("启动服务器成功");
        _ ->
            ?error("启动服务器失败")
    end;
start(center) ->
    case start_service(?start_center_ids) of
        ok ->
            ?info("启动服务器成功");
        _ ->
            ?error("启动服务器失败")
    end.

%% 启动服务
start_service([]) -> ok;
start_service([Id | Ids]) ->
    case catch start_service(get_service(Id)) of
        {ok, _Pid} ->
            start_service(Ids);
        _Err ->
            ?error("启动服务失败, 原因:~w", [_Err]),
            false
    end;
start_service(Service = #service{depend_on = SupMod}) when SupMod /= undefined ->
    supervisor:start_child(SupMod, child_spec(Service));
start_service(Service = #service{}) ->
    supervisor:start_child(worker_sup, child_spec(Service));
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
get_service(gateway_sup) ->
    #service{
        id = gateway_sup,
        start = {gateway_sup, start_link, []},
        type = supervisor,
        depend_on = yiserver_sup
    };
get_service(gateway_acceptor_sup) ->
    #service{
        id = gateway_acceptor_sup,
        start = {gateway_acceptor_sup, start_link, []},
        type = supervisor,
        depend_on = gateway_sup
    };
get_service(gateway_worker_sup) ->
    #service{
        id = gateway_worker_sup,
        start = {gateway_worker_sup, start_link, []},
        type = supervisor,
        depend_on = gateway_sup
    };
get_service(gateway_listener) ->
    #service{
        id = gateway_listener,
        start = {gateway_listener, start_link, []},
        type = worker,
        depend_on = gateway_sup
    }.