%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 其他进程的supervisor
%%%
%%% @end
%%% Created : 08. 三月 2020 20:18
%%%-------------------------------------------------------------------
-module(worker_sup).
-author("huangzaoyi").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("logs.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10},
    SrvConfig = #{
        id => srv_config,
        start => {srv_config, start_link, []},
        restart => permanent,
        shutdown => 10000,
        type => worker,
        modules => [srv_config]
    },
    ChildSpecs = [SrvConfig],
    {ok, {SupFlags, ChildSpecs}}.