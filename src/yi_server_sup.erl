%%%-------------------------------------------------------------------
%% @doc yi_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(yi_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10},
    WorkerSup = #{
        id => worker_sup,
        start => {worker_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [worker_sup]
    },
    Config = #{
        id => config,
        start => {config, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [config]
    },
    ChildSpecs = [Config, WorkerSup],
    {ok, {SupFlags, ChildSpecs}}.
