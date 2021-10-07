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
    SrvConfig = #{
        id => srv_config,
        start => {srv_config, start_link, []},
        restart => permanent,
        shutdown => 300000,
        type => worker,
        modules => [srv_config]
    },
    ChildSpecs = [SrvConfig],
    {ok, {SupFlags, ChildSpecs}}.
