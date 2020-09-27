%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 三月 2019 21:21
%%%-------------------------------------------------------------------
-module(gateway_worker_sup).
-author("huangzaoyi").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

-include("common.hrl").
-include("logs.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    ?info(?start_begin),
    ?info(?start_end),
    SupFlags = #{strategy => simple_one_for_one,
        intensity => 10,
        period => 10},

    Worker = #{
        id => gateway_worker,
        start =>{gateway_worker, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [gateway_worker]
    },
    ChildSpecs = [Worker],
    {ok, {SupFlags, ChildSpecs}}.