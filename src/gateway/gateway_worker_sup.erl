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
    {ok, {{one_for_one, 0, 1}, []}}.