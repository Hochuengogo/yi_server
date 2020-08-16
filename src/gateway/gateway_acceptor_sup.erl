%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关接收器supervisor
%%%
%%% @end
%%% Created : 16. 三月 2020 01:18
%%%-------------------------------------------------------------------
-module(gateway_acceptor_sup).
-author("huangzaoyi").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("common.hrl").
-include("logs.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?info(?start_begin),
    ?info(?start_end),
    {ok, {{one_for_one, 10, 10}, []}}.
