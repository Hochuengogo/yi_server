%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关supervisor
%%%
%%% @end
%%% Created : 16. 三月 2020 00:44
%%%-------------------------------------------------------------------
-module(gateway_sup).
-author("huangzaoyi").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("logs.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?info("开始启动:~w", [?MODULE]),
    ?info("启动成功:~w", [?MODULE]),
    {ok, {{one_for_one, 10, 5}, []}}.