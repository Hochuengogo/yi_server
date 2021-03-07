%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 8月 2020 11:35 下午
%%%-------------------------------------------------------------------
-module(db_lib).
-author("huangzaoyi").

%% API
-export([start_link/0]).

start_link() ->
    {SizeArgs, WorkerArgs} = srv_config:get(db_options),
    db:start_link(SizeArgs, WorkerArgs).
