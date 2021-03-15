%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 8月 2020 16:00
%%%-------------------------------------------------------------------
-module(logs_lib).
-author("jiaoyinyi").

%% API
-export([can_log/1]).

%% @doc 判断是否可以写日志
-spec can_log(debug|info|warn|error) -> boolean().
can_log(Level) ->
    logs:get_level(debug) =< logs:get_level(Level).
