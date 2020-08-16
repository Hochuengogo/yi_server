%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关rpc
%%%
%%% @end
%%% Created : 14. 8月 2020 11:25 下午
%%%-------------------------------------------------------------------
-module(gateway_rpc).
-author("huangzaoyi").

%% API
-export([handle/3]).

-include("logs.hrl").

handle(Code, _Data, _Gateway) ->
    ?error("错误的rpc处理, 代码:~w", [Code]),
    {error, {bad_handle, Code}}.