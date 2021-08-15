%%%-------------------------------------------------------------------
%%% @author {author}
%%% @copyright (C) {year}, <COMPANY>
%%% @doc
%%% {desc}
%%% @end
%%% Created : {create_time}
%%%-------------------------------------------------------------------
-module({mod}).
-author("{author}").

%% API
-export([handle/3]).

-include("common.hrl").
-include("logs.hrl").

handle(Code, Data, _Role) ->
    ?error("错误的rpc处理，code：~w，数据：~w", [Code, Data]),
    {error, {bad_handle, Code}}.