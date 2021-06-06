
-module({mod}).

-include("proto.hrl").

%% API
-export([inc/0, proto/0]).

%% @doc 引用头文件列表
-spec inc() -> list().
inc() ->
    [].

%% @doc 定义协议
-spec proto() -> [#proto{}|_].
proto() ->
    [].
