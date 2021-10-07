%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 数据库操作接口
%%%
%%% @end
%%% Created : 18. 7月 2020 17:24
%%%-------------------------------------------------------------------
-module(db).
-author("jiaoyinyi").

%% API
-export([
    start_link/0,
    query/1, query/2, query/3, query/4,
    get_worker/0, put_worker/1,
    get_conn/1
]).

-include("common.hrl").
-include("logs.hrl").

-define(DB_CALL(Cli_Pid, Call), gen_server:call(Cli_Pid, Call)).
-define(POOL_NAME, db_pool).

start_link() ->
    ?info(?start_begin),
    {SizeArgs, WorkerArgs} = srv_config:get(db_options),
    PoolArgs = [{name, {local, ?POOL_NAME}}, {worker_module, db_cli}] ++ SizeArgs,
    Ret = poolboy:start_link(PoolArgs, WorkerArgs),
    ?info(?start_end),
    Ret.

-spec query(Query) -> Result
    when Query :: iodata(),
    Result :: ok | {ok, list()} | {error, term()}.
query(Query) ->
    Ret = (catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {query, Query}) end)),
    handle_ret(Ret).

-spec query(Query, Args | FilterMap | Timeout) -> Result
    when Query :: iodata(),
    Timeout :: timeout(),
    Args :: [term()],
    FilterMap :: mysql:query_filtermap_fun(),
    Result :: ok | {ok, list()} | {error, term()}.
query(Query, Args) when is_list(Args) ->
    Ret = (catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {query, Query, Args}) end)),
    handle_ret(Ret);

query(Query, FilterMap) when is_function(FilterMap, 1) orelse is_function(FilterMap, 2) ->
    Ret = (catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {query, Query, FilterMap}) end)),
    handle_ret(Ret);

query(Query, Timeout) when is_integer(Timeout) orelse Timeout =:= infinity ->
    Ret = (catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {query, Query, Timeout}) end)),
    handle_ret(Ret).

-spec query(Query, Args, Timeout) -> Result
    when Query :: iodata(),
    Timeout :: timeout(),
    Args :: [term()],
    Result :: ok | {ok, list()} | {error, term()};
    (Query, FilterMap, Timeout) -> Result
    when Query :: iodata(),
    Timeout :: timeout(),
    FilterMap :: mysql:query_filtermap_fun(),
    Result :: ok | {ok, list()} | {error, term()};
    (Query, Args, FilterMap) -> Result
    when Query :: iodata(),
    Args :: [term()],
    FilterMap :: mysql:query_filtermap_fun(),
    Result :: ok | {ok, list()} | {error, term()}.
query(Query, Args, Timeout) when is_list(Args) andalso (is_integer(Timeout) orelse Timeout =:= infinity) ->
    Ret = (catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {query, Query, Args, Timeout}) end)),
    handle_ret(Ret);

query(Query, FilterMap, Timeout) when (is_function(FilterMap, 1) orelse is_function(FilterMap, 2)) andalso (is_integer(Timeout) orelse Timeout =:= infinity) ->
    Ret = (catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {query, Query, FilterMap, Timeout}) end)),
    handle_ret(Ret);

query(Query, Args, FilterMap) when is_list(Args) andalso (is_function(FilterMap, 1) orelse is_function(FilterMap, 2)) ->
    Ret = (catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {query, Query, Args, FilterMap}) end)),
    handle_ret(Ret).

-spec query(Query, Args, FilterMap, Timeout) -> Result
    when Query :: iodata(),
    Timeout :: timeout(),
    Args :: [term()],
    FilterMap :: mysql:query_filtermap_fun(),
    Result :: ok | {ok, list()} | {error, term()}.
query(Query, Args, FilterMap, Timeout) when is_list(Args) andalso (is_function(FilterMap, 1) orelse is_function(FilterMap, 2)) andalso (is_integer(Timeout) orelse Timeout =:= infinity) ->
    Ret = (catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {query, Query, Args, FilterMap, Timeout}) end)),
    handle_ret(Ret).

%%-spec pool_transaction(fun()) -> {ok, term()} | {error, term()}.
%%pool_transaction(Fun) when is_function(Fun, 1) ->
%%    case catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {transaction, Fun}) end) of
%%        {atomic, Term} ->
%%            {ok, Term};
%%        {aborted, Term} ->
%%            {error, Term};
%%        _Err ->
%%            {error, fail}
%%    end.
%%
%%-spec pool_transaction(fun(), Retries) -> {ok, term()} | {error, term()} when Retries :: non_neg_integer() | infinity.
%%pool_transaction(Fun, Retries) when is_function(Fun, 1) ->
%%    case catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {transaction, Fun, Retries}) end) of
%%        {atomic, Term} ->
%%            {ok, Term};
%%        {aborted, Term} ->
%%            {error, Term};
%%        _Err ->
%%            {error, fail}
%%    end.
%%
%%-spec pool_transaction(fun(), list(), Retries) -> {ok, term()} | {error, term()} when Retries :: non_neg_integer() | infinity.
%%pool_transaction(Fun, Args, Retries) when is_function(Fun, 1) ->
%%    case catch poolboy:transaction(?POOL_NAME, fun(Cli) -> ?DB_CALL(Cli, {transaction, Fun, Args, Retries}) end) of
%%        {atomic, Term} ->
%%            {ok, Term};
%%        {aborted, Term} ->
%%            {error, Term};
%%        _Err ->
%%            {error, fail}
%%    end.

%% @doc 从连接池中取出一个工作进程
-spec get_worker() -> {ok, pid()} | {error, fail}.
get_worker() ->
    case catch poolboy:checkout(?POOL_NAME) of
        Worker when is_pid(Worker) ->
            {ok, Worker};
        _Err ->
            {error, fail}
    end.

%% @doc 将工作进程放回连接池
-spec put_worker(pid()) -> ok.
put_worker(Worker) ->
    poolboy:checkin(?POOL_NAME, Worker).

%% @doc 获取连接进程
-spec get_conn(pid()) -> {ok, pid()} | {error, fail}.
get_conn(Worker) ->
    case catch ?DB_CALL(Worker, get_conn) of
        {ok, Conn} when is_pid(Conn) ->
            {ok, Conn};
        _Err ->
            {error, fail}
    end.

%% 处理返回值
handle_ret({ok, _Cols, Rows}) -> %% 返回查询结果
    {ok, rows(Rows)};
handle_ret({'EXIT', Err}) -> %% 报错
    {error, Err};
handle_ret(Ret) -> %% 其他
    Ret.

rows([]) ->
    [];
rows([Rows = [_]] ) ->
    Rows;
rows(Rows = [[_] | _] ) ->
    [Row || [Row] <- Rows];
rows(Rows) ->
    [list_to_tuple(Row) || Row <- Rows].

