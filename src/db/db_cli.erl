%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc mysql池化client进程
%%%
%%% @end
%%% Created : 18. 7月 2020 16:50
%%%-------------------------------------------------------------------
-module(db_cli).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    {ok, Conn} = mysql:start_link(Args),
    {ok, #state{conn = Conn}}.

handle_call(is_connected, _From, State = #state{conn = Conn}) ->
    Reply = mysql:is_connected(Conn),
    {reply, Reply, State};

handle_call({query, Query}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:query(Conn, Query),
    {reply, Reply, State};

handle_call({query, Query, Args}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:query(Conn, Query, Args),
    {reply, Reply, State};

handle_call({query, Query, Args, Timeout}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:query(Conn, Query, Args, Timeout),
    {reply, Reply, State};

handle_call({query, Query, Args, FilterMap, Timeout}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:query(Conn, Query, Args, FilterMap, Timeout),
    {reply, Reply, State};

handle_call({execute, Ref, Args}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:execute(Conn, Ref, Args),
    {reply, Reply, State};

handle_call({execute, Ref, Args, Timeout}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:execute(Conn, Ref, Args, Timeout),
    {reply, Reply, State};

handle_call({execute, Ref, Args, FilterMap, Timeout}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:execute(Conn, Ref, Args, FilterMap, Timeout),
    {reply, Reply, State};

handle_call({prepare, Query}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:prepare(Conn, Query),
    {reply, Reply, State};

handle_call({prepare, Name, Query}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:prepare(Conn, Name, Query),
    {reply, Reply, State};

handle_call({unprepare, Ref}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:unprepare(Conn, Ref),
    {reply, Reply, State};

handle_call(warning_count, _From, State = #state{conn = Conn}) ->
    Reply = mysql:warning_count(Conn),
    {reply, Reply, State};

handle_call(affected_rows, _From, State = #state{conn = Conn}) ->
    Reply = mysql:affected_rows(Conn),
    {reply, Reply, State};

handle_call(autocommit, _From, State = #state{conn = Conn}) ->
    Reply = mysql:autocommit(Conn),
    {reply, Reply, State};

handle_call(insert_id, _From, State = #state{conn = Conn}) ->
    Reply = mysql:insert_id(Conn),
    {reply, Reply, State};

handle_call({encode, Term}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:encode(Conn, Term),
    {reply, Reply, State};

handle_call(in_transaction, _From, State = #state{conn = Conn}) ->
    Reply = mysql:in_transaction(Conn),
    {reply, Reply, State};

handle_call({transaction, Fun}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:transaction(Conn, fun() -> Fun(Conn) end),
    {reply, Reply, State};

handle_call({transaction, Fun, Retries}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:transaction(Conn, fun() -> Fun(Conn) end, Retries),
    {reply, Reply, State};

handle_call({transaction, Fun, Args, Retries}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:transaction(Conn, fun() -> Fun(Conn) end, Args, Retries),
    {reply, Reply, State};

handle_call({change_user, Username, Password}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:change_user(Conn, Username, Password),
    {reply, Reply, State};

handle_call({change_user, Username, Password, Options}, _From, State = #state{conn = Conn}) ->
    Reply = mysql:change_user(Conn, Username, Password, Options),
    {reply, Reply, State};

handle_call(reset_connection, _From, State = #state{conn = Conn}) ->
    Reply = mysql:reset_connection(Conn),
    {reply, Reply, State};

handle_call(get_conn, _From, State = #state{conn = Conn}) ->
    {reply, {ok, Conn}, State};

handle_call(_Call, _From, State) ->
    {reply, {error, bad_call}, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{conn = Conn}) ->
    mysql:stop(Conn),
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.
