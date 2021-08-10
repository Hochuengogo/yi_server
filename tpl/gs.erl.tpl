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

-behaviour(gen_server).

%% API
-export([call/1, cast/1, info/1, apply/2, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("logs.hrl").

-record(state, {}).

call(Request) ->
    ?scall(?MODULE, Request).

cast(Request) ->
    gen_server:cast(?MODULE, Request).

info(Info) ->
    ?MODULE ! Info.

apply(sync, MFA = {M, F, A}) ->
    call({apply, MFA});
apply(sync, FA = {F, A}) ->
    call({apply, FA});
apply(async, MFA = {M, F, A}) ->
    info({apply, MFA});
apply(async, FA = {F, A}) ->
    info({apply, FA}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    ?info("[~w]开始启动", [?MODULE]),
    process_flag(trap_exit, true),
    ?info("[~w]启动完成", [?MODULE]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    case catch do_handle_call(Request, From, State) of
        {reply, Reply, NewState} ->
             {reply, Reply, NewState};
        _Err ->
            ?error("handle_call错误，消息:~w，State:~w，Reason:~w", [Request, State, _Err]),
            {reply, {error, handle_error}, State}
    end.

handle_cast(Request, State) ->
    case catch do_handle_cast(Request, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("handle_cast错误，消息:~w，State:~w，Reason:~w", [Request, State, _Err]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("handle_info错误，消息:~w，State:~w，Reason:~w", [Info, State, _Err]),
            {noreply, State}
    end.

terminate(Reason, _State) ->
    ?info("[~w]开始关闭，原因：~w", [?MODULE, Reason]),
    ?info("[~w]关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_call({apply, {M, F, A}}, _From, State) ->
    case catch util:apply({M, F, [State | A]}) of
        {ok, Reply} ->
            {reply, Reply, State};
        {reply, Reply} ->
            {reply, Reply, State};
        {ok, Reply, NewState} ->
            {reply, Reply, NewState};
        {reply, Reply, NewState} ->
            {reply, Reply, NewState};
        _Err ->
            ?error("同步执行~w:~w:~w错误，State:~w，Reason:~w", [M, F, A, State, _Err]),
            {reply, {error, apply_error}, State}
    end;
do_handle_call({apply, {F, A}}, _From, State) ->
    case catch util:apply({F, [State | A]}) of
        {ok, Reply} ->
            {reply, Reply, State};
        {reply, Reply} ->
            {reply, Reply, State};
        {ok, Reply, NewState} ->
            {reply, Reply, NewState};
        {reply, Reply, NewState} ->
            {reply, Reply, NewState};
        _Err ->
            ?error("同步执行~w:~w错误，State:~w，Reason:~w", [F, A, State, _Err]),
            {reply, {error, apply_error}, State}
    end;

do_handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

do_handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_info({apply, {M, F, A}}, State) ->
    case catch util:apply({M, F, [State | A]}) of
        ok ->
            {noreply, State};
        {ok, NewState} ->
            {noreply, NewState};
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("异步执行~w:~w:~w错误，State:~w，Reason:~w", [M, F, A, State, _Err]),
            {noreply, State}
    end;
do_handle_info({apply, {F, A}}, State) ->
    case catch util:apply({F, [State | A]}) of
        ok ->
            {noreply, State};
        {ok, NewState} ->
            {noreply, NewState};
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            ?error("异步执行~w:~w错误，State:~w，Reason:~w", [F, A, State, _Err]),
            {noreply, State}
    end;

do_handle_info(_Info, State) ->
    {noreply, State}.