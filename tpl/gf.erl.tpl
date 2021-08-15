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

-behaviour(gen_fsm).

%% API
-export([call/1, cast/1, info/1, apply/2, start_link/0]).
-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3, terminate/3, code_change/4]).
-export([
    idle/2
]).

-include("common.hrl").
-include("logs.hrl").

-record(state, {state_name, state_start_time, state_end_time}).

call(Request) ->
    ?fcall(?MODULE, Request).

cast(Request) ->
    gen_fsm:send_all_state_event(?MODULE, Request).

info(Info) ->
    ?MODULE ! Info.

apply(sync, MFA = {_M, _F, _A}) ->
    call({apply, MFA});
apply(sync, FA = {_F, _A}) ->
    call({apply, FA});
apply(async, MFA = {_M, _F, _A}) ->
    info({apply, MFA});
apply(async, FA = {_F, _A}) ->
    info({apply, FA}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info("[~w]开始启动", [?MODULE]),
    process_flag(trap_exit, true),
    Now = time_util:timestamp(ms),
    State = #state{state_name = idle, state_start_time = 0, state_end_time = Now + ?hour_ms},
    ?info("[~w]启动完成", [?MODULE]),
    {ok, idle, State, ?hour_ms}.

continue(StateName, State = #state{state_end_time = StateEndTime}) ->
    {next_state, StateName, State, max(0, StateEndTime - time_util:timestamp(ms))}.
continue(Reply, StateName, State = #state{state_end_time = StateEndTime}) ->
    {reply, Reply, StateName, State, max(0, StateEndTime - time_util:timestamp(ms))}.

handle_sync_event(Request, From, StateName, State) ->
    case catch do_handle_sync_event(Request, From, StateName, State) of
        {reply, Reply, NewStateName, NewState} ->
            continue(Reply, NewStateName, NewState);
        _Err ->
            ?error("handle_sync_event错误，消息:~w，StateName:~w，State:~w，Reason:~w", [Request, StateName, State, _Err]),
            continue({error, handle_error}, StateName, State)
    end.

handle_event(Request, StateName, State) ->
    case catch do_handle_event(Request, StateName, State) of
        {next_state, NewStateName, NewState} ->
            continue(NewStateName, NewState);
        _Err ->
            ?error("handle_event错误，消息:~w，StateName:~w，State:~w，Reason:~w", [Request, StateName, State, _Err]),
            continue(StateName, State)
    end.

handle_info(Info, StateName, State) ->
    case catch do_handle_info(Info, StateName, State) of
        {next_state, NewStateName, NewState} ->
            continue(NewStateName, NewState);
        _Err ->
            ?error("handle_info错误，消息:~w，StateName:~w，State:~w，Reason:~w", [Info, StateName, State, _Err]),
            continue(StateName, State)
    end.

terminate(Reason, StateName, _State) ->
    ?info("[~w]开始关闭，StateName：~w，原因：~w", [?MODULE, StateName, Reason]),
    ?info("[~w]关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

do_handle_sync_event({apply, {M, F, A}}, _From, StateName, State) ->
    case catch util:apply({M, F, [StateName, State | A]}) of
        {ok, Reply} ->
            {reply, Reply, StateName, State};
        {reply, Reply} ->
            {reply, Reply, StateName, State};
        {ok, Reply, NewStateName, NewState} ->
            {reply, Reply, NewStateName, NewState};
        {reply, Reply, NewStateName, NewState} ->
            {reply, Reply, NewStateName, NewState};
        _Err ->
            ?error("同步执行~w:~w:~w错误，StateName:~w，State:~w，Reason:~w", [M, F, A, StateName, State, _Err]),
            {reply, {error, apply_error}, StateName, State}
    end;
do_handle_sync_event({apply, {F, A}}, _From, StateName, State) ->
    case catch util:apply({F, [StateName, State | A]}) of
        {ok, Reply} ->
            {reply, Reply, StateName, State};
        {reply, Reply} ->
            {reply, Reply, StateName, State};
        {ok, Reply, NewStateName, NewState} ->
            {reply, Reply, NewStateName, NewState};
        {reply, Reply, NewStateName, NewState} ->
            {reply, Reply, NewStateName, NewState};
        _Err ->
            ?error("同步执行~w:~w错误，StateName:~w，State:~w，Reason:~w", [F, A, StateName, State, _Err]),
            {reply, {error, apply_error}, StateName, State}
    end;

do_handle_sync_event(_Request, _From, StateName, State) ->
    {reply, {error, bad_request}, StateName, State}.

do_handle_event(_Request, StateName, State) ->
    {next_state, StateName, State}.

do_handle_info({apply, {M, F, A}}, StateName, State) ->
    case catch util:apply({M, F, [StateName, State | A]}) of
        ok ->
            {next_state, StateName, State};
        {ok, NewStateName, NewState} ->
            {next_state, NewStateName, NewState};
        {next_state, NewStateName, NewState} ->
            {next_state, NewStateName, NewState};
        _Err ->
            ?error("异步执行~w:~w:~w错误，StateName:~w，State:~w，Reason:~w", [M, F, A, StateName, State, _Err]),
            {next_state, StateName, State}
    end;
do_handle_info({apply, {F, A}}, StateName, State) ->
    case catch util:apply({F, [StateName, State | A]}) of
        ok ->
            {next_state, StateName, State};
        {ok, NewStateName, NewState} ->
            {next_state, NewStateName, NewState};
        {next_state, NewStateName, NewState} ->
            {next_state, NewStateName, NewState};
        _Err ->
            ?error("异步执行~w:~w错误，StateName:~w，State:~w，Reason:~w", [F, A, StateName, State, _Err]),
            {next_state, StateName, State}
    end;

do_handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

idle(Info, State) ->
    handle_state(Info, idle, State).

handle_state(Info, StateName, State) ->
    case catch do_handle_state(Info, StateName, State) of
        {next_state, NewStateName, NewState} ->
            continue(NewStateName, NewState);
        _Err when Info =:= timeout ->
            ?error("handle_state错误，消息:~w，StateName:~w，State:~w，Reason:~w", [Info, StateName, State, _Err]),
            continue(StateName, State#state{state_end_time = time_util:timestamp(ms) + ?hour_ms});
        _Err ->
            ?error("handle_state错误，消息:~w，StateName:~w，State:~w，Reason:~w", [Info, StateName, State, _Err]),
            continue(StateName, State)
    end.

do_handle_state(_Info, StateName, State) ->
    {next_state, StateName, State}.
