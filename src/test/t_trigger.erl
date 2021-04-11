%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(t_trigger).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([
    evt_test_1_callback/3
    , evt_test_1_callback/4
    , evt_test_2_callback/3
    , evt_test_3_callback/3
]).

-define(SERVER, ?MODULE).

-record(t_trigger_state, {s_trigger, data}).

%% 测试事件
-record(evt_test_1, {data}).
-record(evt_test_2, {data}).
-record(evt_test_3, {data}).

-include("common.hrl").
-include("logs.hrl").
-include("trigger.hrl").

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    T1 = #trigger{event = evt_test_1, callback = {?MODULE, evt_test_1_callback, []}},
    T2 = #trigger{event = evt_test_1, callback = {?MODULE, evt_test_1_callback, [test]}},
    T3 = #trigger{event = evt_test_2, callback = {?MODULE, evt_test_2_callback, []}},
    T4 = #trigger{event = evt_test_3, callback = {?MODULE, evt_test_3_callback, []}},
    {STrigger, _} = trigger:registers(#s_trigger{}, [T1, T2, T3, T4]),
    {ok, #t_trigger_state{s_trigger = STrigger}}.

handle_call(_Request, _From, State = #t_trigger_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #t_trigger_state{}) ->
    {noreply, State}.

handle_info({fire_trigger, EventTuple}, State = #t_trigger_state{s_trigger = STrigger}) ->
    ?debug("test1"),
    erlang:statistics(wall_clock),
    case catch trigger:fire(STrigger, State, EventTuple) of
        {NewSTrigger = #s_trigger{}, NewState} ->
            {_, Time} = erlang:statistics(wall_clock),
            ?debug("执行触发事件完成，~wms", [Time]),
            {noreply, NewState#t_trigger_state{s_trigger = NewSTrigger}};
        _Err ->
            {noreply, State}
    end;

handle_info({}, State = #t_trigger_state{}) ->
    {noreply, State};

handle_info(_Info, State = #t_trigger_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #t_trigger_state{}) ->
    ok.

code_change(_OldVsn, State = #t_trigger_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
evt_test_1_callback(STrigger, State, EventTuple) ->
    {NewSTrigger, NewState} = trigger:fire(STrigger, State, #evt_test_3{}),
    {remove, {evt_test_2, 3}, NewSTrigger, NewState}.

evt_test_1_callback(STrigger, State, EventTuple, Flag) ->
    ok.

evt_test_2_callback(STrigger, State, EventTuple) ->
    {remove, {evt_test_3, 4}}.

evt_test_3_callback(STrigger, State, EventTuple) ->
%%    {NewSTrigger, NewState} = trigger:fire(STrigger, State, #evt_test_1{}),
    {ok, STrigger}.