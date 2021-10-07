%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 定时器测试
%%% @end
%%% Created : 2021-10-07 21:18:09
%%%-------------------------------------------------------------------
-module(t_timer).
-author("jiaoyinyi").

-behaviour(gen_server).

%% API
-export([call/1, cast/1, info/1, apply/2, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    fire/2
]).

-include("common.hrl").
-include("logs.hrl").
-include("stimer.hrl").

-record(state, {s_timer}).

call(Request) ->
    ?scall(?MODULE, Request).

cast(Request) ->
    gen_server:cast(?MODULE, Request).

info(Info) ->
    ?MODULE ! Info.

apply(sync, MFA = {_M, _F, _A}) ->
    call({apply, MFA});
apply(sync, {F, A}) ->
    call({apply, {undefined, F, A}});
apply(async, MFA = {_M, _F, _A}) ->
    info({apply, MFA});
apply(async, {F, A}) ->
    info({apply, {undefined, F, A}}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info("[~w]开始启动", [?MODULE]),
    process_flag(trap_exit, true),
    ?info("[~w]启动完成", [?MODULE]),
    {ok, #state{s_timer = #s_timer{}}}.

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
    case catch util:apply(M, F, [State | A]) of
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

do_handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

do_handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_info({apply, {M, F, A}}, State) ->
    case catch util:apply(M, F, [State | A]) of
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

do_handle_info(test_add, State = #state{s_timer = STimer}) ->
    NewSTimer = test_add(STimer),
    {noreply, State#state{s_timer = NewSTimer}};

do_handle_info(test_add_more, State = #state{s_timer = STimer}) ->
    NewSTimer = test_add_more(STimer),
    {noreply, State#state{s_timer = NewSTimer}};

do_handle_info(test_del, State = #state{s_timer = STimer}) ->
    NewSTimer = test_del(STimer),
    {noreply, State#state{s_timer = NewSTimer}};

do_handle_info(test_del_more, State = #state{s_timer = STimer}) ->
    NewSTimer = test_del_more(STimer),
    {noreply, State#state{s_timer = NewSTimer}};

do_handle_info(test_set, State = #state{s_timer = STimer}) ->
    NewSTimer = test_set(STimer),
    {noreply, State#state{s_timer = NewSTimer}};

do_handle_info(test_set_more, State = #state{s_timer = STimer}) ->
    NewSTimer = test_set_more(STimer),
    {noreply, State#state{s_timer = NewSTimer}};

do_handle_info({timeout, Ref, timer_tick}, State= #state{s_timer = STimer}) ->
    case stimer:tick(STimer, Ref, State) of
        {NewSTimer = #s_timer{}, NewState = #state{}} ->
            {noreply, NewState#state{s_timer = NewSTimer}};
        _ ->
            {noreply, State}
    end;

do_handle_info(_Info, State) ->
    {noreply, State}.

test_add(STimer) ->
    ?debug("测试增加定时器"),
    Timer = #timer{id = test_add, time = -1, timeout = 5000, callback = {?MODULE, fire, [test_add]}},
    stimer:add(STimer, Timer).

test_add_more(STimer) ->
    ?debug("测试增加多个定时器"),
    Timer1 = #timer{id = {test_add, 1}, time = -1, timeout = 5000, callback = {?MODULE, fire, [{test_add, 1}]}},
    Timer2 = #timer{id = {test_add, 2}, time = 1, timeout = 5000, callback = {?MODULE, fire, [{test_add, 2}]}},
    Timer3 = #timer{id = {test_add, 3}, time = 2, timeout = 5000, callback = {?MODULE, fire, [{test_add, 3}]}},
    Timer4 = #timer{id = {test_add, 4}, time = 3, timeout = 5000, callback = {?MODULE, fire, [{test_add, 4}]}},
    stimer:add(STimer, [Timer1, Timer2, Timer3, Timer4]).

test_del(STimer) ->
    ?debug("测试删除定时器"),
    stimer:del(STimer, test_add).

test_del_more(STimer) ->
    ?debug("测试删除多个定时器"),
    stimer:del(STimer, [{test_add, 1}, {test_add, 2}, {test_add, 3}, {test_add, 4}]).

test_set(STimer) ->
    ?debug("测试设置定时器"),
    Timer = #timer{id = test_add, time = 10, timeout = 5000, callback = {?MODULE, fire, [test_add]}},
    stimer:set(STimer, Timer).

test_set_more(STimer) ->
    ?debug("测试设置多个定时器"),
    Timer1 = #timer{id = {test_add, 1}, time = 10, timeout = 5000, callback = {?MODULE, fire, [{test_add, 1}]}},
    Timer2 = #timer{id = {test_add, 2}, time = 20, timeout = 5000, callback = {?MODULE, fire, [{test_add, 2}]}},
    Timer3 = #timer{id = {test_add, 3}, time = 30, timeout = 5000, callback = {?MODULE, fire, [{test_add, 3}]}},
    Timer4 = #timer{id = {test_add, 4}, time = 40, timeout = 5000, callback = {?MODULE, fire, [{test_add, 4}]}},
    stimer:set(STimer, [Timer1, Timer2, Timer3, Timer4]).

fire(State, Id) ->
    ?debug("定时器触发，id：~w", [Id]),
    {ok, State}.