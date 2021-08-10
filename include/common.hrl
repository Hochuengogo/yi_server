%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 公共模块头文件
%%%
%%% @end
%%% Created : 19. 二月 2020 21:35
%%%-------------------------------------------------------------------
-author("huangzaoyi").

%% 整数型bool
-define(true, 1).
-define(false, 0).

%% 时间宏
-define(week_s, 604800).
-define(day_s, 86400).
-define(hour_s, 3600).
-define(min_s, 60).
-define(week_s(Week), Week * ?week_s).
-define(day_s(Day), Day * ?day_s).
-define(hour_s(Hour), Hour * ?hour_s).
-define(min_s(Min), Min * ?min_s).
-define(week_ms, 604800000).
-define(day_ms, 86400000).
-define(hour_ms, 3600000).
-define(min_ms, 60000).
-define(sec_ms, 1000).
-define(week_ms(Week), Week * ?week_ms).
-define(day_ms(Day), Day * ?day_ms).
-define(hour_ms(Hour), Hour * ?hour_ms).
-define(min_ms(Min), Min * ?min_ms).
-define(sec_ms(Sec), Sec * ?sec_ms).
-define(secs_from_0_to_1970, 62167219200).

%% call封装
-define(scall(Server, Call),
    case catch gen_server:call(Server, Call, 10000) of
        {'EXIT', {timeout, _Err}} ->
            {error, timeout};
        {'EXIT', {{nodedown, Node}, _Err}} ->
            {error, {nodedown, Node}};
        {'EXIT', {noproc, _Err}} ->
            {error, noproc};
        {'EXIT', _Err} ->
            {error, _Err};
        Res ->
            Res
    end
).
-define(fcall(Server, Call),
    case catch gen_fsm:sync_send_all_state_event(Server, Call, 10000) of
        {'EXIT', {timeout, _Err}} ->
            {error, timeout};
        {'EXIT', {{nodedown, Node}, _Err}} ->
            {error, {nodedown, Node}};
        {'EXIT', {noproc, _Err}} ->
            {error, noproc};
        {'EXIT', _Err} ->
            {error, _Err};
        Res ->
            Res
    end
).

%% 启动、关闭 输出
-define(start_begin, io_lib:format("开始启动:~w", [?MODULE])).
-define(start_begin(Name), io_lib:format("开始启动:~w", [Name])).
-define(start_end, io_lib:format("启动完成:~w", [?MODULE])).
-define(start_end(Name), io_lib:format("启动完成:~w", [Name])).
-define(stop_begin, io_lib:format("开始关闭:~w, 原因:~w", [?MODULE, _Reason])).
-define(stop_begin(Name), io_lib:format("开始关闭:~w, 原因:~w", [Name, _Reason])).
-define(stop_end, io_lib:format("关闭完成:~w", [?MODULE])).
-define(stop_end(Name), io_lib:format("关闭完成:~w", [Name])).

-export_type([srv_id/0, language/0]).

-type srv_id() :: binary().
-type language() :: chinese.