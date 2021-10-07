%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 公共模块头文件
%%%
%%% @end
%%% Created : 19. 二月 2020 21:35
%%%-------------------------------------------------------------------
-author("huangzaoyi").

-ifndef(common_hrl).
-define(common_hrl, 1).

-define(game_name, <<"greate world">>).

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

-define(error_ver, erlang:error(bad_ver)). %% 版本号抛出异常，防止直接初始化结构错误

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

%% 判断是否true 或者 false，执行对应的方法
-define(if_true(_Is_, _True_, _False_), case _Is_ of true -> _True_; _ -> _False_ end).

%% 将list字符串转成二进制字符串
-define(str(Format), utf8_util:chars_to_utf8(Format)).
-define(str(Format, Args), utf8_util:chars_to_utf8(io_lib:format(Format, Args))).

-export_type([srv_id/0, language/0, void/0, msg/0, role_id/0]).

-type srv_id() :: binary().
-type language() :: chinese.
-type void() :: any().
-type msg() :: pos_integer() | {pos_integer(), list()} | binary() | {pos_integer(), list(), binary()}.
-type role_id() :: {pos_integer(), binary()}.

-endif.