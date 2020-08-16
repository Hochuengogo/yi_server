%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 公共模块头文件
%%%
%%% @end
%%% Created : 19. 二月 2020 21:35
%%%-------------------------------------------------------------------
-author("huangzaoyi").

-define(true, 1).
-define(false, 0).

-define(scall(Server, Call),
    case catch gen_server:call(Server, Call, 10000) of
        {'EXIT', _Err} ->
            {error, _Err};
        Res ->
            Res
    end
).

-define(fcall(Server, Call),
    case catch gen_fsm:sync_send_all_state_event(Server, Call, 10000) of
        {'EXIT', _Err} ->
            {error, _Err};
        Res ->
            Res
    end
).

%% 启动、关闭 输出
-define(start_begin, io_lib:format("开始启动:~w", [?MODULE])).
-define(start_end, io_lib:format("启动完成:~w", [?MODULE])).
-define(stop_begin, io_lib:format("开始关闭:~w, 原因:~w", [?MODULE, _Reason])).
-define(stop_end, io_lib:format("关闭完成:~w", [?MODULE])).


