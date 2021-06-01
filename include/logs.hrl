%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 日志模块头文件
%%%
%%% @end
%%% Created : 08. 三月 2020 22:44
%%%-------------------------------------------------------------------
-author("huangzaoyi").

%% 运行时日志宏
-define(debug(Format, Args), begin case logs_lib:can_log(debug) of true -> logs:log(debug, ?MODULE, ?FUNCTION_NAME, ?LINE, io_lib:format(Format, Args)); _ -> ok end end).
-define(debug(Str), begin case logs_lib:can_log(debug) of true -> logs:log(debug, ?MODULE, ?FUNCTION_NAME, ?LINE, io_lib:format(Str, [])); _ -> ok end end).

-define(info(Format, Args), begin case logs_lib:can_log(info) of true -> logs:log(info, ?MODULE, ?FUNCTION_NAME, ?LINE, io_lib:format(Format, Args)); _ -> ok end end).
-define(info(Str), begin case logs_lib:can_log(info) of true -> logs:log(info, ?MODULE, ?FUNCTION_NAME, ?LINE, io_lib:format(Str, [])); _ -> ok end end).

-define(warn(Format, Args), begin case logs_lib:can_log(warn) of true -> logs:log(warn, ?MODULE, ?FUNCTION_NAME, ?LINE, io_lib:format(Format, Args)); _ -> ok end end).
-define(warn(Str), begin case logs_lib:can_log(warn) of true -> logs:log(warn, ?MODULE, ?FUNCTION_NAME, ?LINE, io_lib:format(Str, [])); _ -> ok end end).

-define(error(Format, Args), begin case logs_lib:can_log(error) of true -> logs:log(error, ?MODULE, ?FUNCTION_NAME, ?LINE, io_lib:format(Format, Args)); _ -> ok end end).
-define(error(Str), begin case logs_lib:can_log(error) of true -> logs:log(error, ?MODULE, ?FUNCTION_NAME, ?LINE, io_lib:format(Str, [])); _ -> ok end end).

-define(print(Format), io:format(Format)).
-define(print(Format, Args), io:format(Format, Args)).
-define(print(Fd, Format, Args), io:format(Fd, Format, Args)).
-define(print_line(Format), begin io:format(Format), io:format("\n") end).

%% 网关调试日志宏
-ifdef(gate_debug).
-define(gate_debug(Format, Args), ?debug(Format, Args)).
-define(gate_debug(Str), ?debug(Str)).
-else.
-define(gate_debug(Format, Args), ok).
-define(gate_debug(Str), ok).
-endif.