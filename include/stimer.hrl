%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 4月 2021 5:24 下午
%%%-------------------------------------------------------------------
-author("huangzaoyi").

-ifndef(stimer_hrl).
-define(stimer_hrl, 1).

%% 定时器结构
-record(s_timer, {
    timers = []              %% 定时器列表 [#timer{}]
    , ref                    %% 定时器引用
}).

%% 定时器
-record(timer, {
    id                       %% 定时器ID
    , time = 1               %% 执行次数 -1 无限次 1 一次 n 多次
    , timeout = 100          %% 定时毫秒数
    , callback               %% 回调方法 {m,f,a}
    , timestamp              %% 定时时间戳 内部确定
}).

-endif.