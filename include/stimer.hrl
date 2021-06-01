%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 4月 2021 5:24 下午
%%%-------------------------------------------------------------------
-author("huangzaoyi").

%% 定时器结构
-record(s_timer, {
    sort_timers = []        %% 排好序的定时器列表 [#timer{}]
    ,unsort_timers = []     %% 未排序的定时器列表 [#timer{}]
    ,ref                    %% 定时器引用
    ,ref_id = 0             %% 定时器引用ID
    ,last_sort_timestamp = 0%% 排好序的最后一个定时器定时时间戳
}).

%% 定时器
-record(timer, {
    id                      %% 定时器ID
    ,time                   %% 执行次数 -1 无限次 1 一次 n 多次
    ,timeout                %% 定时毫秒数
    ,callback               %% 回调方法 {m,f,a}
    ,timestamp              %% 定时时间戳
}).
