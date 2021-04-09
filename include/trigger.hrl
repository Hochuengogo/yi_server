%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 触发器
%%%
%%% @end
%%% Created : 09. 4月 2021 10:07 下午
%%%-------------------------------------------------------------------
-author("huangzaoyi").

%% 触发器结构
-record(s_trigger, {
    triggers = []      %% 触发器列表 [{event, [#trigger{}]}]
    ,next_id = 1       %% 下一个触发器ID
}).

%% 触发器
-record(trigger, {
    id                 %% 触发器ID
    ,event             %% 事件
    ,callback          %% 回调方法 {m,f,a}
}).