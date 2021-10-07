%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 跨服头文件
%%% @end
%%% Created : 2021-10-07 22:40:50
%%%-------------------------------------------------------------------
-author("jiaoyinyi").

-ifndef(cluster_hrl).
-define(cluster_hrl, 1).

%% 服务器信息
-record(srv_info, {
    node             %% 节点
    , srv_list       %% 服务器列表
    , version        %% 服务器版本
}).

-endif.
