%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关模块头文件
%%%
%%% @end
%%% Created : 16. 三月 2020 00:15
%%%-------------------------------------------------------------------
-author("huangzaoyi").

-ifndef(ver_hrl).
-define(ver_hrl, 1).

%% 版本转换结构
-record(ver_parser, {
    data                  %% 需要做版本转换的数据
    , ver                 %% 版本号
    , opts = []           %% 执行列表 [{add,[new_filed_data]},{set,[{pos,new_filed_data}]}]
    , mod                 %% 版本转换模块
}).

-endif.