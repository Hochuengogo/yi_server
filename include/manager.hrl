%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifndef(manager_hrl).
-define(manager_hrl, 1).

%% @doc 管理器模块头文件
-record(service, {
    id          =        undefined %% 服务id
    ,type       =        worker    %% 服务类型 worker | supervisor
    ,depend_on  =        undefined %% 依赖哪个supervisor
    ,start      =        undefined %% 启动方法
    ,restart    =        permanent %% 重启策略 permanent | temporary | transient
    ,shutdown   =        300000    %% 关闭最长时间
    ,desc       =        ""        %% 描述
}).

-endif.