%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关模块头文件
%%%
%%% @end
%%% Created : 16. 三月 2020 00:15
%%%-------------------------------------------------------------------
-author("huangzaoyi").

-define(max_packet_size, 3072). %% 一个协议包最大字节数
-define(max_recv_proto_msg_num, 20). %% 未处理接收协议消息最大数量
-define(max_send_proto_msg_num, 50). %% 未处理发送协议消息最大数量
-define(max_unpack_error_times, 50). %% 最大解包出错次数

-define(heartbeat_interval, 60). %% 检测心跳的时间间隔

%% 网关监听器
-record(gateway_listener, {
    lsock              %% 监听器socket
}).

%% 网关接收器
-record(gateway_acceptor, {
    lsock               %% 监听器socket
    ,ref                %% 引用
}).

%% 网关
-record(gateway, {
    sock                    %% socket
    ,is_login = false       %% 是否已经登录
    ,role_pid               %% 角色pid
    ,account = <<>>         %% 账号
    ,role_id = {0, <<>>}    %% 角色ID
}).

%% client端
-record(gateway_client, {
    sock                    %% socket
    ,is_login = false       %% 是否已经登录
    ,account = <<>>         %% 账号
    ,role_id = {0, <<>>}    %% 角色ID
}).