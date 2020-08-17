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
-define(max_proto_msg_num, 20). %% 未处理协议消息最大数量

-define(max_error_times, 50). %% 最大解包出错次数
-define(max_con_error_times, 10). %% 最大连续解包出错次数
-define(max_data_count, 50). %% 最大剩余数据数量

-record(gateway_listener, {
    lsock              %% 监听器socket
}).

-record(gateway_acceptor, {
    lsock               %% 监听器socket
    ,ref                %% 引用
}).

-record(gateway, {
    sock                    %% socket
    ,is_login = false       %% 是否已经登录
    ,role_pid               %% 角色pid
}).

-type gateway_ret() :: ok | {ok, #gateway{}} | {reply, term()} | {reply, term(), #gateway{}}.