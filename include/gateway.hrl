%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关模块头文件
%%%
%%% @end
%%% Created : 16. 三月 2020 00:15
%%%-------------------------------------------------------------------
-author("huangzaoyi").

-ifndef(gateway_hrl).
-define(gateway_hrl, 1).

%% head:32|compress_flag:8|code:16|data
%% head_data : 1+2+byte_size(data)

-define(gateway_packet_head_byte, 4). %% 网关数据包字节
-define(gateway_packet_head_bits, 32). %% 网关数据包位数
-define(gateway_packet_compress_bits, 8). %% 网关数据包压缩标识位数
-define(gateway_packet_code_bits, 16). %% 网关数据包协议号位数
-define(gateway_packet_data_max_byte, 10240). %% 网关数据包数据部分最大字节
-define(gateway_packet_compress_min_byte, 4096). %% 网关数据包需要打包的最小字节

-define(gateway_heartbeat_interval, 60). %% 检测心跳的时间间隔

%% 网关监听器
-record(gateway_listener, {
    lsock               %% 监听器socket
}).

%% 网关接收器
-record(gateway_acceptor, {
    lsock                %% 监听器socket
    , ref                %% 引用
    , name               %% 名字
}).

%% 网关
-record(gateway, {
    pid                      %% 网关pid
    , sock                   %% socket
    , ip                     %% IP
    , port                   %% 端口
    , is_login = false       %% 是否已经登录
    , role_pid               %% 角色pid
    , account = <<>>         %% 账号
    , role_id = {0, <<>>}    %% 角色ID
    , ref                    %% 引用
    , read_head = true       %% 读数据头部
    , data_size = 0          %% 数据body字节大小
}).

%% 角色上的网关结构
-record(m_gateway, {
    pid                      %% 网关进程
    , sock                   %% socket
    , ip                     %% IP
    , port                   %% 端口
}).

%% client端
-record(gateway_client, {account, sock, ref, read_head, data_size, is_login, action_type}).

-endif.