%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 角色头文件
%%%
%%% @end
%%% Created : 16. 三月 2020 00:15
%%%-------------------------------------------------------------------
-author("huangzaoyi").

-ifndef(role_hrl).
-define(role_hrl, 1).

-define(register_max_role_num, 1). %% 账号在同一个服可以注册的角色数量
-define(max_role_num, 5000). %% 一个服最多有多少个角色

-define(name_len, 20). %% 名字长度

%% 职业
-define(role_career_def, 0). %% 初始化职业
-define(role_career_list, [?role_career_def]).

%% 性别
-define(role_sex_def, 0). %% 初始化性别
-define(role_sex_male, 1). %% 男性
-define(role_sex_female, 2). %% 女性
-define(role_sex_list, [?role_sex_def, ?role_sex_male, ?role_sex_female]).

%% 角色类型
-define(role_type_normal, 0). %% 普通玩家
-define(role_type_gm, 1). %% GM
-define(role_type_guilder, 2). %% 新手指导员
-define(role_type_robot, 3). %% 机器人

-define(role_ver, 1). %% 角色版本号

%% 角色结构
-record(role, {
    ver = ?error_ver             %% 版本号
    , id = {0, <<>>}             %% 角色ID
    , pid                        %% 角色Pid
    , account = <<>>             %% 账号
    , name = <<>>                %% 角色名
    , career = 0                 %% 职业
    , type = 0                   %% 类型 0:普通玩家 1:GM 2:新手指导员
    , sex = 0                    %% 性别
    , face_id = 0                %% 头像ID
    , lev = 1                    %% 等级
    , exp = 0                    %% 经验
    , vip_lev = 0                %% VIP等级
    , vip_exp = 0                %% VIP经验
    , power = 0                  %% 战力
    , max_power = 0              %% 最大战力
    , login_time = 0             %% 登录时间
    , logout_time = 0            %% 登出时间
    , login_ip = <<>>            %% 登录IP
    , is_online = 0              %% 是否在线
    , banned_time = 0            %% 封号时间
    , data_lock_time = 0         %% 数据锁定时间
    , identity = <<>>            %% 身份证号
    , reg_channel = <<>>         %% 注册渠道
    , channel = <<>>             %% 渠道
    , reg_time = 0               %% 注册时间
    , reg_ip = <<>>              %% 注册IP
    , reg_idfa = <<>>            %% 注册设备号
    , idfa = <<>>                %% 设备号
    , reg_device_id = <<>>       %% 注册设备ID
    , device_id = <<>>           %% 设备ID
    , reg_os_name = <<>>         %% 注册设备系统
    , os_name = <<>>             %% 设备系统
    , reg_package_name = <<>>    %% 注册包
    , package_name = <<>>        %% 包
    , reg_package_version = <<>> %% 注册包版本
    , package_version = <<>>     %% 包版本
    , platform = <<>>            %% 平台
    , is_sync = 0                %% 是否要保存数据到磁盘
    , m_gateway                  %% 网关信息
    , s_trigger                  %% 触发器
    , s_timer                    %% 定时器
}).

%% 角色基础数据
-record(role_base, {
    rid = 0                      %% 角色ID
    , srv_id = <<>>              %% 服务器ID
    , account = <<>>             %% 账号
    , name = <<>>                %% 角色名
    , career = 0                 %% 职业
    , type = 0                   %% 类型 0:普通玩家 1:GM 2:新手指导员
    , sex = 0                    %% 性别
    , face_id = 0                %% 头像ID
    , lev = 1                    %% 等级
    , vip_lev = 0                %% VIP等级
    , power = 0                  %% 战力
    , max_power = 0              %% 最大战力
    , login_time = 0             %% 登录时间
    , logout_time = 0            %% 登出时间
    , login_ip = <<>>            %% 登录IP
    , is_online = 0              %% 是否在线
    , banned_time = 0            %% 封号时间
    , data_lock_time = 0         %% 数据锁定时间
    , identity = <<>>            %% 身份证号
    , reg_channel = <<>>         %% 注册渠道
    , channel = <<>>             %% 渠道
    , reg_time = 0               %% 注册时间
    , reg_ip = <<>>              %% 注册IP
    , reg_idfa = <<>>            %% 注册设备号
    , idfa = <<>>                %% 设备号
    , reg_device_id = <<>>       %% 注册设备ID
    , device_id = <<>>           %% 设备ID
    , reg_os_name = <<>>         %% 注册设备系统
    , os_name = <<>>             %% 设备系统
    , reg_package_name = <<>>    %% 注册包
    , package_name = <<>>        %% 包
    , reg_package_version = <<>> %% 注册包版本
    , package_version = <<>>     %% 包版本
    , platform = <<>>            %% 平台
}).

%% 角色查询数据
-record(role_query, {
    key                          %% 主键
    , gate_pid                   %% 网关Pid
    , role_pid                   %% 角色Pid
}).

-endif.