%%%%%%%%%%%%%%%%%%%%%%%%%%%% 配置 %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器配置
{server_id, {server_id}}. %% 服务器ID
{server_type, {server_type}}. %% 服务器类型 zone | center
{platform, <<"{platform}">>}. %% 平台
{center_node, '{platform}_center_0'}. %% 中央服节点

%% 数据库配置
{db_options,
    {
        [
            {size, 10}, %% 连接池大小
            {max_overflow, 20} %%连接池溢出最大值
        ],
        [
            {host, "{mysql_host}"},
            {port, {mysql_port}},
            {user, "{mysql_user}"},
            {password, "{mysql_password}"},
            {database, "{mysql_db}"}
        ]
    }
}.

%% 网关配置
{gateway_host, "{gateway_host}"}. %% 网关host
{gateway_port, {gateway_port}}. %% 网关端口
{gateway_options, [
    binary,
    {active, false},
    {reuseaddr, true},
    {backlog, 1024},
    {nodelay, true},
    {delay_send, true},
    {send_timeout, 30000},
    {send_timeout_close, true},
    {exit_on_close, false}
]}. %% 网关选项
{acceptor_num, 30}. %% 接收器数量

