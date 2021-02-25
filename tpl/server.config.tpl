%%%%%%%%%%%%%%%%%%%%%%%%%%%% 配置 %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器配置
{server_id, <<"{platform}_{server_id}">>}. %% 服务器ID
{server_ids, [<<"{platform}_{server_id}">>]}. %% 服务器ID列表
{server_type, {server_type}}. %% 服务器类型 zone | center
{platform, <<"{platform}">>}. %% 平台
{language, <<"{language}">>}. %% 语言
{version, <<"{version}">>}. %% 版本
{code_path, "{code_path}"}. %% 代码路径

{cookie, <<"{cookie}">>}. %% cookie
{node, '{game_name}_{platform}_{server_type}_{server_id}@{gateway_host}'}. %% 本服节点
{center_node, '{game_name}_{platform}_center_0@{gateway_host}'}. %% 中央服节点

{open_srv_timestamp, {open_srv_timestamp}}. %% 开服时间

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
    {nodelay, false},
    {delay_send, true},
    {send_timeout, 30000},
    {send_timeout_close, true},
    {exit_on_close, false}
]}. %% 网关选项
{acceptor_num, 30}. %% 接收器数量

