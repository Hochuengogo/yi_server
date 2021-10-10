yi_server
=====

网络游戏服务器基础框架  
支持跨服功能

功能
-----
manage.sh 执行脚本
```
get_dep // 获取依赖
clean_dep // 清除依赖
make_dep // 编译依赖
make // 编译项目所有erl文件
make_mod // 编译项目指定模块erl文件
make_file // 编译项目指定erl文件
clean // 清除beam文件
install srv_type srv_id // 安装服务器节点
uninstall srv_type srv_id // 卸载服务器节点
start srv_type srv_id // 启动服务器节点
stop srv_type srv_id // 关闭服务器节点
update srv_type srv_id // 热更服务器节点模块
shell srv_type srv_id // 进入服务器节点shell
remsh srv_type srv_id // remsh方式连接服务器节点
exec srv_type srv_id mod func args // 在服务器节点中执行方法
gen_mod // 生成代码模板
```
配置
-----
setting.sh 项目配置文件
server.sql 项目数据库定义文件
server.config 服务器节点配置文件
