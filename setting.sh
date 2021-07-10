#!/usr/bin/env bash

# 作者
AUTHOR="jiaoyinyi"
# 游戏简称
GAME_NAME="demo"
# 游戏语言
GAME_LANG="chinese"
# 节点cookie
COOKIE="RWou%J7SfHSnplS@"
# 服务器版本
VERSION="1.0.0"

# 服务器根路径
ZONE_PATH="/Users/huangzaoyi/Documents/ProFiles/game_server/zone"
# 平台号
PLATFORM="local"
# 网关基础端口
BASE_PORT=10000

# mysql配置
# mysql host
MYSQL_HOST="localhost"
# mysql端口
MYSQL_PORT=3306
# mysql用户名
MYSQL_USER="root"
# mysql密码
MYSQL_PASSWORD="root"

# 网关host
GATEWAY_HOST="127.0.0.1"

#编译参数
MAKE_ARGS='[debug_info, {i, "include"}, {outdir, "ebin"}]'

# 日志等级
LOG_LEVEL="debug" # debug | info | warn | error

