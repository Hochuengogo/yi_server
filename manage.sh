#!/bin/bash

# 管理脚本

# 配置文件
CFG_FILE="./setting.config"

REBAR=rebar3

# 编译
function make() {
    ${REBAR} compile
}

# 启动
function start() {
    ${REBAR} shell
}

# 生成日志等级文件
function gen_log() {
    LOG_LINE=`grep ^\{log_level ${CFG_FILE}`
    python gen_log.py "${LOG_LINE}"
    if [ $? == 0 ]; then
        echo "生成日志等级文件成功"
    else
        echo "生成日志等级文件失败"
    fi
}

# 帮助文档
function help() {
    echo "make    : 编译"
    echo "start   : 启动"
    echo "gen_log : 生成日志等级文件"
}

case $1 in
    make)
      make
      ;;
    start)
      start
      ;;
    gen_log)
      gen_log
      ;;
    *)
      help 
esac
