#!/bin/bash

# 配置文件
CFG_FILE="./setting.config"

LOG_LINE=`grep ^\{log_level ${CFG_FILE}`
python gen_log.py "${LOG_LINE}"
if [ $? == 0 ]; then
    echo "生成日志等级文件成功"
else
    echo "生成日志等级文件失败"
fi
