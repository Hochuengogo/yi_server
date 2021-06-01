#!/usr/bin/env bash

# 获取根路径
TARGET_FILE=`dirname $0`
TARGET_FILE=`basename ${TARGET_FILE}`
while [ -L "${TARGET_FILE}" ]
do
    TARGET_FILE=`readlink ${TARGET_FILE}`
    cd `dirname ${TARGET_FILE}`
    TARGET_FILE=`basename ${TARGET_FILE}`
done
PHYS_DIR=`pwd -P`
ROOT=${PHYS_DIR}

REBAR=rebar3
ERL=erl
PRINT=printf

# 引用配置脚本
source ${ROOT}/setting.sh

# 定义字典
declare -A cfg
cfg[root]=${ROOT}
cfg[game_name]=${GAME_NAME}
cfg[game_lang]=${GAME_LANG}
cfg[cookie]=${COOKIE}
cfg[version]=${VERSION}
cfg[zone_path]=${ZONE_PATH}
cfg[platform]=${PLATFORM}
cfg[base_port]=${BASE_PORT}
cfg[mysql_host]=${MYSQL_HOST}
cfg[mysql_port]=${MYSQL_PORT}
cfg[mysql_user]=${MYSQL_USER}
cfg[mysql_password]=${MYSQL_PASSWORD}
cfg[gateway_host]=${GATEWAY_HOST}
cfg[make_args]=${MAKE_ARGS}
cfg[log_level]=${LOG_LEVEL}

declare -A msg

# 获取依赖库
msg[get_dep]="获取依赖库"
function get_dep() {
    ${PRINT} "开始获取依赖库\n"
    cd ${cfg[root]}/lib && $REBAR upgrade
    ${PRINT} "获取依赖库完成\n"
}

# 删除依赖库
msg[clean_dep]="删除依赖库"
function clean_dep() {
    ${PRINT} "开始删除依赖库\n"
    if [[ -e ${cfg[root]}/lib/_build ]]; then
        rm -rf ${cfg[root]}/lib/_build
    fi
    if [[ -e ${cfg[root]}/tbin ]]; then
        rm -rf ${cfg[root]}/tbin
    fi
    ${PRINT} "删除依赖库完成\n"
}

# 编译依赖，并将beam复制到tbin目录
msg[make_dep]="编译依赖，并将beam复制到tbin目录"
function make_dep() {
    ${PRINT} "开始编译依赖库\n"
    cd ${cfg[root]}/lib && ${REBAR} compile
    cp_dep
    ${PRINT} "编译依赖库完成\n"
}

# 复制依赖库的beam、app文件
msg[cp_dep]="复制依赖库的beam、app文件"
function cp_dep() {
    ${PRINT} "开始复制依赖库\n"
    lib_path=${cfg[root]}/lib/_build/default/lib
    if [[ ! -e ${cfg[root]}/tbin ]]; then
        mkdir ${cfg[root]}/tbin
    fi
    for lib in `find ${lib_path} -type d -maxdepth 1` ; do
        if [[ ${lib} != ${lib_path} ]]; then
            cp -a ${lib}/ebin/. ${cfg[root]}/tbin
        fi
    done
    ${PRINT} "复制依赖库完成\n"
}

# 安装服务器
msg[install]="安装服务器"
function install() {
  ${PRINT} "请输入服务器类型(zone|center):"
  read server_type
  if [[ ! (${server_type} == zone || ${server_type} == center) ]]; then
      ${PRINT} "服务器类型只能为zone或center\n"
      exit 1
  fi

  ${PRINT} "请输入服务器ID(0~100):"
  read server_id
  expr ${server_id} + 0 1>/dev/null 2>&1
  if [[ $? -eq 0 && ! (${server_id} -ge 0 && ${server_id} -le 100) ]]; then
    ${PRINT} "服务器ID不在0~100范围内\n"
    exit 1
  fi

  # 判断服务器根路径是否存在
  if [[ ! -e ${cfg[zone_path]} ]]; then
      ${PRINT} "创建服务器根目录\n"
      mkdir ${cfg[zone_path]}
  fi

  mysql_db=${cfg[game_name]}_${cfg[platform]}_${server_type}_${server_id}
  mysql_cmd="mysql -h${cfg[mysql_host]} -u${cfg[mysql_user]} -p${cfg[mysql_password]} -P ${cfg[mysql_port]}"
  ${mysql_cmd} -e "use ${mysql_db};"
  mysql_exist=$?

  server_name=${cfg[game_name]}_${cfg[platform]}_${server_type}_${server_id}
  server_path=${cfg[zone_path]}/${server_name}

  if [ ${mysql_exist} = 0 ]; then
      ${PRINT} "数据库%s已存在\n" ${mysql_db}
      exit 1
  fi
  if [[ -e ${server_path} ]]; then
      ${PRINT} "已存在%s路径\n" ${server_path}
      exit 1
  fi

  ${mysql_cmd} -e "create database ${mysql_db};"
  if [ $? = 0 ]; then
      ${PRINT} "数据库%s创建成功\n" ${mysql_db}
  else
      ${PRINT} "数据库%s创建失败\n" ${mysql_db}
      exit 1
  fi
  ${mysql_cmd} -D${mysql_db}<${cfg[root]}/server.sql
  if [ $? = 0 ]; then
      ${PRINT} "数据库%s创建表成功\n" ${mysql_db}
  else
      ${PRINT} "数据库%s创建表失败\n" ${mysql_db}
      exit 1
  fi

  # 创建服务器数据文件夹
  mkdir ${server_path}
  mkdir ${server_path}/dets
  mkdir ${server_path}/log
  gateway_port=$(expr ${cfg[base_port]} + ${server_id})
  open_srv_timestamp=`date '+%s'`
  sed_str="s/{server_id}/${server_id}/g;s/{server_type}/${server_type}/g;s/{platform}/${cfg[platform]}/g;s/{mysql_host}/${cfg[mysql_host]}/g;s/{mysql_port}/${cfg[mysql_port]}/g;s/{mysql_user}/${cfg[mysql_user]}/g;s/{mysql_password}/${cfg[mysql_password]}/g;s/{mysql_db}/${mysql_db}/g;s/{gateway_host}/${cfg[gateway_host]}/g;s/{gateway_port}/${gateway_port}/g;s/{game_name}/${cfg[game_name]}/g;s/{open_srv_timestamp}/${open_srv_timestamp}/g;s/{language}/${cfg[game_lang]}/g;s/{version}/${cfg[version]}/g;s#{code_path}#${cfg[root]}#g;s/{cookie}/${cfg[cookie]}/g"
#  echo ${sed_str}
  cat ${cfg[root]}/tpl/server.config.tpl | sed "${sed_str}" > ${server_path}/server.config

  ${PRINT} "安装服务器%s完成\n" ${server_name}
}

# 卸载服务器
msg[uninstall]="卸载服务器 (uninstall server_type server_id)"
function uninstall() {
    server_type=$1
    server_id=$2
    mysql_db=${cfg[game_name]}_${cfg[platform]}_${server_type}_${server_id}
    mysql_cmd="mysql -h${cfg[mysql_host]} -u${cfg[mysql_user]} -p${cfg[mysql_password]} -P ${cfg[mysql_port]}"
    ${mysql_cmd} -e "use ${mysql_db};"
    mysql_exist=$?

    server_name=${cfg[game_name]}_${cfg[platform]}_${server_type}_${server_id}
    server_path=${cfg[zone_path]}/${server_name}

    if [[ ${mysql_exist} != 0 && ! -e ${server_path} ]]; then
        ${PRINT} "服务器%s未安装\n"
        exit 0
    fi
    ${PRINT} "是否确定卸载服务器%s (yes|no):" ${server_name}
    read check
    if [[ ${check} = yes ]]; then
        ${mysql_cmd} -e "drop database ${mysql_db};"
        if [ $? = 0 ]; then
            ${PRINT} "删除数据库%s完成\n" ${mysql_db}
        else
            ${PRINT} "删除数据库%s失败\n" ${mysql_db}
            exit 1
        fi
        rm -rf ${server_path}
        if [ $? = 0 ]; then
            ${PRINT} "删除服务器路径%s成功\n" ${server_path}
        else
            ${PRINT} "删除服务器路径%s失败\n" ${server_path}
            exit 1
        fi
        ${PRINT} "卸载服务器%s完成\n" ${server_name}
    fi
}

# 编译
msg[make]="编译"
function make() {
    ${PRINT} "开始编译\n"
    cd ${cfg[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    cp -a ${cfg[root]}/tbin/. ${cfg[root]}/ebin
    cp -a ${cfg[root]}/yi_server.app ${cfg[root]}/ebin
    gen_log_level
    mods=`find src -type d | sed "s/^/'/g;s/$/\/\*'\,/g;$ s/.$//g" | tr '\n' ' '`
    opts=${cfg[make_args]}
    emakefile="[{[${mods}], ${opts}}]"
    ${ERL} -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [ $? = 0 ]; then
        ${PRINT} "编译完成\n"
    else
        ${PRINT} "编译失败\n"
    fi
}

# 编译模块
msg[make_mod]="编译模块 (make_mod mod1 mod2 mod3)"
function make_mod() {
    ${PRINT} "开始编译模块\n"
    tmp_mods=$@
    declare -a mods
    i=0
    cd ${cfg[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    for tmp_mod in ${tmp_mods} ; do
        if [ ${tmp_mod} = src ]; then
            mods[${i}]=src
            let i++
        elif [[ -e src/${tmp_mod} ]]; then
            mods[${i}]=src/${tmp_mod}
            let i++
        fi
    done
    if [ ${#mods[@]} = 0 ]; then
        ${PRINT} "找不到[%s]模块\n" ${tmp_mods}
        exit 1
    fi
    make_mods=`echo ${mods[@]} | tr ' ' '\n' | sed "s/^/'/g;s/$/\/\*'\,/g;$ s/.$//g" | tr '\n' ' '`
    opts=${cfg[make_args]}
    emakefile="[{[${make_mods}], ${opts}}]"
    ${ERL} -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [ $? = 0 ]; then
        ${PRINT} "编译模块[%s]完成\n" ${make_mods}
    else
        ${PRINT} "编译模块[%s]失败\n" ${make_mods}
    fi
}

# 编译文件
msg[make_file]="编译文件 (make_file file1 file2 file3)"
function make_file() {
    ${PRINT} "开始编译文件\n"
    tmp_files=$@
    declare -a files
    i=0
    cd ${cfg[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    for tmp_file in ${tmp_files} ; do
        file=`find src -type f -name ${tmp_file}.erl`
        if [[ ${file} != "" ]]; then
            files[${i}]=${file}
            let i++
        fi
    done
    if [ ${#files[@]} = 0 ]; then
        ${PRINT} "找不到[%s]文件\n" ${tmp_files}
        exit 1
    fi
    make_files=`echo ${files[@]} | tr ' ' '\n' | sed "s/^/'/g;s/$/'\,/g;$ s/.$//g" | tr '\n' ' '`
    opts=${cfg[make_args]}
    emakefile="[{[${make_files}], ${opts}}]"
    ${ERL} -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [ $? = 0 ]; then
        ${PRINT} "编译文件[%s]完成\n" ${make_files}
    else
        ${PRINT} "编译文件[%s]失败\n" ${make_files}
    fi
}

# 清除ebin目录
msg[clean]="清除ebin目录"
function clean() {
    ${PRINT} "开始清除ebin目录\n"
    rm -rf ${cfg[root]}/ebin/*
    ${PRINT} "清除ebin目录完成\n"
}

# 热更
msg[update]="热更 (update server_type server_id)"
function update() {
    ${PRINT} "未实现热更功能"
}

# 启动服务器
msg[start]="启动服务器 (start server_type server_id)"
function start() {
    server_type=$1
    server_id=$2
    server_name=${cfg[game_name]}_${cfg[platform]}_${server_type}_${server_id}
    server_path=${cfg[zone_path]}/${server_name}
    node_name=${cfg[game_name]}_${cfg[platform]}_${server_type}_${server_id}@${cfg[gateway_host]}
    min_port=$(expr ${cfg[base_port]} + 10000)
    max_port=$(expr ${min_port} + 100)
    if [[ -e ${server_path} && -e ${server_path}/server.config ]]; then
        cd ${server_path}
        ulimit -S -n 10240 && ${ERL} -pa ${cfg[root]}/ebin -name ${node_name} -setcookie ${cfg[cookie]} -hidden -smp enable +P 1024000 +e 102400 +Q 65536 -kernel inet_dist_listen_min ${min_port} inet_dist_listen_max ${max_port} -s manage start
    else
        ${PRINT} "服务器%s未安装" ${server_name}
        exit 1
    fi
}

# 关闭服务器
msg[stop]="关闭服务器 (stop server_type server_id)"
function stop() {
    ${PRINT} "未实现关闭服务器功能"
}

# 生成日志等级
msg[gen_log_level]="生成日志等级"
function gen_log_level() {
    ${PRINT} "开始生成日志等级\n"
    cat ${cfg[root]}/tpl/logs_lib.erl.tpl | sed "s/{log_level}/${cfg[log_level]}/g" > ${cfg[root]}/src/lib/logs_lib.erl
    if [ $? == 0 ]; then
        ${PRINT} "生成日志等级[%s]成功\n" ${cfg[log_level]}
    else
        ${PRINT} "生成日志等级[%s]失败\n" ${cfg[log_level]}
    fi
}

function help() {
    ${PRINT} "帮助\n"
}

if [[ ${!msg[@]} =~ $1 ]]; then
    cmd=$1
    args=${@:2}
    ${cmd} ${args}
else
    help
fi