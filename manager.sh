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
source setting.sh

# 定义字典
declare -A cfg
cfg[root]=${ROOT}
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
    cd ${cfg[root]} && $REBAR upgrade
    ${PRINT} "获取依赖库完成\n"
}

# 删除依赖库
msg[del_dep]="删除依赖库"
function del_dep() {
    ${PRINT} "开始删除依赖库\n"
    if [[ -e ${cfg[root]}/_build ]]; then
        rm -rf ${cfg[root]}/_build
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
    lib_path=${cfg[root]}/_build/default/lib
    if [[ -d ${lib_path} ]]; then
        for lib in `find ${lib_path} -type d -maxdepth 1` ; do
            if [[ ${lib} != ${lib_path} ]]; then
              cd ${lib} && ${REBAR} compile
            fi
        done
        cp_dep
        ${PRINT} "编译依赖库完成\n"
    else
        ${PRINT} "不存在依赖库，请先获取依赖库\n"
    fi
}

# 复制依赖库的beam、app文件
msg[cp_dep]="复制依赖库的beam、app文件"
function cp_dep() {
    ${PRINT} "开始复制依赖库\n"
    lib_path=${cfg[root]}/_build/default/lib
    if [[ ! -e ${cfg[root]}/tbin ]]; then
        mkdir ${cfg[root]}/tbin
    fi
    for ebin in `find ${lib_path} -type d -name ebin -maxdepth 2` ; do
        cp -a ${ebin}/. ${cfg[root]}/tbin
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

  ${PRINT} "请输入服务器ID(0~10000):"
  read server_id
  expr ${server_id} + 0 1>/dev/null 2>&1
  if [[ $? -eq 0 && ! (${server_id} -ge 0 && ${server_id} -le 10000) ]]; then
    ${PRINT} "服务器ID不在0~10000范围内\n"
    exit 1
  fi

  # 判断服务器根路径是否存在
  if [[ ! -e ${cfg[zone_path]} ]]; then
      ${PRINT} "创建服务器根目录\n"
      mkdir ${cfg[zone_path]}
  fi

  # 创建数据库
  mysql_db=${cfg[platform]}_${server_type}_${server_id}

  # 创建服务器数据文件夹
  server_path=${cfg[zone_path]}/${cfg[platform]}_${server_type}_${server_id}
  if [[ ! -e ${server_path} ]]; then
    mkdir ${server_path}
  else
    ${PRINT} "已存在%s路径\n" ${server_path}
    exit 1
  fi
  mkdir ${server_path}/dets
  mkdir ${server_path}/log
  gateway_port=$(expr ${cfg[base_port]} + ${server_id})
  sed_str="s/{server_id}/"${server_id}"/g;s/{server_type}/"${server_type}"/g;s/{platform}/"${cfg[platform]}"/g;s/{mysql_host}/"${cfg[mysql_host]}"/g;s/{mysql_port}/"${cfg[mysql_port]}"/g;s/{mysql_user}/"${cfg[mysql_user]}"/g;s/{mysql_password}/"${cfg[mysql_password]}"/g;s/{mysql_db}/"${mysql_db}"/g;s/{gateway_host}/"${cfg[gateway_host]}"/g;s/{gateway_port}/"${gateway_port}"/g"
  cat ${cfg[root]}/tpl/server.config.tpl | sed "${sed_str}" > ${server_path}/server.config
  ${PRINT} "安装服务器%s完成\n" ${cfg[platform]}_${server_type}_${server_id}
}

# 卸载服务器
function uninstall() {
    todo
}

# 编译
function make() {
    ${PRINT} "开始编译\n"
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    mods=`find src -type d | sed "s/^/'/;s/$/\/\*'\,/;$ s/.$//" | tr '\n' ' '`
    opts="[debug_info, {i, \"include\"}, {outdir, \"ebin\"}]"
    emakefile="[{[${mods}], ${opts}}]"
    ${ERL} -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    ${PRINT} "编译完成\n"
}

# 编译模块
function make_mod() {
    ${PRINT} "未实现编译模块功能"
}

# 编译文件
function make_file() {
    ${PRINT} "未实现编译文件功能"
}

# 热更
function update() {
    ${PRINT} "未实现热更功能"
}

# 启动服务器
function start() {
    ulimit -S -n 10240 && ${ERL} -pa ebin -pa tbin -s manager start
}

# 关闭服务器
function stop() {
    ${PRINT} "未实现关闭服务器功能"
}

# 生成日志等级文件
function gen_log() {
    log_line=`grep ^\{log_level ${CFG_FILE}`
    python gen_log.py "${log_line}"
    if [ $? == 0 ]; then
        echo "生成日志等级文件成功"
    else
        echo "生成日志等级文件失败"
    fi
}

case $1 in
  get_dep)
    get_dep
  ;;
  del_dep)
      del_dep
    ;;
  make_dep)
      make_dep
    ;;
  cp_dep)
      cp_dep
    ;;
  install)
      install
    ;;
  uninstall)
      uninstall
    ;;
  make)
      make
    ;;
  make_mod)
      make_mod
    ;;
  make_file)
      make_file
    ;;
  update)
      update
    ;;
  start)
      start
    ;;
  stop)
      stop
    ;;
  gen_log)
      gen_log
    ;;
esac