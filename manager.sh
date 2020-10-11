#!/usr/bin/env bash

# 这个脚本文件要在项目的根路径中
# 项目根路径
ROOT=`dirname $0`

# 配置文件
CFG_FILE="./setting.config"

# 输出函数变量
PRINT=printf

# rebar变量
REBAR=rebar3

# erlang变量
ERL=erl

# 获取依赖库
function get_dep() {
    ${PRINT} "开始获取依赖库\n"
    ${REBAR} upgrade
    ${PRINT} "获取依赖库完成\n"
}

# 删除依赖库
function del_dep() {
    ${PRINT} "开始删除依赖库\n"
    if [[ -e _build ]]; then
        rm -rf _build
    fi
    if [[ -e tbin ]]; then
        rm -rf tbin
    fi
    ${PRINT} "删除依赖库完成\n"
}

# 编译依赖，并将beam复制到tbin目录
function make_dep() {
    ${PRINT} "开始编译依赖库\n"
    lib_path="_build/default/lib"
    if [[ -d ${lib_path} ]]; then
        for lib in `find ${lib_path} -type d -maxdepth 1` ; do
            if [[ ${lib} != ${lib_path} ]]; then
              cd ${ROOT}/${lib} && ${REBAR} compile
            fi
        done
        cd ${ROOT}
        cp_dep
        ${PRINT} "编译依赖库完成\n"
    else
        ${PRINT} "不存在依赖库，请先获取依赖库\n"
    fi
}

# 复制依赖库的beam、app文件
function cp_dep() {
    ${PRINT} "开始复制依赖库\n"
    lib_path="_build/default/lib"
    if [[ ! -e tbin ]]; then
        mkdir tbin
    fi
    for ebin in `find ${lib_path} -type d -name ebin -maxdepth 2` ; do
        cp -a ${ROOT}/${ebin}/. tbin
    done
    ${PRINT} "复制依赖库完成\n"
}

# 安装服务器
function install() {
    todo
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