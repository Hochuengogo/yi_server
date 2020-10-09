#!/usr/bin/env bash

ROOT=`dirname $0`

# 输出函数变量
PRINT=printf

# rebar变量
REBAR=rebar3

# 获取依赖库
function get_dep() {
    ${PRINT} "开始获取依赖库\n"
    ${REBAR} upgrade
    ${PRINT} "获取依赖库完成\n"
}

# 删除依赖库
function del_dep() {
    ${PRINT} "开始删除依赖库\n"
    if [[ -e _build ]] && [[ -d _build ]]; then
        rm -rf _build
    fi
    if [[ -e tbin ]] && [[ -d tbin ]]; then
        rm -rf tbin
    fi
    ${PRINT} "删除依赖库完成\n"
}

# 编译依赖，并将beam复制到tbin目录
function make_dep() {
    ${PRINT} "开始编译依赖库\n"
    lib_path=_build/default/lib
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
    lib_path=_build/default/lib
    if [[ ! -e tbin ]]; then
        mkdir tbin
    fi
    for ebin in `find ${lib_path} -type d -name ebin -maxdepth 2` ; do
        cp -a ${ROOT}/${ebin}/. tbin
    done
    ${PRINT} "复制依赖库完成\n"
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
esac