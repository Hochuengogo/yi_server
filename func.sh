#!/usr/bin/env bash

# 获取根路径
function get_root() {
    target_file=`dirname $0`
    target_file=`basename ${target_file}`
    while [ -L "${target_file}" ]
    do
        target_file=`readlink ${target_file}`
        cd `dirname ${target_file}`
        target_file=`basename ${target_file}`
    done
    echo `pwd -P`
}

# 是否在列表里
function check_in() {
    check=$1
    array=${@:2}
    for i in ${array[@]} ; do
        if [ "$i" == "$check" ]; then
            return 0
        else
            continue
        fi
    done
    return 1
}

# 判断是否为空，为空则读取输入
function check_empty() {
    desc=$1
    check=$2
    if [ -z ${check} ]; then
        read -p ${desc} enter
        echo ${enter}
    else
        echo ${check}
    fi
}