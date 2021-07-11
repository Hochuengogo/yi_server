#!/usr/bin/env bash

# 获取根路径
function get_root() {
    target_file=$(dirname $0)
    target_file=$(basename ${target_file})
    while [ -L "${target_file}" ]; do
        target_file=$(readlink ${target_file})
        cd $(dirname ${target_file})
        target_file=$(basename ${target_file})
    done
    echo $(pwd -P)
}

REBAR=rebar3
ERL=erl

# 引用配置脚本
ROOT=$(get_root)
source ${ROOT}/setting.sh
source ${ROOT}/func.sh

# 定义字典
declare -A CFG
CFG[root]=${ROOT}
CFG[game_name]=${GAME_NAME}
CFG[game_lang]=${GAME_LANG}
CFG[cookie]=${COOKIE}
CFG[version]=${VERSION}
CFG[zone_path]=${ZONE_PATH}
CFG[platform]=${PLATFORM}
CFG[base_port]=${BASE_PORT}
CFG[mysql_host]=${MYSQL_HOST}
CFG[mysql_port]=${MYSQL_PORT}
CFG[mysql_user]=${MYSQL_USER}
CFG[mysql_password]=${MYSQL_PASSWORD}
CFG[gateway_host]=${GATEWAY_HOST}
CFG[make_args]=${MAKE_ARGS}
CFG[log_level]=${LOG_LEVEL}
CFG[author]=${AUTHOR}

declare -A MSG

# 获取依赖库
MSG[get_dep]="获取依赖库"
function get_dep() {
    printf "开始获取依赖库\n"
    cd ${CFG[root]}/lib && $REBAR upgrade
    printf "获取依赖库完成\n"
}

# 删除依赖库
MSG[clean_dep]="删除依赖库"
function clean_dep() {
    printf "开始删除依赖库\n"
    if [[ -e ${CFG[root]}/lib/_build ]]; then
        rm -rf ${CFG[root]}/lib/_build
    fi
    if [[ -e ${CFG[root]}/tbin ]]; then
        rm -rf ${CFG[root]}/tbin
    fi
    printf "删除依赖库完成\n"
}

# 编译依赖，并将beam复制到tbin目录
MSG[make_dep]="编译依赖，并将beam复制到tbin目录"
function make_dep() {
    printf "开始编译依赖库\n"
    cd ${CFG[root]}/lib && ${REBAR} compile
    cp_dep
    printf "编译依赖库完成\n"
}

# 复制依赖库的beam、app文件
MSG[cp_dep]="复制依赖库的beam、app文件"
function cp_dep() {
    printf "开始复制依赖库\n"
    lib_path=${CFG[root]}/lib/_build/default/lib
    if [[ ! -e ${CFG[root]}/tbin ]]; then
        mkdir ${CFG[root]}/tbin
    fi
    for lib in $(find ${lib_path} -type d -maxdepth 1); do
        if [[ ${lib} != ${lib_path} ]]; then
            cp -a ${lib}/ebin/. ${CFG[root]}/tbin
        fi
    done
    printf "复制依赖库完成\n"
}

# 安装服务器
MSG[install]="安装服务器(install server_type server_id)"
function install() {
    server_type=$(check_empty "请输入服务器类型(zone|center):", $1)
    if [[ ! (${server_type} == zone || ${server_type} == center) ]]; then
        echo "服务器类型只能为zone或center"
        exit 1
    fi
    server_id=$(check_empty "请输入服务器ID(0~10000):", $2)
    expr ${server_id} + 0 1>/dev/null 2>&1
    if [[ $? -eq 0 && ! (${server_id} -ge 0 && ${server_id} -le 10000) ]]; then
        echo "服务器ID不在0~10000范围内"
        exit 1
    fi

    # 判断服务器根路径是否存在
    if [[ ! -e ${CFG[zone_path]} ]]; then
        echo "创建服务器根目录"
        mkdir ${CFG[zone_path]}
    fi

    mysql_db=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    mysql_cmd="mysql -h${CFG[mysql_host]} -u${CFG[mysql_user]} -p${CFG[mysql_password]} -P ${CFG[mysql_port]}"
    ${mysql_cmd} -e "use ${mysql_db};"
    mysql_exist=$?

    server_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    server_path=${CFG[zone_path]}/${server_name}

    if [ ${mysql_exist} = 0 ]; then
        printf "数据库%s已存在\n" ${mysql_db}
        exit 1
    fi
    if [[ -e ${server_path} ]]; then
        printf "已存在%s路径\n" ${server_path}
        exit 1
    fi

    ${mysql_cmd} -e "create database ${mysql_db};"
    if [ $? = 0 ]; then
        printf "数据库%s创建成功\n" ${mysql_db}
    else
        printf "数据库%s创建失败\n" ${mysql_db}
        exit 1
    fi
    ${mysql_cmd} -D${mysql_db} <${CFG[root]}/server.sql
    if [ $? = 0 ]; then
        printf "数据库%s创建表成功\n" ${mysql_db}
    else
        printf "数据库%s创建表失败\n" ${mysql_db}
        exit 1
    fi

    # 创建服务器数据文件夹
    mkdir ${server_path}
    mkdir ${server_path}/dets
    mkdir ${server_path}/log
    gateway_port=$(expr ${CFG[base_port]} + ${server_id})
    open_srv_timestamp=$(date '+%s')
    sed_str="s/{server_id}/${server_id}/g;s/{server_type}/${server_type}/g;s/{platform}/${CFG[platform]}/g;s/{mysql_host}/${CFG[mysql_host]}/g;s/{mysql_port}/${CFG[mysql_port]}/g;s/{mysql_user}/${CFG[mysql_user]}/g;s/{mysql_password}/${CFG[mysql_password]}/g;s/{mysql_db}/${mysql_db}/g;s/{gateway_host}/${CFG[gateway_host]}/g;s/{gateway_port}/${gateway_port}/g;s/{game_name}/${CFG[game_name]}/g;s/{open_srv_timestamp}/${open_srv_timestamp}/g;s/{language}/${CFG[game_lang]}/g;s/{version}/${CFG[version]}/g;s#{code_path}#${CFG[root]}#g;s/{cookie}/${CFG[cookie]}/g"
    cat ${CFG[root]}/tpl/server.config.tpl | sed "${sed_str}" >${server_path}/server.config

    printf "安装服务器%s完成\n" ${server_name}
}

# 卸载服务器
MSG[uninstall]="卸载服务器(uninstall server_type server_id)"
function uninstall() {
    server_type=$(check_empty "请输入服务器类型(zone|center):", $1)
    server_id=$(check_empty "请输入服务器ID(0~10000):", $2)
    mysql_db=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    mysql_cmd="mysql -h${CFG[mysql_host]} -u${CFG[mysql_user]} -p${CFG[mysql_password]} -P ${CFG[mysql_port]}"
    ${mysql_cmd} -e "use ${mysql_db};"
    mysql_exist=$?

    server_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    server_path=${CFG[zone_path]}/${server_name}

    if [[ ${mysql_exist} != 0 && ! -e ${server_path} ]]; then
        printf "服务器%s未安装\n" ${server_name}
        exit 0
    fi
    printf "是否确定卸载服务器%s (yes|no):" ${server_name}
    read check
    if [[ ${check} == yes ]]; then
        ${mysql_cmd} -e "drop database ${mysql_db};"
        if [ $? = 0 ]; then
            printf "删除数据库%s完成\n" ${mysql_db}
        else
            printf "删除数据库%s失败\n" ${mysql_db}
            exit 1
        fi
        rm -rf ${server_path}
        if [ $? = 0 ]; then
            printf "删除服务器路径%s成功\n" ${server_path}
        else
            printf "删除服务器路径%s失败\n" ${server_path}
            exit 1
        fi
        printf "卸载服务器%s完成\n" ${server_name}
    fi
}

# 编译
MSG[make]="编译"
function make() {
    printf "开始编译\n"
    cd ${CFG[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    cp -a ${CFG[root]}/tbin/. ${CFG[root]}/ebin
    cp -a ${CFG[root]}/yi_server.app ${CFG[root]}/ebin
    gen_log_level
    mods=$(find src -type d | sed "s/^/'/g;s/$/\/\*'\,/g;$ s/.$//g" | tr '\n' ' ')
    opts=${CFG[make_args]}
    emakefile="[{[${mods}], ${opts}}]"
    ${ERL} -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [ $? = 0 ]; then
        printf "编译完成\n"
    else
        printf "编译失败\n"
    fi
}

# 编译模块
MSG[make_mod]="编译模块(make_mod mod...)"
function make_mod() {
    printf "开始编译模块\n"
    tmp_mods=$@
    declare -a mods
    i=0
    cd ${CFG[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    for tmp_mod in ${tmp_mods}; do
        if [ ${tmp_mod} = src ]; then
            mods[${i}]=src
            let i++
        elif [[ -e src/${tmp_mod} ]]; then
            mods[${i}]=src/${tmp_mod}
            let i++
        fi
    done
    if [ ${#mods[@]} = 0 ]; then
        printf "找不到[%s]模块\n" ${tmp_mods}
        exit 1
    fi
    make_mods=$(echo ${mods[@]} | tr ' ' '\n' | sed "s/^/'/g;s/$/\/\*'\,/g;$ s/.$//g" | tr '\n' ' ')
    opts=${CFG[make_args]}
    emakefile="[{[${make_mods}], ${opts}}]"
    ${ERL} -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [ $? = 0 ]; then
        printf "编译模块[%s]完成\n" ${make_mods}
    else
        printf "编译模块[%s]失败\n" ${make_mods}
    fi
}

# 编译文件
MSG[make_file]="编译文件(make_file file...)"
function make_file() {
    printf "开始编译文件\n"
    tmp_files=$@
    declare -a files
    i=0
    cd ${CFG[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    for tmp_file in ${tmp_files}; do
        file=$(find src -type f -name ${tmp_file}.erl)
        if [[ ${file} != "" ]]; then
            files[${i}]=${file}
            let i++
        fi
    done
    if [ ${#files[@]} = 0 ]; then
        printf "找不到[%s]文件\n" ${tmp_files}
        exit 1
    fi
    make_files=$(echo ${files[@]} | tr ' ' '\n' | sed "s/^/'/g;s/$/'\,/g;$ s/.$//g" | tr '\n' ' ')
    opts=${CFG[make_args]}
    emakefile="[{[${make_files}], ${opts}}]"
    ${ERL} -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [ $? = 0 ]; then
        printf "编译文件[%s]完成\n" ${make_files}
    else
        printf "编译文件[%s]失败\n" ${make_files}
    fi
}

# 清除ebin目录
MSG[clean]="清除ebin目录"
function clean() {
    printf "开始清除ebin目录\n"
    rm -rf ${CFG[root]}/ebin/*
    printf "清除ebin目录完成\n"
}

# 启动服务器
MSG[start]="启动服务器(start server_type server_id)"
function start() {
    server_type=$1
    server_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}@${CFG[gateway_host]}
    min_port=$(expr ${CFG[base_port]} + 10000)
    max_port=$(expr ${min_port} + 100)
    if [[ -e ${server_path} && -e ${server_path}/server.config ]]; then
        cmd="ulimit -S -n 10240 && ${ERL} -pa ${CFG[root]}/ebin -name ${node_name} -setcookie ${CFG[cookie]} -hidden -smp enable +P 1024000 +e 102400 +Q 65536 -kernel inet_dist_listen_min ${min_port} inet_dist_listen_max ${max_port} -s manage start"
        cd ${server_path} && screen -dmS ${node_name} && screen -x -S ${node_name} -p 0 -X stuff "${cmd}\n"
        if [ $? == 0 ]; then
            printf "服务器%s启动成功\n" ${node_name}
        else
            printf "服务器%s启动失败\n" ${node_name}
        fi
    else
        printf "服务器%s未安装\n" ${server_name}
        exit 1
    fi
}

# 关闭服务器
MSG[stop]="关闭服务器(stop server_type server_id)"
function stop() {
    server_type=$1
    server_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}@${CFG[gateway_host]}
    exec_node_name=exec_${CFG[game_name]}_${CFG[platform]}@${CFG[gateway_host]}
    $ERL -name ${exec_node_name} -setcookie ${CFG[cookie]} -noshell -eval "rpc:call('${node_name}',manage,stop,[])." -s init stop
    if [ $? = 0 ]; then
        printf "服务器%s关闭成功\n" ${node_name}
    else
        printf "服务器%s关闭失败\n" ${node_name}
    fi
}

# 热更
MSG[update]="热更(update server_type server_id)"
function update() {
    server_type=$1
    server_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}@${CFG[gateway_host]}
    exec_node_name=exec_${CFG[game_name]}_${CFG[platform]}@${CFG[gateway_host]}
    $ERL -name ${exec_node_name} -setcookie ${CFG[cookie]} -noshell -eval "rpc:call('${node_name}',srv_code,hot_update,[])." -s init stop
    if [ $? = 0 ]; then
        printf "服务器%s热更成功\n" ${node_name}
    else
        printf "服务器%s热更失败\n" ${node_name}
    fi
}

# remsh连接节点
MSG[remsh]="remsh连接节点(remsh server_type server_id)"
function remsh() {
    server_type=$1
    server_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}@${CFG[gateway_host]}
    $ERL -name remsh_${CFG[game_name]}_${CFG[platform]}@${CFG[gateway_host]} -remsh ${node_name} -setcookie ${CFG[cookie]}
    if [ $? != 0 ]; then
        ptintf "remsh连接服务器%s失败\n" ${node_name}
    fi
}

# 在节点上执行指令
MSG[exec]="在节点上执行指令(exec server_type server_id m f a)"
function exec() {
    server_type=$1
    server_id=$2
    mod=$3
    func=$4
    args=$5
    server_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${server_type}_${server_id}@${CFG[gateway_host]}
    exec_node_name=exec_${CFG[game_name]}_${CFG[platform]}@${CFG[gateway_host]}
    $ERL -name ${exec_node_name} -setcookie ${CFG[cookie]} -noshell -eval "Ret=rpc:call('${node_name}',${mod},${func},${args}),io:format(\"~w~n\",[Ret])." -s init stop
    if [ $? != 0 ]; then
        ptintf "在服务器%s执行指令%s失败\n" ${node_name} "${mod} ${func} ${args}"
    fi
}

# 生成日志等级
MSG[gen_log_level]="生成日志等级"
function gen_log_level() {
    printf "开始生成日志等级\n"
    cat ${CFG[root]}/tpl/logs_lib.erl.tpl | sed "s/{log_level}/${CFG[log_level]}/g" >${CFG[root]}/src/lib/logs_lib.erl
    if [ $? == 0 ]; then
        printf "生成日志等级[%s]成功\n" ${CFG[log_level]}
    else
        printf "生成日志等级[%s]失败\n" ${CFG[log_level]}
    fi
}

MSG[gen_mod]="生成模块(gen_mod mod_type(erl|gs|gf|rpc|ver|mod|mgr|rank|worker|match) mod_name desc mod_dict)"
function gen_mod() {
    mod_type=$(check_empty "请输入模块类型(erl|gs|gf|rpc|ver|mod|mgr|rank|worker|match):" $1)
    mod_types=("erl" "gs" "gf" "rpc" "ver" "mod" "mgr" "rank" "worker" "match")
    check_in ${mod_type} "${mod_types[@]}"
    if [ $? != 0 ]; then
        echo "没有该模块类型，请重新输入"
        exit 1
    fi
    mod_name=$(check_empty "请输入模块名:" $2)
    desc=$(check_empty "请输入描述:" $3)
    mod_dict=$(check_empty "请输入模块目录:" $4)
    do_gen_mod ${mod_type} ${mod_name} ${desc} ${mod_dict}
}

function do_gen_mod() {
    mod_type=$1
    mod_name=$2
    desc=$3
    mod_dict=$4
    base_mods=("erl" "gs" "gf" "rpc" "ver")
    other_mods=("mod" "mgr" "rank")
    many_mods=("worker" "match")
    check_in ${mod_type} "${base_mods[@]}"
    in_base_mods=$?
    check_in ${mod_type} "${other_mods[@]}"
    in_other_mods=$?
    check_in ${mod_type} "${many_mods[@]}"
    in_many_mods=$?
    if [[ ${in_base_mods} == 0 ]]; then
        echo "基础模块"
    elif [[ ${in_other_mods} == 0 ]]; then
        echo "其他模块"
    elif [[ ${in_many_mods} == 0 ]]; then
        echo "多个模块"
    else
        echo "没有该模块类型"
    fi
}

function help() {
    printf "请输入以下指令：\n"
    for key in "${!MSG[@]}"; do
        printf "%-20s%s\n" "$key" "${MSG[$key]}"
    done
}

check_in $1 "${!MSG[@]}"
if [[ $? == 0 ]]; then
    cmd=$1
    args=${@:2}
    ${cmd} ${args}
else
    help
fi
