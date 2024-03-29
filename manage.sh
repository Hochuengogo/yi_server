#!/usr/bin/env bash

if [[ -x "func.sh" ]]; then
    source func.sh
else
    echo "找不到func.sh脚本"
    exit 1
fi

ROOT=$(get_real_dir)
echo -e "server项目路径:$(color green $ROOT)"

sub_shells=("setting.sh")
for sub_shell in ${sub_shells[@]}; do
    if [[ -f "${ROOT}/${sub_shell}" ]]; then
        source "${ROOT}/${sub_shell}"
    fi
done

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

declare -A DOC
# 获取依赖库
DOC[get_dep]="获取依赖库"
function get_dep() {
    if [[ -d "${CFG[root]}/lib" ]]; then
        echo -e "$(color green "开始获取依赖库")"
        cd "${CFG[root]}/lib" && rebar3 upgrade
        if [[ $? -eq 0 ]]; then
            echo -e "$(color green "获取依赖库完成")"
            return 0
        fi
        echo -e "$(color red "获取依赖库失败")"
        return 1
    fi
    echo -e "$(color red "${CFG[root]}/lib路径不存在")"
    return 1
}

# 删除依赖库
DOC[clean_dep]="删除依赖库"
function clean_dep() {
    echo -e "$(color green "开始删除依赖库")"
    if [[ -e "${CFG[root]}/lib/_build" ]]; then
        rm -rf "${CFG[root]}/lib/_build"
    fi
    if [[ -e "${CFG[root]}/tbin" ]]; then
        rm -rf "${CFG[root]}/tbin"
    fi
    echo -e "$(color green "删除依赖库完成")"
}

# 编译依赖，并将beam复制到tbin目录
DOC[make_dep]="编译依赖，并将beam复制到tbin目录"
function make_dep() {
    if [[ -d "${CFG[root]}/lib" ]]; then
        echo -e "$(color green "开始编译依赖库")"
        cd "${CFG[root]}/lib" && rebar3 compile && cp_dep
        if [[ $? -eq 0 ]]; then
            echo -e "$(color green "编译依赖库完成")"
            return 0
        fi
        echo -e "$(color red "编译依赖库失败")"
        return 1
    fi
    echo -e "$(color red "${CFG[root]}/lib路径不存在")"
    return 1
}

# 复制依赖库的beam、app文件
DOC[cp_dep]="复制依赖库的beam、app文件"
function cp_dep() {
    echo -e "$(color green "开始复制依赖库")"
    lib_path="${CFG[root]}/lib/_build/default/lib"
    if [[ ! -e "${CFG[root]}/tbin" ]]; then
        mkdir "${CFG[root]}/tbin"
    fi
    for lib in $(find $lib_path -type d -maxdepth 1); do
        if [[ "$lib" != "$lib_path" ]]; then
            cp -a "$lib/ebin/." "${CFG[root]}/tbin"
        fi
    done
    echo -e "$(color green "复制依赖库完成")"
}

# 安装服务器
DOC[install]="安装服务器(install srv_type srv_id)"
function install() {
    srv_type=$(check_empty "请输入服务器类型(zone|center):" $1)
    if [[ ${srv_type} != zone ]] && [[ ${srv_type} != center ]]; then
        echo -e "$(color yellow "服务器类型只能为zone或center")"
        return 1
    fi
    srv_id=$(check_empty "请输入服务器ID(非负整数):" $2)
    if ! is_int ${srv_id} || [[ ${srv_id} -lt 0 ]]; then
        echo -e "$(color yellow "非法服务器ID")"
        return 1
    fi
    # 判断服务器根路径是否存在
    if [[ ! -e ${CFG[zone_path]} ]]; then
        echo -e "$(color green "创建服务器根目录")"
        mkdir ${CFG[zone_path]}
    fi

    mysql_db=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    mysql_cmd="mysql -h${CFG[mysql_host]} -u${CFG[mysql_user]} -p${CFG[mysql_password]} -P ${CFG[mysql_port]}"
    ${mysql_cmd} -e "use ${mysql_db};"
    mysql_exist=$?

    server_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    server_path=${CFG[zone_path]}/${server_name}

    if [[ ${mysql_exist} -eq 0 ]]; then
        echo -e "$(color yellow "数据库${mysql_db}已存在")"
        return 1
    fi
    if [[ -e ${server_path} ]]; then
        echo -e "$(color yellow "路径${server_path}已存在")"
        return 1
    fi

    ${mysql_cmd} -e "create database ${mysql_db};"
    if [[ $? -eq 0 ]]; then
        echo -e "$(color green "创建数据库")$(color sky_blue ${mysql_db})$(color green "成功")"
    else
        echo -e "$(color red "创建数据库${mysql_db}失败")"
        return 1
    fi
    ${mysql_cmd} -D${mysql_db} <${CFG[root]}/server.sql
    if [[ $? -eq 0 ]]; then
        echo -e "$(color green "创建数据库")$(color sky_blue ${mysql_db})$(color green "表成功")"
    else
        echo -e "$(color red "创建数据库${mysql_db}表失败")"
        return 1
    fi

    # 创建服务器数据文件夹
    mkdir ${server_path}
    mkdir ${server_path}/dets
    mkdir ${server_path}/log
    gateway_port=$((${CFG[base_port]} + ${srv_id}))
    open_srv_timestamp=$(date '+%s')
    sed_str="s/{server_id}/${srv_id}/g;s/{server_type}/${srv_type}/g;s/{platform}/${CFG[platform]}/g;s/{mysql_host}/${CFG[mysql_host]}/g;s/{mysql_port}/${CFG[mysql_port]}/g;s/{mysql_user}/${CFG[mysql_user]}/g;s/{mysql_password}/${CFG[mysql_password]}/g;s/{mysql_db}/${mysql_db}/g;s/{gateway_host}/${CFG[gateway_host]}/g;s/{gateway_port}/${gateway_port}/g;s/{game_name}/${CFG[game_name]}/g;s/{open_srv_timestamp}/${open_srv_timestamp}/g;s/{language}/${CFG[game_lang]}/g;s/{version}/${CFG[version]}/g;s#{code_path}#${CFG[root]}#g;s/{cookie}/${CFG[cookie]}/g;"
    cat ${CFG[root]}/tpl/server.config.tpl | sed -e "${sed_str}" >${server_path}/server.config

    echo -e "$(color green "安装服务器")$(color sky_blue ${server_name})$(color green "完成")"
}

# 卸载服务器
DOC[uninstall]="卸载服务器(uninstall srv_type srv_id)"
function uninstall() {
    srv_type=$(check_empty "请输入服务器类型(zone|center):", $1)
    srv_id=$(check_empty "请输入服务器ID(非负整数):", $2)
    mysql_db=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    mysql_cmd="mysql -h${CFG[mysql_host]} -u${CFG[mysql_user]} -p${CFG[mysql_password]} -P ${CFG[mysql_port]}"
    ${mysql_cmd} -e "use ${mysql_db};"
    mysql_exist=$?

    server_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    server_path=${CFG[zone_path]}/${server_name}
    if [[ ${mysql_exist} -ne 0 ]] && [[ ! -e ${server_path} ]]; then
        echo -e "服务器$(color sky_blue ${server_name})未安装"
        return 0
    fi
    read -p "是否确定卸载服务器$(color sky_blue ${server_name}) (yes|no):" check
    if [[ ${check} == yes ]]; then
        ${mysql_cmd} -e "drop database ${mysql_db};"
        if [ $? -eq 0 ]; then
            echo -e "$(color green "删除数据库")$(color sky_blue ${mysql_db})$(color green "成功")"
        else
            echo -e "$(color red "删除数据库${mysql_db}失败")"
            return 1
        fi
        rm -rf ${server_path}
        if [[ $? -eq 0 ]]; then
            echo -e "$(color green "删除服务器路径")$(color sky_blue ${server_path})$(color green "成功")"
        else
            echo -e "$(color red "删除服务器路径${server_path}失败")"
            return 1
        fi
        echo -e "$(color green "卸载服务器")$(color sky_blue ${server_name})$(color green "完成")"
    fi
}

# 编译
DOC[make]="编译"
function make() {
    echo -e "$(color green "开始编译")"
    cd ${CFG[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    cp -a ${CFG[root]}/tbin/. ${CFG[root]}/ebin
    cp -a ${CFG[root]}/yi_server.app ${CFG[root]}/ebin
    gen_log_level
    mods="$(find src -type d | awk '{print "'\''"$0"/*'\'',"}' | sed '$s/,//')"
    opts=${CFG[make_args]}
    emakefile="[{[${mods}], ${opts}}]"
    erl -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [[ $? -eq 0 ]]; then
        echo -e "$(color green "编译完成")"
    else
        echo -e "$(color red "编译失败")"
    fi
}

# 编译模块
DOC[make_mod]="编译模块(make_mod mod...)"
function make_mod() {
    echo -e "$(color green "开始编译模块")"
    tmp_mods=$@
    declare -a mods
    i=0
    cd ${CFG[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    for tmp_mod in ${tmp_mods}; do
        if [[ ${tmp_mod} == "src" ]]; then
            mods[$i]="src"
            let i++
        elif [[ -e "src/${tmp_mod}" ]]; then
            mods[$i]="src/${tmp_mod}"
            let i++
        elif [[ -e "src/mod/${tmp_mod}" ]]; then
            mods[$i]="src/mod/${tmp_mod}"
            let i++
        else
            echo -e "$(color yellow "找不到${tmp_mod}模块")"
        fi
    done
    if [[ ${#mods[@]} == 0 ]]; then
        echo -e "$(color red "找不到[${tmp_mods}]模块")"
        return 1
    fi
    make_mods="$(echo ${mods[@]} | tr ' ' '\n' | awk '{print "'\''"$0"/*'\'',"}' | sed '$s/,//')"
    opts=${CFG[make_args]}
    emakefile="[{[${make_mods}], ${opts}}]"
    erl -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [[ $? -eq 0 ]]; then
        echo -e "$(color green "编译模块[")$(color sky_blue "${mods[*]}")$(color green "]完成")"
    else
        echo -e "$(color red "编译模块[${mods[*]}]失败")"
        return 1
    fi
}

# 编译文件
DOC[make_file]="编译文件(make_file file...)"
function make_file() {
    echo -e "$(color green "开始编译文件")"
    tmp_files=$@
    declare -a files
    i=0
    cd ${CFG[root]}
    if [[ ! -e ebin ]]; then
        mkdir ebin
    fi
    for tmp_file in ${tmp_files}; do
        file=$(find src -type f -name ${tmp_file}.erl)
        if [[ -n ${file} ]]; then
            files[$i]=${file}
            let i++
        fi
    done
    if [ ${#files[@]} -eq 0 ]; then
        echo -e "$(color red "找不到[${tmp_files}]文件")"
        return 1
    fi
    make_files="$(echo ${files[@]} | tr ' ' '\n' | awk '{print "'\''"$0"'\'',"}' | sed '$s/,//')"
    opts=${CFG[make_args]}
    emakefile="[{[${make_files}], ${opts}}]"
    erl -noshell -eval "make:all([{emake,${emakefile}}])." -s init stop
    if [[ $? -eq 0 ]]; then
        echo -e "$(color green "编译文件[")$(color sky_blue "${files[*]}")$(color green "]完成")"
    else
        echo -e "$(color red "编译文件[${files[*]}]失败")"
        return 1
    fi
}

# 清除ebin目录
DOC[clean]="清除ebin目录"
function clean() {
    echo -e "$(color green "开始清除ebin目录")"
    rm -rf ${CFG[root]}/ebin/*
    echo -e "$(color green "清除ebin目录完成")"
}

# 启动服务器
DOC[start]="启动服务器节点(start srv_type srv_id)"
function start() {
    srv_type=$1
    srv_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}@${CFG[gateway_host]}
    min_port=$((${CFG[base_port]} + 10000))
    max_port=$((${min_port} + 100))
    if [[ -e ${server_path} ]] && [[ -e ${server_path}/server.config ]]; then
        if has_screen ${node_name}; then
            echo -e "$(color green "服务器")$(color sky_blue ${node_name})$(color green "已启动")"
            return 0
        fi
        log_line=$(cat ${server_path}/screenlog.0 | wc -l)
        if [ ${log_line} -gt 1000000 ]; then
            mv ${server_path}/screenlog.0 ${server_path}/screenlog.1
        fi
        cmd="ulimit -S -n 10240 && erl -pa ${CFG[root]}/ebin -name ${node_name} -setcookie ${CFG[cookie]} -hidden -smp enable +P 1024000 +e 102400 +Q 65536 -kernel inet_dist_listen_min ${min_port} inet_dist_listen_max ${max_port} -s manage start"
        cd ${server_path} && screen -L -dmS ${node_name} && screen -x -S ${node_name} -p 0 -X stuff "${cmd}\n"
        if [[ $? -eq 0 ]]; then
            echo -e "$(color green "启动服务器")$(color sky_blue ${node_name})$(color green "成功")"
            return 0
        fi
        echo -e "$(color red "启动服务器${node_name}失败")"
        return 1
    fi
    echo -e "$(color yellow "未安装服务器${node_name}")"
    return 1
}

# 进入服务器控制台
DOC[shell]="进入screen窗口(shell srv_type srv_id)"
function shell() {
    srv_type=$1
    srv_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}@${CFG[gateway_host]}
    if has_screen ${node_name}; then
        if in_screen ${node_name}; then
            echo -e "$(color green "已在控制台中！！！")"
            return 0
        fi
        echo -e "$(color green "请注意接管！！！")"
        sleep 3
        screen -r ${node_name}
        return 0
    fi
    echo -e "$(color red "不存在服务器${node_name}控制台")"
    return 1
}

# 关闭服务器
DOC[stop]="关闭服务器节点(stop srv_type srv_id)"
function stop() {
    srv_type=$1
    srv_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}@${CFG[gateway_host]}
    exec_node_name=exec_${CFG[game_name]}_${CFG[platform]}@${CFG[gateway_host]}
    if has_screen ${node_name}; then
        ret=$(erl -name ${exec_node_name} -setcookie ${CFG[cookie]} -noshell -eval "Ret=rpc:call('${node_name}',manage,stop,[]),io:format(\"~w~n\",[Ret])." -s init stop)
        if [[ ! $ret == "ok" ]]; then
            echo -e "$(color red "关闭服务器${node_name}失败，返回值：${ret}")"
            return 1
        fi
        ps aux | grep ${node_name} | grep -v grep | awk '{print $2}' | xargs kill
        echo -e "$(color green "关闭服务器")$(color sky_blue ${node_name})$(color green "成功")"
        return 0
    fi
    echo -e "$(color green "服务器")$(color sky_blue ${node_name})$(color green "已关闭")"
    return 0
}

# 热更
DOC[update]="热更(update srv_type srv_id)"
function update() {
    srv_type=$1
    srv_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}@${CFG[gateway_host]}
    exec_node_name=exec_${CFG[game_name]}_${CFG[platform]}@${CFG[gateway_host]}
    ret=$(erl -name ${exec_node_name} -setcookie ${CFG[cookie]} -noshell -eval "Ret=rpc:call('${node_name}',srv_code,hot_update,[]),io:format(\"~w~n\",[Ret])." -s init stop)
    if [[ $ret == "ok" ]]; then
        echo -e "$(color green "热更服务器")$(color sky_blue ${node_name})$(color green "成功")"
        return 0
    fi
    echo -e "$(color red "热更服务器${node_name}失败")"
    return 1
}

# remsh连接节点
DOC[remsh]="remsh连接节点(remsh srv_type srv_id)"
function remsh() {
    srv_type=$1
    srv_id=$2
    server_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}@${CFG[gateway_host]}
    erl -name remsh_${CFG[game_name]}_${CFG[platform]}@${CFG[gateway_host]} -remsh ${node_name} -setcookie ${CFG[cookie]}
    if [[ $? -ne 0 ]]; then
        echo -e "$(color yellow "remsh连接服务器${node_name}失败")"
        return 1
    fi
}

# 在节点上执行指令
DOC[exec]="在节点上执行指令(exec srv_type srv_id m f a)"
function exec() {
    srv_type=$1
    srv_id=$2
    mod=$3
    func=$4
    args=$5
    server_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}
    server_path=${CFG[zone_path]}/${server_name}
    node_name=${CFG[game_name]}_${CFG[platform]}_${srv_type}_${srv_id}@${CFG[gateway_host]}
    exec_node_name=exec_${CFG[game_name]}_${CFG[platform]}@${CFG[gateway_host]}
    erl -name ${exec_node_name} -setcookie ${CFG[cookie]} -noshell -eval "Ret=rpc:call('${node_name}',${mod},${func},${args}),io:format(\"~w~n\",[Ret])." -s init stop
    if [[ $? -ne 0 ]]; then
        echo -e "$(color red "在服务器${node_name}执行指令${mod} ${func} ${args}失败")"
        return 1
    fi
}

# 生成日志等级
DOC[gen_log_level]="生成日志等级"
function gen_log_level() {
    echo -e "$(color green "开始生成日志等级")"
    cat ${CFG[root]}/tpl/logs_lib.erl.tpl | sed "s/{log_level}/${CFG[log_level]}/g" >${CFG[root]}/src/lib/logs_lib.erl
    if [[ $? -eq 0 ]]; then
        echo -e "$(color green "生成日志等级[")$(color sky_blue ${CFG[log_level]})$(color green "]成功")"
        return 0
    fi
    echo -e "$(color red "生成日志等级[${CFG[log_level]}]失败")"
    return 1
}

DOC[gen_mod]="生成模块(gen_mod mod_type(hrl|erl|gs|gf|rpc|ver|mod|mgr|rank|worker|match) mod_name desc mod_dict)"
function gen_mod() {
    mod_type=$(check_empty "请输入模块类型(hrl|erl|gs|gf|rpc|ver|mod|mgr|rank|worker|match):" $1)
    mod_types=("hrl" "erl" "gs" "gf" "rpc" "ver" "mod" "mgr" "rank" "worker" "match")
    member ${mod_type} "${mod_types[@]}"
    if [[ $? -ne 0 ]]; then
        echo -e "$(color red "没有该模块类型，请重新输入")"
        return 1
    fi
    mod_name=$(check_empty "请输入模块名:" $2)
    desc=$(check_empty "请输入描述:" $3)
    if [ ${mod_type} != "hrl" ]; then
        mod_dir=$(check_empty "请输入模块目录:" $4)
    else
        mod_dir=""
    fi
    do_gen_mod ${mod_type} ${mod_name} ${desc} ${mod_dir}
}

# 处理生成模块
function do_gen_mod() {
    mod_type=$1
    mod_name=$2
    desc=$3
    mod_dir=$4
    base_mods=("erl" "gs" "gf" "rpc" "ver")
    other_mods=("mod" "mgr" "rank")
    many_mods=("worker" "match")

    author=${CFG[author]}
    year=$(date "+%Y")
    create_time=$(date "+%Y-%m-%d %H:%M:%S")
    if [ ${mod_type} = "hrl" ]; then
        mod=$mod_name
    elif member ${mod_type} "${base_mods[@]}"; then
        mod=$mod_name
    elif member ${mod_type} "${other_mods[@]}"; then
        echo "其他模块" # 未实现
        return 1
    elif member ${mod_type} "${many_mods[@]}"; then
        echo "多个模块" # 未实现
        return 1
    else
        echo "没有该模块类型"
        return 1
    fi

    if [ ${mod_type} = "hrl" ]; then
        tpl_file=${CFG[root]}/tpl/inc.hrl.tpl
        tar_dir=${CFG[root]}/include
        tar_file=${tar_dir}/${mod}.hrl
        desc_mod=include/${mod}.hrl
    else
        tpl_file=${CFG[root]}/tpl/${mod_type}.erl.tpl
        tar_dir=${CFG[root]}/src/${mod_dir}
        tar_file=${tar_dir}/${mod}.erl
        desc_mod=src/${mod_dir}/${mod}.erl
    fi
    if [[ ! -e $tar_dir ]]; then
        mkdir $tar_dir
    fi
    if [[ -e $tar_file ]]; then
        echo -e "$(color red "[${desc_mod}]模块已存在")"
        return 1
    fi
    sed_str="s/{author}/${author}/g;s/{year}/${year}/g;s/{desc}/${desc}/g;s/{create_time}/${create_time}/g;s/{mod}/${mod}/g;"
    cat $tpl_file | sed -e "${sed_str}" >$tar_file
    if [ $? -ne 0 ]; then
        echo -e "$(color red "[${desc_mod}]模块创建失败")"
        return 1
    fi
    echo -e "$(color green "[${desc_mod}]模块创建成功")"
}

# 帮助
function help() {
    echo -e "$(color green "请输入以下指令：")"
    for key in $(echo "${!DOC[@]}" | tr " " "\n" | sort | tr "\n" " "); do
        printf "%-25s%s\n" "$(color green "$key")" "$(color sky_blue "${DOC[$key]}")"
    done
}

if member $1 "${!DOC[@]}"; then
    cmd=$1
    args=${@:2}
    ${cmd} ${args}
else
    help
fi
