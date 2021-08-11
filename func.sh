#!/usr/bin/env bash
# 通用方法

# 带颜色输出
# color red hello_world
function color() {
    case $1 in
    "black") printf "\033[30m$2\033[0m" ;;
    "red") printf "\033[31m$2\033[0m" ;;
    "green") printf "\033[32m$2\033[0m" ;;
    "yellow") printf "\033[33m$2\033[0m" ;;
    "blue") printf "\033[34m$2\033[0m" ;;
    "purple") printf "\033[35m$2\033[0m" ;;
    "sky_blue") printf "\033[36m$2\033[0m" ;;
    "white") printf "\033[37m$2\033[0m" ;;
    *) printf "$2" ;;
    esac
}

# debug输出
# print_debug "hello_world!"
# print_debug "%s%s" "hello_world!" "123"
function print_debug() {
    if [[ $# -gt 1 ]]; then
        printf "%-17s%s\n" "$(color sky_blue "[DEBUG]")" "$(color sky_blue "$(printf "$1" "${@:2}")")"
    else
        printf "%-17s%s\n" "$(color sky_blue "[DEBUG]")" "$(color sky_blue "$1")"
    fi
}

# info输出
# print_info "hello_world!"
# print_info "%s%s" "hello_world!" "123"
function print_info() {
    if [[ $# -gt 1 ]]; then
        printf "%-17s%s\n" "$(color green "[INFO]")" "$(color green "$(printf "$1" "${@:2}")")"
    else
        printf "%-17s%s\n" "$(color green "[INFO]")" "$(color green "$1")"
    fi
}

# warn输出
# print_warn "hello_world!"
# print_warn "%s%s" "hello_world!" "123"
function print_warn() {
    if [[ $# -gt 1 ]]; then
        printf "%-17s%s\n" "$(color yellow "[WARN]")" "$(color yellow "$(printf "$1" "${@:2}")")"
    else
        printf "%-17s%s\n" "$(color yellow "[WARN]")" "$(color yellow "$1")"
    fi
}

# error输出
# print_error "hello_world!"
# print_error "%s%s" "hello_world!" "123"
function print_error() {
    if [[ $# -gt 1 ]]; then
        printf "%-17s%s\n" "$(color red "[ERROR]")" "$(color red "$(printf "$1" "${@:2}")")"
    else
        printf "%-17s%s\n" "$(color red "[ERROR]")" "$(color red "$1")"
    fi
}

# 是否在数组中
# member 1 ("1" "2" "3")
function member() {
    local array=${@:2}
    for val in ${array[@]}; do
        if [[ "$1" == "$val" ]]; then
            return 0
        fi
    done
    return 1
}

# 判断参数是否为空，为空则提示输入
# check_empty "请输入：" $1
function check_empty() {
    if [[ -z $2 ]]; then
        local enter
        read -p $1 enter
        echo $enter
    fi
    echo $2
}

# 判断是否为整型
# is_int 100
function is_int() {
    local re="^[0-9]+$"
    [[ $1 =~ $re ]]
    return $?
}

# 获取脚本真实路径，如果有链接，会取到源文件路径
# get_real_path
function get_real_path() {
#    local tar_file=${BASH_SOURCE[0]}
    local tar_file=$0
    cd $(dirname $tar_file})
    tar_file=$(basename $tar_file)
    while [ -L "$tar_file" ]; do
        tar_file=$(readlink $tar_file)
        cd $(dirname $tar_file)
        tar_file=$(basename $tar_file)
    done
    echo "$(pwd -P)/$tar_file"
}

# 获取脚本真实目录，如果有链接，会取到源文件目录
# get_real_dir
function get_real_dir() {
    echo $(dirname $(get_real_path))
}

# 是否有screen会话
# has_screen xxx
function has_screen() {
    [[ -n "$(screen -ls | grep -E "[0-9]+\.$1")" ]]
    return $?
}

# 是否在screen会话中
# in_screen xxx
function in_screen() {
    [[ -n "$(echo $STY | grep -E "[0-9]+\.$1")" ]]
    return $?
}
