# coding=utf-8

import re
import sys


# 模板文件
tpl_file = "./tpl/logs_lib.tpl"
# 输出文件
out_file = "./src/lib/logs_lib.erl"
# 日志等级列表
log_list = ["debug", "info", "warn", "error"]


# 从配置文件中读取日志等级
def get_level(log_line):
    if log_line != "":
        pat = "(?<=(^\{log_level,)).+(?=(\}.+))"
        match_ret = re.search(pat, log_line)
        if match_ret is not None:
            return str.strip(match_ret.group())
        else:
            return ""
    else:
        return ""


# 替换字符
def replace_str(log_level):
    if log_level in log_list:
        read_fd = open(tpl_file, "r")
        content = read_fd.read()
        read_fd.close()
        content = content.replace("{log_level}", log_level)
        write_fd = open(out_file, "w")
        write_fd.write(content)
        write_fd.close()
        print "日志等级:" + log_level
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == '__main__':
    line_str = str(sys.argv[1])
    level_str = get_level(line_str)
    replace_str(level_str)
