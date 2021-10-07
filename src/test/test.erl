%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 测试模块
%%% @end
%%% Created : 2021-10-06 22:37:29
%%%-------------------------------------------------------------------
-module(test).
-author("jiaoyinyi").

%% API
-export([parse_name/0]).

-include("common.hrl").
-include("logs.hrl").

%% 解析名字
parse_name() ->
    {ok, Fd} = file:open(filename:join(srv_lib:code_path(), "ext/name.csv"), [read, {encoding, utf8}]),
    {LastNames, MaleNames, FemaleNames} = parse_name2(Fd, [], [], []),
    file:close(Fd),
    ModStr = "-module(name_data).\n",
    ExportStr = "-export([list/1]).\n\n",
    LastNameContent = io_lib:format("list(last) ->\n    ~w;\n", [LastNames]),
    MaleNameContent = io_lib:format("list(male) ->\n    ~w;\n", [MaleNames]),
    FemaleNameContent = io_lib:format("list(female) ->\n    ~w.\n", [FemaleNames]),
    Content = ModStr ++ ExportStr ++ LastNameContent ++ MaleNameContent ++ FemaleNameContent,
    file:write_file(filename:join(srv_lib:code_path(), "src/data/name_data.erl"), Content, [{encoding, utf8}]).

parse_name2(Fd, LastNames, MaleNames, FemaleNames) ->
    case io:get_line(Fd, "") of
        eof ->
            {LastNames, MaleNames, FemaleNames};
        {error, _Err} ->
            {LastNames, MaleNames, FemaleNames};
        Line ->
            case string:split(Line, ",", all) of
                [LastName, MaleName, FemaleName | _] ->
                    NewLastNames = ?if_true(LastName =/= "", [?str(LastName) | LastNames], LastNames),
                    NewMaleNames = ?if_true(MaleName =/= "", [?str(MaleName) | MaleNames], MaleNames),
                    NewFemaleNames = ?if_true(FemaleName =/= "", [?str(FemaleName) | FemaleNames], FemaleNames),
                    parse_name2(Fd, NewLastNames, NewMaleNames, NewFemaleNames);
                _ ->
                    parse_name2(Fd, LastNames, MaleNames, FemaleNames)
            end
    end.