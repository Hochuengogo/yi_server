%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 角色名字模块
%%% @end
%%% Created : 2021-10-06 19:56:45
%%%-------------------------------------------------------------------
-module(role_name).
-author("jiaoyinyi").

%% API
-export([rand/1]).

-include("common.hrl").
-include("logs.hrl").
-include("role.hrl").

%% @doc 随机名字
-spec rand(?role_sex_male | ?role_sex_female) -> {ok, binary()} | false.
rand(Sex) ->
    rand(Sex, 5).
rand(_Sex, Time) when Time =< 0 ->
    false;
rand(Sex, Time) ->
    Name = do_rand(Sex),
    case avail(Name) of
        true ->
            {ok, Name};
        _ ->
            rand(Sex, Time - 1)
    end.

do_rand(?role_sex_male) ->
    <<(list_util:rand(name_data:list(last)))/binary, (list_util:rand(name_data:list(male)))/binary>>;
do_rand(?role_sex_female) ->
    <<(list_util:rand(name_data:list(last)))/binary, (list_util:rand(name_data:list(female)))/binary>>.

%% 判断名字是否可使用
avail(Name) ->
    case db:query(<<"SELECT COUNT(*) FROM role_base WHERE name=?;">>, [Name], 5000) of
        {ok, [0]} ->
            true;
        {ok, [_Num]} ->
            false;
        {error, Err} ->
            {error, Err}
    end.