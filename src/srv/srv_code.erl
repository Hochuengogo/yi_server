%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 处理服务器代码加载
%%% @end
%%%-------------------------------------------------------------------
-module(srv_code).

-behaviour(gen_server).

-export([
    hot_update/0
]).

-export([start_link/0, info/1, cast/1, call/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-include("logs.hrl").

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
%% @doc 热更新
-spec hot_update() -> ok.
hot_update() ->
    info(hot_update).

info(Info) ->
    ?SERVER ! Info.

cast(Info) ->
    gen_server:cast(?SERVER, Info).

call(Info) ->
    ?scall(?SERVER, Info).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?info(?start_begin),
    process_flag(trap_exit, true),
    ets:new(srv_code, [named_table, protected, set, {keypos, 1}]),
    init_srv_code(),
    ?info(?start_end),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(hot_update, State) ->
    io:format("开始热更新服务器代码\n"),
    do_load(hard),
    io:format("热更新服务器代码完成\n"),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?info(?stop_begin),
    ?info(?stop_end),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 初始化服务器代码
init_srv_code() ->
    {EbinPath, EbinFileNames} = get_srv_ebin_info(),
    init_srv_code(EbinFileNames, EbinPath).

init_srv_code([], _EbinPath) ->
    ok;
init_srv_code([FileName | EbinFileNames], EbinPath) ->
    BeamPath = filename:join(EbinPath, FileName),
    case beam_md5_hex_string(BeamPath) of
        {ok, Mod, Md5HexString} ->
            ets:insert(srv_code, {Mod, Md5HexString});
        _ ->
            ok
    end,
    init_srv_code(EbinFileNames, EbinPath).

do_load(Flag) ->
    {EbinPath, EbinFileNames} = get_srv_ebin_info(),
%%    ?debug("ebin路径：~ts，ebin路径下的文件：~w", [EbinPath, EbinFileNames]),
    do_load(EbinFileNames, EbinPath, Flag),
    ok.

do_load([], _EbinPath, _Flag) ->
    ok;
do_load([FileName | EbinFileNames], EbinPath, Flag) ->
    BeamPath = filename:join(EbinPath, FileName),
    case beam_md5_hex_string(BeamPath) of
        {ok, Mod, Md5HexString} ->
            case ets:lookup(srv_code, Mod) of
                [{_, Md5HexString}] ->
                    skip;
                _ ->
                    case code:is_loaded(Mod) of
                        {file, BeamPath} ->
                            code:purge(Mod),
                            case code:load_file(Mod) of
                                {module, Mod} ->
                                    ets:insert(srv_code, {Mod, Md5HexString}),
                                    io:format("热更新服务器代码成功[~w]\n", [Mod]);
                                {error, Reason} ->
                                    io:format("热更新服务器失败[~w]，原因：~w\n", [Mod, Reason])
                            end;
                        false ->
                            case code:load_file(Mod) of
                                {module, Mod} ->
                                    ets:insert(srv_code, {Mod, Md5HexString}),
                                    io:format("热更新服务器代码成功[~w]\n", [Mod]);
                                {error, Reason} ->
                                    io:format("热更新服务器失败[~w]，原因：~w\n", [Mod, Reason])
                            end;
                        _ ->
                            io:format("热更新服务器失败[~w]，原因：~w\n", [Mod, same_mod_loaded])
                    end
            end;
        _ ->
            skip
    end,
    do_load(EbinFileNames, EbinPath, Flag).

%% 获取服务器ebin信息
get_srv_ebin_info() ->
    CodePath = srv_config:get(code_path),
    EbinPath = filename:join(CodePath, "ebin"),
    EbinFileNames = filelib:wildcard("*.beam", EbinPath),
    {EbinPath, EbinFileNames}.

%% beam文件MD5加密16进制字符串
beam_md5_hex_string(BeamPath) ->
    case beam_lib:md5(BeamPath) of
        {ok, {Mod, Md5}} ->
            Md5HexString = crypto_util:hex_string(Md5),
            {ok, Mod, Md5HexString};
        {error, beam_lib, Reason} ->
            ?error("生成beam文件md5加密错误，原因：~w", [Reason]),
            false;
        {'EXIT', Reason} ->
            ?error("生成beam文件md5加密错误，原因：~w", [Reason]),
            false
    end.