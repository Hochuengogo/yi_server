%%%-------------------------------------------------------------------
%% @doc yi_server public API
%% @end
%%%-------------------------------------------------------------------

-module(yi_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, SupPid} = yi_server_sup:start_link(),
    manage:start(srv_config:get(server_type)),
    {ok, SupPid}.

stop(_State) ->
    ok.
