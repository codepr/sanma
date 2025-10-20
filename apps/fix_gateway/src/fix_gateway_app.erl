%%%-------------------------------------------------------------------
%% @doc fix_gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(fix_gateway_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fix_gateway_sup:start_link().

stop(_State) ->
    ok.
