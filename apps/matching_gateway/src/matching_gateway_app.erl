%%%-------------------------------------------------------------------
%% @doc matching_gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(matching_gateway_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    matching_gateway_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
