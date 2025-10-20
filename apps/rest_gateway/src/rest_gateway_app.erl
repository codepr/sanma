%%%-------------------------------------------------------------------
%% @doc matching_gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(rest_gateway_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
    {'_', [
        {"/", ping_handler, []},
        {"/orders", order_handler, #{}}
    ]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }).

stop(_State) ->
    ok.

%% internal functions
