-module(ping_handler).

-export([init/2, allowed_methods/2]).

init(Req, Opts) ->
    Reply = cowboy_req:reply(200, #{
        <<"Content-Type">> => <<"text/plain">>
    }, <<"Hello world!">>, Req),
    {ok, Reply, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.
