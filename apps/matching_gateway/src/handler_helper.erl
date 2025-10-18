-module(handler_helper).

-export([read_body/2, reply_json/3]).

read_body(RawBody, Req) ->
    case RawBody of
        [{Input, true}] ->
            {ok, Input, Req};
        [] ->
            {error, missing_body, Req};
        _ ->
            {error, bad_request, Req}
    end.

reply_json(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"Content-Type">> => <<"application/json">>}, jiffy:encode(Body), Req).
