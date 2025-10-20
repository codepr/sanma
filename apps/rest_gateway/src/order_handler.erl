-module(order_handler).

-export([init/2, allowed_methods/2, submit_order/5]).

-import(handler_helper, [read_body/2, reply_json/3]).

-define(SUBMIT_TIMEOUT, 5000).

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

init(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),

    case read_body(Body, Req1) of
        {ok, Input, Req2} ->
            %% TODO handle errors
            OrderMap = jiffy:decode(Input, [return_maps]),
            #{<<"side">> := SideStr, <<"price">> := Price, <<"quantity">> := Quantity} = OrderMap,
            Side = case SideStr of
                <<"buy">> -> buy;
                <<"sell">> -> sell
            end,
            submit_order(Side, Price, Quantity, Req2, State);

        {error, missing_body, Req3} ->
            Reply = reply_json(400, #{error => <<"empty body">>}, Req3),
            {ok, Reply, State};

        {error, bad_request, Req4} ->
            Reply = reply_json(400, #{error => <<"bad request">>}, Req4),
            {ok, Reply, State}
    end.

submit_order(Side, Price, Quantity, Req, State) ->
    case gen_server:call({global, matching_gateway}, {submit_order, Side, Price, Quantity}, ?SUBMIT_TIMEOUT) of
        {ok, OrderId} ->
            io:format("Submitted buy order: ~p~n", [OrderId]),
            Reply = reply_json(200, #{order_id => OrderId}, Req),
            {ok, Reply, State};
        {error, _} ->
            Reply = reply_json(422, #{}, Req),
            {ok, Reply, State}
    end.
