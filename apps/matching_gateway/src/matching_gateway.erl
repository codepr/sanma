-module(matching_gateway).
-behaviour(gen_server).

-export([start_link/0, submit_order/4, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Matching engine communication protocol message types
-define(MSG_NEW_ORDER, 1).
-define(MSG_ORDER_ACK, 2).
-define(MSG_ORDER_REJECT, 3).
-define(MSG_TRADE, 4).
-define(MSG_CANCEL_ORDER, 5).

-define(ORDER_BUY, 0).
-define(ORDER_SELL, 1).

-define(ORDER_LIMIT, 0).
-define(ORDER_MARKET, 1).

-record(state, {
                socket :: gen_tcp:socket(),
                pending_orders = #{} :: map(),
                subscribers = [] :: [pid()]
               }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Submit a new order
%% Side: buy | sell
%% Price: integer (represents cents precision)
%% Quantity: integer
submit_order(Side, Price, Quantity, Timeout) ->
    gen_server:call(?MODULE, {submit_order, Side, Price, Quantity}, Timeout).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    SocketPath = "/tmp/matching_engine.sock",
    case gen_tcp:connect({local, SocketPath}, 0, [binary, {packet, 0}, {active, true}]) of
        {ok, Socket} ->
            logger:info("Connected to matching engine"),
            {ok, #state{socket = Socket}};
        {error, Reason} ->
            logger:error("Failed to connect: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call({submit_order, Side, Price, Quantity}, From, State) ->
    SideInt = case Side of
                  buy -> ?ORDER_BUY;
                  sell -> ?ORDER_SELL
              end,

    Header = <<?MSG_NEW_ORDER:8, 0:24/little, 12:32/little>>,

    Body = <<Price:32/little,
             Quantity:32/little,
             SideInt:16/little,
             ?ORDER_LIMIT:16/little>>,

    Message = <<Header/binary, Body/binary>>,

    logger:info("Sending new order message"),

    case gen_tcp:send(State#state.socket, Message) of
        ok ->
            NewPending = maps:put(make_ref(), From, State#state.pending_orders),
            {noreply, State#state{pending_orders = NewPending}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages from the matching engine
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    NewState = process_engine_message(Data, State),
    {noreply, NewState};

handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    logger:info("Connection to matching engine close"),
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, #state{socket = Socket} = State) ->
    logger:error("Socket error: ~p", [Reason]),
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

%% Internal functions

process_engine_message(<<MsgType:8, _pad:24, MsgLen:32/little, Body:MsgLen/binary, Rest/binary>>, State) ->
    NewState = handle_engine_message(MsgType, Body, State),
    case Rest of
        <<>> -> NewState;
        _ -> process_engine_message(Rest, NewState)
    end;
process_engine_message(_, State) ->
    %% Incomplete message, require more data
    State.

handle_engine_message(?MSG_ORDER_ACK, <<OrderId:64/little, Status:8, _pad:56/little>>, State) ->
    logger:info("Order ~p acknowledged (status: ~p):", [OrderId, Status]),

    case maps:size(State#state.pending_orders) of
        0 -> State;
        _ ->
            [Ref | _] = maps:keys(State#state.pending_orders),
            case maps:get(Ref, State#state.pending_orders, undefined) of
                undefined -> State;
                From ->
                    gen_server:reply(From, {ok, OrderId}),
                    State#state{pending_orders = maps:remove(Ref, State#state.pending_orders)}
            end
    end;

handle_engine_message(?MSG_ORDER_REJECT, <<OrderId:64/little, _Status:8>>, State) ->
    logger:info("Order ~p rejected", [OrderId]),

    %% Reply to pending caller
    case maps:size(State#state.pending_orders) of
        0 -> State;
        _ ->
            [Ref | _] = maps:keys(State#state.pending_orders),
            case maps:get(Ref, State#state.pending_orders, undefined) of
                undefined -> State;
                From ->
                    gen_server:reply(From, {error, rejected}),
                    State#state{pending_orders = maps:remove(Ref, State#state.pending_orders)}
            end
    end;

handle_engine_message(?MSG_TRADE, Body, State) ->
    %% Direct struct cast - matches C's Trade struct exactly!
    <<BuyOrderId:64/little,
      SellOrderId:64/little,
      Price:32/little,
      Quantity:32/little,
      BuyerId:32/little,
      SellerId:32/little>> = Body,

    Trade = #{
        buy_order_id => BuyOrderId,
        sell_order_id => SellOrderId,
        price => Price,
        quantity => Quantity,
        buyer_id => BuyerId,
        seller_id => SellerId
    },

    logger:info("TRADE: ~p shares at ~p cents (orders: ~p/~p)",
              [Quantity, Price, BuyOrderId, SellOrderId]),

    %% Broadcast to subscribers (market data feed, risk management, etc.)
    lists:foreach(fun(Pid) -> Pid ! {trade, Trade} end, State#state.subscribers),

    State;

handle_engine_message(MsgType, _Body, State) ->
    logger:info("Unknown message type: ~p~n", [MsgType]),
    State.
