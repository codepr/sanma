%% ============================================================================
%% FIX PROTOCOL SERVER
%% Implements FIX 4.4 protocol
%% ============================================================================
-module(fix_gateway).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DEFAULT_PORT, 8081).
-define(SUBMIT_TIMEOUT, 5000).

-define(FIX_LOGON, "A").
-define(FIX_LOGOUT, "5").
-define(FIX_HEARTBEAT, "0").
-define(FIX_NEW_ORDER_SINGLE, "D").
-define(FIX_ORDER_CANCEL_REQUEST, "F").
-define(FIX_EXECUTION_REPORT, "8").
-define(FIX_ORDER_CANCEL_REJECT, "9").

%% FIX Tags
-define(TAG_BEGIN_STRING, "8").
-define(TAG_BODY_LENGTH, "9").
-define(TAG_MSG_TYPE, "35").
-define(TAG_SENDER_COMP_ID, "49").
-define(TAG_TARGET_COMP_ID, "56").
-define(TAG_MSG_SEQ_NUM, "34").
-define(TAG_SENDING_TIME, "52").
-define(TAG_CHECKSUM, "10").

%% Order tags
-define(TAG_CL_ORD_ID, "11").

-record(state, {
    socket :: gen_tcp:socket(),
    transport = tcp :: tcp | ssl,
    sender_comp_id :: binary(),
    target_comp_id :: binary(),
    session_id :: binary(),
    account_id :: binary(),
    msg_seq_num_in = 1 :: integer(),
    msg_seq_num_out = 1 :: integer(),
    authenticated = false :: boolean(),
    heartbeat_timer :: reference(),
    buffer = <<>> :: binary()
}).

%% ============================================================================
%% API Functions
%% ============================================================================

start_link() ->
    start_link(?DEFAULT_PORT).

start_link(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}, {keepalive, true}
    ]),
    logger:info("Listening on port: ~p", [Port]),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, Pid} = gen_server:start(?MODULE, [Socket, tcp], []),
    gen_tcp:controlling_process(Socket, Pid),
    gen_server:cast(Pid, activate),
    accept_loop(ListenSocket).

%% ============================================================================
%% gen_server Callbacks
%% ============================================================================

init([Socket, Transport]) ->
    {ok, #state{socket = Socket, transport = Transport}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(activate, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, #state{socket = Socket, buffer = Buffer} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,

    case parse_fix_messages(NewBuffer) of
        {Messages, Remaining} ->
            NewState = lists:foldl(
                fun process_fix_message/2, State#state{buffer = Remaining}, Messages
            ),
            inet:setopts(Socket, [{active, once}]),
            {noreply, NewState};
        incomplete ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{buffer = NewBuffer}}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    logger:info("Client disconnected"),
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    logger:info("Socket error ~p", [Reason]),
    {stop, Reason, State};
handle_info({heartbeat}, State) ->
    send_heartbeat(State),
    Timer = erlang:send_after(30000, self(), {heartbeat}),
    {noreply, State#state{heartbeat_timer = Timer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

%% ============================================================================
%% FIX Message Parsing
%% ============================================================================

%% FIX messages are delimited by SOH (0x01) characters
parse_fix_messages(Data) ->
    case binary:split(Data, <<1, "10=">>) of
        [_] -> incomplete;
        Parts -> parse_messages_loop(Parts, [])
    end.

parse_messages_loop([Last], Acc) ->
    {lists:reverse(Acc), Last};
parse_messages_loop([Part, Next | Rest], Acc) ->
    % Extract checksum which is 3 digits after tag "10="
    case Next of
        <<Checksum:3/binary, 1, Remaining/binary>> ->
            FullMessage = <<Part/binary, 1, "10=", Checksum/binary, 1>>,
            Message = parse_fix_message(FullMessage),
            parse_messages_loop([Remaining | Rest], [Message | Acc]);
        _ ->
            parse_messages_loop([Next | Rest], Acc)
    end.

parse_fix_message(Binary) ->
    Fields = binary:split(Binary, <<1>>, [global]),
    lists:foldl(
        fun(Field, Acc) ->
            case binary:split(Field, <<"=">>) of
                [Tag, Value] -> maps:put(Tag, Value, Acc);
                _ -> Acc
            end
        end,
        #{},
        Fields
    ).

process_fix_message(Message, State) ->
    MsgType = maps:get(<<"35">>, Message, undefined),
    process(MsgType, Message, State).

% LOGON message
process(?FIX_LOGON, Message, State) ->
    SenderCompId = maps:get(<<"49">>, Message),
    TargetCompId = maps:get(<<"56">>, Message),

    case authenticate_session(SenderCompId) of
        {ok, AccountId} ->
            logger:info("FIX Logon: ~s", [SenderCompId]),

            send_logon_response(State),

            Timer = erlang:send_after(30000, self(), {heartbeat}),

            State#state{sender_comp_id = SenderCompId,
                        target_comp_id = TargetCompId,
                        account_id = AccountId,
                        authenticated = true,
                        heartbeat_timer = Timer };
        {error, _Reason} ->
            send_logout(<<"Authentication failed">>, State),
            State
    end;

% New Order - Single
process(?FIX_NEW_ORDER_SINGLE, Message, #state{authenticated = true} = State) ->
    ClOrdId = maps:get(<<"11">>, Message),
    Symbol = maps:get(<<"55">>, Message),
    SideRaw = maps:get(<<"54">>, Message),
    OrderQty = binary_to_integer(maps:get(<<"38">>, Message)),
    OrdType = maps:get(<<"40">>, Message),
    Price = case OrdType of
      <<"2">> -> binary_to_integer(maps:get(<<"44">>, Message));
      _ -> 0
            end,
    Side = case SideRaw of
      <<"1">> -> buy;
    <<"2">> -> sell
           end,

    Order = #{client_order_id => ClOrdId, symbol => Symbol, side => Side, quantity => OrderQty, price => Price, account_id => State#state.account_id },

    case submit_order(State#state.session_id, Order) of
        {ok, _OrderId} ->
            % TODO send a new execution report here
            State;
        {error, Reason} ->
            send_order_cancel_reject(ClOrdId, Reason, State),
            State
    end;

process(?FIX_HEARTBEAT, _Message, State) -> State;
process(?FIX_LOGOUT, _Message, State) -> send_logout(<<"Logout acknowledged">>, State), State;
process(_Unknown, _Message, State) -> State.

%% ============================================================================
%% Internal utilities
%% ============================================================================

build_fix_message(MsgType, Fields, State) ->
    SendingTime = format_fix_timestamp(erlang:system_time(millisecond)),

    %% Build body
    Body = [
        {?TAG_MSG_TYPE, MsgType},
        {?TAG_SENDER_COMP_ID, State#state.target_comp_id},
        {?TAG_TARGET_COMP_ID, State#state.sender_comp_id},
        {?TAG_MSG_SEQ_NUM, integer_to_binary(State#state.msg_seq_num_out)},
        {?TAG_SENDING_TIME, SendingTime}
        | Fields
    ],

    %% Convert to FIX format
    BodyBinary = lists:foldl(fun({Tag, Value}, Acc) ->
        <<Acc/binary, Tag/binary, "=", Value/binary, 1>>
    end, <<>>, Body),

    %% Calculate checksum
    BodyLength = integer_to_binary(byte_size(BodyBinary)),
    Header = <<?TAG_BEGIN_STRING/binary, "=FIX.4.4", 1,
               ?TAG_BODY_LENGTH/binary, "=", BodyLength/binary, 1>>,

    MessageWithoutChecksum = <<Header/binary, BodyBinary/binary>>,
    Checksum = calculate_checksum(MessageWithoutChecksum),

    <<MessageWithoutChecksum/binary,
      ?TAG_CHECKSUM/binary, "=", Checksum/binary, 1>>.

calculate_checksum(Message) ->
    Sum = lists:foldl(fun(Byte, Acc) -> Acc + Byte end, 0, binary_to_list(Message)),
    ChecksumValue = Sum rem 256,
    io_lib:format("~3..0B", [ChecksumValue]).

format_fix_timestamp(Milliseconds) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Milliseconds, millisecond),
    Ms = Milliseconds rem 1000,
    io_lib:format("~4..0B~2..0B~2..0B-~2..0B:~2..0B:~2..0B.~3..0B",
                  [Year, Month, Day, Hour, Minute, Second, Ms]).

send_fix_message(Message, State) ->
    gen_tcp:send(State#state.socket, Message),
    State#state{msg_seq_num_out = State#state.msg_seq_num_out + 1}.

send_heartbeat(State) ->
    send_heartbeat(<<>>, State).

send_heartbeat(TestReqId, State) ->
    Fields = case TestReqId of
        <<>> -> [];
        _ -> [{<<"112">>, TestReqId}]  % TestReqID
    end,
    Message = build_fix_message(?FIX_HEARTBEAT, Fields, State),
    send_fix_message(Message, State).

send_logon_response(State) ->
    Message = build_fix_message(?FIX_LOGON, [ {<<"98">>, <<"0">>}, {<<"108">>, <<"30">>}], State), send_fix_message(Message, State).

send_logout(Text, State) ->
    Message = build_fix_message(?FIX_LOGOUT, [{<<"58">>, Text}], State), send_fix_message(Message, State).

send_order_cancel_reject(ClOrdId, Reason, State) ->
    Message = build_fix_message(?FIX_ORDER_CANCEL_REJECT, [
        {?TAG_CL_ORD_ID, ClOrdId},
        {<<"102">>, <<"1">>},  % CxlRejReason=Unknown order
        {<<"58">>, atom_to_binary(Reason)}  % Text
    ], State),
    send_fix_message(Message, State).

authenticate_session(SenderCompId) ->
    %% TODO Validate SenderCompId against database
    %% TODO check credentials
    {ok, <<"account_", SenderCompId/binary>>}.

submit_order(_SessionId, Order) ->
    #{side := Side, price := Price, quantity := Quantity} = Order,
    gen_server:call({global, matching_gateway}, {submit_order, Side, Price, Quantity}, ?SUBMIT_TIMEOUT).

