-module(chat_client_handler).

-behavior(cowboy_websocket).

% cowboy_websocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(chat_client_handler_state, {
    protocol_handler=none :: module(),
    protocol_handler_state=none :: any()
}).

% cowboy_websocket callbacks

-spec init(cowboy_req:req(), any()) -> {cowboy_websocket, cowboy_req:req(), any()}.
init(Req, Params) ->
    error_logger:info_msg("chat_client_handler handling new connection", []),
    {cowboy_websocket, Req, Params}.

-spec websocket_init(any()) -> {ok, #chat_client_handler_state{}}.
websocket_init(Params) ->
    error_logger:info_msg("chat_client_handler initializing websocket connection", []),
    {ok, initial_state(Params)}.

-spec websocket_handle({atom(), binary()}, #chat_client_handler_state{}) -> {ok, #chat_client_handler_state{}} | {reply, iodata(), #chat_client_handler_state{}}.
websocket_handle({text, Message}, #chat_client_handler_state{protocol_handler=none}=State) ->
    Decoded = (catch jiffy:decode(Message)),
    case extract_client_version(Decoded) of
        <<"1.0">> ->
            error_logger:info_msg("chat_client_handler starting chat_protocol_v1 conversation", []),
            ProtocolState = chat_protocol_v1:init(),
            NewState = State#chat_client_handler_state{protocol_handler=chat_protocol_v1, protocol_handler_state=ProtocolState},
            {reply, server_hello(<<"1.0">>), NewState};
        error ->
            error_logger:info_msg("chat_client_handler received bad message from client when expecting client_hello:~n~p", [Message]),
            {reply, [protocol_error(<<"CHAT-001">>, <<"A client_hello message was expected">>), close], State};
        UnsupportedVersion ->
            error_logger:info_msg("chat_client_handler received unsupported version in client_hello: ~p", [UnsupportedVersion]),
            {reply, [protocol_error(<<"CHAT-002">>, <<"An unsupported protocol version was requested: ", UnsupportedVersion/binary>>), close], State}
    end;
websocket_handle({text, Message}, #chat_client_handler_state{protocol_handler=Handler, protocol_handler_state=HandlerState}=State) ->
    Decoded = (catch jiffy:decode(Message)),
    protocol_handler_result(Handler:handle_client_message(Decoded, HandlerState), State);
websocket_handle(ping, #chat_client_handler_state{}=State) ->
    {ok, State};
websocket_handle(pong, #chat_client_handler_state{}=State) ->
    {ok, State};
websocket_handle({ping, _Message}, #chat_client_handler_state{}=State) ->
    {ok, State};
websocket_handle({pong, _Message}, #chat_client_handler_state{}=State) ->
    {ok, State};
websocket_handle(Any, #chat_client_handler_state{}=State) ->
    error_logger:info_msg("chat_client_handler received unexpected message type from client:~p~n", [Any]),
    {reply, [protocol_error(<<"CHAT-003">>, <<"Unexpected message">>), close], State}.

-spec websocket_info(any(), #chat_client_handler_state{}) -> {ok, #chat_client_handler_state{}}.
websocket_info(Message, #chat_client_handler_state{protocol_handler=none}=State) ->
    error_logger:warning_msg("chat_client_handler received unexpected info: ~p", [Message]),
    {ok, State};
websocket_info(Message, #chat_client_handler_state{protocol_handler=Handler, protocol_handler_state=HandlerState}=State) ->
    protocol_handler_result(Handler:handle_internal_message(Message, HandlerState), State).

-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
    error_logger:info_msg("chat_client_handler terminating because: ~p", [_Reason]),
    ok.

% private functions

-spec initial_state(any()) -> #chat_client_handler_state{}.
initial_state(_Params) ->
    #chat_client_handler_state{}.

-spec extract_client_version(any()) -> binary() | error.
extract_client_version({[{<<"record">>, <<"client_hello">>}, {<<"protocol_version">>, Version}]}) ->
    Version;
extract_client_version(_Other) ->
    error.

-spec server_hello(binary()) -> iodata().
server_hello(Version) ->
    {text, jiffy:encode({[{<<"record">>, <<"server_hello">>}, {<<"protocol_version">>, Version}]})}.

-spec protocol_handler_result({ok, any()} | {reply, ws_cow:frame(), any()} | {reply, [ws_cow:frame()], any()} | {stop, any()}, #chat_client_handler_state{}) ->
    {ok, #chat_client_handler_state{}} | {reply, ws_cow:frame(), #chat_client_handler_state{}} |
    {reply, [ws_cow:frame()], #chat_client_handler_state{}} | {stop, #chat_client_handler_state{}}.
protocol_handler_result({ok, NewHandlerState}, #chat_client_handler_state{}=State) ->
    {ok, State#chat_client_handler_state{protocol_handler_state=NewHandlerState}};
protocol_handler_result({reply, ReplyOrReplies, NewHandlerState}, #chat_client_handler_state{}=State) ->
    {reply, ReplyOrReplies, State#chat_client_handler_state{protocol_handler_state=NewHandlerState}};
protocol_handler_result({stop, NewHandlerState}, #chat_client_handler_state{}=State) ->
    {stop, State#chat_client_handler_state{protocol_handler_state=NewHandlerState}}.

-spec protocol_error(binary(), binary()) -> iodata().
protocol_error(Code, Reason) ->
    {text, jiffy:encode({[{<<"record">>, <<"protocol_error">>}, {<<"code">>, Code}, {<<"reason">>, Reason}]})}.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

is_protocol_error(Data) ->
    case jiffy:decode(Data) of
        {[{<<"record">>, <<"protocol_error">>} | _Rest]} -> true;
        _Else -> false
    end.

handle_websocket_handles_unexpected_message_test() ->
    State = initial_state(fake_params),
    {reply, [{text, ProtocolError}, close], State} = websocket_handle({binary, <<"blah blah tele blah">>}, State),
    true = is_protocol_error(ProtocolError).

websocket_handle_ping_test() ->
    State = initial_state(fake_params),
    {ok, State} = websocket_handle(ping, State).

websocket_handle_pong_test() ->
    State = initial_state(fake_params),
    {ok, State} = websocket_handle(pong, State).

websocket_handle_ping_with_data_test() ->
    State = initial_state(fake_params),
    {ok, State} = websocket_handle({ping, fake_data}, State).

websocket_handle_pong_with_data_test() ->
    State = initial_state(fake_params),
    {ok, State} = websocket_handle({pong, fake_data}, State).

handle_websocket_initial_client_hello_v1_0_initializes_protocol_handler_test() ->
    State = initial_state(fake_params),
    {reply, _Reply, #chat_client_handler_state{protocol_handler=ProtoHandler, protocol_handler_state=ProtoState}} =
        websocket_handle({text, <<"{\"record\":\"client_hello\", \"protocol_version\":\"1.0\"}">>}, State),
    chat_protocol_v1 = ProtoHandler,
    ProtoState = chat_protocol_v1:init().

handle_websocket_initial_client_hello_v1_0_replies_server_hello_test() ->
    State = initial_state(fake_params),
    {reply, Reply, _NewState} =
        websocket_handle({text, <<"{\"record\":\"client_hello\", \"protocol_version\":\"1.0\"}">>}, State),
    {text, <<"{\"record\":\"server_hello\",\"protocol_version\":\"1.0\"}">>} = Reply.

handle_websocket_initial_client_hello_unexpected_version_test() ->
    State = initial_state(fake_params),
    {reply, [{text, ProtocolError}, close], State} =
        websocket_handle({text, <<"{\"record\":\"client_hello\", \"protocol_version\":\"42\"}">>}, State),
    true = is_protocol_error(ProtocolError).

handle_websocket_initial_not_client_hello_test() ->
    State = initial_state(fake_params),
    {reply, [{text, ProtocolError}, close], State} =
        websocket_handle({text, <<"{\"foo\":\"bar\"}">>}, State),
    true = is_protocol_error(ProtocolError).

handle_websocket_initial_bad_json_test() ->
    State = initial_state(fake_params),
    {reply, [{text, ProtocolError}, close], State} =
        websocket_handle({text, <<"not json">>}, State),
    true = is_protocol_error(ProtocolError).

handle_websocket_forwards_client_messages_to_protocol_handler_test() ->
    Mock = em:new(),
    em:strict(Mock, fake_chat_protocol, handle_client_message, [{[{<<"foo">>, <<"bar">>}]}, fake_protocol_state], {return, {ok, new_fake_protocol_state}}),
    em:replay(Mock),
    State = #chat_client_handler_state{protocol_handler=fake_chat_protocol, protocol_handler_state=fake_protocol_state},
    {ok, #chat_client_handler_state{protocol_handler_state=NewProtocolState}} = websocket_handle({text, <<"{\"foo\":\"bar\"}">>}, State),
    new_fake_protocol_state = NewProtocolState,
    em:verify(Mock).

handle_websocket_handles_replies_from_protocol_handler_test() ->
    Mock = em:new(),
    em:strict(Mock, fake_chat_protocol, handle_client_message, [{[{<<"foo">>, <<"bar">>}]}, fake_protocol_state], {return, {reply, fake_reply, new_fake_protocol_state}}),
    em:replay(Mock),
    State = #chat_client_handler_state{protocol_handler=fake_chat_protocol, protocol_handler_state=fake_protocol_state},
    {reply, fake_reply, #chat_client_handler_state{protocol_handler_state=NewProtocolState}} = websocket_handle({text, <<"{\"foo\":\"bar\"}">>}, State),
    new_fake_protocol_state = NewProtocolState,
    em:verify(Mock).

handle_websocket_handles_stop_from_protocol_handler_test() ->
    Mock = em:new(),
    em:strict(Mock, fake_chat_protocol, handle_client_message, [{[{<<"foo">>, <<"bar">>}]}, fake_protocol_state], {return, {stop, new_fake_protocol_state}}),
    em:replay(Mock),
    State = #chat_client_handler_state{protocol_handler=fake_chat_protocol, protocol_handler_state=fake_protocol_state},
    {stop, #chat_client_handler_state{protocol_handler_state=NewProtocolState}} = websocket_handle({text, <<"{\"foo\":\"bar\"}">>}, State),
    new_fake_protocol_state = NewProtocolState,
    em:verify(Mock).

handle_info_ignored_when_no_protocol_test() ->
    State = initial_state(fake_params),
    {ok, State} = websocket_info(fake_message, State).

handle_info_fowards_messages_to_protocol_handler_when_authenticated_test() ->
    Mock = em:new(),
    em:strict(Mock, fake_chat_protocol, handle_internal_message, [fake_message, fake_protocol_state], {return, {ok, new_fake_protocol_state}}),
    em:replay(Mock),
    State = #chat_client_handler_state{protocol_handler=fake_chat_protocol, protocol_handler_state=fake_protocol_state},
    {ok, #chat_client_handler_state{protocol_handler_state=NewProtocolState}} = websocket_info(fake_message, State),
    new_fake_protocol_state = NewProtocolState,
    em:verify(Mock).

handle_info_handles_replies_from_protocol_handler_test() ->
    Mock = em:new(),
    em:strict(Mock, fake_chat_protocol, handle_internal_message, [fake_message, fake_protocol_state], {return, {reply, fake_reply, new_fake_protocol_state}}),
    em:replay(Mock),
    State = #chat_client_handler_state{protocol_handler=fake_chat_protocol, protocol_handler_state=fake_protocol_state},
    {reply, fake_reply, #chat_client_handler_state{protocol_handler_state=NewProtocolState}} = websocket_info(fake_message, State),
    new_fake_protocol_state = NewProtocolState,
    em:verify(Mock).

handle_info_handles_stop_from_protocol_handler_test() ->
    Mock = em:new(),
    em:strict(Mock, fake_chat_protocol, handle_internal_message, [fake_message, fake_protocol_state], {return, {stop, new_fake_protocol_state}}),
    em:replay(Mock),
    State = #chat_client_handler_state{protocol_handler=fake_chat_protocol, protocol_handler_state=fake_protocol_state},
    {stop, #chat_client_handler_state{protocol_handler_state=NewProtocolState}} = websocket_info(fake_message, State),
    new_fake_protocol_state = NewProtocolState,
    em:verify(Mock).

-endif.
