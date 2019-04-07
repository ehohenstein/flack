-module(chat_protocol_v1).

-behavior(chat_protocol).

-include("chat_stream.hrl").

% chat_protocol functions
-export([init/0, handle_client_message/2, handle_internal_message/2]).

-record(chat_protocol_v1_state, {
    status=authenticating :: authenticating | authenticated,
    username=none :: none | binary(),
    user_id=none :: none | binary(),
    chats=sets:new() :: sets:set()
}).

-record(authenticate, {
    username :: binary() | undefined,
    user_id :: binary() | undefined
}).

-record(join_chat, {
    chat_name :: binary()
}).

-record(ping, {
}).

-type protocol_record() :: #authenticate{} | #join_chat{}.

% chat_protocol functions

-spec init() -> #chat_protocol_v1_state{}.
init() ->
    #chat_protocol_v1_state{}.

-spec handle_client_message(tuple(), #chat_protocol_v1_state{}) ->
    {ok, #chat_protocol_v1_state{}} | {reply, cow_ws:frame(), #chat_protocol_v1_state{}} |
    {reply, [cow_ws:frame()], #chat_protocol_v1_state{}} | {stop, #chat_protocol_v1_state{}}.
handle_client_message(Message, #chat_protocol_v1_state{}=State) ->
    case get_record_type(Message) of
        {error, Code, Reason} ->
            {reply, [protocol_error(Code, Reason), close], State};
        Record ->
            dispatch_message(Record, State)
    end.

-spec handle_internal_message(any(), #chat_protocol_v1_state{}) ->
    {ok, #chat_protocol_v1_state{}} | {reply, cow_ws:frame(), #chat_protocol_v1_state{}} |
    {reply, [cow_ws:frame()], #chat_protocol_v1_state{}} | {stop, #chat_protocol_v1_state{}}.
handle_internal_message(#user_joined{chat=Chat, username=JoinedUsername}=Message, #chat_protocol_v1_state{username=Username}=State) ->
    error_logger:info_msg("chat_protocol_v1 notifying user ~p that ~p joined ~p", [Username, JoinedUsername, Chat]),
    {reply, joined(Message), State};
handle_internal_message(#chat_message{chat_name=Chat}=Message, #chat_protocol_v1_state{username=Username}=State) ->
    error_logger:info_msg("chat_protocol_v1 for user ~p forwarding chat message for chat ~p", [Username, Chat]),
    {reply, chat_message(Message), State};
handle_internal_message(Message, #chat_protocol_v1_state{username=Username}=State) ->
    error_logger:warning_msg("chat_protocol_v1 for user ~p received unhandled internal message:~n~p", [Username, Message]),
    {ok, State}.

% private functions

-spec get_record_type(tuple()) -> protocol_record() | {error, binary(), binary()}.
get_record_type({[{<<"record">>, <<"authenticate">>} | Fields]}) ->
    #authenticate{username=proplists:get_value(<<"user_name">>, Fields),
        user_id=proplists:get_value(<<"user_id">>, Fields)};
get_record_type({[{<<"record">>, <<"join_chat">>} | Fields]}) ->
    #join_chat{chat_name=proplists:get_value(<<"chat_name">>, Fields)};
get_record_type({[{<<"record">>, <<"chat_message">>} | Fields]}) ->
    #chat_message{chat_name=proplists:get_value(<<"chat_name">>, Fields),
        mime_type=proplists:get_value(<<"mime_type">>, Fields),
        message=proplists:get_value(<<"message">>, Fields)};
get_record_type({[{<<"record">>, <<"ping">>}]}) ->
    #ping{};
get_record_type(Message) ->
    error_logger:info_msg("unrecognized message typ", [Message]),
    {error, <<"CHAT_PROTOCOL-001">>, <<"Unrecognized message">>}.

-spec dispatch_message(protocol_record(), #chat_protocol_v1_state{}) ->
    {ok, #chat_protocol_v1_state{}} | {reply, cow_ws:frame(), #chat_protocol_v1_state{}} |
    {reply, [cow_ws:frame()], #chat_protocol_v1_state{}} | {stop, #chat_protocol_v1_state{}}.
dispatch_message(#authenticate{}=Message, #chat_protocol_v1_state{username=none}=State) ->
    authenticate(Message, State);
dispatch_message(Message, #chat_protocol_v1_state{username=none}=State) ->
    error_logger:info_msg("chat_protocol_v1 received unexpected ~p message while not yet authenticated", [Message]),
    {reply, [protocol_error(<<"CHAT_PROTOCOL-002">>, <<"Expected authenticate message">>), close], State};
dispatch_message(#authenticate{}, #chat_protocol_v1_state{username=Username}=State) ->
    error_logger:info_msg("chat_protocol_v1 for user ~p received authenticate message while already authenticated", [Username]),
    {reply, [protocol_error(<<"CHAT_PROTOCOL-003">>, <<"Already authenticated">>), close], State};
dispatch_message(#join_chat{}=Message, #chat_protocol_v1_state{}=State) ->
    join_chat(Message, State);
dispatch_message(#chat_message{}=Message, #chat_protocol_v1_state{}=State) ->
    chat_message(Message, State);
dispatch_message(#ping{}, #chat_protocol_v1_state{}=State) ->
    ping(State).

-spec authenticate(#authenticate{}, #chat_protocol_v1_state{}) ->
    {reply, cow_ws:frame(), #chat_protocol_v1_state{}} | {reply, [cow_ws:frame()], #chat_protocol_v1_state{}}.
authenticate(#authenticate{username=undefined}=Message, #chat_protocol_v1_state{}=State) ->
    error_logger:info_msg("chat_protocol_v1 authenticate message missing user_name field:~n~p", [Message]),
    {reply, [protocol_error(<<"CHAT_PROTOCOL-004">>, <<"authenticate message missing user_name field">>), close], State};
authenticate(#authenticate{username=Username, user_id=undefined}, #chat_protocol_v1_state{}=State) ->
    UserID = user_id:generate(),
    error_logger:info_msg("chat_protocol_v1 authenticating ~p with user_id ~p", [Username, UserID]),
    chat_stream:register_user(Username, UserID),
    {reply, authenticated(UserID), State#chat_protocol_v1_state{username=Username, user_id=UserID}};
authenticate(#authenticate{username=Username, user_id=UserID}, #chat_protocol_v1_state{}=State) ->
    error_logger:info_msg("chat_protocol_v1 authenticating ~p with client provided user_id ~p", [Username, UserID]),
    chat_stream:register_user(Username, UserID),
    {reply, authenticated(UserID), State#chat_protocol_v1_state{username=Username, user_id=UserID}}.

-spec join_chat(#join_chat{}, #chat_protocol_v1_state{}) ->
    {reply, cow_ws:frame(), #chat_protocol_v1_state{}} | {reply, [cow_ws:frame()], #chat_protocol_v1_state{}}.
join_chat(#join_chat{chat_name=undefined}=Message, #chat_protocol_v1_state{username=Username}=State) ->
    error_logger:info_msg("chat_protocol_v1 received join_chat from ~p missing chat name:~n~p", [Username, Message]),
    {reply, [protocol_error(<<"CHAT_PROTOCOL-005">>, <<"Missing chat name">>), close], State};
join_chat(#join_chat{chat_name=Chat}, #chat_protocol_v1_state{username=Username, user_id=UserID, chats=Chats}=State) ->
    case sets:is_element(Chat, Chats) of
        true ->
            error_logger:info_msg("chat_protocol_v1 received request to join chat ~p from ~p but already joinged", [Username, Chat]),
            {reply, [protocol_error(<<"CHAT_PROTOCOL-006">>, <<"Chat already joined">>), close], State};
        false ->
            error_logger:info_msg("chat_protocol_v1 joining user ~p to chat ~p", [Username, Chat]),
            Members = chat_stream:join(Chat, Username, UserID),
            NewChats = sets:add_element(Chat, Chats),
            {reply, chat_state(Chat, Members), State#chat_protocol_v1_state{chats=NewChats}}
    end.

-spec chat_message(#chat_message{}, #chat_protocol_v1_state{}) ->
    {reply, cow_ws:frame(), #chat_protocol_v1_state{}} | {reply, [cow_ws:frame()], #chat_protocol_v1_state{}}.
chat_message(#chat_message{chat_name=Chat}=Message, #chat_protocol_v1_state{user_id=UserID, chats=Chats}=State) ->
    case sets:is_element(Chat, Chats) of
        true ->
            chat_stream:post(Message#chat_message{user_id=UserID}),
            {ok, State};
        false ->
            {reply, [protocol_error(<<"CHAT_PROTOCOL-007">>, <<"Chat has not been joined">>), close], State}
    end.

-spec ping(#chat_protocol_v1_state{}) -> {reply, cow_ws:frame(), #chat_protocol_v1_state{}}.
ping(#chat_protocol_v1_state{username=Username}=State) ->
    error_logger:info_msg("chat_protocol_v1 received ping from ~p", [Username]),
    {reply, ping_reply(), State}.

-spec protocol_error(binary(), binary()) -> {text, iodata()}.
protocol_error(Code, Reason) ->
    {text, jiffy:encode({[{<<"record">>, <<"protocol_error">>}, {<<"code">>, Code}, {<<"reason">>, Reason}]})}.

-spec authenticated(binary()) -> {text, iodata()}.
authenticated(UserID) ->
    {text, jiffy:encode({[{<<"record">>, <<"authenticated">>}, {<<"user_id">>, UserID}]})}.

-spec chat_state(binary(), [{binary(), binary()}]) -> {text, iodata()}.
chat_state(Chat, Members) ->
    UserData = lists:map(fun({Username, UserID}) ->
            {[{<<"record">>, <<"chat_user">>}, {<<"user_name">>, Username}, {<<"user_id">>, UserID}]}
        end, Members),
    Record = {[{<<"record">>, <<"chat_state">>}, {<<"chat_name">>, Chat}, {<<"users">>, UserData}]},
    {text, jiffy:encode(Record)}.

-spec joined(#user_joined{}) -> {text, iodata()}.
joined(#user_joined{chat=Chat, username=Username, user_id=UserID, timestamp=Timestamp, sequence=Sequence}) ->
    Record = {[{<<"record">>, <<"joined">>}, {<<"chat_name">>, Chat}, {<<"user_name">>, Username},
        {<<"user_id">>, UserID}, {<<"timestamp">>, Timestamp}, {<<"sequence">>, Sequence}]},
    {text, jiffy:encode(Record)}.

-spec chat_message(#chat_message{}) -> {text, iodata()}.
chat_message(#chat_message{chat_name=Chat, user_id=UserID, mime_type=MimeType, message=Message, timestamp=Timestamp, sequence=Sequence}) ->
    Record = {[{<<"record">>, <<"chat_message">>}, {<<"chat_name">>, Chat}, {<<"user_id">>, UserID},
        {<<"mime_type">>, MimeType}, {<<"message">>, Message}, {<<"timestamp">>, Timestamp}, {<<"sequence">>, Sequence}]},
    {text, jiffy:encode(Record)}.

-spec ping_reply() -> {text, iodata()}.
ping_reply() ->
    {text, jiffy:encode({[{<<"record">>, <<"ping_reply">>}]})}.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

authenticated_state(Username, UserID) ->
    #chat_protocol_v1_state{username=Username, user_id=UserID}.

authenticated_state(Username, UserID, Chats) ->
    #chat_protocol_v1_state{username=Username, user_id=UserID, chats=sets:from_list(Chats)}.

decode_record_type(Reply) ->
    {[{<<"record">>, RecordType} | _Rest]} =  jiffy:decode(Reply),
    RecordType.

initial_status_is_unauthenticated_test() ->
    #chat_protocol_v1_state{status=Status} = init(),
    authenticating = Status.

initial_chats_are_empty_test() ->
    #chat_protocol_v1_state{chats=Chats} = init(),
    0 = sets:size(Chats).

handle_authenticate_when_unauthenticated_replies_authenticated_test() ->
    State = init(),
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, <<"foobar">>}, {<<"user_id">>, <<"baz">>}]},
    Mock = em:new(),
    em:strict(Mock, chat_stream, register_user, [<<"foobar">>, <<"baz">>], {return, new}),
    em:replay(Mock),
    {reply, {text, Authenticated}, #chat_protocol_v1_state{}} = handle_client_message(Message, State),
    em:verify(Mock),
    <<"authenticated">> = decode_record_type(Authenticated).

handle_authenticate_when_unauthenticated_stores_username_test() ->
    State = init(),
    User = <<"foobar">>,
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, User}, {<<"user_id">>, <<"baz">>}]},
    Mock = em:new(),
    em:strict(Mock, chat_stream, register_user, [User, <<"baz">>], {return, new}),
    em:replay(Mock),
    {reply, {text, _Authenticated}, #chat_protocol_v1_state{username=Username}} = handle_client_message(Message, State),
    em:verify(Mock),
    User = Username.

handle_authenticated_when_unauthenticated_chooses_user_id_if_none_provided_test() ->
    State = init(),
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, <<"foobar">>}]},
    Mock = em:new(),
    em:strict(Mock, user_id, generate, [], {return, <<"baz">>}),
    em:strict(Mock, chat_stream, register_user, [<<"foobar">>, <<"baz">>], {return, new}),
    em:replay(Mock),
    {reply, {text, _Authenticated}, #chat_protocol_v1_state{user_id=UserID}} = handle_client_message(Message, State),
    em:verify(Mock),
    true = erlang:is_binary(UserID).

handle_authenticated_when_unauthenticated_stores_user_id_when_provided_test() ->
    State = init(),
    ID = <<"blahblahteleblah">>,
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, <<"foobar">>}, {<<"user_id">>, ID}]},
    Mock = em:new(),
    em:strict(Mock, chat_stream, register_user, [<<"foobar">>, ID], {return, new}),
    em:replay(Mock),
    {reply, {text, _Authenticated}, #chat_protocol_v1_state{user_id=UserID}} = handle_client_message(Message, State),
    em:verify(Mock),
    ID = UserID.

handle_authenticate_when_already_authenticated_replies_protocol_error_test() ->
    State = init(),
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, <<"foobar">>}, {<<"user_id">>, <<"baz">>}]},
    Mock = em:new(),
    em:strict(Mock, chat_stream, register_user, [<<"foobar">>, <<"baz">>], {return, new}),
    em:replay(Mock),
    {reply, _Reply, NewState} = handle_client_message(Message, State),
    em:verify(Mock),
    {reply, [{text, ProtocolError}, close], NewState} = handle_client_message(Message, NewState),
    <<"protocol_error">> = decode_record_type(ProtocolError).

handle_bad_authenticate_message_replies_protocol_error_test() ->
    State = init(),
    Message = {[{<<"foo">>, <<"bar">>}]},
    {reply, [{text, ProtocolError}, close], _NewState} = handle_client_message(Message, State),
    <<"protocol_error">> = decode_record_type(ProtocolError).

handle_non_authentication_message_when_not_authenticated_replies_protocol_error_test() ->
    State = init(),
    Message = {[{<<"record">>, <<"join_chat">>}, {<<"chat_name">>, <<"foobar">>}]},
    {reply, [{text, ProtocolError}, close], _NewState} = handle_client_message(Message, State),
    <<"protocol_error">> = decode_record_type(ProtocolError).

handle_join_chat_joins_chat_when_not_already_joined_test() ->
    State = authenticated_state(<<"someuser">>, <<"some_user_id">>),
    Message = {[{<<"record">>, <<"join_chat">>}, {<<"chat_name">>, <<"foobar">>}]},
    Mock = em:new(),
    em:strict(Mock, chat_stream, join, [<<"foobar">>, <<"someuser">>, <<"some_user_id">>], {return, []}),
    em:replay(Mock),
    {reply, {text, ChatState}, #chat_protocol_v1_state{chats=Chats}} = handle_client_message(Message, State),
    em:verify(Mock),
    1 = sets:size(Chats),
    <<"chat_state">> = decode_record_type(ChatState).

handle_join_chat_returns_protocol_error_when_already_in_chat_test() ->
    State = authenticated_state(<<"someuser">>, <<"some_user_id">>, [<<"foobar">>]),
    Message = {[{<<"record">>, <<"join_chat">>}, {<<"chat_name">>, <<"foobar">>}]}, 
    {reply, [{text, ProtocolError}, close], State} = handle_client_message(Message, State),
    <<"protocol_error">> = decode_record_type(ProtocolError).

handle_chat_message_returns_protocol_error_for_chat_not_joined_test() ->
    State = authenticated_state(<<"someuser">>, <<"some_user_id">>, [<<"foobar">>]),
    Message = {[{<<"record">>, <<"chat_message">>}, {<<"chat_name">>, <<"barbaz">>}, {<<"mime_type">>, <<"text/plain">>}, {<<"I like cats">>}]},
    {reply, [{text, ProtocolError}, close], State} = handle_client_message(Message, State),
    <<"protocol_error">> = decode_record_type(ProtocolError).

handle_chat_message_forwards_to_chat_stream_for_chat_already_joined_test() ->
    State = authenticated_state(<<"someuser">>, <<"some_user_id">>, [<<"foobar">>]),
    Message = {[{<<"record">>, <<"chat_message">>}, {<<"chat_name">>, <<"foobar">>}, {<<"mime_type">>, <<"text/plain">>}, {<<"message">>, <<"I like cats">>}]},
    PostedMessage = #chat_message{chat_name= <<"foobar">>, user_id= <<"some_user_id">>, mime_type= <<"text/plain">>, message= <<"I like cats">>},
    Mock = em:new(),
    em:strict(Mock, chat_stream, post, [PostedMessage]),
    em:replay(Mock),
    {ok, State} = handle_client_message(Message, State),
    em:verify(Mock).

handle_ping_replies_with_ping_reply_test() ->
    State = authenticated_state(<<"someuser">>, <<"some_user_id">>),
    {reply, {text, <<"{\"record\":\"ping_reply\"}">>}, State} = handle_client_message({[{<<"record">>, <<"ping">>}]}, State).

-endif.
