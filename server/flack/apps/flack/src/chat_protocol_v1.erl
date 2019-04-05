-module(chat_protocol_v1).

-behavior(chat_protocol).

% chat_protocol functions
-export([init/0, handle_client_message/2]).

-record(chat_protocol_v1_state, {
    status=authenticating :: authenticating | authenticated,
    username=none :: none | binary(),
    user_id=none :: none | binary()
}).

% chat_protocol functions

-spec init() -> #chat_protocol_v1_state{}.
init() ->
    #chat_protocol_v1_state{}.

-spec handle_client_message(tuple(), #chat_protocol_v1_state{}) ->
    {ok, #chat_protocol_v1_state{}} | {reply, cow_ws:frame(), #chat_protocol_v1_state{}} |
    {reply, [cow_ws:frame()], #chat_protocol_v1_state{}} | {stop, #chat_protocol_v1_state{}}.
handle_client_message({[{<<"record">>, <<"authenticate">>} | Rest]}, #chat_protocol_v1_state{username=none}=State) ->
    authenticate_username(Rest, State);
handle_client_message(Message, #chat_protocol_v1_state{username=none}=State) ->
    error_logger:info_msg("chat_protocol_v1 received unexpected message while not yet authenticated:~n~p", [Message]),
    {reply, [protocol_error(<<"CHAT_PROTOCOL-001">>, <<"Expected authenticate message">>), close], State};
handle_client_message({[{<<"record">>, <<"authenticate">>} | _Rest]}, #chat_protocol_v1_state{username=Username}=State) ->
    error_logger:info_msg("chat_protocol_v1 for user ~p received authenticate message while already authenticated", [Username]),
    {reply, [protocol_error(<<"CHAT_PROTOCOL-002">>, <<"Already authenticated">>), close], State};
handle_client_message(_Message, #chat_protocol_v1_state{}=State) ->
    {ok, State}.

% private functions

-spec authenticate_username([tuple()], #chat_protocol_v1_state{}) ->
    {reply, cow_ws:frame(), #chat_protocol_v1_state{}} | {reply, [cow_ws:frame()], #chat_protocol_v1_state{}}.
authenticate_username(Fields, #chat_protocol_v1_state{}=State) ->
    case proplists:lookup(<<"user_name">>, Fields) of
        none ->
            error_logger:info_msg("chat_protocol_v1 authenticate message missing user_name field:~n~p", [Fields]),
            {reply, [protocol_error(<<"CHAT_PROTOCOL-003">>, <<"authenticate message missing user_name field">>), close], State};
        {<<"user_name">>, Username} ->
            authenticate_user_id(Username, Fields, State)
    end.

-spec authenticate_user_id(binary(), [tuple()], #chat_protocol_v1_state{}) ->
    {reply, cow_ws:frame(), #chat_protocol_v1_state{}} | {reply, [cow_ws:frame()], #chat_protocol_v1_state{}}.
authenticate_user_id(Username, Fields, #chat_protocol_v1_state{}=State) ->
    UserID = case proplists:lookup(<<"user_id">>, Fields) of
        none -> generate_user_id();
        {<<"user_id">>, ClientUserID} -> ClientUserID
    end,
    error_logger:info_msg("authenticated user ~p with user_id ~p", [Username, UserID]),
    {reply, authenticated(UserID), State#chat_protocol_v1_state{username=Username, user_id=UserID}}.

-spec generate_user_id() -> binary().
generate_user_id() ->
    % This is crappy and dumb. Don't do it like this. It is good enough for this sample
    % application that does not actually do authentication. The intent here is only to
    % generate a token that will indentify a browser session and is unlikely to collide
    % with other tokens so that if two users claim to have the same name, the source of
    % their messages can be distinguished.
    erlang:list_to_binary(lists:foldl(fun(_, Acc) ->
            Index = rand:uniform(16),
            Char = lists:nth(Index, "0123456789abcdef"),
            [Char | Acc]
        end, [], lists:seq(1, 32))).

-spec protocol_error(binary(), binary()) -> {text, iodata()}.
protocol_error(Code, Reason) ->
    {text, jiffy:encode({[{<<"record">>, <<"protocol_error">>}, {<<"code">>, Code}, {<<"reason">>, Reason}]})}.

-spec authenticated(binary()) -> {text, iodata()}.
authenticated(UserID) ->
    {text, jiffy:encode({[{<<"record">>, <<"authenticated">>}, {<<"user_id">>, UserID}]})}.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

decode_record_type(Reply) ->
    {[{<<"record">>, RecordType} | _Rest]} =  jiffy:decode(Reply),
    RecordType.

initial_status_is_unauthenticated_test() ->
    #chat_protocol_v1_state{status=Status} = init(),
    authenticating = Status.

handle_authenticate_when_unauthenticated_replies_authenticated_test() ->
    State = init(),
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, <<"foobar">>}]},
    {reply, {text, Authenticated}, #chat_protocol_v1_state{}} = handle_client_message(Message, State),
    <<"authenticated">> = decode_record_type(Authenticated).

handle_authenticate_when_unauthenticated_stores_username_test() ->
    State = init(),
    User = <<"foobar">>,
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, User}]},
    {reply, {text, _Authenticated}, #chat_protocol_v1_state{username=Username}} = handle_client_message(Message, State),
    User = Username.

handle_authenticated_when_unauthenticated_chooses_user_id_if_none_provided_test() ->
    State = init(),
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, <<"foobar">>}]},
    {reply, {text, _Authenticated}, #chat_protocol_v1_state{user_id=UserID}} = handle_client_message(Message, State),
    true = erlang:is_binary(UserID).

handle_authenticated_when_unauthenticated_stores_user_id_when_provided_test() ->
    State = init(),
    ID = <<"blahblahteleblah">>,
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, <<"foobar">>}, {<<"user_id">>, ID}]},
    {reply, {text, _Authenticated}, #chat_protocol_v1_state{user_id=UserID}} = handle_client_message(Message, State),
    ID = UserID.

handle_authenticate_when_already_authenticated_replies_protocol_error_test() ->
    State = init(),
    Message = {[{<<"record">>, <<"authenticate">>}, {<<"user_name">>, <<"foobar">>}]},
    {reply, _Reply, NewState} = handle_client_message(Message, State),
    {reply, [{text, ProtocolError}, close], NewState} = handle_client_message(Message, NewState),
    <<"protocol_error">> = decode_record_type(ProtocolError).

handle_bad_authenticate_message_replies_protocol_error_test() ->
    State = init(),
    Message = {[{<<"foo">>, <<"bar">>}]},
    {reply, [{text, ProtocolError}, close], _NewState} = handle_client_message(Message, State),
    <<"protocol_error">> = decode_record_type(ProtocolError).

-endif.
