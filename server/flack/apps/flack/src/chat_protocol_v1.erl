-module(chat_protocol_v1).

-behavior(chat_protocol).

% chat_protocol functions
-export([init/0, handle_client_message/2]).

-record(chat_protocol_v1_state, {
    status=authenticating :: authenticating | authenticated
}).

% chat_protocol functions

-spec init() -> #chat_protocol_v1_state{}.
init() ->
    #chat_protocol_v1_state{}.

-spec handle_client_message(tuple(), #chat_protocol_v1_state{}) ->
    {ok, #chat_protocol_v1_state{}} | {reply, cow_ws:frame(), #chat_protocol_v1_state{}} |
    {reply, [cow_ws:frame()], #chat_protocol_v1_state{}} | {stop, #chat_protocol_v1_state{}}.
handle_client_message(_Message, #chat_protocol_v1_state{}=State) ->
    {ok, State}.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

initial_status_is_unauthenticated_test() ->
    #chat_protocol_v1_state{status=Status} = init(),
    authenticating = Status.

-endif.
