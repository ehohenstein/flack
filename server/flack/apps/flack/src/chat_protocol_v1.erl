-module(chat_protocol_v1).

-behavior(chat_protocol).

% chat_protocol functions
-export([init/0]).

-record(chat_protocol_v1_state, {
    status=authenticating :: authenticating | authenticated
}).

% chat_protocol functions

-spec init() -> #chat_protocol_v1_state{}.
init() ->
    #chat_protocol_v1_state{}.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

initial_status_is_unauthenticated_test() ->
    #chat_protocol_v1_state{status=Status} = init(),
    authenticating = Status.

-endif.
