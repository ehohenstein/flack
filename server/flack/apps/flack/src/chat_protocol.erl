-module(chat_protocol).

-type protocol_state() :: any().

-callback init() -> protocol_state().

-callback handle_client_message(tuple(), protocol_state()) ->
    {ok, protocol_state()} | {reply, cow_ws:frame(), protocol_state()} |
    {reply, [cow_ws:frame()], protocol_state()} | {stop, protocol_state()}.

