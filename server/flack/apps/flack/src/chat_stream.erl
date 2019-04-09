-module(chat_stream).

-include("chat_stream.hrl").

% Public API
-export([register_user/2, join/3, post/1, leave/2]).

% Public API

-spec register_user(binary(), binary()) -> new | updated.
register_user(Username, UserID) ->
    gproc:ensure_reg(user_key(erlang:self()), undefined, [{username, Username}, {user_id, UserID}]).

-spec join(binary(), binary(), binary()) -> [{binary(), binary()}].
join(Chat, Username, UserID) ->
    ChatKey = chat_key(Chat),
    Pids = gproc:lookup_pids(ChatKey),
    gproc:reg(ChatKey),
    Now = current_timestamp(),
    %TODO: figure out how to use a gproc counter for the sequence
    gproc:send(ChatKey, #user_joined{chat=Chat, username=Username, user_id=UserID, timestamp=Now, sequence=42}),
    lists:map(fun(Pid) ->
            Props = gproc:get_attributes(user_key(Pid), Pid),
            {proplists:get_value(username, Props), proplists:get_value(user_id, Props)}
        end, Pids).

-spec post(#chat_message{}) -> #chat_message{}.
post(#chat_message{chat_name=Chat}=Message) ->
    Now = current_timestamp(),
    %TODO: figure out how to use a gproc counter for the sequence
    gproc:send(chat_key(Chat), Message#chat_message{timestamp=Now, sequence=42}).

-spec leave(binary(), binary()) -> true.
leave(Chat, UserID) ->
    ChatKey = chat_key(Chat),
    Now = current_timestamp(),
    %TODO: figure out how to use a gproc counter for the sequence
    LeftMessage = #user_left{chat=Chat, user_id=UserID, timestamp=Now, sequence=42},
    gproc:send(ChatKey, LeftMessage),
    gproc:unreg(ChatKey).

% Private functions

-spec user_key(pid()) -> gproc:key().
user_key(Pid) ->
    {n, g, Pid}.

-spec chat_key(binary()) -> gproc:key().
chat_key(Chat) ->
    {p, g, Chat}.

-spec current_timestamp() -> binary().
current_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = stub:universaltime(),
    iolist_to_binary(io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ", [Year, Month, Day, Hour, Min, Sec])).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

join_subscribes_to_chat_and_gets_members_info_and_sends_join_test() ->
    Joined = #user_joined{chat= <<"fake_chat">>, username= <<"fake_user">>, user_id= <<"fake_id">>, timestamp= <<"2019-04-06T08:47:21Z">>, sequence=42},
    Mock = em:new(),
    em:strict(Mock, gproc, lookup_pids, [chat_key(<<"fake_chat">>)], {return, [fake_pid]}),
    em:strict(Mock, gproc, reg, [chat_key(<<"fake_chat">>)]),
    em:strict(Mock, stub, universaltime, [], {return, {{2019, 4, 6}, {8, 47, 21}}}),
    em:strict(Mock, gproc, send, [chat_key(<<"fake_chat">>), Joined]),
    em:strict(Mock, gproc, get_attributes, [user_key(fake_pid), fake_pid], {return, [{username, <<"other_fake_user">>}, {user_id, <<"other_fake_id">>}]}),
    em:replay(Mock),
    [{<<"other_fake_user">>, <<"other_fake_id">>}] = join(<<"fake_chat">>, <<"fake_user">>, <<"fake_id">>),
    em:verify(Mock).

post_forwards_to_chat_members_test() ->
    Message = #chat_message{chat_name= <<"fake_chat">>, user_id= <<"fake_id">>, mime_type= <<"fake_type">>, message= <<"I like cats">>},
    ForwardedMessage = Message#chat_message{timestamp= <<"2019-04-06T08:47:21Z">>, sequence=42},
    Mock = em:new(),
    em:strict(Mock, stub, universaltime, [], {return, {{2019, 4, 6}, {8, 47, 21}}}),
    em:strict(Mock, gproc, send, [chat_key(<<"fake_chat">>), ForwardedMessage]),
    em:replay(Mock),
    post(Message),
    em:verify(Mock).

leave_unsubscribes_from_chat_and_sends_left_test() ->
    LeftMessage = #user_left{chat= <<"foobar">>, user_id= <<"fake_id">>, timestamp= <<"2019-04-06T08:47:21Z">>, sequence=42},
    Mock = em:new(),
    em:strict(Mock, stub, universaltime, [], {return, {{2019, 4, 6}, {8, 47, 21}}}),
    em:strict(Mock, gproc, send, [chat_key(<<"foobar">>), LeftMessage]),
    em:strict(Mock, gproc, unreg, [chat_key(<<"foobar">>)], {return, true}),
    em:replay(Mock),
    leave(<<"foobar">>, <<"fake_id">>),
    em:verify(Mock).

-endif.
