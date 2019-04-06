-module(chat_stream).

-include("chat_stream.hrl").

% Public API
-export([register_user/2, join/3]).

% Public API

-spec register_user(binary(), binary()) -> new | updated.
register_user(Username, UserID) ->
    gproc:ensure_reg(user_key(erlang:self()), undefined, [{username, Username}, {user_id, UserID}]).

-spec join(binary(), binary(), binary()) -> [{binary(), binary()}].
join(Chat, Username, UserID) ->
    ChatKey = chat_key(Chat),
    Pids = gproc:lookup_pids(ChatKey),
    gproc:reg(ChatKey),
    Now = erlang:list_to_binary(calendar:system_time_to_rfc3339(stub:system_time(seconds), [{offset, "Z"}])),
    %TODO: figure out how to use a gproc counter for the sequence
    gproc:send(ChatKey, #user_joined{chat=Chat, username=Username, user_id=UserID, timestamp=Now, sequence=42}),
    lists:map(fun(Pid) ->
            Props = gproc:get_attributes(user_key(Pid)),
            {proplists:get_value(username, Props), proplists:get_value(user_id, Props)}
        end, Pids).

% Private functions

-spec user_key(pid()) -> gproc:key().
user_key(Pid) ->
    {n, g, Pid}.

-spec chat_key(binary()) -> gproc:key().
chat_key(Chat) ->
    {p, g, Chat}.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

join_subscribes_to_chat_and_gets_members_info_and_sends_join_test() ->
    Joined = #user_joined{chat= <<"fake_chat">>, username= <<"fake_user">>, user_id= <<"fake_id">>, timestamp="2019-04-06T08:47:21Z", sequence=42},
    Mock = em:new(),
    em:strict(Mock, gproc, lookup_pids, [chat_key(<<"fake_chat">>)], {return, [fake_pid]}),
    em:strict(Mock, gproc, reg, [chat_key(<<"fake_chat">>)]),
    em:strict(Mock, stub, system_time, [seconds], {return, 1554540441}),
    em:strict(Mock, gproc, send, [chat_key(<<"fake_chat">>), Joined]),
    em:strict(Mock, gproc, get_attributes, [user_key(fake_pid)], {return, [{username, <<"other_fake_user">>}, {user_id, <<"other_fake_id">>}]}),
    em:replay(Mock),
    [{<<"other_fake_user">>, <<"other_fake_id">>}] = join(<<"fake_chat">>, <<"fake_user">>, <<"fake_id">>),
    em:verify(Mock).

-endif.
