-module(user_id).

% Public API
-export([generate/0]).

% Public API

-spec generate() -> binary().
generate() ->
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
