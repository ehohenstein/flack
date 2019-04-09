-module(stub).

% Public API
-export([system_time/1, universaltime/0]).

% Public API

-spec system_time(erlang:time_unit()) -> integer().
system_time(Unit) ->
    os:system_time(Unit).

-spec universaltime() -> {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
universaltime() ->
    erlang:universaltime().
