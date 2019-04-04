-module(flack_sup).

-behavior(supervisor).

% Public API
-export([start_link/0]).

-export([init/1]).

-define(LISTEN_PORT, 8080).
-define(NUM_ACCEPTORS, 100).
-define(MAX_CONNS, 10000).

% public API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Supervisor callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
    %    permanent, 5000, supervisor, [ranch_sup]},

    TransportOptions = #{port => ?LISTEN_PORT, num_acceptors => ?NUM_ACCEPTORS, max_connections => ?MAX_CONNS},
    Dispatch = cowboy_router:compile([{'_', [{<<"/chat-server">>, chat_client_handler, []}]}]),
    ListenerSpec = ranch:child_spec(flack, ranch_tcp, TransportOptions, cowboy_clear, #{env => #{dispatch => Dispatch}}),

    {ok, {{one_for_all, 0, 1}, [ListenerSpec]}}.

