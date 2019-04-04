-module(chat_client_handler).

-behavior(cowboy_websocket).

% cowboy websocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(chat_client_handler_state, {
}).

-spec init(cowboy_req:req(), any()) -> {cowboy_websocket, cowboy_req:req(), any()}.
init(Req, Params) ->
    {cowboy_websocket, Req, Params}.

-spec websocket_init(any()) -> {ok, #chat_client_handler_state{}}.
websocket_init(_Params) ->
    {ok, #chat_client_handler_state{}}.

-spec websocket_handle({atom(), binary()}, #chat_client_handler_state{}) -> {ok, #chat_client_handler_state{}}.
websocket_handle({text, _Message}, #chat_client_handler_state{}=State) ->
    {ok, State}.

-spec websocket_info(any(), #chat_client_handler_state{}) -> {ok, #chat_client_handler_state{}}.
websocket_info(_Message, #chat_client_handler_state{}=State) ->
    {ok, State}.

-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
