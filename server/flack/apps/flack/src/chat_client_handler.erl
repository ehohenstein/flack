-module(chat_client_handler).

-behavior(cowboy_websocket).

% cowboy_websocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(chat_client_handler_state, {
}).

-spec init(cowboy_req:req(), any()) -> {cowboy_websocket, cowboy_req:req(), any()}.
init(Req, Params) ->
    error_logger:info_msg("chat_client_handler handling new connection", []),
    {cowboy_websocket, Req, Params}.

-spec websocket_init(any()) -> {ok, #chat_client_handler_state{}}.
websocket_init(_Params) ->
    error_logger:info_msg("chat_client_handler initializing websocket connection", []),
    {ok, #chat_client_handler_state{}}.

-spec websocket_handle({atom(), binary()}, #chat_client_handler_state{}) -> {ok, #chat_client_handler_state{}}.
websocket_handle({text, Message}, #chat_client_handler_state{}=State) ->
    error_logger:info_msg("chat_client_handler received message: ~p", [Message]),
    Decoded = jiffy:decode(Message),
    error_logger:info_msg("chat_client_handler decoded message: ~p", [Decoded]),
    {ok, State}.

-spec websocket_info(any(), #chat_client_handler_state{}) -> {ok, #chat_client_handler_state{}}.
websocket_info(Message, #chat_client_handler_state{}=State) ->
    error_logger:info_msg("chat_client_handler received info: ~p", [Message]),
    {ok, State}.

-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
    error_logger:info_msg("chat_client_handler terminating because: ~p", [_Reason]),
    ok.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

nothing_test() ->
    ok.

-endif.
