const flack = require("../scripts/flack");
require("jsdom");
$ = require("jquery");
const FakeWebSocket = require("./fakes/fake_websocket");
window.WebSocket = FakeWebSocket.FakeWebSocket;

function get_flack_dom() {
    let html =
        '<div class="room-list-container">' +
            '<div class="list">' +
                '<div class="list-item login">' +
                    '<input type="text" /><button type="button" class="flack-button">Login</button>' +
                '</div>' +
                '<div class="list-item logout">' +
                    '<span class="username">whoami</span><button type="button" class="flack-button">Logout</button>' +
                '</div>' +
            '</div>' +
            '<div class="list rooms">' +
                '<div class="list-item join">' +
                    '<input type="text" /><button type="button" class="flack-button">Join</button>' +
                '</div>' +
            '</div>' +
        '</div>' +
        '<div class="content">' +
        '</div>' +
        '<div class="chatbox">' +
            '<div class="chat-entry">' +
                '<input type="text" class="chat-text" /><button type="button" class="chat-send flack-button">Send</button>' +
            '</div>' +
        '</div>';
    document.body.innerHTML = html;
}

function do_login() {
    $('.login input').val('me');
    $('.login button').click();
}

function do_open() {
    do_login();
    last_websocket().onopen();
}

function do_hello() {
    do_open();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'server_hello', 'protocol_version': '1.0'}));
}

function do_auth() {
    do_hello();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'authenticated', 'user_id': '123456'}));
}

function do_join() {
    do_auth();
    $('.join input').val('foobar').keypress();
    $('.join button').click();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'chat_state', 'chat_name': 'foobar', 'users': [{'record': 'chat_user', 'user_name': 'someguy', 'user_id': '987654'}]}));
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'joined', 'chat_name': 'foobar', 'user_name': 'me', 'user_id': '123456', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 2}));
}

function do_join_another(name) {
    $('.join input').val(name).keypress();
    $('.join button').click();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'chat_state', 'chat_name': name, 'users': [{'record': 'chat_user', 'user_name': 'someguy', 'user_id': '987654'}]}));
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'joined', 'chat_name': name, 'user_name': 'me', 'user_id': '123456', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 2}));
}

function do_leave() {
    do_join();
    $('.rooms .room.active button').click();
}

function do_say(chat, from, message) {
    do_join();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'chat_message', 'chat_name': chat, 'user_id': from, 'mime_type': 'text/plain', 'message': message, 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 2}));
}

function last_websocket() {
    console.log(FakeWebSocket.sockets.length);
    return FakeWebSocket.sockets[FakeWebSocket.sockets.length - 1];
}

function last_message_sent() {
    let frames = last_websocket().frames_sent;
    let frame = frames[frames.length - 1];
    return JSON.parse(frame);
}

function message_at_index(index) {
    return $($('.content-messages.visible .message')[index]);
}

function message_text_at_index(index) {
    return $('p', message_at_index(index));
}

beforeEach(() => {
    get_flack_dom();
    new flack('fake_url');
});

afterEach(() => {
    FakeWebSocket.reset();
});

test('jsdom environment works at all', () => {
    expect($('.room-list-container').length).toBe(1);
});

test('login is visible when disconnected', () => {
    expect($('.login').hasClass('hidden')).toBe(false);
});

test('logout is hidden when disconnected', () => {
    expect($('.logout').hasClass('hidden')).toBe(true);
});

test('join text is disabled when disconnected', () => {
    expect($('.join input').is(':disabled')).toBe(true);
});

test('join button is disabled when disconnected', () => {
    expect($('.join button').is(':disabled')).toBe(true);
});

test('chat input is disabled when disconnected', () => {
    expect($('.chat-entry input').is(':disabled')).toBe(true);
});

test('chat send button is disabled when disconnected', () => {
    expect($('.chat-entry button').is(':disabled')).toBe(true);
});

test('login is disabled when username input field is empty', () => {
    $('.login input').value = '';
    expect($('.login button').is(':disabled')).toBe(true);
});

test('login is enabled when username input field is populated', () => {
    $('.login input').val('me').keypress();
    expect($('.login button').is(':disabled')).toBe(false);
});

test('login input is disabled when submitted', () => {
    do_login();
    expect($('.login input').is(':disabled')).toBe(true);
});

test('login button is disabled when submitted', () => {
    do_login();
    expect($('.login button').is(':disabled')).toBe(true);
});

test('login input is enabled if connection closes', () => {
    do_login();
    last_websocket().onclose(new FakeWebSocket.FakeCloseEvent(42, 'because'));
    expect($('.login input').is(':disabled')).toBe(false); 
});

test('login button is enabled if connection closes', () => {
    do_login();
    last_websocket().onclose(new FakeWebSocket.FakeCloseEvent(42, 'because'));
    expect($('.login button').is(':disabled')).toBe(false); 
});

test('disconnects when bad data received', () => {
    do_open();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent('this is not json'));
    expect($('.login button').is(':disabled')).toBe(false); 
});

test('client_hello sent when connection is open', () => {
    do_open();
    let client_hello = last_message_sent();
    expect(client_hello).toEqual({'record':'client_hello', 'protocol_version':'1.0'});
});

test('disconnects when protocol_error received', () => {
    do_open();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'protocol_error', 'code': 'SERVER-042', 'reason': 'shoot'}));
    expect($('.login button').is(':disabled')).toBe(false);
});

test('authenticate sent when server_hello received', () => {
    do_hello();
    let authenticate = last_message_sent();
    expect(authenticate).toEqual({'record': 'authenticate', 'user_name': 'me'});
});

test('login is hidden when auth completes', () => {
    do_auth();
    expect($('.login').hasClass('hidden')).toBe(true);
});

test('logout is shown when auth completes', () => {
    do_auth();
    expect($('.logout').hasClass('hidden')).toBe(false);
});

test('join input is enabled when authenticated', () => {
    do_auth();
    expect($('.join input').is(':disabled')).toBe(false);
});

test('join button is disabled when authenticated and join input is empty', () => {
    do_auth();
    expect($('.join button').is(':disabled')).toBe(true);
});

test('join button is enabled when authenticated and join input is not empty', () => {
    do_auth();
    $('.join input').val('foobar').keypress();
    expect($('.join button').is(':disabled')).toBe(false);
});

test('join input is disabled while joining', () => {
    do_auth();
    $('.join input').val('foobar').keypress();
    $('.join button').click();
    expect($('.join input').is(':disabled')).toBe(true);
});

test('join button is disabled while joining', () => {
    do_auth();
    $('.join input').val('foobar').keypress();
    $('.join button').click();
    expect($('.join button').is(':disabled')).toBe(true);
});

test('join input is cleared when chat is joined', () => {
    do_join();
    expect($('.join input').val()).toBe('');
});

test('join input is enabled when chat is joined', () => {
    do_join();
    expect($('.join input').is(':disabled')).toBe(false);
});

test('chat appears in chat list when chat is joined', () => {
    do_join();
    expect($('.rooms .room').length).toBe(1);
    expect($('.name', $('.rooms .room')[0]).text()).toBe('foobar');
});

test('chat is selected in chat list when chat is joined', () => {
    do_join();
    expect($($('.rooms .room')[0]).hasClass('active')).toBe(true);
});

test('joining second room deselects first', () => {
    do_join();
    do_join_another('barbaz');
    expect($('.rooms .room.active').length).toBe(1);
});

test('joining second room clears chat input', () => {
    do_join();
    $('.chat-entry input').val('I like cats').keypress();
    do_join_another('barbaz');
    expect($('.chat-entry input').val()).toBe('');
});

test('leave chat button is enabled when chat is joined', () => {
    do_join();
    expect($('button', $('.rooms .room')[0]).is(':disabled')).toBe(false);
});

test('chat entries are visible when chat is joined', () => {
    do_join();
    expect($('.content-messages.visible').length).toBe(1);
});

test('chat input is enabled when chat is joined', () => {
    do_join();
    expect($('.chat-entry input').is(':disabled')).toBe(false);
});

test('chat send button is disabled when chat is joined and input is empty', () => {
    do_join();
    expect($('.chat-entry button').is(':disabled')).toBe(true);
});

test('chat send button is enabled when chat is joined and input is not empty', () => {
    do_join();
    $('.chat-entry input').val('something witty').keypress();
    expect($('.chat-entry button').is(':disabled')).toBe(false);
});

test('users already in chat are initially presented', () => {
    do_join();
    expect($('.content-messages.visible .message').length).toBe(2);
    expect(message_text_at_index(1).text()).toBe('someguy is in the room');
});

test('you joined the chat is initially presented', () => {
    do_join();
    expect($('.content-messages.visible .message').length).toBe(2);
    expect(message_text_at_index(0).text()).toBe('you joined the room');
});

test('chat send button sends chat when clicked', () => {
    do_join();
    $('.chat-entry input').val('I like cats').keypress();
    $('.chat-entry button').click();
    let chat_message = last_message_sent();
    expect(chat_message).toEqual({'record': 'chat_message', 'chat_name': 'foobar', 'mime_type': 'text/plain', 'message': 'I like cats'});
});

test('chat messages from other users already in the room are displayed', () => {
    do_say('foobar', '987654', 'hi there');
    expect($('.content-messages.visible .message').length).toBe(3);
    expect(message_text_at_index(0).text()).toBe('someguy: hi there');
});

test('chat messages from self are displayed', () => {
    do_say('foobar', '123456', 'hello');
    expect($('.content-messages.visible .message').length).toBe(3);
    expect(message_text_at_index(0).text()).toBe('me: hello');
});

test('other users joining room are displayed', () => {
    do_join();  
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'joined', 'chat_name': 'foobar', 'user_name': 'someoneelse', 'user_id': 'abcdef', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect($('.content-messages.visible .message').length).toBe(3);
    expect(message_text_at_index(0).text()).toBe('someoneelse joined the room');
});

test('chat messages from users joining later are displayed', () => {
    do_join();  
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'joined', 'chat_name': 'foobar', 'user_name': 'someoneelse', 'user_id': 'abcdef', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'chat_message', 'chat_name': 'foobar', 'user_id': 'abcdef', 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect($('.content-messages.visible .message').length).toBe(4);
    expect(message_text_at_index(0).text()).toBe('someoneelse: I like cats');
});

test('other users already in room leaving room are displayed', () => {
    do_join();  
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'left', 'chat_name': 'foobar', 'user_id': '987654', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect(message_text_at_index(0).text()).toBe('someguy left the room');
});

test('other users joined after leaving are displayed', () => {
    do_join();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'joined', 'chat_name': 'foobar', 'user_name': 'someoneelse', 'user_id': 'abcdef', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'left', 'chat_name': 'foobar', 'user_id': 'abcdef', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect($('.content-messages.visible .message').length).toBe(4);
    expect(message_text_at_index(0).text()).toBe('someoneelse left the room');
});

test('join messages received for unknown room are ignored', () => {
    do_join();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'joined', 'chat_name': 'barbaz', 'user_name': 'someoneelse', 'user_id': 'abcdef', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect($('.content-messages.visible .message').length).toBe(2);
});

test('chat messages received for unknown room are ignored', () => {
    do_join();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'chat_message', 'chat_name': 'barbaz', 'user_id': 'abcdef', 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect($('.content-messages.visible .message').length).toBe(2);
});

test('chat messages received for unknown user are ignored', () => {
    do_join();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'chat_message', 'chat_name': 'foobar', 'user_id': 'abcdef', 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect($('.content-messages.visible .message').length).toBe(2);
});

test('leave messages received for unknown room are ignored', () => {
    do_join();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'left', 'chat_name': 'barbaz', 'user_id': 'abcdef', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect($('.content-messages.visible .message').length).toBe(2);
});

test('leave messages received for unknown room are ignored', () => {
    do_join();
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'left', 'chat_name': 'foobar', 'user_id': 'abcdef', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 3}));
    expect($('.content-messages.visible .message').length).toBe(2);
});

test('clicking leave button sends leave_chat message', () => {
    do_leave();
    let leave = last_message_sent();
    expect(leave).toEqual({'record': 'leave_chat', 'chat_name': 'foobar'});
});

test('clicking leave button removes room from room list', () => {
    do_leave();
    expect($('.rooms .room').length).toBe(0);
});

test('clicking leave button removes chat content', () => {
    do_leave();
    expect($('.content-messages').length).toBe(0);
});

test('clicking leave button of current selection selects next room if there is one', () => {
    do_join();
    do_join_another('barbaz');
    do_join_another('blah');
    $('.rooms .room.active button').click();
    expect($('.rooms .room.active .name').text()).toBe('foobar');
});

test('clicking leave button of current selection selects previous room if there is one and there is no next room', () => {
    do_join();
    do_join_another('operations');
    do_join_another('production');
    $('.rooms .room.active button').click();
    expect($('.rooms .room.active .name').text()).toBe('operations');
});

test('clicking leave button of last room disables chat input', () => {
    do_leave();
    expect($('.chat-entry input').is(':disabled')).toBe(true);
});

test('clicking leave button of last room disables chat send button', () => {
    do_leave();
    expect($('.chat-entry button').is(':disabled')).toBe(true);
});

test('clicking room in list selects that room', () => {
    do_join();
    do_join_another('barbaz');
    $($('.rooms .room button')[1]).click();
    expect($('.rooms .room.active .name').text()).toBe('foobar');
});

test('clicking active room in list does nothing', () => {
    do_join();
    do_join_another('barbaz');
    $('.chat-entry input').val('I like cats');
    $($('.rooms .room button')[0]).click();
    expect($('.rooms .room.active .name').text()).toBe('barbaz');
    expect($('.chat-entry input').val()).toBe('I like cats');
});

test('clicking room in list deselects previous room', () => {
    do_join();
    do_join_another('barbaz');
    $($('.rooms .room button')[1]).click();
    expect($($('.rooms .room button')[1]).hasClass('active')).toBe(false);
});

test('clicking room in list shows chats for that room', () => {
    do_say('foobar', '123456', 'howdy');
    do_join_another('barbaz');
    $($('.rooms .room button')[1]).click();
    expect($('.content-messages.visible .message').length).toBe(3);
});

test('clicking room in list hides chats for previous room', () => {
    do_say('foobar', '123456', 'howdy');
    do_join_another('barbaz');
    $($('.rooms .room button')[1]).click();
    expect($('.content-messages.visible .message').length).toBe(2);
});

test('clicking room in list clears chat input', () => {
    do_join();
    do_join_another('barbaz');
    $('.chat-entry input').val('I like cats').keypress();
    $($('.rooms .room button')[1]).click();
    expect($('.chat-entry input').val()).toBe('');
});

test('messages arriving for inactive room are not shown in current room', () => {
    do_join();
    do_join_another('barbaz');
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'chat_message', 'chat_name': 'foobar', 'user_id': '987654', 'mime_type': 'text/plain', 'message': 'say wut?', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 2}));
    expect($('.content-messages.visible .message').length).toBe(2);
});

test('messages arriving for inactive room are visible when that room is active', () => {
    do_join();
    do_join_another('barbaz');
    last_websocket().onmessage(new FakeWebSocket.FakeMessageEvent({'record': 'chat_message', 'chat_name': 'foobar', 'user_id': '987654', 'mime_type': 'text/plain', 'message': 'say wut?', 'timestamp': '2019-04-08T15:31:42Z', 'sequence': 2}));
    $($('.rooms .room button')[1]).click();
    expect($('.content-messages.visible .message').length).toBe(3);
});

