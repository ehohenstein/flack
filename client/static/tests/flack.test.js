const flack = require("../scripts/flack");
require("jsdom");
$ = require("jquery");
const FakeWebSocket = require("./fakes/fake_websocket");
window.WebSocket = FakeWebSocket.FakeWebSocket;

function get_flack_dom() {
    document.body.innerHTML =
        '<div class="room-list-container">' +
            '<div class="list">' +
                '<div class="list-item logout">' +
                    '<span class="username">whoami</span><button type="button" class="flack-button">Logout</button>' +
                '</div>' +
                '<div class="list-item login">' +
                    '<input type="text" /><button type="button" class="flack-button">Login</button>'
                '</div>' +
            '</div>' +
            '<div class="list">' +
                '<div class="list-item join">' +
                '<input type="text" /><button type="button" class="flack-button">Join</button/>' +
            '</div>' +
        '</div>' +
        '<div class="content">' +
            '<div class="content-messages">' +
            '</div>' +
        '</div>' +
        '<div class="chatbox">' +
            '<div class="chat-entry">' +
                '<input type="text" class="chat-text" placeholder="Say something" /><button type="button" class="flack-button">Send</button>' +
            '</div>' +
        '</div>';
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

function last_websocket() {
    console.log(FakeWebSocket.sockets.length);
    return FakeWebSocket.sockets[FakeWebSocket.sockets.length - 1];
}

function last_message_sent() {
    let frames = last_websocket().frames_sent;
    let frame = frames[frames.length - 1];
    return JSON.parse(frame);
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
    $('.logout').addClass('hidden');
    expect($('.logout').hasClass('hidden')).toBe(true);
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
