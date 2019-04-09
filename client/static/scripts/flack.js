
const ChatRoom = require('./chat_room');

const STATE_DISCONNECTED = 0;
const STATE_CONNECTING = 1;
const STATE_CONNECTED = 2;
const STATE_AUTHENTICATING = 3;
const STATE_AUTHENTICATED = 4;
const STATE_JOINING = 5;
const STATE_DISCONNECTING = 6;

const PING_TIMEOUT = 20000;

function FlackApp (url) {
    this.url = url;
    this.ws = null;

    this.login = $('.login');
    this.loginInput = $('input', this.login);
    $('.login input').keypress(this.loginChanged.bind(this));
    this.loginButton = $('button', this.login);
    $('.login button').click(this.loginClicked.bind(this));

    this.logout = $('.logout');
    this.logoutText = $('.username', this.logout);
    this.logoutButton = $('button', this.logout);
    this.logoutButton.click(this.logoutClicked.bind(this));

    this.roomList = $('.rooms');

    let join = $('.join', this.roomList);
    this.joinInput = $('input', join);
    this.joinInput.keypress(this.joinChanged.bind(this));
    this.joinButton = $('button', join);
    this.joinButton.click(this.joinClicked.bind(this));

    this.content = $('.content');

    let chatEntry = $('.chat-entry');
    this.chatInput = $('input', chatEntry);
    this.chatInput.keypress(this.chatChanged.bind(this));
    this.chatSendButton = $('button', chatEntry);
    this.chatSendButton.click(this.sendClicked.bind(this));

    this.state = STATE_DISCONNECTED;

    this.username = null;
    this.user_id = null;

    this.chats = {};
    this.currentChat = null;

    this.pingTimeout = null;

    this.init();
}

FlackApp.prototype.init = function () {
    this.checkInputEnabled();
};

FlackApp.prototype.loginChanged = function (event) {
    if (event && (event.keyCode == 13) && (this.loginInput.val() !== '')) {
        this.loginButton.click();
    } else {
        this.checkInputEnabled();
    }
};

FlackApp.prototype.joinChanged = function (event) {
    if (event && (event.keyCode == 13) && (this.joinInput.val() !== '')) {
        this.joinButton.click();
    } else {
        this.checkInputEnabled();
    }
};

FlackApp.prototype.chatChanged = function (event) {
    if (event && (event.keyCode == 13) && (this.chatInput.val() !== '')) {
        this.chatSendButton.click();
    } else {
        this.checkInputEnabled();
    }
};

FlackApp.prototype.checkInputEnabled = function () {
    if (this.state < STATE_AUTHENTICATED) {
        this.login.removeClass('hidden');
        this.logout.addClass('hidden');
        if (this.state === STATE_DISCONNECTED) {
            this.loginInput.prop('disabled', false);
            this.loginButton.prop('disabled', this.loginInput.val() === '');
        } else if (this.state === STATE_CONNECTING) {
            this.loginInput.prop('disabled', true);
            this.loginButton.prop('disabled', true);
        }
        this.joinInput.prop('disabled', true);
        this.joinButton.prop('disabled', true);
        this.chatInput.prop('disabled', true);
        this.chatSendButton.prop('disabled', true);
    } else {
        this.login.addClass('hidden');
        this.logout.removeClass('hidden');
        this.logoutButton.prop('disabled', false);
        if (this.state == STATE_AUTHENTICATED) {
            this.joinInput.prop('disabled', false);
            this.joinButton.prop('disabled', this.joinInput.val() == '');
            let rooms = $('.room.active', this.chatRooms);
            let haveRoom = rooms.length !== 0;
            this.chatInput.prop('disabled', !haveRoom);
            this.chatSendButton.prop('disabled', !haveRoom || (this.chatInput.val() == ''));
        } else if (this.state == STATE_JOINING) {
            this.joinInput.prop('disabled', true);
            this.joinButton.prop('disabled', true);
        }
    }
};

FlackApp.prototype.loginClicked = function () {
    this.state = STATE_CONNECTING;

    console.debug("connecting to flack server at " + this.url);

    this.ws = new WebSocket(this.url);
    this.ws.onopen = this.onOpen.bind(this);
    this.ws.onclose = this.onClose.bind(this);
    this.ws.onmessage = this.onMessage.bind(this);

    this.username = this.loginInput.val();

    this.checkInputEnabled();
};

FlackApp.prototype.logoutClicked = function () {
    console.debug('logout clicked, sending goodbye to flack server');
    this.send({'record': 'client_goodbye'});

    this.close();
};

FlackApp.prototype.joinClicked = function () {
    this.state = STATE_JOINING;

    let name = this.joinInput.val();
    console.debug("joining chat " + name);

    let join = {'record': 'join_chat', 'chat_name': name};
    this.send(join);

    this.chats[name] = new ChatRoom(name, this.user_id, this.roomList, this.content, this.roomSelected.bind(this), this.leaveClicked.bind(this));

    this.checkInputEnabled();
};

FlackApp.prototype.sendClicked = function () {
    let text = this.chatInput.val();
    if (this.currentChat !== null) {
        let activeChat = this.currentChat.name;
        console.debug("sending chat message to room " + activeChat + ": " + text);

        let message = {'record': 'chat_message', 'chat_name': activeChat, 'mime_type': 'text/plain', 'message': text};
        this.send(message);

        this.chatInput.val('');
        this.checkInputEnabled();
    }
};

FlackApp.prototype.roomSelected = function (name) {
    console.debug("room " + name + " selected");
    if (this.currentChat) {
        this.currentChat.hide();
    }
    if (this.chats.hasOwnProperty(name)) {
        this.currentChat = this.chats[name];
        this.currentChat.show();
        this.chatInput.val('');
    }
    this.checkInputEnabled();
};

FlackApp.prototype.leaveClicked = function (name) {
    console.debug("sending leave_chat for room " + name);

    let message = {'record': 'leave_chat', 'chat_name': name};
    this.send(message)

    delete this.chats[name];

    if ((this.currentChat !== null) && (this.currentChat.name === name)) {
        this.currentChat = null;
        this.selectNextChat(name);
    }
};

FlackApp.prototype.onOpen = function () {
    this.state = STATE_CONNECTED;

    console.debug("connected to flack server");

    this.send({'record': 'client_hello', 'protocol_version': '1.0'});
};

FlackApp.prototype.onClose = function (close) {
    console.debug("disconnected from server while in state " + this.state + ", code " + close.code + " because " + close.reason);
    this.fail();
};

FlackApp.prototype.onMessage = function (event) {
    let message = null;
    try {
        message = JSON.parse(event.data);
    } catch (error) {
        // fall through
    }
    if (message !== null) {
        if (this.dispatchMessage(message)) {
            this.onActive();
            return;
        }
    }
    console.debug("received unexpected message from flack server: " + event.data);
    this.fail();
};

FlackApp.prototype.close = function () {
    this.ws.close();
    this.disconnected();
};

FlackApp.prototype.disconnected = function () {
    this.state = STATE_DISCONNECTED;
    this.ws.onclose = null;
    this.ws = null;
    Object.keys(this.chats).forEach((name) => {
        this.chats[name].close();
    });
    this.chats = {};
    this.currentChat = null;

    if (this.pingTimeout !== null) {
        clearTimeout(this.pingTimeout);
        this.pingTimout = null;
    }

    this.chatInput.val('');

    this.checkInputEnabled();
};

FlackApp.prototype.fail = function () {
    this.disconnected();
};

FlackApp.prototype.dispatchMessage = function (message) {
    switch (message.record) {
        case "protocol_error":
            this.onProtocolError(message);
            break;
        case "server_hello":
            this.onServerHello(message);
            break;
        case "authenticated":
            this.onAuthenticated(message);
            break;
        case "chat_state":
            this.onChatState(message);
            break;
        case "joined":
            this.onJoined(message);
            break;
        case "chat_message":
            this.onChatMessage(message);
            break;
        case "left":
            this.onLeft(message);
            break;
        case "ping_reply":
            break;
        default:
            return false;
    }
    return true;
};

FlackApp.prototype.onProtocolError = function (message) {
    console.debug("received protocol_error message from flack server, code=" + message.code + ", reason=" + message.reason);
    this.fail();
};

FlackApp.prototype.onServerHello = function (message) {
    console.debug("received server_hello for protocol version " + message.protocol_version);

    this.state = STATE_AUTHENTICATING;

    let authenticate = {'record': 'authenticate', 'user_name': this.username};
    this.send(authenticate);
};

FlackApp.prototype.onAuthenticated = function (message) {
    console.debug("authenticted with flack server as user_id " + message.user_id);

    this.state = STATE_AUTHENTICATED;

    this.user_id = message.user_id;

    this.logoutText.text(this.username);
    this.checkInputEnabled();
};

FlackApp.prototype.onChatState = function (message) {
    let users = message['users'].map((user) => { return user['user_name']; }).join(', ');
    console.debug("received chat_state from flack server with users " + users);
    this.chats[message.chat_name].setState(message['users']);
};

FlackApp.prototype.onJoined = function (message) {
    let name = message.chat_name;
    console.debug("received joined from flack server for chat " + name + " and user " + message.user_name);

    // TODO: make sure not to set state to authenticated while in a state other than joining
    this.state = STATE_AUTHENTICATED;

    if (this.chats.hasOwnProperty(name)) {
        this.chats[name].userJoined(message.user_name, message.user_id);
        if (message.user_id == this.user_id) {
            if (this.currentChat !== null) {
                this.currentChat.hide();
            }
            this.currentChat = this.chats[name];
            $('.room.active', this.roomList).removeClass('active');
            this.chatInput.val('');

            this.currentChat.enable();

            this.joinInput.val('');
            this.checkInputEnabled();
        }
    }
};

FlackApp.prototype.onChatMessage = function (message) {
    let name = message.chat_name;
    console.debug("received message for chat " + name + " from user " + message.user_id + ": " + message.message);
    if (this.chats.hasOwnProperty(name)) {
        this.chats[message.chat_name].messageReceived(message.user_id, message.message, message.timestamp, message.sequence);
    }
};

FlackApp.prototype.onLeft = function (message) {
    let name = message.chat_name;
    console.debug("received left from flack server for chat " + name + " for user " + message.user_id);

    if (this.chats.hasOwnProperty(name)) {
        this.chats[name].userLeft(message.user_id, message.sequence);
    }
};

FlackApp.prototype.selectNextChat = function (name) {
    let names = Object.keys(this.chats);
    if (names.length > 0) {
        names.sort();
        var next = null;
        names.forEach((chat) => {
            if ((chat.name > name) && (next === null)) {
                next = chat;
            }
        });
        if (next === null) {
            let name = names[names.length - 1];
            next = this.chats[name];
        }
        this.currentChat = next;
        next.show();
    }
    this.checkInputEnabled();
}

FlackApp.prototype.send = function (message) {
    if (this.ws !== null) {
        this.onActive();
        this.ws.send(JSON.stringify(message));
    }
}


FlackApp.prototype.onActive = function () {
    if (this.pingTimeout !== null) {
        clearTimeout(this.pingTimeout);
    }
    this.pingTimeout = setTimeout(this.onIdle.bind(this), PING_TIMEOUT);
};

FlackApp.prototype.onIdle = function () {
    this.pingTimeout = null;
    this.send({'record': 'ping'});
};

module.exports = FlackApp;

