
const STATE_DISCONNECTED = 0;
const STATE_CONNECTING = 1;
const STATE_CONNECTED = 2;
const STATE_AUTHENTICATING = 3;
const STATE_AUTHENTICATED = 4;
const STATE_DISCONNECTING = 5;

function FlackApp (url) {
    this.url = url;
    this.ws = null;

    this.login = $('.login');
    this.loginInput = $('input', this.login);
    $('.login input').keypress(this.checkLogin.bind(this));
    this.loginButton = $('button', this.login);
    $('.login button').click(this.loginClicked.bind(this));

    this.logout = $('.logout');
    this.logoutText = $('.username', this.logout);
    this.logoutButton = $('button', this.logout);

    this.state = STATE_DISCONNECTED;

    this.username = null;

    this.init();
}

FlackApp.prototype.init = function () {
    this.checkLogin();
};

FlackApp.prototype.checkLogin = function () {
    if (this.state < STATE_AUTHENTICATED) {
        this.login.removeClass('hidden');
        this.logout.addClass('hidden');
        if (this.state === STATE_DISCONNECTED) {
            this.loginInput.prop('disabled', false);
            this.loginButton.prop('disabled', $('.login input').val() === '');
        } else if (this.state === STATE_CONNECTING) {
            this.loginInput.prop('disabled', true);
            this.loginButton.prop('disabled', true);
        }
    } else {
        this.login.addClass('hidden');
        this.logout.removeClass('hidden');
        this.logoutButton.prop('disabled', false);
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

    this.checkLogin();
};

FlackApp.prototype.onOpen = function () {
    this.state = STATE_CONNECTED;

    console.debug("connected to flack server");

    this.ws.send(JSON.stringify({'record': 'client_hello', 'protocol_version': '1.0'}));
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
            return;
        }
    }
    console.log("received unexpected message from flack server: " + event.data);
    this.fail();
};

FlackApp.prototype.fail = function () {
    this.state = STATE_DISCONNECTED;
    this.ws.onclose = null;
    this.ws = null;

    this.checkLogin();
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
        default:
            return false;
    }
    return true;
};

FlackApp.prototype.onProtocolError = function (message) {
    console.log("received protocol_error message from flack server, code=" + message.code + ", reason=" + message.reason);
    this.fail();
};

FlackApp.prototype.onServerHello = function (message) {
    console.log("received server_hello for protocol version " + message.protocol_version);

    this.state = STATE_AUTHENTICATING;

    let authenticate = {'record': 'authenticate', 'user_name': this.username};
    this.ws.send(JSON.stringify(authenticate));
};

FlackApp.prototype.onAuthenticated = function (message) {
    console.log("authenticted with flack server as user_id " + message.user_id);

    this.state = STATE_AUTHENTICATED;

    this.logoutText.text(this.username);
    this.checkLogin();
};

module.exports = FlackApp;

