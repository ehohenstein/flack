
globalSockets = [];

function FakeWebSocket(url) {
    this.url = url;
    this.onopen = null;
    this.onerror = null;
    this.onclose = null;
    this.ondata = null;

    this.frames_sent = [];

    globalSockets.push(this);
}

FakeWebSocket.prototype.send = function (data) {
    this.frames_sent.push(data);
};

function FakeCloseEvent(code, reason) {
    this.code = code;
    this.reason = reason;
}

function FakeMessageEvent(payload, encoded) {
    this.data = encoded ? payload : JSON.stringify(payload);
}

function reset() {
    while (globalSockets.length > 0) {
        globalSockets.pop();
    }
}

module.exports = {
    'FakeWebSocket': FakeWebSocket,
    'FakeCloseEvent': FakeCloseEvent,
    'FakeMessageEvent': FakeMessageEvent,
    'sockets': globalSockets,
    'reset': reset
};

