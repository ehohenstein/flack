
function FakeTimer(func, timeout) {
    this.func = func;
    this.timeout = timeout;
    this.cancelled = false;
}

function FakeTimeout() {
    this.timers = [];
}

FakeTimeout.prototype.setTimeout = function (func, timeout) {
    let index = this.timers.length;
    this.timers.push(new FakeTimer(func, timeout));
    return index;
};

FakeTimeout.prototype.clearTimeout = function (index) {
    try {
        this.timers[index].cancelled = true;
    } catch (e) {
        throw index;
    }
};

FakeTimeout.prototype.getTimers = function (index) {
    return this.timers;
};

var timeout = new FakeTimeout();

module.exports = {
    setTimeout: timeout.setTimeout.bind(timeout),
    clearTimeout: timeout.clearTimeout.bind(timeout),
    timers: timeout.getTimers.bind(timeout)
};
