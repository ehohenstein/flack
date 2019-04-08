
define(function () {
    function FlackApp (url) {
        this.url = url;
        this.s = null;
    }

    FlackApp.prototype.start = function () {
        this.s = new WebSocket(this.url);
    }

    return FlackApp;
});
