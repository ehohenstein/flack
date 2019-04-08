
function FlackApp (url) {
    this.url = url;
    this.s = null;
}

FlackApp.prototype.start = function () {
    console.log("connecting to flack server at " + this.url);
    this.s = new WebSocket(this.url);
}

module.exports = FlackApp;

