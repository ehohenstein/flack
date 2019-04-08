
define(["./flack"], function (flack) {
    new flack('ws://flack:8080/chat-server').start();
});

