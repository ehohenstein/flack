
const str_combine = require('./str_combine');
const user_color = require('./user_color');

function ChatRoom(name, user_id, roomList, content, onSelect, onLeave) {
    this.name = name;
    this.user_id = user_id;
    this.roomList = roomList;
    this.content = content;

    this.onSelect = onSelect;
    this.onLeave = onLeave;

    this.listItem = $('<div></div>').addClass('list-item').addClass('room');
    this.listItem.click(this.onSelectClicked.bind(this));
    this.listItem.append($('<span class="name"></span>').text(this.name));
    this.listItem.append($('<button type="button" class="flack-button">Leave</button>'));
    $('button', this.listItem).click(this.onLeaveClicked.bind(this));

    this.messages = $('<div></div>').addClass('content-messages');

    this.users = {};
}

ChatRoom.prototype.onSelectClicked = function () {
    if (!this.listItem.hasClass('active')) {
        this.onSelect(this.name);
    }
};

ChatRoom.prototype.onLeaveClicked = function () {
    this.listItem.remove();
    this.messages.remove();
    this.onLeave(this.name);
};

ChatRoom.prototype.setState = function (users) {
    var names = [];
    users.forEach((user) => {
        names.push(user.user_name);
        this.users[user.user_id] = user.user_name;
    });

    if (names.length > 0) {
        let users_str = str_combine(names) + (names.length > 1 ? ' are' : ' is') + ' in the room';
        this.addMeta(users_str);
    }
};

ChatRoom.prototype.userJoined = function (user_name, user_id, sequence) {
    this.users[user_id] = user_name;

    let name = user_name;
    if (user_id == this.user_id) {
        name = 'you';
    }
    this.addMeta(name + ' joined the room', sequence);
};

ChatRoom.prototype.userLeft = function (user_id, sequence) {
    if (this.users.hasOwnProperty(user_id)) {
        let name = this.users[user_id];
        if (user_id == this.user_id) {
            // ?
            name = 'you';
        }
        this.addMeta(name + ' left the room', sequence);

        delete this.users[user_id];
    }
};

ChatRoom.prototype.messageReceived = function (user_id, text, timestamp, sequence) {
    if (this.users.hasOwnProperty(user_id)) {
        let name = this.users[user_id];
        this.addMessage(timestamp, name, user_id, text, sequence);
    }
};

ChatRoom.prototype.enable = function () {
    let rooms = $('.room', this.roomList);
    let after = null;
    rooms.each((index, room) => {
        let name = $('.name', room).text();
        if (name < this.name) {
            after = room;
        }
    });
    if (after === null) {
        this.roomList.append(this.listItem);
    } else {
        this.listItem.insertAfter(after);
    }

    this.content.append(this.messages);

    this.show();
};

ChatRoom.prototype.show = function () {
    //TODO: de-select the currently selected room first
    this.listItem.addClass('active');
    this.messages.addClass('visible');
};

ChatRoom.prototype.hide = function () {
};

ChatRoom.prototype.addMeta = function (text, sequence) {
    let message = $('<div></div>').addClass('message');

    message.append($('<div></div>').addClass('timestamp'));

    let message_text = $('<div></div>').addClass('message-text');
    let paragraph = $('<p></p>');
    paragraph.append($('<span></span>').addClass('text').text(text));
    message_text.append(paragraph);
    message.append(message_text);

    this.messages.prepend(message);
};

ChatRoom.prototype.addMessage = function (timestamp, user, user_id, text, sequence) {
    let message = $('<div></div>').addClass('message');

    let date = new Date(timestamp);
    const twoDigit = function (d) {
        if (d < 10) {
            return '0' + d.toString();
        }
        return d.toString();
    };
    const twelveHour = function (d) {
        if (d === 0) {
            return '12';
        } else if (d > 12) {
            return (d - 12).toString();
        }
        return d.toString();
    };
    const amPm = function (d) {
        return (d < 12) ? 'am' : 'pm';
    };
    let dateStr = twelveHour(date.getHours()) + ':' + twoDigit(date.getMinutes()) + ':' + twoDigit(date.getSeconds()) + ' ' + amPm(date.getHours());
    message.append($('<div></div>').addClass('timestamp').text(dateStr));

    let paragraph = $('<p></p>');
    paragraph.append($('<span></span>').addClass('username').css('color', user_color(user_id)).text(user));
    paragraph.append($('<span>: </span>'));
    paragraph.append($('<span></span>').addClass('text').text(text));
    message.append(paragraph);

    this.messages.prepend(message);
};

module.exports = ChatRoom;
