#!/usr/bin/python3

import datetime
import json
import optparse
import time
import websocket

CLIENT_STATE_DISCONNECTED = 0
CLIENT_STATE_CONNECTING = 1
CLIENT_STATE_HANDSHAKING = 2
CLIENT_STATE_AUTHENTICATING = 3
CLIENT_STATE_CHAT_STATE = 4
CLIENT_STATE_JOINING1 = 5
CLIENT_STATE_JOINING2 = 6
CLIENT_STATE_SENT_MESSAGE = 7
CLIENT_STATE_SENT_PING = 8
CLIENT_STATE_SENT_LEAVE = 9
CLIENT_STATE_SENT_GOODBYE = 10
CLIENT_STATE_CLOSED = 11

state_names = {
    CLIENT_STATE_DISCONNECTED: 'disconnected',
    CLIENT_STATE_CONNECTING: 'connecting',
    CLIENT_STATE_HANDSHAKING: 'handshaking',
    CLIENT_STATE_AUTHENTICATING: 'authenticating',
    CLIENT_STATE_JOINING1: 'joining1',
    CLIENT_STATE_JOINING2: 'joining2',
    CLIENT_STATE_SENT_MESSAGE: 'message',
    CLIENT_STATE_SENT_PING: 'ping',
    CLIENT_STATE_SENT_LEAVE: 'leave',
    CLIENT_STATE_SENT_GOODBYE: 'goodbye',
    CLIENT_STATE_CLOSED: 'closed'
}

def state_to_name(state):
    name = state_names.get(state, None)
    return 'unknown' if not name else name

class ProtocolError(Exception):
    pass

class Client(object):
    def __init__(self, url, username, chat, count, delay, ws=websocket, tm=time):
        self.conn = None
        self.url = url
        self.username = username
        self.chat = chat
        self.count = count
        self.delay = delay

        self.ws = ws
        self.tm = tm

        self.user_id = None
        self.chat_users = {}

        self.state = CLIENT_STATE_DISCONNECTED

        self.messageDispatch = {
            'protocol_error': self.protocolError,
            'server_hello': self.serverHello,
            'authenticated': self.authenticated,
            'chat_state': self.chatState,
            'joined': self.joined,
            'chat_message': self.chatMessage,
            'left': self.left,
            'ping_reply': self.pingReply,
            'server_goodbye': self.serverGoodbye
        }

    def run(self):
        self.conn = self.ws.WebSocketApp(url=self.url, on_open=self.onOpen, on_message=self.onMessage, on_error=self.onError, on_close=self.onClose)
        self.conn.run_forever()

    def close(self):
        if self.conn:
            self.conn.close()

    def onOpen(self):
        print("connected, sending client_hello")
        client_hello = {"record": "client_hello", "protocol_version": "1.0"}
        self.conn.send(json.dumps(client_hello))
        self.state = CLIENT_STATE_HANDSHAKING

    def onMessage(self, raw_message):
        failed = False
        try:
            message = json.loads(raw_message)

            record_type = message.get('record', None)
            if not record_type:
                raise ProtocolError("message from server missing record type field: {0}".format(message))

            handler = self.messageDispatch.get(record_type, None)
            if not handler:
                raise ProtocolError("unrecognized message from server: {0}".format(message))

            handler(message)
        except json.decoder.JSONDecodeError:
            print("non-JSON message received from server: {0}".format(raw_message))
            failed = True
        except ProtocolError as e:
            print(e)
            failed = True

        if failed:
            self.conn.close()

    def onError(self, error):
        print("\nwebsocket error: {0}".format(error))

    def onClose(self):
        print("\nwebsocket closed")

    def protocolError(self, message):
        self.validate({'record': 'protocol_error', 'code': str, 'reason': str}, message)
        print('received protcol_error message: {0}, {1}'.format(message['code'], message['reason']))
        self.conn.close()

    def serverHello(self, message):
        if self.state != CLIENT_STATE_HANDSHAKING:
            raise ProtocolError('received server_hello message in state {0}'.format(state_to_name(self.state)))
        self.validate({'record': 'server_hello', 'protocol_version': '1.0'}, message)

        print('received server_hello message, sending authenticate')
        authenticate = {'record': 'authenticate', 'user_name': self.username}
        self.conn.send(json.dumps(authenticate))
        self.state = CLIENT_STATE_AUTHENTICATING

    def authenticated(self, message):
        if self.state != CLIENT_STATE_AUTHENTICATING:
            raise ProtocolError('received authenticated message in state {0}'.format(state_to_name(self.state)))
        self.validate({'record': 'authenticated', 'user_id': str}, message)

        print('received authenticate message, sending join_chat for {0}'.format(self.chat))
        self.user_id = message['user_id']
        join = {"record": "join_chat", "chat_name": self.chat}
        self.conn.send(json.dumps(join))
        self.state = CLIENT_STATE_JOINING1

    def chatState(self, message):
        if self.state != CLIENT_STATE_JOINING1:
            raise ProtocolError('received chat_state message in state {0}'.format(state_to_name(self.state)))
        self.validate({'record': 'chat_state', 'chat_name': self.chat, 'users': [{'record': 'chat_user', 'user_name': str, 'user_id': str}]}, message)
        for user in message['users']:
            print('{0} is already in the chat'.format(user['user_name']))
            self.chat_users[user['user_id']] = user['user_name']
        self.state = CLIENT_STATE_JOINING2

    def joined(self, message):
        self.validate({'record': 'joined', 'chat_name': self.chat, 'user_name': str, 'user_id': str, 'timestamp': str, 'sequence': int}, message)
        user_id = message['user_id']
        self.chat_users[user_id] = message['user_name']
        if user_id == self.user_id:
            if self.state != CLIENT_STATE_JOINING2:
                raise ProtocolError('received joined message in state {0}'.format(state_to_name(self.state)))
            print('you joined the chat')
            chat_message = {'record': 'chat_message', 'chat_name': self.chat, 'mime_type': 'text/plain', 'message': 'I like cats'}
            self.conn.send(json.dumps(chat_message))
            self.state = CLIENT_STATE_SENT_MESSAGE
        else:
            if self.state != CLIENT_STATE_SENT_MESSAGE and self.state != CLIENT_STATE_SENT_PING and self.state != CLIENT_STATE_SENT_LEAVE:
                raise ProtocolError('received joined message in state {0}'.format(state_to_name(self.state)))
            print('{0} joined the chat'.format(message['user_name']))

    def chatMessage(self, message):
        if self.state != CLIENT_STATE_SENT_MESSAGE and self.state != CLIENT_STATE_SENT_PING and self.state != CLIENT_STATE_SENT_LEAVE:
            raise ProtocolError('received chat_message in state {0}'.format(state_to_name(self.state)))
        self.validate({'record': 'chat_message', 'chat_name': self.chat, 'user_id': str, 'mime_type': 'text/plain', 'message': str, 'timestamp': datetime.datetime, 'sequence': int}, message)
        if message['user_id'] not in self.chat_users:
            raise ProtocolError("received chat_message from user {0} not in the chat: {1}".format(message['user_id'], message))

        utc = datetime.datetime.strptime(message['timestamp'], "%Y-%m-%dT%H:%M:%SZ")
        local = self.utc2local(utc)
        print("{0}: ({1}) {2}".format(self.chat_users[message['user_id']], datetime.datetime.strftime(local, "%H:%M"), message['message']))
        if message['user_id'] == self.user_id:
            self.count -= 1
            if self.count <= 1:
                leave = {'record': 'leave_chat', 'chat_name': self.chat}
                self.conn.send(json.dumps(leave))
                self.state = CLIENT_STATE_SENT_LEAVE
            else:
                ping = {'record': 'ping'}
                self.conn.send(json.dumps(ping))
                self.state = CLIENT_STATE_SENT_PING

    def left(self, message):
        if self.state != CLIENT_STATE_SENT_MESSAGE and self.state != CLIENT_STATE_SENT_PING and self.state != CLIENT_STATE_SENT_LEAVE:
            raise ProtocolError('received chat_message in state {0}'.format(state_to_name(self.state)))
        self.validate({'record': 'left', 'chat_name': self.chat, 'user_id': str, 'timestamp': datetime.datetime, 'sequence': int}, message)
        if message['user_id'] not in self.chat_users:
            raise ProtocolError("received left message for user not in chat: {0}".format(message))

        user = self.chat_users[message['user_id']]
        del self.chat_users[message['user_id']]

        if message['user_id'] == self.user_id:
            if self.state != CLIENT_STATE_SENT_LEAVE:
                raise ProtocolError("received left message for self without sending leave_chat message: {0}".format(message))
            print("you left the chat")
            goodbye = {'record': 'client_goodbye'}
            self.conn.send(json.dumps(goodbye))
            self.state = CLIENT_STATE_SENT_GOODBYE
        else:
            print("{0} left the chat".format(user))

    def pingReply(self, message):
        if self.state != CLIENT_STATE_SENT_PING:
            raise ProtocolError('received ping_reply message in state {0}'.format(state_to_name(self.state)))
        self.validate({'record': 'ping_reply'}, message)

        print("recieved ping_reply message")
        self.tm.sleep(float(self.delay) / 1000.0)

        chat_message = {'record': 'chat_message', 'chat_name': self.chat, 'mime_type': 'text/plain', 'message': 'I like cats'}
        self.conn.send(json.dumps(chat_message))
        self.state = CLIENT_STATE_SENT_MESSAGE

    def serverGoodbye(self, message):
        if self.state != CLIENT_STATE_SENT_GOODBYE:
            raise ProtocolError('received server_goodbye in state {0}'.format(state_to_name(self.state)))
        self.validate({'record': 'server_goodbye'}, message)

        print("received server_goodbye, closing connection")
        self.conn.close()
        self.state = CLIENT_STATE_CLOSED

    def validate(self, fields, message):
        if sorted(fields.keys()) != sorted(message.keys()):
            raise ProtocolError('invalid {0} record received from server: {1}'.format(message['record'], message))
        for key in fields:
            value = message[key]
            if type(fields[key]) == list:
                if type(value) != list:
                    raise ProtocolError('field {0} of {1} record is expected to be a list: {2}'.format(key, message['record'], message))
                for item in value:
                    self.validate(fields[key][0], item)
            elif fields[key] == datetime.datetime:
                try:
                    datetime.datetime.strptime(value, "%Y-%m-%dT%H:%M:%SZ")
                except ValueError:
                    raise ProtocolError('field {0} of {1} record is not a valid ISO-8601 datetime: {2}'.format(key, message['record'], message))
            elif type(fields[key]) == type:
                if type(value) != fields[key]:
                    raise ProtocolError('field {0} of {1} record does not have the expected type: {2}'.format(key, message['record'], message))
                if type(value) == str and not value:
                    raise ProtocolError('field {0} of {1} record should not be empty: {2}'.format(key, message['record'], message))
            elif value != fields[key]:
                raise ProtocolError('field {0} of {1} record is expected to have the value {2}: {3}'.format(key, message['record'], fields[key], message))

    def utc2local(self, utc):
        epoch = time.mktime(utc.timetuple())
        offset = datetime.datetime.fromtimestamp(epoch) - datetime.datetime.utcfromtimestamp(epoch)
        return utc + offset

def main(args):
    parser = optparse.OptionParser(usage="usage: %prog --url=<url> [--name=<user name>] [--chat=<room name>] [--count=<count>] [-d|--delay=<interval>]")
    parser.add_option("--url", dest="url", help="url to use to connect to server")
    parser.add_option("--name", dest="username", default="hoser", help="username for (fake) authentication")
    parser.add_option("--chat", dest="chat", default="foobar", help="chat name")
    parser.add_option("--count", dest="count", type="int", default=100, help="number of messages to send before exiting")
    parser.add_option("-d", "--delay", dest="delay", type="int", default=1000, help="number of milliseconds to wait between messages")
    options, extra = parser.parse_args(args)

    if extra[1:]:
        print("Extra arguments detected: {0}".format(extra))
        parser.print_help()
        return 1

    if not options.url:
        print("--url option is required")
        parser.print_help()
        return 1

    c = Client(options.url, options.username, options.chat, options.count, options.delay)
    try:
        c.run()
    except KeyboardInterrupt:
        print("\nexiting on KeyboardInterrupt")

    return 0

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))

