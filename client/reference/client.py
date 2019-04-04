#!/usr/bin/python3

import datetime
import json
import optparse
import time
import websocket

class Client(object):
    def __init__(self, url, username, chat, number, delay, ws=websocket, tm=time):
        self.conn = None
        self.url = url
        self.username = username
        self.chat = chat
        self.number = number
        self.delay = delay

        self.ws = ws
        self.tm = tm

        self.user_id = None
        self.chat_users = None

        self.server_hello = False
        self.has_joined = False
        self.has_left = False

        self.messageDispatch = {
            'protocol_error': self.protocolError,
            'server_hello': self.serverHello,
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

    def onOpen(self, ws):
        client_hello = {"record": "client_hello", "protocol_version": "1.0", "username": self.username}
        self.conn.send(json.dumps(client_hello))

    def onMessage(self, ws, raw_message):
        message = None
        try:
            message = json.loads(raw_message)
        except json.decoder.JSONDecodeError:
            print("non-JSON message received from server: {0}".format(raw_message))
            self.conn.close()
            return
        record_type = message.get('record', None)
        if not record_type:
            print("message from server missing record type field: {0}".format(message))
            self.conn.close()
            return
        if not self.server_hello and record_type != 'server_hello':
            print("invalid message sequence from server prior to server_hello: {0}".format(message))
            self.conn.close()
            return
        if self.server_hello and record_type == 'server_hello':
            print("invalid message sequence from server, received second server_hello: {0}".format(message))
            self.conn.close()
            return
        handler = self.messageDispatch.get(record_type, None)
        if not handler:
            print("unrecognized message from server: {0}".format(message))
            self.conn.close()
            return
        handler(message)

    def onError(self, ws, error):
        print("\nwebsocket error: {0}".format(error))

    def onClose(self, ws):
        print("\nwebsocket closed")

    def protocolError(self, message):
        if self.validate({'record': 'protocol_error', 'code': str, 'reason': str}, message):
            print("protocol_error: {0}".format(message))
        self.conn.close()

    def serverHello(self, message):
        if not self.validate({'record': 'server_hello', 'protocol_version': '1.0', 'user_id': str}, message):
            self.conn.close()
            return
        self.user_id = message['user_id']
        join = {"record": "join_chat", "chat_name": self.chat}
        self.conn.send(json.dumps(join))
        self.server_hello = True

    def chatState(self, message):
        if not self.validate({'record': 'chat_state', 'chat_name': self.chat, 'users': [{'record': 'chat_user', 'user_name': str, 'user_id': str}]}, message):
            self.conn.close()
            return
        if self.chat_users != None:
            print("second chat_state record received: {0}".format(message))
            self.conn.close()
            return
        self.chat_users = {}

    def joined(self, message):
        if not self.validate({'record': 'joined', 'chat_name': self.chat, 'user_name': str, 'user_id': str, 'timestamp': str, 'sequence': int}, message):
            self.conn.close()
            return
        if self.chat_users == None:
            print("received joined message before chat_state message: {0}".format(message))
            self.conn.close()
            return
        if not self.has_joined:
            if message['user_name'] != self.username:
                print("received joined for other user before self: {0}".format(message))
                self.conn.close()
                return
            if message['user_id'] != self.user_id:
                print("received joined for other user before self: {0}".format(message))
                self.conn.close()
                return
            self.has_joined = True
        self.chat_users[message['user_id']] = message['user_name']
        chat_message = {'record': 'chat_message', 'chat_name': self.chat, 'mime_type': 'text/plain', 'message': 'I like cats'}
        self.conn.send(json.dumps(chat_message))

    def chatMessage(self, message):
        if not self.validate({'record': 'chat_message', 'chat_name': self.chat, 'user_id': str, 'mime_type': 'text/plain', 'message': str, 'timestamp': datetime.datetime, 'sequence': int}, message):
            self.conn.close()
            return
        if self.chat_users == None:
            print("received chat_message before chat_state: {0}".format(message))
            self.conn.close()
            return
        if message['user_id'] not in self.chat_users:
            print("received chat_message from user {0} not in the chat: {1}".format(message['user_id'], message))
            self.conn.close()
            return
        utc = datetime.datetime.strptime(message['timestamp'], "%Y-%m-%dT%H:%M:%SZ")
        local = self.utc2local(utc)
        print("{0}: ({1}) {2}".format(self.chat_users[message['user_id']], datetime.datetime.strftime(local, "%H:%M"), message['message']))
        if message['user_id'] == self.user_id:
            self.number -= 1
            if self.number <= 1:
                leave = {'record': 'leave_chat', 'chat_name': self.chat}
                self.conn.send(json.dumps(leave))
                self.has_left = True
            else:
                ping = {'record': 'ping'}
                self.conn.send(json.dumps(ping))

    def left(self, message):
        if not self.validate({'record': 'left', 'chat_name': self.chat, 'user_id': str, 'timestamp': datetime.datetime, 'sequence': int}, message):
            self.conn.close()
            return
        if self.chat_users == None:
            print("received left message before chat_state: {0}".format(message))
            self.conn.close()
            return
        if message['user_id'] not in self.chat_users:
            print("received left message for user not in chat: {0}".format(message))
            self.conn.close()
            return
        del self.chat_users[message['user_id']]
        if message['user_id'] == self.user_id:
            if not self.has_left:
                print("received left message for self without sending leave_chat message: {0}".format(message))
                self.conn.close()
                return
            goodbye = {'record': 'client_goodbye'}
            self.conn.send(json.dumps(goodbye))

    def pingReply(self, message):
        if not self.validate({'record': 'ping_reply'}, message):
            self.conn.close()
            return
        if self.chat_users == None:
            print("received ping_reply before chat_state message")
            self.conn.close()
            return
        if self.user_id not in self.chat_users:
            print("received ping_reply before joinging chat")
            self.conn.close()
            return
        self.tm.sleep(self.delay)
        chat_message = {'record': 'chat_message', 'chat_name': self.chat, 'mime_type': 'text/plain', 'message': 'I like cats'}
        self.conn.send(json.dumps(chat_message))

    def serverGoodbye(self, message):
        self.validate({'record': 'server_goodbye'}, message)
        print("received server_goodbye, closing connection")
        self.conn.close()

    def validate(self, fields, message):
        if sorted(fields.keys()) != sorted(message.keys()):
            print('invalid {0} record received from server: {1}'.format(message['record'], message))
            return False
        for key in fields:
            value = message[key]
            if type(fields[key]) == list:
                if type(value) != list:
                    print('field {0} of {1} record is expected to be a list: {2}'.format(key, message['record'], message))
                    return False
                for item in value:
                    if not self.validate(fields[key][0], item):
                        return False
            elif fields[key] == datetime.datetime:
                try:
                    datetime.datetime.strptime(value, "%Y-%m-%dT%H:%M:%SZ")
                except ValueError:
                    print('field {0} of {1} record is not a valid ISO-8601 datetime: {2}'.format(key, message['record'], message))
                    return False
            elif type(fields[key]) == type:
                if type(value) != fields[key]:
                    print('field {0} of {1} record does not have the expected type: {2}'.format(key, message['record'], message))
                    return False
                if type(value) == str and not value:
                    print('field {0} of {1} record should not be empty: {2}'.format(key, message['record'], message))
                    return False
            elif value != fields[key]:
                print('field {0} of {1} record is expected to have the value {2}: {3}'.format(key, message['record'], fields[key], message))
                return False
        return True

    def utc2local(self, utc):
        epoch = time.mktime(utc.timetuple())
        offset = datetime.datetime.fromtimestamp(epoch) - datetime.datetime.utcfromtimestamp(epoch)
        return utc + offset

def main(args):
    parser = optparse.OptionParser(usage="usage: %prog --server=<server> --port=<port>")
    parser.add_option("-u", "--url", dest="url", help="url to use to connect to server")
    parser.add_option("-n", "--name", dest="username", default="hoser", help="username for (fake) authentication")
    parser.add_option("-c", "--chat", dest="chat", default="foobar", help="chat name")
    parser.add_option("-n", "--number", dest="number", type="int", default=100, help="number of messages to send before exiting")
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

    c = Client(options.url, options.username, options.chat, options.number, options.delay)
    try:
        c.run()
    except KeyboardInterrupt:
        print("\nexiting on KeyboardInterrupt")

    return 0

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))

