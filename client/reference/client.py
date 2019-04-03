#!/usr/bin/python3

import json
import optparse
import websocket

class Client(object):
    def __init__(self, url, username, chat, ws=websocket):
        self.conn = None
        self.url = url
        self.username = username
        self.chat = chat
        self.ws = ws

    def run(self):
        self.conn = self.ws.WebSocketApp(url=self.url, on_open=self.onOpen, on_message=self.onMessage, on_error=self.onError, on_close=self.onClose)
        self.conn.run_forever()

    def close(self):
        if self.conn:
            self.conn.close()

    def onOpen(self, ws):
        client_hello = {"record": "client_hello", "protocol_version": "1.0", "username": self.username}
        self.conn.send(json.dumps(client_hello))

    def onMessage(self, ws, message):
        pass

    def onError(self, ws, error):
        pass

    def onClose(self, ws):
        pass

def main(args):
    parser = optparse.OptionParser(usage="usage: %prog --server=<server> --port=<port>")
    parser.add_option("-u", "--url", dest="url", help="url to use to connect to server")
    parser.add_option("-n", "--name", dest="username", default="hoser", help="username for (fake) authentication")
    parser.add_option("-c", "--chat", dest="chat", default="foobar", help="chat name")
    options, extra = parser.parse_args(args)

    if extra[1:]:
        print("Extra arguments detected: {0}".format(extra))
        parser.print_help()
        return 1

    if not options.url:
        print("--url option is required")
        parser.print_help()
        return 1

    c = Client(options.url, options.username, options.chat)
    try:
        c.run()
    except KeyboardInterrupt:
        print("\nexiting on KeyboardInterrupt")

    return 0

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))

