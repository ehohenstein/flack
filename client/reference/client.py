#!/usr/bin/python3

from optparse import OptionParser
import websocket

class Client(object):
    def __init__(self, url, ws=websocket, should_continue=lambda: True):
        self.conn = None
        self.url = url
        self.ws = ws
        self.should_continue = should_continue

    def run(self):
        self.conn = self.ws.WebSocketApp(self.url, self.onOpen, self.onMessage, self.onError, self.onClose)
        while self.should_continue():
            pass

    def close(self):
        if self.conn:
            self.conn.close()

    def onOpen(self, ws):
        pass

    def onMessage(self, ws, message):
        pass

    def onError(self, ws, error):
        pass

    def onClose(self, ws):
        pass

def main(args):
    parser = OptionParser(usage="usage: %prog --server=<server> --port=<port>")
    parser.add_option("-u", "--url", dest="url", help="url to use to connect to server")
    options, extra = parser.parse_args(args)

    if extra[1:]:
        print("Extra arguments detected: {0}".format(extra))
        parser.print_help()
        return 1

    if not options.url:
        print("--url option is required")
        parser.print_help()
        return 1

    c = Client(options.url)
    try:
        c.run()
    except KeyboardInterrupt:
        print("\nexiting on KeyboardInterrupt")

    return 0

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))

