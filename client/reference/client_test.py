#!/usr/bin/python3

import unittest

import client

class FakeWebSocket(object):
    def __init__(self):
        self.url = None
        self.onOpen = None
        self.onMessage = None
        self.onError = None
        self.onClose = None

    def WebSocketApp(self, url, on_open, on_message, on_error, on_close):
        self.url = url
        self.onMessage = on_message
        self.onError = on_error
        self.onClose = on_close
        return self

class ClientTests(unittest.TestCase):
    def setUp(self):
        self.url = 'ws://foobar:4242/some/path'
        self.ws = FakeWebSocket()
        self.continuing = False

    def should_continue(self):
        return self.continuing

    def test_client_connects_to_server(self):
        c = client.Client(url=self.url, ws=self.ws, should_continue=self.should_continue)
        c.run()
        self.assertEqual(self.url, self.ws.url)

unittest.main()

