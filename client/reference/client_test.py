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
        self.running = False

    def WebSocketApp(self, url, on_open, on_message, on_error, on_close):
        self.url = url
        self.onMessage = on_message
        self.onError = on_error
        self.onClose = on_close
        return self

    def run_forever(self):
        self.running = True

class ClientTests(unittest.TestCase):
    def setUp(self):
        self.url = 'ws://foobar:4242/some/path'
        self.ws = FakeWebSocket()
        self.client = client.Client(url=self.url, ws=self.ws)

    def test_client_connects_to_server(self):
        self.client.run()
        self.assertEqual(self.url, self.ws.url)

    def test_client_runs_forever(self):
        self.client.run()
        self.assertTrue(self.ws.running)

unittest.main()

