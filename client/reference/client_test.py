#!/usr/bin/python3

import json
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

        self.sent_frames = []

    def WebSocketApp(self, url, on_open, on_message, on_error, on_close):
        self.url = url
        self.onOpen = on_open
        self.onMessage = on_message
        self.onError = on_error
        self.onClose = on_close
        return self

    def run_forever(self):
        self.running = True

    def send(self, data):
        self.sent_frames.append(data)

class ClientTests(unittest.TestCase):
    def setUp(self):
        self.url = 'ws://foobar:4242/some/path'
        self.ws = FakeWebSocket()
        self.username = "user1234"
        self.chat = "chat5678"
        self.client = client.Client(url=self.url, username=self.username, chat=self.chat, ws=self.ws)
        self.client.run()

    def test_connects_to_server(self):
        self.assertEqual(self.url, self.ws.url)

    def test_assigns_callbacks(self):
        self.assertEqual(self.client.onOpen, self.ws.onOpen)
        self.assertEqual(self.client.onMessage, self.ws.onMessage)
        self.assertEqual(self.client.onError, self.ws.onError)
        self.assertEqual(self.client.onClose, self.ws.onClose)

    def test_runs_forever(self):
        self.assertTrue(self.ws.running)

    def test_sends_client_hello_on_open(self):
        self.client.onOpen(self.ws)
        client_hello = json.loads(self.ws.sent_frames[0])
        self.assertEqual({"record": "client_hello", "protocol_version": "1.0", "username": self.username}, client_hello)


unittest.main()

