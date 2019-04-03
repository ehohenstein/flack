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

    def close(self):
        self.running = False

class ClientTests(unittest.TestCase):
    def setUp(self):
        self.url = 'ws://foobar:4242/some/path'
        self.ws = FakeWebSocket()
        self.username = "user1234"
        self.chat = "chat5678"
        self.user_id = "someone"
        self.now = "2015-11-12T08:15:51Z"
        self.client = client.Client(url=self.url, username=self.username, chat=self.chat, timeout=None, ws=self.ws)
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

    def test_closes_on_bad_json_message(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, 'this is not json')
        self.assertFalse(self.ws.running)

    def test_closes_on_bad_record(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, "{}")
        self.assertFalse(self.ws.running)

    def test_closes_on_protocol_error(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "protocol_error", "code": "ERR-123", "reason": "I don't like you very much"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_invalid_server_hello(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello"}))
        self.assertFalse(self.ws.running)

    def test_sends_join_on_valid_server_hello(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        join = json.loads(self.ws.sent_frames[1])
        self.assertEqual({"record": "join_chat", "chat_name": self.chat}, join)

    def test_closes_on_non_server_hello_message_first(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "ping_reply"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_second_server_hello(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": "someone_else"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_bad_chat_state_message(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_wrong_chat_name_in_chat_state_message(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": "other_chat", "users": []}))
        self.assertFalse(self.ws.running)

    def test_closes_on_second_chat_state_message(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.assertFalse(self.ws.running)

    def test_closes_on_bad_joined_message(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.client.onMessage(self.ws, json.dumps({"record": "joined"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_joined_before_chat_state(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "joined", "chat_name": self.chat, "user_name": self.username, "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_wrong_joined_chat_name(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.client.onMessage(self.ws, json.dumps({"record": "joined", "chat_name": "other_chat", "user_name": self.username, "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_other_user_name_before_self_joined(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.client.onMessage(self.ws, json.dumps({"record": "joined", "chat_name": self.chat, "user_name": "other_user", "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_other_user_name_before_self_joined(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.client.onMessage(self.ws, json.dumps({"record": "joined", "chat_name": self.chat, "user_name": self.username, "user_id": "other_user", "timestamp": self.now, "sequence": 42}))
        self.assertFalse(self.ws.running)

    def test_can_receive_other_user_join_after_self_join(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.client.onMessage(self.ws, json.dumps({"record": "joined", "chat_name": self.chat, "user_name": self.username, "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))
        self.client.onMessage(self.ws, json.dumps({"record": "joined", "chat_name": self.chat, "user_name": "other_user", "user_id": "other_user_id", "timestamp": self.now, "sequence": 42}))
        self.assertTrue(self.ws.running)

    def test_sends_chat_message_after_joining(self):
        self.client.onOpen(self.ws)
        self.client.onMessage(self.ws, json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": self.user_id}))
        self.client.onMessage(self.ws, json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.client.onMessage(self.ws, json.dumps({"record": "joined", "chat_name": self.chat, "user_name": self.username, "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))
        chat_message = json.loads(self.ws.sent_frames[2])
        self.assertEqual({'record': 'chat_message', 'chat_name': self.chat, 'mime_type': 'text/plain', 'message': 'I like cats'}, chat_message)

    def test_closes_on_bad_chat_message(self):
        pass

    def test_closes_on_chat_message_from_user_not_in_chat(self):
        pass

    def test_allows_chat_from_user_in_chat(self):
        pass

    def test_sends_ping_on_chat_from_self(self):
        pass

    def test_closes_on_bad_left_message(self):
        pass

    def test_closes_on_left_message_for_user_not_in_chat(self):
        pass

    def test_closes_when_self_left(self):
        pass

    def test_removes_user_on_left(self):
        pass

    def test_closes_on_message_before_chat_state(self):
        pass

    def test_closes_on_left_before_chat_state(self):
        pass

    def test_closes_on_ping_reply_before_chat_state(self):
        pass

    def test_message_validation_fails_for_extra_fields(self):
        self.assertFalse(self.client.validate({'record': 'foobar'}, {'record': 'foobar', 'foo': 'bar'}))

    def test_message_validation_fails_for_missing_fields(self):
        self.assertFalse(self.client.validate({'record': 'foobar', 'foo': str}, {'record': 'foobar'}))

    def test_message_validation_fails_for_wrong_field_type(self):
        self.assertFalse(self.client.validate({'record': 'foobar', 'foo': str}, {'record': 'foobar', 'foo': 42}))

    def test_message_validation_fails_for_empty_string_type(self):
        self.assertFalse(self.client.validate({'record': 'foobar', 'foo': str}, {'record': 'foobar', 'foo': ''}))

    def test_message_validation_fails_for_wrong_exact_value(self):
        self.assertFalse(self.client.validate({'record': 'foobar', 'foo': 'bar'}, {'record': 'foobar', 'foo': 'baz'}))

    def test_message_validation_fails_for_non_list_in_list_field(self):
        self.assertFalse(self.client.validate({'record': 'foobar', 'foo': [{'record': 'bar'}]}, {'record': 'foobar', 'foo': 'bar'}))

    def test_message_validation_fails_for_bad_list_field(self):
        self.assertFalse(self.client.validate({'record': 'foobar', 'foo': [{'record': 'bar'}]}, {'record': 'foobar', 'foo': [{'record': 'baz'}]}))

    def test_message_validation_passes_for_valid_message(self):
        self.assertTrue(self.client.validate({'record': 'foobar', 'foo': str}, {'record': 'foobar', 'foo': 'baz'}))

    def test_message_validation_passes_for_valid_list_field(self):
        self.assertTrue(self.client.validate({'record': 'foobar', 'foo': [{'record': 'bar'}]}, {'record': 'foobar', 'foo': [{'record': 'bar'}]}))

unittest.main()

