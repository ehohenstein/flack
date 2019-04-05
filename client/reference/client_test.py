#!/usr/bin/python3

import datetime
import json
import unittest

import client

class FakeTime(object):
    def __init__(self):
        self.slept_ms = 0

    def sleep(self, ms):
        self.slept_ms += ms

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
        self.ws = FakeWebSocket()
        self.tm = FakeTime()

        self.url = 'ws://foobar:4242/some/path'
        self.username = "user1234"
        self.chat = "chat5678"
        self.count = 1000
        self.delay = 0

        self.user_id = "someone"
        self.now = "2015-11-12T08:15:51Z"
        self.client = None

    def start_client(self):
        self.client = client.Client(url=self.url, username=self.username, chat=self.chat, count=self.count, delay=self.delay, ws=self.ws, tm=self.tm)
        self.client.run()
        self.client.onOpen()

    def send_valid_server_hello(self):
        self.start_client()
        self.client.onMessage(json.dumps({"record": "server_hello", "protocol_version": "1.0"}))

    def send_valid_authenticated(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "authenticated", "user_id": self.user_id}))

    def send_valid_chat_state(self):
        self.send_valid_authenticated()
        self.client.onMessage(json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))

    def send_valid_joined(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "joined", "chat_name": self.chat, "user_name": self.username, "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))

    def send_valid_chat(self):
        self.send_valid_joined()
        self.client.onMessage(json.dumps({"record": "chat_message", 'chat_name': self.chat, 'user_id': self.user_id, 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': self.now, 'sequence': 43}))

    def send_valid_ping(self):
        self.send_valid_chat()
        self.client.onMessage(json.dumps({"record": "ping_reply"}))

    def send_valid_left(self):
        self.count = 1
        self.send_valid_chat()
        self.client.onMessage(json.dumps({"record": "left", 'chat_name': self.chat, 'user_id': self.user_id, 'timestamp': self.now, 'sequence': 44}))

    def test_connects_to_server(self):
        self.start_client()
        self.assertEqual(self.url, self.ws.url)

    def test_assigns_callbacks(self):
        self.start_client()
        self.assertEqual(self.client.onOpen, self.ws.onOpen)
        self.assertEqual(self.client.onMessage, self.ws.onMessage)
        self.assertEqual(self.client.onError, self.ws.onError)
        self.assertEqual(self.client.onClose, self.ws.onClose)

    def test_runs_forever(self):
        self.start_client()
        self.assertTrue(self.ws.running)

    def test_sends_client_hello_on_open(self):
        self.start_client()
        client_hello = json.loads(self.ws.sent_frames[0])
        self.assertEqual({"record": "client_hello", "protocol_version": "1.0"}, client_hello)

    def test_closes_on_bad_json_message(self):
        self.start_client()
        self.client.onMessage('this is not json')
        self.assertFalse(self.ws.running)

    def test_closes_on_bad_record(self):
        self.start_client()
        self.client.onMessage("{}")
        self.assertFalse(self.ws.running)

    def test_closes_on_protocol_error(self):
        self.start_client()
        self.client.onMessage(json.dumps({"record": "protocol_error", "code": "ERR-123", "reason": "I don't like you very much"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_invalid_server_hello(self):
        self.start_client()
        self.client.onMessage(json.dumps({"record": "server_hello"}))
        self.assertFalse(self.ws.running)

    def test_sends_authenticate_on_valid_server_hello(self):
        self.send_valid_server_hello()
        authenticate = json.loads(self.ws.sent_frames[1])
        self.assertEqual({"record": "authenticate", "user_name": self.username}, authenticate)

    def test_closes_on_invalid_authenticated(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "authenticated"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_authenticated_in_wrong_state(self):
        self.send_valid_authenticated()
        self.client.onMessage(json.dumps({"record": "authenticated", "user_id": self.user_id}))
        self.assertFalse(self.ws.running)

    def test_sends_join_on_valid_authenticated(self):
        self.send_valid_authenticated()
        join = json.loads(self.ws.sent_frames[2])
        self.assertEqual({"record": "join_chat", "chat_name": self.chat}, join)

    def test_closes_on_non_server_hello_message_first(self):
        self.start_client()
        self.client.onMessage(json.dumps({"record": "ping_reply"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_second_server_hello(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "server_hello", "protocol_version": "1.0", "user_id": "someone_else"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_bad_chat_state_message(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "chat_state"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_wrong_chat_name_in_chat_state_message(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "chat_state", "chat_name": "other_chat", "users": []}))
        self.assertFalse(self.ws.running)

    def test_closes_on_second_chat_state_message(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "chat_state", "chat_name": self.chat, "users": []}))
        self.assertFalse(self.ws.running)

    def test_closes_on_bad_joined_message(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "joined"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_joined_before_chat_state(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "joined", "chat_name": self.chat, "user_name": self.username, "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_wrong_joined_chat_name(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "joined", "chat_name": "other_chat", "user_name": self.username, "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_other_user_name_before_self_joined(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "joined", "chat_name": self.chat, "user_name": "other_user", "user_id": self.user_id, "timestamp": self.now, "sequence": 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_other_user_name_before_self_joined(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "joined", "chat_name": self.chat, "user_name": self.username, "user_id": "other_user", "timestamp": self.now, "sequence": 42}))
        self.assertFalse(self.ws.running)

    def test_can_receive_other_user_join_after_self_join(self):
        self.send_valid_joined()
        self.client.onMessage(json.dumps({"record": "joined", "chat_name": self.chat, "user_name": "other_user", "user_id": "other_user_id", "timestamp": self.now, "sequence": 42}))
        self.assertTrue(self.ws.running)

    def test_sends_chat_message_after_joining(self):
        self.send_valid_joined()
        chat_message = json.loads(self.ws.sent_frames[3])
        self.assertEqual({'record': 'chat_message', 'chat_name': self.chat, 'mime_type': 'text/plain', 'message': 'I like cats'}, chat_message)

    def test_closes_on_bad_chat_message(self):
        self.send_valid_joined()
        self.client.onMessage(json.dumps({"record": "chat_message"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_chat_message_for_wrong_chat(self):
        self.send_valid_joined()
        self.client.onMessage(json.dumps({"record": "chat_message", 'chat_name': 'other_chat', 'user_id': self.user_id, 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_chat_message_from_user_not_in_chat(self):
        self.send_valid_joined()
        self.client.onMessage(json.dumps({"record": "chat_message", 'chat_name': self.chat, 'user_id': 'other_user', 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_allows_chat_from_user_in_chat(self):
        self.send_valid_chat()
        self.assertTrue(self.ws.running)

    def test_sends_ping_on_chat_from_self_when_not_done(self):
        self.send_valid_chat()
        ping = json.loads(self.ws.sent_frames[4])
        self.assertEqual({"record": "ping"}, ping)

    def test_sends_leave_chat_on_chat_from_self_when_done(self):
        self.count = 1
        self.send_valid_chat()
        leave_chat = json.loads(self.ws.sent_frames[4])
        self.assertEqual({"record": "leave_chat", "chat_name": self.chat}, leave_chat)

    def test_closes_on_bad_left_message(self):
        self.send_valid_chat()
        self.client.onMessage(json.dumps({"record": "left"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_wrong_chat_name_in_left_message(self):
        self.send_valid_chat()
        self.client.onMessage(json.dumps({"record": "left", 'chat_name': 'other_chat', 'user_id': self.user_id, 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_left_message_for_user_not_in_chat(self):
        self.send_valid_chat()
        self.client.onMessage(json.dumps({"record": "left", 'chat_name': self.chat, 'user_id': 'other_user', 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_left_message_for_self_when_not_sent_leave(self):
        self.send_valid_chat()
        self.client.onMessage(json.dumps({"record": "left", 'chat_name': self.chat, 'user_id': self.user_id, 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_removes_user_on_left(self):
        self.send_valid_joined()
        self.client.onMessage(json.dumps({"record": "joined", "chat_name": self.chat, "user_name": "other_user", "user_id": "someone_else", "timestamp": self.now, "sequence": 43}))
        self.client.onMessage(json.dumps({"record": "left", 'chat_name': self.chat, 'user_id': 'someone_else', 'timestamp': self.now, 'sequence': 44}))
        self.client.onMessage(json.dumps({"record": "chat_message", 'chat_name': self.chat, 'user_id': 'someone_else', 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': self.now, 'sequence': 45}))
        self.assertFalse(self.ws.running)

    def test_sends_goodbye_when_self_left(self):
        self.send_valid_left()
        goodbye = json.loads(self.ws.sent_frames[5])
        self.assertEqual({"record": "client_goodbye"}, goodbye)

    def test_closes_on_message_before_chat_state(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "chat_message", 'chat_name': self.chat, 'user_id': self.user_id, 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_message_before_joined(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "chat_message", 'chat_name': self.chat, 'user_id': self.user_id, 'mime_type': 'text/plain', 'message': 'I like cats', 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_left_before_chat_state(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "left", 'chat_name': self.chat, 'user_id': self.user_id, 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_left_before_joined(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "left", 'chat_name': self.chat, 'user_id': self.user_id, 'timestamp': self.now, 'sequence': 42}))
        self.assertFalse(self.ws.running)

    def test_closes_on_bad_ping_reply(self):
        self.send_valid_chat()
        self.client.onMessage(json.dumps({"record": "ping_reply", "foo": "bar"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_ping_reply_before_chat_state(self):
        self.send_valid_server_hello()
        self.client.onMessage(json.dumps({"record": "ping_reply"}))
        self.assertFalse(self.ws.running)

    def test_closes_on_ping_reply_before_joined(self):
        self.send_valid_chat_state()
        self.client.onMessage(json.dumps({"record": "ping_reply"}))
        self.assertFalse(self.ws.running)

    def test_sends_chat_message_on_ping_reply(self):
        self.send_valid_ping()
        self.client.onMessage(json.dumps({"record": "ping_reply"}))
        chat_message = json.loads(self.ws.sent_frames[5])
        self.assertEqual({'record': 'chat_message', 'chat_name': self.chat, 'mime_type': 'text/plain', 'message': 'I like cats'}, chat_message)

    def test_waits_before_sending_ping_reply(self):
        self.delay = 42
        self.send_valid_ping()
        self.assertEqual(42, self.tm.slept_ms)

    def test_closes_when_server_goodbye_received(self):
        self.send_valid_left()
        self.client.onMessage(json.dumps({"record": "server_goodbye"}))
        self.assertFalse(self.ws.running)

class MessageValidationTests(unittest.TestCase):
    def setUp(self):
        self.url = 'ws://foobar:4242/some/path'
        self.ws = FakeWebSocket()
        self.client = client.Client(url=self.url, username='someone', chat='somechat', count=42, delay=0, ws=self.ws)

    def test_message_validation_fails_for_extra_fields(self):
        self.assertRaises(client.ProtocolError, self.client.validate, {'record': 'foobar'}, {'record': 'foobar', 'foo': 'bar'})

    def test_message_validation_fails_for_missing_fields(self):
        self.assertRaises(client.ProtocolError, self.client.validate, {'record': 'foobar', 'foo': str}, {'record': 'foobar'})

    def test_message_validation_fails_for_wrong_field_type(self):
        self.assertRaises(client.ProtocolError, self.client.validate, {'record': 'foobar', 'foo': str}, {'record': 'foobar', 'foo': 42})

    def test_message_validation_fails_for_empty_string_type(self):
        self.assertRaises(client.ProtocolError, self.client.validate, {'record': 'foobar', 'foo': str}, {'record': 'foobar', 'foo': ''})

    def test_message_validation_fails_for_wrong_exact_value(self):
        self.assertRaises(client.ProtocolError, self.client.validate, {'record': 'foobar', 'foo': 'bar'}, {'record': 'foobar', 'foo': 'baz'})

    def test_message_validation_fails_for_non_list_in_list_field(self):
        self.assertRaises(client.ProtocolError, self.client.validate, {'record': 'foobar', 'foo': [{'record': 'bar'}]}, {'record': 'foobar', 'foo': 'bar'})

    def test_message_validation_fails_for_bad_list_field(self):
        self.assertRaises(client.ProtocolError, self.client.validate, {'record': 'foobar', 'foo': [{'record': 'bar'}]}, {'record': 'foobar', 'foo': [{'record': 'baz'}]})

    def test_message_validation_fails_for_bad_timestamp_field(self):
        self.assertRaises(client.ProtocolError, self.client.validate, {'record': 'foobar', 'ts': datetime.datetime}, {'record': 'foobar', 'ts': 'foobar'})

    def test_message_validation_passes_for_good_timestamp_field(self):
        self.client.validate({'record': 'foobar', 'ts': datetime.datetime}, {'record': 'foobar', 'ts': '2015-11-12T08:15:51Z'})

    def test_message_validation_passes_for_valid_message(self):
        self.client.validate({'record': 'foobar', 'foo': str}, {'record': 'foobar', 'foo': 'baz'})

    def test_message_validation_passes_for_valid_list_field(self):
        self.client.validate({'record': 'foobar', 'foo': [{'record': 'bar'}]}, {'record': 'foobar', 'foo': [{'record': 'bar'}]})

unittest.main()

