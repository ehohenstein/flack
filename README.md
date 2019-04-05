# Flack

## Description

An example distributed chat system written in erlang.

## Dependencies

```
Ubuntu Bionic 18.04
Erlang 20.2
Python3 3.6.7
Rebar3 3.9.1
Cowboy 2.6.1
Ranch 1.7.1
Cowlib 2.7.0
Jiffy 0.15.2
Gcc 7.3.0
Erlymock 7.2.2
```

## Installation

```
sudo apt-get install python3-pip
sudo pip3 install websocket-client==0.56
sudo apt-get install erlang
```

## Tests

## Dependencies

## Use

## Documentation

### Protocol

Clients use websockets to connect to a server. There is no authentication of clients. The client declares its identity using a name and the server trusts the client. If multiple clients are connected simultaneously and join the same chat, it will appear to other users that two users with the same user are simultaneously present in the chat.

Messages are JSON. Framing of messages is handled by the websocket protocol.

The conversation begins with the client sending a `client_hello` message to the server after the websocket connection is established. The server will respond with a `server_hello` message if the server accepts the client's protocol version. If the server rejects the client's protocol version, it responds with a `protocol_error` message and closes the connection.

If at any point during the conversation, the server determines that a message from the client is unacceptable or malformed, the server will respond with a `protocol_error` message and close the connection.

After the server sends a `server_hello` message, the client should send an `authenticate` message to the server. The server will respond with an `authenticated` message.

After the server sends an `authenticated` message to the client, the client can send a `join_chat` message for each chat it wants to join. If a chat does not already exist when a client attempts to join it, the chat will be created. The client can join multiple chats simultaneously. Upon joining a chat, the server will send a `chat_state` message to that user indicating the set of existing users that have already joined the chat and that have not already left followed by a `joined` message for that user to all clients that have joined the chat and not already left, including the client that just joined. Until the client sends a `leave_chat` message, the client can send a `chat_message` to the server for the associated chat and the server will forward the `chat_message` message to all clients that have joined the same chat and not already left, including the client that originally sent the message. After joining a chat the client can send a `leave_chat` message to stop receiving messages for that chat. Upon leaving receiving a `leave_chat` message, the server will send a `left` message to all clients that have joined the chat and not already left, including the client that is leaving.

If the connection has been idle for at least 20 seconds, the client should send a `ping` message to the server. Upon receiving a `ping` message, the server will respond to the client with a `ping_reply` message. If the connection is idle for at least 60 seconds, the server may close the connection without sending any additional message.

Before closing the connection, the client should send a `client_goodbye` message to the server. The server will respond with a `server_goodbye` message, though the client may close the connection without waiting for the `server_goodbye` message.

#### Messages

##### `client_hello`

The `protocol_version` field must contain the value "1.0".

```javascript
{
    "record": "client_hello",
    "protocol_version": "1.0"
}
```

##### `server_hello`

The `protocol_version` field must contain the value "1.0". The `user_id` field will be a non-emtpy ascii string that the server has chosen to identify this connection.

```javascript
{
    "record": "server_hello",
    "protocol_version": "1.0"
}
```

##### `protocol_error`

The `code` field will contain an ascii error code associated with the specific error detected by the server which the client may use to interpret the reason for the error and present a user-friendly error explanation. The `reason` field will contain an ascii English explanation of the error which is intended for debugging.

```javascript
{
    "record": "protocol_error",
    "code": string,
    "reason": string
}
```

##### `authenticate`

The `user_name` field may be any non-empty utf-8 string. The `user_id` field is optional, and if non-empty should be the value of the `user_id` provided to the client in a previous `server_hello` message.

```javascript
{
    "record": "authenticate",
    "user_name": string,
    "user_id", string
}
```

##### `authenticated`

```javascript
{
    "record": "authenticated",
    "user_id": string
}
```

##### `join_chat`

The `chat_name` field can be any non-empty utf-8 string.

```javascript
{
    "record": "join_chat",
    "chat_name": string
}
```

##### `chat_state`

The `chat_name` field will be the name of a chat that the client has joined. The `users` field will be a list containing 0 or more `chat_user` records, one for each user that has joined the chat and not already left.

```javascript
{
    "record": "chat_state",
    "chat_name": string,
    "users": [chat_user]
}
```

##### `chat_user`

The `user_name` field will be a non-empty utf-8 string that is the name of a user that has joined a chat. The `user_id` field will be a non-empty ascii string that uniquely identifies the user.

```javascript
{
    "record": "chat_user",
    "user_name": string,
    "user_id": string
}
```

##### `joined`

The `chat_name` field will be the name of a chat that the client has joined. The `user_name` field will be a non-empty utf-8 string that is the name of the user that joined the chat. The `user_id` field is a non-empty ascii string that uniquely identifies the user that joined the chat. The `timestamp` field will be an ISO 8601 formatted string identifying the UTC time when the user joined the chat. The `sequence` field will be an integer that the client can use to impose a strict ordering on chat stream events should the client receive messages out of order.

Clients should store the association between `user_name` and `user_id` since all subsequent messages associated with the chat will only contain `user_id` to identify the user associated with the message.

```javascript
{
    "record": "joined",
    "chat_name": string,
    "user_name": string,
    "user_id": string,
    "timestamp": string,
    "sequence": integer
}
```

##### `chat_message`

The `chat_name` field will be the name of a chat that the client has joined. The `user_id` field uniquely identifies the user that sent the message. The `mime_type` field indicates the type of content contained in the `message` field which must be "text/plain". The `message` field must contain a utf-8 string. The `timestamp` field will be an ISO 8601 formatted string identifying the UTC time when the message was received by the serer. The `sequence` field will be an integer that the client can use to impose a strict ordering on chat stream events should the client receive messages out of order. The `user_id`, `timestamp`, and `sequence` fields should be ommitted by the client when sending a `chat_message` message to the server but are required in `chat_message` messages sent by the server to clients.

```javascript
{
    "record": "chat_message",
    "chat_name": string,
    "user_id": string,
    "mime_type": "text/plain",
    "message": string,
    "timestamp": string,
    "sequence": integer
}
```

##### `leave_chat`

The `chat_name` field will be the name of a chat that the client has joined.

```javascript
{
    "record": "leave_chat",
    "chat_name": string
}
```

##### `left`

The `chat_name` field will be the name of a chat that the client has joined. The `user_id` field will be a non-empty ascii string uniquely identying the user that left the chat. The `timestamp` field will be an ISO 8601 formatted string identifying the UTC time when the user left the chat. The `sequence` field will be an integer that the client can use to impose a strict ordering on chat stream events should the client receive messages out of order.

```javascript
{
    "record": "left",
    "chat_name": string,
    "user_id": string,
    "timestamp": string,
    "sequence": integer
}
```

##### `ping`

```javascript
{
    "record": "ping"
}
```

##### `ping_reply`

```javascript
{
    "record": "ping_reply"
}
```

##### `client_goodbye`

```javascript
{
    "record": "client_goodbye"
}
```

##### `server_goodbye`

```javascript
{
    "record": "server_goodbye"
}
```
