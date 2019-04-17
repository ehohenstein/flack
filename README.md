# Flack

## Description

An example distributed chat system written in erlang.

The web client has been tested on Windows 7 in Chrome version 73.0.3683.86 and in Firefox 66.0.2.

## Dependencies

#### Server
[Ubuntu Bionic 18.04](http://archive.ubuntu.com/ubuntu/dists/bionic/main/installer-amd64/current/images/netboot/mini.iso)

erlang 20.2

[rebar3 3.9.1](https://github.com/erlang/rebar3)

[cowboy 2.6.1](https://github.com/ninenines/cowboy)

[ranch 1.7.1](https://github.com/ninenines/ranch)

[cowlib 2.7.0](https://github.com/ninenines/cowlib)

[jiffy 0.15.2](https://github.com/davisp/jiffy)

[gproc 0.8.0](https://github.com/uwiger/gproc)

[gen_leader](https://github.com/garret-smith/gen_leader_revival)

#### Web client
```
jquery 3.3.1
```

#### Reference client
```
python3 3.6.7
websocket-client 0.56
```

#### Testing
```
erlymock 7.2.2
npm 3.5.2
browserify 16.2.3
jest 24.7.1
```

## Building
#### Build setup
First install the dependency packages:
```
sudo apt-get update
sudo apt-get install openssh-server
sudo apt-get install net-tools
sudo apt-get install git
sudo apt-get install make
sudo apt-get install npm
sudo apt-get install erlang
sudo apt-get install python3-pip
sudo pip3 install websocket-client==0.56
```

Next download the code:
```
git clone https://github.com/ehohenstein/flack
cd flack
make build
```

## Testing
#### Automated testing
Run `make test` in the root folder.

#### Manual testing
See the single node setup below.

## Setup
#### Single node

Flack can be tested locally on a single node. From the project root folder, first do:

```
cd client/static/
make build
sudo make install
```

This will install the front-end files in `/var/www/`, creating that directory if necessary.

To build and run the server, from the project root folder, do:

```
cd server/flack/
make release
make start
```

The server should be running and listening on port 8080. Using a browser, got to http://<hostname>:8080/.

Enter your name in the text box in the upper left and then press enter or click the login button next to it. Your browser should be connected to the flack server and the name should have changed to a text element with a logout button next to it.

Enter the name of a chat room in the text box below the name you entered and press enter or click the join button next to it. The room name should appear in the room list below the text box and you should see a message saying you joined the room.

Enter some text in the text box at the bottom of the window and press enter or click the send button next to it. The message should appear at the bottom of the message window with your name next to it.

You can test also test using the reference client, though it won't allow you to enter your own text. To use it, from the project root folder, do:

```
cd client/reference/
python3 -u client.py --url=ws://localhost:8080/chat-server
```
By default the reference client will connect to the flack server as the user `hoser`, join a room called `foobar`, and send the message `I like cats` (sorry, not configurable) to it once per second for 1000 seconds. The username, room name, message count, and frequency can all be specified using command line parameters. Type
```
python3 -u client.py --help
```
for usage information.

To stop the single-node flack server, do:

```
cd server/flack/
make stop
```

#### Single node with docker

If you have docker installed, you can run flack in a docker container. If you have started flack using some other method, you will want to stop it before proceeding. From the root of the flack project tree run this:

```
make docker-run-single
```

You should be able to test by going to http://<hostname>:8080/ in a browser and with the reference client, as above.

To stop flack, do this:

```
make docker-stop-single
```

#### Distributed mode (locally)

The server can be tested in distributed mode locally with a little more effort than in single node mode. Note that if you started the single-node server above and it's still running, it will interfere with the distributed version as it will still be using port 8080 needed by the first server node started below.

First, the front-end files should be installed in `/var/www/` if they haven't already been.

Next, build a release tarball for the server. From the project root folder, do:

```
cd server/flack/
make tar
```
The tarball will be built as `_build/prod/rel/flack/flack-1.0.2.tar.gz`. The tarball can be copied anywhere else on the filesystem and extracted and executed. It can even be copied to another host and run without installing erlang. Choose a directory and copy it there. The do:
```
mkdir flack1
cd flack1/
tar -xzf ../flack-1.0.2.tar.gz
cd ..
mkdir flack2
cd flack2
tar -xzf ../flack-1.0.2.tar.gz
cd ..
mkdir flack3
cd flack3
tar -xzf ../flack-1.0.2.tar.gz
cd ..
```
Now you will need to use your favorite editor to modify the `vm.args` file in the `releases/1.0.2/` folder of each of `flack1`, `flack2`, and `flack3`. Change the line with `-sname flack` to be `-sname flack1` under the `flack1` folder, `-sname flack2` under the `flack2` folder, and `-sname flack3` under the `flack3` folder.
Now you will need to modify the `sys.config` file in the same directories. This time you will need to replace `{gproc, [{gproc_dist, all}]}` with `{gproc, [{gproc_dist, [flack1@<hostname>,flack2@<hostname>,flack3@<hostname>]}]}` in each of the 3 files, replacing `<hostname>` with the name of the host where you are testing.

Now the three nodes can be started by doing:
```
cd flack1/
bin/flack start -flack port 8080
cd ../flack2/
bin/flack start -flack port 8081
cd ../flack3/
bin/flack start -flack port 8082
```
All three nodes should now be running locally and connected to each other. You can again test flack in a browser by going to http://<hostname>:8080/. If you login and join the chat `foobar`, you can run the reference client against any of the 3 ports, `8080`, `8081`, or `8082` and you should see the spam from the reference client show up in your browser, which should demonstrate that the 3 nodes are connected and chat messages are flowing between them.

#### Distributed mode with docker

If you have docker isntalled, you can run flack in cluster mode in a set of docker containers. If you have started flack using some other method, you will want to stop it before proceeding. From the root of the flack project tree run this:

```
make docker-run-cluster
```

You should be able to test flack in a browser by going to http://<hostname>:8080/. The reference client can be run against port 8080 which goes to a random flack node through nginx or using ports, 8081, 8082, or 8083, which reach each of the 3 flack nodes directly.

To stop flack do:

```
make docker-stop-cluster
```

#### Fully distributed mode

Setting up flack and using it in a real environment is more complicated but one way of doing it would be to put multiple flack nodes behind a reverse proxy like nginx. The static files could be served by nginx and nginx can be configured to forward the websocket connections to flack.

It might be possible to run many flack nodes and load balance them in a kubernetes cluster, though its use of `gen_leader` likely would make that challenging.

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

The `chat_name` field will be the name of a chat that the client has joined. The `user_name` field will be a non-empty utf-8 string that is the name of the user that joined the chat. The `user_id` field is a non-empty ascii string that uniquely identifies the user that joined the chat. The `timestamp` field will be a RFC3339 formatted string identifying the UTC time when the user joined the chat. The `sequence` field will be an integer that the client can use to impose a strict ordering on chat stream events should the client receive messages out of order.

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

The `chat_name` field will be the name of a chat that the client has joined. The `user_id` field uniquely identifies the user that sent the message. The `mime_type` field indicates the type of content contained in the `message` field which must be "text/plain". The `message` field must contain a utf-8 string. The `timestamp` field will be a RFC3339 formatted string identifying the UTC time when the message was received by the serer. The `sequence` field will be an integer that the client can use to impose a strict ordering on chat stream events should the client receive messages out of order. The `user_id`, `timestamp`, and `sequence` fields should be ommitted by the client when sending a `chat_message` message to the server but are required in `chat_message` messages sent by the server to clients.

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

The `chat_name` field will be the name of a chat that the client has joined. The `user_id` field will be a non-empty ascii string uniquely identying the user that left the chat. The `timestamp` field will be a RFC3339 formatted string identifying the UTC time when the user left the chat. The `sequence` field will be an integer that the client can use to impose a strict ordering on chat stream events should the client receive messages out of order.

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
