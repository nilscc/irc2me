/*
 * Irc2me class definition
 *
 */

function Irc2me(proto_file)
{
    var self = this;

    self._connected = false;
    self._authorized = false;

    dcodeIO.ProtoBuf.loadProtoFile(proto_file, function(err, builder) {

        // quit on error
        if (err) {
            console.error("ProtoBuf: Error loading " + proto_file);
            return;
        }

        self._proto = builder.build("Protobuf.Messages");
    });

    self._receiving = false;
}

Irc2me.ServerMessageType = "irc2me-server-message";
Irc2me.CommandMessageType = "irc2me-command";
Irc2me.LogMessageType = "irc2me-log";

/*
 * Logging
 *
 */

// Logger class
Irc2me.prototype.Logger = function(where, callback)
{
    this._where = where;
    this._callback = callback;
}

Irc2me.prototype.Logger.prototype.log = function(s, m)
{
    return this._callback({ status: s, where: this._where, message: m });
}

Irc2me.prototype.Logger.prototype.warn = function(msg)
{
    return this.log("Warning", msg);
}
Irc2me.prototype.Logger.prototype.info = function(msg)
{
    return this.log("Info", msg);
}
Irc2me.prototype.Logger.prototype.error = function(msg)
{
    return this.log("Error", msg);
}

Irc2me.prototype.setLogger = function(callback)
{
    this._logger_cb = callback;
}

// Set up logger for internal logging
Irc2me.prototype.getLogger = function(where)
{
    return new this.Logger("Irc2me." + where, this._logger_cb);
}

// external logging
Irc2me.prototype.log = function(statusObject)
{
    this._logger_cb(statusObject);
}

/*
 * Sending & receiving
 *
 */

Irc2me.prototype.sendMessage = function(proto_message, cb)
{
    var self = this;

    var Logger = self.getLogger("sendMessage()");

    if (!self._socket) {
        return Logger.error("Socket not initialized.");
    }

    if (!self.isConnected()) {
        return Logger.error("Not connected.");
    }

    // encode message and get its length
    var encodedMessage = proto_message.encodeAB(),
        byteLength     = encodedMessage.byteLength;

    // calculate the varint64 length for the message length
    var varintLength = dcodeIO.ByteBuffer.calculateVarint64(byteLength);

    // sum up
    var totalLength = varintLength + byteLength;

    // allocate byte buffer
    var buffer = new dcodeIO.ByteBuffer(totalLength);

    // prefix message length and append encoded message
    buffer.writeVarint64(byteLength);
    buffer.append(encodedMessage);

    // jump back to buffer start
    buffer.offset = 0;

    // send buffer
    chrome.sockets.tcp.send(self._socket, buffer.toArrayBuffer(), function(res) {

        var bytesSent = res.bytesSent + " of " + totalLength + " bytes sent";

        if (res.resultCode != 0) {
            return Logger.error("Could not send message (" + res.resultCode + ", " + bytesSent + ").");
        }
        if (res.bytesSent < totalLength) {
            Logger.warn("Only " + bytesSent + ".");
        }

        if (cb && typeof cb == "function") {
            cb();
        }
    });
}

Irc2me.AuthenticatedType = "Irc2me.AuthenticatedType";

Irc2me.prototype.handleIncomingMessage = function (message) {

    var self = this;

    var Logger = self.getLogger("handleIncomingMessage()");

    // alias messages
    var ServerMsg = self._proto.Server,
        SystemMsg = self._proto.SystemMsg;

    if (!self.isAuthenticated()) {
        if (message.response_code == ServerMsg.ResponseCode.OK) {

            self._authorized = true;

            // send event to everyone
            chrome.runtime.sendMessage({
                type: Irc2me.AuthenticatedType
            });

            // log system message
            Logger.info("Successfully authenticated.");

        } else {
            self._authorized = false;
            self._disconnect();

            // log system message
            Logger.info("Authentication failed.");
        }

        return; // quit after auth message
    }

    // send all other messages as chrome message
    chrome.runtime.sendMessage({
        type: Irc2me.ServerMessageType,
        content: message,
    });

}
Irc2me.prototype.onReceive = function(info)
{
    var self = this;

    console.log("incoming on " + info.socketId + " (our: " + self._socket + ")");

    // skip other sockets
    if (info.socketId != self._socket) {
        return;
    }

    var totalLength = info.data.byteLength;

    // wrap the array buffer into our byte buffer
    var buffer = dcodeIO.ByteBuffer.wrap(info.data);

    // see if we have any incomplete messages still in buffer
    if (self._incompleteMessageBuffer != null) {

        totalLength += self._incompleteMessageBuffer.remaining();

        // append 'buffer' to the end of incomplete message buffer
        self._incompleteMessageBuffer.append(buffer, self._incompleteMessageBuffer.limit);

        // swap buffer/incomplete buffer
        buffer = self._incompleteMessageBuffer;
        self._incompleteMessageBuffer = null;
    }

    // reset all limit/offsets
    buffer.clear();

    // loop over all messages
    while (buffer.remaining() > 0) {

        // mark current buffer position
        buffer.mark();

        // read message
        var messageLength = buffer.readVarint64();

        if (buffer.remaining() >= messageLength) {

            // set proper limit
            buffer.limit = parseInt(buffer.offset) + parseInt(messageLength);

            // decode message
            var message = self._proto.Server.decode(buffer);

            self.handleIncomingMessage(message);

            // reset buffer and set proper offset
            buffer.offset = buffer.limit;
            buffer.limit  = totalLength;

        } else {

            // jump back to old 'marked' varint64 message length prefix position
            buffer.reset();

            // break out of while loop
            break;
        }
    }

    // check if we have left over data
    if (buffer.remaining() > 0) {

        if (!self._incompleteMessageBuffer) {
            self._incompleteMessageBuffer = buffer;
        } else {
            self._incompleteMessageBuffer.append(buffer);
        }

    }
}

/*
 * Connecting & authentication
 *
 */

Irc2me.prototype.isConnected     = function() { return this._connected;  }
Irc2me.prototype.isAuthenticated = function() { return this._authorized; }

Irc2me.prototype._disconnect = function()
{
    var self = this,
        Logger = self.getLogger("disconnect()");

    if (self._socket == null) {
        // do nothing
        return Logger.warn("No socket available.");
    }

    if (! self.isConnected()) {
        // do nothing
        return Logger.warn("Not connected.");
    }

    // wrap the actual act of disconnecting into a function so that we can call
    // them in our isAuthorized callback
    var do_disconnect = function () {
        chrome.sockets.tcp.disconnect(self._socket, function() {
            chrome.sockets.tcp.close(self._socket);

            self._socket = null;
            self._connected = false;
            self._authorized = false;

            // log disconnect() call
            Logger.log("Disconnected", "Disconnected.");
        });
    }

    // send "disconnect" message first if authorized
    if (self.isAuthenticated()) {

        // alias message
        var ClientMsg = self._proto.Client,
            SystemMsg = self._proto.SystemMsg;

        self.sendMessage(new ClientMsg({
            system_msg: SystemMsg.DISCONNECT,
        }), do_disconnect);

    } else {

        // otherwise just disconnect
        do_disconnect();

    }
}

Irc2me.prototype._connect = function(hostname, port, cb)
{
    var self = this,
        Logger = self.getLogger("connect(\"" + hostname + "\", " + port + ", <" + typeof(cb) + ">)");

    if (self._socket || self.isConnected()) {
        return Logger.error("Already connected");
    }

    Logger.log("Connecting", "Connecting…");

    // create new socket
    chrome.sockets.tcp.create({
        name: "irc2me-tcp-socket",
        persistent: false,
    }, function(createInfo) {

        self._socket = createInfo.socketId;

        Logger.info("Socket created with ID " + createInfo.socketId + ".");

        // (try to) disable delay
        chrome.sockets.tcp.setNoDelay(self._socket, true, function(res) {

            var reason;

            // check error
            if (chrome.runtime.lastError) {
                reason = chrome.runtime.lastError.message;
            }

            if (reason || res != 0) {
                Logger.warn("Could not set NoDelay (" + (reason ? reason : res) + ").");
            }

            // connect
            chrome.sockets.tcp.connect(self._socket, hostname, parseInt(port), function(res) {

                if (res != 0) {
                    // log error
                    Logger.error("Could not connect socket (" + res + ").");

                    // quit
                    return self._disconnect();
                }

                self._connected = true;

                Logger.info("Connected to " + hostname + ":" + port + ".");

                // install handler for incoming data
                if (! self._receiving) {
                    chrome.sockets.tcp.onReceive.addListener(function (info) {
                        self.onReceive(info);
                    });
                    self._receiving = true;
                }

                if (cb && typeof(cb) == "function") {
                    cb();
                }
            });

        });
    });
}

Irc2me.prototype._authenticate = function(username, password)
{
    var self = this,
        Logger = self.getLogger("authenticate(\"" + username + "\", ****)");

    if (! self._connected) {
        return Logger.error("Not connected.");
    }

    Logger.info("Trying to authenticate…");

    self.sendMessage(new self._proto.Authentication({
        login: username,
        password: password,
    }));
}

/*
 * Chrome message interface
 *
 */

Irc2me.connect      = new ChromeMessage("Irc2me.connect");
Irc2me.disconnect   = new ChromeMessage("Irc2me.disconnect");
Irc2me.authenticate = new ChromeMessage("Irc2me.authenticate");

Irc2me.prototype.setListeners = function () {

    var self = this;

    Irc2me.connect.setListener(function(content, sendResponse) {
        var host = content.hostname,
            port = content.port;

        self._connect(host, port, sendResponse);

        // async response
        return true;
    });

    Irc2me.authenticate.setListener(function(content, sendResponse) {
        var user = content.username,
            pass = content.password;

        self._authenticate(user, pass, sendResponse);

        // async response
        return true;
    });

    Irc2me.disconnect.setListener(function(content, sendResponse) {
        self._disconnect();
    });

}
