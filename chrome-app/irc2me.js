/*
 * Irc2me class definition
 *
 */

function Irc2me(proto_file)
{
    var self = this;

    self._connected = false;
    self._incomingMessageCallbacks = [];

    dcodeIO.ProtoBuf.loadProtoFile(proto_file, function(err, builder) {

        // quit on error
        if (err) {
            console.log("ProtoBuf: Error loading " + proto_file);
            return;
        }

        console.log(proto_file + " loaded");
        self._proto = builder.build("Protobuf.Messages");
    });
}

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

Irc2me.prototype.sendMessage = function(proto_message)
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
            return Logger.warn("Only " + bytesSent + ".");
        }
    });
}

Irc2me.prototype.onReceive = function(info)
{
    var self = this;

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

            // run all registered callbacks
            for (var cb_i = 0; cb_i < self._incomingMessageCallbacks.length; cb_i++) {
                if (self._incomingMessageCallbacks[cb_i]
                        && typeof(self._incomingMessageCallbacks[cb_i]) == "function") {
                    self._incomingMessageCallbacks[cb_i](message);
                }
            }

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

Irc2me.prototype.addIncomingMessageListener = function(messageCallback)
{
    return this._incomingMessageCallbacks.push(messageCallback) - 1;
}

Irc2me.prototype.removeIncomingMessageListener = function(index)
{
    this._incomingMessageCallbacks[index] = null;
}

/*
 * Connecting & authentication
 *
 */

Irc2me.prototype.disconnect = function()
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

    chrome.sockets.tcp.close(self._socket);

    self._socket = null;
    self._connected = false;

    // log disconnect() call
    Logger.log("Disconnected", "Disconnected.");
}

Irc2me.prototype.isConnected = function()
{
    var self = this;

    return self._connected;
}

Irc2me.prototype.connect = function(hostname, port, cb)
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
            if (res != 0) {
                return Logger.warn("Could not set NoDelay (" + res + ").");
            }
        });

        // connect
        chrome.sockets.tcp.connect(self._socket, hostname, parseInt(port), function(res) {

            if (res != 0) {
                // log error
                Logger.error("Could not connect socket (" + res + ").");

                // quit
                return self.disconnect();
            }

            self._connected = true;

            Logger.info("Connected to " + hostname + ":" + port + ".");

            // add onReceive callback
            chrome.sockets.tcp.onReceive.addListener(function (info) {
                self.onReceive(info);
            });

            if (cb && typeof(cb) == "function") {
                cb();
            }
        });
    });
}

Irc2me.prototype.authenticate = function(username, password)
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
