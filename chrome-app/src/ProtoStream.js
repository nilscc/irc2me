/*
 * Send and receive protobuf messages over one persistent network socket
 *
 * Optional 'socket' parameter.
 */

function ProtoStream (socket) {

    var self = this;

    self._connected = false;
    self._receiving = false;

    // set socket
    self._socket = socket;

    // load runtime storage
    self._runtime = new RuntimeStorage(self);

    // log nothing
    self._logger = function () { };
}

/*
 * Suspend & restore
 *
 */

ProtoStream.prototype.suspend = function () {
    var self = this;

    self._runtime.storePrivateValues();
}

ProtoStream.restore = function () {

    var self = new ProtoStream();

    self._runtime.restorePrivateValues();

    return self;
}

/*
 * Logging
 *
 */

ProtoStream.prototype.setLogger = function (loggerFunction) {
    var self = this;

    self._logger = loggerFunction;
}

ProtoStream.prototype.getLogger = function (where /* ... */) {
    var self = this;

    var args = Array.prototype.slice.call(arguments);
    args.shift();

    // add arguments to 'where'
    where = "ProtoStream." + where + "(" + args.join(", ") + ")";

    // return Logger
    return new Logger(where, self._logger);
}

ProtoStream.prototype.logToConsole = function () {
    var self = this;
    self.setLogger(function (o) { console.log(o); });
}


/*
 * Handle incoming data
 *
 * handleIncoming : ByteBuffer -> ()
 *
 */
ProtoStream.prototype.setIncomingCallback = function (handleIncoming) {
    var self = this;
    self._incomingCB = handleIncoming;
}

/*
 * Send & receive
 *
 */

ProtoStream.prototype.sendMessage = function (proto_message, callback) {

    var self = this;

    console.log(proto_message);

    var Logger = self.getLogger("sendMessage()");

    if (!self._socket) {
        return Logger.error("Socket not initialized.");
    }

    if (!self._connected) {
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

        if (typeof callback == "function") {
            callback();
        }
    });
}

ProtoStream.prototype._onReceive = function(info) {

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
    }

    // check if callback has been set properly, otherwise delay buffer
    if (typeof self._incomingCB != "function") {
        return; // delay
    }

    // swap buffer/incomplete buffer
    if (self._incompleteMessageBuffer != null) {
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

            // decode message and run callback
            self._incomingCB(buffer);

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
 * Connect & disconnect
 *
 */

ProtoStream.prototype.connect = function (hostname, port, callback) {

    var self = this;

    var log = self.getLogger("connect", hostname, port);

    if (self._socket != null) {
        return log.error("ProtoStream.connect: _socket already set.");
    }

    if (self._connected) {
        return log.error("ProtoStream.connect: Already connected.");
    }

    log.log("Connecting", "Connecting…");

    // create new socket
    chrome.sockets.tcp.create({
        name: "protostream-tcp-socket",
        persistent: true,
    }, function(createInfo) {

        // check error
        if (chrome.runtime.lastError) {
            return log.error(chrome.runtime.lastError.message);
        }

        self._socket = createInfo.socketId;

        log.info("Socket created with ID " + createInfo.socketId + ".");

        // (try to) disable delay
        chrome.sockets.tcp.setNoDelay(self._socket, true, function(res) {

            var reason;

            // check error
            if (chrome.runtime.lastError) {
                reason = chrome.runtime.lastError.message;
            }

            if (reason || res != 0) {
                log.warn("Could not set NoDelay (" + (reason ? reason : res) + ").");
            }

            // connect
            chrome.sockets.tcp.connect(self._socket, hostname, parseInt(port), function(res) {

                // check error
                if (chrome.runtime.lastError) {
                    return log.error(chrome.runtime.lastError.message);
                }

                if (res != 0) {
                    // log error
                    log.error("Could not connect socket (" + res + ").");

                    // quit
                    return self.disconnect();
                }

                self._connected = true;

                log.info("Connected to " + hostname + ":" + port + ".");

                // install handler for incoming data
                if (! self._receiving) {
                    chrome.sockets.tcp.onReceive.addListener(function (info) {
                        self._onReceive(info);
                    });
                    self._receiving = true;
                }

                if (typeof callback == "function") {
                    callback();
                }
            });
        });
    });
}

ProtoStream.prototype.disconnect = function (callback) {

    var self = this,
        log = self.getLogger("disconnect");

    if (self._socket == null) {
        // do nothing
        return log.warn("No socket available.");
    }

    if (! self._connected) {
        // do nothing
        return log.warn("Not connected.");
    }

    // perform disconnect
    chrome.sockets.tcp.disconnect(self._socket, function() {

        self._connected = false;

        if (chrome.runtime.lastError) {

            // probably a closed socket, so unset _socket
            self._socket = null;

            return log.error(chrome.runtime.lastError.message);
        }

        // log disconnect() call
        log.log("Disconnected", "Disconnected.");

        // close socket
        chrome.sockets.tcp.close(self._socket, function () {

            self._socket = null;

            if (chrome.runtime.lastError) {
                return log.error(chrome.runtime.lastError.message);
            }

            if (typeof callback == "function") {
                callback();
            }
        });
    });
}
