/*
 * Send and receive protobuf messages over one persistent network socket
 *
 * Optional 'socket' parameter.
 */

function ProtoStream (socket) {

    var self = this;

    self._connected = false;

    // set socket
    self._socket = socket;

    // log nothing
    self._logger = function () { };
}

/*
 * Suspend & restore
 *
 */

ProtoStream.prototype.suspend = function (cb) {
    var self = this;

    self.disconnect(cb);
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

    if (callback == null) {
        callback = function() {};
    }

    var self = this;

    var Logger = self.getLogger("sendMessage", proto_message);

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

        if (res.resultCode != 0) {
            return callback(false, "Could not send message (" + res.resultCode + ").");
        }

        if (res.bytesSent < totalLength) {
            var bytesSent =
            Logger.warn("Only " +  res.bytesSent + " of " + totalLength + " bytes sent.");
        }

        // done
        return callback(true);
    });
}

ProtoStream.prototype._onReceive = function(info) {

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

    if (callback == null) {
        callback = function () {};
    }

    log.log("Connecting", "Connecting…");

    // create new socket
    chrome.sockets.tcp.create({
        name: "protostream-tcp-socket",
        persistent: true,
    }, function(createInfo) {

        // check error
        if (chrome.runtime.lastError) {
            self.disconnect();
            return callback(false, chrome.runtime.lastError.message);
        }

        self._socket = createInfo.socketId;

        // connect
        chrome.sockets.tcp.connect(self._socket, hostname, parseInt(port), function(res) {

            // check error
            if (chrome.runtime.lastError) {
                self.disconnect();
                return callback(false, chrome.runtime.lastError.message);
            }

            if (res != 0) {
                // quit
                self.disconnect();
                return callback(false, "Could not connect socket (" + res + ").");
            }

            self._connected = true;

            log.info("Connected to " + hostname + ":" + port + ".");

            // (try to) disable delay
            chrome.sockets.tcp.setNoDelay(self._socket, true, function(res) {

                var reason = chrome.runtime.lastError ? chrome.runtime.lastError.message : res;

                // check error
                if (reason) {
                    log.warn("Could not set NoDelay (" + reason + ").");
                }
            });

            // install handler for incoming data
            self._onReceiveListener = function (info) {
                self._onReceive(info);
            };

            chrome.sockets.tcp.onReceive.addListener(self._onReceiveListener);

            // done
            return callback(true);
        });
    });
}

ProtoStream.prototype.disconnect = function (callback) {

    if (callback == null) {
        callback = function () {};
    }

    var self = this,
        log = self.getLogger("disconnect");

    if (self._socket == null) {
        // do nothing
        return log.warn("No socket available.");
    }

    // buffer values
    var socket = self._socket,
        connected = self._connected;

    // reset self status
    self._socket = null;
    self._connected = false;
    self._incomingCB = null;

    // remove chrome api listener
    if (self._onReceiveListener) {
        chrome.sockets.tcp.onReceive.removeListener(self._onReceiveListener);
        self._onReceiveListener = null;
    }

    // perform disconnect
    if (connected) {
        chrome.sockets.tcp.disconnect(socket, function() {

            if (chrome.runtime.lastError) {
                return callback(false, chrome.runtime.lastError.message);
            }

            // log disconnect() call
            log.log("Disconnected", "Disconnected.");

            // close socket
            chrome.sockets.tcp.close(socket, function () {

                if (chrome.runtime.lastError) {
                    return log.error(chrome.runtime.lastError.message);
                }

                // done
                return callback(true);
            });
        });
    }
}
