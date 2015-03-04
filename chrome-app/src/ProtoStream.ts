/// <reference path="libs/chrome" />
/// <reference path="libs/dcodeIO/ByteBuffer" />

"use strict";

import Logger     = require("Logger");
import ByteBuffer = require("ByteBuffer");

export interface Message {
    encodeAB : () => EncodedMessage;
}

export interface EncodedMessage {
    byteLength : number;
}

export class ProtoStream {

    private logger    : (msg : Logger.LogMessage) => void;

    private socket    : any;
    private connected : boolean = false;

    constructor () {
        this.logger = (state) => { console.log(state); };
    }

    /*
     * Logging
     *
     */

    setLogger (f) {
        this.logger = f;
    }

    getLogger (where, ...args) : Logger.Class {
        where = "ProtoStream." + where + "(" + args.join(", ") + ")";
        return new Logger.Class(where, this.logger || ((o) => { console.log(o); }));
    }

    /*
     * Incoming data
     *
     */

    sendMessage (protoMessage : Message, callback? : (boolean, string?) => void) {

        var logger = this.getLogger("sendMessage", protoMessage);

        if (! this.socket) {
            return logger.error("Socket not initialized.");
        }

        if (! this.connected) {
            return logger.error("Not connected.");
        }

        // encode message and get its length
        var encodedMessage = protoMessage.encodeAB(),
            byteLength     = encodedMessage.byteLength;

        // calculate the varint64 length for the message length
        var varintLength = ByteBuffer.calculateVarint64(byteLength);

        // sum up
        var totalLength = varintLength + byteLength;

        // allocate byte buffer
        var buffer = new ByteBuffer.Class(totalLength);

        // prefix message length and append encoded message
        buffer.writeVarint64(byteLength);
        buffer.append(encodedMessage);

        // jump back to buffer start
        buffer.offset = 0;

        // send buffer
        chrome.sockets.tcp.send(this.socket, buffer.toArrayBuffer(), (res) => {

            if (res.resultCode != 0) {
                return callback(false, "Could not send message (" + res.resultCode + ").");
            }

            if (res.bytesSent < totalLength) {
                var bytesSent = logger.warn("Only " +  res.bytesSent + " of " + totalLength + " bytes sent.");
            }

            // done
            if (callback) {
                callback(true);
            }
        });
    }

    private _incompleteMessageBuffer : ByteBuffer.Class;
    private _incomingCB : (any) => void;

    setIncomingCallback (handleIncoming : (any) => void) {
        this._incomingCB = handleIncoming;
    }

    private onReceive (info : any) {

        // skip other sockets
        if (info.socketId != this.socket) {
            return;
        }

        var totalLength = info.data.byteLength;

        // wrap the array buffer into our byte buffer
        var buffer = ByteBuffer.wrap(info.data);

        // see if we have any incomplete messages still in buffer
        if (this._incompleteMessageBuffer != null) {

            totalLength += this._incompleteMessageBuffer.remaining();

            // append 'buffer' to the end of incomplete message buffer
            this._incompleteMessageBuffer.append(buffer, this._incompleteMessageBuffer.limit);
        }

        // check if callback has been set properly, otherwise delay buffer
        if (!this._incomingCB) {
            return; // delay
        }

        // swap buffer/incomplete buffer
        if (this._incompleteMessageBuffer != null) {
            buffer = this._incompleteMessageBuffer;
            this._incompleteMessageBuffer = null;
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
                this._incomingCB(buffer);

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

            if (!this._incompleteMessageBuffer) {
                this._incompleteMessageBuffer = buffer;
            } else {
                this._incompleteMessageBuffer.append(buffer);
            }
        }
    }

    /*
     * Connect & disconnect
     *
     */

    private _onReceiveListener : (any) => void;

    connect (hostname : string, port : any, callback? : (boolean, string?) => void) {

        var log = this.getLogger("connect", hostname, port);

        if (this.socket != null) {
            return log.error("ProtoStream.connect: Socket already set.");
        }

        if (this.connected) {
            return log.error("ProtoStream.connect: Already connected.");
        }

        if (! callback) {
            callback = () => {};
        }

        log.log("Connecting", "Connecting…");

        // create new socket
        chrome.sockets.tcp.create({
            name: "protostream-tcp-socket",
            persistent: true,
        }, (createInfo) => {

            // check error
            if (chrome.runtime.lastError) {
                this.disconnect();
                return callback(false, chrome.runtime.lastError.message);
            }

            this.socket = createInfo.socketId;

            // connect
            chrome.sockets.tcp.connect(this.socket, hostname, parseInt(port), (res) => {

                // check error
                if (chrome.runtime.lastError) {
                    this.disconnect();
                    return callback(false, chrome.runtime.lastError.message);
                }

                if (res != 0) {
                    // quit
                    this.disconnect();
                    return callback(false, "Could not connect socket (" + res + ").");
                }

                this.connected = true;

                log.info("Connected to " + hostname + ":" + port + ".");

                // (try to) disable delay
                chrome.sockets.tcp.setNoDelay(this.socket, true, (res) => {

                    var reason = chrome.runtime.lastError ? chrome.runtime.lastError.message : res.toString();

                    // check error
                    if (reason) {
                        log.warn("Could not set NoDelay (" + reason + ").");
                    }
                });

                // install handler for incoming data
                this._onReceiveListener = (info) => {
                    this.onReceive(info);
                };

                chrome.sockets.tcp.onReceive.addListener(this._onReceiveListener);

                // done
                return callback(true);
            });
        });
    }

    disconnect (callback? : (boolean, string?) => void) {

        callback = callback || (() => {});

        var log = this.getLogger("disconnect");

        if (this.socket == null) {
            // do nothing
            return log.warn("No socket available.");
        }

        // buffer values
        var socket = this.socket,
            connected = this.connected;

        // reset status
        this.socket = null;
        this.connected = false;
        this._incomingCB = null;

        // remove chrome api listener
        if (this._onReceiveListener) {
            chrome.sockets.tcp.onReceive.removeListener(this._onReceiveListener);
            this._onReceiveListener = null;
        }

        // perform disconnect
        if (connected) {
            chrome.sockets.tcp.disconnect(socket, () => {

                if (chrome.runtime.lastError) {
                    return callback(false, chrome.runtime.lastError.message);
                }

                // log disconnect() call
                log.log("Disconnected", "Disconnected.");

                // close socket
                chrome.sockets.tcp.close(socket, () => {

                    if (chrome.runtime.lastError) {
                        return log.error(chrome.runtime.lastError.message);
                    }

                    // done
                    return callback(true);
                });
            });
        }
    }
}
