/*
 * Irc2me class definition
 *
 */

define(function (require) {

    "use strict";

    var ChromeMessage = require("src/ChromeMessage");
    var ProtoStream   = require("src/ProtoStream");
    var Logger        = require("src/Logger");

    // lib imports
    var ProtoBuf      = require("ProtoBuf");

    var Irc2me = function() {
        var self = this;

        self._authenticated = false;

        // prepare connection
        self._protoStream = new ProtoStream();
    };

    /*
     * Logging
     *
     */


    Irc2me.prototype.setLogger = function(callback) {

        var self = this;

        self._logger_cb = callback;

        // also set for protostream
        self._protoStream.setLogger(callback);
    }

    // Set up logger for internal logging
    Irc2me.prototype.getLogger = function(where) {
        return new Logger("Irc2me." + where, this._logger_cb);
    }

    /*
     * Suspend & restore
     *
     */

    Irc2me.prototype.init = function (cb) {
        var self = this;

        self._loadMessages(cb);
    }

    Irc2me.prototype.suspend = function () {
        var self = this;

        // suspend stream
        self.disconnect();
    }

    /*
     * Protobuf
     *
     */

    Irc2me.prototype._loadMessages = function(cb) {
        var self = this;

        ProtoBuf.loadProtoFile("messages.proto", function (err, builder) {

            if (err != null) {
                return console.error(err);
            }

            self._messages = builder.build("Irc2me");

            if (typeof cb == "function") {
                cb();
            }
        });
    }

    Irc2me.prototype.messageTypeByString = function (ty_string) {
        var self = this;

        var Type = self._messages.Network.Message.Type;

        if (Type.hasOwnProperty(ty_string.toUpperCase())) {
            return Type[ty_string.toUpperCase()];
        } else {
            return ty_string;
        }
    }

    Irc2me.prototype._handleAuthenticationResponse = function (buffer) {

        var self = this;

        var log = self.getLogger("_handleAuthenticationResponse");

        // alias
        var ServerMsg = self._messages.ServerMsg;

        // decode as ServerMsg
        var msg = ServerMsg.decode(buffer);

        /*
         * Authentication reply
         *
         */
        if (!self._authenticated) {

            if (msg.response_code == ServerMsg.ResponseCode.OK) {

                self._authenticated = true;

                // setup proper handler
                self._protoStream.setIncomingCallback(function (buffer) {
                    self._handleIncomingMessages(buffer);
                });

                log.info("Successfully authorized.");

                // send signal
                Irc2me.Signals.connected();
            }
            else {

                // authentication failed :(
                self._authenticated = false;

                // disconnect
                self._protoStream.disconnect();

                log.error("Not authorized.");

                // send signals
                Irc2me.Signals.disconnected();
            }
        }
    }

    Irc2me.prototype._handleIncomingMessages = function (buffer) {

        var self = this;

        // alias
        var ServerMsg = self._messages.ServerMsg;

        // decode as ServerMsg
        var msg = ServerMsg.decode(buffer);

        // lookup response ID & callbacks
        var cbs = self._callbacks = self._callbacks || {};
        var id  = msg.response_id;

        // check if 'id' has a callback
        if (id && cbs.hasOwnProperty(id)) {
            cbs[id](msg);
        } else {
            // otherwise notify all listeners
            Irc2me.Signals.incomingMessage(msg);
            console.log(msg);
        }
    }

    /*
     * Connect and authenticate
     *
     */

    Irc2me.prototype.isConnected = function () {
        return self._authenticated;
    }

    Irc2me.prototype.connect = function(hostname, port, username, password) {

        var self = this;

        // default logger
        var log = self.getLogger("connect", hostname, port, username, "***");

        // aliases
        var stream   = self._protoStream,
            messages = self._messages;

        stream.connect(hostname, port, function (success, errmsg) {

            if (!success && errmsg) {
                log.error(errmsg);

                // send signal
                Irc2me.Signals.disconnected();

                return;
            }

            stream.setIncomingCallback(function (buffer) {
                self._handleAuthenticationResponse(buffer);
            });

            // send authentication message
            stream.sendMessage(new messages.Authentication({
                login: username,
                password: password,
            }));
        });
    }

    Irc2me.prototype.disconnect = function () {
        var self = this;

        var log = self.getLogger("disconnect");

        // aliases
        var stream   = self._protoStream,
            messages = self._messages;

        // action to perform when actually disconnecting
        var do_disconnect = function (success, errmsg) {

            if (success == false) {
                log.error(errmsg);
            }

            self._protoStream.disconnect(function (success, errmsg) {

                if (!success) {
                    log.error(errmsg);
                }

                // set status & send signal
                self._authenticated = false;
                Irc2me.Signals.disconnected();
            });
        };

        // check if we have to send DISCONNECT message
        if (self._authenticated == true) {

            var DISCONNECT = new messages.ClientMsg({
                system_msg: messages.SystemMsg.DISCONNECT,
            });

            // send DISCONNECT message
            stream.sendMessage(DISCONNECT, do_disconnect);

        } else {
            do_disconnect();
        }
    }

    /*
     * Sending messages
     *
     */

    // Add a callback, returns the callback ID
    Irc2me.prototype.addCallback = function (cb) {

        var self = this;

        // get (and alias) next ID
        var id  = self._nextId = (self._nextId || 0) + 1;

        // init (and alias) callback object
        var cbs = self._callbacks = self._callbacks || {};

        // store callback
        cbs[id] = cb;

        return id;
    }

    Irc2me.prototype.sendMessage = function(network_id, type, content, parameters, cb) {

        var self = this;

        var messages = self._messages,
            stream   = self._protoStream;

        var msg = new messages.ClientMsg({
            send: new messages.ClientMsg.SendMessage({
                network_id: network_id,
                content: content,
            }),
        });

        if (typeof parameters == "object" && parameters.hasOwnProperty("nick")) {
            msg.send.to_user = parameters;
        }
        else {
            msg.send.params = parameters;
        }

        if (typeof type == "string") {
            msg.send.type_raw = type;
        } else {
            msg.send.type = type;
        }

        if (typeof cb == "function") {
            msg.response_id = self.addCallback(cb);
        }

        console.log(msg);

        stream.sendMessage(msg);
    }


    /*
     * Chrome message interface: Incoming messages
     *
     */

    // data
    Irc2me.getProtobufMesageTypes = new ChromeMessage("Irc2me.getProtobufMesageTypes");
    Irc2me.getProtobufUserflags   = new ChromeMessage("Irc2me.getProtobufUserflags");

    // connection
    Irc2me.connect     = new ChromeMessage("Irc2me.connect");
    Irc2me.isConnected = new ChromeMessage("Irc2me.isConnected");
    Irc2me.disconnect  = new ChromeMessage("Irc2me.disconnect");

    // sending messages
    Irc2me.sendMessage        = new ChromeMessage("Irc2me.sendMessage");
    Irc2me.sendPrivateMessage = new ChromeMessage("Irc2me.sendPrivateMessage");
    Irc2me.sendCommand        = new ChromeMessage("Irc2me.sendCommand");

    Irc2me.prototype.listen = function () {

        var self = this;

        Irc2me.getProtobufUserflags.addListener(function (content, sendResponse) {
            var send = function () { sendResponse(self._messages.Network.User.Userflag); };

            if (!self._messages) {
                self._loadMessages(send);
                return true; // _loadMessages is async
            } else {
                send();
            }
        });

        Irc2me.getProtobufMesageTypes.addListener(function (content, sendResponse) {
            var send = function () { sendResponse(self._messages.Network.Message.Type); };

            if (!self._messages) {
                self._loadMessages(send);
                return true; // _loadMessages is async
            } else {
                send();
            }
        });

        Irc2me.connect.addListener(function(content, sendResponse) {

            var host = content.hostname,
                port = content.port,
                user = content.username,
                pass = content.password;

            // connect and send authentication message
            self.connect(host, port, user, pass);
        });

        Irc2me.disconnect.addListener(function(content, sendResponse) {
            self.disconnect(sendResponse);
        });

        Irc2me.isConnected.addListener(function(content, sendResponse) {
            sendResponse(self._authenticated === true);
        });

        Irc2me.sendMessage.addListener(function(content, sendResponse) {

            var netw = content.network_id,
                type = content.type,
                cont = content.content,
                pars = content.parameters;

            self.sendMessage(netw, type, cont, pars, sendResponse);

            return true; // async callback
        });

        Irc2me.sendPrivateMessage.addListener(function(content, sendResponse) {

            var netw = content.network_id,
                to   = content.to,
                txt  = content.text;

            // alias
            var PRIVMSG = self._messages.Network.Message.Type.PRIVMSG;

            self.sendMessage(netw, PRIVMSG, txt, to, sendResponse);

            return true; // async callback
        });

        Irc2me.sendCommand.addListener(function(content, sendResponse) {

            var netw = content.network_id,
                cmd  = content.command,
                cnt  = content.content || "",
                pars = content.parameters || [];

            // alias
            var Type = self._messages.Network.Message.Type;

            if (typeof cmd == "string") {
                cmd = self.messageTypeByString(cmd);
            }

            self.sendMessage(netw, cmd, cnt, pars, sendResponse);

            return true; // async callback
        });
    }

    /*
     * Chrome message interface: Outgoing signals
     *
     */

    Irc2me.Signals = {};

    Irc2me.Signals.connected        = new ChromeMessage("Irc2me.Signals.connected");
    Irc2me.Signals.disconnected     = new ChromeMessage("Irc2me.Signals.disconnected");
    Irc2me.Signals.incomingMessage  = new ChromeMessage("Irc2me.Signals.incomingMessage");

    /*
     * End of module
     *
     */
    return Irc2me;
});
