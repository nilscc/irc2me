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
    var ProtoBuf = require("ProtoBuf");

    var Irc2me = function() {
        var self = this;

        self._authenticated = false;

        // prepare connection
        self._protoStream = new ProtoStream();
    };

    /*
     * Protobuf messages
     *
     */

    Irc2me.ProtobufMessages = (function () {
        var proto = require("text!/messages.proto");
        return ProtoBuf.loadProto(proto).build("Irc2me");
    })();

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

    Irc2me.prototype.suspend = function () {
        var self = this;

        // suspend stream
        self.disconnect();
    }

    /*
     * Protobuf
     *
     */

    Irc2me.prototype.messageTypeByString = function (ty_string) {
        var self = this;

        var Type = Irc2me.ProtobufMessages.Network.Message.Type;

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
        var ServerMsg = Irc2me.ProtobufMessages.ServerMsg;

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
        var ServerMsg = Irc2me.ProtobufMessages.ServerMsg;

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
        return self._authenticated || false;
    }

    Irc2me.prototype.connect = function(hostname, port, username, password) {

        var self = this;

        // default logger
        var log = self.getLogger("connect", hostname, port, username, "***");

        // aliases
        var stream   = self._protoStream,
            messages = Irc2me.ProtobufMessages;

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
            messages = Irc2me.ProtobufMessages;

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

        var messages = Irc2me.ProtobufMessages,
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

        stream.sendMessage(msg);
    }

    /*
     * GET request messages
     *
     */

    // LIST requrests

    var list_request = function (lreq, cb) {
        var self = this;

        var messages = Irc2me.ProtobufMessages,
            stream = self._protoStream;

        // build client message
        var message = new messages.ClientMsg({
            get: new messages.ClientMsg.GET({
                list: lreq,
            }),
        });

        if (typeof cb == "function") {
            message.response_id = self.addCallback(function (resp) {
                if (resp.networks && resp.networks.length > 0) {
                    cb(resp.networks);
                }
            });
        }

        stream.sendMessage(message);
    };

    Irc2me.prototype.getNetworkList = function (cb) {
        var self = this;
        var LIST_NETWORKS = Irc2me.ProtobufMessages.ClientMsg.GET.ListRequest.LIST_NETWORKS;

        list_request.call(self, LIST_NETWORKS, cb);
    };


    Irc2me.prototype.getConversationList = function (cb) {
        var self = this;
        var LIST_CONVERSATIONS = Irc2me.ProtobufMessages.ClientMsg.GET.ListRequest.LIST_CONVERSATIONS;

        list_request.call(self, LIST_CONVERSATIONS, cb);
    };

    // Backlog requests

    var backlog_request = function (request, callback) {
        var self = this;

        var messages = Irc2me.ProtobufMessages,
            stream   = self._protoStream;

        // build message
        var message = new messages.ClientMsg({
            get: new messages.ClientMsg.GET({
                backlogs: request,
            }),
        });

        if (typeof callback == "function") {
            message.response_id = self.addCallback(function (resp) {
                if (resp.networks && resp.networks.length > 0) {
                    callback(resp.networks);
                }
            });
        }

        stream.sendMessage(message);
    };

    var new_backlog_request = function (network_id, options, callback) {
        var self = this;

        if (typeof options == "function") {
            callback = options;
            options = null;
        }
        options = options || {};

        // build request
        var request = new Irc2me.ProtobufMessages.ClientMsg.GET.BacklogRequest({
            network_id: network_id,
            limit: options.limit,
            before: options.before,
            after: options.after,
        });

        request.send = function () {
            backlog_request.call(self, request, callback);
        };

        return request;
    };

    // get network message backlog
    Irc2me.prototype.getNetworkBacklog = function (network_id, options, callback) {
        new_backlog_request.call(this, network_id, options, callback)
            .send();
    };

    // get private query backlog
    Irc2me.prototype.getQueryBacklog = function (network_id, nickname, options, callback) {
        var req = new_backlog_request.call(this, network_id, options, callback);
        req.query_nickname = nickname;
        req.send();
    };

    // get public channel backlog
    Irc2me.prototype.getChannelBacklog = function (network_id, channel, options, callback) {
        var req = new_backlog_request.call(this, network_id, options, callback);
        req.channel_name = channel;
        req.send();
    };

    /*
     * Chrome message interface: Incoming messages
     *
     */

    // connection
    Irc2me.connect     = new ChromeMessage("Irc2me.connect");
    Irc2me.isConnected = new ChromeMessage("Irc2me.isConnected");
    Irc2me.disconnect  = new ChromeMessage("Irc2me.disconnect");

    // sending messages
    Irc2me.sendMessage        = new ChromeMessage("Irc2me.sendMessage");
    Irc2me.sendPrivateMessage = new ChromeMessage("Irc2me.sendPrivateMessage");
    Irc2me.sendCommand        = new ChromeMessage("Irc2me.sendCommand");

    // GET requests
    Irc2me.getNetworkList       = new ChromeMessage("Irc2me.getNetworkList");
    Irc2me.getConversationList  = new ChromeMessage("Irc2me.getConversationList");

    // GET backlog requests
    Irc2me.getNetworkBacklog    = new ChromeMessage("Irc2me.getNetworkBacklog");
    Irc2me.getChannelBacklog    = new ChromeMessage("Irc2me.getChannelBacklog");
    Irc2me.getQueryBacklog      = new ChromeMessage("Irc2me.getQueryBacklog");

    Irc2me.prototype.listen = function () {

        var self = this;

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
            var PRIVMSG = Irc2me.ProtobufMessages.Network.Message.Type.PRIVMSG;

            self.sendMessage(netw, PRIVMSG, txt, to, sendResponse);

            return true; // async callback
        });

        Irc2me.sendCommand.addListener(function(content, sendResponse) {

            var netw = content.network_id,
                cmd  = content.command,
                cnt  = content.content || "",
                pars = content.parameters || [];

            // alias
            var Type = Irc2me.ProtobufMessages.Network.Message.Type;

            if (typeof cmd == "string") {
                cmd = self.messageTypeByString(cmd);
            }

            self.sendMessage(netw, cmd, cnt, pars, sendResponse);

            return true; // async callback
        });

        /*
         * GET requests
         *
         */

        Irc2me.getNetworkList.addListener(function(content, sendResponse) {
            self.getNetworkList(sendResponse);
            return true; // async
        });

        Irc2me.getConversationList.addListener(function(content, sendResponse) {
            self.getConversationList(sendResponse);
            return true; // async
        });

        // backlog requests

        Irc2me.getNetworkBacklog.addListener(function (content, sendResponse) {
            self.getNetworkBacklog(content.network_id, content, sendResponse);
            return true; // async
        });

        Irc2me.getChannelBacklog.addListener(function (content, sendResponse) {
            self.getChannelBacklog(content.network_id, content.channel_name, content, sendResponse);
            return true; // async
        });

        Irc2me.getQueryBacklog.addListener(function (content, sendResponse) {
            self.getQueryBacklog(content.network_id, content.query_nickname, content, sendResponse);
            return true; // async
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
