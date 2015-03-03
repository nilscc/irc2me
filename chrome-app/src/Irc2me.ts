"use strict";

import proto_messages = require("proto/messages");
import ProtoBuf       = require("src/libs/dcodeIO/ProtoBuf");

import ChromeMessage  = require("src/ChromeMessage");
import Logger         = require("src/Logger");

import protostream    = require("src/ProtoStream");
import ProtoStream    = protostream.ProtoStream;

/*
 * Protobuf messages
 *
 */

export var ProtobufMessages = ProtoBuf.loadProto(proto_messages).build("Irc2me");

/*
 * Messages & signals
 *
 */

// connection
export var connect              = new ChromeMessage("Irc2me.connect");
export var isConnected          = new ChromeMessage("Irc2me.isConnected");
export var disconnect           = new ChromeMessage("Irc2me.disconnect");

// sending messages
export var sendMessage          = new ChromeMessage("Irc2me.sendMessage");
export var sendPrivateMessage   = new ChromeMessage("Irc2me.sendPrivateMessage");
export var sendCommand          = new ChromeMessage("Irc2me.sendCommand");

// GET requests
export var getNetworkList       = new ChromeMessage("Irc2me.getNetworkList");
export var getConversationList  = new ChromeMessage("Irc2me.getConversationList");

// GET backlog requests
export var getNetworkBacklog    = new ChromeMessage("Irc2me.getNetworkBacklog");
export var getChannelBacklog    = new ChromeMessage("Irc2me.getChannelBacklog");
export var getQueryBacklog      = new ChromeMessage("Irc2me.getQueryBacklog");

// Signals
module Signals {
    export var connected        = new ChromeMessage("Irc2me.Signals.connected");
    export var disconnected     = new ChromeMessage("Irc2me.Signals.disconnected");
    export var incomingMessage  = new ChromeMessage("Irc2me.Signals.incomingMessage");
}

/*
 * Irc2me class definition
 *
 */

export class Class {

    private _authenticated : boolean;

    private _protoStream : ProtoStream;

    constructor () {
        this._authenticated = false;

        // prepare connection
        this._protoStream = new ProtoStream();
    }

    /*
     * Logging
     *
     */

    private _logger_cb : (Object) => void;

    private setLogger (callback) {

        this._logger_cb = callback;

        // also set for protostream
        this._protoStream.setLogger(callback);
    }

    // Set up logger for internal logging
    private getLogger (where) {
        return new Logger("Irc2me." + where, this._logger_cb);
    }

    /*
     * Suspend & restore
     *
     */

    suspend () {
        // suspend stream
        this.disconnect();
    }

    /*
     * Protobuf
     *
     */

    messageTypeByString (ty_string) {
        var Type = ProtobufMessages.Network.Message.Type;

        if (Type.hasOwnProperty(ty_string.toUpperCase())) {
            return Type[ty_string.toUpperCase()];
        } else {
            return ty_string;
        }
    }

    private _handleAuthenticationResponse (buffer) {

        var log = this.getLogger("_handleAuthenticationResponse");

        // alias
        var ServerMsg = ProtobufMessages.ServerMsg;

        // decode as ServerMsg
        var msg = ServerMsg.decode(buffer);

        /*
         * Authentication reply
         *
         */
        if (!this._authenticated) {

            if (msg.response_code == ServerMsg.ResponseCode.OK) {

                this._authenticated = true;

                // setup proper handler
                this._protoStream.setIncomingCallback((buffer) => {
                    this._handleIncomingMessages(buffer);
                });

                log.info("Successfully authorized.");

                // send signal
                Signals.connected.call();
            }
            else {

                // authentication failed :(
                this._authenticated = false;

                // disconnect
                this._protoStream.disconnect();

                log.error("Not authorized.");

                // send signals
                Signals.disconnected.call();
            }
        }
    }

    private _callbacks : any;

    private _handleIncomingMessages (buffer) {

        var self = this;

        // alias
        var ServerMsg = ProtobufMessages.ServerMsg;

        // decode as ServerMsg
        var msg = ServerMsg.decode(buffer);

        // lookup response ID & callbacks
        var cbs = this._callbacks = this._callbacks || {};
        var id  = msg.response_id;

        // check if 'id' has a callback
        if (id && cbs.hasOwnProperty(id)) {
            cbs[id](msg);
        } else {
            // otherwise notify all listeners
            Signals.incomingMessage.call(msg);
            console.log(msg);
        }
    }

    /*
     * Connect and authenticate
     *
     */

    isConnected () {
        return this._authenticated || false;
    }

    connect (hostname, port, username, password) {

        // default logger
        var log = this.getLogger("connect(" + [hostname , port, username, "***"].join(", ") + ")");

        // aliases
        var stream   = this._protoStream,
            messages = ProtobufMessages;

        stream.connect(hostname, port, (success, errmsg) => {

            if (!success && errmsg) {
                log.error(errmsg);

                // send signal
                Signals.disconnected.call();

                return;
            }

            stream.setIncomingCallback((buffer) => {
                this._handleAuthenticationResponse(buffer);
            });

            // send authentication message
            stream.sendMessage(new messages.Authentication({
                login: username,
                password: password,
            }));
        });
    }

    disconnect () {

        var log = this.getLogger("disconnect");

        // aliases
        var stream   = this._protoStream,
            messages = ProtobufMessages;

        // action to perform when actually disconnecting
        var do_disconnect = (success = true, errmsg?) => {

            if (success == false) {
                log.error(errmsg);
            }

            this._protoStream.disconnect((success, errmsg) => {

                if (!success) {
                    log.error(errmsg);
                }

                // set status & send signal
                this._authenticated = false;
                Signals.disconnected.call();
            });
        };

        // check if we have to send DISCONNECT message
        if (this._authenticated == true) {

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

    private _nextId : number;

    // Add a callback, returns the callback ID
    addCallback (cb) {

        // get (and alias) next ID
        var id  = this._nextId = (this._nextId || 0) + 1;

        // init (and alias) callback object
        var cbs = this._callbacks = this._callbacks || {};

        // store callback
        cbs[id] = cb;

        return id;
    }

    sendMessage (network_id, type, content, parameters, cb) {

        var messages = ProtobufMessages,
            stream   = this._protoStream;

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
            msg.response_id = this.addCallback(cb);
        }

        stream.sendMessage(msg);
    }

    /*
     * GET request messages
     *
     */

    // LIST requrests

    private list_request (lreq, cb) {

        var messages = ProtobufMessages,
            stream = this._protoStream;

        // build client message
        var message = new messages.ClientMsg({
            get: new messages.ClientMsg.GET({
                list: lreq,
            }),
        });

        if (typeof cb == "function") {
            message.response_id = this.addCallback((resp) => {
                if (resp.networks && resp.networks.length > 0) {
                    cb(resp.networks);
                }
            });
        }

        stream.sendMessage(message);
    }

    getNetworkList (cb) {

        var LIST_NETWORKS = ProtobufMessages.ClientMsg.GET.ListRequest.LIST_NETWORKS;

        this.list_request.call(this, LIST_NETWORKS, cb);
    }

    getConversationList (cb) {

        var LIST_CONVERSATIONS = ProtobufMessages.ClientMsg.GET.ListRequest.LIST_CONVERSATIONS;

        this.list_request.call(this, LIST_CONVERSATIONS, cb);
    }

    // Backlog requests

    private backlog_request (request, callback) {

        var messages = ProtobufMessages,
            stream   = this._protoStream;

        // build message
        var message = new messages.ClientMsg({
            get: new messages.ClientMsg.GET({
                backlogs: request,
            }),
        });

        if (typeof callback == "function") {
            message.response_id = this.addCallback((resp) => {
                if (resp.networks && resp.networks.length > 0) {
                    callback(resp.networks);
                }
            });
        }

        stream.sendMessage(message);
    }

    private new_backlog_request (network_id, options, callback) {
        var self = this;

        if (typeof options == "function") {
            callback = options;
            options = null;
        }
        options = options || {};

        // build request
        var request = new ProtobufMessages.ClientMsg.GET.BacklogRequest({
            network_id: network_id,
            limit: options.limit,
            before: options.before,
            after: options.after,
        });

        request.send = () => {
            this.backlog_request.call(this, request, callback);
        };

        return request;
    }

    // get network message backlog
    getNetworkBacklog (network_id, options, callback) {
        this.new_backlog_request.call(this, network_id, options, callback)
            .send();
    }

    // get private query backlog
    getQueryBacklog (network_id, nickname, options, callback) {
        var req = this.new_backlog_request.call(this, network_id, options, callback);
        req.query_nickname = nickname;
        req.send();
    }

    // get public channel backlog
    getChannelBacklog (network_id, channel, options, callback) {
        var req = this.new_backlog_request.call(this, network_id, options, callback);
        req.channel_name = channel;
        req.send();
    }

    /*
     * Chrome message interface: Incoming messages
     *
     */

    listen = function () {

        connect.addListener((content, sendResponse) => {

            var host = content.hostname,
                port = content.port,
                user = content.username,
                pass = content.password;

            // connect and send authentication message
            this.connect(host, port, user, pass);
        });

        disconnect.addListener((content, sendResponse) => {
            this.disconnect(sendResponse);
        });

        isConnected.addListener((content, sendResponse) => {
            sendResponse(this._authenticated === true);
        });

        sendMessage.addListener((content, sendResponse) => {

            var netw = content.network_id,
                type = content.type,
                cont = content.content,
                pars = content.parameters;

            this.sendMessage(netw, type, cont, pars, sendResponse);

            return true; // async callback
        });

        sendPrivateMessage.addListener((content, sendResponse) => {

            var netw = content.network_id,
                to   = content.to,
                txt  = content.text;

            // alias
            var PRIVMSG = ProtobufMessages.Network.Message.Type.PRIVMSG;

            this.sendMessage(netw, PRIVMSG, txt, to, sendResponse);

            return true; // async callback
        });

        sendCommand.addListener((content, sendResponse) => {

            var netw = content.network_id,
                cmd  = content.command,
                cnt  = content.content || "",
                pars = content.parameters || [];

            // alias
            var Type = ProtobufMessages.Network.Message.Type;

            if (typeof cmd == "string") {
                cmd = this.messageTypeByString(cmd);
            }

            this.sendMessage(netw, cmd, cnt, pars, sendResponse);

            return true; // async callback
        });

        /*
         * GET requests
         *
         */

        getNetworkList.addListener((content, sendResponse) => {
            this.getNetworkList(sendResponse);
            return true; // async
        });

        getConversationList.addListener((content, sendResponse) => {
            this.getConversationList(sendResponse);
            return true; // async
        });

        // backlog requests

        getNetworkBacklog.addListener((content, sendResponse) => {
            this.getNetworkBacklog(content.network_id, content, sendResponse);
            return true; // async
        });

        getChannelBacklog.addListener((content, sendResponse) => {
            this.getChannelBacklog(content.network_id, content.channel_name, content, sendResponse);
            return true; // async
        });

        getQueryBacklog.addListener((content, sendResponse) => {
            this.getQueryBacklog(content.network_id, content.query_nickname, content, sendResponse);
            return true; // async
        });
    }

    /*
     * Chrome message interface: Outgoing signals
     *
     */

}
