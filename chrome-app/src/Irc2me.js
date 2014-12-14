/*
 * Irc2me class definition
 *
 */

function Irc2me()
{
    var self = this;

    self._authenticated = false;

    // prepare connection
    self._protoStream = new ProtoStream();
}

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

    dcodeIO.ProtoBuf.loadProtoFile("messages.proto", function (err, builder) {

        if (err != null) {
            return console.error(err);
        }

        self._messages = builder.build("Irc2me");

        if (typeof cb == "function") {
            cb();
        }
    });
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

    Irc2me.Signals.incomingMessage(msg);

    console.log(msg);
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
 * Chrome message interface: Incoming messages
 *
 */

Irc2me.connect     = new ChromeMessage("Irc2me.connect");
Irc2me.isConnected = new ChromeMessage("Irc2me.isConnected");
Irc2me.disconnect  = new ChromeMessage("Irc2me.disconnect");

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

}

/*
 * Chrome message interface: Outgoing signals
 *
 */

Irc2me.Signals = {};

Irc2me.Signals.connected        = new ChromeMessage("Irc2me.Signals.connected");
Irc2me.Signals.disconnected     = new ChromeMessage("Irc2me.Signals.disconnected");
Irc2me.Signals.incomingMessage  = new ChromeMessage("Irc2me.Signals.incomingMessage");
