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
    self._disconnect();
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

    // alias
    var ServerMsg = self._messages.ServerMsg;

    // decode as ServerMsg
    var msg = ServerMsg.decode(buffer);

    /*
     * Authentication reply
     *
     */
    if (self._authenticated == false || typeof self._authenticated == "function") {

        if (msg.response_code == ServerMsg.ResponseCode.OK) {

            // properly authenticated, run callback (if any)
            if (typeof self._authenticated == "function") {
                self._authenticated();
            }

            self._authenticated = true;

            // setup proper handler
            self._protoStream.setIncomingCallback(function (buffer) {
                self._handleIncomingMessages(buffer);
            });

            // send signal
            Irc2me.Signals.connected();

        } else {
            // authentication failed :(
            self._authenticated = false;

            // send signal
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

    console.log(msg); // TODO;
}

/*
 * Connect and authenticate
 *
 */

Irc2me.prototype._connect = function(hostname, port, username, password) {

    var self = this;

    // aliases
    var stream   = self._protoStream,
        messages = self._messages;

    stream.connect(hostname, port, function () {

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

Irc2me.prototype._disconnect = function (cb) {
    var self = this;

    // aliases
    var stream   = self._protoStream,
        messages = self._messages;

    if (self._authenticated) {

        var DISCONNECT = new messages.ClientMsg({
            system_msg: messages.SystemMsg.DISCONNECT,
        });

        // send DISCONNECT message
        stream.sendMessage(DISCONNECT, function() {
            self._protoStream.disconnect(cb);
        });

    } else {
        self._protoStream.disconnect(cb);
    }

    self._authenticated = false;

    Irc2me.Signals.disconnected();
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

        // store callback
        self._authenticated = sendResponse;

        // connect and send authentication message
        self._connect(host, port, user, pass);

        // async response
        return true;
    });

    Irc2me.disconnect.addListener(function(content, sendResponse) {
        self._disconnect(sendResponse);
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

Irc2me.Signals.connected    = new ChromeMessage("Irc2me.Signals.connected");
Irc2me.Signals.disconnected = new ChromeMessage("Irc2me.Signals.disconnected");
