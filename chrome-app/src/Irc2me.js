/*
 * Irc2me class definition
 *
 */

function Irc2me()
{
    var self = this;

    self._authenticated = false;

    // runtime storagement
    self._runtime = new RuntimeStorage(self);
}

/*
 * Logging
 *
 */


Irc2me.prototype.setLogger = function(callback)
{
    this._logger_cb = callback;
}

// Set up logger for internal logging
Irc2me.prototype.getLogger = function(where)
{
    return new Logger("Irc2me." + where, this._logger_cb);
}

// external logging
Irc2me.prototype.log = function(statusObject)
{
    this._logger_cb(statusObject);
}

/*
 * Suspend & restore
 *
 */

Irc2me.prototype.suspend = function () {
    var self = this;

    self._runtime.storePrivateValues();

    // suspend stream
    if (self._protoStream != null) {
        self._protoStream.suspend();
    }
}

Irc2me.restore = function () {

    var self = new Irc2me();

    self._runtime.restorePrivateValues();

    // restore stream
    self._protoStream = ProtoStream.restore();

    return self;
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

Irc2me.prototype._handleIncomingMessages = function (buffer) {

    var self = this;

    // alias
    var ServerMsg = self._messages.ServerMsg;

    // decode as ServerMsg
    var msg = ServerMsg.decode(buffer);

    console.log(msg);

    /*
     * Authentication reply
     *
     */
    if (self._authenticated == false || typeof self._authenticated == "function") {

        if (msg.response_code == ServerMsg.ResponseCode.OK) {
            if (typeof self._authenticated == "function") {
                self._authenticated();
            }
            self._authenticated = true;
        } else {
            self._authenticated = false;
        }

        return; // authentication reply has no other message fields
    }

    /*
     * Other
     *
     */
    return;
}

/*
 * Connect and authenticate
 *
 */

Irc2me.prototype._connect = function(hostname, port, username, password) {

    var self = this;

    // load protobuf messages if not done already
    if (self._messages == null) {
        return self._loadMessages(function () {
            self._connect(hostname, port, username, password);
        });
    }

    // aliases
    var stream   = self._protoStream,
        messages = self._messages;

    stream.connect(hostname, port, function () {

        stream.setIncomingCallback(function (buffer) {
            self._handleIncomingMessages(buffer);
        });

        // send authentication message
        stream.sendMessage(new messages.Authentication({
            login: username,
            password: password,
        }));

    });
}

Irc2me.prototype._disconnect = function () {
    var self = this;
    self._protoStream.disconnect();
    self._authenticated = false;
}

/*
 * Chrome message interface
 *
 */

Irc2me.connect    = new ChromeMessage("Irc2me.connect");
Irc2me.disconnect = new ChromeMessage("Irc2me.disconnect");

Irc2me.prototype.setListeners = function () {

    var self = this;

    Irc2me.connect.setListener(function(content, sendResponse) {

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

    Irc2me.disconnect.setListener(function(content, sendResponse) {
        self._disconnect();
    });

}
