define(function (require) {

    var Irc2me = require("src/Irc2me");
    var $      = require("jquery");

    var UserInput = function (jquery_context) {
        this.jquery_context = jquery_context;
    };

    var U = UserInput.prototype;

    /*
     * Setter
     *
     */

    /*
    U.setNetwork = function (network_id) {
        this.currentNetwork = network_id;
    };

    var setRecipient = function (recipient) {
        this.recipient = recipient;
    };

    U.setSendToUser = function (network_id, nickname) {
        this.setNetwork(network_id);
        setRecipient.call(this, [nickname]);
    };

    U.setSendToChannel = function (network_id, channel) {
        this.setNetwork(network_id);
        setRecipient.call(this, [channel]);
    };
    */

    U.setSendTo = function (network_id, recipients) {
        this.currentNetworkID = network_id;
        this.recipient        = recipients;
    };

    /*
     * Sending commands
     *
     */

    var protoMsgTypes;

    U.send = function (text, cb) {
        var self = this;

        // require network
        if (! self.currentNetworkID) { return; }

        var network_id = self.currentNetworkID;
        var recipient  = self.recipient;

        if (!text) { return; }

        var cmd, pars;

        if (text[0] == "/") {
            pars = text.slice(1).split(/\s/);
            cmd  = pars.shift().toUpperCase();
        }

        // Handle user command

        var types = Irc2me.ProtobufMessages.Network.Message.Type;

        console.log(types);

        if (cmd) {

            // make sure cmd is valid
            if (types.hasOwnProperty(cmd)) {

                var content = "";

                switch (cmd) {

                    case "PART": {
                        if (! self.currentChannel) { return; }

                        content = pars.join(" ");
                        pars    = [ self.currentChannel ];

                        break;
                    }

                    case "QUIT": {
                        content = pars.join(" ");
                        break;
                    }

                    case "NOTICE": {

                        // trailing text
                        var to  = pars.shift(),
                            txt = pars.join(" ");

                        content = txt;
                        pars    = [to];

                        break;
                    }

                    default: { }
                }

                Irc2me.sendCommand({
                    network_id: network_id,
                    command:    types[cmd],
                    parameters: pars,
                    content:    content,
                }, cb);
            }
        }

        // regular private message

        else {

            // require valid recipient
            if (! recipient) { return; }

            if (cmd == "ME") {
                var SOH = String.fromCharCode(1);
                text = SOH + "ACTION " + pars.join(" ") + SOH;
            }

            // send private message
            Irc2me.sendPrivateMessage({
                network_id: self.currentNetworkID,
                to:         recipient,
                text:       text,
            }, cb);
        }
    };

    /*
     * Key events
     *
     */

    U.bindKeyEvents = function () {
        var self = this;

        $("#input-prompt input", self.jquery_context).keypress(function (e) {

            var input = $(this);

            // enter key
            if (e.which == 13) {
                self.send(input.val(), function () {
                    input.val("");
                });
            }
        });
    };

    /*
     * End of module
     *
     */
    return UserInput;
});
