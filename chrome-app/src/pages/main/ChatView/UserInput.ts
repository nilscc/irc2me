/// <reference path="../../../libs/jquery" />

"use strict";

import Irc2me = require("Irc2me");
import $      = require("jquery");

// load protobuf message types
var protoMsgTypes = (() => {
    var res = {};
    var tys = Irc2me.ProtobufMessages.Network.Message.Type;
    for (var ty in tys) {
        res[tys[ty]] = ty;
    }
    return res;
})();

export class Class {
    private jquery_context;

    constructor (jquery_context) {
        this.jquery_context = jquery_context;
    }

    /*
     * jQuery helper
     *
     */

    input () {
        return $("#input-prompt input", this.jquery_context);
    }

    focus () {
        this.input().focus();
    }

    /*
     * Setter
     *
     */

    private currentNetworkID : number;
    private currentChannel   : string;
    private recipient        : string[];

    setSendTo (network_id, recipients) {
        this.currentNetworkID = network_id;
        this.recipient        = recipients;
    }

    /*
     * Sending commands
     *
     */

    send (text, cb?) {

        // require network
        if (! this.currentNetworkID) { return; }

        var network_id = this.currentNetworkID;
        var recipient  = this.recipient;

        if (!text) { return; }

        var cmd, pars;

        if (text[0] == "/") {
            pars = text.slice(1).split(/\s/);
            cmd  = pars.shift().toUpperCase();
        }

        // Handle user command

        var types = Irc2me.ProtobufMessages.Network.Message.Type;

        if (cmd) {

            // make sure cmd is valid
            if (types.hasOwnProperty(cmd)) {

                var content = "";

                switch (cmd) {

                    case "PART": {
                        if (! this.currentChannel) { return; }

                        content = pars.join(" ");
                        pars    = [ this.currentChannel ];

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

                Irc2me.sendCommand.call({
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
            Irc2me.sendPrivateMessage.call({
                network_id: this.currentNetworkID,
                to:         recipient,
                text:       text,
            }, cb);
        }
    }

    /*
     * Key events
     *
     */

    bindKeyEvents () {

        var input = this.input();

        input.keydown((e) => {

            e.stopPropagation();

            // enter key
            if (e.which == 13) {
                e.preventDefault();
                this.send(input.val(), () => {
                    input.val("");
                });
            }
        });
    }

}
