/// <reference path="../../../libs/jquery" />
/// <reference path="../../../libs/linkify-string" />
/// <reference path="../../../libs/mustache" />
/// <amd-dependency path="Mustache" />

"use strict";

import $ = require("jquery");
import linkify = require("linkify-string");

import Irc2me = require("Irc2me");
import Long   = require("Long");
import Helper = require("common/Helper");
import Types  = require("common/Types");

import MessageTemplate = require("pages/main/templates/Message");

// load protobuf message types
var protoMsgTypes = {};
var tys = Irc2me.ProtobufMessages.Network.Message.Type;
for (var ty in tys) {
    protoMsgTypes[tys[ty]] = ty;
}

/*
 * Class definition
 *
 */

export class Class {

    private jquery_context;

    constructor (jquery_context?) {
        this.jquery_context = jquery_context;
    }

    /*
     * JQuery helper
     *
     */

    private _messagelist;

    private messagelist (selector?) {

        if (! this._messagelist) {
            this._messagelist = $("#message-list", this.jquery_context);
        }

        if (selector) {
            return $(selector, this._messagelist);
        }
        else {
            return this._messagelist;
        }
    }

    /*
     * Mustache template helper functions
     *
     */

    private compileMessageTemplate (template_data) {
        return $(Mustache.to_html(MessageTemplate, { messages: template_data }));
    }

    private getTemplateData (message) {

        var epoch = Long.prototype.toNumber.call(message.timestamp);
        var date  = new Date(epoch);

        var template_data : any = {
            timestamp: date.toLocaleTimeString(),
            content: message.content,
            classes: [],
        };

        // figure out who wrote the message

        if (message.user) {
            template_data.user = message.user;
            template_data.user.flag = "";
        } else {
            template_data.server = message.server;
        }

        // figure out message type

        var set_type = (ty) => {
            template_data[ty] = true;
            template_data.classes.push("type-" + ty);
        }

        if (message.type == "known") {
            set_type(protoMsgTypes[message[message.type]].toLowerCase());
        }

        if (template_data.content) {
            template_data.content = Helper.escapeHtml(template_data.content);
        }

        return template_data;
    }

    private renderMessages (messages, cb?) {

        var template_data = [];
        for (var i = 0; i < messages.length; i++) {
            var message = messages[i];
            var data    = this.getTemplateData(message);

            if (typeof cb === "function" && cb(message, data) === true) {
                continue;
            }

            //if (! this.lastMessage) { // TODO: idk what i tried to do here :)
            //}

            template_data.push(data);
        }

        // the compiled template
        var msg = this.compileMessageTemplate(template_data);

        // make links clickable
        $(".text", msg).html((_, t) => {
            return linkify(t, {
                linkAttributes: {
                    tabindex: -1,
                },
            });
        });

        return msg;
    }

    /*
     * Keep track of message timestamps
     *
     */

    private oldestMessageTimestamp : Date;
    private newestMessageTimestamp : Date;

    private isOlder (message : Types.Message) {
        return (this.oldestMessageTimestamp != null && message.timestamp != null)
            && (Helper.messageTimestamp(message) < this.oldestMessageTimestamp);
    }

    private isNewer (message : Types.Message) {
        return (this.newestMessageTimestamp != null && message.timestamp != null)
            && (this.newestMessageTimestamp < Helper.messageTimestamp(message));
    }

    // set oldest/newest message timestamp
    private updateTimestamps (first : Types.Message, last : Types.Message) {

        var f = Helper.messageTimestamp(first),
            l = Helper.messageTimestamp(last);

        if (! this.oldestMessageTimestamp || f < this.oldestMessageTimestamp) {
            this.oldestMessageTimestamp = f;
        }
        if (! this.newestMessageTimestamp || this.newestMessageTimestamp < l) {
            this.newestMessageTimestamp = l;
        }
    }

    private resetTimestamps () {
        this.oldestMessageTimestamp = null;
        this.newestMessageTimestamp = null;
    }

    /*
     * Load messages
     *
     */

    load (messages : Types.Message[], callback?) {

        // reset all
        this.resetTimestamps();
        this.messagelist().empty();

        this.append(messages, callback);
    }


    append (messages : Types.Message[], callback?) {

        if (messages.length == 0) { return; }

        console.log("append:", messages);

        // first and last message
        var first = messages[0],
            last  = messages[messages.length - 1];

        // render messages
        var rendered = this.renderMessages(messages);

        // jquery ui element
        var messagelist = this.messagelist();

        if (this.isOlder(last)) {
            messagelist.prepend(rendered);
            // TODO: update scroll position
        }
        else {
            if (this.newestMessageTimestamp && !this.isNewer(first)) {
                console.warn("Messages not in chronological order", messages);
            }

            // get current scroll position
            var atBottom = Helper.scrollAtBottom(messagelist);

            messagelist.append(rendered);

            // update scroll position
            if (atBottom) {
                Helper.scrollToBottom(messagelist);
            }
        }

        this.updateTimestamps(first, last);
    }
}
