/// <reference path="../../libs/jquery" />
/// <reference path="../../libs/linkify-string" />

"use strict";

import $                = require("jquery");
import linkify          = require("linkify-string");
import Long             = require("Long");

import Backlog          = require("Backlog");
import Irc2me           = require("Irc2me");
import Helper           = require("common/Helper");
import Types            = require("common/Types");

import ConversationList = require("pages/main/ChatView/ConversationList");
import MessageView      = require("pages/main/ChatView/MessageView");
import UserInput        = require("pages/main/ChatView/UserInput");
import UserList         = require("pages/main/ChatView/UserList");

export class Class {

    private backlog : Backlog.Class;
    private jquery_context;

    private currentNetworkID : number;
    private currentChannel   : string;
    private currentQuery     : Types.User;

    private messageview      : MessageView.Class;
    private conversationlist : ConversationList.Class;
    private userinput        : UserInput.Class;
    private userlist         : UserList.Class;

    private messageCallback  : (msg : Types.Message, data?) => void;

    constructor (backlog : Backlog.Class, context?) {
        this.jquery_context = context;

        this.backlog          = backlog;
        this.messageview      = new MessageView.Class(context);
        this.conversationlist = new ConversationList.Class(context);
        this.userinput        = new UserInput.Class(context);
        this.userlist         = new UserList.Class(context);
    }

    /*
     * jQuery helper
     *
     */

    private mainview (selector? : string) {
        var mv = $("#main-view", this.jquery_context);

        if (selector !== null) {
            return $(selector, mv);
        }
        else {
            return mv;
        }
    }

    /*
     * UI helper
     *
     */

    private setCurrent (network_id, channel, queryuser) {

        this.currentNetworkID = network_id;
        this.currentChannel   = channel;
        this.currentQuery     = queryuser;

        this.userinput.setSendTo(network_id, [channel || queryuser.nick]);
    }

    private setTopic (channel_name, topic?) {
        $("#channel-name").html(channel_name);
        $("#topic").html(Helper.escapeHtml(topic || "")).html(function (_, h) {
            return linkify(h);
        });
    }

    /*
     * Load backlog for channel/network
     *
     */

    private loadNetwork (network_id) {

        var netw = this.backlog.network(network_id);

        this.messageCallback = null

        this.messageview.load(netw.messages);
        this.userlist.hide();
        this.conversationlist.setActiveNetwork(network_id);

        this.userinput.focus();

        this.setCurrent(network_id, null, null);
    }

    private loadChannel (network_id, channel) {

        var chan = this.backlog.channel(network_id, channel);

        this.messageCallback = (message, data) => {
            if (data.topic && data.server) {
                this.setTopic(channel, data.content);
                return true;
            }
        };

        this.messageview.load(chan.messages, this.messageCallback);
        this.userlist.load(chan.users);
        this.conversationlist.setActiveChannel(network_id, channel);

        this.setTopic(channel, chan.topic);
        this.userinput.focus();

        this.setCurrent(network_id, channel, null);
    }

    private loadQuery (network_id, user : Types.User) {

        var query = this.backlog.query(network_id, user);

        // add user to messages
        for (var i = 0; i < query.messages.length; i++) {
            query.messages[i].user = query.messages[i].user || user;
        }

        this.messageCallback = null

        this.messageview.load(query.messages);
        this.userlist.hide();
        this.conversationlist.setActiveQuery(network_id, user);

        this.setTopic(Helper.userFullname(user));
        this.userinput.focus();

        this.setCurrent(network_id, null, user);
    }


    /*
     * Initialize conversation list
     *
     */

    loadConversations () {

        Irc2me.getConversationList.call((networks) => {

            var first = true;

            for (var i = 0; i < networks.length; i++) {

                var network     = networks[i],
                    network_id  = Long.prototype.toNumber.call(network.id);

                this.conversationlist.addNetwork(network_id, network.name);

                // add channels
                for (var j = 0; j < network.channels.length; j++) {
                    var channel = network.channels[j];

                    // add channel to conversation list
                    this.conversationlist.addChannel(network_id, channel,
                        ((network_id, channel_name) => {
                            this.loadChannel(network_id, channel_name);
                        }).bind(this, network_id, channel.name)
                    );

                    // load the first channel
                    if (first) {
                        this.loadChannel(network_id, channel.name);
                        first = false;
                    }
                }

                // add queries
                for (var j = 0; j < network.queries.length; j++) {
                    var query = network.queries[j];

                    // add query to conversation list
                    this.conversationlist.addQuery(network_id, query.user,
                        ((network_id, user) => {
                            this.loadQuery(network_id, user);
                        }).bind(this, network_id, query.user)
                    );
                }
            }
        });
    }

    /*
     * Load/handle new messages
     *
     */

    private unreadNetworkMessage (network_id : number) {
    }

    private unreadPublicMessage (network_id : number, channel : string) {
    }

    private unreadPrivateMessage (network_id : number, user : Types.User) {
    }

    listen () {

        // Helper

        var isCurrentNetwork = (network_id) => {
            return this.currentNetworkID === network_id;
        }
        var isCurrentChannel = (network_id, channel) => {
            return isCurrentNetwork(network_id)
                && this.currentChannel === channel;
        }
        var isCurrentQuery = (network_id, user) => {
            return isCurrentNetwork(network_id)
                && typeof this.currentQuery == "object"
                && this.currentQuery
                && Helper.userFullname(this.currentQuery) === Helper.userFullname(user);
        }

        // Setup backlog subscriptions

        this.backlog.subscribe(Backlog.newMessageSubscriptionID, (nid, messages) => {
            if (isCurrentNetwork(nid) && !this.currentChannel) {
                this.messageview.append(messages);
            } else {
                this.unreadNetworkMessage(nid);
            }
        });

        this.backlog.subscribe(Backlog.newPublicMessageSubscriptionID, (nid, chan, messages) => {
            if (isCurrentChannel(nid, chan)) {
                this.messageview.append(messages, this.messageCallback);
            } else {
                this.unreadPublicMessage(nid, chan);
            }
        });

        this.backlog.subscribe(Backlog.newPrivateMessageSubscriptionID, (nid, user, messages) => {
            if (isCurrentQuery(nid, user)) {
                for (var i = 0; i < messages.length; i++) {
                    messages[i].user = messages[i].user || user;
                }
                this.messageview.append(messages);
            } else {
                this.unreadPrivateMessage(nid, user);
            }
        });
    }

    /*
     * Handle user input
     *
     */

    bindKeyEvents () {

        this.userinput.bindKeyEvents();

        this.mainview().keydown((e) => {
            this.userinput.focus();
            this.userinput.input().trigger(e);
        });
    }
}
