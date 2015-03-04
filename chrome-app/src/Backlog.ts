/// <reference path="libs/dcodeIO/Long" />
/// <reference path="libs/jquery" />

import Irc2me  = require("Irc2me");
import Helper  = require("common/Helper");
import Long    = require("Long");
import $       = require("jquery");

"use strict";

export var newMessageSubscriptionID          = "Backlog.newMessageSubscriptionID";
export var newPublicMessageSubscriptionID    = "Backlog.newPublicMessageSubscriptionID";
export var newPrivateMessageSubscriptionID   = "Backlog.newPrivateMessageSubscriptionID";

export class Class {

    private backlog;

    private protoMsgTypes;

    // constructor
    constructor () {
        this.backlog = {};

        // load protobuf message types
        var tys = Irc2me.ProtobufMessages.Network.Message.Type;
        this.protoMsgTypes = {};
        for (var ty in tys) {
            this.protoMsgTypes[tys[ty]] = ty;
        }
    }

    /*
     * Helper functions
     *
     */

    private initNetwork (network_id) {
        return this.backlog[network_id] = this.network(network_id);
    }

    private initChannel (network_id, channel) {
        this.initNetwork(network_id);
        return this.backlog[network_id].channels[channel] = this.channel(network_id, channel);
    }

    private initQuery (network_id, user) {
        this.initNetwork(network_id);
        return this.backlog[network_id].queries[Helper.userFullname(user)] = this.query(network_id, user);
    }

    /*
     * Protobuf helper functions
     *
     */

    /*
     * Loading messages
     *
     */

    network (network_id) {
        return this.backlog[network_id] || {
            messages: [],
            queries: {},
            channels: {},
        };
    }

    channel (network_id, channel) {
        return (this.network(network_id) && this.network(network_id).channels[channel]) || {
            messages: [],
            users: [],
            topic: "",
        };
    }

    query (network_id, user) {
        return (this.network(network_id) && this.network(network_id).queries[Helper.userFullname(user)]) || {
            messages: [],
        };
    }

    /*
     * Subscription
     *
     */

    private subs;

    subscribe (subscription_id, callback) {

        // create jquery collection
        this.subs = this.subs || $({});

        this.subs.on(subscription_id, (...args) => {
            callback.apply(callback, args.slice(1));
        });
    }

    unsubscribe (subscription_id) {

        if (!this.subs) { return; }

        this.subs.off(subscription_id);
    }

    private publish (subscription_id, ...args) {

        if (!this.subs) { return; }

        this.subs.trigger(subscription_id, args);
    }

    /*
     * Storing messages
     *
     */

    appendNetworkMessages (network_id, messages) {

        if (messages.length == 0) { return; }

        // initialize network
        this.initNetwork(network_id);

        // subscription ID
        var sub_id = newMessageSubscriptionID;

        // append messages
        for (var i = 0; i < messages.length; i++) {
            var msg = messages[i];
            this.network(network_id).messages.push(msg);
        }

        this.publish(sub_id, network_id, messages);
    }

    appendChannelMessages (network_id, channel, messages) {

        if (messages.length == 0) { return; }

        // init channel
        var chan = this.initChannel(network_id, channel);

        // subscription ID
        var sub_id = newPublicMessageSubscriptionID;

        // append messages
        for (var i = 0; i < messages.length; i++) {
            var msg  = messages[i];

            chan.messages.push(msg);

            if (msg.known && this.protoMsgTypes[msg.known] == "TOPIC") {
                chan.topic = msg.content;
            }

        }

        this.publish(sub_id, network_id, channel, messages);
    }

    appendQueryMessages (network_id, user, messages) {

        if (messages.length == 0) { return; }

        // init query
        this.initQuery(network_id, user);

        // subscription ID
        var sub_id = newPrivateMessageSubscriptionID;

        // append messages
        for (var i = 0; i < messages.length; i++) {
            var msg = messages[i];
            this.query(network_id, user).messages.push(msg);
        }

        this.publish(sub_id, network_id, user, messages);
    }

    setUserlist (network_id, channel, users) {
        this.channel(network_id, channel).users = users;
    }

    incomingMessage (msg) {

        // loop over networks
        for (var n = 0; n < msg.networks.length; n++) {
            var network    = msg.networks[n],
                network_id = Long.prototype.toNumber.call(network.id);

            this.appendNetworkMessages(network_id, network.messages);

            // handle private queries
            for (var q = 0; network.queries && q < network.queries.length; q++) {
                var query = network.queries[q];
                this.appendQueryMessages(network_id, query.user, query.messages);
            }

            // handle channel messages
            for (var c = 0; network.channels && c < network.channels.length; c++) {
                var chan = network.channels[c];

                if (chan.users.length > 0) {
                    this.setUserlist(network_id, chan.name, chan.users);
                }

                this.appendChannelMessages(network_id, chan.name, chan.messages);
            }
        }
    }

}
