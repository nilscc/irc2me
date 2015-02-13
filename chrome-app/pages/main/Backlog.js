define(function (require) {

    "use strict";

    var Helper  = require("common/Helper");
    var Long    = require("Long");
    var $       = require("jquery");

    // constructor
    var Backlog = function () {
        this.backlog = {};
    };

    var B = Backlog.prototype;


    /*
     * Helper functions
     *
     */

    var initNetwork = function(network_id) {
        var self = this;
        self.backlog[network_id] = self.network(network_id);
    };

    var initChannel = function(network_id, channel) {
        var self = this;
        initNetwork.call(self, network_id);
        self.backlog[network_id].channels[channel] = self.channel(network_id, channel);
    };

    var initQuery = function (network_id, user) {
        var self = this;
        initNetwork.call(self, network_id);
        self.backlog[network_id].queries[Helper.userFullname(user)] = self.query(network_id, user);
    };

    /*
     * Loading messages
     *
     */

    B.network = function(network_id) {
        return this.backlog[network_id] || {
            messages: [],
            queries: {},
            channels: {},
        };
    };

    B.channel = function(network_id, channel) {
        return this.backlog[network_id].channels[channel] || {
            messages: [],
            users: [],
        };
    };

    B.query = function (network_id, user) {
        return this.backlog[network_id].queries[Helper.userFullname(user)] || {
            messages: [],
        };
    };

    /*
     * Subscription
     *
     */

    B.subscribe = function (subscription_id, callback) {
        var self = this;

        self.subs = self.subs || $({});
        self.subs.on(subscription_id, function () {
            var args = Array.prototype.slice.call(arguments, 1);
            callback.apply(callback, args);
        });
    };

    B.unsubscribe = function (subscription_id) {
        var self = this;

        if (!self.subs) { return; }
        self.subs.off(subscription_id);
    };

    var publish = function (subscription_id, var_args) {
        var self = this;

        if (!self.subs) { return; }
        var args = Array.prototype.slice.call(arguments, 1);
        self.subs.trigger(subscription_id, args);
    };

    B.newMessageSubscriptionID          = "Backlog.newMessageSubscriptionID";
    B.newPublicMessageSubscriptionID    = "Backlog.newPublicMessageSubscriptionID";
    B.newPrivateMessageSubscriptionID   = "Backlog.newPrivateMessageSubscriptionID";

    /*
     * Storing messages
     *
     */

    B.appendNetworkMessages = function(network_id, messages) {

        var self = this;

        // initialize network
        initNetwork.call(self, network_id);

        // subscription ID
        var sub_id = self.newMessageSubscriptionID;

        // append messages
        for (var i = 0; i < messages.length; i++) {
            var msg = messages[i];
            self.network(network_id).messages.push(msg);
            publish.call(self, sub_id, network_id, msg);
        }
    };

    B.appendChannelMessages = function(network_id, channel, messages) {

        var self = this;

        // init channel
        initChannel.call(self, network_id, channel);

        // subscription ID
        var sub_id = self.newPublicMessageSubscriptionID;

        // append messages
        for (var i = 0; i < messages.length; i++) {
            var msg = messages[i];
            self.channel(network_id, channel).messages.push(msg);
            publish.call(self, sub_id, network_id, channel, msg);
        }
    };

    B.appendQueryMessages = function (network_id, user, messages) {
        var self = this;

        // init query
        initQuery.call(self, network_id, user);

        // subscription ID
        var sub_id = self.newPrivateMessageSubscriptionID;

        // append messages
        for (var i = 0; i < messages.length; i++) {
            var msg = messages[i];
            self.query(network_id, user).messages.push(msg);
            publish.call(self, sub_id, network_id, user, msg);
        }
    };

    B.setUserlist = function (network_id, channel, users) {
        var self = this;
        self.channel(network_id, channel).users = users;
    };

    B.incomingMessage = function (msg) {

        var self = this;

        // loop over networks
        for (var n = 0; n < msg.networks.length; n++) {
            var network    = msg.networks[n],
                network_id = Long.prototype.toNumber.call(network.id);

            self.appendNetworkMessages(network_id, network.messages);

            // handle private queries
            for (var q = 0; network.queries && q < network.queries.length; q++) {
                var query = network.queries[q];
                self.appendQueryMessages(network_id, query.user, query.messages);
            }

            // handle channel messages
            for (var c = 0; network.channels && c < network.channels.length; c++) {
                var chan = network.channels[c];

                self.appendChannelMessages(network_id, chan.name, chan.messages);

                if (chan.users.length > 0) {
                    self.setUserlist(network_id, chan.name, chan.users);
                }
            }
        }
    };

    return Backlog;

});
