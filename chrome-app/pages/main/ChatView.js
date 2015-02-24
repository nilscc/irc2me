define(function(require) {

    "use strict";

    // sub modules
    var UserList         = require("main/ChatView/UserList");
    var MessageView      = require("main/ChatView/MessageView");
    var UserInput        = require("main/ChatView/UserInput");
    var ConversationList = require("main/ChatView/ConversationList");

    // dependencies
    var Irc2me           = require("src/Irc2me");
    var Helper           = require("common/Helper");

    // externel dependencies
    var $                = require("jquery");
    var Long             = require("Long");
    var linkify          = require("linkify-string");

    /*
     * Constructor
     *
     */

    var ChatView = function (backlog, jquery_context) {
        this.backlog          = backlog;
        this.userlist         = new UserList(jquery_context);
        this.messageview      = new MessageView(jquery_context);
        this.userinput        = new UserInput(jquery_context);
        this.conversationlist = new ConversationList(jquery_context);
    };

    var C = ChatView.prototype;

    /*
     * UI helper
     *
     */

    var setCurrent = function (network_id, channel, queryuser) {
        var self = this;

        self.currentNetworkID = network_id;
        self.currentChannel   = channel;
        self.currentQuery     = queryuser;

        self.userinput.setSendTo(network_id, [channel || queryuser.nick]);
    };

    var setTopic = function (channel_name, topic) {
        $("#channel-name").html(channel_name);
        $("#topic").html(Helper.escapeHtml(topic || "")).html(function (_, h) {
            return linkify(h);
        });
    };

    /*
     * Load backlog for channel/network
     *
     */

    var messagelist;

    C.loadNetwork = function (network_id) {
        var self = this;

        var netw = self.backlog.network(network_id);

        self.messageCallback = null

        self.messageview.load(netw.messages);
        self.userlist.hide();
        self.conversationlist.setActiveNetwork(network_id);

        self.userinput.focus();

        setCurrent.call(self, network_id, null, null);
    };

    C.loadChannel = function (network_id, channel) {
        var self = this;

        var chan = self.backlog.channel(network_id, channel);

        self.messageCallback = function (message, data) {
            if (data.topic && data.server) {
                setTopic(channel, data.content);
                return true;
            }
        };

        self.messageview.load(chan.messages, self.messageCallback);
        self.userlist.load(chan.users);
        self.conversationlist.setActiveChannel(network_id, channel);

        setTopic(channel, chan.topic);
        self.userinput.focus();

        setCurrent.call(self, network_id, channel, null);
    };

    C.loadQuery = function (network_id, user) {
        var self = this;

        var query = self.backlog.query(network_id, user);

        // add user to messages
        for (var i = 0; i < query.messages.length; i++) {
            query.messages[i].user = query.messages[i].user || user;
        }

        self.messageCallback = null

        self.messageview.load(query.messages);
        self.userlist.hide();
        self.conversationlist.setActiveQuery(network_id, user);

        setTopic(Helper.userFullname(user));
        self.userinput.focus();

        setCurrent.call(self, network_id, null, user);
    };

    /*
     * Channel list
     *
     */

    var unreadNetworkMessage = function (network_id) {
        var self = this;
        self.conversationlist.setUnreadNetworkMessage(network_id, function () {
            self.loadNetwork(network_id);
        });
    };

    var unreadPublicMessage = function (network_id, channel) {
        var self = this;
        self.conversationlist.setUnreadPublicMessage(network_id, channel, function () {
            self.loadChannel(network_id, channel);
        });
    };

    var unreadPrivateMessage = function (network_id, user) {
        var self = this;

        unreadMessage(network_id, Helper.userFullname(user), "query", user.nick, function () {
            self.loadQuery(network_id, user);
        });
    };

    C.loadConversations = function () {
        var self = this;

        Irc2me.getConversationList(function (networks) {

            var first = true;

            for (var i = 0; i < networks.length; i++) {

                var network     = networks[i],
                    network_id  = Long.prototype.toNumber.call(network.id);

                self.conversationlist.addNetwork(network_id, network.name);

                // add channels
                for (var j = 0; j < network.channels.length; j++) {
                    var channel = network.channels[j];

                    // add channel to conversation list
                    self.conversationlist.addChannel(network_id, channel,
                        (function (network_id, channel_name) {
                            this.loadChannel(network_id, channel_name);
                        }).bind(self, network_id, channel.name)
                    );

                    // load the first channel
                    if (first) {
                        self.loadChannel(network_id, channel.name);
                        first = false;
                    }
                }

                // add queries
                for (var j = 0; j < network.queries.length; j++) {
                    var query = network.queries[j];

                    // add query to conversation list
                    self.conversationlist.addQuery(network_id, query.user,
                        (function (network_id, user) {
                            this.loadQuery(network_id, user);
                        }).bind(self, network_id, query.user)
                    );
                }
            }
        });
    };

    /*
     * Load new messages
     *
     */

    C.listenForNewMessages = function () {
        var self = this;

        var backlog = self.backlog;

        var isCurrentNetwork = function (network_id) {
            return self.currentNetworkID === network_id;
        }
        var isCurrentChannel = function (network_id, channel) {
            return isCurrentNetwork(network_id)
                && self.currentChannel === channel;
        }
        var isCurrentQuery = function (network_id, user) {
            return isCurrentNetwork(network_id)
                && typeof self.currentQuery == "object"
                && self.currentQuery
                && Helper.userFullname(self.currentQuery) === Helper.userFullname(user);
        }

        backlog.subscribe(backlog.newMessageSubscriptionID, function (nid, msg) {
            if (isCurrentNetwork(nid) && !self.currentChannel) {
                self.messageview.append([msg]);
            } else {
                unreadNetworkMessage.call(self, nid);
            }
        });

        backlog.subscribe(backlog.newPublicMessageSubscriptionID, function (nid, chan, msg) {
            if (isCurrentChannel(nid, chan)) {
                self.messageview.append([msg], self.messageCallback);
            } else {
                unreadPublicMessage.call(self, nid, chan);
            }
        });

        backlog.subscribe(backlog.newPrivateMessageSubscriptionID, function (nid, user, msg) {
            if (isCurrentQuery(nid, user)) {
                msg.user = msg.user || user;
                self.messageview.append([msg]);
            } else {
                unreadPrivateMessage.call(self, nid, user);
            }
        });
    };

    /*
     * Sending messages
     *
     */

    /*
    */

    C.bindKeyEvents = function (jquery_context) {
        this.userinput.bindKeyEvents();
    };

    /*
     * End of module
     *
     */
    return ChatView;
});
