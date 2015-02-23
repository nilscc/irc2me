define(function(require) {

    "use strict";

    // dependencies
    var Irc2me      = require("src/Irc2me");
    var Helper      = require("common/Helper");

    var UserList    = require("main/UserList");
    var MessageView = require("main/MessageView");
    var UserInput   = require("main/UserInput");

    // externel dependencies
    var $           = require("jquery");
    var Mustache    = require("Mustache");
    var Long        = require("Long");
    var linkify     = require("linkify-string");

    /*
     * Constructor
     *
     */

    var ChatView = function (backlog, jquery_context) {
        this.backlog     = backlog;
        this.userlist    = new UserList(jquery_context);
        this.messageview = new MessageView(jquery_context);
        this.userinput   = new UserInput(jquery_context);
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

    var setActiveEntry = function (class_, id, opt_ctxt) {
        $(".network-list .entry.active:not([data-id=\"" + id + "\"])", opt_ctxt)
            .removeClass("active");

        $(".network-list .entry." + class_ + "[data-id=\"" + id + "\"]", opt_ctxt)
            .removeClass("unread")
            .addClass("active");
    };

    var setTopic = function (channel_name, topic) {
        $("#channel-name").html(channel_name);
        $("#topic").html(Helper.escapeHtml(topic || "")).html(function (_, h) {
            return linkify(h);
        });
    };

    var focusInput = function () {
        $("#input-prompt input").focus();
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

        setActiveEntry("network", network_id);
        focusInput();

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

        setTopic(channel, chan.topic);
        setActiveEntry("channel", channel);
        focusInput();

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
        setTopic(Helper.userFullname(user));
        setActiveEntry("query", Helper.userFullname(user));
        focusInput();

        setCurrent.call(self, network_id, null, user);
    };

    /*
     * Channel list
     *
     */

    var networklistTemplate;

    // get (or create) network HTML element
    var getNetworkElement = function (network_id, network_name) {
        var network = $(".network-list .network[data-network_id=" + network_id + "]");

        // check if network exists
        if (network.length == 0) {

            // load template
            networklistTemplate = networklistTemplate || $("#network-list-template").html();

            var data = {
                network: {
                    id: network_id,
                    name: network_name,
                },
            };

            // compile template to jquery object
            network = $( Mustache.to_html(networklistTemplate, data) );

            $(".network-list").append(network);
        }

        return network;
    };

    // get (or create) network entry HTML element
    var getNetworkEntryElement = function (network_id, class_, id, name, click_cb) {
        var network = getNetworkElement(network_id);

        var entry = $("> .entry." + class_ + "[data-id=\"" + id + "\"]", network);

        // check if entry exists
        if (entry.length == 0) {

            // load template
            var src = $("#network-list-item-template").html();

            var data = {
                id: id,
                name: name,
                "class": class_,
            };

            // compile template to jquery object
            entry = $( Mustache.to_html(src, data) );

            // add click event
            entry.click(click_cb);

            // add to list
            network.append(entry);
        }

        return entry;
    };

    var unreadMessage = function (network_id, id, class_, name, click_cb) {

        var entry = getNetworkEntryElement(network_id, class_, id, name, click_cb);

        // set as 'unread'
        entry.addClass("unread");

        return entry;
    };

    var unreadNetworkMessage = function (network_id) {
        var self = this;

        // TODO: get network name
        unreadMessage(network_id, network_id, "network", "<network>", function () {
            self.loadNetwork(network_id);
        });
    };

    var unreadPublicMessage = function (network_id, channel) {
        var self = this;

        unreadMessage(network_id, channel, "channel", channel, function () {
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

            var entry;
            var first = true;

            for (var i = 0; i < networks.length; i++) {

                var network     = networks[i],
                    network_id  = Long.prototype.toNumber.call(network.id);

                getNetworkElement(network_id, network.name);

                // add channels
                for (var j = 0; j < network.channels.length; j++) {
                    var channel = network.channels[j];
                    getNetworkEntryElement(network_id, "channel", channel.name, channel.name,
                        (function (network_id, channel) {
                            this.loadChannel(network_id, channel);
                        }).bind(self, network_id, channel.name));

                    // load the first channel (if any)
                    if (first) {
                        self.loadChannel(network_id, channel.name);
                        first = false;
                    }
                }

                // add queries
                for (var j = 0; j < network.queries.length; j++) {
                    var query = network.queries[j];
                    getNetworkEntryElement(network_id, "query", query.user.nick, query.user.nick,
                        (function (network_id, user) {
                            this.loadQuery(network_id, user);
                        }).bind(self, network_id, query.user));
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
