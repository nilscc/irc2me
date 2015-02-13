define(function(require) {

    "use strict";

    // dependencies
    var Irc2me      = require("src/Irc2me");
    var Helper      = require("common/Helper");

    // externel dependencies
    var $           = require("jquery");
    var Mustache    = require("Mustache");
    var Long        = require("Long");
    var Autolinker  = require("Autolinker");

    // Constructor
    var ChatView = function (backlog) {
        this.backlog = backlog;
    };

    var C = ChatView.prototype;

    /*
     * General helper
     *
     */

    var autolinker = (function () {

        var al = new Autolinker({
            twitter: false,
            className: "user-link",
        });

        al.htmlCharacterEntitiesRegex =
            /(&nbsp;|&#160;|&lt;|&#60;|&gt;|&#62;|&quot;|&#039;)/gi;

        return al;

    })();

    /*
     * Protobuf helper functions
     *
     */

    var protoMsgTypes;

    var loadProtoMsgTypes = function (cb) {
        Irc2me.getProtobufMesageTypes(function (tys) {
            protoMsgTypes = Array();
            for (var ty in tys) {
                protoMsgTypes[tys[ty]] = ty;
            }
            cb();
        });
    };

    /*
     * Template helper functions
     *
     */

    var messageTemplate;

    var compileMessageTemplate = function (template_data) {

        messageTemplate = messageTemplate || $("#message-template").html();

        return $(Mustache.to_html(messageTemplate, { messages: template_data }));
    };

    var getTemplateData = function (message) {

        // make sure protobuf messages are loaded
        if (typeof protoMsgTypes != "object") {
            console.error("Protobuf message types not loaded");
            return; // quit
        }

        var epoch = Long.prototype.toNumber.call(message.timestamp);
        var date  = new Date(epoch);

        var template_data = {
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

        var set_type = function (ty) {
            template_data[ty] = true;
            template_data.classes.push("type-" + ty);
        }

        if (message.type == "known") {
            set_type(protoMsgTypes[message[message.type]].toLowerCase());
        }

        // highlight links

        if (template_data.content) {
            template_data.content = autolinker.link(
                Helper.escapeHtml(template_data.content)
            );
        }

        return template_data;
    };

    /*
     * Load backlog for channel/network
     *
     */

    var messagelist;

    var appendMessages = function (messages) {

        // make sure protobuf message types are loaded
        if (typeof protoMsgTypes != "object") {
            loadProtoMsgTypes(function () {
                appendMessages(messages);
            });
            return; //quit
        }

        messagelist = messagelist || $("#message-list");

        var template_data = [];
        for (var i = 0; i < messages.length; i++) {
            template_data.push(getTemplateData(messages[i]));
        }

        // the compiled template
        var html = compileMessageTemplate(template_data);

        // get current scroll position
        var atBottom = Helper.scrollAtBottom(messagelist);

        messagelist.append(html);

        // update scroll position
        if (atBottom) {
            Helper.scrollToBottom(messagelist);
        }
    };

    var loadMessages = function (messages) {
        messagelist = messagelist || $("#message-list");

        // replace current message list
        messagelist.empty();

        appendMessages(messages);
    };

    var loadUserlist = function (users) {
        $(".user-list-view").show();
    };

    var emptyUserlist = function () {
        $("#user-list").empty();
    };

    var hideUserlist = function () {
        $(".user-list-view").hide();
    };

    C.loadNetwork = function (network_id) {
        var self = this;

        var netw = self.backlog.network(network_id);
        loadMessages(netw.messages);
        hideUserlist();

        self.currentNetworkID = network_id;
        self.currentChannel   = null;
        self.currentQuery     = null;
    };

    C.loadChannel = function (network_id, channel) {
        var self = this;

        var chan = self.backlog.channel(network_id, channel);
        loadMessages(chan.messages);
        loadUserlist(chan.users);

        self.currentNetworkID = network_id;
        self.currentChannel   = channel;
        self.currentQuery     = null;
    };

    C.loadQuery = function (network_id, user) {
        var self = this;

        var query = self.backlog.query(network_id, user);

        // add user to messages
        for (var i = 0; i < query.messages.length; i++) {
            query.messages[i].user = query.messages[i].user || user;
        }

        loadMessages(query.messages);
        hideUserlist();

        self.currentNetworkID = network_id;
        self.currentChannel   = null;
        self.currentQuery     = user;
    };

    /*
     * Channel list
     *
     */

    var networklistTemplate;

    var unreadMessage = function (network_id, id, class_, name) {
        var network = $(".network-list .network[data-network_id=" + network_id + "]");

        // check if network exists
        if (network.length == 0) {

            // load template
            networklistTemplate = networklistTemplate || $("#network-list-template").html();

            var data = {
                network: {
                    id: network_id,
                },
            };

            // compile template to jquery object
            network = $( Mustache.to_html(networklistTemplate, data) );

            $(".network-list").append(network);
        }

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

            // add to list
            network.append(entry);
        }

        // set as 'unread'
        entry.addClass("unread");

        return entry;
    };

    var unreadNetworkMessage = function (network_id) {
        var self = this;

        // TODO: get network name
        var entry = unreadMessage(network_id, network_id, "network", "<network>");

        entry.click(function () {
            self.loadNetwork(network_id);
        });
    };

    var unreadPublicMessage = function (network_id, channel) {
        var self = this;

        var entry = unreadMessage(network_id, channel, "channel", channel);

        entry.click(function () {
            self.loadChannel(network_id, channel);
        });
    };

    var unreadPrivateMessage = function (network_id, user) {
        var self = this;

        var entry = unreadMessage(network_id, Helper.userFullname(user), "query", user.nick);

        entry.click(function () {
            self.loadQuery(network_id, user);
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
                && Helper.userFullname(self.currentQuery) === Helper.userFullname(user);
        }

        backlog.subscribe(backlog.newMessageSubscriptionID, function (nid, msg) {
            if (isCurrentNetwork(nid) && !self.currentChannel) {
                appendMessages([msg]);
            } else {
                unreadNetworkMessage.call(self, nid);
            }
        });

        backlog.subscribe(backlog.newPublicMessageSubscriptionID, function (nid, chan, msg) {
            if (isCurrentChannel(nid, chan)) {
                appendMessages([msg]);
            } else {
                unreadPublicMessage.call(self, nid, chan);
            }
        });

        backlog.subscribe(backlog.newPrivateMessageSubscriptionID, function (nid, user, msg) {
            if (isCurrentQuery(nid, user)) {
                msg.user = msg.user || user;
                appendMessages([msg]);
            } else {
                unreadPrivateMessage.call(self, nid, user);
            }
        });

    };

    /*
     * Sending messages
     *
     */

    var send = function (text, cb) {
        var self = this;

        console.log("send:", text);

        // require network
        if (! self.currentNetworkID) { return; }

        var network_id = self.currentNetworkID;
        var recipient  = self.currentQuery || [self.currentChannel];

        if (!text) { return; }

        var cmd, pars;

        if (text[0] == "/") {
            pars = text.slice(1).split(/\s/);
            cmd  = pars.shift().toUpperCase();
        }

        console.log(cmd, pars);

        /*
         * Handle user command
         *
         */

        var types = protoMsgTypes;

        if (cmd) {

            // make sure cmd is valid
            if (types.indexOf(cmd) > -1) {

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
                    command:    types.indexOf(cmd),
                    parameters: pars,
                    content:    content,
                }, cb);
            }
        }

        /*
         * regular private message
         *
         */
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

    C.bindKeyEvents = function (jquery_context) {
        var self = this;

        $("input", jquery_context).keypress(function (e) {
            var input = $(this);

            if (e.which == 13) { // enter key
                send.call(self, input.val(), function () {
                    input.val("");
                });
            }
        });
    };

    /*
     * End of module
     *
     */
    return ChatView;
});
