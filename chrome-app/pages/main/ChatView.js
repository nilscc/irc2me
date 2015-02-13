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

    var protoMsgTypes, protoUserflags;

    var loadProtoMsgTypes = function (cb) {
        Irc2me.getProtobufMesageTypes(function (tys) {
            protoMsgTypes = Array();
            for (var ty in tys) {
                protoMsgTypes[tys[ty]] = ty;
            }
            Irc2me.getProtobufUserflags(function (uf) {
                protoUserflags = uf;
                cb();
            });
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
     * UI helper
     *
     */

    var userlistTemplate;

    var loadUserlist = function (users) {

        userlistTemplate = userlistTemplate || $("#user-list-template").html();

        var template_data = {
            operators: [],
            voice: [],
            users: [],
        };

        var flags = protoUserflags;

        for (var i = 0; i < users.length; i++) {

            var user = users[i];

            if (user.flag != null) {
                switch (user.flag) {
                    case flags.OPERATOR: {
                        template_data.operators.push(user);
                        break;
                    }
                    case flags.VOICE: {
                        template_data.voice.push(user);
                        break;
                    }
                    default: {
                        console.error("Unexpected user flag", user.flag);
                    }
                }
            } else {
                template_data.users.push(user);
            }
        }

        // the compiled template
        var html = $(Mustache.to_html(userlistTemplate, template_data));

        $(".user-list-view").show();
        $("#user-list").html(html);
    };

    var emptyUserlist = function () {
        $("#user-list").empty();
    };

    var hideUserlist = function () {
        $(".user-list-view").hide();
    };

    var setCurrent = function (network_id, channel, queryuser) {
        var self = this;

        self.currentNetworkID = network_id;
        self.currentChannel   = channel;
        self.currentQuery     = queryuser;
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
        $("#topic").html(topic || "");
    };

    var focusInput = function () {
        $("#input-prompt input").focus();
    };

    /*
     * Load backlog for channel/network
     *
     */

    var messagelist;

    var appendMessages = function (messages, cb) {

        // make sure protobuf message types are loaded
        if (typeof protoMsgTypes != "object") {
            loadProtoMsgTypes(function () {
                appendMessages(messages, cb);
            });
            return; //quit
        }

        messagelist = messagelist || $("#message-list");

        var template_data = [];
        for (var i = 0; i < messages.length; i++) {
            var message = messages[i];
            var data    = getTemplateData(message);

            if (typeof cb == "function" && cb(message, data)) {
                continue;
            }

            template_data.push(data);
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

    var loadMessages = function (messages, cb) {
        messagelist = messagelist || $("#message-list");

        // replace current message list
        messagelist.empty();

        appendMessages(messages, cb);
    };

    C.loadNetwork = function (network_id) {
        var self = this;

        var netw = self.backlog.network(network_id);
        loadMessages(netw.messages);
        hideUserlist();
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

        loadMessages(chan.messages, self.messageCallback);

        loadUserlist(chan.users);
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

        loadMessages(query.messages);
        hideUserlist();
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

    var unreadMessage = function (network_id, id, class_, name, click_cb) {
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

        // add 'click' event
        entry.click(click_cb);

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
                appendMessages([msg]);
            } else {
                unreadNetworkMessage.call(self, nid);
            }
        });

        backlog.subscribe(backlog.newPublicMessageSubscriptionID, function (nid, chan, msg) {
            if (isCurrentChannel(nid, chan)) {
                appendMessages([msg], self.messageCallback);
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
