define(function (require) {

    var Irc2me      = require("src/Irc2me");
    var Helper      = require("common/Helper");

    var Mustache    = require("Mustache");
    var $           = require("jquery");
    var Long        = require("Long");

    var linkify     = require("linkify-string");

    var templates = {
        Message: require("text!main/templates/Message.html"),
    };

    /*
     * Constructor
     *
     */

    var MessageView = function (jquery_context) {
        this.jquery_context = jquery_context;
    };

    var proto = MessageView.prototype;

    /*
     * JQuery helper
     *
     */

    proto.messagelist = function (opt_selector) {
        var self = this;

        if (! self._messagelist) {
            self._messagelist = $("#message-list", self.jquery_context);
        }

        if (opt_selector != null) {
            return $(opt_selector, self._messagelist);
        }
        else {
            return self._messagelist;
        }
    };

    /*
     * Protobuf helper functions
     *
     */

    var protoMsgTypes = (function () {
        var res = Array();
        var tys = Irc2me.ProtobufMessages.Network.Message.Type;
        for (var ty in tys) {
            res[tys[ty]] = ty;
        }
        return res;
    })();

    /*
     * Mustache template helper functions
     *
     */

    var compileMessageTemplate = function (template_data) {
        return $(Mustache.to_html(templates.Message, { messages: template_data }));
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

        if (template_data.content) {
            template_data.content = Helper.escapeHtml(template_data.content);
        }

        return template_data;
    };

    proto.renderMessages = function (messages) {
        var self = this;

        var template_data = [];
        for (var i = 0; i < messages.length; i++) {
            var message = messages[i];
            var data    = getTemplateData(message);

            if (typeof cb == "function" && cb(message, data) === true) {
                continue;
            }

            if (! self.lastMessage) { // TODO
            }

            template_data.push(data);
        }

        // the compiled template
        var msg = compileMessageTemplate(template_data);

        // make links clickable
        $(".text", msg).html(function (_, t) {
            return linkify(t, {
                linkAttributes: {
                    tabindex: -1,
                },
            });
        });

        return msg;
    };

    /*
     * Keep track of message timestamps
     *
     */

    var isOlder = function (message) {
        return (this.oldestMessageTimestamp != null && message.timestamp != null)
            && Helper.messageTimestamp(message) < this.oldestMessageTimestamp
    };

    var isNewer = function (message) {
        return (this.newestMessageTimestamp != null && message.timestamp != null)
            && this.newestMessageTimestamp < Helper.messageTimestamp(message);
    };

    // set oldest/newest message timestamp
    var updateTimestamps = function (first, last) {
        var self = this;

        var f = Helper.messageTimestamp(first),
            l = Helper.messageTimestamp(last);

        if (! self.oldestMessageTimestamp || f < self.oldestMessageTimestamp) {
            self.oldestMessageTimestamp = f;
        }
        if (! self.newestMessageTimestamp || self.newestMessageTimestamp < l) {
            self.newestMessageTimestamp = l;
        }
    };

    var resetTimestamps = function () {
        self.oldestMessageTimestamp = null;
        self.newestMessageTimestamp = null;
    };

    /*
     * Load messages
     *
     */

    proto.load = function (messages, callback) {
        var self = this;

        // reset all
        resetTimestamps.call(self);
        self.messagelist().empty();

        self.append(messages, callback);
    };


    proto.append = function (messages, callback) {
        var self = this;

        if (messages.length == 0) { return; }

        console.log("append:", messages);

        // first and last message
        var first = messages[0],
            last  = messages[messages.length - 1];

        // render messages
        var rendered = self.renderMessages(messages);

        // jquery ui element
        var messagelist = self.messagelist();

        if (isOlder.call(self, last)) {
            messagelist.prepend(rendered);
            // TODO: update scroll position
        }
        else {
            if (self.newestMessageTimestamp && !isNewer.call(self, first)) {
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

        updateTimestamps.call(self, first, last);
    };

    /*
     * End of module
     *
     */

    return MessageView;
});
