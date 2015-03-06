var main;

require(["../require-common"], function (common) {
    require(["pages/main"], function (m) {
        main = m;
    });
});

/*
 * UI: Chat view
 *
 */

/*

MainPage.prototype.Chatview = {
};

MainPage.prototype.Chatview.load = function (network_id, channel_name, cb) {
    var self = this;

    if (!self._protoMsgTypes) {
        Irc2me.getProtobufMesageTypes(function (tys) {
            self._protoMsgTypes = Array();
            for (var ty in tys) {
                self._protoMsgTypes[tys[ty]] = ty;
            }
            self.load(network_id, channel_name, cb);
        });
        return; // quit
    }

    if (!self._protoUserflags) {
        Irc2me.getProtobufUserflags(function (uf) {
            self._protoUserflags = uf;
            self.load(network_id, channel_name, cb);
        });
        return; // quit
    }

    self.currentNetwork = network_id;
    self.currentChannel = channel_name;

    // clear current messages
    $("#message-list").empty();

    // remove 'active' flag from other channels
    var active = ".network-list .network "
               + ".channel.active:not([data-channel=\"" + channel_name + "\"])";

    $(active).removeClass("active");

    // unset 'unread' flag & set 'active'
    var current = ".network-list "
                + ".network[data-network_id=\"" + network_id + "\"] "
                + ".channel[data-channel=\"" + channel_name + "\"]"

    $(current).removeClass("unread").addClass("active");

    var log = page.Backlog.get(network_id, channel_name);

    // append messages
    self.append(log.backlog);

    // load userlist
    self.setUserlist(log.userlist);

    $("#input-prompt input").focus();

    // run callback (if any)
    if (typeof cb == "function") { cb(); }
};

MainPage.prototype.Chatview.append = function (network_id, channel_name, messages) {
    var self = this;

    // optional network_id/channel_name -> use current values otherwise
    if (typeof network_id == "object" && !(channel_name || messages)) {
        messages = network_id;
        channel_name = self.currentChannel;
        network_id = self.currentNetwork;
    }

    // optional channel name
    if (typeof channel_name == "object") {
        messages = channel_name;
        channel_name = null;
    }

    // alias
    var messageList = $("#message-list");

    // ignore invalid network/channel
    var is_current = network_id   == self.currentNetwork
                  && channel_name == self.currentChannel;

    // build template data
    var template_messages = [];

    for (var i = 0; i < messages.length; i++) {

        var msg = messages[i];

        if (is_current) {
            template_messages.push(
                self._getTemplateData(messages[i])
            );
        } else {
            // not current channel
            var name = channel_name || msg.server || (msg.user && msg.user.nick);
            self.setUnreadMessage(network_id, name);
        }
    }

    // get current scroll position
    var atBottom = Helper.scrollAtBottom(messageList);

    // compile template
    var src               = $("#message-template").html(),
        compiled_template = $( Mustache.to_html(src, { messages: template_messages }) );

    self._inlineImages(compiled_template);

    messageList.append(compiled_template);

    if (atBottom) {
        Helper.scrollToBottom(messageList);
    }
};

MainPage.prototype.Chatview.setUserlist = function (network_id, channel_name, userlist) {
    var self = this;

    if (typeof network_id == "object") {
        userlist = network_id;
        network_id = self.currentNetwork;
        channel_name = self.currentChannel;
    }

    if (network_id != self.currentNetwork && channel_name != self.currentChannel) {
        return;
    }

    // gather data
    var template_data = {
        operators: [],
        voice: [],
        users: [],
    };

    var flags = self._protoUserflags;

    for (var i = 0; i < userlist.length; i++) {

        var user = userlist[i];

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

    // compile template
    var src               = $("#user-list-template").html(),
        compiled_template = $( Mustache.to_html(src, template_data) );

    $("#user-list").html(compiled_template);
}

MainPage.prototype.Chatview._getAutolinker = function () {

    if (!self._Autolinker) {

        self._Autolinker = new Autolinker({
            twitter: false,
            className: "user-link",
        });

        // FIXME: workaround for https://github.com/gregjacobs/Autolinker.js/issues/76
        self._Autolinker.htmlCharacterEntitiesRegex =
            /(&nbsp;|&#160;|&lt;|&#60;|&gt;|&#62;|&quot;|&#039;)/gi;
    }

    return self._Autolinker;
}

MainPage.prototype.Chatview._inlineImages = function (html, cb) {

    // turn into jquery object if not already
    if (typeof html == "string") {
        html = $(html);
    }

    // regular images
    html.find("a:regex(href, \\.(jpg|jpeg|gif|png)$)").click(function () {

        event.preventDefault();

        var a   = $(this),
            url = a.attr("href");

        if (a.hasClass("open")) {

            $("img", a).remove();
            a.removeClass("open");

        } else {

            var error_cb = function () {
                console.log.apply(this, arguments);
            }

            var success_cb = function (data) {
                if (a.hasClass("open")) { // not closed already again
                    a.append("<img class=\"inline-image\" src=\"" + window.URL.createObjectURL(data) + "\">");
                }
            };

            $.ajax(url, {
                dataType: "blob",
                error: error_cb,
                success: success_cb,
            });

            a.addClass("open");
        }
    });
}

MainPage.prototype.Chatview._getTemplateData = function (msg) {

    var self = this;

    var epoch = dcodeIO.Long.prototype.toNumber.call(msg.timestamp);
    var date  = new Date(epoch);

    var template_data = {
        timestamp: date.toLocaleTimeString(),
        content: msg.content,
        classes: [],
    };

    // figure out who wrote the message

    if (msg.from == "user") {
        template_data.user = msg.user;
        template_data.user.flag = "";
    } else {
        template_data.server = msg.server;
    }

    // figure out message type

    var types = self._protoMsgTypes;

    var set_type = function (ty) {
        template_data[ty] = true;
        template_data.classes.push("type-" + ty);
    }

    if (msg.type == "known") {
        set_type(types[msg[msg.type]].toLowerCase());
    }

    // highlight links

    if (template_data.content) {

        // alias
        var autolinker = self._getAutolinker();

        template_data.content = autolinker.link(Helper.escapeHtml(template_data.content));
    }

    return template_data;
}

MainPage.prototype.Chatview.setUnreadMessage = function (network_id, channel_name) {

    var self = this;

    var network = $(".network-list .network[data-network_id=" + network_id + "]");

    // check if network exists
    if (network.length == 0) {

        // load template
        var src = $("#network-list-template").html();

        var data = {
            network: {
                id: network_id,
            },
        };

        // compile template to jquery object
        network = $( Mustache.to_html(src, data) );

        $(".network-list").append(network);
    }

    var channel = $("> .channel[data-channel=\"" + channel_name + "\"]", network);

    // check if channel exists
    if (channel.length == 0) {

        // load template
        var src = $("#network-channel-list-item-template").html();

        var data = {
            channel: {
                name: channel_name,
            }
        };

        // compile template to jquery object
        channel = $( Mustache.to_html(src, data) );

        // load channel view on click
        channel.click(function () {
            self.load(network_id, channel_name);
        });

        // add to list
        network.append(channel);
    }

    // set as 'unread'
    channel.addClass("unread");
};

MainPage.prototype.Chatview.send = function (text, cb) {

    var self = this;

    // require network
    if (! self.currentNetwork) { return; }

    if (!text) { return; }

    var cmd, pars;

    if (text[0] == "/") {
        pars = text.slice(1).split(/\s/);
        cmd  = pars.shift().toUpperCase();
    }

    / *
     * Handle user command
     *
     * /

    var types = self._protoMsgTypes;

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
                network_id: self.currentNetwork,
                command:    types.indexOf(cmd),
                parameters: pars,
                content:    content,
            }, cb);

        }

    } else {

        / *
         * regular private message
         *
         * /

        // require channel
        if (! self.currentChannel) { return; }

        if (cmd == "ME") {
            var SOH = String.fromCharCode(1);
            text = SOH + "ACTION " + pars.join(" ") + SOH;
        }

        // send private message
        Irc2me.sendPrivateMessage({
            network_id: self.currentNetwork,
            to:         self.currentChannel,
            text:       text,
        }, cb);

    }
};

MainPage.prototype.Chatview.bindKeyEvents = function () {

    var self = this;

    $("#input-prompt input").keypress(function (e) {

        var input = $(this);

        if (e.which == 13) { // enter key
            self.send(input.val(), function () {
                input.val("");
            });
        }
    });
};
*/

/*
 * Load UI
 *
 */

    /*
$(document).ready(function () {

    page = new MainPage();

    page.Chatview.load();

    page.Chatview.bindKeyEvents();

    page.listen();

});
    */
