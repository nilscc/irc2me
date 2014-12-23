/*
 * Main page class
 *
 */

var page;

function MainPage () {
}

MainPage.prototype.listen = function () {
    var self = this;

    Irc2me.Signals.disconnected.addListener(function () {
        UIState.ConnectionWindow.open();
        UIState.MainWindow.close();
    });

    Irc2me.Signals.incomingMessage.addListener(function (msg) {
        // loop over networks
        for (var n = 0; n < msg.networks.length; n++) {
            var network    = msg.networks[n],
                network_id = dcodeIO.Long.prototype.toNumber.call(network.id);

            if (network.messages && network.messages.length > 0) {
                self.Backlog.append(network_id, network.messages);
                self.Chatview.append(network_id, network.messages);
            }

            for (var c = 0; network.channels && c < network.channels.length; c++) {
                var chan = network.channels[c];
                self.Backlog.append(network_id, chan.name, chan.messages);
                self.Chatview.append(network_id, chan.name, chan.messages);
            }
        }
    });
}

/*
 * Backlog
 *
 */

MainPage.prototype.Backlog = {
    _backlog: {},
};

MainPage.prototype.Backlog.append = function (network_id, channel_name, messages) {

    var self = this;

    // optional 'channel_name' argument
    if (typeof channel_name == "object") {
        messages = channel_name;
        channel_name = null;
    }

    // init and alias network backlog
    var backlog = self._backlog[network_id] = self._backlog[network_id] || {};

    for (var i = 0; i < messages.length; i++) {

        var msg  = messages[i],
            name = channel_name || msg.sever || (msg.user && msg.user.nick);

        if (name) {
            // init 'name' backlog
            backlog[name] = backlog[name] || [];
            // append message
            backlog[name].push(msg);
        } else {
            console.log(msg);
            console.error("Invalid Backlog.append message");
        }
    }
}

MainPage.prototype.Backlog.get = function (network_id, name) {

    var self = this;

    var backlog = self._backlog[network_id] || {};

    return backlog[name] || [];
}


/*
 * UI: Chat view
 *
 */

MainPage.prototype.Chatview = {

};

MainPage.prototype.Chatview.load = function (network_id, channel_name) {
    var self = this;

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

    self.append(page.Backlog.get(network_id, channel_name));

    $("#input-prompt input").focus();
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

        if (is_current) {

            var msg = messages[i];
            var epoch = dcodeIO.Long.prototype.toNumber.call(msg.timestamp);
            var date  = new Date(epoch);

            var template_data = {
                timestamp: date.toLocaleTimeString(),
                content: msg.content,
            };

            if (msg.from == "user") {
                template_data.user = msg.user;
                template_data.user.flag = "";
            } else {
                template_data.server = msg.server;
            }

            template_messages.push(template_data);

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

    $("#message-list").append(compiled_template);

    if (atBottom) {
        Helper.scrollToBottom(messageList);
    }
};

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

    var channel = $("> .channel[data-channel=" + channel_name + "]", network);

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

    if (!text) { return; }

    var cmd, pars;

    if (text[0] == "/") {
        pars = text.slice(1).split(/\s/);
        cmd  = pars.shift().toUpperCase();
    }

    // require network
    if (! self.currentNetwork) { return; }

    /*
     * Handle user command
     *
     */

    var validCommands = [
        "JOIN",
        "PART",
        "INVITE",
        "QUIT",
        "KICK",
        "NICK",
        "NOTICE",
    ];

    if (cmd && validCommands.indexOf(cmd) != -1) {

        var content = "";

        switch (cmd) {

            case "PART": {
                if (! self.currentChannel) { return;}
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
            command:    cmd,
            parameters: pars,
            content:    content,
        }, cb);

    } else {

        /*
         * regular private message
         *
         */

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

/*
 * Load UI
 *
 */

$(document).ready(function () {

    page = new MainPage();

    page.Chatview.load();

    page.Chatview.bindKeyEvents();

    page.listen();
});
