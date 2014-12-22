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

    self.append(page.Backlog.get(network_id, channel_name));
};

MainPage.prototype.Chatview.append = function (network_id, channel_name, messages) {
    var self = this;

    // optional network_id/channel_name -> use current values otherwise
    if (typeof network_id == "object" && !(channel_name || messages)) {
        messages = network_id;
        channel_name = self.currentChannel;
        network_id = self.currentNetwork;
    }
    // ignore invalid network/channel
    else if (network_id != self.currentNetwork || channel_name != self.currentChannel) {
        return;
    }

    var src = $("#message-template").html();

    for (var i = 0; i < messages.length; i++) {
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

        var compiled_template = Mustache.to_html(src, template_data);

        $("#message-list").append(compiled_template);
    }
};

MainPage.prototype.Chatview.send = function (text, cb) {

    var self = this;

    if (! (self.currentNetwork && self.currentChannel)) {
        return;
    }

    // aliases
    Irc2me.sendPrivateMessage({
        network_id: self.currentNetwork,
        to:         self.currentChannel,
        text:       text,
    }, cb);
}

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
}

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
