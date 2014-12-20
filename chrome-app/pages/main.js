/*
 * Handlebars helper
 *
 */

/*
Handlebars.registerHelper('formatTimestamp', function (timestamp) {
    return (new Date(timestamp)).toLocaleTimeString();
});

Handlebars.registerHelper('userflagClass', function(userflag) {
    // the 3 cases (see messages.proto)
    switch (userflag) {
        case 0:     { return "userflag-operator"; }
        case 1:     { return "userflag-voice"; }
        default:    { return ""; }
    }
});
*/

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
            var network = msg.networks[n];
            self.Chatview.append(network.messages);
        }
    });
}

/*
 * Chat view
 *
 */

MainPage.prototype.Chatview = {
};

MainPage.prototype.Chatview.load = function (channel /* optional */) {
    var self = this;
};

MainPage.prototype.Chatview.append = function (messages) {
    var self = this;

    //console.log(Must

    var src = $("#message-template").html();

    for (var i = 0; i < messages.length; i++) {
        var msg = messages[i];

        var epoch = dcodeIO.Long.prototype.toNumber.call(msg.timestamp);
        var date  = new Date(epoch);

        var compiled_template = Mustache.to_html(src, {
            timestamp: date.toLocaleTimeString(),
            userflag: "",
            nickname: msg.user.nick,
            username: msg.user.name,
            host: msg.user.host,
            content: msg.content,
        });

        $("#message-list").append(compiled_template);
    }
}

/*
 * Load UI
 *
 */

$(document).ready(function () {

    page = new MainPage();

    page.Chatview.load();

    page.listen();
});
