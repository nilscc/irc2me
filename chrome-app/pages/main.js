/*
 * Handlebars helper
 *
 */

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

/*
 * Main page class
 *
 */

var page;

function MainPage () {
}

MainPage.prototype.listen = function () {
    Irc2me.Signals.disconnected.addListener(function () {
        UIState.ConnectionWindow.open();
        UIState.MainWindow.close();
    });
}

/*
 * Chat view
 *
 */

MainPage.prototype.Chatview = {
    messageTemplate: Handlebars.compile( $("#usermessage-template").html() ),
};

MainPage.prototype.Chatview.load = function (channel /* optional */) {

};

MainPage.prototype.Chatview.append = function (messages) {
    var self = this;

    $("#message-list").append(
        self.messageTemplate({ messages: messages });
    );
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
