/*
 * Page class
 *
 */

var page;

function PageConnect() { }

/*
 * Actions
 *
 */

PageConnect.prototype.connect = function() {
    // input data
    var host = Helper.inputByName("hostname"),
        port = Helper.inputByName("port"),
        user = Helper.inputByName("username"),
        pass = Helper.inputByName("password");

    Irc2me.connect({
        hostname: host,
        port: port,
        username: user,
        password: pass,
    }, function () {
        console.log("Authenticated!");
    });
}

PageConnect.prototype.disconnect = function () {
    Irc2me.disconnect();
}

PageConnect.prototype.close = function() {
    chrome.app.window.current().close();
}

PageConnect.prototype.setConnected = function() {
    var self = this;

    $("#connect")
        .text("Disconnect")
        .unbind("click")
        .click(function () { self.disconnect(); });
}

PageConnect.prototype.setDisconnected = function() {
    var self = this;

    $("#connect")
        .text("Connect")
        .unbind("click")
        .click(function () { self.connect(); });
}

/*
 * Connection status
 *
 */

PageConnect.prototype.ConnectionStatus = {};

PageConnect.prototype.ConnectionStatus.load = function () {
    Irc2me.isConnected(function (connected) {
        if (connected) {
            page.setConnected();
        } else {
            page.setDisconnected();
        }
    });
}

PageConnect.prototype.ConnectionStatus.listen = function () {

    var self = this;

    Irc2me.Signals.connected.addListener(function() {
        page.setConnected();
    });

    Irc2me.Signals.disconnected.addListener(function() {
        page.setDisconnected();
    });

}

/*
 * System log
 *
 */

PageConnect.prototype.SystemLog = {};

PageConnect.prototype.SystemLog.append = function (statusObject) {

    var log = $("#connection-log");

    log.append("<p title=\"" + Helper.escapeHtml(statusObject.where || "") + "\">"
            + "[" + statusObject.time + "] "
            + statusObject.message + "</p>");

    // scroll parent to bottom
    var par = log.parent()[0];
    par.scrollTop = par.scrollHeight;

}

PageConnect.prototype.SystemLog.loadAll = function () {
    var self = this;

    UIState.getSystemLogs(function(logs) {

        // empty log
        $("#connection-log").html("");

        for (var i = 0; i < logs.length; i++) {
            self.append(logs[i]);
        }

    });
}

PageConnect.prototype.SystemLog.listen = function () {

    var self = this;

    UIState.Signals.addSystemLog.addListener(function (obj) {
        self.append(obj);
    });

}

/*
 * Setup UI
 *
 */

$(document).ready(function () {

    page = new PageConnect();

    page.SystemLog.loadAll();
    page.SystemLog.listen();

    page.ConnectionStatus.load();
    page.ConnectionStatus.listen();

    // bind "quit" button
    $("#quit").click(page.close);

});
