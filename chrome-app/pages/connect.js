/*
 * Helper class
 *
 */

var Helper = {

    // helper to escape typical html characters
    escapeHtml: function(text) {

        var map = {
            '&': '&amp;',
            '<': '&lt;',
            '>': '&gt;',
            '"': '&quot;',
            "'": '&#039;'
        };

        return text.replace(/[&<>"']/g, function(m) { return map[m]; });
    },

    // helper to access input fields
    inputByName: function(name, context) {
        return $("input[name='" + name + "']", context).val();
    },

};

/*
 * Page class
 *
 */

function PageConnect() {

    var self = this;

    // find important UI elements
    self._connectButton = $("#connect");
    self._quitButton = $("#quit");
}

/*
 * Actions
 *
 */

PageConnect.prototype.connect = function() {
    var self = this;

    // input data
    var host = Helper.inputByName("hostname"),
        port = Helper.inputByName("port"),
        user = Helper.inputByName("username"),
        pass = Helper.inputByName("password");

    Irc2me.connect({
        hostname: host,
        port: port,
    }, function () {

        console.log("connected!");

        Irc2me.authenticate({
            username: user,
            password: pass,
        }, function () {
            console.log("Authenticated!");
        });

    });
}

PageConnect.prototype.disconnect = function () {
    Irc2me.disconnect();
}

PageConnect.prototype.close = function() {
    chrome.app.window.current().close();
}

/*
 * UI updates
 *
 */

PageConnect.prototype.appendLogMessage = function (statusObject) {

    var log = $("#connection-log");

    log.append("<p title=\"" + Helper.escapeHtml(statusObject.where || "") + "\">"
            + "[" + statusObject.time + "] "
            + statusObject.message + "</p>");

    // scroll parent to bottom
    var par = log.parent()[0];
    par.scrollTop = par.scrollHeight;

}

PageConnect.prototype.setConnected = function() {
    var self = this;

    $("#connect")
        .text("Disconnect")
        .unbind("click")
        .click(self.disconnect);
}

PageConnect.prototype.setDisconnected = function() {
    var self = this;

    $("#connect")
        .text("Connect")
        .unbind("click")
        .click(self.connect);
}

/*
 * UI state queries
 *
 */

PageConnect.prototype.loadSystemLog = function () {
    var self = this;

    UIState.getSystemLogs(function(logs) {

        // empty log
        $("#connection-log").html("");

        for (var i = 0; i < logs.length; i++) {
            self.appendLogMessage(logs[i]);
        }

    });
}

PageConnect.prototype.loadConnectionStatus = function () {
    var self = this;

    self.setDisconnected();

    UIState.getConnectionStatus(function(stat) {

        switch (stat) {

            case UIState.ConnectionStatus.Authorized:
            case UIState.ConnectionStatus.Connected: {
                self.setConnected();
                break;
            }

            default: {
                self.setDisconnected();
            }
        }

    });
}

PageConnect.prototype.loadUI = function () {

    var self = this;

    self.loadSystemLog();
    self.loadConnectionStatus();

    // bind quit button
    self._quitButton.click(self.close);
}

/*
 * Messages
 *
 */

/*
PageConnect.prototype.listen = function () {

    var self = this;

    chrome.runtime.onMessage.addListener(function (msg, sndr, sendResponse) {

        // ignore invalid messages
        if (typeof msg != "object" || typeof msg.type != "string") {
            return;
        }

        var async = false;

        switch (msg.type) {
            default: {} // do nothing
        }

        if (async) {
            return true;
        }
    });
}
*/

var page;

$(document).ready(function () {

    page = new PageConnect();

    page.loadUI();

    //page.listen();

});
