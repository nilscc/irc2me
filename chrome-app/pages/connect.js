
/*
 * Page class
 *
 */

var deps =
    [ "src/Irc2me"
    , "src/UIState"
    , "common/Helper"
    , "jquery"
    ];


var main = function(Irc2me, UIState, Helper, $) {

    var PageConnect = function () { }

    /*
     * Actions
     *
     */

    PageConnect.prototype.connect = function() {
        var self = this;

        self.setConnecting();

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
        });
    }

    PageConnect.prototype.disconnect = function () {
        var self = this;

        Irc2me.disconnect(function () {
            self.setDisconnected();
        });
    }

    PageConnect.prototype.close = function() {
        chrome.app.window.current().close();
    }

    PageConnect.prototype.disableInput = function (disabled) {
        if (disabled == null) {
            disabled = true;
        }

        $("#login-form input")
            .prop('disabled', disabled);
    }

    PageConnect.prototype.setConnecting = function () {
        var self = this;

        $("#connect")
            .text("Connecting")
            .addClass("inactive");

        self.disableInput();
    }

    PageConnect.prototype.setConnected = function() {
        var self = this;

        $("#connect")
            .text("Disconnect")
            .removeClass("inactive")
            .one("click", function () { self.disconnect(); });

        self.disableInput();
    }

    PageConnect.prototype.setDisconnected = function() {
        var self = this;

        $("#connect")
            .text("Connect")
            .removeClass("inactive")
            .one("click", function () { self.connect(); });

        // enable input
        self.disableInput(false);
    }

    /*
     * Connection status
     *
     */

    PageConnect.prototype.ConnectionStatus = {};

    PageConnect.prototype.ConnectionStatus.load = function () {
        var self = this;

        Irc2me.isConnected(function (connected) {
            if (connected) {
                page.setConnected();
            } else {
                page.setDisconnected();
            }
        });
    }

    PageConnect.prototype.ConnectionStatus.listen = function () {

        var self = this,
            cur  = chrome.app.window.current();

        Irc2me.Signals.connected.addListener(function() {
            page.setConnected();

            // load main window
            UIState.MainWindow.open();
            UIState.ConnectionWindow.close();
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
        Helper.scrollToBottom(log.parent());
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
        $("#quit").click(function () {
            Irc2me.disconnect();
            UIState.closeAllWindows();
        });

        // bin "enter" key in input fields
        $("input").keypress(function (e) {
            if (e.which == 13) { // enter key
                page.connect();
            }
        });

    });
};

require(["../require-common"], function (common) {
    require(deps, main);
});
