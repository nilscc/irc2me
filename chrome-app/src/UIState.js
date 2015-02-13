/*
 * UI state class
 *
 */

define(function(require) {

    "use strict";

    var ChromeMessage = require("src/ChromeMessage");

    var UIState = function () {
        var self = this;
        self._systemLogs = [];
    }

    /*
     * Handle creating/closing windows etc
     *
     */

    UIState.prototype.ConnectionWindow = {

        id: "irc2me-connection-window",

        open: function (callback) {

            chrome.app.window.create("pages/connect.html", {
                id: this.id,
                frame: "chrome",
                innerBounds: {
                    minWidth: 500,
                    minHeight: 500,
                },
                resizable: true,
            }, callback);

        },

        window: function () {
            return chrome.app.window.get(this.id);
        },

    };

    UIState.prototype.MainWindow = {

        id: "irc2me-main-window",

        open: function (callback) {

            chrome.app.window.create("pages/main.html", {
                id: this.id,
                frame: "chrome",
                innerBounds: {
                    width: 960,
                    height: 600,
                    minWidth: 600,
                    minHeight: 400,
                },
            });

        },

        window: function () {
            return chrome.app.window.get(this.id);
        }

    };

    /*
     * System state
     *
     */

    UIState.prototype.addSystemLog = function(msg) {

        var self = this;

        // add current time & date to backlog message
        msg.time = new Date().toLocaleTimeString();

        // store in state
        self._systemLogs.push(msg);

        // send to listeners
        UIState.Signals.addSystemLog(msg);
    }

    /*
     * Chrome message interface: Incoming messages
     *
     */

    UIState.getSystemLogs = new ChromeMessage("UIState.getSystemLogs");

    UIState.MainWindow = {
        open:  new ChromeMessage("UIState.MainWindow.open"),
        close: new ChromeMessage("UIState.MainWindow.close"),
    }

    UIState.ConnectionWindow = {
        open:  new ChromeMessage("UIState.ConnectionWindow.open"),
        close: new ChromeMessage("UIState.ConnectionWindow.close"),
    }

    UIState.closeAllWindows = new ChromeMessage("UIState.closeAllWindows");

    UIState.prototype.listen = function() {

        var self = this;

        UIState.getSystemLogs.addListener(function(content, sendResponse) {
            sendResponse(self._systemLogs);
        });

        /*
         * Window state
         *
         */

        UIState.MainWindow.open.addListener(function(content, sendResponse) {
            self.MainWindow.open(sendResponse);
        });

        UIState.MainWindow.close.addListener(function(content, sendResponse) {
            self.MainWindow.window().close(sendResponse);
        });

        UIState.ConnectionWindow.open.addListener(function(content, sendResponse) {
            self.ConnectionWindow.open(sendResponse);
        });

        UIState.ConnectionWindow.close.addListener(function(content, sendResponse) {
            self.ConnectionWindow.window().close(sendResponse);
        });

        UIState.closeAllWindows.addListener(function(content, sendResponse) {

            var con  = self.ConnectionWindow.window(),
                main = self.MainWindow.window();

            if (main) {
                main.close();
            }
            if (con) {
                con.close();
            }

            sendResponse();
        });
    }

    /*
     * Chrome message signals: Outgoing messages
     *
     */

    UIState.Signals = {};

    UIState.Signals.addSystemLog = new ChromeMessage ("UIState.Signals.addSystemLog");

    /*
     * End of module
     *
     */
    return UIState;

});
