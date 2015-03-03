/// <reference path="libs/chrome.d.ts" />

"use strict";

import ChromeMessage = require("src/ChromeMessage");

/*
 * Messages & signals
 *
 */

export var getSystemLogs = new ChromeMessage("UIState.getSystemLogs");

export var MainWindow = {
    open:  new ChromeMessage("UIState.MainWindow.open"),
    close: new ChromeMessage("UIState.MainWindow.close"),
};

export var ConnectionWindow = {
    open:  new ChromeMessage("UIState.ConnectionWindow.open"),
    close: new ChromeMessage("UIState.ConnectionWindow.close"),
};

export var closeAllWindows = new ChromeMessage("UIState.closeAllWindows");

module Signals {

    export var addSystemLog = new ChromeMessage ("UIState.Signals.addSystemLog");

}

/*
 * Class definition
 *
 */

export class Class {

    private _systemLogs : string[];

    constructor () {
        this._systemLogs = [];
    }

    /*
     * Handle creating/closing windows etc
     *
     */

    ConnectionWindow = {

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

    MainWindow = {

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

    addSystemLog (msg) {

        // add current time & date to backlog message
        msg.time = new Date().toLocaleTimeString();

        // store in state
        this._systemLogs.push(msg);

        // send to listeners
        Signals.addSystemLog.call(msg);
    }

    /*
     * Chrome message interface: Incoming messages
     *
     */

    listen () {

        getSystemLogs.addListener((content, sendResponse) => {
            sendResponse(this._systemLogs);
        });

        /*
         * Window state
         *
         */

        MainWindow.open.addListener((content, sendResponse) => {
            this.MainWindow.open(sendResponse);
        });

        MainWindow.close.addListener((content, sendResponse) => {
            this.MainWindow.window().close();
            if (sendResponse) {
                sendResponse();
            }
        });

        ConnectionWindow.open.addListener((content, sendResponse) => {
            this.ConnectionWindow.open(sendResponse);
        });

        ConnectionWindow.close.addListener((content, sendResponse) => {
            this.ConnectionWindow.window().close();
            if (sendResponse) {
                sendResponse();
            }
        });

        closeAllWindows.addListener((content, sendResponse) => {

            var con  = this.ConnectionWindow.window(),
                main = this.MainWindow.window();

            if (main) {
                main.close();
            }
            if (con) {
                con.close();
            }

            sendResponse();
        });
    }
}
