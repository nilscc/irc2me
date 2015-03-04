/// <reference path="../libs/jquery.d.ts" />

import $        = require("jquery");

import Backlog  = require("Backlog");
import Irc2me   = require("Irc2me");
import UIState  = require("UIState");

import ChatView = require("pages/main/ChatView");

"use strict";

export var backlog : Backlog.Class, chatview : ChatView.Class;

$(document).ready(() => {

    backlog  = new Backlog.Class();
    chatview = new ChatView.Class(backlog, "body");

    /*
     * Setup channel view
     *
     */

    chatview.listen();
    chatview.bindKeyEvents();
    chatview.loadConversations();

    /*
     * Setup signals
     *
     */

    Irc2me.Signals.disconnected.addListener(function () {
        UIState.ConnectionWindow.open.call();
        UIState.MainWindow.close.call();
    });

    Irc2me.Signals.incomingMessage.addListener(function (msg) {
        backlog.incomingMessage(msg);
    });

});

