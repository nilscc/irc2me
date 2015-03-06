/// <reference path="../libs/jquery.d.ts" />

import $       = require("jquery");
import Helper  = require("common/Helper");
import Irc2me  = require("Irc2me");
import UIState = require("UIState");

// connection page modules
import UI               = require("pages/connect/ui");
import ConnectionStatus = require("pages/connect/connection-status");
import SystemLog        = require("pages/connect/systemlog");

function connect () {

    UI.setConnecting();

    // input data
    var host = Helper.inputByName("hostname"),
        port = Helper.inputByName("port"),
        user = Helper.inputByName("username"),
        pass = Helper.inputByName("password");

    Irc2me.connect.call({
        hostname: host,
        port: port,
        username: user,
        password: pass,
    });
}

function disconnect () {
    Irc2me.disconnect.call(() => {
        UI.setDisconnected(() => {
            connect();
        });
    });
}

$(document).ready(() => {

    SystemLog.load(UI.setSystemLog);
    SystemLog.listen(UI.appendSystemLog);

    ConnectionStatus.load((connected) => {
        if (connected) {
            UI.setConnected(disconnect);
        }
        else {
            UI.setDisconnected(connect);
        }
    });

    ConnectionStatus.listen(
        () => { UI.setConnected(disconnect); },
        () => { UI.setDisconnected(connect); }
    );

    $("#quit").click(() => {
        Irc2me.disconnect.call();
        UIState.closeAllWindows.call();
    });

    $("input").keypress((e) => {
        if (e.which == 13) {
            connect();
        }
    });

});
