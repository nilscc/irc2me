/// <reference path="../../libs/jquery" />

import $         = require("jquery");
import Helper    = require("common/Helper");
import SystemLog = require("pages/connect/systemlog");

function disableInput (disable : boolean = true) {
    $("#login-form input")
        .prop("disabled", disable);
}

export function setConnecting (clickCallback? : () => void) {
    var con = $("#connect");

    con.text("Connecting")
       .addClass("inactive");

    if (clickCallback) {
       con.one("click", clickCallback);
    }
    else {
        con.unbind("click");
    }

    disableInput();
}

export function setConnected (clickCallback : () => void) {

    $("#connect")
        .text("Disconnect")
        .removeClass("inactive")
        .one("click", clickCallback);

    disableInput();
}

export function setDisconnected (clickCallback : () => void) {

    var con = $("#connect");

    con.text("Connect")
       .removeClass("inactive")
       .one("click", clickCallback);

    // enable input
    disableInput(false);
}

var log;
export function appendSystemLog (statusObject : SystemLog.SystemLog) {

    log = log || $("#connection-log");

    log.append("<p title=\"" + Helper.escapeHtml(statusObject.where || "") + "\">"
            + "[" + statusObject.time_string + "] "
            + statusObject.message + "</p>");

    // scroll parent to bottom
    Helper.scrollToBottom(log.parent());
}

export function setSystemLog (statusObjects : SystemLog.SystemLog[]) {

    log = log || $("#connection-log");
    log.empty();

    statusObjects.forEach(appendSystemLog);
}
