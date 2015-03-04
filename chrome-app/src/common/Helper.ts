/// <reference path="../libs/jquery.d.ts" />
/// <reference path="../libs/dcodeIO/Long" />

import $     = require("jquery");
import Long  = require("Long");
import Types = require("common/Types");

export function escapeHtml (text : string) {

    var map = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#39;'
    };

    return text.replace(/[&<>"']/g, function(m) { return map[m]; });
}

/*
 * jQuery helper
 *
 */

// helper to access input fields by name
export function inputByName (name : string, context?) : string {
    return $("input[name='" + name + "']", context).val();
}

export function scrollToBottom (elem) {
    $(elem).scrollTop(function () {
        return this.scrollHeight - $(this).innerHeight();
    });
}

export function scrollAtBottom (elem) {
    if (typeof elem != "object") {
        elem = $(elem);
    }
    return (elem.scrollTop() + 1)
        >= (elem.prop("scrollHeight") - elem.innerHeight());
}

/*
 * dcodeIO / ProtoBuf helper functions
 *
 */

export function longToNumber (l) {
    return Long.prototype.toNumber.call(l);
}

export function messageTimestamp (message : Types.Message) : Date {
    return message.timestamp && new Date(longToNumber(message.timestamp));
}

export function userNameHost (user : Types.User) : string {
    return user.name ? (user.name + (user.host ? ("@" + user.host) : "")) : "?";
}

export function userFullname (user : Types.User) : string {
    return user.nick + (user.name ? (" (" + userNameHost(user) + ")") : "");
}
