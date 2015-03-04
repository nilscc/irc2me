/// <reference path="../libs/jquery.d.ts" />
/// <reference path="../libs/dcodeIO/Long" />

/*
 * Helper namespace
 *
 */

import $    = require("jquery");
import Long = require("Long");

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
    /*
     * jQuery helper
     *
     */

    // helper to access input fields by name
export function inputByName (name, context) {
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

export interface Message {
    timestamp? : Object;
}

export function messageTimestamp (message : Message) : Date {
    return message.timestamp && new Date(longToNumber(message.timestamp));
}

export interface User {
    nick : string;
    name? : string;
    host? : string;
}

export function userNameHost (user : User) : string {
    return user.name ? (user.name + (user.host ? ("@" + user.host) : "")) : "?";
}

export function userFullname (user : User) : string {
    return user.nick + (user.name ? (" (" + userNameHost(user) + ")") : "");
}
