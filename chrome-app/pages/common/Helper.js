/*
 * Helper namespace
 *
 */

define(function (require) {

    var $ = require("jquery");
    var Long = require("Long");

    var Helper = function () { };

    // helper to escape typical html characters
    Helper.escapeHtml = function(text) {

        var map = {
            '&': '&amp;',
            '<': '&lt;',
            '>': '&gt;',
            '"': '&quot;',
            "'": '&#39;'
        };

        return text.replace(/[&<>"']/g, function(m) { return map[m]; });
    };

    /*
     * jQuery helper
     *
     */

    // helper to access input fields by name
    Helper.inputByName = function(name, context) {
        return $("input[name='" + name + "']", context).val();
    };

    Helper.scrollToBottom = function (elem) {
        $(elem).scrollTop(function () {
            return this.scrollHeight - $(this).innerHeight();
        });
    };

    Helper.scrollAtBottom = function (elem) {
        if (typeof elem != "object") {
            elem = $(elem);
        }
        return (elem.scrollTop() + 1)
            >= (elem.prop("scrollHeight") - elem.innerHeight());
    };

    /*
     * protobuf helper
     *
     */

    Helper.longToNumber = function (l) {
        return Long.prototype.toNumber.call(l);
    };

    Helper.messageTimestamp = function (message) {
        return message.timestamp && new Date(Helper.longToNumber(message.timestamp));
    };

    Helper.userNameHost = function (user) {
        return user.name ? (user.name + (user.host ? ("@" + user.host) : "")) : "?";
    };

    Helper.userFullname = function (user) {
        return user.nick + (user.name ? (" (" + Helper.userNameHost(user) + ")") : "");
    };

    return Helper;
});
