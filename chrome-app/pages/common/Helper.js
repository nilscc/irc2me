/*
 * Helper namespace
 *
 */

define(["jquery"], function ($) {

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

    Helper.userNameHost = function (user) {
        return user.name + "@" + user.host;
    };

    Helper.userFullname = function (user) {
        return user.nick + " (" + Helper.userNameHost(user) + ")";
    };

    return Helper;
});
