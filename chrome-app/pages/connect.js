var irc2me;

function escapeHtml(text) {

    var map = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#039;'
    };

    return text.replace(/[&<>"']/g, function(m) { return map[m]; });
}

$(document).ready(function () {

    irc2me = new Irc2me("../messages.proto");

    // bind "quit" button to chrome window close event
    $("#quit").click(function () {
        chrome.app.window.current().close();
    });

    var connect = function() {

        // helper to access input fields
        var input = function(name) { return $("input[name='" + name + "']").val(); };

        irc2me.connect(
            input("hostname"),
            input("port"),
            function () {
                irc2me.authenticate(
                    input("username"),
                    input("password")
                );
            }
        );
    }

    var disconnect = function () {
        irc2me.disconnect();
    };

    // Set logging function
    irc2me.setLogger(function (r) {

        var self = this;

        var log = $("#connection-log"),
            now = new Date();

        log.append("<p title=\"" + escapeHtml(r.where || "") + "\">["
                + now.toLocaleTimeString() + "] " + r.message + "</p>");

        // scroll parent to bottom
        var par = log.parent()[0];
        par.scrollTop = par.scrollHeight;

    });

    irc2me.log({
        status: "Start",
        message: "Application started.",
    });

    $("#connect").click(connect);
});
