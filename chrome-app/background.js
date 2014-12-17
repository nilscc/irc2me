var irc2me = new Irc2me();
var uistate = new UIState();

irc2me.setLogger(function(statusObject) {
    uistate.addSystemLog(statusObject);
});

uistate.listen();
irc2me.listen();

/*
 * Application launch
 *
 */

chrome.app.runtime.onLaunched.addListener(function () {

    irc2me.init(function () {

        if (irc2me.isConnected()) {
            uistate.MainWindow.open();
        }
        else {
            uistate.ConnectionWindow.open();
        }

    });
});

/*
 * Other runtime events
 *
 */

chrome.runtime.onStartup.addListener(function () {

    uistate.addSystemLog({ message: "Application started." });

});

chrome.runtime.onSuspend.addListener(function () {

    irc2me.suspend();

});
