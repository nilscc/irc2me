var irc2me  = Irc2me.restore();
var uistate = UIState.restore(irc2me);

irc2me.setLogger(function(statusObject) {
    uistate.addSystemLog(statusObject);
});

/*
 * Setup chrome messages
 *
 */

uistate.setListeners();
irc2me.setListeners();

ChromeMessage.listenAll();

/*
 * Application launch
 *
 */

chrome.app.runtime.onLaunched.addListener(function () {

    console.log("onLaunched");

    chrome.app.window.create("pages/connect.html", {
        id: "connect",
        frame: "chrome",
        innerBounds: {
            width: 500,
            height: 500,
        },
        resizable: false,
    });

});

/*
 * Other runtime events
 *
 */

chrome.runtime.onSuspend.addListener(function () {

    uistate.suspend();
    irc2me.suspend();

});
