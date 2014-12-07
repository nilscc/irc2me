var irc2me  = new Irc2me("messages.proto");
var uistate = new UIState(irc2me);

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
 * Chrome runtime events
 *
 */

// Start application
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
