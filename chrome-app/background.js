var irc2me = new Irc2me();
var uistate = new UIState();

/*
 * Application launch
 *
 */

function loadApp() {

    irc2me.setLogger(function(statusObject) {
        uistate.addSystemLog(statusObject);
    });

    /*
     * Chrome messages
     *
     */

    uistate.setListeners();
    irc2me.setListeners();

    ChromeMessage.listenAll();

    /*
     * UI
     *
     */

    uistate.addSystemLog({ message: "Application started." });

    chrome.app.window.create("pages/connect.html", {
        id: "connect",
        frame: "chrome",
        innerBounds: {
            width: 500,
            height: 500,
        },
        resizable: false,
    });
}

chrome.app.runtime.onLaunched.addListener(function () {

    console.log("onLaunched");

    irc2me.init(function () {

        // start application
        loadApp();

    });
});

/*
 * Other runtime events
 *
 */

chrome.runtime.onSuspend.addListener(function () {

    irc2me.suspend();

});
