var WhenLoaded = (function () {

    var irc2me, uistate;
    var funcs = [];

    var run = function () {
        if (typeof irc2me != "object" || typeof uistate != "object") {
            return;
        }
        while (funcs.length > 0) {
            funcs.shift().call(this, irc2me, uistate);
        }
    };

    var set = function(irc2me_, uistate_) {
        irc2me = irc2me_;
        uistate = uistate_;
        run();
    };

    var WhenLoaded = function (f) {
        funcs.push(f);
        run();
    };

    WhenLoaded.run = run;
    WhenLoaded.set = set;

    return WhenLoaded;

})();

/*
 * Application launch
 *
 */

chrome.app.runtime.onLaunched.addListener(function () {

    WhenLoaded(function(irc2me, uistate) {
        irc2me.init(function () {
            if (irc2me.isConnected()) {
                uistate.MainWindow.open();
            }
            else {
                uistate.ConnectionWindow.open();
            }
        });
    });

});

/*
 * Other runtime events
 *
 */

chrome.runtime.onStartup.addListener(function () {

    WhenLoaded(function(irc2me, uistate) {
        uistate.addSystemLog({ message: "Application started." });
    });

});

chrome.runtime.onSuspend.addListener(function () {

    WhenLoaded(function(irc2me, uistate) {
        irc2me.suspend();
    });

});

/*
 * Load irc2me and UI state
 *
 */

var mods =
    [ "src/Irc2me"
    , "src/UIState"
    ];

var main = function(Irc2me, UIState) {

    var irc2me  = new Irc2me();
    var uistate = new UIState();

    irc2me.setLogger(function(statusObject) {
        uistate.addSystemLog(statusObject);
    });

    WhenLoaded.set(irc2me, uistate);

    uistate.listen();
    irc2me.listen();
};

require(["./require-common"], function (common) {
    require(mods, main);
});
