/*
 * Install all required chrome handlers.
 *
 * Uses a separate script to not depend on async loading time.
 *
 */

/// <reference path="../declare/chrome" />

chrome.app.runtime.onLaunched.addListener(() => {

    chrome.app.window.create("static/login.html", {
        id: "login-window",
        frame: "chrome",
        innerBounds: {
            minWidth: 500,
            minHeight: 500,
        },
        resizable: true,
    });

});

chrome.runtime.onStartup.addListener(() => {
});
