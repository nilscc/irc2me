/// <reference path="declare/chrome" />

"use strict"

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
