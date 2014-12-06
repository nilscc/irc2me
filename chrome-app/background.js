chrome.app.runtime.onLaunched.addListener(function () {

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
