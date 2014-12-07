function ChromeMessage (id) {

    var f = function (content, callback) {

        if (typeof content == "function" && callback == null) {
            // swap arguments
            callback = content;
            content = null;
        }

        chrome.runtime.sendMessage({
            id: f.id,
            content: content || {},
        }, callback);
    };

    // copy prototype
    f.__proto__ = ChromeMessage.prototype;

    // set current values
    f.id = id;

    return f;
}

ChromeMessage._allListeners = {};

ChromeMessage.listenAll = function () {

    chrome.runtime.onMessage.addListener(function (msg, sender, sendResponse) {

        console.log(msg);

        // ignore invalid messages
        if (typeof msg != "object" || typeof msg.id != "string") {
            return;
        }

        // alias
        var listeners = ChromeMessage._allListeners;

        // lookup and run listener with msg.id
        if (listeners.hasOwnProperty(msg.id) && typeof listeners[msg.id] == "function") {
            return listeners[msg.id](msg.content, sendResponse);
        }

    });

}

// inherit from Function
ChromeMessage.prototype = Object.create(Function.prototype);

ChromeMessage.prototype.setListener = function (run) {
    var self = this;

    ChromeMessage._allListeners[self.id] = run;
}

ChromeMessage.prototype.removeListener = function () {
    var self = this;

    var listeners = ChromeMessage._allListeners;

    // delete property
    if (listeners.hasOwnProperty(self.id) && typeof listeners[self.id] == "function") {
        delete listeners[self.id];
    }
}
