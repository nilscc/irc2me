function ChromeMessage (id) {

    var f = function (content, callback) {

        if (typeof content == "function" && callback == null) {
            // swap arguments
            callback = content;
            content = null;
        }

        chrome.runtime.sendMessage({
            id: f._id,
            content: content || {},
        }, callback);
    };

    // link ChromeMessages prototype to f
    f.__proto__ = ChromeMessage.prototype;

    // set current values
    f._id = id;

    return f;
}

// inherit prototype from Function class
ChromeMessage.prototype = Object.create(Function.prototype);

/*
 * Global/static listener interface
 *
 */

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

/*
 * Prototype listener functions
 *
 */

ChromeMessage.prototype.setListener = function (run) {
    var self = this;

    ChromeMessage._allListeners[self._id] = run;
}

ChromeMessage.prototype.removeListener = function () {
    var self = this;

    var listeners = ChromeMessage._allListeners;

    // delete property
    if (listeners.hasOwnProperty(self._id) && typeof listeners[self._id] == "function") {
        delete listeners[self._id];
    }
}
