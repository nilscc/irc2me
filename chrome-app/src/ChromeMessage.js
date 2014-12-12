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
 * Add listener functions
 *
 */

ChromeMessage.prototype.addListener = function (run) {
    var self = this;

    if (typeof run != "function") {
        return;
    }

    var f = function (msg, sender, sendResponse) {

        // ignore invalid messages
        if (typeof msg != "object" || typeof msg.id != "string") {
            return;
        }

        if (msg.id == self._id) {
            run(msg.content, sendResponse);
        }
    };

    chrome.runtime.onMessage.addListener(f);

    return f;
}
