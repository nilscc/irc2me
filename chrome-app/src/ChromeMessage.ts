/// <reference path="libs/chrome" />

"use strict";

class ChromeMessage {

    private id : string;

    constructor (id : string) {
        this.id = id;
    }

    // overloaded method with only one callback and no data
    call (callback? : (any) => void);
    call (content : {}, callback? : (any) => void);

    // implementation
    call (content, callback?) {

        // test for callback-only overload
        if (typeof content === "function") {
            callback = content;
            content  = {};
        }

        chrome.runtime.sendMessage({
            id: this.id,
            content: content || {},
        }, callback);

    }

    /*
     * Add listener functions
     *
     */

    addListener (run : (Object, any) => boolean|void) {

        chrome.runtime.onMessage.addListener((msg, sender, sendResponse) => {

            // ignore invalid messages
            if (typeof msg != "object" || typeof msg.id != "string") {
                return;
            }

            if (msg.id == this.id) {
                return run(msg.content, sendResponse);
            }
        });
    }
}

export = ChromeMessage;
