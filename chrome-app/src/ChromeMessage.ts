/// <reference path="libs/chrome" />

"use strict";

class ChromeMessage {

    private id : string;

    constructor (id : string) {
        this.id = id;
    }

    call (content? : Object, callback? : (any) => void) {

        chrome.runtime.sendMessage(null, {
            id: this.id,
            content: content || {},
        }, callback);

    }

    /*
     * Add listener functions
     *
     */

    addListener (run : (Object, any) => void) {

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
