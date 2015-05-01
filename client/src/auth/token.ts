export function get(callback : (token : string) => void) {
    chrome.runtime.sendMessage({ request: "auth-token" }, callback);
}

export function set(token : string) {
    chrome.runtime.sendMessage({ set: "auth-token", value: token });
}

/*
 * Install listener
 *
 */

export interface TokenListener {
    getToken : () => string;
    setToken : (token : string) => void;
}

export function listen(listener : TokenListener) {

    chrome.runtime.onMessage.addListener((msg, sender, sendResponse) => {
        if (typeof msg == "object") {
            if ("request" in msg && msg.request == "auth-token") {
                sendResponse( listener.getToken() );
            }
            else if ("set" in msg && msg.set == "auth-token") {
                listener.setToken( msg.token || "" );
            }
        }
    });

}
