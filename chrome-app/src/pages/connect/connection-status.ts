import Irc2me  = require("Irc2me");
import UIState = require("UIState");

export function load (callback : (connected : boolean) => void) {
    Irc2me.isConnected.call(callback);
}

export function listen (connectedCallback : () => void, disconnectedCallback? : () => void) {
    Irc2me.Signals.connected.addListener(() => {
        connectedCallback();

        // open main window & close connection page
        UIState.MainWindow.open.call();
        UIState.ConnectionWindow.close.call();
    });

    if (disconnectedCallback) {
        Irc2me.Signals.disconnected.addListener(disconnectedCallback);
    }
}
