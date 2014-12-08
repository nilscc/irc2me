/*
 * UI state class
 *
 */

function UIState(irc2me) {
    var self = this;

    self._systemLogs = [];
    self._irc2me = irc2me;
    self._runtime = new RuntimeStorage(self);
}

/*
 * Suspend & restore
 *
 */

UIState.prototype.suspend = function () {
    var self = this;

    self._runtime.storePrivateValues(["_systemLogs"]);
}

UIState.restore = function (irc2me) {
    var self = new UIState(irc2me);

    self._runtime.restorePrivateValues();

    return self;
}

UIState.ConnectionStatus = {
    Disconnected: 0,
    Connected: 1,
    Authorized: 2,
}

UIState.prototype.connectionStatus = function () {

    var self = this;

    if (self._irc2me.isConnected()) {
        return UIState.ConnectionStatus.Connected;
    }

    if (self._irc2me.isAuthenticated()) {
        return UIState.ConnectionStatus.Authorized;
    }

    // otherwise
    return UIState.ConnectionStatus.Disconnected;
}

/*
 * System state
 *
 */

UIState.prototype.addSystemLog = function(msg) {

    var self = this;

    // add current time & date to backlog message
    msg.time = new Date().toLocaleTimeString();

    self._systemLogs.push(msg);
}

/*
 * Queries
 *
 * All queries are meant to be called from content scripts
 *
 */

UIState.getSystemLogs       = new ChromeMessage("UIState.getSystemLogs");
UIState.getConnectionStatus = new ChromeMessage("UIState.getConnectionStatus");

/*
 * Messages
 *
 */

UIState.prototype.setListeners = function() {

    var self = this;

    UIState.getSystemLogs.setListener(function(content, sendResponse) {
        sendResponse(self._systemLogs);
    });

    UIState.getConnectionStatus.setListener(function(content, sendResponse) {
        sendResponse(self.connectionStatus());
    });

}
