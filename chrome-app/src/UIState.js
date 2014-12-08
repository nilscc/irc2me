/*
 * UI state class
 *
 */

function UIState(irc2me) {
    this._systemLogs = [ {
        time: new Date().toLocaleTimeString(),
        where: "UIState()",
        message: "Application started.",
    } ];
    this._irc2me = irc2me;
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
