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
 * Chrome message interface
 *
 */

UIState.getSystemLogs = new ChromeMessage("UIState.getSystemLogs");

// Listener

UIState.prototype.setListeners = function() {

    var self = this;

    UIState.getSystemLogs.setListener(function(content, sendResponse) {
        sendResponse(self._systemLogs);
    });

}
