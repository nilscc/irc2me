/*
 * UI state class
 *
 */

function UIState() {
    var self = this;
    self._systemLogs = [];
}

/*
 * System state
 *
 */

UIState.prototype.addSystemLog = function(msg) {

    var self = this;

    // add current time & date to backlog message
    msg.time = new Date().toLocaleTimeString();

    // store in state
    self._systemLogs.push(msg);

    // send to listeners
    UIState.Signals.addSystemLog(msg);
}

/*
 * Chrome message interface: Incoming messages
 *
 */

UIState.getSystemLogs = new ChromeMessage("UIState.getSystemLogs");

UIState.prototype.listen = function() {

    var self = this;

    UIState.getSystemLogs.addListener(function(content, sendResponse) {
        sendResponse(self._systemLogs);
    });

}

/*
 * Chrome message signals: Outgoing messages
 *
 */

UIState.Signals = {};

UIState.Signals.addSystemLog = new ChromeMessage ("UIState.Signals.addSystemLog");
