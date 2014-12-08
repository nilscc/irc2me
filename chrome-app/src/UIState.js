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

    self._systemLogs.push(msg);
}

/*
 * Chrome message interface
 *
 */

UIState.getSystemLogs = new ChromeMessage("UIState.getSystemLogs");

UIState.prototype.setListeners = function() {

    var self = this;

    UIState.getSystemLogs.setListener(function(content, sendResponse) {
        sendResponse(self._systemLogs);
    });

}
