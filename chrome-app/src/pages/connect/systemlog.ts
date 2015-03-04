import UIState = require("UIState");

export interface SystemLog {
    message      : string;
    where?       : string;
    time?        : Date;
    time_string? : string;
}

export function load (callback : (objs : SystemLog[]) => void) {
    UIState.getSystemLogs.call(callback);
}

export function listen (callback : (obj : SystemLog) => void) {
    UIState.Signals.addSystemLog.addListener(callback);
}
