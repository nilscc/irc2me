"use strict";

export interface LogMessage {
    state?  : string;
    where?  : string;
    message : string;
}

export class Class {

    where : string;
    callback : (msg : LogMessage) => void;

    constructor (where : string, callback : (msg : LogMessage) => void) {
        this.where = where;
        this.callback = callback;
    }

    log(state : string, message : string) {
        this.callback({
            state: state,
            where: this.where,
            message: message,
        });
    }

    warn(message : string) {
        this.log("Warning", message);
    }

    info(message : string) {
        this.log("Info", message);
    }

    error(message : string) {
        this.log("Error", message);
    }
}
