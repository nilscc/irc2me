"use strict";

interface Status {
    state : string;
    where : string;
    message : string;
}

class Logger {

    where : string;
    callback : (Status) => void;

    constructor (where : string, callback : (Status) => void) {
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

export = Logger;
