define(function(require) {

    // Logger class
    var Logger = function(where, callback)
    {
        this._where = where;
        this._callback = callback;
    }

    Logger.prototype.log = function(s, m)
    {
        return this._callback({ status: s, where: this._where, message: m });
    }

    Logger.prototype.warn = function(msg)
    {
        return this.log("Warning", msg);
    }

    Logger.prototype.info = function(msg)
    {
        return this.log("Info", msg);
    }

    Logger.prototype.error = function(msg)
    {
        return this.log("Error", msg);
    }

    /*
     * End of module
     *
     */
    return Logger;

});
