/*
 * Create a runtime storage container for 'object'. Optional 'id', otherwise
 * 'object.constructor.name' will be used
 *
 */
function RuntimeStorage (object, id) {
    var self = this;

    self._object = object;

    if (id == null) {
        id = object.constructor.name;
    }
    self._id = id;
}

// static list of types stored by 'storePrivateValues'
RuntimeStorage.storedTypes = [ "string", "number", "boolean" ]

RuntimeStorage.prototype._storageKey = function () {
    return "__" + this._id + "_runtime__";
}

/*
 * Store all private values of the object (ie. all properties with '_' as first
 * character and a non-object/function 'typeof')
 *
 */
RuntimeStorage.prototype.storePrivateValues = function (callback) {

    var self = this;

    // define storage object
    var data = {},
        key  = self._storageKey();

    data[key] = {};

    // fill data with property values
    var properties = Object.keys(self._object);
    for (var i = 0; i < properties.length; i++) {

        var prop = properties[i];

        // check if property is 'private' / starts with an underscore ('_')
        if (prop[0] != "_") {
            continue;
        }

        // check if type of property is in the stored types array
        if (RuntimeStorage.storedTypes.indexOf(typeof self._object[prop]) == -1) {
            continue;
        }

        // store value in obj
        data[key][prop] = self._object[prop];
    }

    // store data
    chrome.storage.local.set(data, callback);
}

/*
 * Restore all properties stored by 'storePrivateValues'. Runtime storage data
 * is deleted after restoring it.
 *
 */
RuntimeStorage.prototype.restorePrivateValues = function (callback) {

    var self = this;

    // alias
    var key = self._storageKey();

    chrome.storage.local.get(key, function (data) {

        console.log(data);

        if (typeof data[key] != "object") {
            return; // no data to restore
        }

        var properties = Object.keys(data[key]);
        for (var i = 0; i < properties.length; i++) {

            var prop = properties[i],
                val  = data[key][prop];

            // restore value
            self._object[prop] = val;
        }

        // remove stored data
        chrome.storage.local.remove(key, function () {

            // done, so check for any errors
            if (chrome.runtime.lastError) {
                return console.error("Error in RuntimeStorage.restorePrivateValues(): "
                    + chrome.runtime.lastError.message);
            }

            if (typeof callback == "function") {
                callback();
            }
        });

    });
}
