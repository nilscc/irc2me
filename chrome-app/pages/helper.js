/*
 * Helper namespace
 *
 */
var Helper = { };

// helper to escape typical html characters
Helper.escapeHtml = function(text) {

    var map = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#039;'
    };

    return text.replace(/[&<>"']/g, function(m) { return map[m]; });
}

// helper to access input fields by name
Helper.inputByName = function(name, context) {
    return $("input[name='" + name + "']", context).val();
}
