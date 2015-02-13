define(function (require) {

    var Irc2me = require("src/Irc2me");
    var $      = require("jquery");

    var UserInput = function () {
    };

    var _ = UserInput.prototype;

    /*
     * Setter
     *
     */

    _.setNetwork = function (network_id) {
        this.currentNetwork = network_id;
    };

    _.setRecipient = function (recipient) {
        this.recipient = recipient;
    };

    /*
     * Key events
     *
     */

    _.bindKeyEvents = function (jquery_context) {
        $("input", jquery_context).keypress(function (e) {
            var input = $(this);

            if (e.which == 13) { // enter key
                self.send(input.val(), function () {
                    input.val("");
                });
            }
        });
    };

    /*
     * Sending commands
     *
     */


    /*
     * End of module
     *
     */
    return UserInput;
});
