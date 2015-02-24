define(function (require) {

    var $        = require("jquery");
    var Mustache = require("Mustache");

    /*
     * Constructor
     *
     */

    var ConversationList = function (jquery_context) {
        this.jquery_context = jquery_context;
    };

    var C = ConversationList.prototype;

    /*
     * jQuery helper
     *
     */

    C.networkList = function (opt_selector) {
        var self = this;

        if (!self._networklist) {
            self._networklist = $(".network-list", self.jquery_context);
        }

        if (opt_selector != null) {
            return $(opt_selector, self._networklist);
        }
        else {
            return self._networklist;
        }
    };

    /*
     * UI
     *
     */

    var networklistTemplate, networklistitemTemplate;

    // get (or create) network HTML element
    var getNetworkElement = function (network_id, network_name, click_cb) {
        var self = this;

        var network = self.networkList(".network[data-network_id=" + network_id + "]");

        // check if network exists
        if (network.length == 0) {

            // load template
            networklistTemplate = networklistTemplate ||
                $("#network-list-template").html();

            // template data
            var data = {
                network: {
                    id: network_id,
                    name: network_name,
                },
            };

            // compile template to jquery object
            network = $( Mustache.to_html(networklistTemplate, data) );

            // make clickable
            if (typeof click_cb == "function") {
                network.click(click_cb);
            }

            self.networkList().append(network);
        }

        return network;
    };

    C.addNetwork = function (network_id, network_name) {
        getNetworkElement.call(this, network_id, network_name);
    };

    // get (or create) network entry HTML element
    var getNetworkEntryElement = function (network_id, class_, id, name, click_cb) {
        var self = this;

        var network = getNetworkElement.call(self, network_id);

        var entry = $("> .entry." + class_ + "[data-id=\"" + id + "\"]", network);

        // check if entry exists
        if (entry.length == 0 && name) {

            // load template
            networklistitemTemplate = networklistitemTemplate ||
                $("#network-list-item-template").html();

            // template data
            var data = {
                id: id,
                name: name,
                "class": class_,
            };

            // compile template to jquery object
            entry = $( Mustache.to_html(networklistitemTemplate, data) );

            // add click event
            if (typeof click_cb == "function") {
                entry.click(click_cb);
            }

            // add to list
            network.append(entry);
        }

        return entry;
    };

    C.addChannel = function (network_id, channel, cb) {
        getNetworkEntryElement.call(this, network_id, "channel", channel.name, channel.name, cb);
    };

    C.addQuery = function (network_id, user, cb) {
        getNetworkEntryElement.call(this, network_id, "query", user.nick, user.nick, cb);
    };

    // Set active conversation

    var setActiveConversation = function (network_id, class_, id) {
        var self = this;

        // unset other 'active' conversations
        self.networkList(".entry.active:not([data-id=\"" + id + "\"])")
            .removeClass("active");

        // load entry
        getNetworkEntryElement.call(self, network_id, class_, id)
            .removeClass("unread")
            .addClass("active");
    };

    C.setActiveNetwork = function (network_id) {
        setActiveConversation.call(this, network_id, "network", network_id);
    };

    C.setActiveChannel = function (network_id, channel_name) {
        setActiveConversation.call(this, network_id, "channel", channel_name);
    };

    C.setActiveQuery = function (network_id, user) {
        setActiveConversation.call(this, network_id, "query", user.nick);
    };

    // Unread messages

    var unreadMessage = function (network_id, id, class_, name, click_cb) {
        return getNetworkEntryElement.call(this, network_id, class_, id, name, click_cb)
            .addClass("unread");
    };

    C.setUnreadNetworkMessage = function (network_id, cb) {
        // TODO: get network name
        unreadMessage.call(this, network_id, network_id, "network", "network_" + network_id, cb)
    };

    C.setUnreadPublicMessage = function (network_id, channel, cb) {
        unreadMessage.call(this, network_id, channel, "channel", channel, cb);
    };

    C.setUnreadPrivateMessage = function (network_id, user, cb) {
        // TODO: proper user identification (-> get from server!)
        unreadMessage.call(this, network_id, user.nick, "query", user.nick, cb);
    };

    /*
     * Events
     *
     */

    /*
     * End of module
     *
     */

    return ConversationList;

});
