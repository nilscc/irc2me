define(function (require) {

    // constructor
    var Backlog = function () {
        this.backlog = {};
    };

    var B = Backlog.prototype;

    /*
     * Helper functions
     *
     */

    B.initNetwork = function(network_id) {
        var self = this;
        self.backlog[network_id] = self.network(network_id);
    }

    B.initChannel = function(network_id, channel) {
        var self = this;
        self.initNetwork(network_id);
        self.backlog[network_id].channels[channel] = self.channel(network_id, channel);
    }

    /*
     * Loading messages
     *
     */

    B.network = function(network_id) {
        return this.backlog[network_id] || {
            messages: [],
            channels: {},
        };
    }

    B.channel = function(network_id, channel) {
        return this.backlog[network_id].channels[channel] || {
            messages: [],
        };
    }

    /*
     * Storing messages
     *
     */

    B.appendServerMessages = function(network_id, messages) {

        var self = this;

        // initialize network
        self.initNetwork(network_id);

        // append messages
        for (var i = 0; i < messages.length; i++) {
            self.network(network_id).messages.push(messages[i]);
        }
    };

    B.appendChannelMessages = function(network_id, channel, messages) {

        var self = this;

        // init channel
        self.initChannel(network_id, channel);

        // append messages
        for (var i = 0; i < messages.length; i++) {
            self.channel(network_id, channel).messages.push(messages[i]);
        }
    };

    return Backlog;

});
