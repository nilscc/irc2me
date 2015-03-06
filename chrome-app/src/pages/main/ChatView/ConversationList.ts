/// <reference path="../../../libs/jquery" />
/// <reference path="../../../libs/mustache" />
/// <amd-dependency path="Mustache" />

import $ = require("jquery");

import NetworkListTemplate     = require("pages/main/templates/NetworkList");
import NetworkListItemTemplate = require("pages/main/templates/NetworkListItem");

/*
 * Class definition
 *
 */

export class Class {

    /*
     * Constructor
     *
     */

    private jquery_context;

    constructor (jquery_context?) {
        this.jquery_context = jquery_context;
    }

    /*
     * jQuery helper
     *
     */

    private _networklist;

    private networkList (selector? : string) {

        if (this._networklist == null) {
            this._networklist = $(".network-list", this.jquery_context);
        }

        if (selector) {
            return $(selector, this._networklist);
        }
        else {
            return this._networklist;
        }
    }

    /*
     * UI
     *
     */

    // get (or create) network HTML element
    private getNetworkElement (network_id : number, network_name? : string, click_cb?) {

        var network = this.networkList(".network[data-network_id=" + network_id.toString() + "]");

        // check if network exists
        if (network.length == 0 && network_name !== null) {

            // template data
            var data = {
                network: {
                    id: network_id,
                    name: network_name,
                },
            };

            // compile template to jquery object
            network = $( Mustache.to_html(NetworkListTemplate, data) );

            // make clickable
            if (typeof click_cb == "function") {
                network.click(click_cb);
            }

            this.networkList().append(network);
        }

        return network;
    }

    addNetwork (network_id : number, network_name : string) {
        this.getNetworkElement(network_id, network_name);
    }

    // get (or create) network entry HTML element
    private getNetworkEntryElement (network_id, class_, id, name?, click_cb?) {

        var network = this.getNetworkElement(network_id);

        var entry = $("> .entry." + class_ + "[data-id=\"" + id + "\"]", network);

        // check if entry exists
        if (entry.length == 0 && name !== null) {

            // template data
            var data = {
                id: id,
                name: name,
                "class": class_,
            };

            // compile template to jquery object
            entry = $( Mustache.to_html(NetworkListItemTemplate, data) );

            // add click event
            if (typeof click_cb == "function") {
                entry.click(click_cb);
            }

            // add to list
            network.append(entry);
        }

        return entry;
    }

    addChannel (network_id, channel, cb) {
        this.getNetworkEntryElement(network_id, "channel", channel.name, channel.name, cb);
    }

    addQuery (network_id, user, cb) {
        this.getNetworkEntryElement(network_id, "query", user.nick, user.nick, cb);
    }

    // Set active conversation

    private setActiveConversation (network_id, class_, id) {

        // unset other 'active' conversations
        this.networkList(".entry.active:not([data-id=\"" + id + "\"])")
            .removeClass("active");

        // load entry
        this.getNetworkEntryElement(network_id, class_, id)
            .removeClass("unread")
            .addClass("active");
    }

    setActiveNetwork (network_id) {
        this.setActiveConversation(network_id, "network", network_id);
    }

    setActiveChannel (network_id, channel_name) {
        this.setActiveConversation(network_id, "channel", channel_name);
    }

    setActiveQuery (network_id, user) {
        this.setActiveConversation(network_id, "query", user.nick);
    }

    // Unread messages

    private unreadMessage (network_id, id, class_, name, click_cb) {
        return this.getNetworkEntryElement(network_id, class_, id, name, click_cb)
            .addClass("unread");
    }

    setUnreadNetworkMessage (network_id, cb) {
        // TODO: get network name
        this.unreadMessage(network_id, network_id, "network", "network_" + network_id, cb)
    }

    setUnreadPublicMessage (network_id, channel, cb) {
        this.unreadMessage(network_id, channel, "channel", channel, cb);
    }

    setUnreadPrivateMessage (network_id, user, cb) {
        // TODO: proper user identification (-> get from server!)
        this.unreadMessage(network_id, user.nick, "query", user.nick, cb);
    }
}
