"use strict";

import Irc2me = require("Irc2me");

import UserListTemplate = require("pages/main/templates/UserList");

export class Class {

    private jquery_context;

    constructor (jquery_context?) {
        this.jquery_context = jquery_context;
    }

    /*
     * DOM modifications
     *
     */

    private jquery_userlistview;

    private userListView (selector?) {

        // initialize userlist view
        if (! this.jquery_userlistview) {
            this.jquery_userlistview = $("#user-list-view", this.jquery_context);
        }

        // return jquery object
        if (selector !== null) {
            return $(selector, this.jquery_userlistview);
        }
        else {
            return this.jquery_userlistview;
        }
    }

    private userList () {
        return this.userListView(".user-list");
    }

    private emptyUserlist () {
        this.userListView(".user-list").empty();
    }

    private showUserlist () {
        this.userListView().show();
    }

    hide () {
        this.userListView().hide();
    }

    /*
     * Loading user lists
     *
     */

    load (users, callback?) {

        var template_data = {
            operators: [],
            voice: [],
            users: [],
        };

        for (var i = 0; i < users.length; i++) {

            var user = users[i];

            var userflags = Irc2me.ProtobufMessages.Network.User.Userflag;

            if (user.flag != null) {
                switch (user.flag) {
                    case userflags.OPERATOR: {
                        template_data.operators.push(user);
                        break;
                    }
                    case userflags.VOICE: {
                        template_data.voice.push(user);
                        break;
                    }
                    default: {
                        console.error("Unexpected user flag", user.flag);
                    }
                }
            } else {
                template_data.users.push(user);
            }
        }

        // compile & insert the template
        this.userList().html(
            $(Mustache.to_html(UserListTemplate, template_data))
        );

        this.userListView().show();

        if (typeof callback == "function") {
            callback();
        }
    }
}
