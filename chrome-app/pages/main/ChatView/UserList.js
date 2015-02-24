define(function (require) {

    "use strict";

    var Irc2me = require("src/Irc2me");

    /*
     * Constructor
     *
     */

    var UserList = function (jquery_context) {
        this.jquery_context = jquery_context;
    };

    var U = UserList.prototype;

    /*
     * DOM modifications
     *
     */

    var userListView = function (opt_selector) {
        var self = this;

        // initialize userlist view
        if (! self.jquery_userlistview) {
            self.jquery_userlistview = $("#user-list-view", self.jquery_context);
        }

        // return jquery object
        if (opt_selector) {
            return $(opt_selector, self.jquery_userlistview);
        }
        else {
            return self.jquery_userlistview;
        }
    };

    var userList = function () {
        return userListView.call(this, ".user-list");
    };

    var emptyUserlist = function () {
        userListView.call(this, ".user-list").empty();
    };

    var showUserlist = function () {
        userListView.call(this).show();
    };

    U.hide = function () {
        userListView.call(this).hide();
    };

    /*
     * Loading user lists
     *
     */

    U.load = function (users, callback) {
        var self = this;

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
        userList.call(self).html(
            $(Mustache.to_html(userlistTemplate, template_data))
        );

        userListView.call(self).show();

        if (typeof callback == "function") {
            callback();
        }
    };

    /*
     * End of module
     *
     */

    return UserList;
});
