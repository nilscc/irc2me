/// <reference path="declare/chrome" />

"use strict"

/*
 * Manage authentication
 *
 */

import token = require("auth/token");

var authToken : string;

token.listen({
    getToken: ()  => { return authToken; },
    setToken: (t) => { authToken = t; },
});
