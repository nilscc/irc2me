
import Irc2me  = require("src/Irc2me");
import UIState = require("src/UIState");

export var irc2me   = new Irc2me.Class();
export var uistate  = new UIState.Class();

export function run () {
    console.log(irc2me, uistate);
}
