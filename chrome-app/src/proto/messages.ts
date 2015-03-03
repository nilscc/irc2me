/// <amd-dependency path="text!messages.proto"

// hack to get around non-code amd modules, see:
//
//  * https://typescript.codeplex.com/discussions/492934
//  * https://typescript.codeplex.com/workitem/1046
//
declare function require(name : string) : string;

var messages = require("text!messages.proto");
export = messages;
