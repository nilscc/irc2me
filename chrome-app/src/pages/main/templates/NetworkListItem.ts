/// <amd-dependency path="text!/pages/main/templates/NetworkListItem.html" />

// hack to get around non-code amd modules, see:
//
//  * https://typescript.codeplex.com/discussions/492934
//  * https://typescript.codeplex.com/workitem/1046
//
declare function require(name : string) : string;

var NetworkListItem = require("text!/pages/main/templates/NetworkListItem.html");

export = NetworkListItem;
