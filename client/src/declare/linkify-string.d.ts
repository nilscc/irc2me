declare module "linkify-string" {

    interface linkifyOptions {
        tagName? : string;
        defaultProtocol? : string;
        target? : string;
        nl2br? : boolean;
        linkClass? : string;
        linkAttributes? : {};
        format? : (link : string, type : string) => string;
        formatHref? : (link : string, type : string) => string;
    }

    function linkifyString(str : string, options? : linkifyOptions) : string;

    export = linkifyString;
}
