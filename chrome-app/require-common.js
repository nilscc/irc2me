require.config({

    paths: {

        // paths
        libs:                           "../libs",
        src:                            "../src",

        // require.js
        require:                        "../libs/require",

        // proto buf libs
        ProtoBuf:                       "../libs/ProtoBuf.min",
        Autolinker:                     "../libs/Autolinker.min",
        ByteBuffer:                     "../libs/ByteBufferAB.min",
        Long:                           "../libs/Long.min",

        // mustache templates
        mustache:                       "../libs/mustache.min",

        // jquery + plugins
        jquery:                         "../libs/jquery-2.1.1.min",
        "jquery-ajax-blog-arraybuffer": "../libs/jquery-ajax-blog-arraybuffers",
        "jquery-regex-filter":          "../libs/jquery-regex-filter",
    },

    shim: {
        mustache: {
            exports: "Mustache",
        },

        "jquery-ajax-blob-arraybuffer": ["jquery"],
        "jquery-regex-filter":          ["jquery"],
    },

});
