require.config({

    basePath: "/build",

    map: {
        "*": {
            "src/libs/dcodeIO/ProtoBuf":    "ProtoBuf",
            "src/libs/dcodeIO/ByteBuffer":  "ByteBuffer",
            "src/libs/dcodeIO/Long":        "Long",
        },
    },

    paths: {

        // paths
        libs:                           "/libs",
        src:                            "/build",
        build:                          "/build",
        pages:                          "/build/pages",
        proto:                          "/build/proto",

        // require.js
        require:                        "/libs/require",
        text:                           "/libs/require-text",

        // proto buf libs
        dcodeIO:                        "/libs/dcodeIO",
        ProtoBuf:                       "/libs/dcodeIO/ProtoBuf.min",
        ByteBuffer:                     "/libs/dcodeIO/ByteBufferAB.min",
        Long:                           "/libs/dcodeIO/Long.min",

        // mustache templates
        Mustache:                       "/libs/mustache.min",

        linkify:                        "/libs/linkify.amd.min",
        "linkify-string":               "/libs/linkify-string.amd.min",

        // jquery + plugins
        jquery:                         "/libs/jquery-2.1.1.min",
        "jquery-ajax-blog-arraybuffer": "/libs/jquery-ajax-blog-arraybuffers",
        "jquery-regex-filter":          "/libs/jquery-regex-filter",
        "jquery-tiny-pubsub":           "/libs/jquery-tiny-pubsub.min",
    },

    shim: {
        Mustache: {
            exports: "Mustache",
        },

        "jquery-ajax-blob-arraybuffer": ["jquery"],
        "jquery-regex-filter":          ["jquery"],
        "jquery-tiny-pubsub":           ["jquery"],
    },

});
