#-------------------------------------------------
#
# Project created by QtCreator 2014-05-31T13:19:09
#
#-------------------------------------------------

QT += core gui network

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = protobuf
TEMPLATE = app

QMAKE_CXXFLAGS += -std=c++11

# protobuf

PROTOS += \
    messages.proto

include(generate_proto.pri)

# files

HEADERS  += \
    irc2me.h \
    protobuf/iodevicestream.h \
    protobuf/messagestream.h \
    form_connect.h \
    form_main.h \
    channelviewer.h \
    ircchannel.h \
    widgets/hostselector.h \
    widgets/networklist.h \
    irc2me/backlog.h \
    form_networks.h \
    form/identities.h

SOURCES += \
    main.cpp \
    irc2me.cpp \
    protobuf/iodevicestream.cpp \
    protobuf/messagestream.cpp \
    form_connect.cpp \
    form_main.cpp \
    channelviewer.cpp \
    ircchannel.cpp \
    widgets/hostselector.cpp \
    widgets/networklist.cpp \
    irc2me/backlog.cpp \
    form_networks.cpp \
    form/identities.cpp

FORMS    += \
    connect.ui \
    form_main.ui \
    form_networks.ui \
    identities.ui

# config

win32 {
        INCLUDEPATH += "d:/dev/protobuf-2.5.0/src"
        LIBS += "-Ld:/mingw/lib"
}

LIBS += -lprotobuf

OTHER_FILES += \
    $$PROTOS \
    generate_proto.pri

RESOURCES +=
