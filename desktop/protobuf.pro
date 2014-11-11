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
    widgets/hostselector.h \
    widgets/networklist.h \
    form/identities.h \
    form/connect.h \
    form/networks.h \
    form/mainwindow.h \
    stdint.h \
    widgets/chatview.h \
    widgets/inputprompt.h

SOURCES += \
    main.cpp \
    irc2me.cpp \
    protobuf/iodevicestream.cpp \
    protobuf/messagestream.cpp \
    widgets/hostselector.cpp \
    widgets/networklist.cpp \
    form/identities.cpp \
    form/connect.cpp \
    form/networks.cpp \
    form/mainwindow.cpp \
    widgets/chatview.cpp \
    widgets/inputprompt.cpp

FORMS    += \
    connect.ui \
    identities.ui \
    networks.ui \
    mainwindow.ui

# config

win32 {
        INCLUDEPATH += "d:/dev/protobuf-2.5.0/src"
        LIBS += "-Ld:/mingw/lib"
}

LIBS += -lprotobuf

OTHER_FILES += \
    $$PROTOS \
    generate_proto.pri \
    widgets/inputprompt.css

RESOURCES += \
    styles.qrc \
    fonts.qrc
