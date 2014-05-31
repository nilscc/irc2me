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

# files

HEADERS  += \
    protobuftest.h \
    irc2me.h \
    protobuf/iodevicestream.h \
    protobuf/messagestream.h

SOURCES += \
    main.cpp \
    protobuftest.cpp \
    irc2me.cpp \
    protobuf/iodevicestream.cpp \
    protobuf/messagestream.cpp

PROTOS += \
    messages.proto

FORMS    += protobuftest.ui

# config

win32 {
        INCLUDEPATH += "d:/dev/protobuf-2.5.0/src"
        LIBS += "-Ld:/mingw/lib"
}

LIBS += -lprotobuf

OTHER_FILES += \
    $$PROTOS \
    generate_proto.pri

include(generate_proto.pri)
