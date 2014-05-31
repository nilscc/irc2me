#include "messagestream.h"

#include <iostream>
#include <cstdio>

using namespace google::protobuf::io;

MessageStream::MessageStream(QAbstractSocket &socket, QObject *parent)
    : QObject(parent)

    , istream(new IODeviceInputStream(socket))
    , ostream(new IODeviceOutputStream(socket))
{
}

MessageStream::~MessageStream()
{
    delete istream;
    delete ostream;
}
