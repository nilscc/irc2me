#include "messagestream.h"

#include <iostream>
#include <cstdio>

using namespace std;
using namespace google::protobuf::io;

MessageStream::MessageStream(QAbstractSocket &socket, QObject *parent)
    : QObject(parent)

    , istream(new IODeviceInputStream(socket))
    , ostream(new IODeviceOutputStream(socket))
{
    QObject::connect(&socket, SIGNAL(readyRead()),
                     this, SLOT(socket_readyRead()));
}

MessageStream::~MessageStream()
{
    delete istream;
    delete ostream;
}

bool MessageStream::send(const Protobuf::Messages::Client &msg,
                         QString *errorMsg)
{
    string s;
    msg.SerializeToString(&s);

    // create new output stream in seperate block to force call on BackUp etc.
    {
        CodedOutputStream out(ostream);
        out.WriteVarint64(s.length());
        out.WriteString(s);
    }

    return ostream->write(errorMsg);
}

/*
 * Signals
 *
 */

void MessageStream::socket_readyRead()
{
    string s;
    uint64_t len;

    {
        CodedInputStream in(istream);
        in.ReadVarint64(&len);
        in.ReadString(&s, len);
    }

    Protobuf::Messages::Server msg;
    msg.ParseFromString(s);

    emit newServerMessage(msg);
}
