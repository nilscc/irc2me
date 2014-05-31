#pragma once

#include <QObject>
#include <QAbstractSocket>
#include <QString>

#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/message.h>

#include "protobuf/iodevicestream.h"

#include "messages.pb.h"

class MessageStream : public QObject
{
    Q_OBJECT

private:

    IODeviceInputStream *istream;
    IODeviceOutputStream *ostream;

public:

    explicit MessageStream(QAbstractSocket &socket, QObject *parent = 0);
    ~MessageStream();

    template <class Msg> bool send(Msg msg, QString *errormsg = nullptr);

signals:

    void newServerMessage(Protobuf::Messages::Server);

};

#include "messagestream.impl.h"
