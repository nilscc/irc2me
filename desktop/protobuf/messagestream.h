#pragma once

#include <string>

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

private slots:

    void socket_readyRead();

public:

    explicit MessageStream(QAbstractSocket &socket, QObject *parent = 0);
    ~MessageStream();

    bool sendString(std::string msg, QString *errorMsg = nullptr);

signals:

    void newServerMessage(Protobuf::Messages::Server);

};
