#include "irc2me.h"

#include <QDebug>

#include <messages.pb.h>

const quint16 Irc2me::DEFAULT_PORT = 6565;
const QString Irc2me::DEFAULT_SERVER = "online.nils.cc";

Irc2me::Irc2me(QObject *parent)
    : QObject(parent)
    , socket(nullptr)
    , mstream(nullptr)
    , is_authorized(false)
{
}

Irc2me::~Irc2me()
{
    disconnect();
}

void Irc2me::connect(const QString &host, quint16 port)
{
    disconnect();

    if (!socket)
    {
        socket = new QTcpSocket();

        socket->setSocketOption(QAbstractSocket::KeepAliveOption, 1);

        QObject::connect(socket, SIGNAL(connected()),
                         this, SLOT(socket_connected()));
        QObject::connect(socket, SIGNAL(disconnected()),
                         this, SLOT(socket_disconnected()));
        QObject::connect(socket, SIGNAL(error(QAbstractSocket::SocketError)),
                         this, SLOT(socket_error(QAbstractSocket::SocketError)));
    }

    socket->connectToHost(host, port);

    is_authorized = false;
}

void Irc2me::disconnect()
{
    if (mstream) delete mstream; mstream = nullptr;

    if (socket)
    {
        socket->flush();
        socket->disconnectFromHost();
    }

    if (socket) delete socket; socket = nullptr;
}

bool Irc2me::send(const Protobuf::Messages::Client &msg, QString *errorMsg)
{
    return mstream->send(msg, errorMsg);
}

bool Irc2me::auth(const QString &login, const QString &password,
                  QString *errorMsg)
{
    Protobuf::Messages::Client clientMsg;

    clientMsg.set_auth_login(login.toStdString());
    clientMsg.set_auth_password(password.toStdString());

    return send(clientMsg, errorMsg);
}

/*
 * Slots
 *
 */

void Irc2me::socket_connected()
{
    mstream = new MessageStream(*socket);

    QObject::connect(mstream, SIGNAL(newServerMessage(Protobuf::Messages::Server)),
                     this, SLOT(mstream_newServerMessage(Protobuf::Messages::Server)));
    emit connected();
}

void Irc2me::socket_disconnected()
{
    emit disconnected();
}

void Irc2me::socket_error(QAbstractSocket::SocketError err)
{
    emit error(err, socket->errorString());
}

void Irc2me::mstream_newServerMessage(Protobuf::Messages::Server msg)
{
    if (!is_authorized && msg.has_response_code())
    {
        if (msg.response_code() == Protobuf::Messages::Server::ResponseOK)
            emit authorized();
        else
            emit notAuthorized();
    }
}
