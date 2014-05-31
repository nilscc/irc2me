#include "irc2me.h"

#include <QDebug>

#include <messages.pb.h>

const quint16 Irc2me::DEFAULT_PORT = 6565;
const QString Irc2me::DEFAULT_SERVER = "online.nils.cc";

Irc2me::Irc2me(QObject *parent)
    : QObject(parent)
    , _socket(nullptr)
    , _mstream(nullptr)
{
}

Irc2me::~Irc2me()
{
    disconnect();
}

void Irc2me::connect(const QString &host, quint16 port)
{
    disconnect();

    if (!socket())
    {
        _socket = new QTcpSocket();

        _socket->setSocketOption(QAbstractSocket::KeepAliveOption, 1);

        QObject::connect(_socket, SIGNAL(connected()),
                         this, SLOT(socket_connected()));
        QObject::connect(_socket, SIGNAL(disconnected()),
                         this, SLOT(socket_disconnected()));
        QObject::connect(_socket, SIGNAL(error(QAbstractSocket::SocketError)),
                         this, SLOT(socket_error(QAbstractSocket::SocketError)));
    }

    socket()->connectToHost(host, port);
}

void Irc2me::disconnect()
{
    if (_mstream) delete _mstream; _mstream = nullptr;

    if (_socket)
    {
        _socket->flush();
        _socket->disconnectFromHost();
    }

    if (_socket) delete _socket; _socket = nullptr;
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
    _mstream = new MessageStream(*_socket);

    emit connected();
}

void Irc2me::socket_disconnected()
{
    emit disconnected();
}

void Irc2me::socket_error(QAbstractSocket::SocketError err)
{
    emit error(err, _socket->errorString());
}
