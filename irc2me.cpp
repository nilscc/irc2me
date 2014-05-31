#include "irc2me.h"

#include <messages.pb.h>

const quint16 Irc2me::defaultPort = 6565;
const QString Irc2me::defaultServer = "nils.cc";

Irc2me::Irc2me(QObject *parent)
    : QObject(parent)
    , _socket(nullptr)
    , _istream(nullptr)
    , _ostream(nullptr)
{
}

Irc2me::~Irc2me()
{
    if (socket())
        delete socket();
}

void Irc2me::connect(const QString &host, quint16 port)
{
    if (!socket())
    {
        _socket = new QTcpSocket();

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
    socket()->disconnectFromHost();
}

bool Irc2me::auth(const QString &login, const QString &password)
{
    return false;
}

/*
 * Slots
 *
 */

void Irc2me::socket_connected() { emit connected(); }

void Irc2me::socket_disconnected() { emit disconnected(); }

void Irc2me::socket_error(QAbstractSocket::SocketError err)
{
    emit error(err, _socket->errorString());
}
