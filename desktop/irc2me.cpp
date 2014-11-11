#include "irc2me.h"

#include <QDebug>

#include <messages.pb.h>
#include <string>

const quint16 Irc2me::DEFAULT_PORT = 6565;
const QString Irc2me::DEFAULT_SERVER = "nils-vm.fritz.box";

namespace Msg = Protobuf::Messages;

using namespace std;

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

/*
 * Message sending
 *
 */

bool Irc2me::sendString(string s, QString *errorMsg)
{
    bool ownErrorMsg = (errorMsg == nullptr);

    if (ownErrorMsg)
        errorMsg = new QString();

    bool success = mstream->sendString(s, errorMsg);

    if (ownErrorMsg)
    {
        if (!success && errorMsg->length() > 0)
            emit sendError(*errorMsg);

        delete errorMsg;
    }

    return success;
}

bool Irc2me::send(Client_T msg, QString *errorMsg)
{
    return sendString(msg.SerializeAsString(), errorMsg);
}

bool Irc2me::send(Client_T msg, Callback_T<Server_T> callback)
{
    addCallback(msg, callback);
    return send(msg);
}

/*
 * Connection & authentication
 *
 */

void Irc2me::connect(const QString &host, quint16 port)
{
    disconnect();

    if (!socket)
    {
        socket = new QTcpSocket();

        socket->setSocketOption(QAbstractSocket::KeepAliveOption, 1);

        QObject::connect(socket, &QAbstractSocket::connected,
                         this,   &Irc2me::socket_connected);

        QObject::connect(socket, &QAbstractSocket::disconnected,
                         this,   &Irc2me::socket_disconnected);

        // new 'connect' method not possible for 'error' yet :(
        QObject::connect(socket, SIGNAL(error(QAbstractSocket::SocketError)),
                         this,   SLOT(socket_error(QAbstractSocket::SocketError)));
    }

    socket->connectToHost(host, port);

    is_authorized = false;
}

bool Irc2me::authenticate(QString login, QString password, QString *errorMsg)
{
    Auth_T msg;
    msg.set_login(login.toStdString());
    msg.set_password(password.toStdString());

    return sendString(msg.SerializeAsString(), errorMsg);
}

void Irc2me::disconnect()
{
    if (mstream)
    {
        if (is_authorized)
        {
            // send DISCONNECT message
            Msg::Client clientMsg;
            clientMsg.set_system_msg(Msg::DISCONNECT);
            send(clientMsg);
        }

        delete mstream;
    }
    mstream = nullptr;

    if (socket)
    {
        socket->flush();
        socket->disconnectFromHost();
    }

    if (socket) delete socket; socket = nullptr;
}

void Irc2me::addCallback(Client_T &clientMsg, Callback_T<Server_T> cb)
{
    if (cb)
    {
        clientMsg.set_response_id(response_id++);
        responseCallbacks[clientMsg.response_id()] = cb;
    }
}

void Irc2me::runCallback(ID_T responseId, const Server_T &msg)
{
    if (responseCallbacks.count(responseId) == 1 && responseCallbacks[responseId])
    {
        responseCallbacks[responseId](msg);
        responseCallbacks.erase(responseId);
    }
}

void Irc2me::respondSystemMsg(const Protobuf::Messages::SystemMsg &msg)
{
    switch (msg)
    {

    case Protobuf::Messages::DISCONNECT:
    {
        return disconnect();
    }

    case Protobuf::Messages::PING:
    {
        Client_T resp;
        resp.set_system_msg(Protobuf::Messages::PONG);

        // ignore error message
        QString ignore;
        send(resp, &ignore);

        return;
    }

    default: {}
    }
}

/*
 * Request slots
 *
 */

/*
 * Slots
 *
 */

void Irc2me::socket_connected()
{
    mstream = new MessageStream(*socket);

    QObject::connect(mstream, &MessageStream::newServerMessage,
                     this,    &Irc2me::mstream_newServerMessage);

    emit connected();
}

void Irc2me::socket_disconnected()
{
    emit disconnected();
}

void Irc2me::socket_error(QAbstractSocket::SocketError err)
{
    emit socketError(err, socket->errorString());
}

void Irc2me::mstream_newServerMessage(Msg::Server msg)
{
    if (!is_authorized)
    {
        if (msg.has_response_code() && msg.response_code() == Server_T::ResponseOK)
        {
            is_authorized = true;
            emit authorized();
        }
        else
        {
            is_authorized = false;
            emit notAuthorized();
            disconnect();
        }

        // quit, auth message is not supposed to contain any other data
        return;
    }

    // handle responses
    if (msg.has_response_id())
        return runCallback(msg.response_id(), msg);

    // system messages
    if (msg.has_system_msg())
        return respondSystemMsg(msg.system_msg());

    for (const Network_T &network : msg.networks())
        for (const Message_T &ircmsg : network.messages())
            emit incomingIrcMessage(network.id(), ircmsg);
}
