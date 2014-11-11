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
    Protobuf::Messages::Authentication msg;
    msg.set_login(login.toStdString());
    msg.set_password(password.toStdString());

    return sendString(msg.SerializeAsString(), errorMsg);
}

void Irc2me::disconnect()
{
    if (mstream)
    {
        // send DISCONNECT message
        Msg::Client clientMsg;
        clientMsg.set_system_msg(Msg::DISCONNECT);
        send(clientMsg);

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

/*
 * Request slots
 *
 */

// Identity slots

/*
void Irc2me::requestIdentities(Callback_T<ResponseCode_T, IdentityList_T> callback)
{
    Msg::Client clientMsg;

    clientMsg.set_identity_get_all(true);

    if (callback)
        addCallback(clientMsg, [callback](Server_T msg) {
            callback(msg.response_code(), msg.identity_list());
        });

    send(clientMsg);
}

void Irc2me::setIdentities(const std::vector<Identity_T> &idents, Callback_T<ResponseCode_T, vector<ID_T>> callback)
{
    Msg::Client clientMsg;

    auto lis = clientMsg.mutable_identity_set();
    for (const Protobuf::Messages::Identity &ident : idents)
        *lis->Add() = ident;

    if (callback)
        addCallback(clientMsg, [callback](Server_T msg)
        {
            vector<ID_T> ids;
            for (const Identity_T &ident: msg.identity_list())
            {
                if (!ident.has_id())
                {
                    qDebug() << "Missing IDENTITY ID on SET IDENTITIES resposne";
                    break;
                }
                ids.push_back(ident.id());
            }
            callback(msg.response_code(), ids);
        });
    send(clientMsg);
}

void Irc2me::deleteIdentities(std::vector<ID_T> identids, Callback_T<ResponseCode_T> callback)
{
    Msg::Client clientMsg;

    for (ID_T id : identids)
        clientMsg.add_identity_remove(id);

    send(clientMsg, [callback](const Server_T &msg) {
        callback(msg.response_code());
    });
}

// Network slots

void Irc2me::requestNetworkNames(Callback_T<ResponseCode_T, NetworkList_T> cb)
{
    Msg::Client clientMsg;

    clientMsg.set_network_get_all_names(true);

    send(clientMsg, [cb](const Server_T &msg){
        cb(msg.response_code(), msg.network_list());
    });

}

void Irc2me::requestNetworkDetails(vector<ID_T> networkids)
{
    Q_UNUSED(networkids);
}

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
        }

        // quit, auth message is not supposed to contain any other data
        return;
    }

    // handle responses
    if (msg.has_response_id())
        return runCallback(msg.response_id(), msg);

    // check for identity list
    /*
    if (msg.identity_list_size() > 0)
    {
        emit identities(msg.identity_list());
    }
    */

    // check for network list
    /*
    if (msg.network_list_size() > 0)
    {
        for (const Msg::Network &network : msg.network_list())
        {
            ID_T networkid = network.network_id();

            if (network.has_network_online())
                emit networkOnline(networkid, network.network_online());

            if (network.has_network_name())
                emit networkName(networkid, network.network_name());

            if (network.network_channels_size() > 0)
            {
                for (const Msg::IrcChannel &channel : network.network_channels())
                {
                    ID_T channelid = channel.channel_id();

                    if (channel.has_channel_name())
                        emit channelName(networkid, channelid, channel.channel_name());

                    if (channel.has_channel_online())
                        emit channelOnline(networkid, channelid, channel.channel_online());
                }
            }
        }
    }
    */

    for (const Msg::IrcMessage &ircmsg : msg.irc_message())
        emit incomingIrcMessage(ircmsg);
}
