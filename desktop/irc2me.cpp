#include "irc2me.h"

#include <QDebug>

#include <messages.pb.h>

const quint16 Irc2me::DEFAULT_PORT = 6565;
const QString Irc2me::DEFAULT_SERVER = "nils.cc";

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

bool Irc2me::send(const Msg::Client &msg, QString *errorMsg)
{
    bool ownErrorMsg = (errorMsg == nullptr);

    if (ownErrorMsg)
        errorMsg = new QString();

    bool success = mstream->send(msg, errorMsg);

    if (ownErrorMsg)
    {
        if (!success && errorMsg->length() > 0)
            emit sendError(*errorMsg);

        delete errorMsg;
    }

    return success;
}

/*
 * Request slots
 *
 */

void Irc2me::requestNetworkNames(vector<ID_T> networkids)
{
    Msg::Client clientMsg;

    if (networkids.size() > 0)
    {
        for (ID_T id : networkids)
            clientMsg.add_network_get_names(id);
    }
    else
    {
//        clientMsg.
    }

    send(clientMsg);
}

void Irc2me::requestNetworkDetails(vector<ID_T> networkids)
{
    Q_UNUSED(networkids);
}

/*
 * Specific messages
 *
 */

bool Irc2me::auth(const QString &login, const QString &password,
                  QString *errorMsg)
{
    Msg::Client clientMsg;

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
    emit socketError(err, socket->errorString());
}

void Irc2me::mstream_newServerMessage(Msg::Server msg)
{
    if (!is_authorized)
    {
        if (msg.has_response_code() &&
            msg.response_code() == Msg::Server::ResponseOK)
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

    // check for network list
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
}
