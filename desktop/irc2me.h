#pragma once

#include <QObject>
#include <QString>
#include <QAbstractSocket>
#include <QTcpSocket>
#include <vector>
#include <map>
#include <functional>
#include <memory>

#include "protobuf/messagestream.h"

/*
 * protobuf type aliases
 *
 */

template <class T>
using Repeated_T = google::protobuf::RepeatedField<T>;

template <class T>
using RepeatedPtr_T = google::protobuf::RepeatedPtrField<T>;

template <typename... T>
using Callback_T     = std::function<void(const T & ...)>;

using Client_T       = Protobuf::Messages::Client;

using Server_T       = Protobuf::Messages::Server;
using ResponseCode_T = Server_T::ResponseCode;

using Identity_T     = Protobuf::Messages::Identity;
using IdentityList_T = RepeatedPtr_T<Identity_T>;

using Network_T      = Protobuf::Messages::Network;
using NetworkList_T  = RepeatedPtr_T<Network_T>;

using ID_T = int64_t;

/*
 * irc2me class definition
 *
 */

class Irc2me : public QObject
{
    Q_OBJECT

public slots:

public:

    explicit Irc2me(QObject *parent = 0);
    ~Irc2me();

    static const QString DEFAULT_SERVER;
    static const quint16 DEFAULT_PORT;

    // message sending

    bool send(Client_T msg, QString *errorMsg = nullptr);

    // Send with callback
    bool send(Client_T msg, Callback_T<Server_T> callback);

    // connecting & authentication process

    void connect(const QString &host, quint16 port);

    bool auth(const QString &login, const QString &password,
              QString *errorMsg = nullptr);

    void disconnect();

    // Identities

/*
    void setIdentities     (const std::vector<Identity_T> &idents,
                            Callback_T<ResponseCode_T, std::vector<ID_T>> ids = Callback_T<ResponseCode_T, std::vector<ID_T>>()
                            );

    void deleteIdentities  (std::vector<ID_T> identids, Callback_T<ResponseCode_T> callback);
    void requestIdentities (Callback_T<ResponseCode_T, IdentityList_T> callback);

    // Networks

    void requestNetworkNames  (Callback_T<ResponseCode_T, NetworkList_T> cb);
    void requestNetworkDetails(std::vector<ID_T> networkids = std::vector<ID_T>());
*/

private:

    QAbstractSocket *socket;
    MessageStream *mstream;

    bool is_authorized;

    ID_T response_id = 0;

    std::map<ID_T, Callback_T<Server_T>> responseCallbacks;

    void addCallback(Client_T &clientMsg, Callback_T<Server_T> cb);
    void runCallback(ID_T responseId, const Server_T &msg);

private slots:

    void socket_connected();
    void socket_disconnected();
    void socket_error(QAbstractSocket::SocketError);

//    void authorized();
//    void notAuthorized();

    void mstream_newServerMessage(Protobuf::Messages::Server);

signals:

    // connection signals

    void connected();
    void disconnected();

    void socketError(QAbstractSocket::SocketError, QString errorString);
    void sendError(QString errorString);

    // authentication signals

    void authorized();
    void notAuthorized();

    // identity signals

    void identities       (const IdentityList_T &identities);

    // network signals

    void networkOnline    (ID_T networkid, bool online);
    void networkName      (ID_T networkid, std::string name);
    void networkReconnect (ID_T networkid, bool reconnect);

    // channel signals

    void channelOnline    (ID_T networkid, ID_T channelid, bool online);
    void channelName      (ID_T networkid, ID_T channelid, std::string name);
};
