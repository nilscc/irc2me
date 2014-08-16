#pragma once

#include <QObject>
#include <QString>
#include <QAbstractSocket>
#include <QTcpSocket>

#include "protobuf/messagestream.h"

/*
 * protobuf type aliases
 *
 */

template <class T>
using Repeated_T = google::protobuf::RepeatedField<T>;

using Network_T     = Protobuf::Messages::Network;
using NetworkList_T = Repeated_T<Network_T>;

using ID_T = int64_t;

/*
 * irc2me class definition
 *
 */

class Irc2me : public QObject
{
    Q_OBJECT

private:

    QAbstractSocket *socket;
    MessageStream *mstream;

    bool is_authorized;

private slots:

    void socket_connected();
    void socket_disconnected();
    void socket_error(QAbstractSocket::SocketError);

//    void authorized();
//    void notAuthorized();

    void mstream_newServerMessage(Protobuf::Messages::Server);

public:

    explicit Irc2me(QObject *parent = 0);
    ~Irc2me();

    static const QString DEFAULT_SERVER;
    static const quint16 DEFAULT_PORT;

    void connect(const QString &host, quint16 port);
    void disconnect();

    bool send(const Protobuf::Messages::Client &msg,
              QString *errorMsg = nullptr);

    bool auth(const QString &login, const QString &password,
              QString *errorMsg = nullptr);

    bool requestNetworkList(QString *errorMsg = nullptr);


signals:

    // connection signals

    void connected();
    void disconnected();

    void error(QAbstractSocket::SocketError, QString errorString);

    // authentication signals

    void authorized();
    void notAuthorized();

    // network signals

    void networkOnline    (ID_T networkid, bool online);
    void networkName      (ID_T networkid, std::string name);
    void networkReconnect (ID_T networkid, bool reconnect);

    // channel signals

    void channelOnline    (ID_T networkid, ID_T channelid, bool online);
    void channelName      (ID_T networkid, ID_T channelid, std::string name);
};
