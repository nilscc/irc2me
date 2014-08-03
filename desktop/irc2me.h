#pragma once

#include <QObject>
#include <QString>
#include <QAbstractSocket>
#include <QTcpSocket>

#include "protobuf/messagestream.h"

typedef google::protobuf::RepeatedPtrField<Protobuf::Messages::Network> NetworkList_T;

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

    void connected();
    void disconnected();

    void error(QAbstractSocket::SocketError, QString errorString);

    void authorized();
    void notAuthorized();

    void networkList(const NetworkList_T &);
};
