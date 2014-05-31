#pragma once

#include <QObject>
#include <QString>
#include <QAbstractSocket>
#include <QTcpSocket>

#include "protobuf/messagestream.h"

class Irc2me : public QObject
{
    Q_OBJECT

private:

    QAbstractSocket *_socket;
    MessageStream *_mstream;

    QAbstractSocket* socket() { return _socket; }

private slots:

    void socket_connected();
    void socket_disconnected();
    void socket_error(QAbstractSocket::SocketError);

//    void authorized();
//    void notAuthorized();

public:

    explicit Irc2me(QObject *parent = 0);
    ~Irc2me();

    static const QString DEFAULT_SERVER;
    static const quint16 DEFAULT_PORT;

    const QAbstractSocket* socket() const { return _socket; }

    template <class Msg>
    bool send(const Msg &msg, QString *errorMsg = nullptr)
    {
        return _mstream->send(msg, errorMsg);
    }

    void connect(const QString &host, quint16 port);
    void disconnect();

    bool auth(const QString &login, const QString &password,
              QString *errorMsg = nullptr);

signals:

    void connected();
    void disconnected();

    void error(QAbstractSocket::SocketError, QString errorString);

//    void authorized();
//    void notAuthorized();
};
