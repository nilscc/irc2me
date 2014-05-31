#pragma once

#include <QObject>
#include <QString>
#include <QAbstractSocket>
#include <QTcpSocket>

#include "protobuf/iodevicestream.h"

class Irc2me : public QObject
{
    Q_OBJECT

private:

    QAbstractSocket *_socket;
    IODeviceInputStream *_istream;
    IODeviceOutputStream *_ostream;

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

    static const QString defaultServer;
    static const quint16 defaultPort;

    const QAbstractSocket* socket() const { return _socket; }

    void connect(const QString &host, quint16 port);
    void disconnect();

    bool auth(const QString &login, const QString &password);

signals:

    void connected();
    void disconnected();

    void error(QAbstractSocket::SocketError, QString errorString);

//    void authorized();
//    void notAuthorized();
};
