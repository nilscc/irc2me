#pragma once

#include <QMainWindow>

#include "irc2me.h"

class FormMainWindow;

namespace Ui {
class FormConnect;
}

class FormConnect : public QMainWindow
{
    Q_OBJECT

private:

    Ui::FormConnect *ui;
    Irc2me &irc2me;
    FormMainWindow *form_main = nullptr;
    bool connected = false;

    void log(QString msg);
    void lockServerInput(bool lock);

public:

    explicit FormConnect(Irc2me &irc, QWidget *parent = 0);
    ~FormConnect();

    void unsetFormMain();

    void connectToServer();
    void disconnectFromServer();

private slots:

    void irc2me_connected();
    void irc2me_disconnected();
    void irc2me_socketError(QAbstractSocket::SocketError, QString msg);
    void irc2me_sendError(QString err);

    void irc2me_authorized();
    void irc2me_notAuthorized();

    void on_pushButton_connect_clicked();
};
