#pragma once

#include <QMainWindow>

#include "irc2me.h"

namespace Ui {
class FormConnect;
}

class FormConnect : public QMainWindow
{
    Q_OBJECT

private:

    Ui::FormConnect *ui;
    Irc2me &irc2me;

    void log(QString msg);
    void lockServerInput(bool lock);

public:

    explicit FormConnect(Irc2me &irc, QWidget *parent = 0);
    ~FormConnect();

private slots:

    void irc2me_connected();
    void irc2me_disconnected();
    void irc2me_error(QAbstractSocket::SocketError, QString msg);

    void irc2me_authorized();
    void irc2me_notAuthorized();

    void irc2me_networkList(const NetworkList &);

    void on_pushButton_connect_clicked();
};
