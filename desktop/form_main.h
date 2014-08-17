#pragma once

#include <QMainWindow>

#include "irc2me.h"

namespace Ui {
class FormMain;
}

class FormMain : public QMainWindow
{
    Q_OBJECT

private slots:

    void quit();
    void showStatusWindow();
    void showNetworksWindow();

public:

    explicit FormMain(Irc2me&, QMainWindow &form_connect, QWidget *parent = 0);
    ~FormMain();

private:
    Ui::FormMain *ui;

    Irc2me &irc2me;
    QMainWindow &form_connect;
    QMainWindow *form_networks;

};
