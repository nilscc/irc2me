#pragma once

#include <QMainWindow>

#include "form_connect.h"
#include "irc2me.h"

namespace Ui {
class FormMain;
}

class FormMain : public QMainWindow
{
    Q_OBJECT

public:

    explicit FormMain(Irc2me&, FormConnect &form_connect, QWidget *parent = 0);
    ~FormMain();

private slots:

    void quit();
    void showStatusWindow();
    void showNetworksWindow();
    void showIdentitiesWindow();

private:
    Ui::FormMain *ui;

    Irc2me &irc2me;
    FormConnect &form_connect;
    QMainWindow *form_ident;
    QMainWindow *form_networks;

};
