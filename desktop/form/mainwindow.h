#pragma once

#include <QMainWindow>

#include "form/connect.h"
#include "form/identities.h"
#include "irc2me.h"

namespace Ui {
class FormMainWindow;
}

class FormConnect;

class FormMainWindow : public QMainWindow
{
    Q_OBJECT

public:

    explicit FormMainWindow(Irc2me&, FormConnect &form_connect, QWidget *parent = 0);
    ~FormMainWindow();

private slots:

    void quit();
    void showStatusWindow();
    void showNetworksWindow();
    void showIdentitiesWindow();

private:
    Ui::FormMainWindow *ui;

    Irc2me &irc2me;
    FormConnect &form_connect;
    FormIdentities *form_ident;
    QMainWindow *form_networks;

};
