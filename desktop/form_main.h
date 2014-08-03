#ifndef FORM_MAIN_H
#define FORM_MAIN_H

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

public:
    explicit FormMain(Irc2me&, QMainWindow &form_connect, QWidget *parent = 0);
    ~FormMain();

//    public setNetworks()

private:
    Ui::FormMain *ui;

    Irc2me &irc2me;
    QMainWindow &form_connect;
};

#endif // FORM_MAIN_H
