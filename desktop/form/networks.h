#pragma once

#include "irc2me.h"

#include <QMainWindow>

namespace Ui {
class FormNetworks;
}

class FormNetworks : public QMainWindow
{
    Q_OBJECT

public:

    explicit FormNetworks(Irc2me &irc2me, QWidget *parent = 0);
    ~FormNetworks();

private slots:

    void on_pushButton_close_clicked();

    void on_pushButton_network_add_clicked();

private:

    Ui::FormNetworks *ui;

    Irc2me &irc2me;

    void reset();
};
