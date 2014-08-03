#pragma once

#include "irc2me.h"
#include <QWidget>

namespace Ui {
class NetworkList;
}

class NetworkList : public QWidget
{
    Q_OBJECT

public:
    explicit NetworkList(QWidget *parent = 0);
    ~NetworkList();

    void connectTo(Irc2me &irc2me);

private:
    Ui::NetworkList *ui;

public slots:

    void setNetworkList(const NetworkList_T &list);

};
