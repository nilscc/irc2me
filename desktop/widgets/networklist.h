#pragma once

#include <QTreeWidget>
#include <QAction>
#include <map>

#include "irc2me.h"
#include "messages.pb.h"

class NetworkList : public QTreeWidget
{
    Q_OBJECT

public:

    // constructor
    explicit NetworkList(QWidget *parent = 0);

    // setup functions
    void connectTo(Irc2me &irc2me);

    // colors:

    QColor networkInactiveColor = QColor(120,120,120);
    QColor networkActiveColor   = QColor(0,  0,  0);

    QColor channelInactiveColor = networkInactiveColor;
    QColor channelActiveColor   = networkActiveColor;

signals:

    void channelSelected(ID_T networkid, ID_T channelid);

private slots:

    // link from `itemActivated` to `channelSelected`
    void emitChannelSelected(QTreeWidgetItem *item, int column);

    // link to irc2me network signals

    void setNetworkOnline    (ID_T networkid, bool online);
    void setNetworkName      (ID_T networkid, std::string name);

    // link to irc2me channel signals

    void setChannelOnline    (ID_T networkid, ID_T channelid, bool online);
    void setChannelName      (ID_T networkid, ID_T channelid, std::string name);

private:

    // menues

    // TODO

    // displaying network list

    using ItemMap_T       = std::map<ID_T, QTreeWidgetItem*>;
    using NestedItemMap_T = std::map<ID_T, ItemMap_T>;

    ItemMap_T       networkItems;
    NestedItemMap_T channelItems;

    const int NETWORK_ID_ROLE = Qt::UserRole + 0;
    const int CHANNEL_ID_ROLE = Qt::UserRole + 1;

    QTreeWidgetItem *getNetworkItem(ID_T networkid);
    QTreeWidgetItem *getChannelItem(ID_T networkid, ID_T channelid);
};
