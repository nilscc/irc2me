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

    // protobuf type aliases
    using Network    = Protobuf::Messages::Network;
    using IrcChannel = Protobuf::Messages::IrcChannel;

    // constructor
    explicit NetworkList(QWidget *parent = 0);

    // setup functions
    void connectTo(Irc2me &irc2me);

    // colors:

    QColor networkInactiveColor = QColor(120,120,120);
    QColor networkActiveColor   = QColor(0,  0,  0);

    QColor channelInactiveColor = networkInactiveColor;
    QColor channelActiveColor   = networkActiveColor;

public slots:

    void setNetworkList(const NetworkList_T &list);

signals:

    void channelSelected(const Network &, const IrcChannel &);

private slots:

    // link from `itemActivated` to `channelSelected`
    void emitChannelSelected(QTreeWidgetItem *item, int column);

private:

    // menues

    // TODO

    // displaying network list

    using NetworkMap_T    = std::map<int, Network>;
    using ChannelMap_T    = std::map<int, std::map<int, IrcChannel>>;
    using ItemMap_T       = std::map<int, QTreeWidgetItem*>;
    using NestedItemMap_T = std::map<int, ItemMap_T>;

    NetworkMap_T    networks;
    ChannelMap_T    channels;

    ItemMap_T       networkItems;
    NestedItemMap_T channelItems;

    const int NETWORK_ID_ROLE = Qt::UserRole + 0;
    const int CHANNEL_ID_ROLE = Qt::UserRole + 1;

    QTreeWidgetItem* addNetwork(const Network &network);
    QTreeWidgetItem* addChannel(const Network &network, const IrcChannel &channel);

    void updateNetworkList();
};
