#pragma once

#include <QTreeWidget>
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

public slots:

    void setNetworkList(const NetworkList_T &list);

signals:

    void channelSelected(const Network &, const IrcChannel &);

private slots:

    // link from `itemActivated` to `channelSelected`
    void emitChannelSelected(QTreeWidgetItem *item, int column);

private:

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

    void addNetwork(const Network &network);

    void updateNetworkList();
};
