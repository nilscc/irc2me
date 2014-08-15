#include "widgets/networklist.h"

using namespace std;

NetworkList::NetworkList(QWidget *parent) :
    QTreeWidget(parent)
{
    setRootIsDecorated(false);
    setHeaderHidden(true);

    // connect private slots
    connect(this, SIGNAL(itemActivated(QTreeWidgetItem*,int)),
            this, SLOT(emitChannelSelected(QTreeWidgetItem*,int)));

}

void NetworkList::connectTo(Irc2me &irc2me)
{
    connect(&irc2me, SIGNAL(networkList(NetworkList_T)),
            this, SLOT(setNetworkList(NetworkList_T)));

    // request data
    irc2me.requestNetworkList();
}

/*
 * Slots
 *
 */

void NetworkList::setNetworkList(const NetworkList_T &list)
{
    for (const auto &network : list)
    {
        networks[network.network_id()] = network;
        for (const auto &channel : network.network_channels())
            channels[network.network_id()][channel.channel_id()];
    }

    // remove old network list from UI
    clear();

    updateNetworkList();
}

void NetworkList::emitChannelSelected(QTreeWidgetItem *item, int column)
{
    Q_UNUSED(column);

    bool n_ok, ch_ok;
    int networkid = item->data(0, NETWORK_ID_ROLE).toInt(&n_ok);
    int channelid = item->data(0, CHANNEL_ID_ROLE).toInt(&ch_ok);

    // quit on error
    if (! (n_ok && ch_ok))
        return;

    // quit on invalid network
    if (networks.count(networkid) == 0)
        return;

    // quit on invalid channel
    if (channels.count(networkid) == 0 || channels[networkid].count(channelid) == 0)
        return;

    const Network    &network = networks[networkid];
    const IrcChannel &channel = channels[networkid][channelid];

    emit channelSelected(network, channel);
}

/*
 * Helper
 *
 */

void NetworkList::addNetwork(const Protobuf::Messages::Network &network)
{
    string networkname = network.network_name();
    int64_t networkid = network.network_id();

    // create network item
    QTreeWidgetItem *netwItem = new QTreeWidgetItem();
    netwItem->setText(0, QString::fromStdString(networkname));

    // store network ID
    netwItem->setData(0, NETWORK_ID_ROLE, networkid);
    netwItem->setData(0, CHANNEL_ID_ROLE, -1);

    // add item to lookup map
    networkItems[networkid] = netwItem;

    for (const auto &channel : network.network_channels())
    {
        int64_t channelid  = channel.channel_id();
        string channelname = channel.channel_name();

        // create channel item
        QTreeWidgetItem *chnItem = new QTreeWidgetItem();
        chnItem->setText(0, QString::fromStdString(channelname));

        // store channel ID
        chnItem->setData(0, NETWORK_ID_ROLE, networkid);
        chnItem->setData(0, CHANNEL_ID_ROLE, channelid);

        // add item to lookup map
        channelItems[networkid][channelid] = chnItem;

        // add channel to network as child
        netwItem->addChild(chnItem);
    }

    // add network to list
    addTopLevelItem(netwItem);
}

void NetworkList::updateNetworkList()
{
    for (const auto &network : networks)
    {
        // check if network is already being displayed
        if (networkItems.count(network.first) == 0)
            addNetwork(network.second);
    }
}
