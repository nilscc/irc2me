#include "widgets/networklist.h"

using namespace std;

NetworkList::NetworkList(QWidget *parent) :
    QTreeWidget(parent)/*,
    networkAction(QAction(this)),
    channelAction(QAction(this))*/
{
    setRootIsDecorated(false);
    setHeaderHidden(true);

    // connect private slots
    connect(this, SIGNAL(itemActivated(QTreeWidgetItem*,int)),
            this, SLOT(emitChannelSelected(QTreeWidgetItem*,int)));

//    connect(this, SIGNAL())
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
 * Private functions
 *
 */

QTreeWidgetItem* NetworkList::addNetwork(const Protobuf::Messages::Network &network)
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

    // add network to list
    addTopLevelItem(netwItem);

    for (const auto &channel : network.network_channels())
    {
        QTreeWidgetItem *chnItem = addChannel(network, channel);

        // add channel to network as child
        netwItem->addChild(chnItem);
    }

    return netwItem;
}

QTreeWidgetItem* NetworkList::addChannel(const Network &network, const IrcChannel &channel)
{
    int64_t networkid = network.network_id();

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

    return chnItem;
}

void NetworkList::updateNetworkList()
{
    for (const auto &network_pair : networks)
    {
        int64_t networkid = network_pair.first;
        const Network &network = network_pair.second;

        // check if network is already being displayed
        if (networkItems.count(networkid) == 0)
            addNetwork(network);

        QTreeWidgetItem *netwItem = networkItems[networkid];

        // color network
        if (network.network_online())
            netwItem->setTextColor(0, networkActiveColor);
        else
            netwItem->setTextColor(0, networkInactiveColor);

        for (const auto &channel_pair : channels[networkid])
        {
            int channelid = channel_pair.first;
            const IrcChannel &channel = channel_pair.second;

            if (channelItems[networkid].count(channelid) == 0)
                addChannel(network, channel);

            QTreeWidgetItem *chnItem = channelItems[networkid][channelid];

            // color channels
            if (network.network_online() && channel.channel_online())
                chnItem->setTextColor(0, channelActiveColor);
            else
                chnItem->setTextColor(0, channelInactiveColor);
        }
    }
}
