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
    // connect network signals

    connect(&irc2me, SIGNAL(networkOnline(ID_T,bool)),
            this, SLOT(setNetworkOnline(ID_T,bool)));

    connect(&irc2me, SIGNAL(networkName(ID_T,std::string)),
            this, SLOT(setNetworkName(ID_T,std::string)));

    // connect channel signals

    connect(&irc2me, SIGNAL(channelOnline(ID_T,ID_T,bool)),
            this, SLOT(setChannelOnline(ID_T,ID_T,bool)));

    connect(&irc2me, SIGNAL(channelName(ID_T,ID_T,std::string)),
            this, SLOT(setChannelName(ID_T,ID_T,std::string)));

    // request data
    irc2me.requestNetworkNames();
}

/*
 * Slots
 *
 */

void NetworkList::setNetworkOnline(ID_T networkid, bool online)
{
    QTreeWidgetItem *networkItem = getNetworkItem(networkid);

    if (online)
        networkItem->setTextColor(0, networkActiveColor);
    else
        networkItem->setTextColor(0, networkInactiveColor);
}

void NetworkList::setNetworkName(ID_T networkid, string name)
{
    QTreeWidgetItem *networkItem = getNetworkItem(networkid);

    networkItem->setText(0, QString::fromStdString(name));
}

void NetworkList::setChannelOnline(ID_T networkid, ID_T channelid, bool online)
{
    QTreeWidgetItem *channelItem = getChannelItem(networkid, channelid);

    if (online)
        channelItem->setTextColor(0, channelActiveColor);
    else
        channelItem->setTextColor(0, channelInactiveColor);
}

void NetworkList::setChannelName(ID_T networkid, ID_T channelid, string name)
{
    QTreeWidgetItem *channelItem = getChannelItem(networkid, channelid);

    channelItem->setText(0, QString::fromStdString(name));
}

/*
 * Private slots
 *
 */

void NetworkList::emitChannelSelected(QTreeWidgetItem *item, int column)
{
    Q_UNUSED(column);

    bool n_ok, ch_ok;
    ID_T networkid = item->data(0, NETWORK_ID_ROLE).toLongLong(&n_ok);
    ID_T channelid = item->data(0, CHANNEL_ID_ROLE).toLongLong(&ch_ok);

    // quit on error
    if (! (n_ok && ch_ok))
        return;

    emit channelSelected(networkid, channelid);
}

/*
 * Private functions
 *
 */

QTreeWidgetItem* NetworkList::getNetworkItem(ID_T networkid)
{
    QTreeWidgetItem *networkItem;

    if (networkItems.count(networkid) == 0)
    {
        // Item does not exist yet, create new one:
        networkItem = new QTreeWidgetItem();
        networkItem->setTextColor(0, networkInactiveColor);

        networkItem->setData(0, NETWORK_ID_ROLE, networkid);
        networkItem->setData(0, CHANNEL_ID_ROLE, -1);

        // add to UI
        addTopLevelItem(networkItem);
    }
    else
    {
        networkItem = networkItems[networkid];
    }

    return networkItem;
}

QTreeWidgetItem* NetworkList::getChannelItem(ID_T networkid, ID_T channelid)
{
    QTreeWidgetItem *networkItem = getNetworkItem(networkid);
    QTreeWidgetItem *channelItem;

    if (channelItems[networkid].count(channelid) == 0)
    {
        // Item does not exist yet, create new one:
        channelItem = new QTreeWidgetItem();
        channelItem->setTextColor(0, channelInactiveColor);

        channelItem->setData(0, NETWORK_ID_ROLE, networkid);
        channelItem->setData(0, CHANNEL_ID_ROLE, channelid);

        // add to network item as child
        networkItem->addChild(channelItem);
    }
    else
    {
        channelItem = channelItems[networkid][channelid];
    }

    return channelItem;
}
