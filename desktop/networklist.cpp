#include "networklist.h"
#include "ui_networklist.h"

NetworkList::NetworkList(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::NetworkList)
{
    ui->setupUi(this);
}

NetworkList::~NetworkList()
{
    delete ui;
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
    ui->treeWidget_NetworkList->clear();

    for (const Protobuf::Messages::Network& network : list)
    {
        // network properties to display
        QString networkName = QString::fromStdString(network.network_name());

        QTreeWidgetItem *item = new QTreeWidgetItem();
        item->setText(0, networkName);

        ui->treeWidget_NetworkList->addTopLevelItem(item);
    }
}
