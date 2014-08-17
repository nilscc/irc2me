#include "ui_mainwindow.h"

#include "form/mainwindow.h"
#include "form/networks.h"
#include "form/identities.h"

#include "widgets/networklist.h"

#include <QApplication>
#include <QBoxLayout>
#include <QList>

FormMainWindow::FormMainWindow(Irc2me &irc2me, FormConnect &form_connect, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormMainWindow),
    irc2me(irc2me),
    form_connect(form_connect),
    form_ident(nullptr),
    form_networks(nullptr)

{
    ui->setupUi(this);
    setAttribute(Qt::WA_DeleteOnClose);

    // setup splitter layout

    ui->splitter->setStretchFactor(0, 0);
    ui->splitter->setStretchFactor(1, 1);
    ui->splitter->setSizes(QList<int>() << 150 << 1);

    // connect UI

    connect(ui->actionShow_connection_status, SIGNAL(triggered()),
            this, SLOT(showStatusWindow()));

    connect(ui->action_Identities, SIGNAL(triggered()),
            this, SLOT(showIdentitiesWindow()));

    connect(ui->action_Networks, SIGNAL(triggered()),
            this, SLOT(showNetworksWindow()));

    connect(ui->actionClose, SIGNAL(triggered()),
            this, SLOT(quit()));

    ui->treeWidget_networklist->connectTo(irc2me);
}

FormMainWindow::~FormMainWindow()
{
    if (form_networks != nullptr)
    {
        form_networks->close();
        form_networks->deleteLater();
        form_networks = nullptr;
    }

    if (form_ident != nullptr)
    {
        form_ident->close();
        form_ident->deleteLater();
        form_ident = nullptr;
    }

    delete ui;

    form_connect.unsetFormMain();
    form_connect.disconnectFromServer();
}

/*
 * Slots
 *
 */

void FormMainWindow::quit()
{
    QApplication::quit();
}

void FormMainWindow::showStatusWindow()
{
    form_connect.show();
}

void FormMainWindow::showNetworksWindow()
{
    if (form_networks == nullptr)
        form_networks = new FormNetworks();

    form_networks->show();
}

void FormMainWindow::showIdentitiesWindow()
{
    if (form_ident == nullptr)
        form_ident = new FormIdentities();

    form_ident->show();
}
