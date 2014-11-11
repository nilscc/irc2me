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

    // connect menus

    connect(ui->actionShow_connection_status, SIGNAL(triggered()),
            this, SLOT(showStatusWindow()));

    connect(ui->actionClose, SIGNAL(triggered()),
            this, SLOT(quit()));

    // user input

    connect(ui->inputPrompt, &InputPrompt::userInput,
            this, &FormMainWindow::inputPrompt_userInput);

    // connect network list
    ui->treeWidget_networklist->connectTo(irc2me);

    // connect chat view
    ui->chatView->connectTo(irc2me);
}

FormMainWindow::~FormMainWindow()
{
    if (form_ident != nullptr)
    {
        form_ident->close();
        form_ident->deleteLater();
        form_ident = nullptr;
    }

    if (form_networks != nullptr)
    {
        form_networks->close();
        form_networks->deleteLater();
        form_networks = nullptr;
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
    {
        if (form_ident == nullptr)
            form_ident = new FormIdentities(irc2me);
        form_networks = new FormNetworks(irc2me, form_ident);

        form_ident->loadIdentities();
    }

    form_networks->show();
}

void FormMainWindow::showIdentitiesWindow()
{
    if (form_ident == nullptr)
        form_ident = new FormIdentities(irc2me);

    form_ident->loadIdentities();
    form_ident->show();
}

/*
 * Irc UI events
 *
 */

void FormMainWindow::inputPrompt_userInput(QString input)
{
    if (input.size() == 0)
        return;

    // build protobuf message

    Client_T msg;

    Network_T *netw = msg.add_networks();
    netw->set_id(currentNetwork);

    Message_T *ircmsg = netw->add_messages();
    ircmsg->set_to(currentChannel.toStdString());

    // check commands
    if (input[0] == '/')
    {
        input.remove(0, 1);

        QStringList words = input.split(' ');

        if (! words.isEmpty())
        {
            ircmsg->set_type_raw(words[0].toStdString());

            words.removeFirst();
            ircmsg->set_content(words.join(" ").toStdString());
        }
    }

    // set type if not already done
    if (! (ircmsg->has_type_raw() || ircmsg->has_type()) )
        ircmsg->set_type(Message_T::PRIVATEMESSAGE);

    // set content if not already done
    if (! ircmsg->has_content() )
        ircmsg->set_content(input.trimmed().toStdString());

    irc2me.send(msg);
}
























