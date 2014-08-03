#include "form_connect.h"
#include "ui_connect.h"

FormConnect::FormConnect(Irc2me &irc2me, QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::FormConnect)
    , irc2me(irc2me)
{
    ui->setupUi(this);

    ui->lineEdit_server->setText(Irc2me::DEFAULT_SERVER);
    ui->lineEdit_port->setText(QString::number(Irc2me::DEFAULT_PORT));

    connect(&irc2me, SIGNAL(connected()),
            this, SLOT(irc2me_connected()));
    connect(&irc2me, SIGNAL(disconnected()),
            this, SLOT(irc2me_disconnected()));
    connect(&irc2me, SIGNAL(error(QAbstractSocket::SocketError, QString)),
            this, SLOT(irc2me_error(QAbstractSocket::SocketError, QString)));

    connect(&irc2me, SIGNAL(authorized()),
            this, SLOT(irc2me_authorized()));
    connect(&irc2me, SIGNAL(notAuthorized()),
            this, SLOT(irc2me_notAuthorized()));

    connect(&irc2me, SIGNAL(networkList(NetworkList)),
            this, SLOT(irc2me_networkList(NetworkList)));
}

FormConnect::~FormConnect()
{
    delete ui;
}

void FormConnect::log(QString msg)
{
    ui->listWidget->addItem(msg);
    ui->listWidget->scrollToBottom();
}

void FormConnect::lockServerInput(bool lock)
{
    ui->pushButton_connect->setDisabled(lock);
    ui->pushButton_save->setDisabled(lock);

    ui->lineEdit_login->setDisabled(lock);
    ui->lineEdit_password->setDisabled(lock);
    ui->lineEdit_port->setDisabled(lock);
    ui->lineEdit_server->setDisabled(lock);
}

/*
 * Slots
 *
 */

void FormConnect::on_pushButton_connect_clicked()
{
    if (!connected)
    {
        QString server = ui->lineEdit_server->text();
        QString port = ui->lineEdit_port->text();
        QString login = ui->lineEdit_login->text();
        QString pw = ui->lineEdit_password->text();

        // check server
        if (server == "")
        {
            log("No server specified.");
            return;
        }

        // try to read port number
        short port_num;
        if (port == "")
        {
            port_num = Irc2me::DEFAULT_PORT;
            log("No port specified, using default port " + QString::number(port_num));
        }
        else
        {
            bool port_ok;
            port_num = port.toShort(&port_ok);
            if (!port_ok)
            {
                log("Invalid port number: " + port);
                return;
            }
        }

        if (login == "" || pw == "")
        {
            log("Missing login info");
            return;
        }

        lockServerInput(true);
        log("Connecting to " + server + ":" + QString::number(port_num) + "...");
        irc2me.connect(server, port_num);
    }
    else
    {
        lockServerInput(true);
        log("Disconnecting...");
        irc2me.disconnect();
    }
}

void FormConnect::irc2me_connected()
{
    log("Connected. Trying to authorize...");

    const QString &login = ui->lineEdit_login->text();
    const QString &pw = ui->lineEdit_password->text();

    QString errorMsg;
    if (!irc2me.auth(login, pw, &errorMsg))
        log("Error: " + errorMsg);

    connected = true;
    ui->pushButton_connect->setDisabled(false);
    ui->pushButton_connect->setText("Disconnect");
}

void FormConnect::irc2me_disconnected()
{
    log("Disconnected from server.");
    lockServerInput(false);
    connected = false;
    ui->pushButton_connect->setText("Connect");
}

void FormConnect::irc2me_error(QAbstractSocket::SocketError, QString err)
{
    log("Error: " + err);
    lockServerInput(false);
}

void FormConnect::irc2me_authorized()
{
    log("Authorized!");
//    lockServerInput(false);
//    irc2me.disconnect();

    // request network list
    irc2me.requestNetworkList();
}

void FormConnect::irc2me_notAuthorized()
{
    log("Failed to login.");
    irc2me.disconnect();
}

void FormConnect::irc2me_networkList(const NetworkList &networks)
{
    log("Network list received.");
    for (const Protobuf::Messages::Network &network : networks)
    {
        QString id = QString::number(network.network_id());
        QString name = QString::fromStdString(network.network_name());
        log(" * [" + id + "] " + name );
    }
}
