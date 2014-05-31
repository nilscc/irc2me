#include "protobuftest.h"
#include "ui_protobuftest.h"

ProtobufTest::ProtobufTest(Irc2me &irc2me, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::ProtobufTest),
    irc2me(irc2me)
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
}

ProtobufTest::~ProtobufTest()
{
    delete ui;
}

void ProtobufTest::log(QString msg)
{
    ui->listWidget->addItem(msg);
}

void ProtobufTest::lockServerInput(bool read_only)
{
//    ui->pushButton_connect->blockSignals(read_only);
    ui->lineEdit_login->setReadOnly(read_only);
    ui->lineEdit_password->setReadOnly(read_only);
    ui->lineEdit_port->setReadOnly(read_only);
    ui->lineEdit_server->setReadOnly(read_only);
}

/*
 * Slots
 *
 */

void ProtobufTest::on_pushButton_connect_clicked()
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

    irc2me.connect(server, port_num);

    log("Connecting to " + server + ":" + QString::number(port_num) + "...");
}

void ProtobufTest::irc2me_connected()
{
    log("Connected. Trying to authorize...");

    const QString &login = ui->lineEdit_login->text();
    const QString &pw = ui->lineEdit_password->text();

    QString errorMsg;
    if (!irc2me.auth(login, pw, &errorMsg))
        log("Error: " + errorMsg);
}

void ProtobufTest::irc2me_disconnected()
{
    log("Could not connect to server.");
    lockServerInput(false);
}

void ProtobufTest::irc2me_error(QAbstractSocket::SocketError, QString err)
{
    log("Error: " + err);
    lockServerInput(false);
}

void ProtobufTest::irc2me_authorized()
{
    log("Authorized!");
    lockServerInput(false);
}

void ProtobufTest::irc2me_notAuthorized()
{
    log("Failed to login.");
    lockServerInput(false);
}
