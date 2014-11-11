#include "form/connect.h"
#include "ui_connect.h"

#include "form/mainwindow.h"

FormConnect::FormConnect(Irc2me &irc2me, QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::FormConnect)
    , irc2me(irc2me)
{
    ui->setupUi(this);

    ui->lineEdit_server->setText(Irc2me::DEFAULT_SERVER);
    ui->lineEdit_port->setText(QString::number(Irc2me::DEFAULT_PORT));

    // show only title + close button
    setWindowFlags(Qt::FramelessWindowHint);
    setWindowFlags(Qt::WindowTitleHint);
    setWindowFlags(Qt::WindowCloseButtonHint);

    // connect

    connect(&irc2me, &Irc2me::connected,
            this,    &FormConnect::irc2me_connected);

    connect(&irc2me, &Irc2me::disconnected,
            this,    &FormConnect::irc2me_disconnected);

    connect(&irc2me, &Irc2me::socketError,
            this,    &FormConnect::irc2me_socketError);

    connect(&irc2me, &Irc2me::authorized,
            this,    &FormConnect::irc2me_authorized);

    connect(&irc2me, &Irc2me::notAuthorized,
            this,    &FormConnect::irc2me_notAuthorized);
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

void FormConnect::unsetFormMain()
{
    form_main = nullptr;
}

void FormConnect::connectToServer()
{
    if (connected)
        return;

    QString server = ui->lineEdit_server->text();
    QString port = ui->lineEdit_port->text();
    QString login = ui->lineEdit_login->text();
    QString pw = ui->lineEdit_password->text();

    // check server
    if (server == "")
    {
        log(tr("No server specified."));
        return;
    }

    // try to read port number
    short port_num;
    if (port == "")
    {
        port_num = Irc2me::DEFAULT_PORT;
        log(tr("No port specified, using default port") + " " + QString::number(port_num));
    }
    else
    {
        bool port_ok;
        port_num = port.toShort(&port_ok);
        if (!port_ok)
        {
            log(tr("Invalid port number") + ": " + port);
            return;
        }
    }

    if (login == "" || pw == "")
    {
        log(tr("Missing login info"));
        return;
    }

    lockServerInput(true);
    log(tr("Connecting to") + " " + server + ":" + QString::number(port_num) + "...");
    irc2me.connect(server, port_num);
}

void FormConnect::disconnectFromServer()
{
    if (!connected)
        return;

    lockServerInput(true);
    log(tr("Disconnecting..."));
    irc2me.disconnect();

    // hide main window (if available)
    if (form_main != nullptr)
    {
        form_main->close();
        form_main = nullptr;
    }
}

/*
 * Slots
 *
 */

void FormConnect::on_pushButton_connect_clicked()
{
    if (!connected)
        connectToServer();
    else
        disconnectFromServer();
}

void FormConnect::irc2me_connected()
{
    log(tr("Connected. Trying to authorize..."));

    const QString &login = ui->lineEdit_login->text();
    const QString &pw = ui->lineEdit_password->text();

    QString errorMsg;
    if (!irc2me.authenticate(login, pw, &errorMsg))
        log(tr("Error") + ": " + errorMsg);

    connected = true;
    ui->pushButton_connect->setDisabled(false);
    ui->pushButton_connect->setText(tr("Disconnect"));
}

void FormConnect::irc2me_disconnected()
{
    log("Disconnected from server.");
    lockServerInput(false);
    connected = false;
    ui->pushButton_connect->setText(tr("Connect"));

    if (form_main)
    {
        form_main->close();
        form_main = nullptr;
    }
}

void FormConnect::irc2me_socketError(QAbstractSocket::SocketError, QString err)
{
    show();
    log(tr("Error") + ": " + err);
    if (!connected)
        lockServerInput(false);
}

void FormConnect::irc2me_sendError(QString err)
{
    show();
    log(tr("Error") + ": " + err);
    if (!connected)
        lockServerInput(false);
}

void FormConnect::irc2me_authorized()
{
    log(tr("Authorized!"));

    // show main form and hide self
    if (!form_main)
        form_main = new FormMainWindow(irc2me, *this);

    form_main->show();
    hide();
}

void FormConnect::irc2me_notAuthorized()
{
    log(tr("Failed to login."));
}
