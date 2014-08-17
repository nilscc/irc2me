#include "form/networks.h"
#include "ui_networks.h"

FormNetworks::FormNetworks(Irc2me &irc2me, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormNetworks),
    irc2me(irc2me)
{
    ui->setupUi(this);

    // set size

    resize(700, 400);

    // setup splitter layout

    ui->splitter->setStretchFactor(0, 0);
    ui->splitter->setStretchFactor(1, 1);
    ui->splitter->setSizes(QList<int>() << 150 << 1);
    ui->splitter->setCollapsible(0, false);
    ui->splitter->setCollapsible(1, false);

    // show only title + close button

    setWindowFlags(Qt::FramelessWindowHint);
    setWindowFlags(Qt::WindowTitleHint);
    setWindowFlags(Qt::WindowCloseButtonHint);

    // show correct tab

    ui->tabWidget->setCurrentIndex(0);
}

FormNetworks::~FormNetworks()
{
    delete ui;
}

/*
 * private functions
 *
 */

void FormNetworks::reset()
{

}

/*
 * UI slots
 *
 */

void FormNetworks::on_pushButton_close_clicked()
{
    reset();

    close();
}

void FormNetworks::on_pushButton_network_add_clicked()
{

}
