#include "form_networks.h"
#include "ui_form_networks.h"

FormNetworks::FormNetworks(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormNetworks)
{
    ui->setupUi(this);

    // setup splitter layout

    ui->splitter->setStretchFactor(0, 0);
    ui->splitter->setStretchFactor(1, 1);
    ui->splitter->setSizes(QList<int>() << 200 << 1);
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

void FormNetworks::on_pushButton_cancel_clicked()
{
    reset();

    close();
}
