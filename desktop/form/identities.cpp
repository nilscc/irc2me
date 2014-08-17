#include "form/identities.h"
#include "ui_identities.h"

FormIdentities::FormIdentities(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormIdentities)
{
    ui->setupUi(this);

    resize(500, 300);

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
}

FormIdentities::~FormIdentities()
{
    delete ui;
}

void FormIdentities::on_pushButton_close_clicked()
{
    close();
}
