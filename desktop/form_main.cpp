#include "form_main.h"
#include "ui_form_main.h"
#include "widgets/networklist.h"

#include <QApplication>
#include <QBoxLayout>
#include <QList>

FormMain::FormMain(Irc2me &irc2me, QMainWindow &form_connect, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormMain),
    irc2me(irc2me),
    form_connect(form_connect)
{
    ui->setupUi(this);

    // setup splitter layout

    ui->splitter->setStretchFactor(0, 0);
    ui->splitter->setStretchFactor(1, 1);
    ui->splitter->setSizes(QList<int>() << 150 << 1);

    // connect UI
    connect(ui->actionShow_connection_status, SIGNAL(triggered()),
            this, SLOT(showStatusWindow()));
    connect(ui->actionClose, SIGNAL(triggered()),
            this, SLOT(quit()));

    ui->treeWidget_networklist->connectTo(irc2me);
}

FormMain::~FormMain()
{
    delete ui;
}

/*
 * Slots
 *
 */

void FormMain::quit()
{
    QApplication::quit();
}

void FormMain::showStatusWindow()
{
    form_connect.show();
}
