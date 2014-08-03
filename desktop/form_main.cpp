#include "form_main.h"
#include "ui_form_main.h"

#include <QApplication>
#include <QBoxLayout>

FormMain::FormMain(Irc2me &irc2me, QMainWindow &form_connect, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormMain),
    irc2me(irc2me),
    form_connect(form_connect)
{
    ui->setupUi(this);

    connect(ui->actionShow_connection_status, SIGNAL(triggered()),
            this, SLOT(showStatusWindow()));

    connect(ui->actionClose, SIGNAL(triggered()),
            this, SLOT(quit()));
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
