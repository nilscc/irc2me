#include "form_main.h"
#include "ui_form_main.h"
#include "widgets/networklist.h"

#include <QApplication>
#include <QBoxLayout>

FormMain::FormMain(Irc2me &irc2me, QMainWindow &form_connect, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormMain),
    irc2me(irc2me),
    form_connect(form_connect)
{
    ui->setupUi(this);

    // connect UI
    connect(ui->actionShow_connection_status, SIGNAL(triggered()),
            this, SLOT(showStatusWindow()));
    connect(ui->actionClose, SIGNAL(triggered()),
            this, SLOT(quit()));

    ui->treeWidget_networklist->connectTo(irc2me);

    // load view
//    loadDefaultView();
}

FormMain::~FormMain()
{
    delete ui;
}

/*
 * Views & layouts
 *
 */

//void FormMain::loadDefaultView()
//{
////    QWidget *view = new QWidget();
////    QBoxLayout *layout = new QBoxLayout(QBoxLayout::Direction::LeftToRight);

////    // add network list
////    NetworkList *netlis = new NetworkList();
////    // connect to IRC
////    netlis->connectTo(irc2me);
////    layout->addWidget(netlis);

////    // create and add channel input widget
////    QWidget *chanInput = newChannelInputWidget();
////    layout->addWidget(chanInput);

////    // set central widget with new layout
////    view->setLayout(layout);
////    setCentralWidget(view);
//}

//QWidget* FormMain::newChannelInputWidget()
//{
////    QWidget *widget = new QWidget();
////    QBoxLayout *topToBot = new QBoxLayout(QBoxLayout::Direction::TopToBottom);
////    topToBot->setContentsMargins(0,0,0,0);

////    // load channel view
////    ChannelView *chan = new ChannelView();
////    topToBot->addWidget(chan, 1);

////    widget->setLayout(topToBot);
////    return widget;
//}

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
