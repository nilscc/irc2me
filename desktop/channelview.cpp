#include "channelview.h"
#include "ui_channelview.h"

ChannelView::ChannelView(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ChannelView)
{
    ui->setupUi(this);
}

ChannelView::~ChannelView()
{
    delete ui;
}
