#ifndef CHANNELVIEW_H
#define CHANNELVIEW_H

#include <QWidget>

namespace Ui {
class ChannelView;
}

class ChannelView : public QWidget
{
    Q_OBJECT

public:
    explicit ChannelView(QWidget *parent = 0);
    ~ChannelView();

private:
    Ui::ChannelView *ui;
};

#endif // CHANNELVIEW_H
