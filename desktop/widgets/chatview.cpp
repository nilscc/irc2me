#include "widgets/chatview.h"

#include <QTime>

using namespace std;

/*
 * Conversions
 *
 */

namespace TypeConversion
{

static const QString ircTypes[] = {
    "PRIVATEMESSAGE",
    "JOIN",
    "PART",
    "INVITE",
    "QUIT",
    "KICK",
    "NICK",
    "NOTICE",
    "TOPIC",
    "MOTD"
};

QString ircTypeToString(Protobuf::Messages::IrcMessage::IrcType ty)
{
    return ircTypes[ty];
}

}

/*
 * ChatView item model
 *
 */

ChatView::Model::Model(QObject *parent)
    : QStandardItemModel(0, 4, parent)
{
}

void ChatView::Model::appendIrcMessage(Protobuf::Messages::IrcMessage msg)
{
    QString curTime = "[" + QTime::currentTime().toString() + "]";

    QString from;
    if (msg.has_from_user())
        from = "<" + QString::fromStdString(msg.from_user().nick()) + ">";
    else
        from = "(" + QString::fromStdString(msg.from_server()) + ")";

    QString content = QString::fromStdString(msg.content());

    QString cmd = msg.has_type() ? TypeConversion::ircTypeToString(msg.type())
                                 : QString::fromStdString(msg.type_raw());

    qDebug() << msg.has_type() << msg.type();

    QStandardItem *time  = new QStandardItem(curTime);
    QStandardItem *who   = new QStandardItem(from);
    QStandardItem *what  = new QStandardItem(cmd);
    QStandardItem *cont  = new QStandardItem(content);

    appendRow({time, who, what, cont});
}

/*
 * ChatView item view
 *
 */

ChatView::ChatView(QWidget *parent)
    : QTreeView(parent)
{
    setModel(new Model(this));

    setRootIsDecorated(false);
    setHeaderHidden(true);
}

ChatView::Model* ChatView::model() const
{
    return static_cast<ChatView::Model*>(QTreeView::model());
}

void ChatView::connectTo(const Irc2me &irc2me)
{
    connect(&irc2me, &Irc2me::incomingIrcMessage,
            this, &ChatView::incomingIrcMessage);
}

void ChatView::incomingIrcMessage(Protobuf::Messages::IrcMessage msg)
{
    model()->appendIrcMessage(std::move(msg));
}
