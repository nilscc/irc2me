#pragma once

#include <QTableView>
#include <QTreeView>
#include <QColumnView>
#include <QListView>
#include <QStandardItemModel>

#include "irc2me.h"

class ChatView : public QTreeView
{
    Q_OBJECT
private:

    class Model : public QStandardItemModel
    {
    public:
        explicit Model(QObject *parent = nullptr);

        void appendIrcMessage(Protobuf::Messages::IrcMessage msg);
    };

public:
    explicit ChatView(QWidget *parent = nullptr);
    Model* model() const;

    void connectTo(const Irc2me &irc2me);

    void incomingIrcMessage(Protobuf::Messages::IrcMessage msg);
};
