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

        void appendIrcMessage(ID_T networkid, Message_T msg);
    };

public:
    explicit ChatView(QWidget *parent = nullptr);
    Model* model() const;

    void connectTo(const Irc2me &irc2me);

    void incomingIrcMessage(ID_T networkid, Message_T msg);
};
