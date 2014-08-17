#pragma once

#include "irc2me.h"

#include <array>

#include <QListWidget>
#include <QMainWindow>

namespace Ui {
class FormIdentities;
}

class FormIdentities : public QMainWindow
{
    Q_OBJECT

public:
    explicit FormIdentities(Irc2me &irc2me, QWidget *parent = 0);
    ~FormIdentities();

    void loadIdentityDetails(ID_T identid);

private slots:

    // irc2me slots

    void addIdentities(const IdentityList_T &idents);

    // UI slots

    void on_pushButton_close_clicked();
    void on_pushButton_ident_add_clicked();

    void on_listWidget_identities_itemActivated(QListWidgetItem *item);

private:
    Ui::FormIdentities *ui;

    Irc2me &irc2me;

    static const int IDENTITY_ID_ROLE = Qt::UserRole;

    std::map<ID_T, Identity_T> identities;

    std::map<ID_T, QListWidgetItem*> identityItems;
};
