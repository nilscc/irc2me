#pragma once

#include "irc2me.h"

#include <vector>
#include <map>

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

private:

    void setInputEnabled(bool enabled = true);

    // irc2me slots

    void addIdentityItem(Identity_T identity, QListWidgetItem *item = nullptr);

    void addIdentities(const IdentityList_T &idents);

    Ui::FormIdentities *ui;

    Irc2me &irc2me;

    ID_T currentIdentity = -1;

    static const int IDENTITY_ID_ROLE = Qt::UserRole;

    std::map<ID_T, Identity_T> identities;
    std::map<ID_T, QListWidgetItem*> identityItems;

    // response-ids
    std::map<ID_T, ID_T> deleteResponseIDs;

    void deleteFromUI(ID_T identid);

    QListWidgetItem *newIdentityItem = nullptr;
    void removeNewIdentityItemFromList();

private slots:

    // UI slots

    void on_pushButton_close_clicked();
    void on_pushButton_ident_add_clicked();

    void on_pushButton_ident_save_clicked();

    void on_pushButton_ident_delete_clicked();

    void on_listWidget_identities_itemClicked(QListWidgetItem *item);

};
