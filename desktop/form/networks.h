#pragma once

#include "form/identities.h"
#include "irc2me.h"

#include <QMainWindow>

namespace Ui {
class FormNetworks;
}

class FormNetworks : public QMainWindow
{
    Q_OBJECT

public:

    explicit FormNetworks(Irc2me &irc2me, FormIdentities *f_ident, QWidget *parent = 0);
    ~FormNetworks();

    /*
     * Identities
     *
     */

public:

    void setIdentityMap(const std::map<ID_T, Identity_T> &idents);

    void addIdentity(const Identity_T &identity);

    void sortIdentities();

private slots:

    void identitySelected(int row);

private:

    std::map<ID_T, std::pair<int, Identity_T>> identities;

    /*
     * Other
     *
     */

private slots:

    void on_pushButton_close_clicked();

    void on_pushButton_network_add_clicked();

    void on_toolButton_idents_manage_clicked();

private:

    Ui::FormNetworks *ui;

    Irc2me &irc2me;

    // Other forms
    FormIdentities *formIdentities = nullptr;

    void reset();
};
