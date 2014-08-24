#include "form/networks.h"
#include "ui_networks.h"

using namespace std;

FormNetworks::FormNetworks(Irc2me &irc2me, FormIdentities *f_ident, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormNetworks),
    irc2me(irc2me),
    formIdentities(f_ident)
{
    ui->setupUi(this);

    // connect to identities

    connect(formIdentities, &FormIdentities::identitiesChanged,
            this,           &FormNetworks::setIdentityMap);

    // connect UI

    connect(ui->comboBox_idents, SIGNAL(currentIndexChanged(int)),
            this,                SLOT(identitySelected(int)));

    // set size

    resize(700, 400);

    // setup splitter layout

    ui->splitter->setStretchFactor(0, 0);
    ui->splitter->setStretchFactor(1, 1);
    ui->splitter->setSizes(QList<int>() << 150 << 1);
    ui->splitter->setCollapsible(0, false);
    ui->splitter->setCollapsible(1, false);

    // show only title + close button

    setWindowFlags(Qt::FramelessWindowHint);
    setWindowFlags(Qt::WindowTitleHint);
    setWindowFlags(Qt::WindowCloseButtonHint);

    // show correct tab

    ui->tabWidget->setCurrentIndex(0);
}

FormNetworks::~FormNetworks()
{
    delete ui;
}

/*
 * Identities
 *
 */

void FormNetworks::setIdentityMap(const std::map<ID_T, Identity_T> &idents)
{
    bool ok;
    ID_T current = ui->comboBox_idents->itemData(
                ui->comboBox_idents->currentIndex()
                ).toLongLong(&ok);

    ui->comboBox_idents->clear();

    identities.clear();

    for (auto &pair : idents)
        addIdentity(pair.second);

    if (ok && identities.count(current) == 1)
        ui->comboBox_idents->setCurrentIndex(identities[current].first);
}

void FormNetworks::addIdentity(const Identity_T &identity)
{
    // add to UI

    QString ident_str = QString::fromStdString(identity.nick());

    if (identities.count(identity.id()) == 0)
    {
        int row = ui->comboBox_idents->count();

        // remember
        identities[identity.id()] = make_pair(row, identity);

        // insert new item to combo box
        ui->comboBox_idents->insertItem(row, ident_str, identity.id());
    }
    else
    {
        // update existing item
        int row = identities[identity.id()].first;
        ui->comboBox_idents->setItemText(row, ident_str);
        ui->comboBox_idents->setItemData(row, identity.id());
    }
}

void FormNetworks::sortIdentities()
{
    ui->comboBox_idents->model()->sort(0);
}

void FormNetworks::identitySelected(int row)
{
    // get identity ID from combobox item
    bool ok;
    ID_T id = ui->comboBox_idents->itemData(row).toLongLong(&ok);

    if (!ok || identities.count(id) != 1)
        return;

    Identity_T ident = identities[id].second;

    QStringList nick_alt;
    for (const string &n : ident.nick_alt())
        nick_alt.push_back(QString::fromStdString(n));

    ui->label_ident_nick->setText(QString::fromStdString(ident.nick()));
    ui->label_ident_nick_alt->setText(nick_alt.join(", "));
    ui->label_ident_user->setText(QString::fromStdString(ident.name()));
    ui->label_ident_real->setText(QString::fromStdString(ident.realname()));
}

/*
 * private functions
 *
 */

void FormNetworks::reset()
{

}

/*
 * UI slots
 *
 */

void FormNetworks::on_pushButton_close_clicked()
{
    reset();
    close();
}

void FormNetworks::on_pushButton_network_add_clicked()
{
    ui->comboBox_idents->clear();
    identities.clear();
}

void FormNetworks::on_toolButton_idents_manage_clicked()
{
    formIdentities->show();
}
