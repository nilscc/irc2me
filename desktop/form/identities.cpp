#include "form/identities.h"
#include "ui_identities.h"

using namespace std;
using namespace Protobuf::Messages;

FormIdentities::FormIdentities(Irc2me &irc2me, QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::FormIdentities),
    irc2me(irc2me)
{
    ui->setupUi(this);

    resize(500, 300);

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

    // connect to irc2me
    connect(&irc2me, SIGNAL(identities(IdentityList_T)),
            this, SLOT(addIdentities(IdentityList_T)));

    connect(&irc2me, SIGNAL(response(ID_T,Protobuf::Messages::Server::ResponseCode,std::string)),
            this, SLOT(response(ID_T,Protobuf::Messages::Server::ResponseCode,std::string)));

    // request all identities
    irc2me.requestIdentities();

    // disable editing group
    ui->groupBox->setEnabled(false);
}

FormIdentities::~FormIdentities()
{
    delete ui;
}

/*
 * UI slots
 *
 */

void FormIdentities::on_pushButton_close_clicked()
{
    close();
}

void FormIdentities::on_pushButton_ident_add_clicked()
{
    irc2me.requestNewIdentity();
}

void FormIdentities::on_pushButton_ident_save_clicked()
{
    if (currentIdentity < 0)
        return;

    Identity_T ident;

    string nick     = ui->lineEdit_ident_nick->text().trimmed().toStdString();
    string username = ui->lineEdit_ident_username->text().trimmed().toStdString();
    string realname = ui->lineEdit_ident_realname->text().trimmed().toStdString();

    ident.set_id(currentIdentity);
    ident.set_nick(nick);
    ident.set_name(username);
    ident.set_realname(realname);

    // add list of alternative nicknames
    QStringList nick_alts = ui->lineEdit_ident_nick_alt->text().split(",", QString::SkipEmptyParts);
    auto lis = ident.mutable_nick_alt();
    for (QString &nick_alt : nick_alts)
        *lis->Add() = nick_alt.trimmed().toStdString();

    irc2me.setIdentities(vector<Identity_T>({ident}));
}

void FormIdentities::on_pushButton_ident_delete_clicked()
{
    if (currentIdentity < 0)
        return;

    ID_T response_id = irc2me.deleteIdentities(vector<ID_T>({currentIdentity}));

    deleteResponseIDs[response_id] = currentIdentity;
}

/*
 * Public functions
 *
 */

void FormIdentities::loadIdentityDetails(ID_T identid)
{
    if (identities.count(identid) == 0)
        return;

    setInputEnabled(true);

    const Identity_T &ident = identities[identid];

    QString nick     = QString::fromStdString( ident.nick() );

    QStringList nick_alts;
    for (const string &na : ident.nick_alt())
        nick_alts << QString::fromStdString(na);
    QString nick_alt = nick_alts.join(", ");

    QString username = QString::fromStdString( ident.name() );
    QString realname = QString::fromStdString( ident.realname() );

    ui->lineEdit_ident_nick    ->setText(nick);
    ui->lineEdit_ident_nick_alt->setText(nick_alt);
    ui->lineEdit_ident_username->setText(username);
    ui->lineEdit_ident_realname->setText(realname);

    ui->listWidget_identities->setCurrentItem(identityItems[identid]);
    ui->lineEdit_ident_nick->setFocus();

    currentIdentity = identid;
}

/*
 * Private functions
 *
 */

void FormIdentities::deleteFromUI(ID_T identid)
{
    if (identityItems.count(identid) != 1)
        return;

    // delete list item
    QListWidgetItem *item = identityItems[identid];

    // get row of itembefore deleting it
    int row = ui->listWidget_identities->row(item);

    delete item;

    // adjust row index
    row = max<int>(0, min<int>(ui->listWidget_identities->count()-1, row));

    // remove from map
    identityItems.erase(identid);

    // select first item or disable input
    QListWidgetItem *first = ui->listWidget_identities->item(row);
    if (first)
    {
        bool ok;
        ID_T newid = first->data(IDENTITY_ID_ROLE).toInt(&ok);
        if (ok)
            loadIdentityDetails(newid);
        else
            setInputEnabled(false);
    }
    else
    {
        setInputEnabled(false);
    }
}

/*
 * Private slots
 *
 */

void FormIdentities::response(ID_T id, Protobuf::Messages::Server::ResponseCode code, const std::string &msg)
{
    Q_UNUSED(msg); // TODO

    // check if response is to delete request
    if (deleteResponseIDs.count(id) == 1)
    {
        if (code == Server::ResponseOK)
        {
            deleteFromUI(deleteResponseIDs[id]);
        }
        deleteResponseIDs.erase(id);
    }
}

void FormIdentities::addIdentities(const IdentityList_T &idents)
{
    ID_T firstNewIdentity = -1;

    for (const Protobuf::Messages::Identity &ident : idents)
    {
        ID_T identid = ident.id();
        QString nick = QString::fromStdString(ident.nick());

        if (firstNewIdentity < 0)
            firstNewIdentity = identid;

        // add to identity map
        identities[identid] = ident;

        // check if new item
        if (identityItems.count(identid) == 0)
        {
            // create and add new item
            QListWidgetItem *item = new QListWidgetItem();
            ui->listWidget_identities->addItem(item);

            // set widget data
            item->setData(IDENTITY_ID_ROLE, QVariant(identid));

            // add to item list
            identityItems[identid] = item;
        }

        identityItems[identid]->setText(nick);
    }

    if (firstNewIdentity > 0)
        loadIdentityDetails(firstNewIdentity);
}

void FormIdentities::setInputEnabled(bool enabled)
{
    ui->groupBox->setEnabled(enabled);
}

void FormIdentities::on_listWidget_identities_itemClicked(QListWidgetItem *item)
{
    bool ok;
    ID_T identid = item->data(IDENTITY_ID_ROLE).toInt(&ok);
    if (!ok)
        return;

    loadIdentityDetails(identid);
}
