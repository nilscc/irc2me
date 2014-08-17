#include "form/identities.h"
#include "ui_identities.h"

using namespace std;

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

    // request all identities
    irc2me.requestIdentities();

    // disable editing group
    ui->groupBox->setEnabled(false);
}

FormIdentities::~FormIdentities()
{
    delete ui;
}

void FormIdentities::on_pushButton_close_clicked()
{
    close();
}

void FormIdentities::on_pushButton_ident_add_clicked()
{

}

/*
 * Public functions
 *
 */

void FormIdentities::loadIdentityDetails(ID_T identid)
{
    if (identities.count(identid) == 0)
        return;

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

    ui->groupBox->setEnabled(true);
}

/*
 * Private slots
 *
 */

void FormIdentities::addIdentities(const IdentityList_T &idents)
{

    for (const Protobuf::Messages::Identity &ident : idents)
    {
        ID_T identid = ident.id();
        QString nick = QString::fromStdString(ident.nick());

        // add to identity map
        identities[identid] = ident;

        // check if new item
        if (identityItems.count(identid) == 0)
        {
            // create and add new item
            QListWidgetItem *item = new QListWidgetItem(ui->listWidget_identities);
            ui->listWidget_identities->addItem(item);

            // set widget data
            item->setData(IDENTITY_ID_ROLE, identid);

            // add to item list
            identityItems[identid] = item;
        }

        identityItems[identid]->setText(nick);
    }
}

void FormIdentities::on_listWidget_identities_itemActivated(QListWidgetItem *item)
{
    bool ok;
    ID_T identid = item->data(IDENTITY_ID_ROLE).toInt(&ok);
    if (!ok)
        return;

    loadIdentityDetails(identid);
}
