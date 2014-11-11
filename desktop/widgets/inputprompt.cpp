#include "widgets/inputprompt.h"

#include <QPainter>
#include <QDebug>
#include <QKeyEvent>
#include <QLabel>
#include <QStyleOption>
#include <QApplication>
#include <QLineEdit>
#include <QFile>

#include <QHBoxLayout>

using namespace std;

InputPrompt::InputPrompt(QWidget *parent) :
    QWidget(parent)

{
    buildUI();

    setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    setFixedHeight(sizeHint().height());

    setFont(QFont("DejaVu Sans Mono", 9));

    loadStylesheet(":/widgets/inputprompt.css");
}

void InputPrompt::buildUI()
{
    // setup prompt label

    _prompt = new QLabel("λ");
    _prompt->setObjectName("prompt");

    // setup input field

    _lineedit = new QLineEdit();
    _lineedit->setObjectName("input");

    connect(_lineedit, &QLineEdit::returnPressed,
            this, &InputPrompt::lineEditReturnPressed);

    // setup layout

    QHBoxLayout *layout = new QHBoxLayout();
    layout->setObjectName("layout");

    layout->addWidget(_prompt);
    layout->addWidget(_lineedit);

    layout->setContentsMargins(5, 2, 5, 2);

    setLayout(layout);
}

/*
 * Custom functions
 *
 */

void InputPrompt::setPrompt(QString prompt)
{
    _prompt->setText(prompt);
    if (prompt.isEmpty())
        _prompt->hide();
    else
        _prompt->show();
}

void InputPrompt::lineEditReturnPressed()
{
    emit userInput(_lineedit->text());
    _lineedit->clear();
}

void InputPrompt::loadStylesheet(QString ressource)
{
    QFile file(ressource);
    file.open(QIODevice::ReadOnly);
    if (file.isOpen())
        setStyleSheet(file.readAll());
    else
        qWarning("Could not open stylesheet ressource \"%s\"\n", ressource.toLatin1().constData());
}

/*
 * Reimplemented functions
 *
 */

void InputPrompt::paintEvent(QPaintEvent *e)
{
    Q_UNUSED(e);

    // draw widget for stylesheet
    QStyleOption opt;
    opt.init(this);
    QPainter p(this);
    style()->drawPrimitive(QStyle::PE_Widget, &opt, &p, this);
}

QFont InputPrompt::font() const
{
    return _prompt->font();
}

void InputPrompt::setFont(const QFont &font)
{
    _prompt->setFont(font);
    _lineedit->setFont(font);
}

void InputPrompt::clear()
{
    _lineedit->clear();
}

QString InputPrompt::text() const
{
    return _lineedit->text();
}

void InputPrompt::setText(QString t)
{
    _lineedit->setText(t);
}
