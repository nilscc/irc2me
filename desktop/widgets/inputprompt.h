#pragma once

#include <QTextEdit>
#include <QWidget>
#include <QSize>
#include <QLabel>
#include <QLineEdit>
#include <QString>

class InputPrompt : public QWidget
{
    Q_OBJECT

public:

    explicit InputPrompt(QWidget *parent = 0);

    void setPrompt(QString);

    QFont font() const;
    void setFont(const QFont &font);

    QString text() const;
    void setText(QString);

    void clear();

    void loadStylesheet(QString ressource);

    void paintEvent(QPaintEvent *);

private:

    void buildUI();

    QLabel *_prompt;
    QLineEdit *_lineedit;

    void lineEditReturnPressed();

signals:

    void userInput(QString);

public slots:

};
