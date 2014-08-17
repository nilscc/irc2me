#pragma once

#include <QMainWindow>

namespace Ui {
class FormIdentities;
}

class FormIdentities : public QMainWindow
{
    Q_OBJECT

public:
    explicit FormIdentities(QWidget *parent = 0);
    ~FormIdentities();

private slots:
    void on_pushButton_close_clicked();

private:
    Ui::FormIdentities *ui;
};
