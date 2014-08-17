#ifndef FORM_NETWORKS_H
#define FORM_NETWORKS_H

#include <QMainWindow>

namespace Ui {
class FormNetworks;
}

class FormNetworks : public QMainWindow
{
    Q_OBJECT

public:

    explicit FormNetworks(QWidget *parent = 0);
    ~FormNetworks();

private slots:

    void on_pushButton_cancel_clicked();

private:

    Ui::FormNetworks *ui;

    void reset();
};

#endif // FORM_NETWORKS_H
