#include "form_connect.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    Irc2me irc2me;

    FormConnect w(irc2me);
    w.show();

    return a.exec();
}
