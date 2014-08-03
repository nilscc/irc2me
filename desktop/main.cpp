#include "form_connect.h"
#include <QApplication>
#include <QTranslator>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    // Load translation
    QTranslator translator;
    translator.load("lang");
    a.installTranslator(&translator);

    Irc2me irc2me;

    FormConnect w(irc2me);
    w.show();

    return a.exec();
}
