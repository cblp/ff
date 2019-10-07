#include <QtCore/QProcess>
#include <QtCore/QUrl>
#include <QtGui/QClipboard>
#include <QtGui/QDesktopServices>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>

#include "LinkButton.hxx"


LinkButton::LinkButton(QString text, QString url): super(text) {
    setCursor(Qt::PointingHandCursor);
    setFont([this]{
        auto f = font();
        f.setBold(false);
        f.setUnderline(true);
        return f;
    }());
    setIcon(QIcon());
    setPalette([this]{
        auto p = palette();
        p.setColor(QPalette::ButtonText, palette().color(QPalette::Link));
        return p;
    }());
    setToolTip(url);
    connect(this, &self::clicked, [&]{
        /// \todo(2019-02-23, cblp, https://github.com/ff-notes/ff/issues/135,
        /// browser@support.yandex.ru [Ticket#19022310562166345])
        /// Yandex.Browser on macOS doesn't open a tab via
        /// QDesktopServices::openUrl called from Haskell;
        /// other browser, or other OS, or haskell-less app work well
        #ifdef __APPLE__
            QProcess::execute("open", {url});
        #else
            QDesktopServices::openUrl(QUrl(url));
        #endif
    });

    // context menu
    setContextMenuPolicy(Qt::ActionsContextMenu);
    addAction([&]{
        auto action = new QAction("Copy link address");
        connect(
            action,
            &QAction::triggered,
            [&]{ qApp->clipboard()->setText(url); }
        );
        return action;
    }());
}
