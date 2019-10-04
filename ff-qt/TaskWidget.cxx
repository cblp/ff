#include <QtGui/QClipboard>
#include <QtWidgets/QApplication>

#include "DateComponent.hxx"
#include "FFI/Cxx.hxx"
#include "LinkButton.hxx"
#include "TaskActionsBar.hxx"
#include "TaskWidget.hxx"
#include "util.hxx"


TaskWidget::TaskWidget(QWidget * parent, StorageHandle storage, Note task):
    TaskWidget(parent, storage, task, QString::fromStdString(task.text))
{}


TaskWidget::TaskWidget(
    QWidget * parent,
    StorageHandle storageHandle,
    Note task,
    const QString & text
):
    super(parent),
    label(new QLabel(text))
{
    auto storage = Storage{storageHandle};

    label->setWordWrap(true);

    start = new DateComponent(
        "Start:",
        toQDate(task.start),
        not task.isTracking,
        [task, storage](QDate const & date){
            storage.assignStart(task.id, fromQDate(date));
        }
    );
    end = new DateComponent(
        "Deadline:",
        toQDate(task.end),
        not task.isTracking,
        [task, storage](QDate const & date){
            storage.assignEnd(task.id, fromQDate(date));
        }
    );

    auto box = new QVBoxLayout(this);
    box->addWidget(label);
    box->addLayout([&]{
        auto box = new QHBoxLayout;
        box->addLayout(start);
        box->addLayout(end);
        box->addWidget(new TaskActionsBar(storageHandle, task));
        box->addStretch();
        return box;
    }());
    if (task.isTracking) {
        box->addWidget(new LinkButton(
            QString::fromStdString(
                task.track.provider + ": " + task.track.source + " #"
                + task.track.externalId
            ),
            QString::fromStdString(task.track.url)
        ));
    }

    // context menu
    setContextMenuPolicy(Qt::ActionsContextMenu);
    addAction([&]{
        auto action = new QAction("Copy text");
        connect(
            action,
            &QAction::triggered,
            [&]{ qApp->clipboard()->setText(text); }
        );
        return action;
    }());
}


void TaskWidget::updateContent(Note task) {
    label->setText(QString::fromStdString(task.text));
    start->setDate(toQDate(task.start));
    end  ->setDate(toQDate(task.end  ));
}
