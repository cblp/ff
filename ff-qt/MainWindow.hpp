#ifndef ff_qt_MainWindow_hpp
#define ff_qt_MainWindow_hpp


#include <QtWidgets>


struct StorageHandle { void * ptr; };

struct NoteId { QByteArray bytes; };

// \todo(2019-02-10, cblp) generate with ron-schema
struct Note {
    NoteId id;
    QString text;
    QDate start;
    QDate end; // isNull() if the end is not set
};

extern "C" {
    void c_postpone(StorageHandle, const char * noteId);
}

struct Storage {
    StorageHandle handle;
    void postpone(NoteId id) const { c_postpone(handle, id.bytes); }
};


class DateComponent: public QHBoxLayout {
    using super = QHBoxLayout;

public:

    DateComponent(QString label, QDate date) {
        addWidget(new QLabel(label));
        {
            auto dateEdit = new QDateEdit(date);
            dateEdit->setCalendarPopup(true);
            dateEdit->setReadOnly(true);
            addWidget(dateEdit);
        }
    }
};


class TaskActionsButton: public QToolButton {
    using super = QToolButton;

public:

    TaskActionsButton (StorageHandle storageHandle, NoteId id) {
        auto storage = Storage{storageHandle};
        setText("⋮");
        setPopupMode(InstantPopup);
        {
            auto menu = new QMenu;
            menu->addAction("Postpone", [=]{ storage.postpone(id); });
            setMenu(menu);
        }
    }

};


class TaskWidget: public QFrame {
    using super = QFrame;

private:

    QLabel * label;

public:

    TaskWidget(QWidget * parent, StorageHandle storage, Note task):
        super(parent), label(new QLabel(task.text))
    {
        auto box = new QVBoxLayout(this);
        box->addWidget(label);
        {
            auto fields = new QHBoxLayout;
            fields->addLayout(new DateComponent("Start:", task.start));
            fields->addLayout(new DateComponent("Deadline:", task.end));
            fields->addWidget(new TaskActionsButton(storage, task.id));
            fields->addStretch();
            box->addLayout(fields);
        }
    }

};


class TaskListWidget: public QTreeView {
    using super = QTreeView;

private:

    StorageHandle storage;

public:

    TaskListWidget(QWidget * parent, StorageHandle storage):
        super(parent), storage(storage)
    {
        setAlternatingRowColors(true);
        setHeaderHidden(true);
        setModel(new QStandardItemModel);

        QPalette p = palette();
        p.setColor(
            QPalette::Highlight, p.color(QPalette::Highlight).lighter(200)
        );
        setPalette(p);
    }

    void upsertTask(Note task) {
        auto item = new QStandardItem;
        model().appendRow(item);
        auto taskWidget = new TaskWidget(this, storage, task);
        setIndexWidget(item->index(), taskWidget);
    }

    QStandardItemModel & model() const {
        return static_cast<QStandardItemModel &>(*super::model());
    }

};


class MainWindow: public QMainWindow {

private:

    TaskListWidget * agenda;

public:

    MainWindow(StorageHandle storage):
        agenda(new TaskListWidget(this, storage))
    {
        // https://wiki.qt.io/Saving_Window_Size_State
        QSettings settings;
        restoreGeometry(settings.value("mainWindowGeometry").toByteArray());

        auto tabs = new QTabWidget;
        tabs->addTab(agenda, "Agenda");
        setCentralWidget(tabs);
        setWindowTitle("ff");
        agenda->setFocus();

        restoreState(settings.value("mainWindowState").toByteArray());
    }

    void upsertTask(Note note) { agenda->upsertTask(note); }

    void closeEvent(QCloseEvent * event) override {
        // https://wiki.qt.io/Saving_Window_Size_State
        QSettings settings;
        settings.setValue("mainWindowGeometry", saveGeometry());
        settings.setValue("mainWindowState", saveState());
    }
};


#endif // ff_qt_MainWindow_hpp
