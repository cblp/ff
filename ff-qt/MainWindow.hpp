#ifndef ff_qt_MainWindow_hpp
#define ff_qt_MainWindow_hpp


#include <map>
    using std::map;
#include <string>
    using std::string;
#include <vector>
    using std::vector;
#include <experimental/optional>
    using std::experimental::optional;
#include <QtWidgets>


struct StorageHandle { void * ptr; };

struct NoteId { string str; };

// \todo(2019-02-10, cblp) generate with ron-schema
struct Note {
    NoteId id;
    QString text;
    QDate start;
    optional<QDate> end;
};

extern "C" {
    void ff_postpone(StorageHandle, const char *);
}


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

    TaskActionsButton (StorageHandle storage, NoteId id) {
        setText("⋮");
        setPopupMode(InstantPopup);
        {
            auto menu = new QMenu;
            menu->addAction("Postpone", [=]{
                ff_postpone(storage, id.str.c_str());
            });
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
            if (task.end)
                fields->addLayout(new DateComponent("Deadline:", *task.end));
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
    }

    void addTask(Note task) {
        auto item = new QStandardItem;
        model().appendRow(item);
        auto wrap = new QWidget(this);
        {
            // trick: a transparent widget around an opaque one
            auto wrapBox = new QVBoxLayout(wrap);
            auto taskWidget = new TaskWidget(wrap, storage, task);
            taskWidget->setAutoFillBackground(true);
            wrapBox->addWidget(taskWidget);
        }
        setIndexWidget(item->index(), wrap);
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

        restoreState(settings.value("mainWindowState").toByteArray());
    }

    void addTask(Note note) { agenda->addTask(note); }

    void closeEvent(QCloseEvent * event) override {
        // https://wiki.qt.io/Saving_Window_Size_State
        QSettings settings;
        settings.setValue("mainWindowGeometry", saveGeometry());
        settings.setValue("mainWindowState", saveState());
    }
};


#endif // ff_qt_MainWindow_hpp
