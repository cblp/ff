#include <QtWidgets/QMenu>
#include <QtWidgets/QToolButton>

#include "TaskActionsBar.hxx"


TaskActionsBar::TaskActionsBar(StorageHandle storageHandle, Note task) {
    auto storage = Storage{storageHandle};
    auto id = task.id;

    auto doneAction =
        addAction("Done and archive", [&]{ storage.done(id); });
    if (task.isTracking) {
        doneAction->setEnabled(false);
        doneAction->setToolTip("External task must be done first.");
    }

    addWidget([&]{
        auto btn = new QToolButton;
        btn->setMenu([&]{
            auto menu = new QMenu;
            menu->addAction("Postpone", [&]{ storage.postpone(id); });
            return menu;
        }());
        btn->setPopupMode(QToolButton::InstantPopup);
        btn->setText("▼");
        return btn;
    }());
}
