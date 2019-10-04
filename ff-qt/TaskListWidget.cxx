#include "TaskListWidget.hxx"
#include "TaskWidget.hxx"


const QColor LightBlue(179, 215, 255);


TaskListWidget::TaskListWidget(QWidget * parent, StorageHandle storage):
    super(parent)
{
    setAlternatingRowColors(true);
    setHeaderHidden(true);
    setModel(new Model);
    setPalette([this]{
        auto p = palette();
        p.setColor(QPalette::Highlight, LightBlue);
        return p;
    }());
    connect(
        &model(),
        &Model::rowsInserted,
        [this, storage](const QModelIndex & parent, int first, int last){
            for (int i = first; i <= last; ++i) {
                auto ix = model().index(i, 0, parent);
                setIndexWidget(
                    ix, new TaskWidget(this, storage, model().task(ix))
                );
                if (not currentIndex().isValid()) {
                    setCurrentIndex(ix);
                }
            }
            updateGeometries();
        }
    );
    connect(
        &model(),
        &Model::dataChanged,
        [this](QModelIndex topLeft, QModelIndex bottomRight){
            for (int i = topLeft.row(); i <= bottomRight.row(); ++i) {
                auto ix = topLeft.siblingAtRow(i);
                static_cast<TaskWidget*>(indexWidget(ix))
                    ->updateContent(model().task(ix));
            }
        }
    );
}


void TaskListWidget::upsertTask(Note task) {
    model().upsertTask(task);
}


Model & TaskListWidget::model() const {
    return static_cast<Model &>(*super::model());
}
