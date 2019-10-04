#include <QtCore/QDate>

#include "DateComponent.hxx"

using std::function;


DateComponent::DateComponent(
    QString const & title,
    QDate const & date,
    bool const isEditable,
    function<void(QDate const &)> onDateChanged
):
    isEditable(isEditable),
    title(title),
    label(new QLabel)
{
    addWidget(label);
    if (isEditable) {
        label->setText(title);
        dateEdit = new QDateEdit(date);
        dateEdit->setCalendarPopup(true);
        connect(dateEdit, &QDateEdit::dateChanged, onDateChanged);
        addWidget(dateEdit);
    }
    setDate(date);
}


void DateComponent::setDate(QDate date) {
    if (isEditable) {
        dateEdit->setDate(date);
    } else { // not editable
        if (date.isValid()) {
            label->setText(title + " " + date.toString());
        }
    }
}
