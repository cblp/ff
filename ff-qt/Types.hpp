#ifndef ff_qt_Types_hpp
#define ff_qt_Types_hpp


#include <QtCore>


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


#endif // ff_qt_Types_hpp
