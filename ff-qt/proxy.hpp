#ifndef ff_qt_proxy_hpp
#define ff_qt_proxy_hpp


#include <string>


struct StorageHandle { void * ptr; };

struct NoteId { std::string bytes; };

struct Date { int year, month, day; };

// \todo(2019-02-10, cblp) generate with ron-schema
struct Note {
    NoteId id;
    std::string text;
    Date start;
    Date end;
};

class MainWindow;

extern "C" {
    void c_postpone(StorageHandle, const char * noteId);
}

struct Storage {
    StorageHandle handle;
    void postpone(NoteId id) const { c_postpone(handle, id.bytes.c_str()); }
};

int qApp_exec();

MainWindow * proxy_main(std::string version, StorageHandle);

void MainWindow_upsertTask(MainWindow *, Note);


#endif // ff_qt_proxy_hpp
