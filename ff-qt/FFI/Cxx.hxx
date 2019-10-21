#ifndef ff_qt_FFI_Cxx_hxx
#define ff_qt_FFI_Cxx_hxx


#include <functional>
#include <string>


struct StorageHandle { void * ptr; };

typedef std::string NoteId;

struct Date { int year, month, day; };

// \todo(2019-02-10, https://github.com/ff-notes/ron/issues/117, cblp) generate
// with ron-schema
struct Note {
    NoteId id;
    bool isActive;
    std::string text;
    Date start;
    Date end;
    bool isTracking;
    struct Track {
        std::string provider;
        std::string source;
        std::string externalId;
        std::string url;
    } track;
};

class MainWindow;

extern "C" {
    void hs_assignStart(
        StorageHandle, const char * noteId, int year, int month, int day
    );
    void hs_assignEnd(
        StorageHandle, const char * noteId, int year, int month, int day
    );
    void hs_done    (StorageHandle, const char * noteId);
    void hs_postpone(StorageHandle, const char * noteId);
}

struct Storage {
    StorageHandle handle;

    void assignStart(NoteId id, Date d) const {
        hs_assignStart(handle, id.c_str(), d.year, d.month, d.day);
    }

    void assignEnd(NoteId id, Date d) const {
        hs_assignEnd(handle, id.c_str(), d.year, d.month, d.day);
    }

    void done    (NoteId id) const { hs_done    (handle, id.c_str()); }
    void postpone(NoteId id) const { hs_postpone(handle, id.c_str()); }
};


#endif // ff_qt_FFI_Cxx_hxx
