#ifndef LUMPFILE_H
#define LUMPFILE_H

// NOTE: much of the declarations in lumpfile.bi are in filelayer.hpp on the C side

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Lump Lump;
typedef struct FileWrapper FileWrapper;

FileWrapper* FileWrapper_open(Lump* lump);
void FileWrapper_close(FileWrapper*);
int FileWrapper_seek(FileWrapper*, int offset, int whence);
int FileWrapper_read(FileWrapper*, void* buffer, int size, int maxnum);

void log_openfile(const char *filename);
boolint read_recent_files_list(int idx, const char **filename, double *opentime);

#ifdef __cplusplus
}
#endif

#endif
