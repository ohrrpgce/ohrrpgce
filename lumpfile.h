#ifndef LUMPFILE_H
#define LUMPFILE_H

#ifdef __cplusplus
extern "C" {
#endif

struct Lump;
struct FileWrapper;

FileWrapper* FileWrapper_open(Lump* lump);
void FileWrapper_close(FileWrapper*);
int FileWrapper_seek(FileWrapper*, int offset, int whence);
int FileWrapper_read(FileWrapper*, void* buffer, int size, int maxnum);

void log_openfile(const char *filename);

#ifdef __cplusplus
}
#endif

#endif
