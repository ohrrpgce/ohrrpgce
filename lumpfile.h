/* OHRRPGCE - Lumped file format routines
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 *
 * NOTE: many of the declarations in lumpfile.bi are in filelayer.hpp on the C side
 *       In fact most of these declarations aren't even used.
 */

#ifndef LUMPFILE_H
#define LUMPFILE_H


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
