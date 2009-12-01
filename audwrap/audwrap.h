/*
(C) Copyright 2006 Mike Caron
Please read LICENSE.txt for GPL License details and disclaimer of liability
See README.txt for code docs. This code (unlike the main source) is clean and
elegant, so no appologies are necessary.
*/

#ifndef __AUDWRAP_H__
#define __AUDWRAP_H__

#ifdef WIN32_DLL
#ifdef AUDWRAP_EXPORTS
#define AUDWRAP_API __declspec(dllexport)
#else
#define AUDWRAP_API __declspec(dllimport)
#endif
#endif

#ifndef AUDWRAP_API
#define AUDWRAP_API 
#endif

#ifndef __cplusplus
#error Audwrap requires C++!
#endif

#include "audiere.h"

extern "C" {

struct Lump;
struct FileWrapper;

FileWrapper *FileWrapper_open(Lump *lump);
void FileWrapper_close(FileWrapper&);
int FileWrapper_seek(FileWrapper&, int offset, int whence);
int FileWrapper_read(FileWrapper&, void *buffer, int size, int maxnum);

}

class LumpFile : public audiere::RefImplementation<audiere::File> {
public:
    LumpFile(Lump *lump);
    ~LumpFile();
    int read(void *buffer, int size);
    bool seek(int position, SeekMode mode);
    int tell();

private:
    FileWrapper *wrapper;
    int length;
};

extern "C" {

AUDWRAP_API int AudInit(void);
AUDWRAP_API void AudClose(void);
AUDWRAP_API int AudLoadSound(const char *f, bool st);
AUDWRAP_API int AudLoadSoundLump(Lump *lump, bool st);
AUDWRAP_API void AudUnloadSound(int s);
AUDWRAP_API void AudSetVolume(int s, float v);
AUDWRAP_API float AudGetVolume(int s);
AUDWRAP_API void AudSetRepeat(int s, bool r);
AUDWRAP_API bool AudGetRepeat(int s);
AUDWRAP_API bool AudIsValidSound(int s);
AUDWRAP_API bool AudIsPlaying(int s);
AUDWRAP_API void AudPlay(int s);
AUDWRAP_API void AudStop(int s);
AUDWRAP_API void AudPause(int s);
}
#endif
