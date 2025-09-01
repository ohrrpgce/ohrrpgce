// (C) Copyright 2006-2017 Mike Caron and the OHRRPGCE Developers
// Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

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
#include "../lumpfile.h"

class LumpFile : public audiere::RefImplementation<audiere::File> {
public:
    LumpFile(Lump *lump);
    ~LumpFile();
    int ADR_CALL read(void *buffer, int size);
    bool ADR_CALL seek(int position, SeekMode mode);
    int ADR_CALL tell();

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
AUDWRAP_API bool AudIsSeekable(int s);
AUDWRAP_API double AudGetLength(int s);
AUDWRAP_API void AudSetPosition(int s, double position);
AUDWRAP_API double AudGetPosition(int s);
}
#endif
