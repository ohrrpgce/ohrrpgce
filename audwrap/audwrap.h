/*
(C) Copyright 2006 Mike Caron
Please read LICENSE.txt for GPL License details and disclaimer of liability
See README.txt for code docs. This code (unlike the main source) is clean and
elegant, so no appologies are necessary.
*/

#ifndef __AUDWRAP_H__
#define __AUDWRAP_H__

#ifdef WIN32
#ifdef AUDWRAP_EXPORTS
#define AUDWRAP_API __declspec(dllexport)
#else
#define AUDWRAP_API __declspec(dllimport)
#endif
#else
#define AUDWRAP_API 
#endif

#ifndef __cplusplus
#error Audwrap requires C++!
#endif

extern "C" {

AUDWRAP_API int AudInit(void);
AUDWRAP_API void AudClose(void);
AUDWRAP_API int AudLoadSound(const char *f, bool st);
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
