// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the AUDWRAP_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// AUDWRAP_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifndef __AUDWRAP_H__
#define __AUDWRAP_H__


#ifdef AUDWRAP_EXPORTS
#define AUDWRAP_API __declspec(dllexport)
#else
#define AUDWRAP_API __declspec(dllimport)
#endif

extern "C" {

//extern AUDWRAP_API int naudwrap;

//AUDWRAP_API int fnaudwrap(void);

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