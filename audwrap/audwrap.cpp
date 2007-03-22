/*
(C) Copyright 2006 Mike Caron
Please read LICENSE.txt for GPL License details and disclaimer of liability
See README.txt for code docs. This code (unlike the main source) is clean and
elegant, so no appologies are necessary.
*/

//#include "stdafx.h"
//#include <stdlib.h>
/*
#ifdef WIN32
#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#include <windows.h>
#endif*/


#include "audwrap.h"
/*
#ifdef WIN32
BOOL APIENTRY DllMain( HANDLE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return TRUE;
}
#endif
*/

/*

// This is an example of an exported variable
AUDWRAP_API int naudwrap=0;

// This is an example of an exported function.
AUDWRAP_API int fnaudwrap(void)
{
	return 42;
}

*/

#include <assert.h>

#include "audiere.h"
using namespace audiere;

AudioDevicePtr device = 0;

OutputStreamPtr * sounds = 0; //yes, I know it's a pointer to a pointer.
int numSounds = 0;

int findFreeSlot(void);
inline bool isvalid(int s);

AUDWRAP_API int AudInit(void) {
	device = OpenDevice();
    //assert(false);
	if (!device) {
		return(-1);
	}

	numSounds = 10; //10 sounds by default, but can grow
	sounds = new OutputStreamPtr[numSounds];
    if (!sounds) {
        //device = 0;
        device->unref();
        device = 0;
        return(-1);
    }

	return(0);

}

AUDWRAP_API void AudClose(void) {
    for(int i = 0;i < numSounds; i++) sounds[i] = 0;
    delete [] sounds;
	device = 0;
}

AUDWRAP_API int AudLoadSound(const char *f, bool st) {
    int s = findFreeSlot();

    if(s < 0) return (-1);

    sounds[s] = OpenSound(device, f, st);

    if(!sounds[s]) return(-1);

    sounds[s]->setVolume(1.0f);

    return(s);
}

AUDWRAP_API void AudUnloadSound(int s) {
    if(!isvalid(s)) return;

    sounds[s] = 0;
}

AUDWRAP_API void AudSetVolume(int s, float v) {
    if(!isvalid(s)) return;

    sounds[s]->setVolume(v);
}

AUDWRAP_API float AudGetVolume(int s) {
    if(!isvalid(s)) return(0.0);

    return(sounds[s]->getVolume());
}

AUDWRAP_API void AudSetRepeat(int s, bool r) {
    if(!isvalid(s)) return;

    sounds[s]->setRepeat(r);
}

AUDWRAP_API bool AudGetRepeat(int s) {
    if(!isvalid(s)) return(false);

    return(sounds[s]->getRepeat());
}

AUDWRAP_API bool AudIsValidSound(int s) {
    return(isvalid(s));
}

AUDWRAP_API bool AudIsPlaying(int s) {
    if(!isvalid(s)) return(false);

    return(sounds[s]->isPlaying() == 1);
}

AUDWRAP_API void AudPlay(int s) {
    if(!isvalid(s)) return;

    if(sounds[s]->isPlaying()) return;

    sounds[s]->play();
}

AUDWRAP_API void AudStop(int s) {
    if(!isvalid(s)) return;

    if(sounds[s]->isPlaying()) sounds[s]->stop();
    sounds[s]->reset();
}

AUDWRAP_API void AudPause(int s) {
    if(!isvalid(s)) return;

    if(sounds[s]->isPlaying()) sounds[s]->stop();
}

int findFreeSlot(void) {
   	int i;
	for(i = 0; i < numSounds; i++) {
		if(sounds[i] == 0) {
		    return(i);
		}
	}

	int newMax = numSounds * 2;
	OutputStreamPtr * s = new OutputStreamPtr[newMax];
	if(s == 0) return(-1);

	for(i=0;i < numSounds; i++) {
	    s[i] = sounds[i];
	}

	delete [] sounds;
	sounds = s;
	numSounds = newMax;

	return(i);

}

inline bool isvalid(int s) {
    if(s < 0 || s >= numSounds) return(false);
    if(sounds[s] == 0) return(false);
    return(true);
}