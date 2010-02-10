/*
(C) Copyright 2006 Mike Caron
Please read LICENSE.txt for GPL License details and disclaimer of liability
See README.txt for code docs. This code (unlike the main source) is clean and
elegant, so no appologies are necessary.
*/

#include <stdio.h>
#include "audwrap.h"
using namespace audiere;

AudioDevicePtr device = 0;

OutputStreamPtr * sounds = 0; //yes, I know it's a pointer to a pointer.
int numSounds = 0;

//private functions
int findFreeSlot(void);
inline bool isvalid(int s);

//Initializes Audiere, returning a pointer
AUDWRAP_API int AudInit(void) {
    //open Audiere
	device = OpenDevice();
    
    //OpenDevice() returns 0 on failure
	if (!device) {
		return(-1);
	}

    //this should've been an optional parameter, but meh
    
	numSounds = 10; //10 sounds by default, but can grow
	sounds = new OutputStreamPtr[numSounds];
    if (!sounds) {
        device = 0;
        return(-1);
    }

	return(0);

}

//terminates Audiere
AUDWRAP_API void AudClose(void) {
    for(int i = 0;i < numSounds; i++) sounds[i] = 0;
    if(sounds) delete [] sounds;
	device = 0;
}


//Loads a sound into the internal buffer, and returns an index to it
//st is whether or not to stream the sound (MP3, OGG should be streamed, WAV probably not, etc)

//f is the file name
AUDWRAP_API int AudLoadSound(const char *f, bool st) {
    if (!device) return -1; 

    int s = findFreeSlot();

    if(s < 0) return (-1);

    sounds[s] = OpenSound(device, f, st);

    if(!sounds[s]) return(-1);

    sounds[s]->setVolume(1.0f);

    return(s);
}

AUDWRAP_API int AudLoadSoundLump(Lump *lump, bool st) {
    if (!device) return -1; 

    int s = findFreeSlot();

    if(s < 0) return (-1);

    sounds[s] = OpenSound(device, FilePtr(new LumpFile(lump)), st);

    if(!sounds[s]) return(-1);

    sounds[s]->setVolume(1.0f);

    return(s);
}

//Unloads a file from the buffer, and frees its slot
AUDWRAP_API void AudUnloadSound(int s) {
    if(!isvalid(s)) return;

    sounds[s] = 0;
}

//Sets the volume on a given sound.
AUDWRAP_API void AudSetVolume(int s, float v) {
    if(!isvalid(s)) return;

    sounds[s]->setVolume(v);
}

//Gets the volume on a given sound
AUDWRAP_API float AudGetVolume(int s) {
    if(!isvalid(s)) return(0.0);

    return(sounds[s]->getVolume());
}

//Toggles repeating on a given sound
AUDWRAP_API void AudSetRepeat(int s, bool r) {
    if(!isvalid(s)) return;

    sounds[s]->setRepeat(r);
}

//Reads repeat setting on a given sound
AUDWRAP_API bool AudGetRepeat(int s) {
    if(!isvalid(s)) return(false);

    return(sounds[s]->getRepeat());
}

//Returns whether the index provided is valid
AUDWRAP_API bool AudIsValidSound(int s) {
    return(isvalid(s));
}

//Returns whether the index provided is playing
AUDWRAP_API bool AudIsPlaying(int s) {
    if(!isvalid(s)) return(false);

    return(sounds[s]->isPlaying() == 1);
}

//Causes the given sound to play
AUDWRAP_API void AudPlay(int s) {
    if(!isvalid(s)) return;
    
    //no sense in re-playing an already playing sound
    if(sounds[s]->isPlaying()) return;

    sounds[s]->play();
}

//Stops a sound
AUDWRAP_API void AudStop(int s) {
    if(!isvalid(s)) return;

    //again, no sense in stopping an already stopped sound
    if(sounds[s]->isPlaying()) sounds[s]->stop();
    
    //Shuffles the position back to the start, as opposed to AudPause
    sounds[s]->reset();
}

//Stops the sound, but doesn't reset its cursor
AUDWRAP_API void AudPause(int s) {
    if(!isvalid(s)) return;

    if(sounds[s]->isPlaying()) sounds[s]->stop();
}

//interates the slots until it finds a sound. If there isn't any room, it grows the array
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

//checks the index in a couple of ways to determine validity
inline bool isvalid(int s) {
    if(s < 0 || s >= numSounds) return(false);
    if(sounds[s] == 0) return(false);
    return(true);
}

//LumpFile wrapper around the FileWrapper wrapper around the Lump wrapper

LumpFile::LumpFile(Lump *lump) {
    wrapper = FileWrapper_open(lump);
    length = FileWrapper_seek(*wrapper, 0, SEEK_END);
    //FileWrapper_seek(*wrapper, 0, SEEK_SET);
}

LumpFile::~LumpFile() {
    FileWrapper_close(*wrapper);
}

int LumpFile::read(void *buffer, int size) {
    return FileWrapper_read(*wrapper, buffer, 1, size);
}

bool LumpFile::seek(int position, SeekMode mode) {
    int initpos;
    switch (mode) {
    case CURRENT:
        initpos = tell();
        return FileWrapper_seek(*wrapper, position, SEEK_CUR) - initpos == position;
    case BEGIN:
        return FileWrapper_seek(*wrapper, position, SEEK_SET) == position;
    case END:
        return FileWrapper_seek(*wrapper, position, SEEK_END) - length == position;
    }
    return false;
}

int LumpFile::tell() {
    return FileWrapper_seek(*wrapper, SEEK_CUR, 0);
}
