/*
(C) Copyright 2006 Mike Caron
Please read LICENSE.txt for GPL License details and disclaimer of liability
See README.txt for code docs. This code (unlike the main source) is clean and
elegant, so no appologies are necessary.
*/

#include <stdio.h>
#include <string>
#include "../errorlog.h"
#include "audwrap.h"
using namespace audiere;

AudioDevicePtr device = 0;

OutputStreamPtr *sounds = 0;  // Array of streams
int numSounds = 0;

//private functions
int findFreeSlot();
inline bool isvalid(int slot);

//Initializes Audiere, returning 0 on success
AUDWRAP_API int AudInit() {
    //open Audiere
    device = OpenDevice();
    
    //OpenDevice() returns 0 on failure
    if (!device) {
        return -1;
    }
    
    numSounds = 10; //10 sounds by default, but can grow
    sounds = new OutputStreamPtr[numSounds];
    if (!sounds) {
        device = 0;
        return -1;
    }

    std::string formats = "Audiere: supported formats ";
    formats += hidden::AdrGetSupportedFileFormats();
    debug(errInfo, formats.c_str());

    return 0;
}

//terminates Audiere
AUDWRAP_API void AudClose() {
    for (int i = 0; i < numSounds; i++)
        sounds[i] = 0;
    if (sounds)
        delete [] sounds;
    device = 0;
}


//Loads a sound into the internal buffer, and returns an index to it
//streaming: MP3, OGG should be streamed, WAV probably not, etc

AUDWRAP_API int AudLoadSound(const char *filename, bool streaming) {
    if (!device) return -1; 

    int slot = findFreeSlot();

    if (slot < 0) return (-1);

    sounds[slot] = OpenSound(device, filename, streaming);

    if (!sounds[slot]) return -1;

    sounds[slot]->setVolume(1.0f);

    return slot;
}

AUDWRAP_API int AudLoadSoundLump(Lump *lump, bool streaming) {
    if (!device) return -1; 

    int slot = findFreeSlot();
    if (slot < 0) return (-1);

    sounds[slot] = OpenSound(device, FilePtr(new LumpFile(lump)), streaming);
    if (!sounds[slot]) return -1;

    sounds[slot]->setVolume(1.0f);

    return slot;
}

//Unloads a file from the buffer, and frees its slot
AUDWRAP_API void AudUnloadSound(int slot) {
    if (!isvalid(slot)) return;

    sounds[slot] = 0;
}

//Sets the volume on a given sound.
AUDWRAP_API void AudSetVolume(int slot, float volume) {
    if (!isvalid(slot)) return;

    sounds[slot]->setVolume(volume);
}

//Gets the volume on a given sound
AUDWRAP_API float AudGetVolume(int slot) {
    if (!isvalid(slot)) return 0.0;

    return sounds[slot]->getVolume();
}

//Toggles repeating on a given sound
AUDWRAP_API void AudSetRepeat(int slot, bool repeat) {
    if (!isvalid(slot)) return;

    sounds[slot]->setRepeat(repeat);
}

//Reads repeat setting on a given sound
AUDWRAP_API bool AudGetRepeat(int slot) {
    if (!isvalid(slot)) return false;

    return sounds[slot]->getRepeat();
}

//Returns whether the index provided is valid
AUDWRAP_API bool AudIsValidSound(int slot) {
    return isvalid(slot);
}

//Returns whether the index provided is playing
AUDWRAP_API bool AudIsPlaying(int slot) {
    if (!isvalid(slot)) return false;

    return sounds[slot]->isPlaying() == 1;
}

//Causes the given sound to play
AUDWRAP_API void AudPlay(int slot) {
    if (!isvalid(slot)) return;
    
    //no sense in re-playing an already playing sound
    if (sounds[slot]->isPlaying()) return;

    sounds[slot]->play();
}

//Stops a sound
AUDWRAP_API void AudStop(int slot) {
    if (!isvalid(slot)) return;

    //again, no sense in stopping an already stopped sound
    if (sounds[slot]->isPlaying()) sounds[slot]->stop();
    
    //Shuffles the position back to the start, as opposed to AudPause
    sounds[slot]->reset();
}

//Stops the sound, but doesn't reset its cursor
AUDWRAP_API void AudPause(int slot) {
    if (!isvalid(slot)) return;

    if (sounds[slot]->isPlaying()) sounds[slot]->stop();
}

//interates the slots until it finds a sound. If there isn't any room, it grows the array
int findFreeSlot() {
    for (int slot = 0; slot < numSounds; slot++) {
        if (sounds[slot] == 0) {
            return slot;
        }
    }

    int ret = numSounds;
    int newMax = numSounds * 2;
    OutputStreamPtr *new_sounds = new OutputStreamPtr[newMax];
    if (new_sounds == 0) return -1;

    for (int slot = 0; slot < numSounds; slot++) {
        new_sounds[slot] = sounds[slot];
    }

    delete [] sounds;
    sounds = new_sounds;
    numSounds = newMax;

    return ret;
}

//checks the index in a couple of ways to determine validity
inline bool isvalid(int slot) {
    if (slot < 0 || slot >= numSounds) return false;
    if (sounds[slot] == 0) return false;
    return true;
}

//LumpFile wrapper around the FileWrapper wrapper around the Lump wrapper

LumpFile::LumpFile(Lump *lump) {
    wrapper = FileWrapper_open(lump);
    length = FileWrapper_seek(wrapper, 0, SEEK_END);
    //FileWrapper_seek(wrapper, 0, SEEK_SET);
}

LumpFile::~LumpFile() {
    FileWrapper_close(wrapper);
}

int LumpFile::read(void *buffer, int size) {
    return FileWrapper_read(wrapper, buffer, 1, size);
}

bool LumpFile::seek(int position, SeekMode mode) {
    switch (mode) {
        case CURRENT:
        {
            int initpos = tell();
            return FileWrapper_seek(wrapper, position, SEEK_CUR) - initpos == position;
        }
        case BEGIN:
        {
            return FileWrapper_seek(wrapper, position, SEEK_SET) == position;
        }
        case END:
        {
            return FileWrapper_seek(wrapper, position, SEEK_END) - length == position;
        }
    }
    return false;
}

int LumpFile::tell() {
    return FileWrapper_seek(wrapper, SEEK_CUR, 0);
}
