// (C) Copyright 2006-2017 Mike Caron and the OHRRPGCE Developers
// Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include <stdio.h>
#include <string>
#include "../errorlog.h"
#include "audwrap.h"
using namespace audiere;

AudioDevicePtr device = 0;

struct SoundSlot {
    OutputStreamPtr stream;
    SampleSourcePtr source;
};

SoundSlot *sounds = 0;  // Array
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
    sounds = new SoundSlot[numSounds];
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

    sounds[slot].source = OpenSampleSource(filename, FF_AUTODETECT);
    if (!sounds[slot].source) {
        debug(errError, "audiere: Can't open source %s", filename);
        return -1;
    }

    sounds[slot].stream = OpenSound(device, sounds[slot].source, streaming);
    if (!sounds[slot].stream) {
        debug(errError, "audiere: Failed to open stream from %s", filename);
        return -1;
    }

    sounds[slot].stream->setVolume(1.0f);

    return slot;
}

AUDWRAP_API int AudLoadSoundLump(Lump *lump, bool streaming) {
    if (!device) return -1; 

    int slot = findFreeSlot();
    if (slot < 0) return (-1);

    sounds[slot].source = OpenSampleSource(FilePtr(new LumpFile(lump)), FF_AUTODETECT);
    if (!sounds[slot].source) {
        debug(errError, "audiere: Can't open source from Lump");
        return -1;
    }

    sounds[slot].stream = OpenSound(device, sounds[slot].source, streaming);
    if (!sounds[slot].stream) {
        debug(errError, "audiere: Failed to open stream from Lump");
        return -1;
    }

    sounds[slot].stream->setVolume(1.0f);

    return slot;
}

//Unloads a file from the buffer, and frees its slot
AUDWRAP_API void AudUnloadSound(int slot) {
    if (!isvalid(slot)) return;

    sounds[slot].stream = 0;
    sounds[slot].source = 0;
}

//Sets the volume on a given sound.
AUDWRAP_API void AudSetVolume(int slot, float volume) {
    if (!isvalid(slot)) return;

    sounds[slot].stream->setVolume(volume);
}

//Gets the volume on a given sound
AUDWRAP_API float AudGetVolume(int slot) {
    if (!isvalid(slot)) return 0.0;

    return sounds[slot].stream->getVolume();
}

//Toggles repeating on a given sound
AUDWRAP_API void AudSetRepeat(int slot, bool repeat) {
    if (!isvalid(slot)) return;

    sounds[slot].stream->setRepeat(repeat);
}

//Reads repeat setting on a given sound
AUDWRAP_API bool AudGetRepeat(int slot) {
    if (!isvalid(slot)) return false;

    return sounds[slot].stream->getRepeat();
}

//Returns whether the index provided is valid
AUDWRAP_API bool AudIsValidSound(int slot) {
    return isvalid(slot);
}

//Returns whether the index provided is playing
AUDWRAP_API bool AudIsPlaying(int slot) {
    if (!isvalid(slot)) return false;

    return sounds[slot].stream->isPlaying() == 1;
}

//Causes the given sound to play
AUDWRAP_API void AudPlay(int slot) {
    if (!isvalid(slot)) return;
    
    //no sense in re-playing an already playing sound
    if (sounds[slot].stream->isPlaying()) return;

    sounds[slot].stream->play();
}

//Stops a sound
AUDWRAP_API void AudStop(int slot) {
    if (!isvalid(slot)) return;

    //again, no sense in stopping an already stopped sound
    if (sounds[slot].stream->isPlaying()) sounds[slot].stream->stop();
    
    //Shuffles the position back to the start, as opposed to AudPause
    sounds[slot].stream->reset();
}

//Stops the sound, but doesn't reset its cursor
AUDWRAP_API void AudPause(int slot) {
    if (!isvalid(slot)) return;

    if (sounds[slot].stream->isPlaying()) sounds[slot].stream->stop();
}

AUDWRAP_API int AudSampleRate(int slot) {
    if (!isvalid(slot)) return 0;
    int channel_count, sample_rate;
    SampleFormat format;
    sounds[slot].source->getFormat(channel_count, sample_rate, format);
    return sample_rate;
}

//Whether AudSetPosition (should) work
AUDWRAP_API bool AudIsSeekable(int slot) {
    if (!isvalid(slot)) return false;
    return sounds[slot].stream->isSeekable();
}

//Length in seconds, -1 on failure
AUDWRAP_API double AudGetLength(int slot) {
    if (!isvalid(slot)) return -1.0;
    int sample_rate = AudSampleRate(slot);
    if (!sample_rate) return -1.0;
    return sounds[slot].source->getLength() / sample_rate;
}

//Sets time in seconds
AUDWRAP_API void AudSetPosition(int slot, double position) {
    if (!isvalid(slot)) return;
    int sample_rate = AudSampleRate(slot);
    if (!sample_rate) return;
    sounds[slot].stream->setPosition(position * sample_rate);
}

//Time in seconds, -1 on failure
AUDWRAP_API double AudGetPosition(int slot) {
    if (!isvalid(slot)) return -1.0;
    int sample_rate = AudSampleRate(slot);
    if (!sample_rate) return -1.0;
    return sounds[slot].stream->getPosition() * sample_rate;
}

//interates the slots until it finds a sound. If there isn't any room, it grows the array
int findFreeSlot() {
    for (int slot = 0; slot < numSounds; slot++) {
        if (sounds[slot].stream == 0) {
            return slot;
        }
    }

    int ret = numSounds;
    int newMax = numSounds * 2;
    SoundSlot *new_sounds = new SoundSlot[newMax];
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
    if (sounds[slot].stream == 0) return false;
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
