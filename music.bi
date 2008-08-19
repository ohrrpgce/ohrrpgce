#IFNDEF __MUSIC_BI__
#DEFINE __MUSIC_BI__

#INCLUDE "compat.bi"

'' External music functions

ENUM music_format
	FORMAT_BAM = 1
	FORMAT_MIDI = 2
	FORMAT_MOD = 4
	FORMAT_OGG = 8
	FORMAT_MP3 = 16
	FORMAT_XM = 32
	FORMAT_IT = 64
	FORMAT_S3M = 128
	FORMAT_WAV = 256
END ENUM

'' FX=WAV or OGG only, Music=all but WAV and MP3
#define VALID_FX_FORMAT (FORMAT_WAV or FORMAT_OGG or FORMAT_MP3)
#define VALID_MUSIC_FORMAT (FORMAT_BAM or FORMAT_MIDI or FORMAT_MOD or FORMAT_OGG or FORMAT_MP3 or FORMAT_XM or FORMAT_IT or FORMAT_S3M)
'SDL_Mixer crashes on a lot of MP3s
#if not defined(MUSIC_SDL_BACKEND)
#define PREVIEWABLE_FX_FORMAT (FORMAT_WAV or FORMAT_OGG or FORMAT_MP3)
#define PREVIEWABLE_MUSIC_FORMAT (FORMAT_BAM or FORMAT_MIDI or FORMAT_MOD or FORMAT_OGG or FORMAT_MP3 or FORMAT_XM or FORMAT_IT or FORMAT_S3M)
#else
#define PREVIEWABLE_FX_FORMAT (FORMAT_WAV or FORMAT_OGG)
#define PREVIEWABLE_MUSIC_FORMAT (FORMAT_BAM or FORMAT_MIDI or FORMAT_MOD or FORMAT_OGG or FORMAT_XM or FORMAT_IT or FORMAT_S3M)
#endif

declare sub music_init()
declare sub music_close()

declare sub music_play(songname as string, fmt as integer=FORMAT_BAM)
declare sub music_pause()
declare sub music_resume()
declare sub music_stop()

declare sub music_setvolume(vol as integer)
declare function music_getvolume() as integer
declare sub music_fade(targetvol as integer)


declare sub sound_init()
declare sub sound_close()
declare sub sound_reset()

'parameter s is 0 if num is an OHR sound number, -1 if it's a slot number (returned from loadSound)
declare sub sound_play(byval num as integer, byval loopcount as integer,  byval s as integer = 0)
declare sub sound_pause(byval num as integer,  byval s as integer = 0)
declare sub sound_stop(byval num as integer,  byval s as integer = 0)
declare sub sound_free(byval num as integer)'only used by custom for the importing interface

declare function sound_playing(byval num as integer,  byval s as integer = 0) as integer

declare function LoadSound overload(byval num as integer) as integer
declare function LoadSound overload(byval filename as string,  byval num as integer = -1) as integer

declare sub UnloadSound(byval num as integer)

#ENDIF
