#IFNDEF __MUSIC_BI__
#DEFINE __MUSIC_BI__

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

declare sub sound_play(byval num as integer, byval l as integer,  byval s as integer = 0)
declare sub sound_pause(byval num as integer,  byval s as integer = 0)
declare sub sound_stop(byval num as integer,  byval s as integer = 0)
declare sub sound_free(byval num as integer)'only used by custom for the importing interface

declare function sound_playing(byval slot as integer,  byval s as integer = 0) as integer

declare function LoadSound overload(byval num as integer) as integer
declare function LoadSound overload(byval f as string,  byval num as integer = -1) as integer

declare sub UnloadSound(byval num as integer)

#ENDIF