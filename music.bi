'' External music functions

ENUM music_format
	FORMAT_BAM = 0,
	FORMAT_MIDI,
	FORMAT_MOD,
	FORMAT_OGG
END ENUM
 
declare sub music_init()	
declare sub music_close()

declare sub music_play(songname as string, fmt as music_format=FORMAT_BAM)
declare sub music_pause()
declare sub music_resume()
declare sub music_stop()

declare sub music_setvolume(vol as integer)
declare function music_getvolume() as integer
declare sub music_fade(targetvol as integer)


declare sub sound_init()	
declare sub sound_close()

declare sub sound_play(byval num as integer, byval l as integer)
declare sub sound_pause(byval num as integer)
declare sub sound_stop(byval num as integer)
declare sub sound_free(byval num as integer)'only used by custom for the importing interface

declare function sound_playing(byval slot as integer) as integer
