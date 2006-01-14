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

declare sub music_setvolume(vol as integer)
declare function music_getvolume() as integer
declare sub music_fade(targetvol as integer)