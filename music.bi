'OHRRPGCE - Music backend API
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#IFNDEF __MUSIC_BI__
#DEFINE __MUSIC_BI__

#INCLUDE "config.bi"
#INCLUDE "lumpfile.bi"
#INCLUDE "audiofile.bi"


'==========================================================================================
'                                          Music

declare sub music_init()
declare sub music_close()

'Return runtime information. Called both before and after initialisation, should report as much as can be managed
'(eg. just return dll version)
declare function music_get_info() as string

'Returns combination of MusicFormatEnum bits. Music file formats which we can attempt to play.
'(Also used to exclude a format if we could play it, but shouldn't because it's broken)
declare function music_supported_formats() as integer

'The fmt arg is not very useful, and mostly ignored.
declare sub music_play overload(filename as string, byval fmt as MusicFormatEnum = FORMAT_UNSPECIFIED)
declare sub music_play overload(byval lump as Lump ptr, byval fmt as MusicFormatEnum = FORMAT_UNSPECIFIED)
declare sub music_pause()
declare sub music_resume()
declare sub music_stop()

declare sub music_setvolume(byval vol as single)
declare function music_getvolume() as single

' Optional. A menu to edit backend-specific settings.
' Return YES if a menu was shown, NO otherwise
declare function music_settings_menu() as bool

'==========================================================================================
'                                            SFX

' Data tracked per playing sfx slot by all music backends
TYPE SFXCommonData
	effectID as integer        'OHR sound effect number
	original_volume as single  'The volume without the global volume multiplied in
END TYPE

declare sub sound_init()
declare sub sound_close()
declare sub sound_reset()

'Returns combination of MusicFormatEnum bits. SFX file formats which we can attempt to play.
'(Also used to exclude a format if we could play it, but shouldn't because it's broken)
declare function sound_supported_formats() as integer

' loopcount is N to play N+1 times, -1 to loop forever.
declare sub sound_play(slot as integer, loopcount as integer, volume as single = 1.0)
declare sub sound_pause(slot as integer)
declare sub sound_stop(slot as integer)
declare sub sound_setvolume(slot as integer, volume as single)
declare function sound_getvolume(slot as integer) as single

' Returns the first sound slot with the given sound effect ID (num);
' if the sound is not loaded, returns -1.
declare function sound_slot_with_id(num as integer) as integer

declare function sound_playing(slot as integer) as bool

' Loads a sound into a slot, and marks its ID num (equal to OHR sfx number).
' Returns the slot number, or -1 if an error occurs.
declare function sound_load overload(lump as Lump ptr, num as integer = -1) as integer
declare function sound_load overload(filename as string, num as integer = -1) as integer

declare sub sound_unload(slot as integer)
' Unload all sound effects slots with a certain ID
declare sub sound_free(num as integer)

declare function sound_slotdata(slot as integer) as SFXCommonData ptr
declare function sound_lastslot() as integer

'==========================================================================================
'' Functions in bam2mid.bas

declare sub bam2mid(infile as string, outfile as string)


#ENDIF
