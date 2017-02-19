''
'' music_stub.bas - A dummy backend for isolating problems that may or may not
'' be backend related
''
'' part of OHRRPGCE - see elsewhere for license details
''

#include "music.bi"

'these functions intentionally left blank
sub music_init() : end sub

sub music_close() : end sub

function music_get_info() as string
	return ""
end function

sub music_play(byval lump as Lump ptr, byval fmt as MusicFormatEnum) : end sub

sub music_play(songname as string, byval fmt as MusicFormatEnum) : end sub

sub music_pause() : end sub

sub music_resume() : end sub

sub music_stop() : end sub

sub music_setvolume(byval vol as single) : end sub

function music_getvolume() as single
	return 0.5
end function

sub sound_init() : end sub

sub sound_close() : end sub

sub sound_reset() : end sub

sub sound_play(num as integer, loopcount as integer, num_is_slot as bool = NO) : end sub

sub sound_pause(num as integer, num_is_slot as bool = NO) : end sub

sub sound_stop(num as integer, num_is_slot as bool = NO) : end sub

sub sound_free(num as integer) : end sub

function sound_slot_with_id(num as integer) as integer
	return -1
end function

function sound_playing(num as integer, num_is_slot as bool = NO) as bool
	return NO
end function

function sound_load overload(lump as Lump ptr, num as integer = -1) as integer
	return 0
end function

function sound_load overload(filename as string, num as integer = -1) as integer
	return 0
end function

sub sound_unload(slot as integer) : end sub
