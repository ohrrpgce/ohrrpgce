'OHRRPGCE - Dummy music_silence audio backend
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
'A dummy backend for isolating problems that may or may not be backend related

#include "music.bi"

'these functions intentionally left blank
sub music_init() : end sub

sub music_close() : end sub

function music_get_info() as string
	return ""
end function

function music_supported_formats() as integer
	return 0
end function

function sound_supported_formats() as integer
	return 0
end function

function music_settings_menu() as bool
	return NO
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

sub sound_play(slot as integer, loopcount as integer, volume as single) : end sub

sub sound_pause(slot as integer) : end sub

sub sound_stop(slot as integer) : end sub

sub sound_free(num as integer) : end sub

sub sound_setvolume(slot as integer, volume as single) : end sub

function sound_getvolume(slot as integer) as single
	return 0.
end function

function sound_slot_with_id(num as integer) as integer
	return -1
end function

function sound_playing(slot as integer) as bool
	return NO
end function

function sound_slotdata(slot as integer) as SFXCommonData ptr
	return NULL
end function

function sound_lastslot() as integer
	return 0
end function

function sound_load overload(lump as Lump ptr, num as integer = -1) as integer
	return 0
end function

function sound_load overload(filename as string, num as integer = -1) as integer
	return 0
end function

sub sound_unload(slot as integer) : end sub
