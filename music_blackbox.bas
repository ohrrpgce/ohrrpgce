'OHRRPGCE - music_blackbox audio backend 
'(C) Copyright 1997-2025 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

'This is based on music_sdl. The required API set has been refactored and some "common" code pasted in here.
'The actual blackbox backend should provide the needed apis

#include "music.bi"
#include "common_base.bi"

' blackbox APIs
''''''''''''''''''''''''''''''''''''''''''''''''
extern "C"

' specs
Declare Function blackbox_music_supported_formats() As Integer
Declare Function blackbox_sound_supported_formats() As Integer

' commands on music
Declare Function blackbox_music_play(songname As const zstring ptr, fmt As Integer) As Any ptr
Declare Sub blackbox_music_stop()
Declare Sub blackbox_music_setvolume(vol As Single)

' commands to manage sfx life cycles
Declare Function blackbox_sound_load(filename As const zstring ptr) As any ptr
Declare Sub blackbox_sound_unload(handle As any Ptr)

' commands on existing sfx
Declare Sub blackbox_sound_play(handle As any ptr, loopcount As Integer, volume As Single)
Declare Sub blackbox_sound_pause(handle As any Ptr)
Declare Sub blackbox_sound_stop(handle As any ptr)
Declare Sub blackbox_sound_set_volume(handle As any ptr, volume As Single)
Declare Function blackbox_sound_playing(handle As any ptr) As Boolean

End extern
''''''''''''''''''''''''''''''''''''''''''''''''

' Types

TYPE SoundEffectSlot EXTENDS SFXCommonData
	used as bool        'whether this slot is free
	playing as bool     'Set to false by a callback when the channel finishes
	buf as any ptr
	vol as single
End TYPE

'variables
dim shared music_vol As Double       '0 to 1 nominally; values above 1 useful when other multipliers exist
dim shared music_running As Boolean = False
dim shared music_song As any ptr = NULL
dim shared sound_inited As Boolean = False

'music_blackbox has an arbitrary limit of 16 sound effects playing at once (copied from music_sdl):
dim shared sfx_slots(15) As SoundEffectSlot

Sub music_init()
	'do nothing if already running
	If music_running Then Exit Sub

	' defaults copied from music_sdl
	music_vol = 0.5
	music_running = True

End Sub

Sub music_close()

	'We don't expect this to get used... 
	'Even though it makes no sense to turn off audio on blackbox, we need to clean the state properly (volumes and channels ETC)

	'do nothing if not running
	If Not music_running Then Exit Sub

	music_stop()

End Sub

Function music_get_info() As String
	' it's judged unlikely that the game will need any of this information (hz, etc)
	Return "music_blackbox"
End Function

Function music_supported_formats() as Integer
	Return blackbox_music_supported_formats() And VALID_MUSIC_FORMAT
End function

function sound_supported_formats() as Integer
	Return blackbox_sound_supported_formats() And VALID_SFX_FORMAT
End Function

Function music_settings_menu() As bool
	' todo: did this return things to make the in-game sfx/mus vol menus appear
	Return NO
End Function

Sub music_play(ByVal lump As Lump ptr, ByVal fmt As MusicFormatEnum)
	' not supported?
End Sub

Sub music_play(songname As String, ByVal fmt As MusicFormatEnum)
	If Not music_running Then Exit Sub

	' this is not really so complex
	music_stop()
	music_song = blackbox_music_play(songname, fmt)

End Sub

Sub music_pause()
	'reportedly these don't work, so don't do anything
End Sub

Sub music_resume()
	'reportedly these don't work, so don't do anything
End Sub

Sub music_stop()
	blackbox_music_stop()
End Sub

Sub music_setvolume(ByVal vol As Single)
	music_vol = large(vol, 0.)
	blackbox_music_setvolume(vol)
End Sub

Function music_getvolume() as Single
	Return music_vol
End Function

Sub sound_init()
	'if this were called twice, the world would end.
	If sound_inited Then Exit Sub

	'anything that might be initialized here is done in music_init
	'but, I must do it here too
	music_init()
	sound_inited = YES

End Sub

Sub sound_reset()
	'trying to free something that's already freed... bad!
	If sound_inited = NO Then Exit Sub
	For slot As Integer = 0 To ubound(sfx_slots)
		sound_unload(slot)
	Next
End Sub

Sub sound_close()
	sound_reset()
	sound_inited = NO
End Sub

' Returns -1 if too many sounds already playing/loaded
Function next_free_slot() As Integer
	Static retake_slot As Integer = 0
	Dim i As Integer

	'Look for empty slots
	For i = 0 To ubound(sfx_slots)
		If sfx_slots(i).used = NO Then
			Return i
		End If
	Next

	'Look for silent slots
	For i = 0 To ubound(sfx_slots)
		retake_slot = (retake_slot + 1) Mod (ubound(sfx_slots) + 1)
		With sfx_slots(retake_slot)
			If blackbox_sound_playing(.buf) = NO Then
				blackbox_sound_unload(.buf)
				.used = NO
				.buf = 0
				Return retake_slot
			End If
		End With
	Next

	Return -1 ' no slot found
End Function

'Resumes a sfx if it's paused
Sub sound_play(slot As Integer, loopcount As Integer, volume As Single)
	If slot = -1 Then Exit Sub

	' sfx_slots acts like a cache in this backend, since .buf
	' remains loaded after the sound effect has stopped.
	With sfx_slots(slot)
		If .buf = 0 Then
			showbug "sound_play: not loaded"
			Exit Sub
		End If

		If blackbox_sound_playing(.buf) = NO Then
			' Note that the i-th sfx slot is played on the i-th SDL_mixer channel,
			' which is just a simplification.
			.vol = volume
			blackbox_sound_play(.buf, loopcount, volume)
		End If

		' in case it was already playing... ??
		blackbox_sound_set_volume(.buf, volume)

	End With
End Sub

Sub sound_pause(slot As Integer)
	If slot = -1 Then Exit Sub
	With sfx_slots(slot)
		blackbox_sound_pause(.buf)
	End With
End Sub

Sub sound_stop(slot As Integer)
	If slot = -1 Then Exit Sub
	With sfx_slots(slot)
		blackbox_sound_stop(.buf)
	End With
End Sub

Sub sound_free(num As Integer)
	For slot As Integer = 0 To ubound(sfx_slots)
		With sfx_slots(slot)
			If .effectID = num Then sound_unload slot
		End With
	Next
End Sub

Sub sound_setvolume(slot As Integer, volume As Single)
	If slot = -1 Then Exit Sub
	sfx_slots(slot).vol = volume
	blackbox_sound_set_volume(sfx_slots(slot).buf, volume)
End Sub

Function sound_getvolume(slot As Integer) As Single
	If slot = -1 Then Return 0.
	return sfx_slots(slot).vol
End Function

Function sound_slot_with_id(num as integer) as Integer
	For slot As Integer = 0 To ubound(sfx_slots)
		With sfx_slots(slot)
			If .used AndAlso .effectID = num Then Return slot
		End With
	Next

	Return -1
End Function

Function sound_playing(slot As Integer) As bool
	If slot = -1 Then Return NO
	If sfx_slots(slot).used = NO Then Return NO
	Return blackbox_sound_playing(sfx_slots(slot).buf)
End Function

Function sound_slotdata(slot As Integer) As SFXCommonData ptr
	If slot < 0 Or slot > ubound(sfx_slots) Then Return NULL
	If sfx_slots(slot).used = NO Then Return NULL
	Return @sfx_slots(slot)
End Function

Function sound_lastslot() as Integer
	Return ubound(sfx_slots)
End Function

Function sound_load overload(lump As Lump ptr, num As Integer = -1) As Integer
	' not supported?
	Return -1
End Function

Function sound_load overload(filename As String, num As Integer = -1) As Integer
	Dim slot As Integer
	Dim sfx As any ptr

	If filename = "" Then Return -1
	If Not isfile(filename) Then Return -1

	log_openfile filename
	sfx = blackbox_sound_load(filename)
	If sfx = NULL Then
		debug "Couldn't blackbox_sound_load " & filename
		Return -1
	End If

	slot = next_free_slot()
	'debuginfo "sound_load(" & filename & "," & num & ") in slot " & slot

	If slot = -1 Then
		debuginfo "sound_load(""" & filename & """, " & num & ") no more sound slots available"
	Else
		With sfx_slots(slot)
			.used = YES
			.effectID = num
			.buf = sfx
		End With
	End If

	Return slot
End Function

'Unloads a sound loaded in a slot. TAKES A CACHE SLOT, NOT AN SFX ID NUMBER!
Sub sound_unload(slot As Integer)
	With sfx_slots(slot)
		If .used = NO Then Exit Sub
		blackbox_sound_unload(.buf)
		.used = NO
		.effectID = 0
		.buf = 0
	End With
End Sub
