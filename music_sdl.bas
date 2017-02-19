''
'' music_sdl.bas - External music functions implemented with SDL 1.2 + SDL_mixer 1.2
''
'' part of OHRRPGCE - see elsewhere for license details
''

#include "config.bi"

#ifdef __FB_WIN32__
	'In FB >= 1.04 SDL.bi includes windows.bi; we have to include it first to do the necessary conflict prevention
	include_windows_bi()
#endif

#include "music.bi"
#include "gfx.bi"
#include "util.bi"
#include "common.bi"
#include "file.bi"
'warning: due to a FB bug, overloaded functions must be declared before SDL.bi is included

#ifdef __UNIX__
	'In FB >= 1.04 SDL.bi includes Xlib.bi; fix a conflict
	#undef font
#endif

#include "SDL\SDL.bi"
#include "SDL\SDL_mixer.bi"

' External functions

declare function safe_RWops(byval rw as SDL_RWops ptr) as SDL_RWops ptr
declare sub safe_RWops_close (byval rw as SDL_RWops ptr)

extern "C"

declare function SDL_RWFromLump(byval lump as Lump ptr) as SDL_RWops ptr

'The decoder enum functions are only available in SDL_mixer > 1.2.8 which is the version shipped with
'Debian 6.0 Squeeze and hence older Ubuntu. Squeeze was superceded by 7.0 Wheezy in May 2013.
'So don't depend on these functions.
dim shared _Mix_GetNumMusicDecoders as function () as Sint32
dim shared _Mix_GetNumChunkDecoders as function () as Sint32
dim shared _Mix_GetMusicDecoder as function  (byval index as Sint32) as zstring ptr
dim shared _Mix_GetChunkDecoder as function (byval index as Sint32) as zstring ptr

' Older FB have out of date SDL headers
#if __FB_VERSION__ < "1.04"
	declare function Mix_LoadMUS_RW (byval rw as SDL_RWops ptr) as Mix_Music ptr
	' Mix_GetMusicDecoder etc also missing
#endif

end extern


' Local functions

declare function next_free_slot() as integer
declare function sfx_slot_info (byval slot as integer) as string

enum MusicStatusEnum
  musicError = -1  ' Don't try again
  musicOff = 0
  musicOn = 1
end enum

dim shared music_status as MusicStatusEnum = musicOff
dim shared music_vol as integer      '0 to 128
dim shared music_paused as bool      'Always false: we never pause! (see r5406)
dim shared music_song as Mix_Music ptr = NULL
dim shared music_song_rw as SDL_RWops ptr = NULL
dim shared orig_vol as integer = -1
dim shared nonmidi_playing as bool = NO

'The music module needs to manage a list of temporary files to
'delete when closed, mainly for custom, so they don't get lumped
type delitem
	fname as zstring ptr
	nextitem as delitem ptr
end type

dim shared delhead as delitem ptr = null
dim shared callback_set_up as bool = NO

sub quit_sdl_audio()
	if SDL_WasInit(SDL_INIT_AUDIO) then
		SDL_QuitSubSystem(SDL_INIT_AUDIO)
		if SDL_WasInit(0) = 0 then
			SDL_Quit()
		end if
	end if
end sub

function music_get_info() as string
	dim ver as const SDL_version ptr
	dim ret as string = "music_sdl"

	dim libhandle as any ptr
	libhandle = dylibload("libSDL_mixer.so")
	if libhandle then
		_Mix_GetNumMusicDecoders = dylibsymbol(libhandle, "Mix_GetNumMusicDecoders")
		_Mix_GetNumChunkDecoders = dylibsymbol(libhandle, "Mix_GetNumChunkDecoders")
		_Mix_GetMusicDecoder = dylibsymbol(libhandle, "Mix_GetMusicDecoder")
		_Mix_GetChunkDecoder = dylibsymbol(libhandle, "Mix_GetChunkDecoder")
	end if

	if gfxbackend <> "sdl" then
		ver = SDL_Linked_Version()
		ret += ", SDL " & ver->major & "." & ver->minor & "." & ver->patch
	end if

	ver = Mix_Linked_Version()
	ret += ", SDL_Mixer " & ver->major & "." & ver->minor & "." & ver->patch

	if music_status = musicOn then
		dim freq as int32, format as ushort, channels as int32
		Mix_QuerySpec(@freq, @format, @channels)
		ret += " (" & freq & "Hz"

		if _Mix_GetNumMusicDecoders andalso _Mix_GetMusicDecoder then
			ret += ", Music decoders:"
			for i as integer = 0 to _Mix_GetNumMusicDecoders() - 1
				if i > 0 then ret += ","
				ret += *_Mix_GetMusicDecoder(i)
			next
		end if

		if _Mix_GetNumChunkDecoders andalso _Mix_GetChunkDecoder then
			ret += " Sample decoders:"
			for i as integer = 0 to _Mix_GetNumChunkDecoders() - 1
				if i > 0 then ret += ","
				ret += *_Mix_GetChunkDecoder(i)
			next
		end if

		ret += ")"
	end if

	dylibfree(libhandle)

	return ret
end function

sub music_init()
	if music_status = musicOff then
		dim audio_rate as integer
		dim audio_format as Uint16
		dim audio_channels as integer
		dim audio_buffers as integer

		' We're going to be requesting certain things from our audio
		' device, so we set them up beforehand
		' MIX_DEFAULT_FREQUENCY is 22050, which slightly worsens sound quality
		' than playing at 44100, but using 44100 causes tracks between 22-44
		' to be sped up, which sounds worse. See https://sourceforge.net/p/ohrrpgce/bugs/2026/
		audio_rate = MIX_DEFAULT_FREQUENCY
		audio_format = MIX_DEFAULT_FORMAT
		audio_channels = 2
		'Despite the documentation, non power of 2 buffer size MAY work depending on the driver, and pygame even does it
		'1024 seems to give much lower delay than 1536 before being played, maybe a non-power of two problem
		audio_buffers = 1024 '1536

		if SDL_WasInit(0) = 0 then

			if SDL_Init(SDL_INIT_AUDIO) then
				debug "Can't start SDL (audio): " & *SDL_GetError
				music_status = musicError
				exit sub
			end if
		elseif SDL_WasInit(SDL_INIT_AUDIO) = 0 then
			if SDL_InitSubSystem(SDL_INIT_AUDIO) then
				debug "Can't start SDL audio subsys: " & *SDL_GetError
				music_status = musicError
				quit_sdl_audio()
				exit sub
			end if
		end if

		if (Mix_OpenAudio(audio_rate, audio_format, audio_channels, audio_buffers)) <> 0 then
			'if (Mix_OpenAudio(audio_rate, audio_format, audio_channels, 2048)) <> 0 then
				debug "Can't open audio : " & *Mix_GetError
				music_status = musicError
				quit_sdl_audio()
				exit sub
			'end if
		end if

		music_vol = 64
		music_status = musicOn
		music_paused = NO
	end if
end sub

sub music_close()
	if music_status = musicOn then
		if orig_vol > 0 then
			'restore original volume
			Mix_VolumeMusic(orig_vol)
		else
			'arbitrary medium value
			Mix_VolumeMusic(0.5 * MIX_MAX_VOLUME)
		end if

		music_stop()
		Mix_CloseAudio()
		quit_sdl_audio()

		music_status = musicOff
		callback_set_up = NO	' For SFX

		if delhead <> null then
			'delete temp files
			dim ditem as delitem ptr
			dim dlast as delitem ptr

			ditem = delhead
			while ditem <> null
				if isfile(*(ditem->fname)) then
					kill *(ditem->fname)
				end if
				deallocate ditem->fname 'deallocate string
				dlast = ditem
				ditem = ditem->nextitem
				deallocate dlast 'deallocate delitem
			wend
			delhead = null
		end if
	end if
end sub

sub music_play(byval lump as Lump ptr, byval fmt as MusicFormatEnum)

end sub

sub music_play(songname as string, byval fmt as MusicFormatEnum)
	if music_status = musicOn then
		songname = rtrim$(songname)	'lose any added nulls

		if fmt = FORMAT_BAM then
			dim midname as string
			dim as integer flen
			flen = filelen(songname)
			'use last 3 hex digits of length as a kind of hash,
			'to verify that the .bmd does belong to this file
			flen = flen and &h0fff
			midname = tmpdir & trimpath(songname) & "-" & lcase(hex(flen)) & ".bmd"
			'check if already converted
			if isfile(midname) = 0 then
				bam2mid(songname, midname)
				'add to list of temp files
				dim ditem as delitem ptr
				if delhead = null then
					delhead = allocate(sizeof(delitem))
					ditem = delhead
				else
					ditem = delhead
					while ditem->nextitem <> null
						ditem = ditem->nextitem
					wend
					ditem->nextitem = allocate(sizeof(delitem))
					ditem = ditem->nextitem
				end if
				ditem->nextitem = null
				'allocate space for zstring
				ditem->fname = allocate(len(midname) + 1)
				*(ditem->fname) = midname 'set zstring
			end if
			songname = midname
			fmt = FORMAT_MIDI
		end if

		music_stop

		'Workaround: internally, Mix_LoadMUS creates a RWops to read the music file.
		'However, because SDL_mixer is such a bug ridden mess, at least the MAD and libMikMod
		'backend wrappers do not bother to actually close the RWops!
		'So we do the RWops wrapping ourselves, and close it after stopping the music

		music_song_rw = SDL_RWFromFile(songname, @"rb")
		if music_song_rw = NULL then
			debug "Could not load song " + songname + " (SDL_RWFromFile failed)"
			exit sub
		end if
		music_song_rw = safe_RWops(music_song_rw)
		music_song = Mix_LoadMUS_RW(music_song_rw)
		if music_song = 0 then
			debug "Could not load song " + songname + " : " & *Mix_GetError
			exit sub
		end if

		Mix_PlayMusic(music_song, -1)
		music_paused = NO

		'not really working when songs are being faded in.
		if orig_vol = -1 then
			orig_vol = Mix_VolumeMusic(-1)
		end if

		Mix_VolumeMusic(music_vol)

		if fmt <> FORMAT_MIDI then
			nonmidi_playing = YES
		else
			nonmidi_playing = NO
		end if
	end if
end sub

sub music_pause()
	'Pause is broken in SDL_Mixer, so just stop.
	'A look at the source indicates that it won't work for MIDI
	if music_status = musicOn then
		if music_song > 0 then
			Mix_HaltMusic
			nonmidi_playing = NO
		end if
	end if
end sub

sub music_resume()
	if music_status = musicOn then
		if music_song > 0 then
			Mix_ResumeMusic
			music_paused = NO
		end if
	end if
end sub

sub music_stop()
	if music_song <> 0 then
		Mix_FreeMusic(music_song)
		music_song = 0
		music_paused = NO
		nonmidi_playing = NO
	end if
	if music_song_rw <> 0 then
		'Is safe even if has already been closed and freed
		safe_RWops_close(music_song_rw)
		music_song_rw = NULL
	end if
end sub


' Info on [Bug 843] Sound effects now affected by volume (on Windows)
'
' Note that Mix_VolumeMusic(-1) does not return the system
' MIDI volume level on Windows, it just returns what was set last.
'
' Windows XP:
' In Volume Control is a slider for SW Synth, the MIDI
' synthesizer. Setting the music volume while playing a MIDI
' sends a MIDI event which sets the SW Synth volume level,
' which can be overridden in Volume Control (note that Volume
' Control doesn't update the slider live).
'
' Windows 7+:
' The MIDI volume control is gone. Instead, apparently trying
' to set the MIDI volume (by midiOutSetVolume, which is what
' SDL_mixer does), actually sets the process's volume instead
' (waveOutSetVolume). The process volume is AFAIK not otherwise
' modified by SDL. Meaning sfx can only be quieter than MIDI.
' TODO: does it make sense to reset waveOutSetVolume after
' playing a MIDI?
' Two possible workarounds:
' -Set volume on each MIDI note instead of on the stream
'  (would need to patch SDL_mixer/use music_native)
' -Use a separate process to play MIDI, see code from Eternity Engine here:
'  https://www.doomworld.com/vb/post/1124981
' -Maybe use some new Vista+ API:
'  http://stackoverflow.com/a/19940489/1185152
' See https://www.doomworld.com/vb/source-ports/63861-windows-sound-any-general-fixes/
' for a summary
'
' Also even in old Windows there are problems with the MIDI
' volume if not using the SW Synth.
' http://forums.libsdl.org/viewtopic.php?t=949
'
' See also http://odamex.net/bugs/show_bug.cgi?id=863
' about the midiOutSetVolume volume curve being logarithmic,
' unlike SDL_mixer's internal volume.

' Volume fading: see r2283

sub music_setvolume(byval vol as single)
	music_vol = bound(vol, 0., 1.) * MIX_MAX_VOLUME
	if music_status = musicOn then
		Mix_VolumeMusic(music_vol)
	end if
end sub

function music_getvolume() as single
	'return Mix_VolumeMusic(-1) / MIX_MAX_VOLUME
	music_getvolume = music_vol / MIX_MAX_VOLUME
end function

'------------ Sound effects --------------

DECLARE sub SDL_done_playing cdecl(byval channel as int32)

' The SDL_Mixer channel number is equal to the SoundEffectSlot index
TYPE SoundEffectSlot
	used as bool        'whether this slot is free
	effectID as integer 'which sound is loaded

	paused as bool
	playing as bool     'Set to false by a callback when the channel finishes

	buf as Mix_Chunk ptr
END TYPE

'music_sdl has an arbitrary limit of 16 sound effects playing at once:
dim shared sfx_slots(15) as SoundEffectSlot

dim shared sound_inited as bool

sub sound_init
	'if this were called twice, the world would end.
	if sound_inited then exit sub

	'anything that might be initialized here is done in music_init
	'but, I must do it here too
	music_init
	Mix_AllocateChannels(ubound(sfx_slots) + 1)
	if callback_set_up = NO then
		Mix_channelFinished(@SDL_done_playing)
		callback_set_up = YES
	end if
	sound_inited = YES
end sub

sub sound_reset
	'trying to free something that's already freed... bad!
	if sound_inited = NO then exit sub
	for slot as integer = 0 to ubound(sfx_slots)
		sound_unload(slot)
	next
end sub

sub sound_close
	sound_reset()
	sound_inited = NO
end sub


' Returns -1 if too many sounds already playing/loaded
function next_free_slot() as integer
	static retake_slot as integer = 0
	dim i as integer

	'Look for empty slots
	for i = 0 to ubound(sfx_slots)
		if sfx_slots(i).used = NO then
			return i
		end if
	next

	'Look for silent slots
	for i = 0 to ubound(sfx_slots)
		retake_slot = (retake_slot + 1) mod (ubound(sfx_slots)+1)
		with sfx_slots(retake_slot)
			if .playing = NO and .paused = NO then
				Mix_FreeChunk(.buf)
				.used = NO
				return retake_slot
			end if
		end with
	next

	return -1 ' no slot found
end function

sub sound_play(slot as integer, loopcount as integer, volume as single = 1.)
	if slot = -1 then exit sub

	' sfx_slots acts like a cache in this backend, since .buf
	' remains loaded after the sound effect has stopped.
	with sfx_slots(slot)
		if .buf = 0 then
			debugc errPromptBug, "sound_play: not loaded"
			exit sub
		end if

		if .paused then
			Mix_Resume(slot)
			.paused = NO
		end if

		if .playing = NO then
			' Note that the i-th sfx slot is played on the i-th SDL_mixer channel,
			' which is just a simplification.
			if Mix_PlayChannel(slot, .buf, loopcount) = -1 then
				debugc errPromptBug, "sound_play: Mix_PlayChannel failed"
				exit sub
			end if
			.playing = YES
		end if

		' SDL_mixer has separate channel and chunk volumes and multiples them.
		' We do the multiplication ourselves, only using channel volumes.
		' Note that the built-in support for fades works by adjust channel
		' volumes, not chunk volumes. And volumes are capped to 100%.
		Mix_Volume(slot, volume * MIX_MAX_VOLUME)
	end with
end sub

sub sound_pause(slot as integer)
	if slot = -1 then exit sub
	with sfx_slots(slot)
		if .playing <> NO and .paused = NO then
			.paused = YES
			Mix_Pause(slot)
		end if
	end with
end sub

sub sound_stop(slot as integer)
	if slot = -1 then exit sub
	with sfx_slots(slot)
		if .playing <> NO then
			Mix_HaltChannel(slot)
			.playing = NO
			.paused = NO
		end if
	end with
end sub

sub sound_setvolume(slot as integer, volume as single)
	if slot = -1 then exit sub
	Mix_Volume(slot, volume * MIX_MAX_VOLUME)
end sub

function sound_getvolume(slot as integer) as single
	if slot = -1 then return 0.
	return Mix_Volume(slot, -1) / MIX_MAX_VOLUME
end function

sub sound_free(num as integer)
	for slot as integer = 0 to ubound(sfx_slots)
		with sfx_slots(slot)
			if .effectID = num then sound_unload slot
		end with
	next
end sub

function sound_playing(slot as integer) as bool
	if slot = -1 then return NO
	if sfx_slots(slot).used = NO then return NO

	return sfx_slots(slot).playing
end function

' Returns the first sound slot with the given sound effect ID (num);
' if the sound is not loaded, returns -1.
function sound_slot_with_id(num as integer) as integer
	for slot as integer = 0 to ubound(sfx_slots)
		with sfx_slots(slot)
			if .used AND .effectID = num then return slot
		end with
	next

	return -1
end function

'Loads a sound into a slot, and marks its ID num (equal to OHR sfx number).
'Returns the slot number, or -1 if an error occurs.
function sound_load overload(lump as Lump ptr, num as integer = -1) as integer
	return -1
end function

function sound_load overload(filename as string, num as integer = -1) as integer
	dim slot as integer
	dim sfx as Mix_Chunk ptr

	if filename = "" then return -1
	if not isfile(filename) then return -1

	'File size restriction to stop massive oggs being decompressed
	'into memory.
	'(this check is now only done in browse.bas when importing)
	'if filelen(filename) > 500*1024 then
	'	debug "Sound effect file too large (>500k): " & filename
	'	return -1
	'end if

	sfx = Mix_LoadWAV(@filename[0])
	if sfx = NULL then
		debug "Couldn't Mix_LoadWAV " & filename
		return -1
	end if

	slot = next_free_slot()
	'debuginfo "sound_load(" & filename & "," & num & ") in slot " & slot

	if slot = -1 then
		debuginfo "sound_load(""" & filename & """, " & num & ") no more sound slots available"
	else
		with sfx_slots(slot)
			.used = YES
			.effectID = num
			.buf = sfx
			.playing = NO
			.paused = NO
		end with
	end if

	return slot
end function

'Unloads a sound loaded in a slot. TAKES A CACHE SLOT, NOT AN SFX ID NUMBER!
sub sound_unload(slot as integer)
	if sfx_slots(slot).used = NO then exit sub
	with sfx_slots(slot)
		Mix_FreeChunk(.buf)
		.paused = NO
		.playing = NO
		.used = NO
		.effectID = 0
		.buf = 0
	end with
end sub

sub SDL_done_playing cdecl(byval channel as int32)
	sfx_slots(channel).playing = NO
end sub

'-- for debugging
function sfx_slot_info (byval slot as integer) as string
	with sfx_slots(slot)
		return .used & " " & .effectID & " " & .paused & " " & .playing & " " & .buf
	end with
end function
