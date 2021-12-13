'OHRRPGCE - music_native audio backend
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

'' Windows lowlevel event-based MIDI playback + Audiere. Supports loop points and sysexs.
'' Can also be compiled for other OSes, without MIDI support.
''
'' Supposedly "horribly laggy" due to improper MIDI timing.
'' Suffers from major bugs like 244: Midi voices switching to ACGPIANO randomly

#include "config.bi"

#IFDEF __FB_WIN32__
	#define WIN_INCLUDEALL
	include_windows_bi()
	' #include once "win/mmsystem.bi"
	' #include once "win/mmreg.bi"   'Doesn't exist and not needed in old FB
	' #include once "win/msacm.bi"
#ELSE
	'No MIDI support
#ENDIF

#include once "allmodex.bi"
#include once "common.bi"
#include once "const.bi"
#include once "util.bi"


#DEFINE ESCAPE_SEQUENCE Goto endOfSong


#include "music_native_subs.bas"


#include once "music.bi"

#IFDEF __FB_WIN32__
dim shared midi_handle as HMIDIOUT
#ELSE
dim shared midi_handle as FILE ptr
#ENDIF


' Local functions

DECLARE Sub PlayBackThread(byval dummy as any ptr)
DECLARE Sub UpdateDelay(byref delay as double, byval tempo as integer)

' Module-local variables

dim shared music_init_count as integer = 0     'Number of of times music_init called
dim shared music_vol as single = .5
dim shared sound_song as integer = -1          'Sound slot for non-MIDI music, otherwise equal to -1.

dim shared midi_paused as bool
dim shared midi_playing as bool
dim shared midi_song as MIDI_EVENT ptr = NULL  'Loaded MIDI file. Nonzero only when current music is MIDI
dim shared playback_thread as any ptr

'for playback
dim shared division as short



dim shared midibuffer as UByte ptr, midibufferlen as integer, midibufferused as integer

function openMidi() as integer
	#IFDEF __FB_WIN32__
	dim moc as MIDIOUTCAPS
	midiOutGetDevCaps MIDI_MAPPER, @moc, len(MIDIOUTCAPS)
	debuginfo "Midi port supports Volume changes:" + str(moc.dwSupport AND MIDICAPS_VOLUME)

	return midiOutOpen (@midi_handle,MIDI_MAPPER,0,0,0)
	#ENDIF
end function

function closeMidi() as integer
	#IFDEF __FB_WIN32__
	return midiOutClose (midi_handle)
	#ENDIF
end function

'emit a single event to the device
function shortMidi(event as UByte, a as UByte, b as UByte) as integer
	#IFDEF __FB_WIN32__
	if b = 255 then '-1
		return midiOutShortMSG(midi_handle,event SHL 0 + a SHL 8)
	else
		return midiOutShortMSG(midi_handle,event SHL 0 + a SHL 8 + b SHL 16)
	end if
	#ELSE
	'todo
	#ENDIF
end function

'emit a stream of bytes to the device
function longMidi(dat as UByte ptr, byval l as integer) as integer
	#IFDEF __FB_WIN32__
	'??? - api doesn't support streaming?
	#ELSE
	'???
	#ENDIF
	return -1
end function

Sub BufferEvent(event as UByte, a as Byte = -1, b as Byte = -1) 'pass -1 to a and b to ignore them
	shortMidi event, a, b
End Sub

Sub FlushMidiBuffer()
	exit sub 'nothing to do, as midi events are unbuffered
End Sub


Sub ResetMidi
	dim n as UByte, c as UByte
	flushmidibuffer

	for c = 0 to 15
		for n = 0 to 127
			BufferEvent(&H80 + c,n,0) 'turn off all notes
		next

		if midi_paused = NO then
			BufferEvent(&HB0 + c,121,-1) 'controller reset
			bufferevent(&HC0 + c,0,0) 'reset instruments
		end if
		flushmidibuffer ' to keep the buffer from growing /too/ big
	next
end sub

Sub AddJumpToEnd(head as MIDI_EVENT ptr)
	'traverse the tree - ugh
	dim curevent as MIDI_EVENT ptr', newhead as MIDI_EVENT ptr
	curevent = head
	do
		if curevent->next then
			curevent = curevent->next
		else
			exit do
		end if
	loop

	curevent->next = CreateMidiEvent(&H0,0,0,0)
	if curevent->next = 0 then exit sub

	curevent = curevent->next

	dim jumpdat(5) as UByte
	jumpdat(0) = ASC("O")
	jumpdat(1) = ASC("H")
	jumpdat(2) = ASC("R")
	jumpdat(3) = ASC("m")
	jumpdat(4) = &H02
	jumpdat(5) = &H0
	curevent->extralen = ubound(jumpdat) + 1
	curevent->extradata = Allocate(curevent->extralen)
	memcpy curEvent->extraData, @(jumpdat(0)), curevent->extralen
End Sub



'The music module needs to manage a list of temporary files to
'delete when closed, mainly for custom, so they don't get lumped
type delitem
	fname as zstring ptr
	nextitem as delitem ptr
end type

dim shared delhead as delitem ptr = null

sub music_init()
	music_init_count += 1
	'debug "music init = " & music_init_count
	if music_init_count <> 1 then exit sub

	openMidi

	'sound_init()
end sub

sub music_close()
	music_init_count -= 1
	'debug "music close = " & music_init_count
	if music_init_count <> 0 then exit sub
	midi_playing = NO
	midi_paused = NO

	if playback_thread then threadWait playback_thread: playback_Thread = 0

	if midi_song then
		FreeMidiEventList(midi_song)
		midi_song = 0
	end if

	if sound_song >= 0 then
		sound_stop(sound_song)
		sound_unload(sound_song)
	end if

	CloseMidi
	if delhead <> null then
		'delete temp files
		dim ditem as delitem ptr
		dim dlast as delitem ptr

		ditem = delhead
		while ditem <> null
			if isfile(*(ditem->fname)) then
				kill *(ditem->fname)
			end if
			Deallocate ditem->fname 'deallocate string
			dlast = ditem
			ditem = ditem->nextitem
			Deallocate dlast 'deallocate delitem
		wend
		delhead = null
	end if

	'sound_close
end sub

function music_get_info() as string
	return ""
end function

'NOTE: actually, audiere doesn't play anything on Windows at the moment, only MIDI works!
function music_supported_formats() as integer
	#ifdef __FB_WIN32__
		return FORMAT_BAM or FORMAT_MIDI or FORMAT_MODULES or FORMAT_OGG or FORMAT_MP3 or FORMAT_FLAC
	#else
		return FORMAT_MODULES or FORMAT_OGG or FORMAT_MP3 or FORMAT_FLAC
	#endif
end function

function sound_supported_formats() as integer
	return FORMAT_WAV or FORMAT_OGG or FORMAT_MP3 or FORMAT_FLAC
end function

function music_settings_menu() as bool
	return NO
end function

sub music_play overload(byval lump as Lump ptr, byval fmt as MusicFormatEnum)
end sub

sub music_play(filename as string, byval fmt as MusicFormatEnum)
	if music_init_count then
		dim songname as string = filename
		dim ext as string = lcase(justextension(songname))
		if fmt = FORMAT_BAM then
			dim midname as string
			dim as integer flen
			flen = filelen(songname)
			'use last 3 hex digits of length as a kind of hash,
			'to verify that the .bmd does belong to this file
			flen = flen and &h0fff
			midname = tmpdir & trimpath(songname) & "-" & lcase(hex(flen)) & ".bmd"
			'check if already converted
			if isfile(midname) = NO then
				bam2mid(songname, midname)
				'add to list of temp files
				dim ditem as delitem ptr
				if delhead = null then
					delhead = Allocate(sizeof(delitem))
					ditem = delhead
				else
					ditem = delhead
					while ditem->nextitem <> null
						ditem = ditem->nextitem
					wend
					ditem->nextitem = Allocate(sizeof(delitem))
					ditem = ditem->nextitem
				end if
				ditem->nextitem = null
				'Allocate space for zstring
				ditem->fname = Allocate(len(midname) + 1)
				*(ditem->fname) = midname 'set zstring
			end if
			songname = midname
			ext = "mid"
			fmt = FORMAT_MIDI
		end if

		'stop current song
		if midi_song <> 0 then
			midi_playing = NO
			if playback_thread then threadWait playback_thread: playback_Thread = 0
			FreeMidiEventList(midi_song)
			midi_song = 0
			midi_paused = NO
		end if
		'debug "sound_song = " & sound_song
		if sound_song <> -1 then
			sound_stop(sound_song)
			sound_unload(sound_song)
		end if

		if fmt = FORMAT_MIDI then
			midi_song = CreateMidiEventList(songname,@division)
			if midi_song = 0 then
				'debug "Could not load song " + songname
				exit sub
			end if

			converttorelative midi_song
			addJumpToEnd midi_song

			midi_paused = NO
			midi_playing = YES
			playback_thread = threadcreate(@PlayBackThread,0)
		else
			sound_song = sound_load(songname)
			sound_play(sound_song, -1, music_vol)
		end if
	end if
end sub

sub music_pause()
	if music_init_count then
		if midi_song then
			if midi_paused = NO then
				midi_paused = YES
			end if
		end if
		if sound_song >= 0 then
			sound_pause(sound_song)
		end if
	end if
end sub

sub music_resume()
	if music_init_count then
		if midi_song then
			midi_paused = NO
		end if
		if sound_song >= 0 then
			sound_play(sound_song, -1)
		end if
	end if
end sub

sub music_stop()
	if midi_song then music_pause()
	if sound_song >= 0 then sound_stop(sound_song)
end sub

sub music_setvolume(volume as single)
	music_vol = bound(volume, 0., 1.)
	if sound_song >= 0 then sound_setvolume(sound_song, music_vol)
end sub

function music_getvolume() as single
	if sound_song >= 0 then return sound_getvolume(sound_song)
	if midi_song then
		' Get the actual MIDI volume? Important on Windows?
	end if
	return music_vol
end function

Sub dumpdata(m as MIDI_EVENT ptr)
	dim d as string
	dim i as integer

	'for i = 0 to m->extralen - 1
	'	d += hex(m->extradata[i]) + " "
	'next
	'debug d
end sub

Sub PlayBackThread(byval dummy as any ptr)
	dim curtime as double, curevent as MIDI_EVENT ptr, starttime as double, delta as double, tempo as integer, delay as double
	dim played as integer, carry as double, pauseflag as integer
	dim labels(15) as MIDI_EVENT ptr, jumpcount(15) as integer, choruswas as MIDI_EVENT ptr
	labels(0) = midi_song
	for curtime = 0 to 15
		jumpcount(curtime) = -1
	next

	tempo = 500000 'assume 120 bmp

	curevent = midi_song

	updateDelay delay, tempo
	'debug "" & delay
	starttime = -1
	do while midi_playing
		if starttime = -1 then
			delta = 0
		else
			delta = timer - starttime  '+ carry
		end if
		curtime += delta * delay

		starttime = timer
		if midi_playing = NO then ESCAPE_SEQUENCE


		if cint(curevent->time) - int(curtime) > 0 then
			goto skipevents
		end if

		played = curtime
		do
			if curevent = 0 then exit do
			curtime -= curevent->time
			if curtime < 0 then curtime = 0 : exit do

			if midi_playing = NO then ESCAPE_SEQUENCE
			select case as const curevent->status
			case &HB0 to &HBF 'controller
				if curevent->data(0) = &H6F then 'rpg maker loop point
					labels(0) = curevent
				else
					'shortMidi curevent->status,curevent->data(0),curevent->data(1)
					BufferEvent curevent->status,CByte(curevent->data(0)),CByte(curevent->data(1))
				end if
			case &H80 to &H8F, &H90 to &H9F 'note on/off
				BufferEvent curevent->status,curevent->data(0),curevent->data(1) * music_vol
			case &HA0 to &HAF 'pressure
				BufferEvent curevent->status,curevent->data(0),curevent->data(1) * music_vol
			case &HC0 to &HCF 'program change
				BufferEvent curevent->status,curevent->data(0),-1
			case &HD0 to &HDF 'channel pressure
				BufferEvent curevent->status,curevent->data(0),-1
			case &HE0 to &HEF 'pitch bend
				BufferEvent curevent->status,curevent->data(0),curevent->data(1)
			case &H0, &HF0 'Sysex
				'first, check the id
				dim sysex_id as uinteger, p as integer
				p = 0
				sysex_id = *cptr(uinteger ptr, curevent->extradata + p)
				sysex_id = BE_LONG(sysex_id)
				if sysex_id = SIG_ID("O","H","R","m") then
					p += 4
					FlushMidiBuffer ' in case we loop
				sysex:
					'debug "Music code: " + hex(curevent->extradata[p])
					select case as const curevent->extradata[p]
					case &H0 'stop
						midi_playing = NO
						ESCAPE_SEQUENCE
					case &H1 'set label (should be removed to an initial scan)
						p += 1
						labels(curevent->extradata[p]) = curevent
					case &H2 'jump
						p += 1
						curevent = labels(curevent->extradata[p])
					case &H3 'limited jump
						p +=1
						if labels(curevent->extradata[p+1]) then
							if jumpcount(curevent->extradata[p]) = -1 then
								'new jump
								jumpcount(curevent->extradata[p]) = curevent->extradata[p+2] - 1
								curevent = labels(curevent->extradata[p+1])
							elseif labels(curevent->extradata[p]) = 0 then
								'end of jump
								jumpcount(curevent->extradata[p]) = -1
							elseif labels(curevent->extradata[p]) > 0 then
								'middle of jump
								jumpcount(curevent->extradata[p]) -= 1
								curevent = labels(curevent->extradata[p+1])
							else
								'invalid???
								jumpcount(curevent->extradata[p]) = -1
							end if
						end if
					case &H4 'chorus
						p+=1
						if choruswas = 0 then
							choruswas = curevent
							curevent = labels(curevent->extradata[p])
						end if
					case &H5 'return
						if choruswas then
							curevent = choruswas
							choruswas = 0
						end if
					case &H14 'sfx playing
						p += 1
						if sfxisplaying(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) then
							p += 4
							goto sysex
						end if
					case &H24 '!sfx  playing
						p += 1
						if NOT sfxisplaying(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) then
							p += 4
							goto sysex
						end if
					'case &H30 'load sound
						'p += 1
						'debug "load sound(" + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + "," + str(curevent->extradata[p+2]) + ")"
						'
						'sound_load cint(*(curevent->extradata + p + 2)),find_sfx_lump(BE_SHORT(*Cptr(short ptr, curevent->extradata + p)))
						'
						'p += 3
					case &H31 'play sound
						p += 1
						'debug "play sound (" + str(curevent->extradata[p]) + "," + str(curevent->extradata[p+1]) + ")"
						' Either loop forever (loopcount = YES = -1) or play once (loopcount = NO = 0)
						playsfx curevent->extradata[p], curevent->extradata[p + 1] <> 0
						p += 2
					case &H32 'stop sound
						p += 1
						'debug "stop sound (" + str(curevent->extradata[p]) + ")"
						stopsfx curevent->extradata[p]
						p += 1
					'case &H33 'free sound
						'p += 1
						'debug "free sound(" + str(curevent->extradata[p]) + ")"
						'sound_free curevent->extradata[p]
						' p += 1
					end select
				end if

			case &HFF
				dim metatype as integer
				metatype = curevent->data(0)
				if metatype = &H51 then
					tempo = curevent->extradata[0] SHL 16 + curevent->extradata[1] SHL 8 + curevent->extradata[2]
					updateDelay delay, tempo
				end if
			case else
				'debug("Unknown status: " + hex(curevent->status))
			end select
			curevent = curevent->next
		loop while midi_playing
		curtime = 0
		FlushMidiBuffer
	skipevents:

		if midi_playing = NO then ESCAPE_SEQUENCE

		if curevent = 0 then
			midi_playing = NO

			ESCAPE_SEQUENCE
		end if

		if midi_playing = NO then ESCAPE_SEQUENCE
		do
			sleep 10,1
			if midi_playing = NO then ESCAPE_SEQUENCE
			if midi_paused <> NO AND pauseflag = 0 then
				pauseflag = 1
				resetMidi ' kill stuck notes
			end if
		loop while midi_paused
		pauseflag = 0
	loop


endOfSong:
	midi_paused = NO
	resetMidi
	playback_thread = NULL

End Sub

Sub UpdateDelay(byref delay as double, byval tempo as integer)
	delay = division * 1000000 \ tempo - 1
end sub
