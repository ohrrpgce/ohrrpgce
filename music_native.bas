''
'' music_native.bas - External music functions implemented natively
''
'' part of OHRRPGCE - see elsewhere for license details
''

'#include "crt.bi"

'glup
#include "config.bi"
#undef opaque

'uncomment this to try allegro
'#DEFINE USE_ALLEGRO

#IFDEF USE_ALLEGRO
	#include "allegro.bi"
	#undef default_palette
	#undef bitmap
	#undef fixed
	#undef arc
	#undef ellipse
	#undef floodfill
	#undef getpixel
	#undef setpixel
	#undef polygon
	#undef textout
#ENDIF

'#IFNDEF USE_ALLEGRO
#IFDEF __FB_WIN32__
	#undef getcommandline
	#include once "windows.bi"
	#undef copyfile
	#undef createevent
	#include "externs.bi"
	#include once "win/msacm.bi"
	#IFNDEF USE_ALLEGRO
		#include once "win/mmsystem.bi"
	#ELSE
		DECLARE SUB win_set_window CDECL ALIAS "win_set_window" (byval wnd as HWND)
		DECLARE FUNCTION win_get_window CDECL ALIAS "win_get_window"() as HWND
	#ENDIF
#ELSE
	'???
#ENDIF

'#ENDIF
#undef rectangle
#undef paintat
#undef max
#undef setmouse
#undef regtype
#undef isfile
#undef min
#undef opaque
#undef transparent
#undef ellipse
#include once "allmodex.bi"

#include once "common.bi"

#include once "const.bi"

#include once "util.bi"


#DEFINE ESCAPE_SEQUENCE Goto endOfSong


#include "music_native_subs.bas"


#include once "music.bi"

#IFNDEF USE_ALLEGRO
#IFDEF __FB_WIN32__
dim shared midi_handle as HMIDIOUT
#ELSE
dim shared midi_handle as FILE ptr
#ENDIF
#ENDIF


' Local functions

DECLARE Sub PlayBackThread(byval dummy as any ptr)
DECLARE Sub UpdateDelay(byref delay as double, byval tempo as integer)

' Module-local variables

dim shared music_on as integer = 0
dim shared music_vol as single = .5
dim shared music_paused as integer
dim shared music_playing as integer
dim shared music_song as MIDI_EVENT ptr = NULL
dim shared orig_vol as integer = -1
dim shared playback_thread as any ptr
dim shared inited_once as integer = 0

dim shared sound_song as integer = -1'if it's not a midi
'for playback
dim shared division as short



dim shared midibuffer as UByte ptr, midibufferlen as integer, midibufferused as integer

function openMidi() as integer
	#IFDEF __FB_WIN32__
	dim moc as MIDIOUTCAPS
	midiOutGetDevCaps MIDI_MAPPER, @moc, len(MIDIOUTCAPS)
	'debug "Midi port supports Volume changes:" + str(moc.dwSupport AND MIDICAPS_VOLUME)

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

		if not music_paused then BufferEvent(&HB0 + c,121,-1) 'controller reset
		if music_paused = 0 then bufferevent(&HC0 + c,0,0) 'reset instruments
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

	curevent->next = CreateEvent(&H0,0,0,0)
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
	music_on += 1
	'debug "music init = " & music_on
	if music_on <> 1 then exit sub

	openMidi

	'sound_init()
end sub

sub music_close()
	music_on -= 1
	'debug "music close = " & music_on
	if music_on <> 0 then exit sub
	music_playing = 0
	music_paused = 0

	if playback_thread then threadWait playback_thread: playback_Thread = 0

	if music_song then
		FreeMidiEventList(music_song)
		music_song = 0
	end if

	if sound_song >= 0 then
		sound_stop(sound_song, -1)
		UnloadSound(sound_song)
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

sub music_play overload(byval lump as Lump ptr, byval fmt as integer=FORMAT_BAM)

end sub

sub music_play(songname as string, byval fmt as integer)
	if music_on then
		songname = rtrim(songname)	'lose any added nulls
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
			if isfile(midname) = 0 then
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
		if music_song <> 0 then
			music_playing = 0
			if playback_thread then threadWait playback_thread: playback_Thread = 0
			FreeMidiEventList(music_song)
			music_song = 0
			music_paused = 0
		end if
		'debug "sound_song = " & sound_song
		if sound_song <> -1 then
			sound_stop(sound_song, -1)
			UnloadSound(sound_song)
		end if

		if fmt = FORMAT_MIDI then
			music_song = CreateMidiEventList(songname,@division)
			if music_song = 0 then
				'debug "Could not load song " + songname
				exit sub
			end if

			converttorelative music_song
			addJumpToEnd music_song

			music_paused = 0
			music_playing = 1
			playback_thread = threadcreate(@PlayBackThread,0)
		else
			sound_song = LoadSound(songname)
			sound_play(sound_song,-1,-1)
		end if
	end if
end sub

sub music_pause()
	if music_on then
		if music_song > 0 then
			if music_paused = 0 then
				music_paused = 1
			end if
		end if
		if sound_song >= 0 then
			sound_pause(sound_song, -1)
		end if
	end if
end sub

sub music_resume()
	if music_on then
		if music_song > 0 then
			music_paused = 0
		end if
		if sound_song >= 0 then
			sound_play(sound_song, -1, -1)
		end if
	end if
end sub

sub music_stop()
	if music_song > 0 then music_pause()
	if sound_song >= 0 then sound_stop(sound_song, -1)
end sub

sub music_setvolume(byval vol as single)
	music_vol = vol
	if music_on then
		'Don't know what this is meant to do
		'if music_song > 0 then music_vol = vol
		'need sound setting...
	end if
end sub

function music_getvolume() as single
'Note: this doesn't seem to work
	music_getvolume = music_vol
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
	labels(0) = music_song
	for curtime = 0 to 15
		jumpcount(curtime) = -1
	next

	tempo = 500000 'assume 120 bmp

	curevent = music_song

	updateDelay delay, tempo
	'debug "" & delay
	starttime = -1
	do while music_playing
		if starttime = -1 then
			delta = 0
		else
			delta = timer - starttime  '+ carry
		end if
		curtime += delta * delay

		starttime = timer
		if music_playing = 0 then ESCAPE_SEQUENCE


		if cint(curevent->time) - int(curtime) > 0 then
			goto skipevents
		end if

		played = curtime
		do
			if curevent = 0 then exit do
			curtime -= curevent->time
			if curtime < 0 then curtime = 0 : exit do

			if music_playing = 0 then ESCAPE_SEQUENCE
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
						music_playing = 0
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
						'sound_load cint(*(curevent->extradata + p + 2)),soundfile(BE_SHORT(*Cptr(short ptr, curevent->extradata + p)))
						'
						'p += 3
					case &H31 'play sound
						p += 1
						'debug "play sound (" + str(curevent->extradata[p]) + "," + str(curevent->extradata[p+1]) + ")"
						sound_play curevent->extradata[p], curevent->extradata[p + 1] <> 0
						p += 2
					case &H32 'stop sound
						p += 1
						'debug "stop sound (" + str(curevent->extradata[p]) + ")"
						sound_stop curevent->extradata[p]
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
		loop while music_playing
		curtime = 0
		FlushMidiBuffer
	skipevents:

		if music_playing = 0 then ESCAPE_SEQUENCE

		if curevent = 0 then
			music_playing = 0

			ESCAPE_SEQUENCE
		end if

		if music_playing = 0 then ESCAPE_SEQUENCE
		do
			sleep 10,1
			if music_playing = 0 then ESCAPE_SEQUENCE
			if music_paused <> 0 AND pauseflag = 0 then
				pauseflag = 1
				resetMidi ' kill stuck notes
			end if
		loop while music_paused
		pauseflag = 0
	loop


endOfSong:
	music_paused = 0
	resetMidi
	playback_thread = NULL

End Sub

Sub UpdateDelay(byref delay as double, byval tempo as integer)
	delay = division * 1000000 \ tempo - 1
end sub
