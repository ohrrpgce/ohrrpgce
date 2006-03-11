''
'' music_native.bas - External music functions implemented natively.
''
'' part of OHRRPGCE - see elsewhere for license details
''
' Many subs in this file are ported from SDL_Mixer (used under the GPL):
' /*
'     native_midi:  Hardware Midi support for the SDL_mixer library
'     Copyright (C) 2000,2001  Florian 'Proff' Schulze
' */
'
' Ported to FreeBasic by Mike Caron

option explicit


#include "crt.bi"

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
#IFDEF __FB_LINUX__
	'???
#ELSE
	#include once "windows.bi"
	#undef createevent
	'#undef lockfile '?
	#IFNDEF USE_ALLEGRO
		#include once "win/mmsystem.bi"
		#undef MIDIEvent
	#ELSE
		#include "externs.bi"
		DECLARE SUB win_set_window CDECL ALIAS "win_set_window" (BYVAL wnd as HWND)
		DECLARE FUNCTION win_get_window CDECL ALIAS "win_get_window"() as HWND
	#ENDIF
	
#ENDIF

'#ENDIF


#DEFINE ESCAPE_SEQUENCE Goto endOfSong

#IFDEF IS_GAME
TYPE Regtype
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE
#include gglobals.bi
#ENDIF





#include music.bi


Type MidiTrack
	len as integer
	data as UByte ptr
End Type

Type MidiFile
	division as integer
	nTracks as integer
	track as MidiTrack ptr
End Type


Type MIDIEvent
	time as UInteger
	status as UByte
	data(1) as UByte
	extraLen as UInteger
	extraData as UByte ptr
	tmp as Uinteger

	next as MIDIEvent ptr

End Type

'/* Some macros that help us stay endianess-independant */
#IF FALSE
#define BE_SHORT(x) (x)
#define BE_LONG(x) (x)
#else
#define BE_SHORT(x)	((((x) and &HFF) shl 8) OR (((x)shr 8) and &HFF))
#define BE_LONG(x)	((((x) and &H0000FF) shl 24) OR (((x)and &H00FF00) shl 8) OR (((x)and &HFF0000) shr 8) OR (((x) shr 24)and &HFF))
#endif

#define SIG_ID(a,b,c,d) (asc(a) shl 24 + asc(b) shl 16 + asc(c) shl 8 + asc(d) shl 0)

'extern
declare sub debug(s$)
declare sub bam2mid(infile as string, outfile as string)
declare function isfile(n$) as integer
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)


DECLARE Function GetVLQ(Byval track as MidiTrack ptr,ByRef p as integer) as integer
DECLARE Function CreateEvent(t as UInteger, e as UByte, a as UByte, b as UByte) as MIDIEvent ptr
DECLARE Function MidiTracktoStream(track as Miditrack ptr) as MidiEvent ptr
DECLARE function readmidifile(mididata as midifile ptr, fp as FILE ptr) as integer
DECLARE function CreateMIDIEventList(midifile as string, division as short ptr) as MIDIEvent ptr
Declare sub FreeMidiEventList(head as MidiEvent ptr)
DECLARE Sub PlayBackThread(dummy as integer)
DECLARE sub fade_daemon(byval targetvol as integer)


dim shared music_on as integer = 0
dim shared music_vol as integer
dim shared music_paused as integer
dim shared music_playing as integer
dim shared music_song as MIDIEvent ptr = NULL
dim shared orig_vol as integer = -1
dim shared playback_thread as integer
dim shared fade_thread as integer
dim shared inited_once as integer = 0

'for playback
dim shared division as integer



Function GetVLQ(Byval track as MidiTrack ptr,ByRef p as integer) as integer
	dim l as integer = 0
	dim c as UByte

	do while 1
		c = track->data[p]

		p += 1
		l += (c AND &H7F)
		if not (c AND &H80) then
			return l
		end if
		l = l shl 7
	loop

end function

' /* Create a single MIDIEvent */

Function CreateEvent(t as UInteger, e as UByte, a as UByte, b as UByte) as MIDIEvent ptr
	dim newEvent as MidiEvent ptr

	newEvent = calloc(1, len(MidiEvent))

	if newEvent then
		newEvent->time = t
		newEvent->status = e
		newEvent->data(0) = a
		newEvent->data(1) = b
	else
		'print "Error creating new event"
	end if

	return newEvent
end Function

Function ReadByte(d as UByte ptr, p as integer) as UByte
	ReadByte = d[p]
	p += 1
End Function

' /* Convert a single midi track to a list of MIDIEvents */

Function MidiTracktoStream(track as Miditrack ptr) as MidiEvent ptr
	dim atime as UInteger
	dim length as UInteger
	dim as UByte event, t, a, b
	Dim as UByte laststatus
	dim as UByte lastchan
	dim as integer currentPos
	dim as integer e


	Dim as MidiEvent ptr head, currentEvent
	head = CreateEvent(0,0,0,0)
	currentEvent = head


	do while not e

		if currentPos >= track->len then

			exit do
		end if

		atime += GetVLQ(track, currentPos)


		event = track->data[currentpos]  'ReadByte(track->data,currentpos)

		currentPos += 1

'
' 		/* Handle SysEx seperatly */

		if (event shr 4) = &HF then

			if event = &HFF then

				t = track->data[currentpos]  'ReadByte(track->data,currentpos)
				currentPos += 1
				if t = &H2f then

					exit do
				end if
			else

				t = 0
			end if
			length = GetVLQ(track, currentPos)
			if length > track->len then

			end if

'
' 			/* Create an event and attach the extra data, if any */

			currentEvent->next = CreateEvent(atime, event, t, 0)
 			currentEvent = currentEvent->next

 			if currentEvent = 0 then

 				FreeMidiEventList(head)
 				return 0
 			end if

 			if length then
 				currentEvent->extraLen = length
 				currentEvent->extraData = malloc(length)
 				memcpy currentEvent->extraData, @(track->data[currentPos]),length
				currentPos += length
			end if
		else
			a = event


			if a AND &H80 then '/* It's a status byte */

' 				/* Extract channel and status information */
				lastchan = a AND &HF
				laststatus = (a shr 4) AND &HF
' 				/* Read the next byte which should always be a data byte */
				a = track->data[currentpos] AND &H7F 'readbyte(track->data,currentPos) AND &H7F
				currentPos += 1
			end if

			if (laststatus >= &H8 AND laststatus <= &HB) OR laststatus = &HE then
				b = track->data[currentpos] AND &H7F 'readbyte(track->data,currentPos) AND &H7F
				currentPos += 1
				currentEvent->next = CreateEvent(atime, (laststatus shl 4) + lastchan, a, b)
				currentEvent = currentEvent->next
				if not currentEvent then
					FreeMidiEventList(head)
					return 0
				end if

			end if

			if (laststatus >= &HC AND laststatus <= &HD) then
				a = a AND &H7F
				currentEvent->next = CreateEvent(atime, (laststatus shl 4) + lastchan, a, CUByte(-1))
				currentEvent = currentEvent->next
				if not currentEvent then
					FreeMidiEventList(head)
					return 0
				end if

			end if

' 			default: /* Sysex already handled above */

		end if

	loop

	currentEvent = head->next
	free head

	return currentEvent


end function

' /*
'  *  Convert a midi song, consisting of up to 32 tracks, to a list of MIDIEvents.
'  *  To do so, first convert the tracks seperatly, then interweave the resulting
'  *  MIDIEvent-Lists to one big list.
'  */
function MiditoStream(midiData as midifile ptr) as midievent ptr
	dim as midievent ptr ptr track
	dim as midievent ptr head, currentEvent
	head = CreateEvent(0,0,0,0)
	currentEvent = head
	dim trackID as integer



	if not head then return 0
'

	track = cptr(MIDIEvent ptr ptr,calloc(1,len(MidiEvent ptr) * mididata->nTracks))

	if not track then return 0


' 	/* First, convert all tracks to MIDIEvent lists */
	for trackID = 0 to mididata->nTracks - 1
		track[trackID] = MIDITrackToStream(@mididata->track[trackID])
	next


' 	/* Now, merge the lists. */
' 	/* TODO */
 	Do
		dim lowestTime as Uinteger = 4294967295
		dim CurrentTrackID as integer= -1
'
' 		/* Find the next event */
		for trackID = 0 to mididata->nTracks - 1
 			if track[trackID] <> 0 then
 				if track[trackID]->time < lowestTime then
	 				currentTrackID = trackID
	 				lowestTime = track[currentTrackID]->time
 				end if
 			end if
		next

' 		/* Check if we processes all events */
 		if currentTrackID = -1 then exit do

 		currentEvent->next = track[currentTrackID]
 		track[currentTrackID] = track[currentTrackID]->next

 		currentEvent = currentEvent->next

 		lowestTime = 0
	Loop

' 	/* Make sure the list is properly terminated */
 	currentEvent->next = 0

 	currentEvent = head->next
    free track
 	free head	'/* release the dummy head event */
 	return currentEvent
end function

function readmidifile(mididata as midifile ptr, fp as FILE ptr) as integer

	dim i as integer
	dim ID as uinteger
	dim size as uinteger
	dim format as ushort
	dim tracks as ushort
	dim division as ushort

 	if not mididata then return 0
 	if not fp then return 0

' 	/* Make sure this is really a MIDI file */
 	fread(@ID, 1, 4, fp)
 	if BE_LONG(ID) <> SIG_ID("M","T","h","d") then return 0


' 	/* Header size must be 6 */
 	fread(@size, 1, 4, fp)
 	size = BE_LONG(size)
 	if size <> 6 then return 0


' 	/* We only support format 0 and 1, but not 2 */
 	fread(@format, 1, 2, fp)
 	format = BE_SHORT(format)
 	if format <> 0 and format <> 1 then return 0


 	fread(@tracks, 1, 2, fp)
 	tracks = BE_SHORT(tracks)
 	mididata->nTracks = tracks


'     /* Allocate tracks */
	mididata->track = cptr(MIDITrack ptr, calloc(1, len(MIDITrack) * mididata->nTracks))
	if not mididata->track then
		goto bail
	end if


' 	/* Retrieve the PPQN value, needed for playback */
 	fread(@division, 1, 2, fp)
 	mididata->division = BE_SHORT(division)


	for i = 0 to tracks - 1
 		fread(@ID, 1, 4, fp) ' /* We might want to verify this is MTrk... */

 		fread(@size, 1, 4, fp)
 		size = BE_LONG(size)

 		mididata->track[i].len = size
 		mididata->track[i].data = malloc(size)
 		if (not mididata->track[i].data) then
 			goto bail
		end if
 		fread(mididata->track[i].data, 1, size, fp)
	next

	return 1

bail:
	while i >= 0
 		if mididata->track[i].data then	free mididata->track[i].data
 		i -= 1
	wend

 	return 0
end function

function CreateMIDIEventList(midifile as string, division as short ptr) as MIDIEvent ptr
 	dim as FILE ptr fp
 	dim as MIDIFile ptr mididata
 	dim as MIDIEvent ptr eventList
 	dim as integer trackID

 	mididata = calloc(1, len(MIDIFile))
 	if not mididata then

 		return 0
 	end if

' 	/* Open the file */
 	fp = fopen(midifile, "rb")
 	if fp <> 0 then

' 		/* Read in the data */
 		if  not ReadMIDIFile(mididata, fp) then
 			free(mididata)
 			fclose(fp)
 			return 0
		end if
 		fclose(fp)
 	else

 		free(mididata)
 		return 0
	end if

 	if division then *division = mididata->division


 	eventList = MIDItoStream(mididata)

	for trackID = 0 to mididata->nTracks
 		if mididata->track[trackID].data then free(mididata->track[trackID].data)
	next
 	free(mididata->track)
    free(mididata)

    return eventList
end function

Sub ConvertToRelative(head as MidiEvent ptr)
 	dim lasttime as uinteger, curevent as MidiEvent ptr, lastevent as MidiEvent ptr

	lastevent = head
	lastevent->tmp = lastevent->time
	curevent = head->next
'	/* convert all times to relative */
 	Do while curevent

		curevent->tmp = curevent->time - lastevent->time

 		lastevent = curevent
 		curevent = curevent->next

 	Loop
	curevent = head
 	Do while curevent

		curevent->time = curevent->tmp
 		curevent = curevent->next

 	Loop
end Sub


sub FreeMidiEventList(head as MidiEvent ptr)
 	dim as MIDIEvent ptr cur, n
	on error goto error_handle
 	cur = head

 	do while cur
 		n = cur->next
 		if cur->extraData then free cur->extraData
		free cur
 		cur = n
 	loop
 	exit sub
 	
 	error_handle:
 	debug "Error #" + str$(err)
 	exit sub
end sub

#IFNDEF USE_ALLEGRO
#IFNDEF __FB_LINUX__
dim shared midi_handle as HMIDIOUT
#ENDIF
#ENDIF

dim shared midibuffer as UByte ptr, midibufferlen as integer, midibufferused as integer

function openMidi() as integer
	#IFNDEF USE_ALLEGRO
    #IFNDEF __FB_LINUX__
    dim moc as MIDIOUTCAPS
    midiOutGetDevCaps MIDI_MAPPER, @moc, len(MIDIOUTCAPS)
    'debug "Midi port supports Volume changes:" + str$(moc.dwSupport AND MIDICAPS_VOLUME)
    
    return midiOutOpen (@midi_handle,MIDI_MAPPER,0,0,0)
    #ENDIF
    #ELSE
    'see if allegro's been initialized
    if not inited_once then
    
    	win_set_window FB_Win32.Wnd
    	allegro_init
    	
    	inited_once = 1
    end if
    
    install_sound(-1,-1,"")
    load_midi_patches
    
    midibuffer = Allocate(60)
	midibufferlen = 60 '20 events, roughly
	midibufferused = 0
    #ENDIF
end function

function closeMidi() as integer
	#IFNDEF USE_ALLEGRO
    #IFNDEF __FB_LINUX__
    return midiOutClose (midi_handle)
    #ENDIF
    #ELSE
    
    remove_sound
    deallocate(midibuffer)
    midibufferused = 0
    midibufferlen = 0
    #ENDIF
end function

'emit a single event to the device
function shortMidi(event as UByte, a as UByte, b as UByte) as integer
	#IFNDEF USE_ALLEGRO
    #IFDEF __FB_LINUX__
    if b = 255 then '-1
    	return putc(event, midi_handle) OR putc(a, midi_handle)
    ELSE
    	return putc(event, midi_handle) OR putc(a, midi_handle) OR putc(b, midi_handle)
    END IF
    #ELSE
    if b = 255 then '-1
    	return midiOutShortMSG(midi_handle,event SHL 0 + a SHL 8)
    else
    	return midiOutShortMSG(midi_handle,event SHL 0 + a SHL 8 + b SHL 16)
    end if
    #ENDIF
    
    #ELSE

    dim d(0 to 2) as UByte
    d(0) = event
    d(1) = a
    d(2) = b
    if b = 255 then
    	midi_out @d(0), 2
    else
    	midi_out @d(0), 3
    end if
    
    #ENDIF   
end function

'emit a stream of bytes to the device
function longMidi(dat as UByte ptr, l as integer) as integer
	#IFNDEF USE_ALLEGRO
    #IFDEF __FB_LINUX__
    '???
    #ELSE
    '??? - api doesn't support streaming?
    #ENDIF
    
    #ELSE
    
    midi_out dat, l
    
    #ENDIF   
end function

function getVolMidi() as integer
	dim vol as integer, ret as integer
	#IFNDEF USE_ALLEGRO
	#IFDEF __FB_LINUX__
    return 0 '???
    #ELSE
    ret = midiOutGetVolume(midi_handle, @vol)
    vol = int((vol AND &HFFFF + vol SHR 16) / 2) 'average the left and right channel volumes.
    vol = vol SHR 12 'we only care about the most significant digit
    return vol
    #ENDIF
        
    #ELSE
    'hmm, does allegro have a way to read the volume?
    return 8
    #ENDIF
end function

sub setVolMidi(v as integer)
	dim vol as integer, ret as integer
	#IFNDEF USE_ALLEGRO
	#IFNDEF __FB_LINUX__
    vol = v
    vol = vol + vol shl 4 + vol shl 8 + vol shl 12
    vol += vol shl 16 'set left and right volumes
    ret = midiOutSetVolume (midi_handle, vol)
    #ENDIF
    
    #ELSE
    set_volume -1, int(v * 16 + (v \ 4)) 'don't care, midi volume
    #ENDIF
end sub

Sub BufferEvent(event as UByte, a as Byte = -1, b as Byte = -1) 'pass -1 to a and b to ignore them

#IFDEF USE_ALLEGRO
	dim goingtouse as integer, tmpbuf(2) as UByte, i as integer
	
	if midibuffer = NULL then
		midibuffer = Allocate(60)
		midibufferlen = 60 '20 events, roughly
		midibufferused = 0
	end if
	
	tmpbuf(0) = event: goingtouse = 1
	if a <> -1 then tmpbuf(1) = a: goingtouse = 2
	if b <> -1 then tmpbuf(2) = b: goingtouse = 3
	'debug "buffering event of " + Str$(goingtouse) + " bytes"
	do while midibufferlen - midibufferused < goingtouse
		dim newbuf as UByte ptr
		'debug "Reallocating buffer from " + str(midibufferlen) + " to " + str(midibufferlen * 2)
		newbuf = Reallocate(midibuffer, midibufferlen * 2)
		if newbuf = NULL then
			debug "FAILED TO REALLOCATE MIDI BUFFER, DROPPING EVENTS"
			'ehh... problem. The only thing we can do it toss the extra events...
			exit sub
		end if
		midibufferlen *= 2
		midibuffer = newbuf 'in case it moved
	loop
	
	'memcpy @tmpbuf(0), midibuffer + midibufferused, goingtouse
	for i = 0 to goingtouse - 1
		midibuffer[midibufferused +  i] = tmpbuf(i)
	next
	
	
	midibufferused += goingtouse	
	
#ELSE ' well, maybe the buffer is possible with the API. I dunno. TODO: check later
	shortMidi event, a, b
#ENDIF

End Sub



Sub FlushMidiBuffer()
#IFNDEF USE_ALLEGRO
	exit sub 'nothing to do, as midi events are unbuffered
#ELSE
	If midibuffer = NULL then
		exit sub 'no buffer? buh?
	end if
	
	#IFDEF USE_ALLEGRO 
	load_midi_patches
	#ENDIF
	longMidi midibuffer, midibufferused
	midibufferused = 0
	'should I null the buffer? ehh, let's see how it runs without
#ENDIF
End Sub


Sub ResetMidi
	dim n as UByte, c as UByte
	flushmidibuffer
	
	for c = 0 to 15
		for n = 0 to 127
			BufferEvent(&H80 + c,n,0) 'turn off all notes
		next
		
		BufferEvent(&HB0 + c,121,-1) 'controller reset
		if not music_paused then bufferevent(&HC0 + c,0,0) 'reset instruments
		flushmidibuffer ' too keep the buffer from growing /too/ big
	next
	
end sub

Sub AddJumpToEnd(head as MidiEvent ptr)
	'traverse the tree - ugh
	dim curevent as midievent ptr', newhead as midievent ptr
	curevent = head
	do
		if curevent->next then
			curevent = curevent->next
		else
			exit do
		end if
	loop

	curevent->next = CreateEvent(&H0,0,0,0)
	if not curevent->next then exit sub

	curevent = curevent->next

	dim jumpdat(5) as UByte
	jumpdat(0) = ASC("O")
	jumpdat(1) = ASC("H")
	jumpdat(2) = ASC("R")
	jumpdat(3) = ASC("m")
	jumpdat(4) = &H02
	jumpdat(5) = &H0
	curevent->extralen = ubound(jumpdat) + 1
	curevent->extradata = malloc(curevent->extralen)
	memcpy curEvent->extraData, @(jumpdat(0)),curevent->extralen

End Sub



'The music module needs to manage a list of temporary files to
'delete when closed, mainly for custom, so they don't get lumped
type delitem
	fname as zstring ptr
	nextitem as delitem ptr
end type

dim shared delhead as delitem ptr = null

sub music_init()
	dim version as uinteger
	if music_on = 0 then
		openMidi
		music_on = 1
	end if
end sub

sub music_close()
	if music_on = 1 then
		music_playing = 0
		music_paused = 0
		music_on = 0
		
		if fade_thread then
			threadwait fade_thread
		end if
		
		if playback_thread then threadWait playback_thread: playback_Thread = 0
		
		FreeMidiEventList(music_song)
		music_song = 0
		
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
				deallocate ditem->fname 'deallocate string
				dlast = ditem
				ditem = ditem->nextitem
				deallocate dlast 'deallocate delitem
			wend
			delhead = null
		end if
	end if
end sub

sub music_play(songname as string, fmt as music_format)
	if music_on = 1 then
		songname = rtrim$(songname)	'lose any added nulls

		if fmt = FORMAT_BAM then
			dim midname as string
			dim as integer bf, flen
			'get length of input file
			bf = freefile
			open songname for binary access read as #bf
			flen = lof(bf)
			close #bf
			'use last 3 hex digits of length as a kind of hash, 
			'to verify that the .bmd does belong to this file
			flen = flen and &h0fff
			midname = songname + "-" + lcase(hex(flen)) + ".bmd"
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

		'stop current song
		if music_song <> 0 then
			music_playing = 0
			if playback_thread then threadWait playback_thread: playback_Thread = 0
			FreeMidiEventList(music_song)
			music_song = 0
			music_paused = 0
		end if

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

	end if
end sub

sub music_pause()
	if music_on = 1 then
		if music_song > 0 then
			if music_paused = 0 then
				music_paused = 1
			end if
		end if
	end if
end sub

sub music_resume()
	if music_on = 1 then
		if music_song > 0 then
			music_paused = 0
		end if
	end if
end sub

sub music_stop()
	music_pause()
end sub

sub music_setvolume(vol as integer)
	music_vol = vol
	if music_on = 1 then
		setvolmidi vol
	end if
end sub

function music_getvolume() as integer
	music_getvolume = getvolmidi
end function

sub music_fade(targetvol as integer)
''Unlike the original version, this will pause everything else while it
''fades, so make sure it doesn't take too long

'lies, now it fades with a thread
	dim I as integer

	if fade_thread then
		threadwait fade_thread
	end if
	fade_thread = threadcreate (@fade_daemon,targetvol)
end sub

sub fade_daemon(byval targetvol as integer)
	dim vstep as integer = 1
	dim i as integer
	if music_vol > targetvol then vstep = -1
	for i = music_vol to targetvol step vstep
		music_setvolume(i)
		sleep 10
	next
	fade_thread = 0
end sub



Sub dumpdata(m as midiEvent ptr)
	dim d$, i as integer
	
	for i = 0 to m->extralen - 1
		d$ += hex$(m->extradata[i]) + " "
	next
	
	debug d$
	
end sub



Sub PlayBackThread(dummy as integer)
dim curtime as double, curevent as MIDIEvent ptr, starttime as double, delta as double, tempo as integer, delay as double
dim played as integer, carry as double, pauseflag as integer
dim labels(15) as midievent ptr, jumpcount(15) as integer, choruswas as MIDIEvent ptr
labels(0) = music_song
for curtime = 0 to 15
	jumpcount(15) = -1
next

tempo = 500000 'assume 120 bmp

curevent = music_song

gosub updateDelay

starttime = -1
do while music_playing
	if starttime = -1 then
		delta = 0
	else
		delta = timer - starttime '+ carry
	end if
	curtime += delta * delay

	starttime = timer
	if music_playing = 0 then ESCAPE_SEQUENCE


	if cint(curevent->time) - int(curtime) > 0 then
		goto skipevents
	end if

	played = curtime
	do
		if not curevent then exit do
		if (int(curtime) - curevent->time) < 0 then curtime = 0 : exit do
		curtime -= curevent->time

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
			BufferEvent curevent->status,curevent->data(0),curevent->data(1)
		case &HA0 to &HAF 'pressure
			BufferEvent curevent->status,curevent->data(0),curevent->data(1)
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
					if not choruswas then
						choruswas = curevent
						curevent = labels(curevent->extradata[p])
					end if
				case &H5 'return
					if choruswas then
						curevent = choruswas
						choruswas = 0
					end if
				#IFNDEF IS_CUSTOM
				#IF FALSE
				case &H6 'unset tag
					p +=1
					'debug "Unset tag # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p)))
					setbit tag(), 0, BE_SHORT(*Cptr(short ptr, curevent->extradata + p)), 0
				case &H7 'set tag
					p +=1
					'debug "Set tag # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p)))
					setbit tag(), 0, BE_SHORT(*Cptr(short ptr, curevent->extradata + p)), 1
				case &H8 'set variable
					p+=1
					'debug "Set variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " to " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) = BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2))
				case &H9 'variable ++
					p+=1
					'debug "Increment variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " by " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) += BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2))
				case &HA 'variable --
					p+=1
					'debug "Decrement variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " by " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) -= BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2))
				#ENDIF
				case &H10 'if tag
					p+=1
					'debug "If Tag # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p)))
					'dumpdata(curevent)
					if readbit (tag(), 0, BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) then
						p += 2
						goto sysex
					end if
				case &H11 'if variable
					p+=1
					'debug "If variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " = " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					if global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) = BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)) then
						p += 4
						goto sysex
					end if
				case &H12 'if variable >
					p+=1
					'debug "If variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " > " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					if global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) > BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)) then
						p += 4
						goto sysex
					end if
				case &H13 'if variable <
					p+=1
					'debug "If variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " < " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					if global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) < BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)) then
						p += 4
						goto sysex
					end if
				case &H20 'if !tag
					p+=1
					'debug "If Tag # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p)))
					if not readbit (tag(), 0, BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) then
						p += 2
						goto sysex
					end if
				case &H21 'if !variable
					p+=1
					'debug "If variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " = " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					if global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) <> BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)) then
						p += 4
						goto sysex
					end if
				case &H22 'if !variable >
					p+=1
					'debug "If variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " > " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					if global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) <= BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)) then
						p += 4
						goto sysex
					end if
				case &H23 'if !variable <
					p+=1
					'debug "If variable # " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) + " < " + str(BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)))
					if global(BE_SHORT(*Cptr(short ptr, curevent->extradata + p))) >= BE_SHORT(*Cptr(short ptr, curevent->extradata + p + 2)) then
						p += 4
						goto sysex
					end if
				#ENDIF
				end select
			end if
		case &HFF
			dim metatype as integer
			metatype = curevent->data(0)
			if metatype = &H51 then
				tempo = curevent->extradata[0] SHL 16 + curevent->extradata[1] SHL 8 + curevent->extradata[2]
				gosub updateDelay
			end if
		case else
			debug("Unknown status: " + hex(curevent->status))
		end select
		curevent = curevent->next
	loop while music_playing
	curtime = 0
	FlushMidiBuffer
skipevents:
	
	if music_playing = 0 then ESCAPE_SEQUENCE

	if not curevent then
		music_playing = 0

		ESCAPE_SEQUENCE
	end if

	if music_playing = 0 then ESCAPE_SEQUENCE
	do
		sleep 1,1
		if music_playing = 0 then ESCAPE_SEQUENCE
		if music_paused <> 0 AND pauseflag = 0 then
			pauseflag = 1
			resetMidi ' kill stuck notes
		end if
	loop while music_paused
	pauseflag = 0
loop


endOfSong:
resetMidi
playback_thread = 0
exit sub

updateDelay:
delay = division / tempo * 1000000
return
End Sub