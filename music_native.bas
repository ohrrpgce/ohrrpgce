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
#IFDEF __FB_LINUX__
'???
#ELSE
#include "windows.bi"
#include "win/mmsystem.bi"
#undef MIDIEVENT
#undef createevent
#undef lockfile
#ENDIF


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
#if false
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

DECLARE sub sysex_callback (byval as UByte ptr, byval as Uinteger)


dim shared music_on as integer = 0
dim shared music_vol as integer
dim shared music_paused as integer
dim shared music_playing as integer
dim shared music_song as MIDIEvent ptr = NULL
dim shared orig_vol as integer = -1
dim shared sysex_cb as sub(byval as UByte ptr, byval as Uinteger)
dim shared playback_thread as integer
dim shared fade_thread as integer

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

	'print head



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
				currentEvent->next = CreateEvent(atime, (laststatus shl 4) + lastchan, a, 0)
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
' static MIDIEvent *MIDItoStream(MIDIFile *mididata)
' {
function MiditoStream(midiData as midifile ptr) as midievent ptr
' 	MIDIEvent **track;
' 	MIDIEvent *head = CreateEvent(0,0,0,0);	/* dummy event to make handling the list easier */
' 	MIDIEvent *currentEvent = head;
' 	int trackID;
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

' static int ReadMIDIFile(MIDIFile *mididata, FILE *fp)
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
		'print "error allocating tracks"
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
 			'print "error loading track"
 			goto bail
		end if
 		fread(mididata->track[i].data, 1, size, fp)
	next

	return 1

bail:
	'print "I/O Error"
	while i >= 0
 		if mididata->track[i].data then	free mididata->track[i].data
 		i -= 1
	wend

 	return 0
end function

' MIDIEvent *CreateMIDIEventList(char *midifile, Uint16 *division)
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

' 	for(trackID = 0; trackID < mididata->nTracks; trackID++)
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

		'print curevent->tmp
		'sleep 10

 		lastevent = curevent
 		curevent = curevent->next

 	Loop
	curevent = head
	'debug "Event times:"
 	Do while curevent

		curevent->time = curevent->tmp
	'	debug "->" + str(curevent->tmp)
 		curevent = curevent->next

 	Loop
	'debug "End of event times"
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

#IFDEF __FB_LINUX__
dim shared midi_handle as FILE ptr
#ELSE
dim shared midi_handle as HMIDIOUT
#ENDIF
function openMidi() as integer
    #IFDEF __FB_LINUX__
    midi_handle = fopen("/dev/sequencer","w")
    return midi_handle = NULL
    #ELSE
    dim moc as MIDIOUTCAPS
    midiOutGetDevCaps MIDI_MAPPER, @moc, len(MIDIOUTCAPS)
    'debug "Midi port supports Volume changes:" + str$(moc.dwSupport AND MIDICAPS_VOLUME)
    
    return midiOutOpen (@midi_handle,MIDI_MAPPER,0,0,0)
    #ENDIF
end function

function closeMidi() as integer
    #IFDEF __FB_LINUX__
    return fclose(midi_handle)
    #ELSE
    return midiOutClose (midi_handle)
    #ENDIF
end function

function shortMidi(event as UByte, a as UByte, b as UByte) as integer
    #IFDEF __FB_LINUX__
    return putc(event, midi_handle) OR putc(a, midi_handle) OR putc(b, midi_handle)
    #ELSE
    return midiOutShortMSG(midi_handle,event SHL 0 + a SHL 8 + b SHL 16)
    #ENDIF
end function

function getVolMidi() as integer
	dim vol as integer, ret as integer
	#IFDEF __FB_LINUX__
    return 0 '???
    #ELSE
    ret = midiOutGetVolume(midi_handle, @vol)
    vol = int((vol AND &HFFFF + vol SHR 16) / 2) 'average the left and right channel volumes.
    vol = vol SHR 12 'we only care about the most significant digit
    return vol
    #ENDIF
end function

sub setVolMidi(v as integer)
	dim vol as integer, ret as integer
	#IFDEF __FB_LINUX__
    '???
    #ELSE
    vol = v
    vol = vol + vol shl 4 + vol shl 8 + vol shl 12
    vol += vol shl 16 'set left and right volumes
    'debug "vol = " + HEX$(vol)
    ret = midiOutSetVolume (midi_handle, vol)
    
    #ENDIF
end sub



Sub ResetMidi
	dim n as UByte, c as UByte
	'debug "RESET!"
	for c = 0 to 15
		for n = 0 to 127
			shortMidi(&H80 + c,n,0) 'turn off all notes
		next
		shortMidi(&HB0 + c,121,0) 'controller reset
		if not music_paused then shortMidi(&HC0 + c,0,0) 'reset instruments
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

	'head->time += 3
	'newHead = createEvent(&

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
			'debug "waiting for music thread..."
			if playback_thread then threadWait playback_thread: playback_Thread = 0
			'debug "done"
			'debug "Freeing existing song"
			FreeMidiEventList(music_song)
			'debug "done"
			music_song = 0
			music_paused = 0
		end if

		'music_song = Mix_LoadMUS(songname)
		music_song = CreateMidiEventList(songname,@division)
		if music_song = 0 then
			debug "Could not load song " + songname
			exit sub
		end if

		converttorelative music_song
		addJumpToEnd music_song

		'Mix_HookMusic(@sysex_callback,NULL)
		sysex_cb = @sysex_callback

		'Mix_PlayMusic(music_song, -1)
		music_paused = 0
		music_playing = 1
		playback_thread = threadcreate(@PlayBackThread,0)

	end if
end sub

sub music_pause()
	if music_on = 1 then
		if music_song > 0 then
			if music_paused = 0 then
				'Mix_PauseMusic	'doesn't seem to work
				music_paused = 1
			end if
		end if
	end if
end sub

sub music_resume()
	if music_on = 1 then
		if music_song > 0 then
			'Mix_ResumeMusic
			music_paused = 0
		end if
	end if
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

	'if music_vol > targetvol then vstep = -1
	'for i = music_vol to targetvol step vstep
	'	music_setvolume(i)
	'	sleep 10
	'next
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
		delta = timer - starttime + carry
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
		'curtime = 0
		select case as const curevent->status
		case &HB0 to &HBF 'controller
			if curevent->data(0) = &H6F then 'rpg maker loop point
				labels(0) = curevent
			else
				shortMidi curevent->status,curevent->data(0),curevent->data(1)
			end if
		case &H80 to &HEF 'reg-oo-lar event
			shortMidi curevent->status,curevent->data(0),curevent->data(1)
		case &H0, &HF0 'Sysex
			'if sysex_cb then
			'	sysex_cb(curevent->extraData, curevent->extralen)
			'end if
			'debug("Sysex")
			'first, check the id
			dim sysex_id as uinteger, p as integer
			p = 0
			sysex_id = *cptr(uinteger ptr, curevent->extradata + p)
			sysex_id = BE_LONG(sysex_id)
			'debug str(sysex_id) + " " + str(SIG_ID("O","H","R","m"))
			if sysex_id = SIG_ID("O","H","R","m") then
			p += 4
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
			select case metatype
			case &H51
				tempo = curevent->extradata[0] SHL 16 + curevent->extradata[1] SHL 8 + curevent->extradata[2]

				gosub updateDelay
				'print "New tempo: ", tempo
				'sleep
			case &H7F, &H5, &H3, &H20, &H58, &H59
				'ignore
			case else
				'print "Unknown event (" + hex(metatype) + ")"
			end select
		case else
			debug("Unknown status: " + hex(curevent->status))
		end select
		curevent = curevent->next
	loop while music_playing
	curtime = 0
skipevents:

	if music_playing = 0 then ESCAPE_SEQUENCE

	if not curevent then
		music_playing = 0

		ESCAPE_SEQUENCE
	end if

	if music_playing = 0 then ESCAPE_SEQUENCE
	do
		sleep 5,1
		if music_playing = 0 then ESCAPE_SEQUENCE
		if music_paused <> 0 AND pauseflag = 0 then
			'debug "detecting pause, reseting"
			pauseflag = 1
			resetMidi ' kill stuck notes
		end if
	loop while music_paused
	pauseflag = 0
loop


endOfSong:
'debug "End of Song!"
resetMidi
playback_thread = 0
exit sub

updateDelay:
delay = tempo / division
delay /= 1000000
delay = 1 / delay
return
End Sub




sub sysex_callback (byval d as UByte ptr, byval l as Uinteger)
	'debug str(l) + "|" + str(d[1])
	'dim i as integer, s as string
	'for i = 0 to 32
	'	s += hex(d[i]) + " "
	'next
	'debug s

	debug("sysex")

end sub

