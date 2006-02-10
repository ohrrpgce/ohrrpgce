''
'' music_sdl.bas - External music functions implemented in SDL.
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
#include "windows.bi"
#include "win/mmsystem.bi"
#undef MIDIEVENT
#undef createevent



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



DECLARE Function GetVLQ(Byval track as MidiTrack ptr,ByRef p as integer) as integer
DECLARE Function CreateEvent(t as UInteger, e as UByte, a as UByte, b as UByte) as MIDIEvent ptr
DECLARE Function MidiTracktoStream(track as Miditrack ptr) as MidiEvent ptr
DECLARE function readmidifile(mididata as midifile ptr, fp as FILE ptr) as integer
DECLARE function CreateMIDIEventList(midifile as string, division as short ptr) as MIDIEvent ptr
Declare sub FreeMidiEventList(head as MidiEvent ptr)
DECLARE Sub PlayBackThread(dummy as integer)

DECLARE sub sysex_callback (byval as UByte ptr, byval as Uinteger)
' /* Get Variable Length Quantity */

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
		print "Error creating new event"
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


		event = ReadByte(track->data,currentpos)

		'currentPos += 1

'
' 		/* Handle SysEx seperatly */

		if (event shr 4) = &HF then

			if event = &HFF then

				t = ReadByte(track->data,currentpos)
				'currentPos += 1
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
				a = readbyte(track->data,currentPos) AND &H7F
				'currentPos += 1
			end if

			if (laststatus >= &H8 AND laststatus <= &HB) OR laststatus = &HE then
				b = readbyte(track->data,currentPos) AND &H7F
				'currentPos += 1
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
		print "error allocating tracks"
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
 			print "error loading track"
 			goto bail
		end if
 		fread(mididata->track[i].data, 1, size, fp)
	next

	return 1

bail:
	print "I/O Error"
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
 	Do while curevent

		curevent->time = curevent->tmp
 		curevent = curevent->next

 	Loop

end Sub


sub FreeMidiEventList(head as MidiEvent ptr)
 	dim as MIDIEvent ptr cur, n

 	cur = head

 	do while cur
 		n = cur->next
 		if cur->extraData then free (cur->extraData)
		free (cur)
 		cur = n
 	loop
end sub

dim shared midi_handle as HMIDIOUT

function openMidi() as integer
	return midiOutOpen (@midi_handle,-1,0,0,0)
end function

function closeMidi() as integer
	return midiOutClose (midi_handle)
end function

function shortMidi(event as UByte, a as UByte, b as UByte) as integer
	return midiOutShortMSG(midi_handle,event SHL 0 + a SHL 8 + b SHL 16)
end function

Sub ResetMidi
	dim n as UByte, c as UByte
	for c = 0 to 15
		for n = 0 to 127
			shortMidi(8 SHL 4 + c,n,0)
		next
	next
end sub

Sub AddJumpToEnd(head as MidiEvent ptr)
	'traverse the tree - ugh
	exit sub
	dim curevent as midievent ptr
	curevent = head
	do
		if curevent->next then
			curevent = curevent->next
		else
			exit do
		end if
	loop

	curevent->next = CreateEvent(&HF0,0,0,0)
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



'declare sub sysex_callback cdecl(byval as any ptr, byval as UByte ptr, byval as integer)

dim shared music_on as integer = 0
'dim shared music_song as FMOD_SOUND ptr = 0 'integer = 0
'dim shared fmod as FMOD_SYSTEM ptr
'dim shared fmod_channel as FMOD_CHANNEL ptr = 0
dim shared music_vol as integer
dim shared music_paused as integer
dim shared music_playing as integer
dim shared music_song as MIDIEvent ptr = NULL
dim shared orig_vol as integer = -1
dim shared sysex_cb as sub(byval as UByte ptr, byval as Uinteger)
dim shared playback_thread as integer

'for playback
dim shared division as integer

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
		CloseMidi
		music_playing = 0
		music_paused = 0
		music_on = 0
	end if
end sub

sub music_play(songname as string, fmt as music_format)
'would be nice if we had a routine that took the number as a param
'instead of the name, maybe abstract one into compat.bas?
	if music_on = 1 then
		songname = rtrim$(songname)	'lose any added nulls

		if fmt = FORMAT_BAM then
			dim midname as string
			midname = songname + ".mid"
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
			songname = songname + ".mid"
			fmt = FORMAT_MIDI
		end if

		'stop current song
		if music_song <> 0 then
			music_playing = 0
			debug "waiting for music thread..."
			if playback_thread then threadWait playback_thread: playback_Thread = 0
			debug "done"
			resetmidi
			'Mix_FreeMusic(music_song)
			debug "Freeing existing song"
			FreeMidiEventList(music_song)
			debug "done"
			music_song = 0
			music_paused = 0
		end if

		'music_song = Mix_LoadMUS(songname)
		music_song = CreateMidiEventList(songname,@division)
		if music_song = 0 then
			debug "Could not load song " + songname
			exit sub
		end if

		'Mix_HookMusic(@sysex_callback,NULL)
		sysex_cb = @sysex_callback

		'Mix_PlayMusic(music_song, -1)
		music_paused = 0
		music_playing = 1
		playback_thread = threadcreate(@PlayBackThread,0)

		'if orig_vol = -1 then
		'	orig_vol = Mix_VolumeMusic(-1)
		'end if

		'dim realvol as single
		'realvol = music_vol / 15
		'FMOD_Channel_SetVolume(fmod_channel, realvol)
		'if music_vol = 0 then
		'	Mix_VolumeMusic(0)
		'else
		'	'add a small adjustment because 15 doesn't go into 128
		'	Mix_VolumeMusic((music_vol * 8) + 8)
		'end if
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
' 		if music_vol = 0 then
' 			Mix_VolumeMusic(0)
' 		else
' 			'add a small adjustment because 15 doesn't go into 128
' 			Mix_VolumeMusic((music_vol * 8) + 8)
' 		end if
	end if
end sub

function music_getvolume() as integer
	music_getvolume = music_vol
end function

sub music_fade(targetvol as integer)
'Unlike the original version, this will pause everything else while it
'fades, so make sure it doesn't take too long
	dim vstep as integer = 1
	dim i as integer

	if music_vol > targetvol then vstep = -1
	for i = music_vol to targetvol step vstep
		music_setvolume(i)
		sleep 10
	next
end sub





Sub PlayBackThread(dummy as integer)
dim curtime as integer, curevent as MIDIEvent ptr, starttime as double, delta as double, tempo as integer, delay as double
tempo = 500000 'assume 120 bmp

curevent = music_song

gosub updateDelay

starttime = timer
do while music_playing
	delta = timer - starttime

	curtime += delta / delay
	starttime = timer
	'print cint(curevent->time) - curtime
	if music_playing = 0 then exit sub

	if cint(curevent->time) - curtime > 0 then
		goto skipevents
	end if


	do
		if not curevent then exit do
		if cint(curevent->time) - curtime > 0 then exit do
		if music_playing = 0 then exit sub
		curtime = 0
		select case curevent->status
		case &H80 to &HF0 'reg-oo-lar event
			shortMidi curevent->status,curevent->data(0),curevent->data(1)
		case &HF0 'Sysex
			if sysex_cb then
				sysex_cb(curevent->extraData, curevent->extralen)
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
		end select
		curevent = curevent->next
	loop while music_playing
	curtime = 0
skipevents:

	if music_playing = 0 then exit sub

	if not curevent then
		curevent = music_song
		curtime = 0
		starttime = timer
	end if

	do
		sleep 10,1
		if music_playing = 0 then exit sub
	loop while music_paused

loop

exit sub

updateDelay:
debug "old delay:" + str(delay)
delay = tempo / division / 1050000
debug "new delay:" + str(delay)
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