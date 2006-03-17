''
'' music_native_subs.bas - Routines used by music_native*
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

DECLARE Function GetVLQ(Byval track as MidiTrack ptr,ByRef p as integer) as integer
DECLARE Function CreateEvent(t as UInteger, e as UByte, a as UByte, b as UByte) as MIDIEvent ptr
DECLARE Function MidiTracktoStream(track as Miditrack ptr) as MidiEvent ptr
DECLARE function readmidifile(mididata as midifile ptr, fp as FILE ptr) as integer
DECLARE function CreateMIDIEventList(midifile as string, division as short ptr) as MIDIEvent ptr
Declare sub FreeMidiEventList(head as MidiEvent ptr)

'in fbcompat.bas
DECLARE Function OAllocate(Byval as integer) as Any Ptr
DECLARE Function OCAllocate(Byval as integer) as Any Ptr
DECLARE Sub ODeallocate(Byval as Any Ptr)
DECLARE Function OReallocate(Byval as Any Ptr, Byval as integer) as Any Ptr
declare sub debug(s$)


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

	newEvent = OCAllocate(len(MidiEvent))

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
 				currentEvent->extraData = OAllocate(length)
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
	ODeallocate head

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

	track = cptr(MIDIEvent ptr ptr,OCAllocate(len(MidiEvent ptr) * mididata->nTracks))

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
    ODeallocate track
 	ODeallocate head	'/* release the dummy head event */
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
	mididata->track = cptr(MIDITrack ptr, OCAllocate(len(MIDITrack) * mididata->nTracks))
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
 		mididata->track[i].data = OAllocate(size)
 		if (not mididata->track[i].data) then
 			goto bail
		end if
 		fread(mididata->track[i].data, 1, size, fp)
	next

	return 1

bail:
	while i >= 0
 		if mididata->track[i].data then	ODeallocate mididata->track[i].data
 		i -= 1
	wend

 	return 0
end function

function CreateMIDIEventList(midifile as string, division as short ptr) as MIDIEvent ptr
 	dim as FILE ptr fp
 	dim as MIDIFile ptr mididata
 	dim as MIDIEvent ptr eventList
 	dim as integer trackID

 	mididata = OCAllocate(len(MIDIFile))
 	if not mididata then

 		return 0
 	end if

' 	/* Open the file */
 	fp = fopen(midifile, "rb")
 	if fp <> 0 then

' 		/* Read in the data */
 		if  not ReadMIDIFile(mididata, fp) then
 			ODeallocate(mididata)
 			fclose(fp)
 			return 0
		end if
 		fclose(fp)
 	else

 		ODeallocate(mididata)
 		return 0
	end if

 	if division then *division = mididata->division


 	eventList = MIDItoStream(mididata)

	for trackID = 0 to mididata->nTracks
 		if mididata->track[trackID].data then ODeallocate(mididata->track[trackID].data)
	next
 	ODeallocate(mididata->track)
    ODeallocate(mididata)

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
 		if cur->extraData then ODeallocate cur->extraData
		ODeallocate cur
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