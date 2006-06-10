''
'' music_native.bas - External music functions implemented natively
''
'' part of OHRRPGCE - see elsewhere for license details
''

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
	#undef lockfile '?
	#include "externs.bi"
	#IFNDEF USE_ALLEGRO
		#include once "win/mmsystem.bi"
  #ELSE
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


#include music_native_subs.bas


#include music.bi

#IFNDEF USE_ALLEGRO
#IFNDEF __FB_LINUX__
dim shared midi_handle as HMIDIOUT
#ENDIF
#ENDIF



'extern

declare sub bam2mid(infile as string, outfile as string, useOHRm as integer)
declare function isfile(n$) as integer
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)


DECLARE Sub PlayBackThread(dummy as integer)
DECLARE sub fade_daemon(byval targetvol as integer)



dim shared music_on as integer = 0
dim shared music_vol as integer
dim shared music_paused as integer
dim shared music_playing as integer
dim shared music_song as MIDI_EVENT ptr = NULL
dim shared orig_vol as integer = -1
dim shared playback_thread as integer
dim shared fade_thread as integer
dim shared inited_once as integer = 0

'for playback
dim shared division as integer



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
    Deallocate(midibuffer)
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
		newbuf = OReallocate(midibuffer, midibufferlen * 2)
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
	curevent->extradata = Allocate(curevent->extralen)
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
				Deallocate ditem->fname 'deallocate string
				dlast = ditem
				ditem = ditem->nextitem
				Deallocate dlast 'deallocate delitem
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
				bam2mid(songname, midname,1)
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



Sub dumpdata(m as MIDI_EVENT ptr)
	dim d$, i as integer
	
	for i = 0 to m->extralen - 1
		d$ += hex$(m->extradata[i]) + " "
	next
	
	'debug d$
	
end sub



Sub PlayBackThread(dummy as integer)
dim curtime as double, curevent as MIDI_EVENT ptr, starttime as double, delta as double, tempo as integer, delay as double
dim played as integer, carry as double, pauseflag as integer
dim labels(15) as MIDI_EVENT ptr, jumpcount(15) as integer, choruswas as MIDI_EVENT ptr
labels(0) = music_song
for curtime = 0 to 15
	jumpcount(curtime) = -1
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
music_paused = 0
resetMidi
playback_thread = 0
exit sub

updateDelay:
delay = division / tempo * 1000000
return
End Sub



#include "win/dsound.bi"
DECLARE sub loadWaveFileToBuffer(fi as string, buf as LPDIRECTSOUNDBUFFER ptr)
DECLARE function isPlaying(slot as integer) as integer

TYPE sound_effect
  used as integer 'whether this slot is free
  
  paused as integer
  
  buf as LPDIRECTSOUNDBUFFER
  
END TYPE

dim shared sfx_slots(7) as sound_effect

'=== WARNING: The following code uses COM. COM is a compiler independant way of
'    doing OOP in Windows. It is also a pain for non-OOP languages, but unlike
'    regular OOP, this is workable. Unfortunately, COM is the only way to access
'    DirectX.

dim shared sound_inited as integer 'must be non-zero for anything but _init to work
dim shared ds8 as LPDIRECTSOUND8 'DirectSound8 object


sub sound_init
  dim res as HRESULT
  'if this were called twice, the world would end.
  if sound_inited then exit sub
  'debug "sound_init"
  'create DirectSound
  res = DirectSoundCreate8(NULL,@ds8,NULL)
  if res <> DS_OK then
    'hell
    debug "could not start up direct sound: " + hex(res)
    exit sub
  end if
  
  'debug "ds8 = 0x" + hex(cint(ds8))
  
  'next, set the co-op level
  'games use the "priority" level
  res = IDirectSound_SetCooperativeLevel(ds8,FB_win32.wnd,DSSCL_PRIORITY)
  if res <> DS_OK then
    'bugger
    debug "priority level failed: " + hex(res)
    exit sub
  end if

  
  'at this point, we're basically done
  
end sub

sub sound_close
  'trying to free something that's already freed... bad!
  if not sound_inited then exit sub
  
  IDirectSound_Release(ds8)
  ds8 = null
  
  dim i as integer
  for i = 0 to ubound(sfx_slots)
    with sfx_slots(i)
      .used = 0
      .paused = 0
      .buf =  NULL 'directsound frees this for us
    end with
  next
  
  sound_inited = 0
end sub

function sound_load(byval slot as integer, f as string) as integer
  'slot is the sfx_slots element to use, or -1 to automatically pick one
  'f is the file.
  dim i as integer
  
  if f = "" then return -1
  
  if slot = -1 then
    for i = 0 to ubound(sfx_slots)
    
      if not sfx_slots(i).used then
        slot = i
        exit for
      end if
    next
    
    if slot = ubound(sfx_slots) + 1 then return -1 'no free slots...
  end if
  
  with sfx_slots(slot)
    if .used then
      sound_free(slot)
    end if
    
    loadWaveFileToBuffer(f,@.buf)
    
    if .buf = NULL then return -1
    .used = 1
  end with
  
  return slot 'yup, that's all
  
end function

sub sound_free(byval slot as integer)
  with sfx_slots(slot)
    if sound_playing(slot) then sound_stop(slot)
    if .used then
      .used = 0
      IDirectSoundBuffer_Release(.buf)
    end if
  end with
end sub


sub sound_play(byval slot as integer, byval l as integer)
  with sfx_slots(slot)
    if not .used then exit sub
    if sound_playing(slot) and not .paused then exit sub
    if not .buf then exit sub
    
    if l then l = DSBPLAY_LOOPING
    
    .paused = 0

    IDirectSoundBuffer_Play(.buf, 0, 0, l)
  end with
end sub

sub sound_pause(byval slot as integer)
  with sfx_slots(slot)
    if not .used then exit sub
    if not sound_playing(slot) then exit sub
    if .paused then exit sub
    
    .paused = 1
    IDirectSoundBuffer_Stop(.buf)
  end with
end sub

sub sound_stop(byval slot as integer)
  with sfx_slots(slot)
    if not .used then exit sub
    if not sound_playing(slot) then exit sub
    
    .paused = 0
    
    IDirectSoundBuffer_Stop(.buf)
    IDirectSoundBuffer_SetCurrentPosition(.buf,0)
  end with
end sub

function sound_playing(byval slot as integer) as integer
  dim stat as integer
  
  with sfx_slots(slot)
    if not .used then return 0
    
    IDirectSoundBuffer8_GetStatus(.buf,@stat)
    
    if stat AND DSBSTATUS_PLAYING then return 1 ELSE return 0
  end with
end function

function sound_slots as integer
  return ubound(sfx_slots)
end function

#DEFINE ID(a,b,c,d) asc(a) SHL 0 + asc(b) SHL 8 + asc(c) SHL 16 + asc(d) SHL 24

sub loadWaveFileToBuffer(fi as string, buf as LPDIRECTSOUNDBUFFER ptr)
  'dim RIFF as integer = asc("R") SHL 0 + asc("I") SHL 8 + asc("F") SHL 16 + asc("F") SHL 24
  dim _RIFF as integer = ID("R","I","F","F")
  dim _WAVE as integer = ID("W","A","V","E")
  dim _fmt_ as integer = ID("f","m","t"," ")
  dim _data as integer = ID("d","a","t","a")
  
  'debug "_RIFF = " + hex(_RIFF)
  'debug "_WAVE = " + hex(_WAVE)
  'debug "_fmt_ = " + hex(_fmt_)
  'debug "_data = " + hex(_data)
  
  dim chnk_ID as integer
  dim chnk_size as integer
  
  dim fmt as WAVEFORMATEX, fmt_len as integer
  dim dat as ubyte ptr, dat_len as integer
  
  dim f as integer = Freefile
  
  'debug "LoadWaveFile"
  
  'ok. here we go
  
  open fi for binary as #f
  
  get #f,,chnk_ID 'file format
  
  if chnk_ID <> _RIFF then
    'not a RIFF, thus not a wave.
    debug "not a RIFF"
    close f
    exit sub
  end if
  
  get #f,,chnk_size 'don't particularly care, but whatever
  
  get #f,,chnk_ID
  
  if chnk_ID <> _WAVE then
    'not a wave file...
    debug "not a Wave"
    close f
    exit sub
  end if
  
  'ok, now we need to start moving through the chunks
  do until eof(f)
    '$DYNAMIC
    dim buff(0) as UByte, i as integer
    get #f,,chnk_ID
    get #f,,chnk_size
    if chnk_size MOD 2 then
      redim buff(chnk_size) 'odd size = rounded up
    else
      redim buff(chnk_size-1)     'even size = as is
    end if
    
    get #f,,buff()
    
    'debug "found chnk_ID of " + hex(chnk_ID)
    
    select case chnk_ID
    case _fmt_ 'wave format
      'debug "detected as format"
      fmt_len = chnk_size
      'fmt = Allocate(fmt_len)
      'for i = 0 to chnk_size
      '  fmt[i] = buff(i)
      'next
      
      memcpy(@fmt,@buff(0),len(WAVEFORMATEX))
    case _data 'actual data
      'debug "detected as data"
      dat_len = chnk_size
      dat = Allocate(dat_len)
      'for i = 0 to chnk_size
      '  dat[i] = buff(i)
      'next
      memcpy(dat,@buff(0),dat_len)
    case else
      'yawn
      'debug "buh, something else?"
    end select
  loop
  close f
  
  
  if fmt_len = 0 or dat_len = 0 then
    'uh... no fmt_ or data?
    debug "fake Wave!"
    exit sub
  end if
  
  
  'now, to copy the data to the direct sound buffer!
  
  'er... need to create it first.
  
  'debug "preparing buffer data"
  dim bd as DSBUFFERDESC
  bd.dwSize = len(DSBUFFERDESC)
  bd.dwFlags = DSBCAPS_CTRLPAN OR DSBCAPS_CTRLVOLUME OR DSBCAPS_CTRLFREQUENCY
  bd.dwBufferBytes = dat_len
  
  bd.lpwfxFormat = @fmt
  
  'debug "ds8 = 0x" + hex(cint(ds8))
  
  if IDirectSound8_CreateSoundBuffer(ds8,@bd,buf,NULL) <> DS_OK then
    'uh... fuck.
    debug "Could not create buffer"
    buf = null
    exit sub
  end if
  
  dim cursor as UByte ptr, cursor_count as integer
  
  'NOW we can copy data to it.
  'debug "locking buffer"
  'first, lock the buffer and get a pointer
  if IDirectSoundBuffer8_Lock(*buf, 0, 0, @cursor, @cursor_count, NULL, NULL, DSBLOCK_ENTIREBUFFER) <> DS_OK then
    'oh hell.
    'what to do?
    debug "could not lock buffer for some reason"
    IDirectSoundBuffer8_Release(*buf)
    buf = null
    exit sub
  end if
  
  'debug "copying waveform"
  'next, copy der data
  if cursor_count >= dat_len then
    cursor_count = dat_len
  end if
  memcpy(cursor, dat, dat_len)
  
  'debug "unlocking buffer"
  'finally, unlock
  IDirectSoundBuffer8_Unlock(*buf,cursor,cursor_count,NULL,NULL)
  
  'and... that's a wrap!
  
  '... I hope
    
  
  
end sub

