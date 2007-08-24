''
'' music_native.bas - External music functions implemented natively
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit


'#include "crt.bi"

'glup
#include "compat.bi"
#include "util.bi"

'#IFNDEF USE_ALLEGRO
#IFDEF __FB_LINUX__
	'???
#ELSE
	#undef getcommandline
	#include once "windows.bi"
	#undef createevent
	#include "externs.bi"
	#include once "win/msacm.bi"
	#include once "win/mmsystem.bi"
#ENDIF

#undef rectangle
#undef copyfile
#include once "allmodex.bi"

#include once "common.bi"
#include once "const.bi"


#IFDEF IS_GAME
#include once "gglobals.bi"
#ELSE
dim shared tag(2000), global(1025)
#ENDIF


#include "music_native_subs.bas"


#include once "music.bi"



#undef MIDIEVENT 'fb's MIDIEVENT is wrong
type MIDIEVENT
  dwDeltaTime as integer
  dwReserved as integer
  dwEvent as integer
end type




#DEFINE USE_DEBUG_WINDOW 0



'extern
extern tmpdir$

declare sub bam2mid(infile as string, outfile as string, useOHRm as integer)


DECLARE sub fade_daemon(byval targetvol as integer)
DECLARE Sub UpdateDelay(BYREF delay as integer, tempo as integer)
DECLARE Sub StreamCallback(Byval handle as HMIDIOUT, byval umsg as Uinteger, byval dwInstance as UInteger, byval dwParam1 as UInteger, byval dwParam2 as UInteger)
DECLARE Sub PrepareNextBeat(byval unused as integer)
Declare Sub ResetInternals()
DECLARE function streamPosition as integer


dim shared music_on as integer = 0
dim shared music_vol as integer = 8
dim shared music_vol_mod as single = .5
dim shared music_paused as integer
dim shared music_playing as integer

dim shared music_song as MIDI_EVENT ptr = NULL
dim shared song_ptr as MIDI_EVENT ptr = NULL

dim shared orig_vol as integer = -1
dim shared fade_thread as intptr
dim shared inited_once as integer = 0

dim shared device as any ptr

dim shared sound_song as integer = -1'if it's not a midi
'for playback
dim shared division as short

dim shared looppoint as MIDI_EVENT ptr = NULL

dim shared origvol as integer

dim shared midibuffer as UByte ptr, midibufferlen as integer, midibufferused as integer

dim shared current_beat as MIDIEVENT ptr = NULL, current_head as MIDIHDR ptr = NULL
dim shared buffer_beat as MIDIEVENT ptr = NULL, buffer_head as MIDIHDR ptr = NULL
dim shared current_size as integer, buffer_size as integer

dim shared skip_ticks as integer
dim shared buffer_thread as intptr = NULL

declare function DebugWndProc (byval hwnd as HWND, byval uMsg as uinteger, byval wParam as WPARAM, byval lParam as LPARAM)
Declare sub DebugWindowThread(byval useless as integer)

#include once "fbgfx.bi"

'this should probably be in an include...

Extern FB_Win32 Alias "fb_win32" As WIN32DRIVER

Function ProgInstance() As HINSTANCE
  Return FB_Win32.hInstance
End Function

 

dim shared DebugWndClass as ATOM '??? dunno.
dim shared DebugWnd as HWND
dim shared clasName as ZString * 9
Sub initDebugWindow
  dim clas as WNDCLASSEX
  'dim mainHandle as integer
  
  clasName = "WndMusDbg"
  
  'screencontrol GET_WINDOW_HANDLE, mainHandle
  
  with clas
    .cbSize = len(WNDCLASSEX)
    .lpszClassName = @"WndMusDbg"
    .hInstance = ProgInstance
    .hbrBackground = cptr(HBRUSH, COLOR_WINDOW)
    .lpfnWndProc = @DebugWndProc
    .style = CS_NOCLOSE
    .hCursor = LoadImage(0, MAKEINTRESOURCE(OCR_NORMAL), IMAGE_CURSOR, 0, 0, LR_DEFAULTSIZE OR LR_SHARED)
  end with
  
  DebugWndClass = RegisterClassEx(@clas)
  
  if DebugWndClass = 0 then
    'failed.
    debug "WndClass failed"
    exit sub
  end if
  
  ThreadCreate @DebugWindowThread, 0
  
end sub

sub killDebugWindow
  'SendMessage(DebugWnd, WM_USER, 0, 0)
  'debug "asking the window to die plzkthx"
  SendMessage(DebugWnd, WM_USER, 0, 0)
  UnregisterClass("WndMusDbg", ProgInstance)
end sub

sub DebugWindowThread(byval useless as integer)
  dim i as integer, msg as MSG
  
  DebugWnd = CreateWindowEx(0, "WndMusDbg", "Debug", WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, 0, 200, 200, 0, 0, ProgInstance, 0)
  
  if DebugWnd = 0 then
    'failed
    debug "CreateWindow failed (" & GetLastError & ")"
    exit sub
  end if
  
  ShowWindow(DebugWnd, SW_SHOWDEFAULT)
  
  do 
    i = GetMessage(@msg, DebugWnd, 0, 0)
    if i = 0 then exit do
    if i = -1 then
      debug "Error in GetMessage (" & GetLastError & ")"
      exit do
    end if
    
    TranslateMessage(@msg)
    DispatchMessage(@msg)
  loop
end sub

function DebugWndProc(byval hwnd as HWND, byval uMsg as uinteger, byval wParam as WPARAM, byval lParam as LPARAM) as LRESULT
  
  select case as const uMsg
  case WM_USER
    'this thread created the window, this thread must destroy it...
    DestroyWindow(DebugWnd)
  case WM_CREATE
    'debug "WM_CREATE"
    SetTimer(hWnd, 1, 100, 0)
    return 0
  case WM_DESTROY
    'debug "WM_DESTROY"
    KillTimer(hwnd, 1)
    PostQuitMessage(0)
    return 0
  case WM_TIMER
    'debug "WM_TIMER"
    RedrawWindow(hwnd, 0, 0, RDW_INVALIDATE OR RDW_ERASE)
    'SetTimer(DebugWnd, 1, 100, 0)
    return 0
  case WM_PAINT
    'debug "WM_PAINT"
    dim dc as HDC, o as string, ps as PAINTSTRUCT
    dc = BeginPaint(hwnd, @ps)
    
    SetBkMode(dc, TRANSPARENT)
    
    if music_song then
      'draw the buffer size
      o = "Buffer size: " & str(buffer_size)
      ExtTextOut(dc, 5, 10, 0, 0, strptr(o), len(o), 0)
      
      o = "Division: " & str(division)
      ExtTextOut(dc, 5, 25, 0, 0, strptr(o), len(o), 0)
      
      o = "Position: " & str(StreamPosition)
      ExtTextOut(dc, 5, 40, 0, 0, strptr(o), len(o), 0)
    else
      o = "Not playing a midi"
      ExtTextOut(dc, 5, 10, 0, 0, strptr(o), len(o), 0)
    end if
    
    o = "music_song: " & hex(music_song)
    ExtTextOut(dc, 5, 65, 0, 0, strptr(o), len(o), 0)
    
    EndPaint(hwnd, @ps)
    return 0
  case else
    return DefWindowProc(hwnd,umsg,wParam,lParam)
  end select
  

end function



function openMidi() as integer
    #IFNDEF __FB_LINUX__
    'dim moc as MIDIOUTCAPS
    'midiOutGetDevCaps MIDI_MAPPER, @moc, len(MIDIOUTCAPS)
    'debug "Midi port supports Volume changes:" + str$(moc.dwSupport AND MIDICAPS_VOLUME)

    dim mididev as integer = 0, erro as MMRESULT
    'debug "opening midi device"
    erro = midiStreamOpen(@device, @mididev, 1, cint(@StreamCallback), 0, CALLBACK_FUNCTION)

    if erro then
      'debug "midiStreamOpen error"
      'if erro = MMSYSERR_BADDEVICEID then debug "Not sure, but the device ID is bad."
      'if erro = MMSYSERR_INVALPARAM then debug "Doesn't like one of my parameters"
      'if erro = MMSYSERR_NOMEM then debug "Can't lock memory or something"
    end if

    origvol = music_getvolume

    #IF USE_DEBUG_WINDOW
    initDebugWindow
    #ENDIF
    
    Return erro
    #ENDIF
end function

function closeMidi() as integer
#IFNDEF __FB_LINUX__

    #IF USE_DEBUG_WINDOW
    killDebugWindow
    #ENDIF
    
    ResetInternals
    
    return 0


#ENDIF
end function

sub dumpMidi_event overload(byval m as MIDI_EVENT ptr, byval c as integer = -1, byval f as string = "mididump.txt")
  dim i as integer, fh as integer, w as string

  'debug "dumping internal buffer"

  fh = freefile
  open f for output as #fh

  DO while m <> NULL and (c = -1 OR (c <> -1 AND i < c))
    w = ""
    w = "D = " & m->time & ", "
    w = w & "S = " & m->status
    select case (m->status SHR 4) and &HF
      case &H8
        w = w & " (Note on)"
      case &H9
        w = w & " (Note off)"
      case &HA
        w = w & " (Pressure)"
      case &HB
        w = w & " (Controller)"
      case &HC
        w = w & " (Program change)"
      case &HD
        w = w & " (Channel pressure)"
      case &HE
        w = w & " (Pitch bend)"
      case &HF
        if m->status = &HFF then w = w & " (Sysex)" else w = w & " (Meta)"
      case else
        w = w & " (Unknown!?)"
    end select
    w = w & ", "
    w = w & "A = " & cbyte(m->data(0)) & ", "
    w = w & "B = " & cbyte(m->data(1))

    print #fh, w
    m = m->next
    i+=1
  LOOP

  close #fh
end sub

sub dumpMidi_event (byval m as MIDIEVENT ptr, byval c as integer, byval f as string = "mididump.txt")
  dim i as integer, fh as integer, w as string

  debug "dumping external buffer (c = " & c & ")"

  dim D as UInteger, S as UByte, A as UByte, B as UByte, Fl as UByte

  fh = freefile
  open f for output as #fh

  if c <= 0 then exit sub

  DO until i >= c
    w = ""
    D = m->dwDeltaTime
    B = (m->dwEvent AND &HFF0000) SHR 16
    A = (m->dwEvent AND &HFF00) SHR 8
    S = (m->dwEvent AND &HFF)
    Fl = (m->dwEvent AND &HFF000000) SHR 24
    w = "D = " & D & ", " 
    w = w & "S = " & hex(S)
    select case (S SHR 4) and &HF
      case &H8
        w = w & " (Note on)"
      case &H9
        w = w & " (Note off)"
      case &HA
        w = w & " (Pressure)"
      case &HB
        w = w & " (Controller)"
      case &HC
        w = w & " (Program change)"
      case &HD
        w = w & " (Channel pressure)"
      case &HE
        w = w & " (Pitch bend)"
      case &HF
        if S = &HFF then w = w & " (Sysex)" else w = w & " (Meta)"
      case else
        w = w & " (Unknown!?)"
    end select
    w = w & ", "
    w = w & "A = " & hex(A) & ", "
    w = w & "B = " & hex(B) & ", "
    w = w & "Fl = " & Fl


    print #fh, w
    m += 1
    i+=1
  LOOP

  close #fh
end sub

#define MAKEWINNOTE(t,s,a,b) Type((t), 0, (((b) AND 255) SHL 16) OR (((a) AND 255) SHL 8) OR ((s) AND 255))
#define MIDINOP(t) Type((t), 0, MEVT_NOP SHL 24)

dim shared bufferlen as integer = 120

Sub PrepareNextBeat(byval unused as integer)
  dim as double prof1, prof2
  if music_song = 0 then exit sub
  prof1 = timer

  dim tmp as MIDIHDR ptr, tmp2 as MIDIEVENT ptr, tmp3 as integer, newflag as integer = 0
  dim erro as MMRESULT

  'debug "prepare next beat"
  /'
    basically, this sub does the following:

    1. unprepare the current header
    2. swap the current and bufferered pointers
    3. play the current header
    4. null and refill the buffer
    5. prepare the buffer header
  '/

  '1. unprepare the current header

  if current_head = 0 then  'if it's the first beat, there won't be a current_head.
    'debug "first beat, allocating buffers"
    current_head = CAllocate(len(MIDIHDR))
    buffer_head = CAllocate(len(MIDIHDR)) 'if the current one doesn't exist, neither will this
    newflag = -1 'we need to get data out now!
  end if
  
  if current_head->lpData <> 0 then
    erro = midiOutUnprepareHeader(device, current_head, len(MIDIHDR))

    if erro then
      'debug "midiOutUnprepareHeader error"
      'if erro = MIDIERR_STILLPLAYING then debug "The buffer to be unprepared is still playing?!"
      'if erro = MMSYSERR_INVALHANDLE then debug "The device hasn't been opened...?"
      'if erro = MMSYSERR_INVALPARAM then debug "Doesn't like the buffer."
    end if
  end if
  
  '2. swap buffers
  tmp = current_head
  current_head = buffer_head
  buffer_head = tmp

  tmp2 = current_beat
  current_beat = buffer_beat
  buffer_beat = tmp2
  
  tmp3 = current_size
  current_size = buffer_size
  buffer_size = tmp3


  if newflag = 0 then 'ifn the buffers are empty, we need to defer playing them.

    '3. play the current buffer
    erro = midiStreamOut(device, current_head, len(MIDIHDR))

    if erro then
      'debug "midiStreamOut error"
      select case erro
      case MMSYSERR_NOMEM
        'debug "The buffer can't be locked?!"
      case MIDIERR_STILLPLAYING
        'debug "The buffer to be played is still playing?!"
      case MIDIERR_UNPREPARED
        'debug "The buffer hasn't been prepared?!"
      case MMSYSERR_INVALHANDLE
        'debug "The device hasn't been opened...?"
      case MMSYSERR_INVALPARAM
        'debug "Doesn't like the buffer."
      case else
        'debug "mysterious other error!"
      end select
    end if
  else
    'debug "deferring buffer 'til later"
  end if


  '4. empty and refill the buffer

  dim ticks as integer = bufferlen + skip_ticks

  '4.1. deallocate buffer
  if buffer_beat <> 0 then Deallocate buffer_beat: buffer_beat = 0: buffer_size = 0

  dim orig as MIDI_EVENT ptr, cnt as integer

  '4.2. Sanity checking
  if song_ptr = NULL then 'we ran out of music.
    'debug "end of song"
    if looppoint <> NULL then
      'debug "looping to point"
      song_ptr = looppoint
      song_ptr = song_ptr->next 'loop point, skip it
    else
      song_ptr = music_song
    end if
  end if

bufferanyway:
    if song_ptr = NULL then exit sub 'sanity
    'debug str(skip_ticks)
    if ticks < song_ptr->time then 'no notes this tick.
      'debug "no events this beat (" & song_ptr->time & ")"
      skip_ticks += bufferlen 'skip this many ticks next beat.

    else
      '4.2. traverse tree, to figure out how many events we need
orig = song_ptr
      do
        ticks -= song_ptr->time
        cnt += 1
        song_ptr = song_ptr->next
      loop while ticks > 0 AND song_ptr <> NULL

      'debug "found " & cnt & " events this beat"
      song_ptr = orig

      if cnt > 0 then
        buffer_beat = CAllocate(len(MIDIEVENT) * cnt)
        buffer_size = cnt

        dim i as integer
        for i = 0 to cnt - 1
          with *song_ptr
            select case as const .status
            case &HB0 to &HBF
              IF .data(0) = &H6F then
                'loop point!
                looppoint = song_ptr
                buffer_beat[i] = MIDINOP(.time - skip_ticks) 'do nothing this beat
              Else
                buffer_beat[i] = MAKEWINNOTE(.time - skip_ticks,.status, .data(0), .data(1))
              End if
            case &HF0 'sysex, pass a nop
              buffer_beat[i] = MIDINOP(.time - skip_ticks) 'do nothing this beat
            case &HFF 'meta
              if .data(0) = &H51 then
                dim tempo as integer = .extradata[0] SHL 16 + .extradata[1] SHL 8 + .extradata[2]
                buffer_beat[i] = Type(.time - skip_ticks, 0, (MEVT_TEMPO SHL 24) OR tempo)
              end if
            case &H80 to &H8F, &H90 to &H9f
                buffer_beat[i] = MAKEWINNOTE(.time - skip_ticks,.status, .data(0), .data(1) * music_vol / 16)
            case &HC0 to &HCF
                'debug "program change: " & .status & " - " & .data(0)
                buffer_beat[i] = MAKEWINNOTE(.time - skip_ticks, .status, .data(0), 0)
            case else
              if .data(0) <> -1 AND .data(1) <> -1 THEN
                  buffer_beat[i] = MAKEWINNOTE(.time - skip_ticks,.status, .data(0), .data(1))
                elseif .data(0) <> -1 AND .data(1) = -1 THEN
                  buffer_beat[i] = MAKEWINNOTE(.time - skip_ticks,.status, .data(0), 0)
                elseif .data(0) = -1 THEN
                  buffer_beat[i] = MAKEWINNOTE(.time - skip_ticks,.status, 0, 0)
                end if
            end select
          end with
          song_ptr = song_ptr->next
          skip_ticks = 0
        next
        'that should be all the buffering
        'these are for 'debugging
        'debug "dumping 'debug log"
        'dumpMidi_event orig, cnt, "midimike.txt"
        'dumpMidi_event buffer_beat, cnt, "midiwin.txt"

        with *buffer_head
          .dwBufferLength = len(MIDIEVENT) * cnt
          .dwBytesRecorded = .dwBufferLength
          .dwFlags = 0
          .lpData = cptr(LPSTR, buffer_beat)
        end with
      Else
        ':\ um, this should've been caught above.
        'debug "sanity check failure!"
        buffer_beat = CAllocate(len(Midievent)) 'just one event, in case windows tries to read it anyway
        buffer_size = 1
        with *buffer_head
          .dwBufferLength = len(MIDIEVENT)
          .dwBytesRecorded = .dwBufferLength
          .dwFlags = 0
          .lpData = cptr(LPSTR, buffer_beat)
        end with

        buffer_beat[0] = MIDINOP(bufferlen) 'do nothing this beat
      end if
  end if

  '5. Prepare buffer
  'debug "preparing buffer"
  erro = midiOutPrepareHeader(device, buffer_head, len(MIDIHDR))

  if erro then
    'debug "midiOutPrepareHeader error"
    'if erro = MMSYSERR_NOMEM then debug "The buffer can't be locked?!"
    'if erro = MMSYSERR_INVALHANDLE then debug "The device hasn't been opened...?"
    'if erro = MMSYSERR_INVALPARAM then debug "Doesn't like the buffer."
  end if

  'debug "end of prepare next buffer"
  if newflag then
    'debug "recurse! (current_head = " & current_head & ", buffer_head = " & buffer_head & ")"
    PrepareNextBeat 0 'new song, no front buffer yet
  end if

'   if DebugWnd then
'     'debug "telling the debug window to update itself"
'     'SendMessage(DebugWnd, WM_PAINT, 0, 0)
'     RedrawWindow(DebugWnd, 0, 0, RDW_INVALIDATE OR RDW_ERASE)
'   end if
  
  buffer_thread = NULL
  
  prof2 = timer

  'debug str(prof2 - prof1)
End Sub

Sub StreamCallback(Byval handle as HMIDIOUT, byval umsg as Uinteger, byval dwInstance as UInteger, byval dwParam1 as UInteger, byval dwParam2 as UInteger)
  'must make this quick
  select case as const umsg
  case MOM_DONE
    'debug "callback"
    'the midi is done playing the buffer
    if buffer_thread = NULL and music_playing <> 0 then buffer_thread = ThreadCreate(@PrepareNextBeat)
  case MOM_OPEN
    'debug "callback - opening!"
  case MOM_CLOSE
    'debug "callback - closing!"
  end select
End Sub

Sub ResetInternals
		music_playing = 0
    dim erro as HRESULT
    if device then erro = midiStreamStop(device)
    
'     if erro then
'       debug "midiStreamStop error"
'       if erro = MMSYSERR_INVALHANDLE then debug "The device hasn't been opened...?"
'     end if
    
    if buffer_thread then threadwait buffer_thread
		if current_head then erro = midiOutUnprepareHeader(device, current_head, len(MIDIHDR))
'     if erro then
'       debug "midiOutUnprepareHeader error"
'       if erro = MIDIERR_STILLPLAYING then debug "The buffer is still playing"
'       if erro = MMSYSERR_INVALHANDLE then debug "The device hasn't been opened...?"
'       if erro = MMSYSERR_INVALPARAM then debug "Doesn't like the buffer."
'     end if
    
    if buffer_head then erro = midiOutUnprepareHeader(device, buffer_head, len(MIDIHDR))
'     if erro then
'       debug "midiOutUnprepareHeader error"
'       if erro = MIDIERR_STILLPLAYING then debug "The buffer is still playing"
'       if erro = MMSYSERR_INVALHANDLE then debug "The device hasn't been opened...?"
'       if erro = MMSYSERR_INVALPARAM then debug "Doesn't like the buffer."
'     end if
    
    deallocate current_head: current_head = 0
    deallocate current_beat: current_beat = 0
    deallocate buffer_head : buffer_head = 0
    deallocate buffer_beat : buffer_beat = 0

		FreeMidiEventList(music_song)
		music_song = 0
		song_ptr = 0
		music_song = 0
		music_paused = 0
end sub

function streamPosition as integer
  if device then
    dim erro as MMRESULT
    dim t as MMTIME
    
    t.wType = TIME_TICKS
    
    erro = midiStreamPosition(device, @t, len(t))
    
    return t.ticks
  end if
end function


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

		if fade_thread then
			threadwait fade_thread
		end if

		if music_song <> 0 then
		  FreeMidiEventList(music_song)
		  music_song = 0
		  song_ptr = 0
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

sub music_play(songname as string, fmt as integer)
  dim erro as MMRESULT
	if music_on then
		songname = rtrim$(songname)	'lose any added nulls
    dim ext as string = lcase(justextension(songname))
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
			midname = tmpdir$ & trimpath$(songname) & "-" & lcase(hex(flen)) & ".bmd"
			'check if already converted
			if isfile(midname) = 0 then
				bam2mid(songname, midname,0)
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
		  ResetInternals
		end if
'    debug "sound_song = " & sound_song
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
  		song_ptr = music_song
  		'addJumpToEnd music_song

  		music_paused = 0
  		music_playing = 1
  		'playback_thread = threadcreate(@PlayBackThread,0)


  		'we need to set the time base for the midi
  		dim timebase as MIDIPROPTIMEDIV
  		timebase.cbStruct = len(timebase)
  		timebase.dwTimeDiv = division

  		bufferlen = division
  		'debug str(division)

  		erro = midiStreamProperty(device, cptr(LPBYTE, @timebase), MIDIPROP_SET OR MIDIPROP_TIMEDIV)
      if erro then
        'debug "midiStreamProperty error"
        'if erro = MMSYSERR_INVALHANDLE then debug "The device hasn't been opened...?"
        'if erro = MMSYSERR_INVALPARAM then debug "Doesn't like the property."
      end if

      erro = midiStreamRestart(device)
      if erro then
        debug "midiStreamRestart error"
        if erro = MMSYSERR_INVALHANDLE then debug "The device hasn't been opened...?"
      end if


  		PrepareNextBeat 0
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
				midiStreamPause(device) 'rumours have it that this closes the device. true? not true?
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
			midiStreamRestart(device)
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

sub music_setvolume(vol as integer)
	music_vol = vol
	if music_on then
		'if music_song > 0 then setvolmidi vol
		dim v as integer
		'vol = vol AND &HF
		'v = vol + vol SHL 4
		'v += v SHR 8
		'v += v SHR 16
		'lazese for &hvvvvvvvv, which is equal volume on both left and right
    'midiOutSetVolume(device, v)
	end if
end sub

function music_getvolume() as integer
	'music_getvolume = getvolmidi
	dim v as integer
  'midiOutGetVolume(device, @v)

  'v = (v shr 12) AND &HF

  'music_vol = v
  return music_vol
end function

sub music_fade(targetvol as integer)
''Unlike the original version, this will pause everything else while it
''fades, so make sure it doesn't take too long

'lies, now it fades with a thread
	dim I as integer

	if fade_thread then
		threadwait fade_thread
	end if
	fade_thread = threadcreate(@fade_daemon, cast(intptr, targetvol))
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


#include "audwrap/audwrap.bi"
Declare Function SoundSlot(byval num as integer) as integer

TYPE SoundEffect
  used as integer 'sentinel, not 0 = this entry contains valid data
  effectID as integer 'OHR sound effect
  soundID as integer 'Audiere number
  paused as integer
END TYPE

dim Shared SoundPool as SoundEffect ptr, SoundPoolSize as integer




dim shared sound_inited as integer 'must be non-zero for anything but _init to work

sub sound_init
  sound_inited += 1
  'debug "sound init = " & sound_inited

  if sound_inited <> 1 then exit sub

  if AudInit() then
    exit sub ':(
  end if

  'music_init 'for safety (don't worry, they won't recurse (much))

  'at this point, we're basically done

end sub

sub sound_close
  dim i as integer = 0
  sound_inited -= 1
  'debug "sound close = " & sound_inited

  'trying to free something that's already freed... bad!
  if sound_inited <> 0 then exit sub
'  debug "sound_close"

  for i = 0 to SoundPoolSize - 1
    UnloadSound(i)
  next

  AudClose()

  'music_close
end sub


sub sound_play(byval num as integer, byval l as integer,  byval s as integer = 0)
  'debug ">>sound_play(" & num & ", " & l & ")"
  dim slot as integer

  if not s then slot = SoundSlot(num) else slot = num
  if slot = -1 then
    'debug "sound not loaded, loading."
    slot = LoadSound(num)
    if slot = -1 then debug "failed": exit sub
  end if
  'slot -= 1
  'debug "slot " & slot
  with SoundPool[slot]
  'debug str(AudIsPlaying(.soundID))
    if AudIsPlaying(.soundID) <> 0 and .paused = 0 then
      'debug "<<already playing"
      exit sub
    end if

    .paused = 0
    AudSetRepeat(.soundID, l)
    AudPlay(.soundID)
  end with
'  debug "<<done"
end sub

sub sound_pause(byval num as integer,  byval s as integer = 0)
  'debug ">>sound_pause(" + trim(str(slot)) + ")"
  dim slot as integer
  if not s then slot = SoundSlot(num) else slot = num
  if slot = -1 then exit sub

  with SoundPool[slot]
    if sound_playing(num) = 0 OR .paused then
      exit sub
    end if

    .paused = -1
    AudPause(.soundID)
  end with
  'debug "<<sound_pause"
end sub

sub sound_stop(byval num as integer,  byval s as integer = 0)
  'debug ">>sound_stop(" + trim(str(slot)) + ")"
  dim slot as integer
  if not s then slot = SoundSlot(num) else slot = num
  if slot = -1 then exit sub

  with SoundPool[slot]

    AudStop(.soundID)

    .paused = 0
  end with
  'debug "<<sound_stop"
end sub

sub sound_free(byval num as integer)

  UnloadSound num
end sub

function sound_playing(byval num as integer,  byval s as integer = 0) as integer
  dim stat as integer
  dim slot as integer
  if not s then slot = SoundSlot(num) else slot = num
  if slot = -1 then return 0

  if AudIsPlaying(SoundPool[slot].soundID) then return 1 ELSE return 0

end function



'-------------------------------------------------------------------------------



'Returns the slot+1 in the sound pool which corresponds to the given sound effect
' if the sound is not loaded, returns -1

Function SoundSlot(byval num as integer) as integer
  dim i as integer
  for i = 0 to SoundPoolSize - 1
    with SoundPool[i]
      'debug "i = " & i & ", used = " & .used & ", effID = " & .effectID & ", sndID = " & .soundID & ", AudIsValid = " & AudIsValidSound(.soundID)
      if .used AND (.effectID = num OR num = -1) AND AudIsValidSound(.soundID) then return i
    end with
  next

  return -1
End Function

'Loads an OHR sound (num) into a slot. Returns the slot number, or -1 if an error
' ocurrs
Function LoadSound overload(byval num as integer) as integer
  ' 0 - sanity checks

  'eh... we don't really need to throw an error if the sound is already loaded...
  dim ret as integer
  ret = SoundSlot(num)
  if ret >= 0 then return ret

  return LoadSound(soundfile(num), num)
End Function

Function LoadSound(byval f as string,  byval num as integer = -1) as integer
  dim ret as integer
  'the steps
  ' 1. allocate space in the sound pool
  ' 2. load the sound

  ' 1. make room

  dim i as integer

  'iterate through the pool
  for i = 0 to SoundPoolSize - 1
    if SoundPool[i].used <> -1 then exit for 'if we found a free spot, coo'
  next

  'otherwise, i will be left =ing SoundPoolSize
  if i = SoundPoolSize then
    'Grow the sound pool
    dim newpool as SoundEffect ptr
    newpool = Reallocate(SoundPool, len(SoundEffect) * (SoundPoolSize * 1.1 + 3))

    if newpool = NULL then return -1 'crap

    SoundPool = newpool 'vw00t
    SoundPoolSize = SoundPoolSize * 1.1 + 3
  end if

  'ok, now i points at a valid slot. goody.

  ' 2. load the sound

  'TODO: abstract file name to a function or something
  'loadWaveFileToBuffer soundfile(num), @derbuffer

  dim fe as string = lcase(right(f,3))
  dim s as integer
'  debug f & ": " & fe
  if fe = "mp3" or fe = "ogg" then 'intended for streaming
    s = AudLoadSound(f, true)
  else
    s = AudLoadSound(f, false)
  end if

'  debug "slot is " & s

  if s = -1 then return -1 'crap

  'if we got this far, yes!
  with SoundPool[i]
    .used = -1
    .soundID = s
    .effectID = num
  end with

  return i


End Function

'Unloads a sound loaded in a slot. TAKES A SLOT, NOT AN SFX NUMBER!
Sub UnloadSound(byval slot as integer)
  if not SoundPool[slot].used then exit sub
  with SoundPool[slot]
    if AudIsValidSound(.soundID) then AudUnloadSound(.soundID)
    .used = 0
    .paused = 0
    .soundID = 0
    .effectID = 0
  end with
End Sub
