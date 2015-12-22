#include "config.bi"
#include "crt.bi"
#IFDEF __UNIX__
'Open Sound System
#include "soundcard.bi"

'These headers are both totally nonfunctional, so use manual declarations
'#include "crt/linux/fcntl.bi"
'#include "crt/io.bi"

#IFNDEF O_WRONLY
#define O_WRONLY 01
#ENDIF
declare function _close cdecl alias "close" (byval as integer) as integer
declare function _open cdecl alias "open" (byval as zstring ptr, byval as integer) as integer
declare function _write cdecl alias "write" (byval as integer, byval as any ptr, byval as uinteger) as integer

#ELSE
#include "windows.bi"
#include "win/mmsystem.bi"
#undef MIDIEVENT
#undef createevent
#ENDIF

#IFDEF __UNIX__
dim shared midi_handle as integer
#ELSE
dim shared midi_handle as HMIDIOUT
#ENDIF
function openMidi() as integer
    #IFDEF __UNIX__
    midi_handle = _open("/dev/sequencer",O_WRONLY)
    return midi_handle = 0
    #ELSE
    'dim moc as MIDIOUTCAPS
    'midiOutGetDevCaps MIDI_MAPPER, @moc, len(MIDIOUTCAPS)
    'debug "Midi port supports Volume changes:" + str$(moc.dwSupport AND MIDICAPS_VOLUME)

    return midiOutOpen (@midi_handle,MIDI_MAPPER,0,0,0)

    #ENDIF
end function

function closeMidi() as integer
    #IFDEF __UNIX__
    return _close(midi_handle)
    #ELSE
    return midiOutClose (midi_handle)
    #ENDIF
end function

function shortMidi(event as UByte, a as UByte, b as UByte) as integer
    #IFDEF __UNIX__
    DIM packet(3) as UByte
    packet(0) = SEQ_MIDIPUTC
    packet(1) = event
    _write(midi_handle,@packet(0),4)
    packet(1) = a
    _write(midi_handle,@packet(0),4)
    packet(1) = b
    _write(midi_handle,@packet(0),4)
    return 0
    #ELSE
    return midiOutShortMSG(midi_handle,event SHL 0 + a SHL 8 + b SHL 16)
    #ENDIF
end function

sub waitforkey
    sleep
    'Clear keypress
    dim dummy as string = inkey
end sub

print "(after each step, press a key)"

print "Open midi"
print openMidi
waitforkey

print "Note on"
print shortMidi(&H90,&H40,100)
waitforkey

print "Note off"
print shortMidi(&H80,&H40,0)
waitforkey

print "Close midi"
print closeMidi
waitforkey
