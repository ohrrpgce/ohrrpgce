'OHRRPGCE - music_native_subs.bi
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF MUSIC_NATIVE_SUBS_BI
#DEFINE MUSIC_NATIVE_SUBS_BI

declare function getvlq(byval track as miditrack ptr,byref p as integer) as integer
declare function createevent(t as uinteger, e as ubyte, a as ubyte, b as ubyte) as midi_event ptr
declare function readbyte(d as ubyte ptr, p as integer) as ubyte
declare function miditracktostream(track as miditrack ptr) as midi_event ptr
declare function miditostream(mididata as midifile ptr) as midi_event ptr
declare function readmidifile(mididata as midifile ptr, fp as file ptr) as integer
declare function createmidieventlist(midifile as string, division as short ptr) as midi_event ptr
declare sub converttorelative(head as midi_event ptr)
declare sub freemidieventlist(head as midi_event ptr)

#ENDIF
