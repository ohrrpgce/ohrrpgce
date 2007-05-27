'(C) Copyright 2006 Mike Caron
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs. This code (unlike the main source) is clean and
'elegant, so no appologies are necessary.

#ifndef __AUDWRAP_BI__
#define __AUDWRAP_BI__

#libpath "audwrap"
#inclib "audwrap"

'initing routines
declare function AudInit cdecl alias "AudInit" () as integer
declare sub AudClose cdecl alias "AudClose" ()

'sound management
declare function AudLoadSound cdecl alias "AudLoadSound" (byval as ZString ptr, byval as integer) as integer
declare sub AudUnloadSound cdecl alias "AudUnloadSound" (byval as integer)
declare function AudIsPlaying cdecl alias "AudIsPlaying" (byval as integer) as integer
declare sub AudPlay cdecl alias "AudPlay" (byval as integer)
declare sub AudPause cdecl alias "AudPause" (byval as integer)
declare sub AudStop cdecl alias "AudStop" (byval as integer)
declare function AudIsValidSound cdecl alias "AudIsValidSound" (byval as integer) as integer

'sound settings
declare sub AudSetVolume cdecl alias "AudSetVolume" (byval as integer, byval as single)
declare function AudGetVolume cdecl alias "AudGetVolume" (byval as integer) as single
declare sub AudSetRepeat cdecl alias "AudSetRepeat" (byval as integer, byval as integer)
declare function AudGetRepeat cdecl alias "AudGetRepeat" (byval as integer) as integer

#endif
