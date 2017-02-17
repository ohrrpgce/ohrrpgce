'(C) Copyright 2006 Mike Caron
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs. This code (unlike the main source) is clean and
'elegant, so no appologies are necessary.

#ifndef __AUDWRAP_BI__
#define __AUDWRAP_BI__

#include "../lumpfile.bi"

extern "C"

'initing routines
declare function AudInit () as integer
declare sub AudClose ()

'sound management
declare function AudLoadSound (filename as ZString ptr, streaming as bool) as integer
declare function AudLoadSoundLump (lump as Lump ptr, streaming as bool) as integer
declare sub AudUnloadSound (slot as integer)
declare function AudIsPlaying (slot as integer) as bool
declare sub AudPlay (slot as integer)
declare sub AudPause (slot as integer)
declare sub AudStop (slot as integer)
declare function AudIsValidSound (slot as integer) as bool

'sound settings
declare sub AudSetVolume (as integer, as single)
declare function AudGetVolume (slot as integer) as single
declare sub AudSetRepeat (slot as integer, repeat as bool)
declare function AudGetRepeat (slot as integer) as bool

end extern

#endif
