'OHHRPGCE COMMON - Lump stdio-like file wrapper
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

'This is in a separate include file because of FB bug #2776255:
'once fbc sees an extern block, you can't declare overloaded functions anymore

'----------------------------------------------------------------------
'                         Lump FileWrapper
'Here is a C stdio-style wrapper around a Lump plus file position, since
'all of FB_FILE, SDL_RWops and Audiere's File need such a wrapper

type FileWrapper
	lump as Lump ptr
	'index as LumpIndex ptr
	pos as integer
	' for FileLumps?
	'fhandle as integer
end type

extern "C"

declare function FileWrapper_open(byval lump as Lump ptr) as FileWrapper ptr
declare sub FileWrapper_close(byref this as FileWrapper)
'returns final position in lump. whence is one of SEEK_SET, SEEK_CUR, or SEEK_END from stdio.h
declare function FileWrapper_seek(byref this as FileWrapper, byval offset as integer, byval whence as integer) as integer
'returns number of records (size bytes), out of maxnum requested, successfully read
declare function FileWrapper_read(byref this as FileWrapper, byval bufr as any ptr, byval size as integer, byval maxnum as integer) as integer
'declare function FileWrapper_write(byref this as FileWrapper, byval bufr as any ptr, byval size as integer, byval maxnum as integer) as integer
'don't need tell: call seek

end extern
