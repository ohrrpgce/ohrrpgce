'OHRRPGCE - SDL_RWops Lump wrapper
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

'Used by music_sdl, but could be used with other SDL libraries

#include "config.bi"
#include "SDL/SDL.bi"
#include "lumpfile.bi"
#include "lumpfilewrapper.bi"
#include "vector.bi"
#include "common_base.bi"  'debug

#define FWptr(context)  cast(FileWrapper ptr, context->hidden.unknown.data1)

function lumprwops_seek cdecl (byval context as SDL_RWops ptr, byval offset as integer, byval whence as integer) as integer
	return FileWrapper_seek(*FWptr(context), offset, whence)
end function

function lumprwops_read cdecl (byval context as SDL_RWops ptr, byval bufr as any ptr, byval size as integer, byval maxnum as integer) as integer
	return FileWrapper_read(*FWptr(context), bufr, size, maxnum)
end function

function lumprwops_close cdecl (byval context as SDL_RWops ptr) as integer
	if context then
		FileWrapper_close(*FWptr(context))
		SDL_FreeRW(context)
	end if
	return 0
end function

function SDL_RWFromLump(byval lump as Lump ptr) as SDL_RWops ptr
	dim rw as SDL_RWops ptr
	rw = SDL_AllocRW()
	if rw = NULL then return NULL
	with *rw
		.seek = @lumprwops_seek
		.read = @lumprwops_read
		.write = NULL
		.close = @lumprwops_close
		'it appears that .type is never used, and contains garbage
		.hidden.unknown.data1 = FileWrapper_open(lump)
	end with
	return rw
end function

type FnRWopsClose as function cdecl(byval as SDL_RWops ptr) as integer

'vectors of all the safe RWops that are not yet closed, and the corresponding close functions.
'They won't be closed, but no matter.
dim shared live_RWops as any ptr vector
dim shared live_RWops_closefuncs as any ptr vector
v_new live_RWops
v_new live_RWops_closefuncs

'The intent of this function is remove the RWops from live_RWops when it is closed:
'actually calling SDL_RWclose twice is definitely an error
private function safe_RW_close_wrap cdecl (byval context as SDL_RWops ptr) as integer
	dim num as integer = v_find(live_RWops, context)
	if num > -1 then
		'It is live, close it
		dim ret as integer
		ret = cast(FnRWopsClose, live_RWops_closefuncs[num])(context)
		v_delete_slice live_RWops, num, num + 1
		v_delete_slice live_RWops_closefuncs, num, num + 1
		return ret  'I don't know what this signifies
	else
		debug "caught double-close of safe_SDL_RWops: maybe failed to use safe_RW_close?"
	end if
end function

'Wrap an SDL_RWops, overriding the close function. Can be closed/deleted just once,
'can be safely checked for liveness
function safe_RWops (byval rw as SDL_RWops ptr) as SDL_RWops ptr
	v_append live_RWops, rw
	v_append live_RWops_closefuncs, cast(any ptr, rw->close)
	rw->close = @safe_RW_close_wrap
	return rw
end function

'After calling 'safe_RWops' on an SDL_RWops, can use this to safely
'close it if it has not already been
sub safe_RWops_close (byval rw as SDL_RWops ptr)
	dim num as integer = v_find(live_RWops, rw)
	if num = -1 then exit sub
	SDL_RWclose(cast(SDL_RWops ptr, live_RWops[num]))
end sub
