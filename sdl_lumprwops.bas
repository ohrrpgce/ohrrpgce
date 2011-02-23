'OHRRPGCE - SDL_RWops Lump wrapper
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

'Used by music_sdl, but could be used with other SDL libraries

#include "config.bi"
#include "SDL/SDL.bi"
#include "lumpfile.bi"
#include "lumpfilewrapper.bi"

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
