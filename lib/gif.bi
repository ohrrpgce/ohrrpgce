#pragma once

#include once "crt/stdio.bi"

extern "C"

type GifPalette
	bitDepth as long
	r(0 to 255) as ubyte
	g(0 to 255) as ubyte
	b(0 to 255) as ubyte
	treeSplitElt(0 to 255) as ubyte
	treeSplit(0 to 255) as ubyte
end type

type GifWriter
	f as FILE ptr
	oldImage as ubyte ptr
	firstFrame as bool
	deltaCoded as bool
	globalPal as GifPalette ptr
end type

declare function GifBegin(byval writer as GifWriter ptr, byval file as FILE ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval transparent as boolean = false, byval globalPal as const GifPalette ptr = NULL) as bool
declare function GifWriteFrame(byval writer as GifWriter ptr, byval image as const ubyte ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval bitDepth as long = 8, byval dither as boolean = false) as bool
declare function GifWriteFrame8(byval writer as GifWriter ptr, byval image as const ubyte ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval pal as const GifPalette ptr = NULL) as bool
declare function GifEnd(byval writer as GifWriter ptr) as bool

end extern
