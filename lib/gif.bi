#pragma once

#include once "crt/stdio.bi"
#include once "surface.bi"

extern "C"

' type GifRGBA
' 	as ubyte b, g, r, a
' end type
type GifRGBA as RGBcolor

type GifPalette
	bitDepth as long
	colors(0 to 255) as GifRGBA
end type

type GifWriter
	f as FILE ptr
	oldImage as ubyte ptr
	firstFrame as boolean
	deltaCoded as boolean
	globalPal as GifPalette ptr
        maxWidth as integer
        maxHeight as integer
        currentWidth as integer
        currentHeight as integer
        sizeChanged as boolean
        lastFramePos as clong
end type

declare function GifBegin(byval writer as GifWriter ptr, byval file as FILE ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval transparent as boolean = false, byval globalPal as const GifPalette ptr = NULL) as bool
declare function GifWriteFrame(byval writer as GifWriter ptr, byval image as const GifRGBA ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval bitDepth as long = 8, byval dither as boolean = false) as bool
declare function GifWriteFrame8(byval writer as GifWriter ptr, byval image as const ubyte ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval pal as const GifPalette ptr = NULL) as bool
declare sub GifOverwriteLastDelay(byval writer as GifWriter ptr, byval delay as ulong)
declare function GifEnd(byval writer as GifWriter ptr) as bool

' In gif.cpp, not part of gif.h itself
declare sub dither_image(byval image as const GifRGBA ptr, byval width as ulong, byval height as ulong, byval result as ubyte ptr, byval palette as const GifRGBA ptr, byval bitDepth as long, byval firstindex as long)

end extern
