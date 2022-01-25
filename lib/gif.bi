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

type GifKDTree as GifKDTreeFwd

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

declare function GifBegin(byval writer as GifWriter ptr, byval file as FILE ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval transparent as boolean = false, byval globalPal as const GifPalette ptr = NULL) as boolean
declare function GifWriteFrame(byval writer as GifWriter ptr, byval image as const GifRGBA ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval bitDepth as long = 8, byval dither as boolean = false) as boolean
declare function GifWriteFrame8(byval writer as GifWriter ptr, byval image as const ubyte ptr, byval width as ulong, byval height as ulong, byval delay as ulong, byval pal as const GifPalette ptr = NULL) as boolean
declare sub GifOverwriteLastDelay(byval writer as GifWriter ptr, byval delay as ulong)
declare function GifEnd(byval writer as GifWriter ptr) as boolean

declare sub GifGetClosestPaletteColor(byval tree as GifKDTree ptr, byval color as GifRGBA, byval bestInd as long ptr, byval bestDiff as long ptr, byval nodeIndex as long = 0)


' The following are in gif.cpp, not part of gif.h itself

declare sub dither_image(byval image as const GifRGBA ptr, byval width as ulong, byval height as ulong, byval result as ubyte ptr, byval computePalette as boolean, byval palette as const GifRGBA ptr, byval bitDepth as long, byval firstIndex as long, byval maxError as long)

declare function make_KDTree_for_palette(byval palette as const RGBcolor ptr, byval bitDepth as long, byval firstindex as long) as GifKDTree ptr
declare sub delete_KDTree(byval tree as GifKDTree ptr)
declare function query_KDTree(byval tree as GifKDTree ptr, byval color as RGBcolor) as long

extern kGifMaxAccumError as integer

end extern
