#pragma once

extern "C"

#define _UJPEG_H_

type _uj_result as long
enum
	UJ_OK = 0
	UJ_NO_CONTEXT = 1
	UJ_NOT_DECODED = 2
	UJ_INVALID_ARG = 3
	UJ_IO_ERROR = 4
	UJ_OUT_OF_MEM = 5
	UJ_NO_JPEG = 6
	UJ_PROGRESSIVE = 7
	UJ_UNSUPPORTED = 8
	UJ_SYNTAX_ERROR = 9
	UJ_INTERNAL_ERR = 10
	__UJ_FINISHED
	UJ_UNKNOWN_SEGM = &hFF00
end enum

type ujResult as _uj_result

type _uj_plane
	width as long
	height as long
	stride as long
	pixels as ubyte ptr
end type

type ujPlane as _uj_plane
type ujImage as any ptr
declare function ujGetError() as ujResult
declare function ujCreate() as ujImage
declare sub ujDisableDecoding(byval img as ujImage)

const UJ_CHROMA_MODE_FAST = 1
const UJ_CHROMA_MODE_ACCURATE = 0
const UJ_CHROMA_MODE_DEFAULT = 0

declare sub ujSetChromaMode(byval img as ujImage, byval mode as long)
declare function ujDecode(byval img as ujImage, byval jpeg as const any ptr, byval size as const long) as ujImage
declare function ujDecodeFile(byval img as ujImage, byval filename as const zstring ptr) as ujImage
declare function ujIsValid(byval img as ujImage) as long
declare function ujGetWidth(byval img as ujImage) as long
declare function ujGetHeight(byval img as ujImage) as long
declare function ujIsColor(byval img as ujImage) as long
declare function ujGetImageSize(byval img as ujImage) as long
declare function ujGetPlane(byval img as ujImage, byval num as long) as ujPlane ptr
declare function ujGetImage(byval img as ujImage, byval dest as ubyte ptr) as ubyte ptr
declare sub ujDestroy(byval img as ujImage)
#macro ujFree(img)
	scope
		ujDestroy(img)
		img = NULL
	end scope
#endmacro

end extern
