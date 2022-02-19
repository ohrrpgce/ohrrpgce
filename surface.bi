'OHRRPGCE - Surfaces, part of the graphics API
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

'Surface struct, and the software implementation and function pointers of the new render plan
'Note: this header corresponds to surface.h plus gfxRender.h

#IFNDEF SURFACE_BI
#DEFINE SURFACE_BI

#include "util.bi"

Union RGBcolor
	as uint32 col
	type
		' Opaque is a=255. Not pre-multiplied. However, only the rasterizer
		' and BMP and PNG import uses the alpha channel, all other code ignores it.
		as ubyte b, g, r, a
	end type
	DECLARE OPERATOR CAST () as string
End Union

'Masks for components of RGBcolor
const RGB_Rmask = &hFF0000
const RGB_Gmask = &hFF00
const RGB_Bmask = &hFF
const RGB_Amask = &hFF000000

Enum SurfaceFormat
	SF_8bit = 0
	SF_32bit = 1
End Enum

Enum SurfaceUsage
	SU_Source = 0       ' Surfaces that can be drawn to render targets
	SU_RenderTarget = 1
	SU_Staging = 2      ' Surfaces that don't get sent to GPU
End Enum

Type Palette16Fwd as Palette16
Type FrameFwd as Frame
Type SurfaceFwd as Surface

Type Surface
	Union
		Type
			width as int32
			height as int32
		End Type
		size as XYPair
	End Union
	pitch as int32   'Measured in pixels, not bytes
	refcount as int32
	isview as int32  'Is a view onto a Frame or another Surface (see below)
			 'FB enums are 64 bit on a 64 bit machine, unlike C/C++ which uses 'int'
	format as int32  ' SurfaceFormat
	usage as int32   ' SurfaceUsage
	' The following are only used if isview is true; at most one of them is non-NULL
	base_frame as FrameFwd ptr  'If not NULL, is a view of a whole Frame
	base_surf as SurfaceFwd ptr 'If not NULL, is a view of part of a Surface

	handle as any ptr
	Union
		pRawData as any ptr
		pColorData as RGBcolor ptr  'uint32s
		pPaletteData as ubyte ptr
	End Union
	pMaskData as ubyte ptr  'Optional, nonzero for opaque pixels. May only be present on 8-bit surfaces.
End Type

Type SurfaceRect
	left as int32
	top as int32
	right as int32   'right and bottom are INCLUSIVE
	bottom as int32
End Type

'palettes

Type RGBPalette
	col(255) as RGBcolor
	from_backend as boolean 'True if allocated from gfx_palette* API, false from masterpal_to_gfxpal
	'handle as any ptr      'Not used yet
End Type

Enum 'BlendMode
	blendModeNormal = 0
	blendModeAdd = 1
	blendModeMultiply = 2
	blendModeLAST = 2
End Enum
Type BlendMode as integer

Enum 'BlendAlgo
	blendAlgoDither = 0
	blendAlgoLessDither = 1
	blendAlgoNoDither = 2
	blendAlgoLAST = 2
End Enum
Type BlendAlgo as integer

'frame_draw additional draw options
Type DrawOptions
	' Size multiplier.
	' (Not implemented for Surfaces or render API and never will be)
	scale as integer = 1

	' gfx_render* and gfx_surfaceCopy API ONLY, all other functions take
	' separate 'trans' arguments Whether colour 0 (or mask 0, in Surfaces
	' with masks) of 8-bit source textures is transparent.
	color_key0 as boolean

	' If the destination has a mask, sets the mask for the destination rectangle
	' equal to the mask (or color-key) for the source rectangle. Does not OR them.
	' (TODO: not implemented for 32-bit draws (gfx_surfaceCopy))
	write_mask as boolean

	' If false, all blending/modulation options are ignored. Used as an early-out
	with_blending as boolean

	' gfx_render* API only (TODO: implement for gfx_surfaceCopy): whether to use
	' the alpha channel of 32-bit source textures
	alpha_channel as boolean

	blend_mode as BlendMode

	' gfx_render* API only: modulates the vertex colors (including alpha); hence
	' not supported by drawQuadTexture/drawTriangleTexture
	argbModifier as RGBcolor = (&hffffffff)

	' Redundant to argbModifier.a, but is supported by frame_draw. (TODO: should
	' probably remove)
	opacity as single = 1.

	declare constructor (scale as integer = 1)
End Type

extern "C"

'In blit.c
extern blend_algo as BlendAlgo
extern nearcolor_cache(65535) as ubyte

'In allmodex.bas
extern def_drawoptions as DrawOptions

end extern

'Vertices

' Type Position
' 	as single x, y
' End Type

' Type TexCoord
' 	as single u, v
' End Type

Type Position as Float2
Type TexCoord as Float2

Type VertexPC
	pos as Position
	col as RGBcolor
End Type

Type VertexPT
	pos as Position
	tex as TexCoord
End Type

Type VertexPTC
	pos as Position
	tex as TexCoord
	col as RGBcolor
End Type


extern "C"

	extern gfx_surfaceCreate as function ( byval width as integer, byval height as integer, byval format as SurfaceFormat, byval usage as SurfaceUsage, byval ppSurfaceOut as Surface ptr ptr) as integer
	extern gfx_surfaceCreatePixelsView as function ( byval pixels as any ptr, byval width as integer, byval height as integer, byval pitch as integer, byval format as SurfaceFormat, byval ppSurfaceOut as Surface ptr ptr) as integer
	extern gfx_surfaceCreateFrameView as function ( byval pFrameIn as FrameFwd ptr, byval ppSurfaceOut as Surface ptr ptr) as integer
	extern gfx_surfaceCreateView as function ( byval pSurfaceIn as Surface ptr, byval x as integer, byval y as integer, byval width as integer, byval height as integer, byval ppSurfaceOut as Surface ptr ptr) as integer
	extern gfx_surfaceDestroy as function ( byval ppSurfaceIn as Surface ptr ptr ) as integer
	extern gfx_surfaceReference as function ( byval pSurfaceIn as Surface ptr ) as Surface ptr
	extern gfx_surfaceUpdate as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceGetData as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceFill as function ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceFillAlpha as function ( byval fillColor as RGBcolor, byval alpha as double, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceStretch as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_surfaceCopy as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as RGBcolor ptr, pPal8 as Palette16Fwd ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byref opts as DrawOptions ) as integer

	extern gfx_paletteFromRGB as function ( byval pColorsIn as RGBcolor ptr, byval ppPaletteOut as RGBPalette ptr ptr) as integer
	extern gfx_paletteDestroy as function ( byval ppPaletteIn as RGBPalette ptr ptr ) as integer
	extern gfx_paletteUpdate as function ( byval pPaletteIn as RGBPalette ptr ) as integer

	#ifdef USE_RASTERIZER

	extern gfx_renderQuadColor as sub ( byval pQuad as VertexPC ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )
	extern gfx_renderQuadTexture as sub ( byval pQuad as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )
	extern gfx_renderQuadTextureColor as sub ( byval pQuad as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )

	extern gfx_renderTriangleColor as sub ( byval pTriangle as VertexPC ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )
	extern gfx_renderTriangleTexture as sub ( byval pTriangle as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )
	extern gfx_renderTriangleTextureColor as sub ( byval pTriangle as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )

	#endif

	' Only for surface_from_pixels
	Type PixelFormat as integer
	Enum
		PIXFMT_RGB,   ' RGB triples, 3 bytes per pixel
		PIXFMT_GREY,  ' Grey level, 1 byte per pixel
	End Enum

	'' Convenience and utility functions not specific to any Surface implementation
	declare sub surface_assign (ptr_to_replace as Surface ptr ptr, new_value as Surface ptr)  'In allmodex.bas
	declare function surface_scale ( surf as Surface ptr, destWidth as integer, destHeight as integer ) as Surface ptr
	declare function surface_duplicate ( surf as Surface ptr ) as Surface ptr
	declare function surface32_from_pixels ( pixels as ubyte ptr, w as integer, h as integer, format as PixelFormat ) as Surface ptr
	declare function surface32_to_pixels( surf as Surface ptr, format as PixelFormat ) as ubyte ptr

	declare function surfaceFrameShim( pFrameIn as FrameFwd ptr, pSurfaceOut as Surface ptr ) as integer

	'' Roto-zoomer, implemented in rotozoom.c
	declare function rotozoomSurface(src as Surface ptr, angle as double, zoomx as double, zoomy as double, smooth as boolint) as Surface ptr
	declare sub rotozoomSurfaceSize(width as integer, height as integer, angle as double, zoomx as double, zoomy as double, dstwidth as integer ptr, dstheight as integer ptr)
	declare function rotateSurface90Degrees(src as Surface ptr, numClockwiseTurns as integer) as Surface ptr

	'' The following software-rasterised implementation of the above interface is in surface.cpp.
	declare function gfx_debugSurfaces_SW ( ) as integer
	declare function gfx_surfaceCreate_SW ( byval width as integer, byval height as integer, byval format as SurfaceFormat, byval usage as SurfaceUsage, byval ppSurfaceOut as Surface ptr ptr ) as integer
	declare function gfx_surfaceCreatePixelsView_SW ( byval pixels as any ptr, byval width as integer, byval height as integer, byval pitch as integer, byval format as SurfaceFormat, byval ppSurfaceOut as Surface ptr ptr) as integer
	declare function gfx_surfaceCreateFrameView_SW ( byval pFrameIn as FrameFwd ptr, byval ppSurfaceOut as Surface ptr ptr) as integer
	declare function gfx_surfaceCreateView_SW ( byval pSurfaceIn as Surface ptr, byval x as integer, byval y as integer, byval width as integer, byval height as integer, byval ppSurfaceOut as Surface ptr ptr) as integer
	declare function gfx_surfaceDestroy_SW ( byval ppSurfaceIn as Surface ptr ptr ) as integer
	declare function gfx_surfaceReference_SW ( byval pSurfaceIn as Surface ptr ) as Surface ptr
	declare function gfx_surfaceUpdate_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceGetData_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceFill_SW ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceFillAlpha_SW ( byval fillColor as RGBcolor, byval alpha as double, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceStretch_SW ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_surfaceCopy_SW ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as RGBcolor ptr, pPal8 as Palette16Fwd ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byref opts as DrawOptions ) as integer

	declare function gfx_paletteFromRGB_SW ( byval pColorsIn as RGBcolor ptr, byval ppPaletteOut as RGBPalette ptr ptr) as integer
	declare function gfx_paletteDestroy_SW ( byval ppPaletteIn as RGBPalette ptr ptr ) as integer
	declare function gfx_paletteUpdate_SW ( byval pPaletteIn as RGBPalette ptr ) as integer

	declare function unrollPalette16( byval pPal8 as Palette16Fwd ptr, byval pPalette as RGBcolor ptr ) as RGBPalette ptr

	declare sub gfx_renderQuadColor_SW ( byval pQuad as VertexPC ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )
	declare sub gfx_renderQuadTexture_SW ( byval pQuad as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )
	declare sub gfx_renderQuadTextureColor_SW ( byval pQuad as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )

	declare sub gfx_renderTriangleColor_SW ( byval pTriangle as VertexPC ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )
	declare sub gfx_renderTriangleTexture_SW ( byval pTriangle as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )
	declare sub gfx_renderTriangleTextureColor_SW ( byval pTriangle as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr, byval pOpts as DrawOptions ptr )

end extern

#ENDIF
