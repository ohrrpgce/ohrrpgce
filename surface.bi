'Surface struct, and the software implementation and function pointers of the new render plan
'Note: this header corresponds to surface.h plus gfxRender.h

#IFNDEF SURFACE_BI
#DEFINE SURFACE_BI

#include "util.bi"

Union RGBcolor
	as uint32 col
	type
		' Opaque is a=255. Not pre-multiplied. However, only the rasterizer
		' and BMP import uses the alpha channel, all other code ignores it.
		as ubyte b, g, r, a
	end type
End Union

Enum SurfaceFormat
	SF_8bit = 0
	SF_32bit = 1
End Enum
	
Enum SurfaceUsage
	SU_Source = 0       ' Surfaces that can be drawn to render targets
	SU_RenderTarget = 1
	SU_Staging = 2      ' Surfaces that don't get sent to GPU
End Enum

Type FrameFwd as Frame

Type Surface
	handle as any ptr
	refcount as int32
	Union
		Type
			width as int32
			height as int32
		End Type
		size as XYPair
	End Union
			'FB enums are 64 bit on a 64 bit machine, unlike C/C++ which uses 'int'
	format as int32 ' SurfaceFormat
	usage as int32  ' SurfaceUsage
	frame as FrameFwd ptr  ' If not NULL, is a view onto a Frame which owns the data
	Union
		pRawData as any ptr
		pColorData as RGBcolor ptr  'uint32s
		pPaletteData as ubyte ptr
	End Union
End Type

Type SurfaceRect
	left as int32
	top as int32
	right as int32   'right and bottom are INCLUSIVE
	bottom as int32
End Type

'palettes

Type RGBPalette
	handle as any ptr
	col(255) as RGBcolor
End Type

'Vertices

Type Position
	as single x, y
End Type

Type TexCoord
	as single u, v
End Type

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
	extern gfx_surfaceFromFrame as function ( byval pFrameIn as FrameFwd ptr, byval ppSurfaceOut as Surface ptr ptr) as integer
	extern gfx_surfaceDestroy as function ( byval ppSurfaceIn as Surface ptr ptr ) as integer
	extern gfx_surfaceReference as function ( byval pSurfaceIn as Surface ptr ) as Surface ptr
	extern gfx_surfaceUpdate as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceGetData as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceFill as function ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceStretch as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_surfaceCopy as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	extern gfx_paletteCreate as function ( byval ppPaletteOut as RGBPalette ptr ptr) as integer
	extern gfx_paletteFromRGB as function ( byval pColorsIn as RGBcolor ptr, byval ppPaletteOut as RGBPalette ptr ptr) as integer
	extern gfx_paletteDestroy as function ( byval ppPaletteIn as RGBPalette ptr ptr ) as integer
	extern gfx_paletteUpdate as function ( byval pPaletteIn as RGBPalette ptr ) as integer

	extern gfx_renderQuadColor as function ( byval pQuad as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderQuadTexture as function ( byval pQuad as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderQuadTextureColor as function ( byval pQuad as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	extern gfx_renderTriangleColor as function ( byval pTriangle as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderTriangleTexture as function ( byval pTriangle as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderTriangleTextureColor as function ( byval pTriangle as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	'' Convenience wrappers not specific to any Surface implementation
	declare sub surface_assign (ptr_to_replace as Surface ptr ptr, new_value as Surface ptr)  'In allmodex.bas
	declare function surface_scale ( surf as Surface ptr, destWidth as integer, destHeight as integer ) as Surface ptr
	declare function surface_duplicate ( surf as Surface ptr ) as Surface ptr

	'' The following software-rasterised implementation of the above interface is in surface.cpp.
	declare function gfx_surfaceCreate_SW ( byval width as integer, byval height as integer, byval format as SurfaceFormat, byval usage as SurfaceUsage, byval ppSurfaceOut as Surface ptr ptr ) as integer
	declare function gfx_surfaceFromFrame_SW ( byval pFrameIn as FrameFwd ptr, byval ppSurfaceOut as Surface ptr ptr) as integer
	declare function gfx_surfaceDestroy_SW ( byval ppSurfaceIn as Surface ptr ptr ) as integer
	declare function gfx_surfaceReference_SW ( byval pSurfaceIn as Surface ptr ) as Surface ptr
	declare function gfx_surfaceUpdate_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceGetData_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceFill_SW ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceStretch_SW ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_surfaceCopy_SW ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	declare function gfx_paletteCreate_SW ( byval ppPaletteOut as RGBPalette ptr ptr ) as integer
	declare function gfx_paletteFromRGB_SW ( byval pColorsIn as RGBcolor ptr, byval ppPaletteOut as RGBPalette ptr ptr) as integer
	declare function gfx_paletteDestroy_SW ( byval ppPaletteIn as RGBPalette ptr ptr ) as integer
	declare function gfx_paletteUpdate_SW ( byval pPaletteIn as RGBPalette ptr ) as integer

	declare function gfx_renderQuadColor_SW ( byval pQuad as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderQuadTexture_SW ( byval pQuad as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderQuadTextureColor_SW ( byval pQuad as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	declare function gfx_renderTriangleColor_SW ( byval pTriangle as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderTriangleTexture_SW ( byval pTriangle as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderTriangleTextureColor_SW ( byval pTriangle as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as RGBPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

end extern

#ENDIF
