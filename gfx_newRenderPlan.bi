'gfx_newRenderPlan.bi
'7/3/11
'presents the structures, software implementation, and function pointers of the new render plan

#IFNDEF GFX_NEWRENDERPLAN_BI
#DEFINE GFX_NEWRENDERPLAN_BI

#INCLUDE "udts.bi"

'Surfaces

Enum SurfaceFormat
	SF_8bit = 0
	SF_32bit = 1
End Enum
	
Enum SurfaceUsage
	SU_Source = 0
	SU_RenderTarget = 1
End Enum

Type Surface
	handle as any ptr
	width as int32
	height as int32
	format as SurfaceFormat
	usage as SurfaceUsage
	Union
		pRawData as any ptr
		pColorData as uint32 ptr
		pPaletteData as ubyte ptr
	End Union
End Type

Type SurfaceRect
	left as int32
	top as int32
	right as int32
	bottom as int32
End Type

'palettes

Type BackendPalette
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
	extern gfx_surfaceDestroy as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceUpdate as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceGetData as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceFill as function ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceStretch as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_surfaceCopy as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	extern gfx_paletteCreate as function ( byval ppPaletteOut as BackendPalette ptr ptr) as integer
	extern gfx_paletteDestroy as function ( byval pPaletteIn as BackendPalette ptr ) as integer
	extern gfx_paletteUpdate as function ( byval pPaletteIn as BackendPalette ptr ) as integer

	extern gfx_renderQuadColor as function ( byval pQuad as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderQuadTexture as function ( byval pQuad as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderQuadTextureColor as function ( byval pQuad as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	extern gfx_renderTriangleColor as function ( byval pTriangle as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderTriangleTexture as function ( byval pTriangle as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderTriangleTextureColor as function ( byval pTriangle as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	extern gfx_present as function ( byval pSurfaceIn as Surface ptr, byval pPalette as BackendPalette ptr ) as integer

	declare function gfx_surfaceCreate_SW ( byval width as integer, byval height as integer, byval format as SurfaceFormat, byval usage as SurfaceUsage, byval ppSurfaceOut as Surface ptr ptr ) as integer
	declare function gfx_surfaceDestroy_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceUpdate_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceGetData_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceFill_SW ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceStretch_SW ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_surfaceCopy_SW ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	declare function gfx_paletteCreate_SW ( byval ppPaletteOut as BackendPalette ptr ptr ) as integer
	declare function gfx_paletteDestroy_SW ( byval pPaletteIn as BackendPalette ptr ) as integer
	declare function gfx_paletteUpdate_SW ( byval pPaletteIn as BackendPalette ptr ) as integer

	declare function gfx_renderQuadColor_SW ( byval pQuad as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderQuadTexture_SW ( byval pQuad as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderQuadTextureColor_SW ( byval pQuad as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	declare function gfx_renderTriangleColor_SW ( byval pTriangle as VertexPC ptr, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderTriangleTexture_SW ( byval pTriangle as VertexPT ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderTriangleTextureColor_SW ( byval pTriangle as VertexPTC ptr, byval pTexture as Surface ptr, byval pPalette as BackendPalette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	declare function gfx_present_SW ( byval pSurfaceIn as Surface ptr, byval pPalette as BackendPalette ptr ) as integer

end extern

#ENDIF