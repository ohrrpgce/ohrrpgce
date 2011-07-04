'gfx_newRenderPlan.bi
'7/3/11
'presents the structures, software implementation, and function pointers of the new render plan

#IFNDEF GFX_NEWRENDERPLAN_BI
#DEFINE GFX_NEWRENDERPLAN_BI

'surfaces
enum SurfaceFormat
{
	SF_8bit = 0,
	SF_32bit = 1,
};
enum SurfaceUsage
{
	SU_Source = 0,
	SU_RenderTarget = 1,
};

Type Surface
	width as integer;
	height as integer;
	SurfaceFormat format;
	SurfaceUsage usage;
	pRawData as any ptr;
	End Type

Type SurfaceRect
	left as integer
	top as integer
	right as integer
	bottom as integer
	End Type

'palettes
Type Palette
	DIM p[256] as integer
	End Type

extern "C"

	extern gfx_surfaceCreate as function ( byval width as integer, byval height as integer, byval SurfaceFormat format, byval SurfaceUsage usage, Surface** ppSurfaceOut ) as integer
	extern gfx_surfaceDestroy as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceUpdate as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceGetData as function ( byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceFill as function ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	extern gfx_surfaceStretch as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_surfaceCopy as function ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	extern gfx_paletteCreate as function ( Palette** ppPaletteOut ) as integer
	extern gfx_paletteDestroy as function ( byval pPaletteIn as Palette ptr ) as integer
	extern gfx_paletteUpdate as function ( byval pPaletteIn as Palette ptr ) as integer

	extern gfx_renderQuadColor as function ( byval VertexPC* pQuad, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderQuadTexture as function ( byval VertexPT* pQuad, byval pTexture as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderQuadTextureColor as function ( byval VertexPTC* pQuad, byval pTexture as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	extern gfx_renderTriangleColor as function ( byval VertexPC* pTriangle, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderTriangleTexture as function ( byval VertexPT* pTriangle, byval pTexture as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	extern gfx_renderTriangleTextureColor as function ( byval VertexPTC* pTriangle, byval pTexture as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	extern gfx_present as function ( byval pSurfaceIn as Surface ptr, byval pPalette as Palette ptr ) as integer

	declare function gfx_surfaceCreate_SW ( byval width as integer, byval height as integer, byval SurfaceFormat format, byval SurfaceUsage usage, Surface** ppSurfaceOut ) as integer
	declare function gfx_surfaceDestroy_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceUpdate_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceGetData_SW ( byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceFill_SW ( byval fillColor as integer, byval pRect as SurfaceRect ptr, byval pSurfaceIn as Surface ptr ) as integer
	declare function gfx_surfaceStretch_SW ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_surfaceCopy_SW ( byval pRectSrc as SurfaceRect ptr, byval pSurfaceSrc as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	declare function gfx_paletteCreate_SW ( Palette** ppPaletteOut ) as integer
	declare function gfx_paletteDestroy_SW ( byval pPaletteIn as Palette ptr ) as integer
	declare function gfx_paletteUpdate_SW ( byval pPaletteIn as Palette ptr ) as integer

	declare function gfx_renderQuadColor_SW ( byval VertexPC* pQuad, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderQuadTexture_SW ( byval VertexPT* pQuad, byval pTexture as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderQuadTextureColor_SW ( byval VertexPTC* pQuad, byval pTexture as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	declare function gfx_renderTriangleColor_SW ( byval VertexPC* pTriangle, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderTriangleTexture_SW ( byval VertexPT* pTriangle, byval pTexture as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer
	declare function gfx_renderTriangleTextureColor_SW ( byval VertexPTC* pTriangle, byval pTexture as Surface ptr, byval pPalette as Palette ptr, byval bUseColorKey0 as integer, byval argbModifier as integer, byval pRectDest as SurfaceRect ptr, byval pSurfaceDest as Surface ptr ) as integer

	declare function gfx_present_SW ( byval pSurfaceIn as Surface ptr, byval pPalette as Palette ptr ) as integer

end extern

#ENDIF