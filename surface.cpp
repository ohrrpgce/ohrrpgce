/* Contains implementation of surface.h and gfxRender.hpp routines */

#include <stdlib.h>
#include <string.h>
#include <list>
#include <algorithm>

#include "surface.h"
#include "gfxRender.hpp"
#include "rasterizer.hpp"
#include "common.h"

#define bound(x, low, high)  std::max(std::min(x, high), low)

QuadRasterizer g_rasterizer;
std::list< Surface* > g_surfaces;
std::list< RGBPalette* > g_palettes;

int gfx_surfaceCreate_SW( uint32_t width, uint32_t height, SurfaceFormat format, SurfaceUsage usage, Surface** ppSurfaceOut )
{//done
	if (!ppSurfaceOut) {
		debug(errPromptBug, "surfaceCreate_SW: NULL out ptr");
		return -1;
	}
	Surface *ret = new Surface {NULL, 1, width, height, format, usage, NULL};
	if(format == SF_8bit)
		ret->pPaletteData = new uint8_t[width*height];
	else
		ret->pColorData = new uint32_t[width*height];

	g_surfaces.push_back(ret);
	*ppSurfaceOut = ret;
	return 0;
}

// Return a Surface which is a view onto a Frame. The Surface and Frame should both
// be destroyed as normal.
// (The Frame refcount is incremented)
int gfx_surfaceFromFrame_SW( Frame* pFrameIn, Surface** ppSurfaceOut )
{
	if (pFrameIn->w != pFrameIn->pitch) {
		debug(errPromptBug, "gfx_surfaceFromFrame_SW: pitch != width"); // Unimplemented: Would have to make a copy of the data
		return -1;
	}
	Surface *ret = new Surface {NULL, 1, (uint32_t)pFrameIn->w, (uint32_t)pFrameIn->h, SF_8bit, SU_Source, frame_reference(pFrameIn)};
	ret->pPaletteData = ret->frame->image;
	*ppSurfaceOut = ret;
	return 0;
}

int gfx_surfaceDestroy_SW( Surface** ppSurfaceIn ) {
	if (!ppSurfaceIn) {
		debug(errPromptBug, "surfaceDestroy_SW: NULL in ptr");
		return -1;
	}
	Surface *pSurfaceIn = *ppSurfaceIn;
	*ppSurfaceIn = NULL;
	if (pSurfaceIn) {
		if(--pSurfaceIn->refcount > 0)
			return 0;
		if(pSurfaceIn->frame)
		{
			// Is a view onto a Frame, so don't delete the pixel data ourselves
			frame_unload(&pSurfaceIn->frame);
		}
		else if(pSurfaceIn->pRawData)
		{
			if(pSurfaceIn->format == SF_8bit)
				delete [] pSurfaceIn->pPaletteData;
			else
				delete [] pSurfaceIn->pColorData;
		}
		g_surfaces.remove(pSurfaceIn);
		delete pSurfaceIn;
	}
	return 0;
}

Surface *gfx_surfaceReference_SW( Surface* pSurfaceIn )
{
	if(pSurfaceIn)
		pSurfaceIn->refcount++;
	return pSurfaceIn;
}

int gfx_surfaceUpdate_SW( Surface* pSurfaceIn )
{//done
	return 0;
}

int gfx_surfaceGetData_SW( Surface* pSurfaceIn )
{//done
	return 0;
}

int gfx_surfaceFill_SW( uint32_t fillColor, SurfaceRect* pRect, Surface* pSurfaceIn )
{//done
	if( !pSurfaceIn )
		return -1;

	if(pRect)
	{
		if(pSurfaceIn->format == SF_8bit)
			for(int i = pRect->top; i <= pRect->bottom; i++)
				for(int j = pRect->left; j <= pRect->right; j++)
					pSurfaceIn->pPaletteData[i*pSurfaceIn->width + j] = fillColor;
		else
			for(int i = pRect->top; i <= pRect->bottom; i++)
				for(int j = pRect->left; j <= pRect->right; j++)
					pSurfaceIn->pPaletteData[i*pSurfaceIn->width + j] = fillColor;
	}
	else
	{
		if(pSurfaceIn->format == SF_8bit)
			for(int i = 0, end = pSurfaceIn->width*pSurfaceIn->height; i < end; i++)
				pSurfaceIn->pPaletteData[i] = fillColor;
		else
			for(int i = 0, end = pSurfaceIn->width*pSurfaceIn->height; i < end; i++)
				pSurfaceIn->pColorData[i] = fillColor;
	}

	return 0;
}

int gfx_surfaceStretch_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//needs work
	return -1;
}

// (Not used.) Modify rect inplace
void clampRectToSurface( SurfaceRect* pRect, Surface* pSurf ) {
	pRect->top = bound(pRect->top, 0, (int)pSurf->height - 1);
	pRect->left = bound(pRect->left, 0, (int)pSurf->width - 1);
	pRect->bottom = bound(pRect->bottom, pRect->top, (int)pSurf->height - 1);
	pRect->right = bound(pRect->right, pRect->left, (int)pSurf->width - 1);
}

// The src and dest rectangles may be different sizes; the image is not
// stretched over the rectangle.  Instead the top-left corner of the source rect
// is drawn at the top-left corner of the dest rect.  The rectangles may be over
// the edge of the respective Surfaces; they are clamped. Negative width or
// height means the draw is a noop.
// bUseColorKey0 says whether color 0 in 8-bit source images is transparent
int gfx_surfaceCopy_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest ) {
	if (!pSurfaceSrc || !pSurfaceDest) {
		debug(errPromptBug, "surfaceCopy_SW: NULL ptr %p %p", pSurfaceSrc, pSurfaceDest);
		return -1;
	}
	if (pSurfaceSrc->format == SF_32bit && pSurfaceDest->format == SF_8bit) {
		debug(errPromptBug, "surfaceCopy_SW: can't copy from 32-bit to 8-bit Surface");
		return -1;
	}

	SurfaceRect rectDest, rectSrc;
	if (!pRectDest)
		pRectDest = &(rectDest = {0, 0, (int)pSurfaceDest->width - 1, (int)pSurfaceDest->height - 1});
	if (!pRectSrc)
		pRectSrc = &(rectSrc = {0, 0, (int)pSurfaceSrc->width - 1, (int)pSurfaceSrc->height - 1});

	// Determine the top-left pixel on the src and dest surfaces which is copied.
	int srcX = pRectSrc->left, srcY = pRectSrc->top;
	int destX = pRectDest->left, destY = pRectDest->top;
	if (destX < 0) {
		srcX -= destX;
		destX = 0;
	}
	if (destY < 0) {
		srcY -= destY;
		destY = 0;
	}
	if (srcX < 0) {
		destX -= srcX;
		srcX = 0;
	}
	if (srcY < 0) {
		destY -= srcY;
		srcY = 0;
	}

	// Clamp right/bottom to surface edges and find src/dest rect size (may be negative)
	int srcWidth   = std::min(pRectSrc->right,   (int)pSurfaceSrc->width - 1)   - srcX  + 1;
	int srcHeight  = std::min(pRectSrc->bottom,  (int)pSurfaceSrc->height - 1)  - srcY  + 1;
	int destWidth  = std::min(pRectDest->right,  (int)pSurfaceDest->width - 1)  - destX + 1;
	int destHeight = std::min(pRectDest->bottom, (int)pSurfaceDest->height - 1) - destY + 1;

	int itX_max = std::min(srcWidth, destWidth);
	int itY_max = std::min(srcHeight, destHeight);
	if (itX_max <= 0 || itY_max <= 0)
		return 0;

	// Number of pixels skipped from the end of one row to start of next
	int srcLineEnd = pSurfaceSrc->width - itX_max;
	int destLineEnd = pSurfaceDest->width - itX_max;

	// Two of these are invalid
	uint8_t *restrict srcp8 = &pSurfaceSrc->pixel8(srcX, srcY);
	uint8_t *restrict destp8 = &pSurfaceDest->pixel8(destX, destY);
	uint32_t *restrict srcp32 = &pSurfaceSrc->pixel32(srcX, srcY);
	uint32_t *restrict destp32 = &pSurfaceDest->pixel32(destX, destY);

	if (pSurfaceSrc->format == SF_32bit) { //both are 32bit (since already validated destination target)
		for (int itY = 0; itY < itY_max; itY++) {
			memcpy(destp32, srcp32, 4 * itX_max);
			srcp32 += pSurfaceSrc->width;
			destp32 += pSurfaceDest->width;
		}
	} else if (pSurfaceDest->format == SF_8bit) { //both are 8bit
		if (bUseColorKey0) {
			for (int itY = 0; itY < itY_max; itY++) {
				for (int itX = 0; itX < itX_max; itX++) {
					if (*srcp8)
						*destp8 = *srcp8;
					srcp8++;
					destp8++;
				}
				srcp8 += srcLineEnd;
				destp8 += destLineEnd;
			}
		} else {
			for (int itY = 0; itY < itY_max; itY++) {
				memcpy(destp8, srcp8, 1 * itX_max);
				srcp8  += pSurfaceSrc->width;
				destp8+= pSurfaceDest->width;
			}
		}
	} else { //source is 8bit, dest is 32bit
		if (!pPalette) {
			debug(errPromptBug, "surfaceCopy_SW: NULL palette");
			return -1;
		}

		if (bUseColorKey0) {
			for (int itY = 0; itY < itY_max; itY++) {
				for (int itX = 0; itX < itX_max; itX++)
				{
					if (*srcp8)
						*destp32 = pPalette->col[*srcp8].col;
					srcp8++;
					destp32++;
				}
				srcp8 += srcLineEnd;
				destp32 += destLineEnd;
			}
		} else {
			for (int itY = 0; itY < itY_max; itY++) {
				for (int itX = 0; itX < itX_max; itX++) {
					*destp32++ = pPalette->col[*srcp8++].col;
				}
				srcp8 += srcLineEnd;
				destp32 += destLineEnd;
			}
		}
	}

	return 0;
}

int gfx_paletteCreate_SW( RGBPalette** ppPaletteOut )
{//done
	if( !ppPaletteOut )
		return -1;
	*ppPaletteOut = new RGBPalette();
	g_palettes.push_back(*ppPaletteOut);
	return 0;
}

// Return a Surface which is a view onto a Frame. The Surface and Frame should both
// be destroy as normal.
int gfx_paletteFromRGB_SW( RGBcolor* pColorsIn, RGBPalette** ppPaletteOut )
{
	RGBPalette *ret = new RGBPalette;
	memcpy(ret->col, pColorsIn, 256 * 4);
	for(int i = 0; i < 256; i++)
		ret->col[i].a = 255;   // Set to opaque (alpha in the input is unused)
	*ppPaletteOut = ret;
	return 0;
}

int gfx_paletteDestroy_SW (RGBPalette** ppPaletteIn) {
	if (*ppPaletteIn) {
		g_palettes.remove(*ppPaletteIn);
		delete *ppPaletteIn;
	}
	*ppPaletteIn = NULL;
	return 0;
}

int gfx_paletteUpdate_SW( RGBPalette* pPaletteIn )
{//done
	return 0;
}

int gfx_renderQuadColor_SW( VertexPC* pQuad, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawQuadColor(pQuad, argbModifier, pRectDest, pSurfaceDest);
	return 0;
}

int gfx_renderQuadTexture_SW( VertexPT* pQuad, Surface* pTexture, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawQuadTexture(pQuad, pTexture, pPalette, bUseColorKey0, pRectDest, pSurfaceDest);
	return 0;
}

int gfx_renderQuadTextureColor_SW( VertexPTC* pQuad, Surface* pTexture, RGBPalette* pPalette, int bUseColorKey0, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawQuadTextureColor(pQuad, pTexture, pPalette, bUseColorKey0, argbModifier, pRectDest, pSurfaceDest);
	return 0;
}

int gfx_renderTriangleColor_SW( VertexPC* pTriangle, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawTriangleColor(pTriangle, argbModifier, pRectDest, pSurfaceDest);
	return 0;
}

int gfx_renderTriangleTexture_SW( VertexPT* pTriangle, Surface* pTexture, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawTriangleTexture(pTriangle, pTexture, pPalette, bUseColorKey0, pRectDest, pSurfaceDest);
	return 0;
}

int gfx_renderTriangleTextureColor_SW( VertexPTC* pTriangle, Surface* pTexture, RGBPalette* pPalette, int bUseColorKey0, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawTriangleTextureColor(pTriangle, pTexture, pPalette, bUseColorKey0, argbModifier, pRectDest, pSurfaceDest);
	return 0;
}
