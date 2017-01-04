#include <stdlib.h>
#include <string.h>
#include <list>

#include "gfx_newRenderPlan.h"
#include "rasterizer.h"

QuadRasterizer g_rasterizer;
std::list< Surface* > g_surfaces;
std::list< Palette* > g_palettes;

int gfx_surfaceCreate_SW( uint32_t width, uint32_t height, SurfaceFormat format, SurfaceUsage usage, Surface** ppSurfaceOut )
{//done
	if( !ppSurfaceOut )
		return -1;
	Surface *ret = new Surface {NULL, width, height, format, usage, NULL};
	if(format == SF_8bit)
		ret->pPaletteData = new uint8_t[width*height];
	else
		ret->pColorData = new uint32_t[width*height];

	g_surfaces.push_back(ret);
	*ppSurfaceOut = ret;
	return 0;
}

// Return a Surface which is a view onto a Frame. The Surface and Frame should both
// be destroy as normal.
int gfx_surfaceFromFrame_SW( Frame* pFrameIn, Surface** ppSurfaceOut )
{
	if(pFrameIn->w != pFrameIn->pitch)
		// Would have to make a copy of the data
		return -1;
	Surface *ret = new Surface {NULL, (uint32_t)pFrameIn->w, (uint32_t)pFrameIn->h, SF_8bit, SU_Source, frame_reference(pFrameIn)};
	ret->pPaletteData = ret->frame->image;
	*ppSurfaceOut = ret;
	return 0;
}

int gfx_surfaceDestroy_SW( Surface* pSurfaceIn )
{//done
	if(pSurfaceIn)
	{
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
	}
	return 0;
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

int gfx_surfaceStretch_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, Palette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//needs work
	return -1;
}

// bUseColorKey0 says whether color 0 in 8-bit source images is transparent
int gfx_surfaceCopy_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, Palette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( !pSurfaceSrc || !pSurfaceDest )
		return -1;
	if( pSurfaceSrc->format == SF_32bit && pSurfaceDest->format == SF_8bit )
		return -1; //cannot copy 32bit images to palette-types

	int32_t srcWidth = 0, srcHeight = 0, srcX = 0, srcY = 0;
	int32_t destWidth = 0, destHeight = 0, destX = 0, destY = 0;
	
	if( !pRectSrc )
	{//copy entire surface
		srcWidth = pSurfaceSrc->width-1;
		srcHeight = pSurfaceSrc->height-1;
	}
	else
	{//copy region
		srcWidth = pRectSrc->right - pRectSrc->left;
		srcHeight = pRectSrc->bottom - pRectSrc->top;
		srcX = pRectSrc->left;
		srcY = pRectSrc->top;
	}

	if( !pRectDest )
	{//copy to entire surface
		destWidth = pSurfaceDest->width-1;
		destHeight = pSurfaceDest->height-1;
	}
	else
	{//copy to region
		destWidth = pRectDest->right - pRectDest->left;
		destHeight = pRectDest->bottom - pRectDest->top;
		destX = pRectDest->left;
		destY = pRectDest->top;
	}
	
	int8_t value = 0; //used for 8bit to 8bit, or 8bit to 32bit when using a colorkey

	if( pSurfaceSrc->format == SF_32bit ) //both are 32bit (since already validated destination target)
	{
		for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
			{
				pSurfaceDest->pColorData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pSurfaceSrc->pColorData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
			}
		}
	}
	else if( pSurfaceDest->format == SF_8bit ) //both are 8bit
	{
		if( bUseColorKey0 )
		{
			for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
			{
				for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
				{
					value = pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
					if( value )
						pSurfaceDest->pPaletteData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = value;
				}
			}
		}
		else
		{
			for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
			{
				for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
				{
					pSurfaceDest->pPaletteData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
				}
			}
		}
	}
	else //source is 8bit, dest is 32bit
	{
		if( !pPalette )
			return -1;

		if( bUseColorKey0 )
		{
			for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
			{
				for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
				{
					value = pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
					if( value )
						pSurfaceDest->pColorData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pPalette->p[value];
				}
			}
		}
		else
		{
			for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
			{
				for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
				{
					pSurfaceDest->pColorData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pPalette->p[ pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)] ];
				}
			}
		}
	}

	return 0;
}

int gfx_paletteCreate_SW( Palette** ppPaletteOut )
{//done
	if( !ppPaletteOut )
		return -1;
	*ppPaletteOut = new Palette();
	g_palettes.push_back(*ppPaletteOut);
	return 0;
}

// Return a Surface which is a view onto a Frame. The Surface and Frame should both
// be destroy as normal.
int gfx_paletteFromRGB_SW( RGBcolor* pColorsIn, Palette** ppPaletteOut )
{
	Palette *ret = new Palette;
	memcpy(ret->p, pColorsIn, 256 * 4);
	for(int i = 0; i < 256; i++)
		ret->p[i].a = 255;   // Set to opaque (alpha in the input is unused)
	*ppPaletteOut = ret;
	return 0;
}

int gfx_paletteDestroy_SW( Palette* pPaletteIn )
{//done
	if( pPaletteIn )
	{
		g_palettes.remove(pPaletteIn);
		delete pPaletteIn;
	}
	return 0;
}

int gfx_paletteUpdate_SW( Palette* pPaletteIn )
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

int gfx_renderQuadTexture_SW( VertexPT* pQuad, Surface* pTexture, Palette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawQuadTexture(pQuad, pTexture, pPalette, bUseColorKey0, pRectDest, pSurfaceDest);
	return 0;
}

int gfx_renderQuadTextureColor_SW( VertexPTC* pQuad, Surface* pTexture, Palette* pPalette, int bUseColorKey0, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
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

int gfx_renderTriangleTexture_SW( VertexPT* pTriangle, Surface* pTexture, Palette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawTriangleTexture(pTriangle, pTexture, pPalette, bUseColorKey0, pRectDest, pSurfaceDest);
	return 0;
}

int gfx_renderTriangleTextureColor_SW( VertexPTC* pTriangle, Surface* pTexture, Palette* pPalette, int bUseColorKey0, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	if( pSurfaceDest->format == SF_8bit )
		return -1; //can't have 8bit destination

	SurfaceRect tmp = {0, 0, int32_t(pSurfaceDest->width) - 1, int32_t(pSurfaceDest->height) - 1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawTriangleTextureColor(pTriangle, pTexture, pPalette, bUseColorKey0, argbModifier, pRectDest, pSurfaceDest);
	return 0;
}

int gfx_present_SW( Surface* pSurfaceIn, Palette* pPalette )
{//done
	return 0;
}
