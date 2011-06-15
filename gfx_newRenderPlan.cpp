#include "gfx_newRenderPlan.h"
#include "rasterizer.h"
#include <list>

QuadRasterizer g_rasterizer;
std::list< Surface* > g_surfaces;
std::list< Palette* > g_palettes;

int gfx_surfaceCreate( uint32_t width, uint32_t height, SurfaceFormat format, SurfaceUsage usage, Surface** ppSurfaceOut )
{//done
	if( !ppSurfaceOut )
		return -1;
	Surface temp = {width, height, format, usage};
	if(format == SF_8bit)
		temp.pPaletteData = new uint8_t[width*height];
	else
		temp.pColorData = new uint32_t[width*height];

	Surface* pNewSurface = new Surface;
	*pNewSurface = temp;
	g_surfaces.push_back(pNewSurface);

	*ppSurfaceOut = pNewSurface;

	return 0;
}

int gfx_surfaceDestroy( Surface* pSurfaceIn )
{//done
	if(pSurfaceIn)
	{
		if(pSurfaceIn->pRawData)
		{
			if(pSurfaceIn->usage == SF_8bit)
				delete [] pSurfaceIn->pPaletteData;
			else
				delete [] pSurfaceIn->pColorData;
		}
		for(std::list< Surface* >::iterator iter = g_surfaces.begin(); iter != g_surfaces.end(); iter++)
			if(*iter == pSurfaceIn)
			{
				g_surfaces.erase(iter);
				break;
			}
		delete pSurfaceIn;
	}

	return 0;
}

int gfx_surfaceUpdate( Surface* pSurfaceIn )
{//done
	return 0;
}

int gfx_surfaceFill( uint32_t fillColor, SurfaceRect* pRect, Surface* pSurfaceIn )
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

int gfx_surfaceStretch( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette )
{//needs work
	return 0;
}

int gfx_surfaceStretchWithColorKey( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette, uint8_t colorKey )
{//needs work
	return 0;
}

int gfx_surfaceCopy( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette )
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
		for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
			{
				pSurfaceDest->pPaletteData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
			}
		}
	}
	else //source is 8bit, dest is 32bit
	{
		if( !pPalette )
			return -1;

		for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
			{
				pSurfaceDest->pColorData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pPalette->p[ pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)] ];
			}
		}
	}

	return 0;
}

int gfx_surfaceCopyWithColorKey( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette, uint8_t colorKey )
{//done
	if( !pSurfaceSrc || !pSurfaceDest )
		return -1;

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
	
	int8_t value = 0;

	if( pSurfaceDest->format == SF_8bit ) //both are 8bit
	{
		for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
			{
				value = pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
				if(value != colorKey)
					pSurfaceDest->pPaletteData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = value;
			}
		}
	}
	else //source is 8bit, dest is 32bit
	{
		if( !pPalette )
			return -1;

		for(int32_t accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(int32_t accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
			{
				value = pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
				if(value != colorKey)
					pSurfaceDest->pColorData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pPalette->p[ value ];
			}
		}
	}

	return 0;
}

int gfx_paletteCreate( Palette** ppPaletteOut )
{//done
	if( !ppPaletteOut )
		return -1;
	Palette* pNewPalette = new Palette;
	g_palettes.push_back(pNewPalette);

	*ppPaletteOut = pNewPalette;

	return 0;
}

int gfx_paletteDestroy( Palette* pPaletteIn )
{//done
	if( pPaletteIn )
	{
		for(std::list< Palette* >::iterator iter = g_palettes.begin(); iter != g_palettes.end(); iter++)
			if(*iter == pPaletteIn)
			{
				g_palettes.erase(iter);
				break;
			}
		delete pPaletteIn;
	}
	return 0;
}

int gfx_paletteUpdate( Palette* pPaletteIn )
{//done
	return 0;
}

int gfx_renderQuadColor( QuadC* pQuad, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	SurfaceRect tmp = {0,0,pSurfaceDest->width-1,pSurfaceDest->height-1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawColor(pSurfaceDest, pRectDest, pQuad, argbModifier);
	return 0;
}

int gfx_renderQuadTexture( QuadT* pQuad, Surface* pTexture, Palette* pPalette, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	SurfaceRect tmp = {0,0,pSurfaceDest->width-1,pSurfaceDest->height-1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawTexture(pSurfaceDest, pRectDest, pQuad, pTexture, pPalette, argbModifier);
	return 0;
}

int gfx_renderQuadTextureWithColorKey( QuadT* pQuad, Surface* pTexture, Palette* pPalette, uint8_t colorKey, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	SurfaceRect tmp = {0,0,pSurfaceDest->width-1,pSurfaceDest->height-1};
	if( !pRectDest )
		pRectDest = &tmp;
	g_rasterizer.drawTextureWithColorKey(pSurfaceDest, pRectDest, pQuad, pTexture, pPalette, colorKey, argbModifier);
	return 0;
}

int gfx_renderBegin()
{//done
	return 0;
}

int gfx_renderEnd()
{//done
	return 0;
}

int gfx_present( Surface* pSurfaceIn )
{//done
	return 0;
}
