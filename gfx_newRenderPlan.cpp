#include "gfx_newRenderPlan.h"
#include "rasterizer.h"
//#include <memory.h>

QuadRasterizer g_rasterizer;

int gfx_surfaceCreate( unsigned long width, unsigned long height, SurfaceFormat format, SurfaceUsage usage, Surface** ppSurfaceOut )
{//needs adjustment for managed memory model
	if( !ppSurfaceOut )
		return -1;
	Surface temp = {width, height, format, usage};
	if(usage == SF_8bit)
		temp.pPaletteData = new unsigned char[width*height];
	else
		temp.pColorData = new unsigned long[width*height];

	//an internal memory mechanism should be in place, but some other backend adjustments are needed then
	*ppSurfaceOut = new Surface;
	**ppSurfaceOut = temp;

	return 0;
}

int gfx_surfaceDestroy( Surface* pSurfaceIn )
{//needs adjustment for managed memory model
	if(pSurfaceIn)
	{
		if(pSurfaceIn->pRawData)
		{
			if(pSurfaceIn->usage == SF_8bit)
				delete [] pSurfaceIn->pPaletteData;
			else
				delete [] pSurfaceIn->pColorData;
		}
		delete pSurfaceIn;
	}

	return 0;
}

int gfx_surfaceUpdate( Surface* pSurfaceIn )
{//done
	return 0;
}

int gfx_surfaceFill( unsigned long fillColor, SurfaceRect* pRect, Surface* pSurfaceIn )
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

int gfx_surfaceStretchWithColorKey( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette, unsigned char colorKey )
{//needs work
	return 0;
}

int gfx_surfaceCopy( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette )
{//done
	if( !pSurfaceSrc || !pSurfaceDest )
		return -1;
	if( pSurfaceSrc->format == SF_32bit && pSurfaceDest->format == SF_8bit )
		return -1; //cannot copy 32bit images to palette-types

	long srcWidth = 0, srcHeight = 0, srcX = 0, srcY = 0;
	long destWidth = 0, destHeight = 0, destX = 0, destY = 0;
	
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
		for(long accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(long accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
			{
				pSurfaceDest->pColorData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pSurfaceSrc->pColorData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
			}
		}
	}
	else if( pSurfaceDest->format == SF_8bit ) //both are 8bit
	{
		for(long accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(long accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
			{
				pSurfaceDest->pPaletteData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)];
			}
		}
	}
	else //source is 8bit, dest is 32bit
	{
		if( !pPalette )
			return -1;

		for(long accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(long accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
			{
				pSurfaceDest->pColorData[(accumY + destY) * pSurfaceDest->width + (accumX + destX)] = pPalette->p[ pSurfaceSrc->pPaletteData[(accumY + srcY) * pSurfaceSrc->width + (accumX + srcX)] ];
			}
		}
	}

	return 0;
}

int gfx_surfaceCopyWithColorKey( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette, unsigned char colorKey )
{//done
	if( !pSurfaceSrc || !pSurfaceDest )
		return -1;

	long srcWidth = 0, srcHeight = 0, srcX = 0, srcY = 0;
	long destWidth = 0, destHeight = 0, destX = 0, destY = 0;
	
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
	
	unsigned char value = 0;

	if( pSurfaceDest->format == SF_8bit ) //both are 8bit
	{
		for(long accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(long accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
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

		for(long accumY = 0, accumY_max = srcHeight < destHeight ? srcHeight : destHeight; accumY <= accumY_max; accumY++)
		{
			for(long accumX = 0, accumX_max = srcWidth < destWidth ? srcWidth : destWidth; accumX <= accumX_max; accumX++)
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
{//needs adjustment for managed memory model
	if( !ppPaletteOut )
		return -1;
	*ppPaletteOut = new Palette;
	return 0;
}

int gfx_paletteDestroy( Palette* pPaletteIn )
{//needs adjustment for managed memory model
	if( pPaletteIn )
		delete pPaletteIn;
	return 0;
}

int gfx_paletteUpdate( Palette* pPaletteIn )
{//done
	return 0;
}

int gfx_renderQuadColor( QuadC* pQuad, unsigned long argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	g_rasterizer.drawColor(pSurfaceDest, pRectDest, pQuad, argbModifier);
	return 0;
}

int gfx_renderQuadTexture( QuadT* pQuad, Surface* pTexture, Palette* pPalette, unsigned long argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	g_rasterizer.drawTexture(pSurfaceDest, pRectDest, pQuad, pTexture, pPalette, argbModifier);
	return 0;
}

int gfx_renderQuadTextureWithColorKey( QuadT* pQuad, Surface* pTexture, Palette* pPalette, unsigned char colorKey, unsigned long argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//done
	g_rasterizer.drawTexture(pSurfaceDest, pRectDest, pQuad, pTexture, pPalette, colorKey, argbModifier);
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
