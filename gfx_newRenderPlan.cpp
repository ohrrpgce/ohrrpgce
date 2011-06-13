#include "gfx_newRenderPlan.h"
#include "rasterizer.h"
//#include <memory.h>

QuadRasterizer g_rasterizer;

int gfx_surfaceCreate( unsigned long width, unsigned long height, SurfaceFormat format, SurfaceUsage usage, Surface** ppSurfaceOut )
{//needs adjustment for managed memory model
	if( !pSurfaceOut )
		return -1;
	Surface temp = {width, height, format, usage};
	if(usage == SF_8bit)
		temp.pPaletteData = new unsigned char[width*height];
	else
		temp.pColorData = new unsigned long[width*height];

	//an internal memory mechanism should be in place, but some other backend adjustments are needed then
	*pSurfaceOut = new Surface;
	**pSurfaceOut = temp;

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
{//needs work
	return 0;
}

int gfx_surfaceCopyWithColorKey( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette, unsigned char colorKey )
{//needs work
	return 0;
}

int gfx_paletteCreate( Palette** ppPaletteOut )
{//needs adjustment for managed memory model
	if( !pPaletteOut )
		return -1;
	*pPaletteOut = new Palette;
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
