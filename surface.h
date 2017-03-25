#ifndef SURFACE_H
#define SURFACE_H

#include <stdint.h>
#include "allmodex.h"

typedef union {
	uint32_t col;
	struct {
		unsigned char r, g, b, a;
	};
} RGBcolor;

enum SurfaceFormat
{
	SF_8bit = 0,
	SF_32bit = 1,
};

enum SurfaceUsage
{
	SU_Source = 0,       // Surfaces that can be drawn to render targets
	SU_RenderTarget = 1,
	SU_Staging = 2,      // Surfaces that don't get sent to GPU
};

typedef struct _Frame Frame;

typedef struct
{
	void* handle;
	int refcount;
	uint32_t width;
	uint32_t height;
	enum SurfaceFormat format;
	enum SurfaceUsage usage;
	Frame *frame;       // If not NULL, is a view onto a Frame which owns the data
	union
	{
		void* pRawData;
		uint32_t* pColorData;
		uint8_t* pPaletteData;
	};
} Surface;

typedef struct
{
	// right and bottom are INCLUSIVE
	int32_t left, top, right, bottom;
} SurfaceRect;

typedef struct
{
	void* handle;
	RGBcolor col[256];
} RGBPalette;

//interfaces
#ifdef __cplusplus
extern "C"
{
#endif

	int gfx_surfaceCreate_SW( uint32_t width, uint32_t height, enum SurfaceFormat format, enum SurfaceUsage usage, Surface** ppSurfaceOut );
	int gfx_surfaceFromFrame_SW( Frame* pFrameIn, Surface** ppSurfaceOut );
	int gfx_surfaceDestroy_SW( Surface* pSurfaceIn );
	Surface *gfx_surfaceReference_SW( Surface* pSurfaceIn );
	int gfx_surfaceUpdate_SW( Surface* pSurfaceIn );
	int gfx_surfaceGetData_SW( Surface* pSurfaceIn );
	int gfx_surfaceFill_SW( uint32_t fillColor, SurfaceRect* pRect, Surface* pSurfaceIn );
	int gfx_surfaceStretch_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	int gfx_surfaceCopy_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest );

	int gfx_paletteCreate_SW( RGBPalette** ppPaletteOut );
	int gfx_paletteFromRGB_SW( RGBcolor* pColorsIn, RGBPalette** ppPaletteOut );
	int gfx_paletteDestroy_SW( RGBPalette* pPaletteIn );
	int gfx_paletteUpdate_SW( RGBPalette* pPaletteIn );
#ifdef __cplusplus
};
#endif

#endif
