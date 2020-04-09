#ifndef SURFACE_H
#define SURFACE_H

#include "config.h"

typedef union {
	struct {
		// Opaque is a=255. Not pre-multiplied. However, only the rasterizer
		// and BMP import uses the alpha channel, all other code ignores it.
		unsigned char b, g, r, a;
	};
	uint32_t col;
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

typedef struct Surface Surface;
typedef struct Frame Frame;
typedef struct Palette16 Palette16;

struct Surface
{
	int32_t width;
	int32_t height;
	int32_t pitch;      // Measured in pixels, not bytes
	int refcount;
	int isview;         // Is a view onto a Frame or another Surface (see below)
	enum SurfaceFormat format;
	enum SurfaceUsage usage;
	// The following are only used if isview is true; at most one of them is non-NULL
	Frame *base_frame;  // If not NULL, is a view of a whole Frame
	Surface *base_surf; // If not NULL, is a view of part of a Surface

	void* handle;
	union {
		void* pRawData;
		uint32_t* pColorData;
		uint8_t* pPaletteData;
	};

#ifdef __cplusplus
	uint8_t& pixel8(int x, int y) { return pPaletteData[pitch * y + x]; }
	RGBcolor& pixel32(int x, int y) { return ((RGBcolor*)pColorData)[pitch * y + x]; }
#endif
};

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

enum BlendMode
{
	blendModeNormal = 0,
	blendModeAdd = 1,
	blendModeMultiply = 2,
	blendModeLAST = 2
};

enum BlendAlgo
{
	blendAlgoDitherSlow = 0,
	blendAlgoDitherFast = 1,
	blendAlgoNoDither = 2,
	blendAlgoLAST = 2
};

// frame_draw additional draw options
typedef struct
{
	// Default 1.
	// (Not implemented for Surfaces)
	int scale;

	// If the destination has a mask, sets the mask for the destination rectangle
	// equal to the mask (or color-key) for the source rectangle. Does not OR them.
	// (Not implemented for Surfaces)
	boolint write_mask;

	// If false, all blending/modulation options are ignored. Used as an early-out
	boolint with_blending;

	enum BlendMode blend_mode;

	float opacity;
} DrawOptions;

// In blit.c
extern enum BlendAlgo blend_algo;
extern uint8_t nearcolor_cache[32768];

// In allmodex.bas
extern DrawOptions def_drawoptions;

//interfaces
#ifdef __cplusplus
extern "C"
{
#endif

	// Software implementation
	int gfx_debugSurfaces_SW( );
	int gfx_surfaceCreate_SW( int32_t width, int32_t height, enum SurfaceFormat format, enum SurfaceUsage usage, Surface** ppSurfaceOut );
	int gfx_surfaceCreateView_SW( Surface *pSurfaceIn, int x, int y, int width, int height, Surface** ppSurfaceOut );
	int gfx_surfaceCreatePixelsView_SW( void *pixels, int width, int height, int pitch, enum SurfaceFormat format, Surface** ppSurfaceOut );
	int gfx_surfaceCreateFrameView_SW( Frame* pFrameIn, Surface** ppSurfaceOut );
	int gfx_surfaceDestroy_SW( Surface** ppSurfaceIn );
	Surface *gfx_surfaceReference_SW( Surface* pSurfaceIn );
	int gfx_surfaceUpdate_SW( Surface* pSurfaceIn );
	int gfx_surfaceGetData_SW( Surface* pSurfaceIn );
	int gfx_surfaceFill_SW( uint32_t fillColor, SurfaceRect* pRect, Surface* pSurfaceIn );
	int gfx_surfaceFillAlpha_SW( RGBcolor fillColor, double alpha, SurfaceRect* pRect, Surface* pSurface );
	int gfx_surfaceStretch_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	Surface* gfx_surfaceShrink_SW( Surface *surf, int destWidth, int destHeight );
	int gfx_surfaceCopy_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, Palette16* pPal8, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* opts );

	int gfx_paletteCreate_SW( RGBPalette** ppPaletteOut );
	int gfx_paletteFromRGB_SW( RGBcolor* pColorsIn, RGBPalette** ppPaletteOut );
	int gfx_paletteDestroy_SW( RGBPalette** ppPaletteIn );
	int gfx_paletteUpdate_SW( RGBPalette* pPaletteIn );

	// Function pointers to the selected implementation
	extern int (*gfx_surfaceCreate)( int32_t width, int32_t height, enum SurfaceFormat format, enum SurfaceUsage usage, Surface** ppSurfaceOut );
	extern int (*gfx_surfaceWithFrame)( Frame* pFrameIn, Surface** ppSurfaceOut );
	extern int (*gfx_surfaceDestroy)( Surface** ppSurfaceIn );
	extern Surface* (*gfx_surfaceReference)( Surface* pSurfaceIn );
	extern int (*gfx_surfaceUpdate)( Surface* pSurfaceIn );
	extern int (*gfx_surfaceGetData)( Surface* pSurfaceIn );
	extern int (*gfx_surfaceFill)( uint32_t fillColor, SurfaceRect* pRect, Surface* pSurfaceIn );
	extern int (*gfx_surfaceFillAlpha)( RGBcolor fillColor, double alpha, SurfaceRect* pRect, Surface* pSurfaceIn );
	extern int (*gfx_surfaceStretch)( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	extern Surface* (*gfx_surfaceShrink)( Surface *surf, int destWidth, int destHeight );
	extern int (*gfx_surfaceCopy)( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, Palette16* pPal8, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* opts );

	extern int (*gfx_paletteCreate)( RGBPalette** ppPaletteOut );
	extern int (*gfx_paletteFromRGB)( RGBcolor* pColorsIn, RGBPalette** ppPaletteOut );
	extern int (*gfx_paletteDestroy)( RGBPalette** ppPaletteIn );
	extern int (*gfx_paletteUpdate)( RGBPalette* pPaletteIn );

	// Only for surface32_from_pixels and surface32_to_pixels
	enum PixelFormat
	{
		PIXFMT_RGB,   // RGB triples, 3 bytes per pixel
		PIXFMT_GREY,  // Grey level, 1 byte per pixel
	};

	// Convenience and utility functions not specific to any Surface implementation
	void surface_assign( Surface** ptr_to_replace, Surface* new_value );
	Surface* surface_scale( Surface *surf, int destWidth, int destHeight );
	Surface* surface_duplicate( Surface* surf );
	Surface* surface32_from_pixels( char *input, int w, int h, enum PixelFormat format );
	char *surface32_to_pixels( Surface *surf, enum PixelFormat format );

	// Roto-zooming functions (implemented in rotozoom.c)
	Surface *rotozoomSurface(Surface * src, double angle, double zoomx, double zoomy, int smooth);
	void rotozoomSurfaceSize(int width, int height, double angle, double zoomx, double zoomy, int *dstwidth, int *dstheight);
	Surface* rotateSurface90Degrees(Surface* src, int numClockwiseTurns);


#ifdef __cplusplus
};
#endif

#endif
