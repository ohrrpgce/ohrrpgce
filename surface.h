/* OHRRPGCE - Surfaces, part of the graphics API
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

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
	uint8_t* pMaskData;  // Optional, nonzero for opaque pixels. May only be present on 8-bit surfaces.

#ifdef __cplusplus
	uint8_t& pixel8(int x, int y) { return pPaletteData[pitch * y + x]; }
	uint8_t& mask8(int x, int y) { return pMaskData[pitch * y + x]; }
	RGBcolor& pixel32(int x, int y) { return ((RGBcolor*)pColorData)[pitch * y + x]; }

	Surface(int _width, int _height, int _pitch, SurfaceFormat _format, SurfaceUsage _usage)
		: width(_width), height(_height), pitch(_pitch), refcount(1), isview(0),
		format(_format), usage(_usage), base_frame(0), base_surf(0), handle(0), pRawData(0),
		pMaskData(0)
		{}
#endif
};

typedef struct
{
	// right and bottom are INCLUSIVE
	int32_t left, top, right, bottom;
} SurfaceRect;

typedef struct
{
	RGBcolor col[256];
	bool from_backend;  // True if allocated from gfx_palette* API, false from masterpal_to_gfxpal
	//void* handle;     // Not used yet
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
	blendAlgoDither = 0,
	blendAlgoLessDither = 1,
	blendAlgoNoDither = 2,
	blendAlgoLAST = 2
};

// frame_draw additional draw options
typedef struct
{
	// Default 1.
	// (Not implemented for Surfaces)
	int scale;

	// gfx_render* and gfx_surfaceCopy API ONLY, all other functions take
	// separate 'trans' arguments Whether colour 0 (or mask 0, in Surfaces
	// with masks) of 8-bit source textures is transparent.
	bool color_key0;

	// If the destination has a mask, sets the mask for the destination rectangle
	// equal to the mask (or color-key) for the source rectangle. Does not OR them.
	// (TODO: not implemented for 32-bit draws (gfx_surfaceCopy))
	bool write_mask;

	// If false, all blending/modulation options are ignored. Used as an early-out
	bool with_blending;

	// gfx_render* API only (TODO: implement for gfx_surfaceCopy): whether to use
	// the alpha channel of 32-bit source textures
	bool alpha_channel;

	enum BlendMode blend_mode;

	// Multiply each component of the source.
	// Supported only by draw{Tri,Quad}Color and draw{Tri,Quad}TextureColor
	// Default 0xffffffff.
	RGBcolor argbModifier;

	// Redundant to argbModifier.a, but is supported by frame_draw. (TODO: should
	// probably remove)
	float opacity;
} DrawOptions;


//interfaces
#ifdef __cplusplus
extern "C" {
#endif

// In blit.c
extern enum BlendAlgo blend_algo;
extern uint8_t nearcolor_cache[65536];

// In allmodex.bas
extern DrawOptions def_drawoptions;

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
	int gfx_surfaceStretch_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	Surface* gfx_surfaceShrink_SW( Surface *surf, int destWidth, int destHeight );
	int gfx_surfaceCopy_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBcolor* pPalette, Palette16* pPal8, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* opts );

	int gfx_paletteFromRGB_SW( RGBcolor* pColorsIn, RGBPalette** ppPaletteOut );
	int gfx_paletteDestroy_SW( RGBPalette** ppPaletteIn );
	int gfx_paletteUpdate_SW( RGBPalette* pPaletteIn );

	RGBPalette* unrollPalette16( Palette16* pPal8, RGBcolor* pPalette );

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
	extern void (*gfx_surfaceCopy)( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBcolor* pPalette, Palette16* pPal8, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* opts );

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
	int surfaceFrameShim( Frame* pFrameIn, Surface* pSurfaceOut );

	// In blend.h
	//RGBcolor alpha_blend( RGBcolor src, RGBcolor dest, int alpha, enum BlendMode mode, bool channel_alpha = false );

	// In blit.c
	int nearcolor_faster(RGBcolor searchcol);

	// Roto-zooming functions (implemented in rotozoom.c)
	Surface *rotozoomSurface(Surface * src, double angle, double zoomx, double zoomy, int smooth);
	void rotozoomSurfaceSize(int width, int height, double angle, double zoomx, double zoomy, int *dstwidth, int *dstheight);
	Surface* rotateSurface90Degrees(Surface* src, int numClockwiseTurns);


#ifdef __cplusplus
};
#endif

#endif
