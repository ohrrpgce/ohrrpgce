/* OHRRPGCE - Surfaces
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * Contains implementation of surface.h and gfxRender.hpp routines.
 */

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include <stdlib.h>
#include <string.h>
#include <list>
#include <algorithm>

#include "mutex.hpp"
#include "surface.h"
#include "allmodex.h"
#include "gfxRender.hpp"
#include "rasterizer.hpp"
#include "errorlog.h"
#include "blend.h"

void clampRectToSurface( SurfaceRect* inRect, SurfaceRect* outRect, Surface* pSurf );

#define bound(x, low, high)  std::max(std::min(x, high), low)

#ifdef USE_RASTERIZER
// g_rasterizer has no state, so is threadsafe
QuadRasterizer g_rasterizer;
#endif

// Access to g_surfaces and g_palettes is gated by surfaceMutex
std::list< Surface* > g_surfaces;
//std::list< RGBPalette* > g_palettes;

mutex surfaceMutex;

// Print out all Surfaces, return the number
int gfx_debugSurfaces_SW() {
	debuginfo("%d Surfaces:", (int)g_surfaces.size());
	for (auto pSurf : g_surfaces) {
		debuginfo("%p %d*%d refc=%d view=%d 32bit=%d base_surf=%p base_frame=%p", pSurf, pSurf->width, pSurf->height, pSurf->refcount, pSurf->isview, (pSurf->format == SF_32bit), pSurf->base_surf, pSurf->base_frame);
	}
	return g_surfaces.size();
}

int gfx_surfaceCreate_SW( int32_t width, int32_t height, SurfaceFormat format, SurfaceUsage usage, Surface** ppSurfaceOut )
{//done
	if (!ppSurfaceOut) {
		debug(errShowBug, "surfaceCreate_SW: NULL out ptr");
		return -1;
	}
	Surface *ret = new Surface(width, height, width, format, usage);
	ret->isview = 0;
	if(format == SF_8bit)
		ret->pPaletteData = new uint8_t[width*height]();  //zero-initialise
	else
		ret->pColorData = new uint32_t[width*height]();
	// No mask

	surfaceMutex.lock();
	g_surfaces.push_back(ret);
	surfaceMutex.unlock();

	*ppSurfaceOut = ret;
	return 0;
}

// Return a Surface which is a view onto an existing Surface. Both should be
// destroyed as usual.
// Note: width and height are not inclusive
int gfx_surfaceCreateView_SW( Surface *pSurfaceIn, int x, int y, int width, int height, Surface** ppSurfaceOut )
{//done
	if (!ppSurfaceOut) {
		debug(errShowBug, "surfaceCreateView_SW: NULL out ptr");
		return -1;
	}
	if (x < 0) {
		width -= -x;
		x = 0;
	}
	if (y < 0) {
		height -= -y;
		y = 0;
	}
	width = bound(width, 0, pSurfaceIn->width - x);
	height = bound(height, 0, pSurfaceIn->height - y);
	Surface *ret = new Surface(width, height, pSurfaceIn->pitch, pSurfaceIn->format, pSurfaceIn->usage);
	ret->isview = 1;
	if(ret->format == SF_8bit) {
		ret->pPaletteData = &pSurfaceIn->pixel8(x, y);
		if (pSurfaceIn->pMaskData)
			ret->pMaskData = &pSurfaceIn->mask8(x, y);
	} else {
		ret->pColorData = (uint32_t*)&pSurfaceIn->pixel32(x, y);
	}

	ret->base_surf = pSurfaceIn;
	gfx_surfaceReference_SW(pSurfaceIn);

	surfaceMutex.lock();
	g_surfaces.push_back(ret);
	surfaceMutex.unlock();
	*ppSurfaceOut = ret;
	return 0;
}

int gfx_surfaceCreatePixelsView_SW( void *pixels, int width, int height, int pitch, SurfaceFormat format, Surface** ppSurfaceOut )
{
	if (!ppSurfaceOut) {
		debug(errShowBug, "surfaceCreatePixelsView: NULL out ptr");
		return -1;
	}
	Surface *ret = new Surface(width, height, pitch, format, SU_Staging);
	ret->isview = 1;
	ret->pRawData = pixels;

	surfaceMutex.lock();
	g_surfaces.push_back(ret);
	surfaceMutex.unlock();
	*ppSurfaceOut = ret;
	return 0;
}

// Return a Surface which is a view onto a Frame. The Surface and Frame should both
// be destroyed as normal.
// (The Frame refcount is incremented)
int gfx_surfaceCreateFrameView_SW( Frame* pFrameIn, Surface** ppSurfaceOut )
{
	if (pFrameIn->surf) {
		// The Frame is a view onto a surface. We assume that it's a view of
		// the whole surface, because that's all that's currently possible.
		// This is a temporary kludge anyway.
		return gfx_surfaceCreateView_SW(pFrameIn->surf, 0, 0, pFrameIn->w, pFrameIn->h, ppSurfaceOut);
	}
	Surface *ret = new Surface(pFrameIn->w, pFrameIn->h, pFrameIn->pitch, SF_8bit, SU_Source);
	ret->isview = 1;
	ret->base_frame = frame_reference(pFrameIn);
	ret->pPaletteData = pFrameIn->image;
	ret->pMaskData = pFrameIn->mask;
	*ppSurfaceOut = ret;
	return 0;
}

// Copy data from a Frame* into a Surface*.
// This is a temporary kludge to avoid needing to call gfx_surfaceCreateView in frame_draw
// to draw a Frame onto a Surface, where the new/delete overhead would be huge.
// OK to call gfx_surfaceDestroy on pSurfaceOut
int surfaceFrameShim( Frame* pFrameIn, Surface* pSurfaceOut )
{
	if (pFrameIn->surf) {
		// The Frame is a view onto a surface; error.
		return 1;
	}
	pSurfaceOut->width = pFrameIn->w;
	pSurfaceOut->height = pFrameIn->h;
	pSurfaceOut->pitch = pFrameIn->pitch;
	pSurfaceOut->pPaletteData = pFrameIn->image;
	pSurfaceOut->pMaskData = pFrameIn->mask;
	pSurfaceOut->format = SF_8bit;
	pSurfaceOut->usage = SU_Source;
	pSurfaceOut->refcount = 999;  //Ensure never deleted
	pSurfaceOut->isview = 1;
	pSurfaceOut->base_frame = pFrameIn;  //For debugging; should never be used for anything
	pSurfaceOut->base_surf = NULL;
	pSurfaceOut->handle = NULL;
	return 0;
}

int gfx_surfaceDestroy_SW( Surface** ppSurfaceIn ) {
	if (!ppSurfaceIn) {
		debug(errShowBug, "surfaceDestroy_SW: NULL in ptr");
		return -1;
	}
	Surface *pSurfaceIn = *ppSurfaceIn;
	*ppSurfaceIn = NULL;
	if (pSurfaceIn) {
		if(--pSurfaceIn->refcount > 0)
			return 0;
		if(pSurfaceIn->isview) {
			// We don't own the pixel data, deref the parent instead if
			// it's a Frame or Surface, rather than a view on a pixel buffer
			if(pSurfaceIn->base_frame) {
				frame_unload(&pSurfaceIn->base_frame);
			} else if(pSurfaceIn->base_surf) {
				gfx_surfaceDestroy_SW(&pSurfaceIn->base_surf);
			}
		}
		else {
			if(pSurfaceIn->pRawData) {
				if(pSurfaceIn->format == SF_8bit)
					delete [] pSurfaceIn->pPaletteData;
				else
					delete [] pSurfaceIn->pColorData;
			}
			if(pSurfaceIn->pMaskData)
				delete [] pSurfaceIn->pMaskData;
		}
		surfaceMutex.lock();
		g_surfaces.remove(pSurfaceIn);
		surfaceMutex.unlock();
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

// fillColor is either an RGBcolor or a palette index!!
int gfx_surfaceFill_SW( uint32_t fillColor, SurfaceRect* pRect, Surface* pSurface )
{//done
	if( !pSurface )
		return -1;

	SurfaceRect rect;
	clampRectToSurface(pRect, &rect, pSurface);

	if(pSurface->format == SF_8bit)
		for(int y = rect.top; y <= rect.bottom; y++)
			for(int x = rect.left; x <= rect.right; x++)
				pSurface->pixel8(x, y) = fillColor;
	else
		for(int y = rect.top; y <= rect.bottom; y++)
			for(int x = rect.left; x <= rect.right; x++)
				pSurface->pixel32(x, y).col = fillColor;

	return 0;
}

// Draw a transparent rect - 32bit only! alpha must be from 0. to 1.
// pRect may be NUL for the whole Surface.
int gfx_surfaceFillAlpha_SW( RGBcolor fillColor, double alpha, SurfaceRect* pRect, Surface* pSurface )
{
	if (!pSurface)
		return -1;

	if (pSurface->format == SF_8bit)
		// Call trans_rectangle instead.
		return -1;

	SurfaceRect rect;
	clampRectToSurface(pRect, &rect, pSurface);

	for(int y = rect.top; y <= rect.bottom; y++) {
		for(int x = rect.left; x <= rect.right; x++) {
			RGBcolor srcColor = pSurface->pixel32(x, y);
			int fillA = 256 * alpha, srcA = 256 - fillA;

			//integer method of blending
			RGBcolor finalColor;
			finalColor.r = ( srcColor.r * srcA + fillColor.r * fillA ) >> 8;
			finalColor.g = ( srcColor.g * srcA + fillColor.g * fillA ) >> 8;
			finalColor.b = ( srcColor.b * srcA + fillColor.b * fillA ) >> 8;
			finalColor.a = 255;
			pSurface->pixel32(x, y) = finalColor;
		}
	}

	return 0;
}

int gfx_surfaceStretch_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest )
{//needs work
	return -1;
}

// input is a buffer of pixels, formatted according to format. Convert to a SF_32bit surface (BGRA).
Surface *surface32_from_pixels( char *restrict input, int w, int h, PixelFormat format ) {
	Surface *ret;
	if (gfx_surfaceCreate(w, h, SF_32bit, SU_Staging, &ret))
		return NULL;

	for (int y = 0; y < h; y++) {
		for (int x = 0; x < w; x++) {
			RGBcolor &col = ret->pixel32(x, y);
			col.a = 255;
			if (format == PIXFMT_GREY) {
				col.r = col.g = col.b = input[0];
				input += 1;
			} else {
				col.r = input[0];
				col.g = input[1];
				col.b = input[2];
				input += 3;
			}
		}
	}
	return ret;
}

// Convert a SF_32bit Surface to a pixel buffer formatted according to format. free() the result.
char *surface32_to_pixels( Surface *surf, PixelFormat format ) {
	if (!surf || surf->format != SF_32bit)
		return NULL;

	int bytes_per_px;
	if (format == PIXFMT_GREY)
		bytes_per_px = 1;
	else
		bytes_per_px = 3;

	char *ret = (char*)malloc(surf->width * surf->height * bytes_per_px);
	if (!ret) return ret;

	char *out = ret;
	for (int y = 0; y < surf->height; y++) {
		for (int x = 0; x < surf->width; x++) {
			RGBcolor &col = surf->pixel32(x, y);
			if (format == PIXFMT_GREY) {
				*out++ = col.r;
			} else {
				*out++ = col.r;
				*out++ = col.g;
				*out++ = col.b;
			}
		}
	}
	return ret;
}

Surface* surface_duplicate( Surface* surf ) {
	Surface *ret;
	if (gfx_surfaceCreate( surf->width, surf->height, surf->format, surf->usage, &ret ))
		return NULL;
	// def_drawoptions.color_key0 == false
	gfx_surfaceCopy(NULL, surf, NULL, NULL, NULL, ret, &def_drawoptions);

	return ret;
}

// This choice of precision allows downscaling by a factor of 4096x without overflow
#define FIXEDPNT 0x1000
typedef unsigned int fixedpoint;  // A number multipled by FIXEDPNT

// Write out a scaled down row or column.
// srcp[i * srcpstep] are the input RGBcolor pixels, destp[i * destpstep] are
// the output pixels, i varies in [0, num_out_pixels).
// runlen is the number of input pixels to mix into each output pixel.
static void scalerow(RGBcolor *srcp, int srcpstep, RGBcolor *destp, int destpstep, int num_out_pixels, fixedpoint runlen) {
	// Accumulators
	fixedpoint Racc = 0, Gacc = 0, Bacc = 0;
	fixedpoint run;  // Number of pixels left to mix into the accumulators
	fixedpoint pos = 0;   // Position within the current src pixel; a remainder in range [0, FIXEDPNT)

	for (int outpix = 0; outpix < num_out_pixels; outpix++) {
		run = runlen;
		// Length of the dest pixel that overlaps the current src pixel
		fixedpoint overlap = std::min(runlen, FIXEDPNT - pos);
		Racc = srcp->r * overlap;
		Gacc = srcp->g * overlap;
		Bacc = srcp->b * overlap;
		run -= overlap;
		pos = (pos + overlap) % FIXEDPNT;
		if (!pos)
			srcp += srcpstep;
		if (run) {
			// Read any whole pixels
			for (int i = run / FIXEDPNT; i; i--) {
				Racc += srcp->r * FIXEDPNT;
				Gacc += srcp->g * FIXEDPNT;
				Bacc += srcp->b * FIXEDPNT;
				run -= FIXEDPNT;
				srcp += srcpstep;
			}
			if (run) {
				// Read the remainder from the final src pixel
				Racc += srcp->r * run;
				Gacc += srcp->g * run;
				Bacc += srcp->b * run;
			}
			pos = run;
		}
		destp->b = uint8_t(Bacc/runlen);
		destp->g = uint8_t(Gacc/runlen);
		destp->r = uint8_t(Racc/runlen);
		destp->a = 255;
		destp += destpstep;
	}
}

// Scale a 32bit Surface to a given size using the 'pixel mixing' method (I don't
// know a standard name); basically the inverse of bilinear interpolation.
// Ignores alpha.
Surface* surface_scale(Surface *surf, int destWidth, int destHeight) {
	if (surf->format != SF_32bit) {
		debug(errShowBug, "surface_scale: input must be 32-bit Surface");
		return NULL;
	}
	if (destWidth < 1 || destHeight < 1) {
		debug(errError, "surface_scale: invalid dest size %d*%d (src size %d*%d)",
		      destWidth, destWidth, surf->width, surf->height);
		return NULL;
	}

	Surface *dest, *temp;
	if (gfx_surfaceCreate(destWidth, destHeight, SF_32bit, SU_Staging, &dest))
		return NULL;
	if (gfx_surfaceCreate(destWidth, surf->height, SF_32bit, SU_Staging, &temp))
		return NULL;  // Memory leak; I don't care

	// Scale surf horizontally, put result in temp
	fixedpoint runlen = surf->width * FIXEDPNT / destWidth;  // Rounds down, so we will never read off the end
	for (int y = 0; y < surf->height; y++) {
		scalerow(&surf->pixel32(0, y), 1, &temp->pixel32(0, y), 1, dest->width, runlen);
	}

	// Scale temp vertically, put result in dest
	runlen = surf->height * FIXEDPNT / destHeight;
	for (int x = 0; x < temp->width; x++) {
		scalerow(&temp->pixel32(x, 0), temp->width, &dest->pixel32(x, 0), dest->width, dest->height, runlen);
	}

	gfx_surfaceDestroy(&temp);
	return dest;
}

// Clamp *inRect to dimensions of the rect, out in *outRect.
// inRect and outRect can be the same. inRect can be NULL, for the whole Surface
void clampRectToSurface( SurfaceRect* inRect, SurfaceRect* outRect, Surface* pSurf ) {
	if (inRect) {
		outRect->top = bound(inRect->top, 0, pSurf->height - 1);
		outRect->left = bound(inRect->left, 0, pSurf->width - 1);
		outRect->bottom = bound(inRect->bottom, inRect->top, pSurf->height - 1);
		outRect->right = bound(inRect->right, inRect->left, pSurf->width - 1);
	} else {
		*outRect = {0, 0, pSurf->width - 1, pSurf->height - 1};
	}
}

// Draw a Surface, optionally with transparency or palette, but without scaling/stretching/rotation.
// The src and dest rectangles may be different sizes; the image is not
// stretched over the rectangle.  Instead the top-left corner of the source rect
// is drawn at the top-left corner of the dest rect.  The rectangles may be over
// the edge of the respective Surfaces; they are clamped. Negative width or
// height means the draw is a noop.
int gfx_surfaceCopy_SW( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, RGBcolor* pPalette, Palette16* pPal8, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts ) {
	if (!pSurfaceSrc || !pSurfaceDest || !pOpts) {
		debug(errShowBug, "surfaceCopy_SW: NULL ptr %p %p %p", pSurfaceSrc, pSurfaceDest, pOpts);
		return -1;
	}

	SurfaceRect rectDest, rectSrc;
	if (!pRectDest)
		pRectDest = &(rectDest = {0, 0, pSurfaceDest->width - 1, pSurfaceDest->height - 1});
	if (!pRectSrc)
		pRectSrc = &(rectSrc = {0, 0, pSurfaceSrc->width - 1, pSurfaceSrc->height - 1});

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
	int srcWidth   = std::min(pRectSrc->right,   pSurfaceSrc->width - 1)   - srcX  + 1;
	int srcHeight  = std::min(pRectSrc->bottom,  pSurfaceSrc->height - 1)  - srcY  + 1;
	int destWidth  = std::min(pRectDest->right,  pSurfaceDest->width - 1)  - destX + 1;
	int destHeight = std::min(pRectDest->bottom, pSurfaceDest->height - 1) - destY + 1;

	int itX_max = std::min(srcWidth, destWidth);
	int itY_max = std::min(srcHeight, destHeight);
	if (itX_max <= 0 || itY_max <= 0)
		return 0;

	int alpha = 256;
	bool with_blending = pOpts->with_blending;
	if (with_blending) {
		alpha = pOpts->opacity * 256;
		if (pOpts->opacity <= 0.)
			return 0;  //TODO: remove this if write_mask is implemented!
		if (pOpts->opacity >= 1. && pOpts->blend_mode == blendModeNormal)
			with_blending = false;
	}

	// Number of pixels skipped from the end of one row to start of next
	int srcLineEnd = pSurfaceSrc->pitch - itX_max;
	int destLineEnd = pSurfaceDest->pitch - itX_max;

	// Two of the following pointers are invalid
	uint8_t *restrict srcp8 = &pSurfaceSrc->pixel8(srcX, srcY);
	uint8_t *restrict destp8 = &pSurfaceDest->pixel8(destX, destY);
	RGBcolor *restrict srcp32 = &pSurfaceSrc->pixel32(srcX, srcY);
	RGBcolor *restrict destp32 = &pSurfaceDest->pixel32(destX, destY);
	// maskp is only valid for 8 bit source
	uint8_t *restrict maskp;
	if (pSurfaceSrc->pMaskData)
		maskp = &pSurfaceSrc->mask8(srcX, srcY);
	else
		maskp = srcp8;

	if (pSurfaceSrc->format == SF_32bit && pSurfaceDest->format == SF_32bit) {
		// TODO: implement alpha channel-based blending
		RGBcolor colorkey = {};
		if (pPalette)
			colorkey = pPalette[0];
		if (!pOpts->color_key0)
			colorkey.col = 0;

		if (with_blending) {
			for (int itY = 0; itY < itY_max; itY++) {
				for (int itX = 0; itX < itX_max; itX++) {
					if (srcp32->col != colorkey.col)
						*destp32 = alpha_blend(*srcp32, *destp32, alpha, pOpts->blend_mode);
					srcp32++;
					destp32++;
				}
				srcp32 += srcLineEnd;
				destp32 += destLineEnd;
			}
		} else if (pOpts->color_key0) {
			for (int itY = 0; itY < itY_max; itY++) {
				for (int itX = 0; itX < itX_max; itX++) {
					if (srcp32->col != colorkey.col)
						*destp32 = *srcp32;
					srcp32++;
					destp32++;
				}
				srcp32 += srcLineEnd;
				destp32 += destLineEnd;
			}
		} else {
			for (int itY = 0; itY < itY_max; itY++) {
				memcpy(destp32, srcp32, 4 * itX_max);
				srcp32 += pSurfaceSrc->pitch;
				destp32 += pSurfaceDest->pitch;
			}
		}

	} else if (pSurfaceSrc->format == SF_32bit && pSurfaceDest->format == SF_8bit) {
		// Slow fallback to doing master palette lookups. Because this is just a fallback, we don't
		// do any dithering like blitohr() does.
		// TODO: implement alpha channel-based blending and colorkeying
		if (!pPalette) {
			debug(errShowBug, "surfaceCopy_SW: NULL palette");
			return -1;
		}

		for (int itY = 0; itY < itY_max; itY++) {
			for (int itX = 0; itX < itX_max; itX++) {
				RGBcolor destcol;
				if (with_blending)
					destcol = alpha_blend(*srcp32++, pPalette[*destp8], alpha, pOpts->blend_mode);
				else
					destcol = *srcp32++;
				*destp8 = nearcolor_faster(destcol);
				destp8++;
			}
			srcp32 += srcLineEnd;
			destp8 += destLineEnd;
		}

	} else if (pSurfaceSrc->format == SF_8bit && pSurfaceDest->format == SF_8bit) {
		// alpha/opacity ignored, not supported. Handled by blitohr in blit.c
		// so this path is not typically used
		if (pOpts->color_key0) {
			for (int itY = 0; itY < itY_max; itY++) {
				for (int itX = 0; itX < itX_max; itX++) {
					if (pPal8) {
						if (*maskp)
							*destp8 = pPal8->col[*srcp8];
					} else {
						if (*maskp)
							*destp8 = *srcp8;
					}
					srcp8++;
					maskp++;
					destp8++;
				}
				srcp8 += srcLineEnd;
				maskp += srcLineEnd;
				destp8 += destLineEnd;
			}
		} else {
			if (pPal8) {
				for (int itY = 0; itY < itY_max; itY++) {
					for (int itX = 0; itX < itX_max; itX++)
						*destp8++ = pPal8->col[*srcp8++];
					srcp8 += srcLineEnd;
					destp8 += destLineEnd;
				}
			} else {
				for (int itY = 0; itY < itY_max; itY++) {
					memcpy(destp8, srcp8, 1 * itX_max);
					srcp8 += pSurfaceSrc->pitch;
					destp8 += pSurfaceDest->pitch;
				}
			}
		}

	} else { //source is 8bit, dest is 32bit
		if (!pPalette) {
			debug(errShowBug, "surfaceCopy_SW: NULL palette");
			return -1;
		}

		// Form a temp palette to avoid double-indirection on every pixel
		RGBcolor *restrict pal32 = unrollPalette16(pPal8, pPalette)->col;

		if (pOpts->color_key0) {
			for (int itY = 0; itY < itY_max; itY++) {
				for (int itX = 0; itX < itX_max; itX++) {
					if (*maskp) {
						if (with_blending)
							*destp32 = alpha_blend(pal32[*srcp8], *destp32, alpha, pOpts->blend_mode);
						else
							*destp32 = pal32[*srcp8];
					}
					srcp8++;
					maskp++;
					destp32++;
				}
				srcp8 += srcLineEnd;
				maskp += srcLineEnd;
				destp32 += destLineEnd;
			}
		} else {
			for (int itY = 0; itY < itY_max; itY++) {
				for (int itX = 0; itX < itX_max; itX++) {
					if (with_blending)
						*destp32 = alpha_blend(pal32[*srcp8], *destp32, alpha, pOpts->blend_mode);
					else
						*destp32 = pal32[*srcp8];
					srcp8++;
					destp32++;
				}
				srcp8 += srcLineEnd;
				destp32 += destLineEnd;
			}
		}
	}

	return 0;
}


//This is slow. Use masterpal_to_gfxpal instead
int gfx_paletteFromRGB_SW( RGBcolor* pColorsIn, RGBPalette** ppPaletteOut )
{
	RGBPalette *ret = new RGBPalette;
	ret->from_backend = true;
	memcpy(ret->col, pColorsIn, 256 * 4);
	for(int i = 0; i < 256; i++)
		ret->col[i].a = 255;   // Set to opaque, though it should be anyway
	*ppPaletteOut = ret;
	return 0;
}

//Produce a temporary palette from a Palette16
//Hack: pPalette can be either a RGBPalette* or a RGBcolor[256]; it's assumed it's actually
//a RGBPalette* if pPal8==NULL!
RGBPalette* unrollPalette16(Palette16* pPal8, RGBcolor* pPalette) {
	static RGBPalette temppal;
	if (pPal8) {
		for (int idx = 0; idx < pPal8->numcolors; idx++) {
			temppal.col[idx] = pPalette[pPal8->col[idx]];
		}
		temppal.from_backend = false;
		return &temppal;
	} else {
		return (RGBPalette*)pPalette;
	}
}

int gfx_paletteDestroy_SW (RGBPalette** ppPaletteIn) {
	if (*ppPaletteIn && (*ppPaletteIn)->from_backend) {
		/*
		surfaceMutex.lock();
		g_palettes.remove(*ppPaletteIn);
		surfaceMutex.unlock();
		*/
		delete *ppPaletteIn;
	}
	*ppPaletteIn = NULL;
	return 0;
}

int gfx_paletteUpdate_SW( RGBPalette* pPaletteIn )
{//done
	return 0;
}

#ifdef USE_RASTERIZER

void gfx_renderQuadColor_SW( VertexPC* pQuad, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts ) {
	g_rasterizer.drawQuadColor(pQuad, pRectDest, pSurfaceDest, pOpts);
}

void gfx_renderQuadTexture_SW( VertexPT* pQuad, Surface* pTexture, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts ) {
	g_rasterizer.drawQuadTexture(pQuad, pTexture, pPalette, pRectDest, pSurfaceDest, pOpts);
}

void gfx_renderQuadTextureColor_SW( VertexPTC* pQuad, Surface* pTexture, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts ) {
	g_rasterizer.drawQuadTextureColor(pQuad, pTexture, pPalette, pRectDest, pSurfaceDest, pOpts);
}

void gfx_renderTriangleColor_SW( VertexPC* pTriangle, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts ) {
	g_rasterizer.drawTriangleColor(pTriangle, pRectDest, pSurfaceDest, pOpts);
}

void gfx_renderTriangleTexture_SW( VertexPT* pTriangle, Surface* pTexture, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts ) {
	g_rasterizer.drawTriangleTexture(pTriangle, pTexture, pPalette, pRectDest, pSurfaceDest, pOpts);
}

void gfx_renderTriangleTextureColor_SW( VertexPTC* pTriangle, Surface* pTexture, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts ) {
	g_rasterizer.drawTriangleTextureColor(pTriangle, pTexture, pPalette, pRectDest, pSurfaceDest, pOpts);
}

#endif
