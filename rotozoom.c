/*
Rotozoomer and zoomer for 32bit or 8bit surfaces
Adapted from SDL2_rotozoom.h from sdl2_gfx, version 1.0.4 - zlib licensed.
http://www.ferzkopp.net/wordpress/2016/01/02/sdl_gfx-sdl2_gfx/
https://sourceforge.net/projects/sdl2gfx/

Copyright (C) 2012-2014  Andreas Schiffler

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
claim that you wrote the original software. If you use this software
in a product, an acknowledgment in the product documentation would be
appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be
misrepresented as being the original software.

3. This notice may not be removed or altered from any source
distribution.

Andreas Schiffler -- aschiffler at ferzkopp dot net

*/

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "misc.h"
#include "surface.h"

#define MAX(a,b)    (((a) > (b)) ? (a) : (b))

/*
Number of guard rows added to destination surfaces.

This is a simple but effective workaround for observed issues.
These rows allocate extra memory and are then hidden from the surface.
Rows are added to the end of destination surfaces when they are allocated.
This catches any potential overflows which seem to happen with
just the right src image dimensions and scale/rotation and can lead
to a situation where the program can segfault.
*/
#define GUARD_ROWS 2

/*
Lower limit of absolute zoom factor or rotation degrees.
*/
#define VALUE_LIMIT	0.001



/*
Internal 32 bit Zoomer with optional anti-aliasing by bilinear interpolation.

Zooms 32 bit RGBA/ABGR 'src' surface to 'dst' surface.
Assumes src and dst surfaces are of 32 bit depth.
Assumes dst surface was allocated with the correct dimensions.

`src`    : The surface to zoom (input).
`dst`    : The zoomed surface (output).
`flipx`  : Flag indicating if the image should be horizontally flipped.
`flipy`  : Flag indicating if the image should be vertically flipped.
`smooth` : Whether to antialias.

Returns 0 for success or -1 for error.
*/
int _zoomSurfaceRGBA(Surface *src, Surface *dst, boolint flipx, boolint flipy, boolint smooth)
{
	int x, y, sx, sy, ssx, ssy, *sax, *say, *csax, *csay, *salast, csx, csy, ex, ey, cx, cy, sstep, sstepx, sstepy;
	RGBcolor *c00, *c01, *c10, *c11;
	RGBcolor *sp, *csp, *dp;
	int spixelgap, spixelw, spixelh, dgap, t1, t2;

	/* Allocate memory for row/column increments */
	if ((sax = (int *) malloc((dst->width + 1) * sizeof(uint32_t))) == NULL) {
		return -1;
	}
	if ((say = (int *) malloc((dst->height + 1) * sizeof(uint32_t))) == NULL) {
		free(sax);
		return -1;
	}

	/* Precalculate row increments */
	spixelw = (src->width - 1);
	spixelh = (src->height - 1);
	if (smooth) {
		sx = (int) (65536.0 * (float) spixelw / (float) (dst->width - 1));
		sy = (int) (65536.0 * (float) spixelh / (float) (dst->height - 1));
	} else {
		sx = (int) (65536.0 * (float) (src->width) / (float) (dst->width));
		sy = (int) (65536.0 * (float) (src->height) / (float) (dst->height));
	}

	/* Maximum scaled source size */
	ssx = (src->width << 16) - 1;
	ssy = (src->height << 16) - 1;

	/* Precalculate horizontal row increments */
	csx = 0;
	csax = sax;
	for (x = 0; x <= dst->width; x++) {
		*csax = csx;
		csax++;
		csx += sx;

		/* Guard from overflows */
		if (csx > ssx) {
			csx = ssx;
		}
	}

	/* Precalculate vertical row increments */
	csy = 0;
	csay = say;
	for (y = 0; y <= dst->height; y++) {
		*csay = csy;
		csay++;
		csy += sy;

		/* Guard from overflows */
		if (csy > ssy) {
			csy = ssy;
		}
	}

	sp = (RGBcolor *) src->pColorData;
	dp = (RGBcolor *) dst->pColorData;
	dgap = dst->pitch - dst->width;
	spixelgap = src->pitch;

	if (flipx) sp += spixelw;
	if (flipy) sp += (spixelgap * spixelh);

	if (smooth) {
		/*
		* Interpolating Zoom
		*/
		csay = say;
		for (y = 0; y < dst->height; y++) {
			csp = sp;
			csax = sax;
			for (x = 0; x < dst->width; x++) {
				/* Setup color source pointers */
				ex = (*csax & 0xffff);
				ey = (*csay & 0xffff);
				cx = (*csax >> 16);
				cy = (*csay >> 16);
				sstepx = cx < spixelw;
				sstepy = cy < spixelh;
				c00 = sp;
				c01 = sp;
				c10 = sp;
				if (sstepy) {
					if (flipy) {
						c10 -= spixelgap;
					} else {
						c10 += spixelgap;
					}
				}
				c11 = c10;
				if (sstepx) {
					if (flipx) {
						c01--;
						c11--;
					} else {
						c01++;
						c11++;
					}
				}

				/* Draw and interpolate colors */
				t1 = ((((c01->r - c00->r) * ex) >> 16) + c00->r) & 0xff;
				t2 = ((((c11->r - c10->r) * ex) >> 16) + c10->r) & 0xff;
				dp->r = (((t2 - t1) * ey) >> 16) + t1;
				t1 = ((((c01->g - c00->g) * ex) >> 16) + c00->g) & 0xff;
				t2 = ((((c11->g - c10->g) * ex) >> 16) + c10->g) & 0xff;
				dp->g = (((t2 - t1) * ey) >> 16) + t1;
				t1 = ((((c01->b - c00->b) * ex) >> 16) + c00->b) & 0xff;
				t2 = ((((c11->b - c10->b) * ex) >> 16) + c10->b) & 0xff;
				dp->b = (((t2 - t1) * ey) >> 16) + t1;
				t1 = ((((c01->a - c00->a) * ex) >> 16) + c00->a) & 0xff;
				t2 = ((((c11->a - c10->a) * ex) >> 16) + c10->a) & 0xff;
				dp->a = (((t2 - t1) * ey) >> 16) + t1;

				/* Advance source pointer x */
				salast = csax;
				csax++;
				sstep = (*csax >> 16) - (*salast >> 16);
				if (flipx) {
					sp -= sstep;
				} else {
					sp += sstep;
				}

				/* Advance destination pointer x */
				dp++;
			}

			/* Advance source pointer y */
			salast = csay;
			csay++;
			sstep = (*csay >> 16) - (*salast >> 16);
			sstep *= spixelgap;
			if (flipy) {
				sp = csp - sstep;
			} else {
				sp = csp + sstep;
			}

			/* Advance destination pointer y */
			dp += dgap;
		}

	} else {
		/*
		* Non-Interpolating Zoom
		*/
		csay = say;
		for (y = 0; y < dst->height; y++) {
			csp = sp;
			csax = sax;
			for (x = 0; x < dst->width; x++) {
				/* Draw */
				*dp = *sp;

				/* Advance source pointer x */
				salast = csax;
				csax++;
				sstep = (*csax >> 16) - (*salast >> 16);
				if (flipx) sstep = -sstep;
				sp += sstep;

				/* Advance destination pointer x */
				dp++;
			}
			/* Advance source pointer y */
			salast = csay;
			csay++;
			sstep = (*csay >> 16) - (*salast >> 16);
			sstep *= spixelgap;
			if (flipy) sstep = -sstep;
			sp = csp + sstep;

			/* Advance destination pointer y */
			dp += dgap;
		}
	}

	free(sax);
	free(say);
	return 0;
}

/*
Internal 8 bit Zoomer without smoothing.

Zooms 8bit palette/Y 'src' surface to 'dst' surface.
Assumes src and dst surfaces are of 8 bit depth.
Assumes dst surface was allocated with the correct dimensions.

`src`   : The surface to zoom (input).
`dst`   : The zoomed surface (output).
`flipx` : Flag indicating if the image should be horizontally flipped.
`flipy` : Flag indicating if the image should be vertically flipped.

Returns 0 for success or -1 for error.
*/
int _zoomSurfaceY(Surface *src, Surface *dst, boolint flipx, boolint flipy)
{
	int x, y;
	uint32_t *sax, *say, *csax, *csay;
	int csx, csy;
	uint8_t *sp, *dp, *csp;
	int dgap;

	/* Allocate memory for row increments */
	if ((sax = (uint32_t *) malloc((dst->width + 1) * sizeof(uint32_t))) == NULL) {
		return -1;
	}
	if ((say = (uint32_t *) malloc((dst->height + 1) * sizeof(uint32_t))) == NULL) {
		free(sax);
		return -1;
	}

	/* Pointer setup */
	sp = csp = (uint8_t *) src->pPaletteData;
	dp = (uint8_t *) dst->pPaletteData;
	dgap = dst->pitch - dst->width;

	if (flipx) csp += src->width - 1;
	if (flipy) csp += src->pitch * (src->height - 1);

	/* Precalculate row increments */
	csx = 0;
	csax = sax;
	for (x = 0; x < dst->width; x++) {
		csx += src->width;
		*csax = 0;
		while (csx >= dst->width) {
			csx -= dst->width;
			(*csax)++;
		}
		(*csax) = (*csax) * (flipx ? -1 : 1);
		csax++;
	}
	csy = 0;
	csay = say;
	for (y = 0; y < dst->height; y++) {
		csy += src->height;
		*csay = 0;
		while (csy >= dst->height) {
			csy -= dst->height;
			(*csay)++;
		}
		(*csay) = (*csay) * (flipy ? -1 : 1);
		csay++;
	}

	/* Draw */
	csay = say;
	for (y = 0; y < dst->height; y++) {
		csax = sax;
		sp = csp;
		for (x = 0; x < dst->width; x++) {
			/* Draw */
			*dp = *sp;
			/* Advance source pointers */
			sp += (*csax);
			csax++;
			/* Advance destination pointer */
			dp++;
		}
		/* Advance source pointer (for row) */
		csp += *csay * src->pitch;
		csay++;

		/* Advance destination pointers */
		dp += dgap;
	}

	free(sax);
	free(say);
	return 0;
}

/*
Internal 32 bit rotozoomer with optional anti-aliasing.

Rotates and zooms 32 bit RGBA/ABGR 'src' surface to 'dst' surface based on the control
parameters by scanning the destination surface and applying optionally anti-aliasing
by bilinear interpolation.
Assumes src and dst surfaces are of 32 bit depth.
Assumes dst surface was allocated with the correct dimensions.

`src`    : Source surface.
`dst`    : Destination surface.
`cx`     : Horizontal center coordinate.
`cy`     : Vertical center coordinate.
`isin`   : Integer version of sine of angle.
`icos`   : Integer version of cosine of angle.
`flipx`  : Flag indicating horizontal mirroring should be applied.
`flipy`  : Flag indicating vertical mirroring should be applied.
`smooth` : Whether to antialias.
*/
void _transformSurfaceRGBA(Surface *src, Surface *dst, int cx, int cy, int isin, int icos, boolint flipx, boolint flipy, boolint smooth)
{
	int x, y, t1, t2, dx, dy, xd, yd, sdx, sdy, ax, ay, ex, ey, sw, sh;
	RGBcolor c00, c01, c10, c11, cswap;
	RGBcolor *pc, *sp;
	int gap;

	/* Variable setup */
	xd = ((src->width - dst->width) << 15);
	yd = ((src->height - dst->height) << 15);
	ax = (cx << 16) - (icos * cx);
	ay = (cy << 16) - (isin * cx);
	sw = src->width - 1;
	sh = src->height - 1;
	pc = (RGBcolor *)dst->pColorData;
	gap = dst->pitch - dst->width;

	if (smooth) {
		/*
		* Interpolating version
		*/
		for (y = 0; y < dst->height; y++) {
			dy = cy - y;
			sdx = (ax + (isin * dy)) + xd;
			sdy = (ay - (icos * dy)) + yd;
			for (x = 0; x < dst->width; x++) {
				dx = (sdx >> 16);
				dy = (sdy >> 16);
				if (flipx) dx = sw - dx;
				if (flipy) dy = sh - dy;
				if ((dx > -1) && (dy > -1) && (dx < (src->width-1)) && (dy < (src->height-1))) {
					sp = (RGBcolor *)src->pColorData + src->pitch * dy + dx;
					c00 = *sp;
					sp += 1;
					c01 = *sp;
					sp += src->pitch;
					c11 = *sp;
					sp -= 1;
					c10 = *sp;
					if (flipx) {
						cswap = c00; c00=c01; c01=cswap;
						cswap = c10; c10=c11; c11=cswap;
					}
					if (flipy) {
						cswap = c00; c00=c10; c10=cswap;
						cswap = c01; c01=c11; c11=cswap;
					}

					/* Interpolate colors */
					ex = (sdx & 0xffff);
					ey = (sdy & 0xffff);
					t1 = ((((c01.r - c00.r) * ex) >> 16) + c00.r) & 0xff;
					t2 = ((((c11.r - c10.r) * ex) >> 16) + c10.r) & 0xff;
					pc->r = (((t2 - t1) * ey) >> 16) + t1;
					t1 = ((((c01.g - c00.g) * ex) >> 16) + c00.g) & 0xff;
					t2 = ((((c11.g - c10.g) * ex) >> 16) + c10.g) & 0xff;
					pc->g = (((t2 - t1) * ey) >> 16) + t1;
					t1 = ((((c01.b - c00.b) * ex) >> 16) + c00.b) & 0xff;
					t2 = ((((c11.b - c10.b) * ex) >> 16) + c10.b) & 0xff;
					pc->b = (((t2 - t1) * ey) >> 16) + t1;
					t1 = ((((c01.a - c00.a) * ex) >> 16) + c00.a) & 0xff;
					t2 = ((((c11.a - c10.a) * ex) >> 16) + c10.a) & 0xff;
					pc->a = (((t2 - t1) * ey) >> 16) + t1;
				}
				sdx += icos;
				sdy += isin;
				pc++;
			}
			pc += gap;
		}

	} else {
		/*
		* Non-interpolating version
		*/
		for (y = 0; y < dst->height; y++) {
			dy = cy - y;
			sdx = (ax + (isin * dy)) + xd;
			sdy = (ay - (icos * dy)) + yd;
			for (x = 0; x < dst->width; x++) {
				dx = (short) (sdx >> 16);
				dy = (short) (sdy >> 16);
				if (flipx) dx = (src->width-1)-dx;
				if (flipy) dy = (src->height-1)-dy;
				if ((dx >= 0) && (dy >= 0) && (dx < src->width) && (dy < src->height)) {
					sp = (RGBcolor *)src->pColorData + src->pitch * dy + dx;
					*pc = *sp;
				}
				sdx += icos;
				sdy += isin;
				pc++;
			}
			pc += gap;
		}
	}
}


/*
Rotates and zooms 8 bit palette/Y 'src' surface to 'dst' surface without smoothing.

Rotates and zooms 8 bit RGBA/ABGR 'src' surface to 'dst' surface based on the control
parameters by scanning the destination surface.
Assumes src and dst surfaces are of 8 bit depth.
Assumes dst surface was allocated with the correct dimensions.

`src`   : Source surface.
`dst`   : Destination surface.
`cx`    : Horizontal center coordinate.
`cy`    : Vertical center coordinate.
`isin`  : Integer version of sine of angle.
`icos`  : Integer version of cosine of angle.
`flipx` : Flag indicating horizontal mirroring should be applied.
`flipy` : Flag indicating vertical mirroring should be applied.
*/
void _transformSurfaceY(Surface *src, Surface *dst, int cx, int cy, int isin, int icos, boolint flipx, boolint flipy)
{
	int x, y, dx, dy, xd, yd, sdx, sdy, ax, ay;
	uint8_t *pc, *sp;
	int gap;

	/* Variable setup */
	xd = ((src->width - dst->width) << 15);
	yd = ((src->height - dst->height) << 15);
	ax = (cx << 16) - (icos * cx);
	ay = (cy << 16) - (isin * cx);
	pc = dst->pPaletteData;
	gap = dst->pitch - dst->width;
	/* Clear surface to colorkey (zero) */
	memset(pc, 0, dst->pitch * dst->height);

	/* Iterate through destination surface */
	for (y = 0; y < dst->height; y++) {
		dy = cy - y;
		sdx = (ax + (isin * dy)) + xd;
		sdy = (ay - (icos * dy)) + yd;
		for (x = 0; x < dst->width; x++) {
			dx = (short) (sdx >> 16);
			dy = (short) (sdy >> 16);
			if (flipx) dx = (src->width-1)-dx;
			if (flipy) dy = (src->height-1)-dy;
			if ((dx >= 0) && (dy >= 0) && (dx < src->width) && (dy < src->height)) {
				sp = src->pPaletteData + src->pitch * dy + dx;
				*pc = *sp;
			}
			sdx += icos;
			sdy += isin;
			pc++;
		}
		pc += gap;
	}
}

/*
Rotates a 8/16/24/32 bit surface in increments of 90 degrees.

Specialized 90 degree rotator which rotates a 'src' surface in 90 degree
increments clockwise returning a new surface. Faster than rotozoomer since
no scanning or interpolation takes place. Input surface must be 8/16/24/32 bit.
(code contributed by J. Schiller, improved by C. Allport and A. Schiffler)

`src`               : Source surface to rotate.
`numClockwiseTurns` : Number of clockwise 90 degree turns to apply to the source.

Returns the new, rotated surface; or NULL for surfaces with incorrect input format.
*/
Surface* rotateSurface90Degrees(Surface* src, int numClockwiseTurns)
{
	int row, col, newWidth, newHeight;
	Surface* dst;
	uint8_t* srcBuf;
	uint8_t* dstBuf;
	int normalizedClockwiseTurns;

	if (!src) {
		debug(errShowBug, "rotateSurface90Degrees: NULL source surface");
		return NULL;
	}

	/* normalize numClockwiseTurns */
	normalizedClockwiseTurns = (numClockwiseTurns % 4);
	if (normalizedClockwiseTurns < 0) {
		normalizedClockwiseTurns += 4;
	}

	/* If turns are even, our new width/height will be the same as the source surface */
	if (normalizedClockwiseTurns % 2) {
		newWidth = src->height;
		newHeight = src->width;
	} else {
		newWidth = src->width;
		newHeight = src->height;
	}

	if (gfx_surfaceCreate(newWidth, newHeight, src->format, SU_Staging, &dst))
		return NULL;

	int bpp = src->format == SF_32bit ? 4 : 1;  /* bytes per pixel */

	switch(normalizedClockwiseTurns) {
	case 0: /* Make a copy of the surface */
		{
			if (src->pitch == dst->pitch) {
				/* If the pitch is the same for both surfaces, the memory can be copied all at once. */
				memcpy(dst->pRawData, src->pRawData, (src->height * src->pitch) * bpp);
			}
			else
			{
				/* If the pitch differs, copy each row separately */
				srcBuf = (uint8_t*)(src->pRawData);
				dstBuf = (uint8_t*)(dst->pRawData);
				int bpr = src->width * bpp;
				for (row = 0; row < src->height; row++) {
					memcpy(dstBuf, srcBuf, bpr);
					srcBuf += src->pitch * bpp;
					dstBuf += dst->pitch * bpp;
				}
			}
		}
		break;

		/* rotate clockwise */
	case 1: /* rotated 90 degrees clockwise */
		{
			for (row = 0; row < src->height; ++row) {
				srcBuf = (uint8_t*)(src->pRawData) + (row * src->pitch) * bpp;
				dstBuf = (uint8_t*)(dst->pRawData) + (dst->width - row - 1) * bpp;
				for (col = 0; col < src->width; ++col) {
					memcpy (dstBuf, srcBuf, bpp);
					srcBuf += bpp;
					dstBuf += dst->pitch * bpp;
				}
			}
		}
		break;

	case 2: /* rotated 180 degrees clockwise */
		{
			for (row = 0; row < src->height; ++row) {
				srcBuf = (uint8_t*)(src->pRawData) + (row * src->pitch * bpp);
				dstBuf = (uint8_t*)(dst->pRawData) + ((dst->height - row - 1) * dst->pitch + dst->width - 1) * bpp;
				for (col = 0; col < src->width; ++col) {
					memcpy (dstBuf, srcBuf, bpp);
					srcBuf += bpp;
					dstBuf -= bpp;
				}
			}
		}
		break;

	case 3: /* rotated 270 degrees clockwise */
		{
			for (row = 0; row < src->height; ++row) {
				srcBuf = (uint8_t*)(src->pRawData) + (row * src->pitch * bpp);
				dstBuf = (uint8_t*)(dst->pRawData) + (row * bpp) + ((dst->height - 1) * dst->pitch) * bpp;
				for (col = 0; col < src->width; ++col) {
					memcpy (dstBuf, srcBuf, bpp);
					srcBuf += bpp;
					dstBuf -= dst->pitch * bpp;
				}
			}
		}
		break;
	}
	/* end switch */

	return dst;
}

/* Correct x/y zoom to positive non-zero values. (Negative zoom means the image is flipped).

`zoomx`     : Pointer to the horizontal zoom factor.
`zoomy`     : Pointer to the vertical zoom factor.
*/
void _normaliseZoom(double *zoomx, double *zoomy)
{
	*zoomx = fabs(*zoomx);
	*zoomy = fabs(*zoomy);
	if (*zoomx < VALUE_LIMIT) *zoomx = VALUE_LIMIT;
	if (*zoomy < VALUE_LIMIT) *zoomy = VALUE_LIMIT;
}

/*
Internal target surface sizing function for rotozooms with trig result return.

`width`      : The source surface width.
`height`     : The source surface height.
`angle`      : The angle to rotate in degrees.
`zoomx`      : The horizontal scaling factor.
`zoomy`      : The vertical scaling factor.
`dstwidth`   : The calculated width of the destination surface.
`dstheight`  : The calculated height of the destination surface.
`canglezoom` : The sine of the angle adjusted by the zoom factor.
`sanglezoom` : The cosine of the angle adjusted by the zoom factor.
*/
void _rotozoomSurfaceSizeTrig(int width, int height, double angle, double zoomx, double zoomy,
	int *dstwidth, int *dstheight,
	double *canglezoom, double *sanglezoom)
{
	_normaliseZoom(&zoomx, &zoomy);

	if (fabs(angle) <= VALUE_LIMIT) {
		/* No rotation */
		*dstwidth = (int) floor(((double) width * zoomx) + 0.5);
		*dstwidth = MAX(*dstwidth, 1);
		*dstheight = (int) floor(((double) height * zoomy) + 0.5);
		*dstheight = MAX(*dstheight, 1);
		return;
	}

	double x, y, cx, cy, sx, sy;
	double radangle;
	int dstwidthhalf, dstheighthalf;

	/* Determine destination width and height by rotating a centered source box */
	radangle = angle * (M_PI / 180.0);
	*sanglezoom = sin(radangle);
	*canglezoom = cos(radangle);
	*sanglezoom *= zoomx;
	*canglezoom *= zoomy;
	x = (double)(width / 2);
	y = (double)(height / 2);
	cx = *canglezoom * x;
	cy = *canglezoom * y;
	sx = *sanglezoom * x;
	sy = *sanglezoom * y;

	dstwidthhalf = MAX((int)
		ceil(MAX(MAX(MAX(fabs(cx + sy), fabs(cx - sy)), fabs(-cx + sy)), fabs(-cx - sy))), 1);
	dstheighthalf = MAX((int)
		ceil(MAX(MAX(MAX(fabs(sx + cy), fabs(sx - cy)), fabs(-sx + cy)), fabs(-sx - cy))), 1);
	*dstwidth = 2 * dstwidthhalf;
	*dstheight = 2 * dstheighthalf;
}


/*
Returns the size of the resulting target surface for a rotozoomSurface() call.
The minimum size of the target surface is 1. The input factors can be positive or negative.

`width`     : The source surface width.
`height`    : The source surface height.
`angle`     : The angle to rotate in degrees.
`zoomx`     : The horizontal scaling factor.
`zoomy`     : The vertical scaling factor.
`dstwidth`  : The calculated width of the rotozoomed destination surface.
`dstheight` : The calculated height of the rotozoomed destination surface.
*/
void rotozoomSurfaceSize(int width, int height, double angle, double zoomx, double zoomy, int *dstwidth, int *dstheight)
{
	double dummy_sanglezoom, dummy_canglezoom;

	_rotozoomSurfaceSizeTrig(width, height, angle, zoomx, zoomy, dstwidth, dstheight, &dummy_sanglezoom, &dummy_canglezoom);
}

/*
Rotates and zooms a surface with different horizontal and vertival scaling factors and optional anti-aliasing.

Rotates and zooms a 32bit or 8bit 'src' surface to newly created 'dst' surface.
'angle' is the rotation in degrees, 'zoomx and 'zoomy' scaling factors. If 'smooth' is set
then the destination 32bit surface is anti-aliased. If the surface is not 8bit
or 32bit RGBA/ABGR it will be converted into a 32bit RGBA format on the fly.

`src`    : The surface to rotozoom.
`angle`  : The angle to rotate in degrees.
`zoomx`  : The horizontal scaling factor.
`zoomy`  : The vertical scaling factor.
`smooth` : Whether to antialias.

Returns the new rotozoomed surface.
*/
Surface *rotozoomSurface(Surface *src, double angle, double zoomx, double zoomy, boolint smooth)
{
	Surface *rz_dst = NULL;
	double zoominv;
	double sanglezoom, canglezoom, sanglezoominv, canglezoominv;
	int dstwidthhalf, dstwidth, dstheighthalf, dstheight;
	boolint flipx = (zoomx < 0.), flipy = (zoomy < 0.);
	_normaliseZoom(&zoomx, &zoomy);

	if (!src)
		return NULL;

	/* Determine target size */
	_rotozoomSurfaceSizeTrig(src->width, src->height, angle, zoomx, zoomy, &dstwidth, &dstheight, &canglezoom, &sanglezoom);

	/* Alloc space to completely contain the zoomed/rotated surface */
	if (gfx_surfaceCreate(dstwidth, dstheight + GUARD_ROWS, src->format, SU_Staging, &rz_dst))
		return NULL;

	/* Adjust for guard rows */
	rz_dst->height = dstheight;

	/* Check if we have a rotozoom or just a zoom */
	if (fabs(angle) > VALUE_LIMIT) {
		/* angle != 0: Full rotozoom */

		/* Calculate target factors from sin/cos and zoom */
		zoominv = 65536.0 / (zoomx * zoomx);
		sanglezoominv = sanglezoom;
		canglezoominv = canglezoom;
		sanglezoominv *= zoominv;
		canglezoominv *= zoominv;

		dstwidthhalf = dstwidth / 2;
		dstheighthalf = dstheight / 2;

		if (src->format == SF_32bit) {
			_transformSurfaceRGBA(src, rz_dst, dstwidthhalf, dstheighthalf,
				(int)sanglezoominv, (int)canglezoominv, flipx, flipy, smooth);
		} else {
			_transformSurfaceY(src, rz_dst, dstwidthhalf, dstheighthalf,
				(int)sanglezoominv, (int)canglezoominv, flipx, flipy);
		}

	} else {
		/* angle == 0: Just a zoom */
		if (src->format == SF_32bit) {
			_zoomSurfaceRGBA(src, rz_dst, flipx, flipy, smooth);
		} else {
			_zoomSurfaceY(src, rz_dst, flipx, flipy);
		}
	}

	return rz_dst;
}
