/* OHRRPGCE - 8-bit graphics blitting
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

#include "config.h"
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "allmodex.h"
#include "surface.h"
#include "errorlog.h"
#include "blend.h"

void smoothzoomblit_8_to_8bit(uint8_t *srcbuffer, uint8_t *destbuffer, XYPair size, int pitch, int zoom, int smooth, RGBcolor dummypal[]);
void smoothzoomblit_8_to_32bit(uint8_t *srcbuffer, RGBcolor *destbuffer, XYPair size, int pitch, int zoom, int smooth, RGBcolor pal[]);
void smoothzoomblit_32_to_32bit(RGBcolor *srcbuffer, RGBcolor *destbuffer, XYPair size, int pitch, int zoom, int smooth, RGBcolor dummypal[]);

//// Globals
enum BlendAlgo blend_algo = blendAlgoDither;
// This cache is wiped as needed in intpal_changed()
uint8_t nearcolor_cache[65536] = {0};


// Memoize nearcolor_fast, dropping 3 least-significant bits red & blue, 2 from green.
int nearcolor_faster(RGBcolor searchcol) {
	int idx = ((searchcol.r >> 3) << 11) | ((searchcol.g >> 2) << 5) | (searchcol.b >> 3);
	int res = nearcolor_cache[idx];
	if (!res) {
		searchcol.r = (searchcol.r & 0xf8) + 3;
		searchcol.g = (searchcol.g & 0xfc) + 1;
		searchcol.b = (searchcol.b & 0xf8) + 3;
		res = nearcolor_fast(searchcol);  // Never returns 0
		nearcolor_cache[idx] = res;
	}
	return res;
}

static inline int get_frame_buf(Frame *spr, uint8_t *restrict *srcpp, uint8_t *restrict *maskpp) {
	if (spr->surf) {
		if (spr->surf->format != SF_8bit) {
			debug(errShowBug, "blitohr[scaled]: 32bit sprite!");
			return 0;
		}
		*srcpp = spr->surf->pPaletteData;
		*maskpp = spr->surf->pMaskData;
	} else {
		*srcpp = spr->image;
		*maskpp = spr->mask;
	}
	return 1;
}

// 8-bit -> 8-bit scale=1 blitting routine. Supports 8-bit Surface-backed Frames too.
// The arguments must already be clipped to the destination (done in draw_clipped())
// opts should be a ptr to def_drawoptions if no extra options are needed.
void blitohr(Frame *spr, Frame *destspr, Palette16 *pal, int startoffset, int startx, int starty, int endx, int endy, boolint trans, DrawOptions *opts) {
	int i, j;
	unsigned char *maskp, *srcp, *original_maskp, *restrict destp, *restrict destmaskp;
	int srclineinc, destlineinc;

	if (!opts) {
		debug(errShowBug, "blitohr: opts not optional!");
		return;
	}

	if (!get_frame_buf(spr, &srcp, &maskp))
		return;
	if (maskp == NULL) {
		//we could add an optimised version for this case, which is the 99% case
		maskp = srcp;
	}

	srcp += startoffset;
	maskp += startoffset;
	original_maskp = maskp;

	srclineinc = spr->pitch - (endx - startx + 1);

	if (!get_frame_buf(destspr, &destp, &destmaskp))
		return;
	destp += startx + starty * destspr->pitch;
	if (destmaskp)
		destmaskp += startx + starty * destspr->pitch;

	destlineinc = destspr->pitch - (endx - startx + 1);

	int tog = 0;

	if (opts->with_blending && (opts->opacity < 1. || opts->blend_mode != blendModeNormal)) {
		int alpha = 256 * opts->opacity;
		if (alpha <= 0)
			goto no_draw;

		for (i = starty; i <= endy; i++) {
			int errr = 0, errg = 0, errb = 0;
			for (j = endx - startx; j >= 0; j--) {
				tog ^= 1;
				if (trans && *maskp == 0) {
					destp++;
					maskp++;
					srcp++;
					continue;
				}

				RGBcolor srcc, destc = curmasterpal[*destp];
				if (pal)
					srcc = curmasterpal[pal->col[*srcp]];
				else
					srcc = curmasterpal[*srcp];

				// Blend source and dest pixels in 24-bit colour space (.a ignored for now)
				RGBcolor blended = alpha_blend(srcc, destc, alpha, opts->blend_mode, false);

				// Convert back to master palette, possibly performing error diffusion.
				int res;
				/* if (blend_algo == -1) {
					res = *destp;
				} else */
				if (blend_algo == blendAlgoNoDither) {
					// Use faster lookup, because you don't care about color accuracy
					// anyway if you're using this.
					res = nearcolor_faster(blended);
				} else {
					int r = blended.r + errr;
					int g = blended.g + errg;
					int b = blended.b + errb;
					if (r > 255) r = 255;
					else if (r < 0) r = 0;
					if (g > 255) g = 255;
					else if (g < 0) g = 0;
					if (b > 255) b = 255;
					else if (b < 0) b = 0;

					RGBcolor searchcol = {{b, g, r, 0}};
					res = nearcolor_faster(searchcol);

					// blendAlgoDither is a chequered dither where we wipe the
					// error every 2nd pixel, propagate just 1/2 the error from 1/4
					// of the pixelsand propagate 3/4 from the other 1/4.
					// This creates quarter-dither patterns so that as opacity
					// is faded there are smaller, smoother steps. The low amount
					// of error propagation reduces grain and artifacts at the cost
					// of color accuracy, which is unimportant.
					if (tog) {  // (i^j)&1
						errr = errg = errb = 0;
					} else {
						int m;
						if (blend_algo == blendAlgoDither)
							m = 2 + ((i&j)&1);  // 2 or 3
						else // blendAlgoLessDither
							m = 1;
						errr = m * (r - curmasterpal[res].r) / 4;
						errg = m * (g - curmasterpal[res].g) / 4;
						errb = m * (b - curmasterpal[res].b) / 4;
					}
				}

				destp[0] = res;
				destp++;
				maskp++;
				srcp++;
			}
			destp += destlineinc;
			maskp += srclineinc;
			srcp += srclineinc;
			tog ^= srclineinc & 1;
			tog ^= 1;
		}
	} else
	if (pal != NULL && trans != 0) {
		for (i = starty; i <= endy; i++) {
			//loop unrolling copied from below, but not nearly as effective
			for (j = endx - startx; j >= 3; j -= 4) {
				if (maskp[0]) destp[0] = pal->col[srcp[0]];
				if (maskp[1]) destp[1] = pal->col[srcp[1]];
				if (maskp[2]) destp[2] = pal->col[srcp[2]];
				if (maskp[3]) destp[3] = pal->col[srcp[3]];
				maskp += 4;
				srcp += 4;
				destp += 4;
			}
			for (; j >= 0; j--) {
				if (maskp++[0]) destp[0] = pal->col[srcp[0]];
				destp++;
				srcp++;
			}

			destp += destlineinc;
			maskp += srclineinc;
			srcp += srclineinc;
		}
	} else if (pal != NULL && trans == 0) {
		for (i = starty; i <= endy; i++) {
			//loop unrolling blindly copied from below
			for (j = endx - startx; j >= 3; j -= 4) {
				destp[0] = pal->col[srcp[0]];
				destp[1] = pal->col[srcp[1]];
				destp[2] = pal->col[srcp[2]];
				destp[3] = pal->col[srcp[3]];
				srcp += 4;
				destp += 4;
			}
			for (; j >= 0; j--)
				destp++[0] = pal->col[srcp++[0]];

			destp += destlineinc;
			srcp += srclineinc;
		}
	} else if (trans == 0) { //&& pal == NULL
		for (i = starty; i <= endy; i++) {
			memcpy(destp, srcp, endx - startx + 1);
			srcp += spr->pitch;
			destp += destspr->pitch;
		}
	} else { //pal == NULL && trans != 0
		for (i = starty; i <= endy; i++) {
			//a little loop unrolling
			for (j = endx - startx; j >= 3; j -= 4) {
				//the following line is surprisingly slow
				//*(int*)destp = (*(int*)srcp & *(int*)maskp) | (*(int*)destp & ~*(int*)maskp)
				if (maskp[0]) destp[0] = srcp[0];
				if (maskp[1]) destp[1] = srcp[1];
				if (maskp[2]) destp[2] = srcp[2];
				if (maskp[3]) destp[3] = srcp[3];
				maskp += 4;
				srcp += 4;
				destp += 4;
			}
			for (; j >= 0; j--) {
				if (*maskp++) *destp = *srcp;
				srcp++;
				destp++;
			}

			destp += destlineinc;
			maskp += srclineinc;
			srcp += srclineinc;
		}
	}
  no_draw:

	// Set the destination mask
	if (opts->write_mask && destmaskp) {
		srcp = original_maskp;
		destp = destmaskp;
		for (i = starty; i <= endy; i++) {
			memcpy(destp, srcp, endx - startx + 1);
			srcp += spr->pitch;
			destp += destspr->pitch;
		}
	}
}

// 8 bit scaled blitting routine. Supports 8-bit Surface-backed Frames too.
// The arguments must already be clipped to the destination (done in draw_clipped_scaled())
//horribly slow; keep putting off doing something about it
//(This function will be replaced with rotozoomSurface(), which has a fast path for scaling)
// opts should be a ptr to def_drawoptions if no extra options are needed.
void blitohrscaled(Frame *spr, Frame *destspr, Palette16 *pal, int x, int y, int startx, int starty, int endx, int endy, boolint trans, DrawOptions *opts) {
	unsigned char *restrict destbuf, *restrict destmaskp;
	unsigned char *restrict maskp;
	unsigned char *restrict srcp;
	int tx, ty;
	int pix, spix;

	if (!opts) {
		debug(errShowBug, "blitohrscaled: opts not optional!");
		return;
	}

	if (!get_frame_buf(spr, &srcp, &maskp)) return;
	if (!get_frame_buf(destspr, &destbuf, &destmaskp)) return;

	int scale = opts->scale;

	bool write_mask = opts->write_mask;
	if (maskp == 0) {
		maskp = srcp;
	}
	if (destmaskp == 0) {
		write_mask = false;
	}

	if (trans == 0) {
		for (ty = starty; ty <= endy; ty++) {
			//tx = startx
			for (tx = startx; tx <= endx; tx++) {
				//figure out where to put the pixel
				pix = (ty * destspr->pitch) + tx;
				//and where to get the pixel from
				spix = (((ty - y) / scale) * spr->pitch) + ((tx - x) / scale);

				if (pal != 0)
					destbuf[pix] = pal->col[srcp[spix]];
				else
					destbuf[pix] = srcp[spix];
				if (write_mask)
					destmaskp[pix] = maskp[spix];
			}
		}
	} else {
		for (ty = starty; ty <= endy; ty++) {
			//tx = startx
			for (tx = startx; tx <= endx; tx++) {
				//figure out where to put the pixel
				pix = (ty * destspr->pitch) + tx;
				//and where to get the pixel from
				spix = (((ty - y) / scale) * spr->pitch) + ((tx - x) / scale);

				//check mask
				if (maskp[spix]) {
					if (pal != 0)
						destbuf[pix] = pal->col[srcp[spix]];
					else
						destbuf[pix] = srcp[spix];
				}
				if (write_mask)
					destmaskp[pix] = maskp[spix];
			}
		}
	}
}

typedef void (*smoothblitfunc_t)(void *srcbuffer, void *destbuffer, XYPair size, int pitch, int zoom, int smooth, RGBcolor pal[]);

// The smoothzoomblit functions implement smoothing at 2x, 3x and 4x zooms.
// This implements smoothing at other zooms by chaining together calls to those functions
bool multismoothblit(int srcbitdepth, int destbitdepth, void *srcbuffer, void *destbuffer, XYPair size, int pitch, int zoom, int *smooth, RGBcolor pal[]) {
	if (zoom < 4 || !*smooth)
		return false;

	smoothblitfunc_t func1, func2;
	if (srcbitdepth == 32) {
		assert(destbitdepth == 32);
		// Slow: have to use 32 bit whole way
		func1 = func2 = (smoothblitfunc_t)smoothzoomblit_32_to_32bit;
	} else {
		assert(srcbitdepth == 8);
		assert(destbitdepth == 8 || destbitdepth == 32);
		// Use 8 bit for intermediate steps
		func1 = (smoothblitfunc_t)smoothzoomblit_8_to_8bit;
		if (destbitdepth == 8)
			func2 = (smoothblitfunc_t)smoothzoomblit_8_to_8bit;
		else
			func2 = (smoothblitfunc_t)smoothzoomblit_8_to_32bit;
	}

	// Scale in 2 or 3 steps
	int zoom0 = 1, zoom1 = 0, zoom2 = 0;
	int finalsmooth = 1;
	if (zoom == 4) { zoom1 = 2; zoom2 = 2; }
	else if (zoom == 6) { zoom1 = 3; zoom2 = 2; }
	else if (zoom == 8) { zoom0 = 2, zoom1 = 2; zoom2 = 2; }
	else if (zoom == 9) { zoom1 = 3; zoom2 = 3; }
	else if (zoom == 12) { zoom0 = 2; zoom1 = 3; zoom2 = 2; }
	else if (zoom == 16) { zoom0 = 2; zoom1 = 2; zoom2 = 4; finalsmooth = 0; }
	else {
		// Still attempt to smooth zooms like 5x, 7x, but the effect is very slight
		return false;
	}
	int zoom01 = zoom0 * zoom1;

	void *intermediate_buffer;
	intermediate_buffer = malloc(size.w * size.h * zoom01 * zoom01 * srcbitdepth / 8);
	if (!intermediate_buffer)
		debugc(errFatalError, "multismoothblit: malloc failed");
	void *first_buffer = srcbuffer;

	if (zoom0 > 1) {
		first_buffer = destbuffer;
		func1(srcbuffer, first_buffer, size, size.w * zoom0, zoom0, 1, pal);
	}
	func1(first_buffer, intermediate_buffer, (XYPair){size.w * zoom0, size.h * zoom0}, size.w * zoom01, zoom1, 1, pal);
	func2(intermediate_buffer, destbuffer, (XYPair){size.w * zoom01, size.h * zoom01}, pitch, zoom2, finalsmooth, pal);
	free(intermediate_buffer);
	return true;
}

void smoothzoomblit_8_to_8bit(uint8_t *srcbuffer, uint8_t *destbuffer, XYPair size, int pitch, int zoom, int smooth, RGBcolor dummypal[]) {
//srcbuffer: source w x h buffer paletted 8 bit
//destbuffer: destination scaled buffer pitch x h*zoom also 8 bit
//supports zoom 1 to 16

	if (multismoothblit(8, 8, srcbuffer, destbuffer, size, pitch, zoom, &smooth, dummypal))
		return;

	uint8_t *sptr;
	int i, j;
	int wide = size.w * zoom, high = size.h * zoom;

	sptr = destbuffer;

	if (zoom == 1) {
		for (i = 0; i <= size.h - 1; i++) {
			memcpy(sptr, srcbuffer, size.w);
			srcbuffer += size.w;
			sptr += pitch;
		}
	} else {
		for (j = 0; j <= size.h - 1; j++) {
			// Write up to 4 copies of a pixel at a time.
			// Skip last 4 pixels so that we can never write off the end of the image buffer.
			for (i = size.w; i >= 4; i--) {
				uint32_t temp = *srcbuffer++;
				temp *= 0x1010101;
				//temp |= temp << 16;
				//temp |= temp << 8;
				((uint32_t *)sptr)[0] = temp;
				if (zoom > 4) {
					((uint32_t *)sptr)[1] = temp;
					if (zoom > 8) {
						((uint32_t *)sptr)[2] = temp;
						if (zoom > 12)
							((uint32_t *)sptr)[3] = temp;
					}
				}
				sptr += zoom;
			}
			while (i-- > 0) {
				uint8_t temp = *srcbuffer++;
				for (int ii = zoom; ii--; )
					*sptr++ = temp;
			}
			sptr += pitch - wide;
			//repeat row zoom times
			for (i = 2; i <= zoom; i++) {
				memcpy(sptr, sptr - pitch, wide);
				sptr += pitch;
			}
		}
	}

	if (smooth == 1 && zoom >= 2) {
		int fy = 1;
		int pstep;
		if (zoom == 3) {
			pstep = 1;
		} else {
			pstep = zoom;
			fy = zoom - 1;
		}
		uint8_t *sptr1, *sptr2, *sptr3;
		for (; fy <= high - 2; fy += pstep) {
			sptr1 = destbuffer + pitch * (fy - 1) + 1;  //(1,0)
			sptr2 = sptr1 + pitch; //(1,1)
			sptr3 = sptr2 + pitch; //(1,2)
			for (int fx = wide - 2; fx >= 1; fx--) {
				//p0=point(fx,fy)
				//p1=point(fx-1,fy-1)//nw
				//p2=point(fx+1,fy-1)//ne
				//p3=point(fx+1,fy+1)//se
				//p4=point(fx-1,fy+1)//sw
				//if p1 = p3 then p0 = p1
				//if p2 = p4 then p0 = p2
				if (sptr1[1] == sptr3[-1])
					sptr2[0] = sptr1[1];
				else
					if (sptr1[-1] == sptr3[1])
						sptr2[0] = sptr1[-1];
				
				sptr1 += 1;
				sptr2 += 1;
				sptr3 += 1;
			}
		}
	}
}

void smoothzoomblit_8_to_32bit(uint8_t *srcbuffer, RGBcolor *destbuffer, XYPair size, int pitch, int zoom, int smooth, RGBcolor pal[]) {
//srcbuffer: source w x h buffer paletted 8 bit
//destbuffer: destination scaled buffer pitch x h*zoom 32 bit (so pitch is in pixels, not bytes)
//supports any positive zoom

	if (multismoothblit(8, 32, srcbuffer, destbuffer, size, pitch, zoom, &smooth, pal))
		return;

	uint32_t *sptr;
	uint32_t pixel;
	int i, j;
	int wide = size.w * zoom, high = size.h * zoom;

	sptr = (uint32_t *)destbuffer;

	for (j = 0; j <= size.h - 1; j++) {
		for (i = 0; i <= size.w - 1; i++) {
			//get colour
			pixel = pal[*srcbuffer].col;
			//zoom sptrs for each srcbuffer
			for (int k = zoom; k >= 1; k--) {
				*sptr = pixel;
				sptr += 1;
			}
			srcbuffer += 1;
		}
		sptr += pitch - wide;
		//repeat row zoom times
		for (i = 2; i <= zoom; i++) {
			memcpy(sptr, sptr - pitch, 4 * wide);
			sptr += pitch;
		}
	}

	if (smooth == 1 && zoom >= 2) {
		int pstep;
		if (zoom == 2)
			pstep = 2;
		else
			pstep = 1;
		uint32_t *sptr1, *sptr2, *sptr3;
		for (int fy = 1; fy <= (high - 2); fy += pstep) {
			sptr1 = (uint32_t *)destbuffer + pitch * (fy - 1) + 1;  //(1,0)
			sptr2 = sptr1 + pitch; //(1,1)
			sptr3 = sptr2 + pitch; //(1,2)
			for (int fx = wide - 2; fx >= 1; fx--) {
				//p0=point(fx,fy)
				//p1=point(fx-1,fy-1)//nw
				//p2=point(fx+1,fy-1)//ne
				//p3=point(fx+1,fy+1)//se
				//p4=point(fx-1,fy+1)//sw
				//if p1 = p3 then p0 = p1
				//if p2 = p4 then p0 = p2
				if (sptr1[1] == sptr3[-1])
					sptr2[0] = sptr1[1];
				else
					if (sptr1[-1] == sptr3[1])
						sptr2[0] = sptr1[-1];
				
				//pset(fx,fy),p0
				sptr1 += 1;
				sptr2 += 1;
				sptr3 += 1;
			}
		}
	}
}

void smoothzoomblit_32_to_32bit(RGBcolor *srcbuffer, RGBcolor *destbuffer, XYPair size, int pitch, int zoom, int smooth, RGBcolor dummypal[]) {
//srcbuffer: source w*h buffer, 32 bit
//destbuffer: destination scaled buffer (pitch*zoom)*(h*zoom), 32 bit (so pitch is in pixels, not bytes)
//supports any positive zoom

	if (multismoothblit(32, 32, srcbuffer, destbuffer, size, pitch, zoom, &smooth, dummypal))
		return;

	uint32_t *sptr;
	uint32_t pixel;
	int i, j;
	int wide = size.w * zoom, high = size.h * zoom;

	sptr = (uint32_t *)destbuffer;

	for (j = 0; j <= size.h - 1; j++) {
		for (i = 0; i <= size.w - 1; i++) {
			pixel = (*srcbuffer++).col;
			for (int k = zoom; k > 0; k--) {
				*sptr++ = pixel;
			}
		}
		sptr += pitch - wide;
		uint32_t *srcline = sptr - pitch;

		//repeat row zoom times
		for (i = 2; i <= zoom; i++) {
			memcpy(sptr, srcline, 4 * wide);
			sptr += pitch;
		}
	}

	if (smooth == 1 && zoom >= 2) {
		int pstep;
		if (zoom == 2)
			pstep = 2;
		else
			pstep = 1;
		uint32_t *sptr1, *sptr2, *sptr3;
		for (int fy = 1; fy <= (high - 2); fy += pstep) {
			sptr1 = (uint32_t *)destbuffer + pitch * (fy - 1) + 1;  //(1,0)
			sptr2 = sptr1 + pitch; //(1,1)
			sptr3 = sptr2 + pitch; //(1,2)
			for (int fx = wide - 2; fx >= 1; fx--) {
				if (sptr1[1] == sptr3[-1])
					sptr2[0] = sptr1[1];
				else
					if (sptr1[-1] == sptr3[1])
						sptr2[0] = sptr1[-1];
				
				sptr1 += 1;
				sptr2 += 1;
				sptr3 += 1;
			}
		}
	}
}
