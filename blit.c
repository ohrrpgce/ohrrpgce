/*
 * blit.c - Expensive graphics utility functions
 *
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 */


#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include "allmodex.h"
#include "common.h"

void smoothzoomblit_8_to_8bit(uint8_t *srcbuffer, uint8_t *destbuffer, int w, int h, int pitch, int zoom, int smooth);
void smoothzoomblit_8_to_32bit(uint8_t *srcbuffer, RGBcolor *destbuffer, int w, int h, int pitch, int zoom, int smooth, int pal[]);
void smoothzoomblit_32_to_32bit(RGBcolor *srcbuffer, RGBcolor *destbuffer, int w, int h, int pitch, int zoom, int smooth);

// write_mask:
//    If the destination has a mask, sets the mask for the destination rectangle
//    equal to the mask (or color-key) for the source rectangle. Does not OR them.
void blitohr(struct Frame *spr, struct Frame *destspr, struct Palette16 *pal, int startoffset, int startx, int starty, int endx, int endy, boolint trans, boolint write_mask) {
	int i, j;
	unsigned char *maskp, *srcp, *destp;
	int srclineinc, destlineinc;

	srcp = spr->image;

	maskp = spr->mask;
	if (maskp == NULL) {
		//we could add an optimised version for this case, which is the 99% case
		maskp = srcp;
	}

	srcp += startoffset;
	maskp += startoffset;

	srclineinc = spr->pitch - (endx - startx + 1);

	destp = destspr->image + startx + starty * destspr->pitch;
	destlineinc = destspr->pitch - (endx - startx + 1);

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

	// Set the destination mask
	if (write_mask && destspr->mask) {
		srcp = (spr->mask ? spr->mask : spr->image) + startoffset;
		destp = destspr->mask + startx + starty * destspr->pitch;
		for (i = starty; i <= endy; i++) {
			memcpy(destp, srcp, endx - startx + 1);
			srcp += spr->pitch;
			destp += destspr->pitch;
		}
	}
}

//horribly slow; keep putting off doing something about it
// write_mask:
//    If the destination has a mask, sets the mask for the destination rectangle
//    equal to the mask (or color-key) for the source rectangle. Does not OR them.
void blitohrscaled(struct Frame *spr, struct Frame *destspr, struct Palette16 *pal, int x, int y, int startx, int starty, int endx, int endy, boolint trans, boolint write_mask, int scale) {
	unsigned char *restrict destbuf;
	unsigned char *restrict maskbuf;
	int tx, ty;
	int pix, spix;

	destbuf = destspr->image;
	maskbuf = spr->mask;
	if (spr->mask == 0) {
		maskbuf = spr->image;
	}
	if (destspr->mask == 0) {
		write_mask = false;
	}
	
	//ty = starty
	if (trans == 0) {
		for (ty = starty; ty <= endy; ty++) {
			//tx = startx
			for (tx = startx; tx <= endx; tx++) {
				//figure out where to put the pixel
				pix = (ty * destspr->pitch) + tx;
				//and where to get the pixel from
				spix = (((ty - y) / scale) * spr->pitch) + ((tx - x) / scale);
				
				if (pal != 0)
					destbuf[pix] = pal->col[spr->image[spix]];
				else
					destbuf[pix] = spr->image[spix];
				if (write_mask)
					destspr->mask[pix] = maskbuf[spix];
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
				if (maskbuf[spix]) {
					if (pal != 0)
						destbuf[pix] = pal->col[spr->image[spix]];
					else
						destbuf[pix] = spr->image[spix];
				}
				if (write_mask)
					destspr->mask[pix] = maskbuf[spix];
			}
		}
	}
}

typedef void (*smoothblitfunc_t)(void *srcbuffer, void *destbuffer, int w, int h, int pitch, int zoom, int smooth);

// The smoothzoomblit functions implement smoothing at 2x, 3x and 4x zooms.
// This implements smoothing at other zooms by chaining together calls to those functions
bool multismoothblit(int bitdepth1, int bitdepth2, void *srcbuffer, void *destbuffer, int w, int h, int pitch, int zoom, int *smooth) {
	if (zoom < 4 || !*smooth)
		return false;

	smoothblitfunc_t func1, func2;
	if (bitdepth1 == 32) {
		assert(bitdepth2 == 32);
		// Slow: have to use 32 bit whole way
		func1 = func2 = (smoothblitfunc_t)smoothzoomblit_32_to_32bit;
	} else {
		assert(bitdepth1 == 8);
		assert(bitdepth2 == 8 || bitdepth2 == 32);
		// Use 8 bit for intermediate steps
		func1 = (smoothblitfunc_t)smoothzoomblit_8_to_8bit;
		if (bitdepth2 == 8)
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
	intermediate_buffer = malloc(w * h * zoom01 * zoom01 * bitdepth1 / 8);
	if (!intermediate_buffer)
		debugc(errDie, "multismoothblit: malloc failed");
	void *first_buffer = srcbuffer;

	if (zoom0 > 1) {
		first_buffer = destbuffer;
		func1(srcbuffer, first_buffer, w, h, w * zoom0, zoom0, 1);
	}
	func1(first_buffer, intermediate_buffer, w * zoom0, h * zoom0, w * zoom01, zoom1, 1);
	func2(intermediate_buffer, destbuffer, w * zoom01, h * zoom01, pitch, zoom2, finalsmooth);
	return true;
}

void smoothzoomblit_8_to_8bit(uint8_t *srcbuffer, uint8_t *destbuffer, int w, int h, int pitch, int zoom, int smooth) {
//srcbuffer: source w x h buffer paletted 8 bit
//destbuffer: destination scaled buffer pitch x h*zoom also 8 bit
//supports zoom 1 to 16

	if (multismoothblit(8, 8, srcbuffer, destbuffer, w, h, pitch, zoom, &smooth))
		return;

	uint8_t *sptr;
	int i, j;
	int wide = w * zoom, high = h * zoom;

	sptr = destbuffer;

	if (zoom == 1) {
		for (i = 0; i <= h - 1; i++) {
			memcpy(sptr, srcbuffer, w);
			srcbuffer += w;
			sptr += pitch;
		}
	} else {
		for (j = 0; j <= h - 1; j++) {
			// Write up to 4 copies of a pixel at a time.
			// Skip last 4 pixels so that we can never write off the end of the image buffer.
			for (i = w; i >= 4; i--) {
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

void smoothzoomblit_8_to_32bit(uint8_t *srcbuffer, RGBcolor *destbuffer, int w, int h, int pitch, int zoom, int smooth, int pal[]) {
//srcbuffer: source w x h buffer paletted 8 bit
//destbuffer: destination scaled buffer pitch x h*zoom 32 bit (so pitch is in pixels, not bytes)
//supports any positive zoom

	if (multismoothblit(8, 32, srcbuffer, destbuffer, w, h, pitch, zoom, &smooth))
		return;

	uint32_t *sptr;
	uint32_t pixel;
	int i, j;
	int wide = w * zoom, high = h * zoom;

	sptr = destbuffer;

	for (j = 0; j <= h - 1; j++) {
		for (i = 0; i <= w - 1; i++) {
			//get colour
			pixel = pal[*srcbuffer];
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
				
				//pset(fx,fy),p0
				sptr1 += 1;
				sptr2 += 1;
				sptr3 += 1;
			}
		}
	}
}

void smoothzoomblit_32_to_32bit(RGBcolor *srcbuffer, RGBcolor *destbuffer, int w, int h, int pitch, int zoom, int smooth) {
//srcbuffer: source w*h buffer, 32 bit
//destbuffer: destination scaled buffer (pitch*zoom)*(h*zoom), 32 bit (so pitch is in pixels, not bytes)
//supports any positive zoom

	if (multismoothblit(32, 32, srcbuffer, destbuffer, w, h, pitch, zoom, &smooth))
		return;

	uint32_t *sptr;
	uint32_t pixel;
	int i, j;
	int wide = w * zoom, high = h * zoom;

	sptr = (uint32_t*)destbuffer;

	for (j = 0; j <= h - 1; j++) {
		for (i = 0; i <= w - 1; i++) {
			pixel = *srcbuffer++;
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
