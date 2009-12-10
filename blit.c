/*
 * blit.c - Expensive graphics utility functions
 *
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 */


#include <string.h>

struct Palette16 {
	unsigned char col[16];  //indicies into the master palette
	int refcount; //private
};

//sprites use this
struct Frame {
	int w;
	int h;
	int pitch;     //pixel (x,y) is at .image[.x + .pitch * .y]; mask and image pitch are the same!
	unsigned char *image;
	unsigned char *mask;
	int refcount;  //see sprite_unload in particular for documentation
	int arraylen;  //how many frames were contiguously allocated in this frame array
	struct Frame *base;   //the Frame which actually owns this memory
	int cached:1;  //(not set for views onto cached sprites)
	int arrayelem:1;  //not the first frame in a frame array
	int isview:1;
};


void blitohr(struct Frame *spr, struct Frame *destspr, struct Palette16 *pal, int startoffset, int startx, int starty, int endx, int endy, int trans) {
	int i, j;
	unsigned char *maskp, *srcp, *destp;
	int srclineinc, destlineinc;

	srcp = spr->image;

	maskp = spr->mask;
	if (maskp == NULL)
		//we could add an optimised version for this case, which is the 99% case
		maskp = srcp;

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
}

//horribly slow; keep putting off doing something about it
void blitohrscaled(struct Frame *spr, struct Frame *destspr, struct Palette16 *pal, int x, int y, int startx, int starty, int endx, int endy, int trans, int scale) {
	unsigned char *sptr;
	unsigned char *mptr;
	int tx, ty;
	int sx, sy, pix, spix;

	sptr = destspr->image;

	mptr = spr->mask;
	if (spr->mask == 0) {
		mptr = spr->image;
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
					sptr[pix] = pal->col[spr->image[spix]];
				else
					sptr[pix] = spr->image[spix];
			}
		}
	} else {
		for (ty = starty; ty <= endy; ty++) {
			//tx = startx
			for (x = startx; x <= endx; x++) {
				//figure out where to put the pixel
				pix = (ty * destspr->pitch) + tx;
				//and where to get the pixel from
				spix = (((ty - y) / scale) * spr->pitch) + ((tx - x) / scale);
					
				//check mask
				if (mptr[spix]) {
					if (pal != 0)
						sptr[pix] = pal->col[spr->image[spix]];
					else
						sptr[pix] = spr->image[spix];
				}
			}
		}
	}
}

void smoothzoomblit_8_to_8bit(unsigned char *rptr, unsigned char *dptr, int w, int h, int pitch, int zoom, int smooth) {
//rptr: source w x h buffer paletted 8 bit
//dptr: destination scaled buffer pitch x h*zoom also 8 bit
//supports zoom 1 to 4

	unsigned char *sptr;
	unsigned int mult = 1;
	int i, j;
	int fx, fy, p0, p1, p2, p3, p4, pstep;  //for 2x/3x filtering
	int wide = w * zoom, high = h * zoom;

	sptr = dptr;

	if (zoom == 1) {
		for (i = 0; i <= h - 1; i++) {
			memcpy(sptr, rptr, w);
			rptr += pitch;
			sptr += w;
		}
	} else {
		for (i = 2; i <= zoom; i++)
			mult = (mult << 8) + 1;

		for (j = 0; j <= h - 1; j++) {
			for (i = w / 4 - 1; i >= 0; i--) {
				//could just multiple by &h1010101, but FB produces truly daft code for multiplication by constants
				*(int *)sptr = rptr[0] * mult;
				sptr += zoom;
				*(int *)sptr = rptr[1] * mult;
				sptr += zoom;
				*(int *)sptr = rptr[2] * mult;
				sptr += zoom;
				*(int *)sptr = rptr[3] * mult;
				sptr += zoom;
				rptr += 4;
			}
			for (i = (w % 4) - 1; i >= 0; i--) {
				*(int *)sptr = rptr[0] * mult;
				sptr += zoom;
				rptr += 1;
			}
			sptr += pitch - wide;
			//repeat row zoom times
			for (i = 2; i <= zoom; i++) {
				memcpy (sptr, sptr - pitch, wide);
				sptr += pitch;
			}
		}
	}

	if (smooth == 1 && (zoom == 2 || zoom == 3)) {
		if (zoom == 3)
			pstep = 1;
		else
			pstep = 2;
		char *sptr1, *sptr2, *sptr3;
		for (fy = 1; fy <= high - 2; fy += pstep) {
			sptr1 = dptr + pitch * (fy - 1) + 1;  //(1,0)
			sptr2 = sptr1 + pitch; //(1,1)
			sptr3 = sptr2 + pitch; //(1,2)
			for (fx = wide - 2; fx >= 1; fx--) {
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

void smoothzoomblit_8_to_32bit(unsigned char *rptr, unsigned char *dptr, int w, int h, int pitch, int zoom, int smooth, int pal[]) {
//rptr: source w x h buffer paletted 8 bit
//dptr: destination scaled buffer pitch x h*zoom 32 bit (so pitch is in pixels, not bytes)
//supports zoom 1 to 4

	unsigned int *sptr;
	int pixel;
	int i, j;
	int fx, fy, p0, p1, p2, p3, p4, pstep;  //for 2x/3x filtering
	int wide = w * zoom, high = h * zoom;

	sptr = (int*)dptr;

	for (j = 0; j <= h - 1; j++) {
		//repeat row zoom times
		for (i = 0; i <= w - 1; i++) {
			//get colour
			pixel = pal[*rptr];
			//zoom sptrs for each rptr
			for (j = zoom; j >= 1; j++) {
				*sptr = pixel;
				sptr += 1;
			}
			rptr += 1;
		}

		sptr += pitch - wide;
		//repeat row zoom times
		for (i = 2; i <= zoom; i++) {
			memcpy (sptr, sptr - pitch, 4 * wide);
			sptr += pitch;
		}
	}
	if (smooth == 1 && (zoom == 2 || zoom == 3)) {
		if (zoom == 3)
			pstep = 1;
		else
			pstep = 2;
		int *sptr1, *sptr2, *sptr3;
		for (fy = 1; fy <= (high - 2); fy += pstep) {
			sptr1 = (int *)dptr + pitch * (fy - 1) + 1;  //(1,0)
			sptr2 = sptr1 + pitch; //(1,1)
			sptr3 = sptr2 + pitch; //(1,2)
			for (fx = wide - 2; fx >= 1; fx--) {
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

/*
void smoothzoomblit_anybit(char *rptr, char *dptr, int w, int h, int pitch, int zoom, int smooth, int bpp) {
//moved from gfx_sdl, it might be needed, probably just replace with 15/16 bit version of above
//no smoothing done yet
//rptr: source pitch x h buffer variable bbp
//dptr: destination scaled buffer variable bbp

	int x, y, b, z;
	int *sptr;

	sptr = dptr;

	for (y = h - 1; y >= 0; y--) {
		for (x = 0; x <= w - 1; x++) {
			for (z = 0; z <= zoom - 1; z++) {
				*sptr = *rptr;
				sptr += bpp;
			}
			rptr += bpp;
		}

		sptr += pitch - x * zoom;
		//repeat row zoom times
		for (i = 2; i <= zoom; i++) {
			memcpy(sptr, sptr - pitch, 4 * wide);
			sptr += pitch;
		}
	}
}
*/
