/* OHRRPGCE - allmodex C header
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 */

#ifndef ALLMODEX_H
#define ALLMODEX_H

#include <stdint.h>
#include "surface.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	int w;
	int h;
} XYPair;

typedef struct Palette16 {
	int numcolors;
	int refcount;            //Always >= 1 (palcache counts as a reference). Can not be NOREFC.
	int palnum;              //>= 0: numbered palette, cached. -1: not loaded from file, uncached.

	unsigned char col[256];  //indices into the master palette
} Palette16;

typedef struct SpriteCacheEntry SpriteCacheEntry;
typedef struct SpriteSet SpriteSet;

typedef struct Frame {
	int w;
	int h;
	XYPair offset; //Draw offset from the position passed to frame_draw. Not used yet.
	int pitch;     //pixel (x,y) is at .image[.x + .pitch * .y]; mask and image pitch are the same!
	unsigned char *image; //Pointer to top-left corner. NULL if and only if Surface-backed.
	unsigned char *mask;  //Same shape as image. If not NULL, nonzero bytes in mask are opaque, rather
	                      //than nonzero bytes in image. Most Frames don't have a mask.
	int refcount;  //see sprite_unload in particular for documentation
		       //Must be NOREFC if allocated on stack rather than heap (frame_load/etc)
	int arraylen;  //how many frames were contiguously allocated in this frame array
	int frameid;   //Used by frames in a frameset (always in increasing order): alternative to frame number
	Frame *base;   //the Frame which actually owns this memory
	SpriteCacheEntry *cacheentry;  //First Frame in array only
	int cached:1;  //(not set for views onto cached sprites) integer, NOT bool! First Frame in array only.
	int arrayelem:1; //not the first frame in a frame array
	int isview:1;    //View of another Frame (which might be backed by a Surface, in which we will be
	                 //backed by a Surface too, created with gfx_surfaceCreateView).
	                 //Aside from that, this is NOT true for Surface-backed Frames which aren't views!
	                 //If this is a view, then 'image' and 'mask' mustn't be freed, but 'surf' must be.
	int noresize:1;  //(Video pages only.) Don't resize this page to the window size
	int fixeddepth:1;//(Video pages only.) Not affected by switch_to_32bit/8bit_vpages: Not a render target.

	Surface *surf;   //If not NULL, this is a Surface-backed Frame, and image/mask are NULL,
	                 //but all other members are correct (including .pitch), and match the Surface.
	                 //(View of a WHOLE Surface.) Holds a single reference to surf.

	SpriteSet *sprset;  //if not NULL, this Frame array is part of a SpriteSet which
	                    //will need to be freed at the same time
	                    //First Frame in array only.
	int defpal;      //Default palette or -1 if not loaded. Only set on first frame of array!
} Frame;

Frame* frame_reference(Frame *p);
void frame_unload(Frame** p);
uint8_t nearcolor_fast(RGBcolor col);
uint8_t nearcolor_master(RGBcolor col, int firstindex);

// Master palette
extern RGBcolor curmasterpal[256];

#ifdef __cplusplus
}
#endif

#endif
