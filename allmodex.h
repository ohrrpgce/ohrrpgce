// structs translated from udts.bi, for C interoperability with allmodex.bas

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

typedef struct {
	int numcolors;
	int refcount;            //private
	unsigned char col[256];  //indices into the master palette
} Palette16;

struct SpriteCacheEntry;
struct SpriteSet;

typedef struct _Frame {
	int w;
	int h;
	XYPair offset; //Draw offset from the position passed to frame_draw. Used by frame_dissolve
	int pitch;     //pixel (x,y) is at .image[.x + .pitch * .y]; mask and image pitch are the same!
	unsigned char *image;
	unsigned char *mask;
	int refcount;  //see sprite_unload in particular for documentation
	int arraylen;  //how many frames were contiguously allocated in this frame array
	struct _Frame *base;   //the Frame which actually owns this memory
	struct SpriteCacheEntry *cacheentry;
	int cached:1;  //(not set for views onto cached sprites) integer, NOT bool!
	int arrayelem:1;  //not the first frame in a frame array
	int isview:1;
	int noresize:1;  //(Video pages only.) Don't resize this page to the window size

	struct SpriteSet *sprset;  //if not NULL, this Frame array is part of a SpriteSet which
	                           //will need to be freed at the same time
} Frame;

Frame* frame_reference(Frame *p);
void frame_unload(Frame** p);

#ifdef __cplusplus
}
#endif

#endif
