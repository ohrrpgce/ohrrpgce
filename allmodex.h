// structs translated from udts.bi, for C interoperability with allmodex.bas

#ifndef ALLMODEX_H
#define ALLMODEX_H

#include <stdint.h>

struct XYPair {
	int w;
	int h;
};

struct Palette16 {
	unsigned char col[16];  //indicies into the master palette
	int refcount; //private
};

typedef	uint32_t RGBcolor;

struct SpriteCacheEntry;
struct SpriteSet;

struct Frame {
	int w;
	int h;
	int pitch;     //pixel (x,y) is at .image[.x + .pitch * .y]; mask and image pitch are the same!
	unsigned char *image;
	unsigned char *mask;
	int refcount;  //see sprite_unload in particular for documentation
	int arraylen;  //how many frames were contiguously allocated in this frame array
	struct Frame *base;   //the Frame which actually owns this memory
	struct SpriteCacheEntry *cacheentry;
	int cached:1;  //(not set for views onto cached sprites)
	int arrayelem:1;  //not the first frame in a frame array
	int isview:1;

	//used only by frames in a SpriteSet, for now
	struct XYPair offset;
	struct SpriteSet *sprset;  //if not NULL, this Frame array is part of a SpriteSet which
	                           //will need to be freed at the same time
};

#endif
