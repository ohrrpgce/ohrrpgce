/*
Rotozoomer, zoomer and shrinker for 32bit or 8bit surfaces
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

#ifndef rotozoom_h
#define rotozoom_h

#include <math.h>

/* Set up for C function definitions, even when using C++ */
#ifdef __cplusplus
extern "C" {
#endif

	/* ---- Defines */

	/*!
	\brief Disable anti-aliasing (no smoothing).
	*/
#define SMOOTHING_OFF		0

	/*!
	\brief Enable anti-aliasing (smoothing).
	*/
#define SMOOTHING_ON		1

	/* ---- Function Prototypes */

	/* 

	Rotozoom functions

	*/

	SDL_Surface *rotozoomSurfaceXY
		(SDL_Surface * src, double angle, double zoomx, double zoomy, int smooth);

	void rotozoomSurfaceSizeXY
		(int width, int height, double angle, double zoomx, double zoomy, 
		int *dstwidth, int *dstheight);

	void zoomSurfaceSize(int width, int height, double zoomx, double zoomy, int *dstwidth, int *dstheight);

	/* 

	Specialized rotation functions

	*/

	SDL_Surface* rotateSurface90Degrees(SDL_Surface* src, int numClockwiseTurns);

	/* Ends C function definitions when using C++ */
#ifdef __cplusplus
}
#endif

#endif				/* rotozoom_h */
