/* OHRRPGCE - Inline graphics routines
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * Inline functions used in blitting and alpha blending inner loops.
 */

#include "allmodex.h"
#include "surface.h"

#ifdef __cplusplus
extern "C" {
#endif


// Blend source and dest pixels in 24-bit colour space.
// This blends using fixed alpha (range 0-256) and optionally
// also combining with the alpha channel value (range 0-255)
static inline RGBcolor alpha_blend(RGBcolor src, RGBcolor dest, unsigned int alpha, enum BlendMode mode, bool alpha_channel) {
	RGBcolor res;
	if (alpha_channel)
		alpha = alpha * src.a / 255;
	if (mode == blendModeNormal) {
		// Equivalent to SDL2's SDL_BLENDMODE_BLEND
		unsigned int alpha2 = 256 - alpha;
		res.r = (src.r * alpha + dest.r * alpha2) / 256;
		res.g = (src.g * alpha + dest.g * alpha2) / 256;
		res.b = (src.b * alpha + dest.b * alpha2) / 256;
		if (alpha_channel)
			res.a = alpha + dest.a * alpha2 / 256;  // Correct
		else
			res.a = dest.a;  // Wrong?

		// Failed optimsation attempt, actually slower
		/*
		res.col = ( ( ( (src.col & 0x00ff00ff) * alpha + (dest.col & 0x00ff00ff) * alpha2 )
			      >> 8) & 0x00ff00ff) |
			  ( ( ( (src.col & 0xff00ff00) >> 8) * alpha + ((dest.col & 0xff00ff00) >> 8) * alpha2 )
			       & 0xff00ff00);
		*/
	} else if (mode == blendModeAdd) {
		// Equivalent to SDL2's SDL_BLENDMODE_ADD
		unsigned int r, g, b;
		r = src.r * alpha / 256 + dest.r;
		if (r > 255) r = 255;
		g = src.g * alpha / 256 + dest.g;
		if (g > 255) g = 255;
		b = src.b * alpha / 256 + dest.b;
		if (b > 255) b = 255;
		res.r = r;
		res.g = g;
		res.b = b;
		res.a = dest.a;  // Correct
	} else { // (mode == blendModeMultiply) {
		// Equivalent to SDL2's new SDL_BLENDMODE_MUL, except that
		// requires pre-multiplied alpha, unlike other modes!
		// So we will need to call SDL_SetSurfaceColorMod(surf, a, a, a)
		unsigned int r, g, b;
		unsigned int alpha2 = 256 - alpha;
		//r = (src.r * dest.r + dest.r * alpha2) / 255;   // premultiplied alpha
		r = (src.r * dest.r * alpha / 256 + dest.r * alpha2) / 256;
		if (r > 255) r = 255;  // Is this necessary?
		g = (src.g * dest.g * alpha / 256 + dest.g * alpha2) / 256;
		if (g > 255) g = 255;
		b = (src.b * dest.b * alpha / 256 + dest.b * alpha2) / 256;
		if (b > 255) b = 255;
		res.r = r;
		res.g = g;
		res.b = b;
		res.a = dest.a;  // Correct
	}
	return res;
}

// Memoize nearcolor_fast, dropping 3 least-significant bits red & blue, 2 from green.
inline int nearcolor_faster(RGBcolor searchcol) {
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

struct RGBerrors {
	int r, g, b;
};

static inline int map_rgb_to_masterpal(RGBcolor src, struct RGBerrors *rgberr, int tog, int x_and_y) {
	// Convert back to master palette, possibly performing error diffusion.
	int res;

	/* if (blend_algo == -1) {
	   res = *destp;
	   } else */
	if (blend_algo == blendAlgoNoDither) {
		// Use faster lookup, because you don't care about color accuracy
		// anyway if you're using this.
		res = nearcolor_faster(src);
	} else {
		int r = src.r + rgberr->r;
		int g = src.g + rgberr->g;
		int b = src.b + rgberr->b;
		if (r > 255) r = 255;
		else if (r < 0) r = 0;
		if (g > 255) g = 255;
		else if (g < 0) g = 0;
		if (b > 255) b = 255;
		else if (b < 0) b = 0;

		RGBcolor searchcol = {{(unsigned char)b, (unsigned char)g, (unsigned char)r, 0}};
		res = nearcolor_faster(searchcol);

		// blendAlgoDither is a chequered dither where we wipe the
		// error every 2nd pixel, propagate just 1/2 the error from 1/4
		// of the pixelsand propagate 3/4 from the other 1/4.
		// This creates quarter-dither patterns so that as opacity
		// is faded there are smaller, smoother steps. The low amount
		// of error propagation reduces grain and artifacts at the cost
		// of color accuracy, which is unimportant.
		if (tog) {  // (i^j)&1
			rgberr->r = rgberr->g = rgberr->b = 0;
		} else {
			int m;
			if (blend_algo == blendAlgoDither)
				m = 2 + (x_and_y & 1);  // 2 or 3
			else // blendAlgoLessDither
				m = 1;
			rgberr->r = m * (r - curmasterpal[res].r) / 4;
			rgberr->g = m * (g - curmasterpal[res].g) / 4;
			rgberr->b = m * (b - curmasterpal[res].b) / 4;
		}
	}
	return res;
}


#ifdef __cplusplus
}
#endif
