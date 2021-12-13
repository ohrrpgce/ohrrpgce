/* OHRRPGCE - RGB alpha blending
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

// Blend source and dest pixels in 24-bit colour space.
// Alpha channel-based blending isn't enabled yet; this blends using fixed alpha
// alpha is 0-256.
static inline RGBcolor alpha_blend(RGBcolor src, RGBcolor dest, int alpha, enum BlendMode mode) {
	RGBcolor res;
	// In future to support alpha channels uncomment the two commented lines.
	// alpha = alpha * src.a / 256;
	if (mode == blendModeNormal) {
		// Equivalent to SDL2's SDL_BLENDMODE_BLEND
		int alpha2 = 256 - alpha;
		res.r = (src.r * alpha + dest.r * alpha2) / 256;
		res.g = (src.g * alpha + dest.g * alpha2) / 256;
		res.b = (src.b * alpha + dest.b * alpha2) / 256;
		//res.a = alpha + dest.a * alpha2 / 256;
		res.a = dest.a;  // Wrong
	} else if (mode == blendModeAdd) {
		// Equivalent to SDL2's SDL_BLENDMODE_ADD
		int r, g, b;
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
		int r, g, b;
		int alpha2 = 256 - alpha;
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
