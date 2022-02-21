/* OHRRPGCE - software 3D rasterizer public API
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * This is the public complement to rasterizer.hpp.
 */

#ifndef GFXRENDER_H
#define GFXRENDER_H

#include <stdint.h>
#include "matrixMath.h"
#include "surface.h"

typedef float2 Position;
typedef float2 TexCoord;

//argb dword
struct Color
{
	union
	{
		uint32_t dw : 32;
		struct
		{
			uint8_t b : 8;
			uint8_t g : 8;
			uint8_t r : 8;
			uint8_t a : 8;
		};
		uint8_t comp[4];
	};
	Color& operator= (uint32_t rhs) {dw = rhs; return *this;}
	Color& operator= (const Color& rhs) {dw = rhs.dw; return *this;}
	operator uint32_t () const {return dw;}
	Color() : dw(0) {}
	Color(uint32_t col) : dw(col) {}
	Color(RGBcolor col) : dw(col.col) {}
	Color(uint8_t A, uint8_t R, uint8_t G, uint8_t B) : dw(0) {a=A;r=R;g=G;b=B;}
	operator RGBcolor() const { return RGBcolor{.col=dw}; }
	void scale(Color argbModifier)
	{
		
		// a = (a * argbModifier.a + 255) / 256;
		// r = (r * argbModifier.r + 255) / 256;
		// g = (g * argbModifier.g + 255) / 256;
		// b = (b * argbModifier.b + 255) / 256;

		a = a * argbModifier.a / 255;
		r = r * argbModifier.r / 255;
		g = g * argbModifier.g / 255;
		b = b * argbModifier.b / 255;
	}
	//void scale(Color16 argbModifier);

	void scale(Color c2, uint8_t weight)
	{
		a = (a*weight + c2.a*(255-weight)) / 255;
		r = (r*weight + c2.r*(255-weight)) / 255;
		g = (g*weight + c2.g*(255-weight)) / 255;
		b = (b*weight + c2.b*(255-weight)) / 255;
	}
};

// Color with signed 16-bit components, for measuring small increments
struct Color16
{
	int16_t comp[4];
	Color16() {}
	Color16(Color col) {
		for (int i = 0; i < 4; i++)
			comp[i] = col.comp[i] * 128;

	}
	operator Color() const {
		return Color(comp[0]>>7, comp[1]>>7, comp[2]>>7, comp[3]>>7);
	}
	Color16& operator+=(const Color16 &c2) {
		for (int i = 0; i < 4; i++)
			comp[i] += c2.comp[i];
		return *this;
	}
	Color16 operator-(const Color16 &c2) const {
		Color16 ret;
		for (int i = 0; i < 4; i++)
			ret.comp[i] = comp[i] - c2.comp[i];
		return ret;
	}
	// weight isn't multiplied by 255, unlike Color
	void scale(Color16 c2, float weight) {
		for (int i = 0; i < 4; i++)
			comp[i] = (comp[i]*weight + c2.comp[i]*(1.-weight));
	}
};

/* This overload slows down most code paths a little, though did speed up rasterTextureColor in clang
inline void Color::scale(Color16 argbModifier)	{
	b = b * argbModifier.comp[0] >> 15;
	g = g * argbModifier.comp[1] >> 15;
	r = r * argbModifier.comp[2] >> 15;
	a = a * argbModifier.comp[3] >> 15;
}
*/

struct VertexPC
{
	Position pos;
	Color col;

	// For measuring small increments in VertexPC
	struct VertexPC16 {
		Position pos;
		Color16 col;
		//VertexPC16(VertexPC v) : pos(v.pos), col(v.col) {}

		void interpolateComponents(const VertexPC16& v2, float scale) {
			float invScale(-scale + 1.0f);
			pos = pos * scale + v2.pos * invScale;
			col.scale(v2.col, scale);
		}
		void scaleDownUV() {}
		VertexPC16 &operator+=(const VertexPC16& v2) {
			pos += v2.pos;
			col += v2.col;
			return *this;
		}
		VertexPC16 operator-(const VertexPC16& v2) const {
			return VertexPC16{pos - v2.pos, col - v2.col};
		}
	};
	typedef VertexPC16 IncType;

	VertexPC() : pos(), col() {}
	VertexPC(IncType v) : pos(v.pos), col(v.col) {}
	operator IncType() const { return VertexPC16{pos, col}; }
};

struct VertexPT
{
	Position pos;
	TexCoord tex;
	typedef VertexPT IncType;

	//VertexPT() : pos(), tex() {}
	void interpolateComponents(const VertexPT& v2, float scale) {
		float invScale(-scale + 1.0f);
		pos = pos * scale + v2.pos * invScale;
		tex.u = scale * tex.u + invScale * v2.tex.u;
		tex.v = scale * tex.v + invScale * v2.tex.v;
	}
	void scaleDownUV() { tex *= 65535./65536.; }
	VertexPT &operator+=(const VertexPT& v2) {
		pos += v2.pos;
		tex += v2.tex;
		return *this;
	}
	IncType operator-(const VertexPT& v2) const {
		return VertexPT{pos - v2.pos, tex - v2.tex};
	}
};

struct VertexPTC
{
	Position pos;
	TexCoord tex;
	Color col;

	// For measuring small increments in VertexPTC
	struct VertexPTC16 {
		Position pos;
		TexCoord tex;
		Color16 col;
		//VertexPTC16(VertexPTC v) : pos(v.pos), tex(v.tex), col(v.col) {}

		void interpolateComponents(const VertexPTC16& v2, float scale) {
			float invScale(-scale + 1.0f);
			pos = pos * scale + v2.pos * invScale;
			tex.u = scale * tex.u + invScale * v2.tex.u;
			tex.v = scale * tex.v + invScale * v2.tex.v;
			col.scale(v2.col, scale);
		}
		void scaleDownUV() { tex *= 65535./65536.; }
		VertexPTC16 &operator+=(const VertexPTC16& v2) {
			pos += v2.pos;
			tex += v2.tex;
			col += v2.col;
			return *this;
		}
		VertexPTC16 operator-(const VertexPTC16& v2) const {
			return VertexPTC16{pos - v2.pos, tex - v2.tex, col - v2.col};
		}
	};
	typedef VertexPTC16 IncType;

	VertexPTC() : pos(), tex(), col() {}
	VertexPTC(IncType v) : pos(v.pos), tex(v.tex), col(v.col) {}
	operator IncType() const { return VertexPTC16{pos, tex, col}; }
};

//The following interface is implemented in surface.cpp
extern "C"
{

	void gfx_renderQuadColor_SW( VertexPC* pQuad, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts );
	void gfx_renderQuadTexture_SW( VertexPT* pQuad, Surface* pTexture, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts );
	void gfx_renderQuadTextureColor_SW( VertexPTC* pQuad, Surface* pTexture, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts );

	void gfx_renderTriangleColor_SW( VertexPC* pTriangle, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts );
	void gfx_renderTriangleTexture_SW( VertexPT* pTriangle, Surface* pTexture, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts );
	void gfx_renderTriangleTextureColor_SW( VertexPTC* pTriangle, Surface* pTexture, RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts );
};

#endif
