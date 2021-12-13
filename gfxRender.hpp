/* OHRRPGCE - software 3D rasterizer public API
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * This is the public complement to rasterizer.hpp.
 */

#ifndef GFXRENDER_H
#define GFXRENDER_H

#include <stdint.h>
#include "surface.h"

//vertices
struct Position
{
	float x,y;
	//FPInt x,y;
	Position() : x(0), y(0) {}
	Position(float X, float Y) : x(X), y(Y) {}
	Position operator+(const Position& rhs) const {Position tmp(x+rhs.x, y+rhs.y); return tmp;}
	Position operator-(const Position& rhs) const {Position tmp(x-rhs.x, y-rhs.y); return tmp;}
	Position operator*(const Position& rhs) const {Position tmp(x*rhs.x, y*rhs.y); return tmp;}
	Position operator/(const Position& rhs) const {Position tmp(x/rhs.x, y/rhs.y); return tmp;}
	Position& operator+=(const Position& rhs) {x += rhs.x; y += rhs.y; return *this;}
	Position& operator-=(const Position& rhs) {x -= rhs.x; y -= rhs.y; return *this;}
	Position& operator*=(const Position& rhs) {x *= rhs.x; y *= rhs.y; return *this;}
	Position& operator/=(const Position& rhs) {x /= rhs.x; y /= rhs.y; return *this;}
	Position operator+(float rhs) const {Position tmp(x+rhs, y+rhs); return tmp;}
	Position operator-(float rhs) const {Position tmp(x-rhs, y-rhs); return tmp;}
	Position operator*(float rhs) const {Position tmp(x*rhs, y*rhs); return tmp;}
	Position operator/(float rhs) const {Position tmp(x/rhs, y/rhs); return tmp;}
	Position& operator+=(float rhs) {x += rhs; y += rhs; return *this;}
	Position& operator-=(float rhs) {x -= rhs; y -= rhs; return *this;}
	Position& operator*=(float rhs) {x *= rhs; y *= rhs; return *this;}
	Position& operator/=(float rhs) {x /= rhs; y /= rhs; return *this;}
};
struct TexCoord
{
	float u,v;
	//FPInt u,v;
	TexCoord() : u(0), v(0) {}
	TexCoord(float U, float V) : u(U), v(V) {}
	TexCoord operator+(const TexCoord& rhs) const {TexCoord tmp(u+rhs.u, v+rhs.v); return tmp;}
	TexCoord operator-(const TexCoord& rhs) const {TexCoord tmp(u-rhs.u, v-rhs.v); return tmp;}
	TexCoord operator*(const TexCoord& rhs) const {TexCoord tmp(u*rhs.u, v*rhs.v); return tmp;}
	TexCoord operator/(const TexCoord& rhs) const {TexCoord tmp(u/rhs.u, v/rhs.v); return tmp;}
	TexCoord& operator+=(const TexCoord& rhs) {u += rhs.u; v += rhs.v; return *this;}
	TexCoord& operator-=(const TexCoord& rhs) {u -= rhs.u; v -= rhs.v; return *this;}
	TexCoord& operator*=(const TexCoord& rhs) {u *= rhs.u; v *= rhs.v; return *this;}
	TexCoord& operator/=(const TexCoord& rhs) {u /= rhs.u; v /= rhs.v; return *this;}
	TexCoord operator+(float rhs) const {TexCoord tmp(u+rhs, v+rhs); return tmp;}
	TexCoord operator-(float rhs) const {TexCoord tmp(u-rhs, v-rhs); return tmp;}
	TexCoord operator*(float rhs) const {TexCoord tmp(u*rhs, v*rhs); return tmp;}
	TexCoord operator/(float rhs) const {TexCoord tmp(u/rhs, v/rhs); return tmp;}
	TexCoord& operator+=(float rhs) {u += rhs; v += rhs; return *this;}
	TexCoord& operator-=(float rhs) {u -= rhs; v -= rhs; return *this;}
	TexCoord& operator*=(float rhs) {u *= rhs; v *= rhs; return *this;}
	TexCoord& operator/=(float rhs) {u /= rhs; v /= rhs; return *this;}
};

//argb dword
// Was also used for storing an 8 bit palette index in 'b', but
// that seems to be a bad idea.
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
	};
	Color& operator= (uint32_t rhs) {dw = rhs; return *this;}
	Color& operator= (const Color& rhs) {dw = rhs.dw; return *this;}
	operator uint32_t () const {return dw;}
	// operator uint8_t () const {return b;}
	Color() : dw(0) {}
	Color(uint32_t col) : dw(col) {}
	Color(RGBcolor col) : dw(col.col) {}
	Color(uint8_t A, uint8_t R, uint8_t G, uint8_t B) : dw(0) {a=A;r=R;g=G;b=B;}
	// Color(uint8_t palette) : dw(0) {b=palette;}
	void scale(Color argbModifier)
	{
		a = a * argbModifier.a / 255;
		r = r * argbModifier.r / 255;
		g = g * argbModifier.g / 255;
		b = b * argbModifier.b / 255;
	}
	void scale(Color c2, uint8_t weight)
	{
		a = (a*weight + c2.a*(255-weight)) / 255;
		r = (r*weight + c2.r*(255-weight)) / 255;
		g = (g*weight + c2.g*(255-weight)) / 255;
		b = (b*weight + c2.b*(255-weight)) / 255;
	}
};

struct VertexPC
{
	Position pos;
	Color col;
	VertexPC() : pos(), col() {}
	void interpolateComponents(const VertexPC& v2, float scale) {
		float invScale(-scale + 1.0f);
		pos = pos * scale + v2.pos * invScale;
		col.scale(v2.col, 255.0f*scale);
	}
};
struct VertexPT
{
	Position pos;
	TexCoord tex;
	VertexPT() : pos(), tex() {}
	void interpolateComponents(const VertexPT& v2, float scale) {
		float invScale(-scale + 1.0f);
		pos = pos * scale + v2.pos * invScale;
		tex.u = scale * tex.u + invScale * v2.tex.u;
		tex.v = scale * tex.v + invScale * v2.tex.v;
	}
};
struct VertexPTC
{
	Position pos;
	TexCoord tex;
	Color col;
	VertexPTC() : pos(), tex(), col() {}
	void interpolateComponents(const VertexPTC& v2, float scale) {
		float invScale(-scale + 1.0f);
		pos = pos * scale + v2.pos * invScale;
		tex.u = scale * tex.u + invScale * v2.tex.u;
		tex.v = scale * tex.v + invScale * v2.tex.v;
		col.scale(v2.col, 255.0f*scale);
	}
};

//The following interface is implemented in surface.cpp
extern "C"
{

	int gfx_renderQuadColor_SW( VertexPC* pQuad, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	int gfx_renderQuadTexture_SW( VertexPT* pQuad, Surface* pTexture, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	int gfx_renderQuadTextureColor_SW( VertexPTC* pQuad, Surface* pTexture, RGBPalette* pPalette, int bUseColorKey0, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest );

	int gfx_renderTriangleColor_SW( VertexPC* pTriangle, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	int gfx_renderTriangleTexture_SW( VertexPT* pTriangle, Surface* pTexture, RGBPalette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	int gfx_renderTriangleTextureColor_SW( VertexPTC* pTriangle, Surface* pTexture, RGBPalette* pPalette, int bUseColorKey0, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest );
};

#endif
