//gfx_newRenderPlan.h
//exposes the proposed functions to the engine

#ifndef GFX_NEWRENDERPLAN_H
#define GFX_NEWRENDERPLAN_H

//#include "FPInt.h"
#include <stdint.h>

//surfaces
enum SurfaceFormat
{
	SF_8bit = 0,
	SF_32bit = 1,
};
enum SurfaceUsage
{
	SU_Source = 0,
	SU_RenderTarget = 1,
	SU_Backbuffer = 2,
};
struct Surface
{
	uint32_t width;
	uint32_t height;
	SurfaceFormat format;
	SurfaceUsage usage;
	union
	{
		void* pRawData;
		uint32_t* pColorData;
		uint8_t* pPaletteData;
	};
};
struct SurfaceRect
{
	int32_t left, top, right, bottom;
};

//palettes
struct Palette
{
	uint32_t p[256];
};

//vertices
struct Position
{
	float x,y;
	//FPInt x,y;
	Position() : x(0), y(0) {}
	//Position operator+(const Position& rhs) const {Position tmp = {x+rhs.x, y+rhs.y}; return tmp;}
	//Position operator-(const Position& rhs) const {Position tmp = {x-rhs.x, y-rhs.y}; return tmp;}
	//Position operator*(const Position& rhs) const {Position tmp = {x*rhs.x, y*rhs.y}; return tmp;}
	//Position operator/(const Position& rhs) const {Position tmp = {x/rhs.x, y/rhs.y}; return tmp;}
	//Position& operator+=(const Position& rhs) {x += rhs.x; y += rhs.y; return *this;}
	//Position& operator-=(const Position& rhs) {x -= rhs.x; y -= rhs.y; return *this;}
	//Position& operator*=(const Position& rhs) {x *= rhs.x; y *= rhs.y; return *this;}
	//Position& operator/=(const Position& rhs) {x /= rhs.x; y /= rhs.y; return *this;}
};
struct TexCoord
{
	float u,v;
	//FPInt u,v;
	TexCoord() : u(0), v(0) {}
	//TexCoord operator+(const TexCoord& rhs) const {TexCoord tmp = {u+rhs.u, v+rhs.v}; return tmp;}
	//TexCoord operator-(const TexCoord& rhs) const {TexCoord tmp = {u-rhs.u, v-rhs.v}; return tmp;}
	//TexCoord operator*(const TexCoord& rhs) const {TexCoord tmp = {u*rhs.u, v*rhs.v}; return tmp;}
	//TexCoord operator/(const TexCoord& rhs) const {TexCoord tmp = {u/rhs.u, v/rhs.v}; return tmp;}
	//TexCoord& operator+=(const TexCoord& rhs) {u += rhs.u; v += rhs.v; return *this;}
	//TexCoord& operator-=(const TexCoord& rhs) {u -= rhs.u; v -= rhs.v; return *this;}
	//TexCoord& operator*=(const TexCoord& rhs) {u *= rhs.u; v *= rhs.v; return *this;}
	//TexCoord& operator/=(const TexCoord& rhs) {u /= rhs.u; v /= rhs.v; return *this;}
};
struct Color //argb dword; palette stored in lowest byte, that is 'b'
{
	union
	{
		uint32_t dw : 32;
		struct
		{
			uint8_t b : 8; //lowest; also used for palette
			uint8_t g : 8;
			uint8_t r : 8;
			uint8_t a : 8; //highest
		};
	};
	Color& operator= (uint32_t rhs) {dw = rhs; return *this;}
	Color& operator= (const Color& rhs) {dw = rhs.dw; return *this;}
	operator uint32_t () const {return dw;}
	operator uint8_t () const {return b;}
	Color() : dw(0) {}
	Color(uint32_t col) : dw(col) {}
	Color(uint8_t A, uint8_t R, uint8_t G, uint8_t B) : dw(0) {a=A;r=R;g=G;b=B;}
	Color(uint8_t palette) : dw(0) {b=palette;}
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
struct VertexC
{
	Position pos;
	Color col;
	VertexC() : pos(), col() {}
	void interpolateComponents(const VertexC& v2, float scale) {col.scale(v2.col, 255.0f*scale);}
};
struct VertexT
{
	Position pos;
	Color col;
	TexCoord tex;
	VertexT() : pos(), col(), tex() {}
	void interpolateComponents(const VertexT& v2, float scale) {
		col.scale(v2.col, 255.0f*scale);
		float invScale(-scale + 1);
		tex.u = scale * tex.u + invScale * v2.tex.u;
		tex.v = scale * tex.v + invScale * v2.tex.v;
	}
};

//quads
struct QuadC
{
	VertexC pnt[4];
};
struct QuadT
{
	VertexT pnt[4];
};



//interfaces
extern "C"
{
	int gfx_surfaceCreate( uint32_t width, uint32_t height, SurfaceFormat format, SurfaceUsage usage, Surface** ppSurfaceOut );
	int gfx_surfaceDestroy( Surface* pSurfaceIn );
	int gfx_surfaceUpdate( Surface* pSurfaceIn );
	int gfx_surfaceFill( uint32_t fillColor, SurfaceRect* pRect, Surface* pSurfaceIn );
	int gfx_surfaceStretch( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette );
	int gfx_surfaceStretchWithColorKey( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette, uint8_t colorKey );
	int gfx_surfaceCopy( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette );
	int gfx_surfaceCopyWithColorKey( SurfaceRect* pRectSrc, Surface* pSurfaceSrc, SurfaceRect* pRectDest, Surface* pSurfaceDest, Palette* pPalette, uint8_t colorKey );

	int gfx_paletteCreate( Palette** ppPaletteOut );
	int gfx_paletteDestroy( Palette* pPaletteIn );
	int gfx_paletteUpdate( Palette* pPaletteIn );

	int gfx_renderQuadColor( QuadC* pQuad, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	int gfx_renderQuadTexture( QuadT* pQuad, Surface* pTexture, Palette* pPalette, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	int gfx_renderQuadTextureWithColorKey( QuadT* pQuad, Surface* pTexture, Palette* pPalette, uint8_t colorKey, uint32_t argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest );
	int gfx_renderBegin();
	int gfx_renderEnd();

	int gfx_present( Surface* pSurfaceIn );
};

#endif