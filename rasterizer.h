//rasterizer.h
//by Jay Tennant 5/30/11
//exposes method of triangle rasterization on software surfaces

#pragma once

#include "fpInt.h"
#include "surface.h"
#include <queue>

struct Position
{
	FPInt x,y;
};
struct TexCoord
{
	FPInt u,v;
	TexCoord() {}
	TexCoord(FPInt x, FPInt y) : u(x), v(y) {}
};
struct Color //argb dword; palette stored in lowest byte, that is 'b'
{
	union
	{
		unsigned __int32 dw : 32;
		struct
		{
			unsigned __int8 b : 8; //lowest; also used for palette
			unsigned __int8 g : 8;
			unsigned __int8 r : 8;
			unsigned __int8 a : 8; //highest
		};
	};
	Color& operator= (unsigned __int32 rhs) {dw = rhs; return *this;}
	Color& operator= (const Color& rhs) {dw = rhs.dw; return *this;}
	operator unsigned __int32 () const {return dw;}
	operator unsigned __int8 () const {return b;}
	Color() : dw(0) {}
	Color(unsigned __int32 col) : dw(col) {}
	Color(unsigned __int8 A, unsigned __int8 R, unsigned __int8 G, unsigned __int8 B) : dw(0) {a=A;r=R;g=G;b=B;}
	Color(unsigned __int8 palette) : dw(0) {b=palette;}
};
//struct Color //fixed point color; kind of messy and unnecessary--probably should remove altogether
//{
//	FPInt a,r,g,b;
//	operator(unsigned __int32) () {
//		unsigned __int32 n = 0x0; 
//		n |= ((0xff & (a.fraction >> 8)) << 24);
//		n |= ((0xff & (r.fraction >> 8)) << 16);
//		n |= ((0xff & (g.fraction >> 8)) << 8);
//		n |=  (0xff & (b.fraction >> 8));
//		return n;
//	}
//	operator(unsigned __int8) () {return (0xff & (a.fraction >> 8));}
//};

//samples a surface; might want to expand this functionality later,
//such as setting sampler state (otherwise, what's the point of this class?)
class Tex2DSampler
{
public:
	Color sample(const Surface* pSurface, FPInt x, FPInt y) const;
};

struct Vertex
{
	Position pos;
	TexCoord tex;
	Color col;
};

struct Triangle
{
	Vertex pnt[3];
};

struct ClippingRect
{
	FPInt left, top, right, bottom;
};

struct DrawingRange
{
	Vertex least, greatest;
	DrawingRange(const Vertex& Least, const Vertex& Greatest) : least(Least), greatest(Greatest) {}
};

class TriRasterizer
{
protected:
	std::queue<DrawingRange> m_rasterLines;
	Tex2DSampler m_sampler;
	void calculateTriangleRect(ClippingRect& clipOut, const Triangle* pTriangle);
	void interpolateVertices(Vertex& vertexOut, const Vertex& v1, const Vertex& v2, FPInt scale);
	void calculateRasterPixels(unsigned __int32 row, FPInt minimum, FPInt maximum, const Surface* pSurface, const Triangle* pTriangle);
	void rasterColor(Surface* pSurface, const DrawingRange& range, const Triangle* pTriangle);
	void rasterTexture(Surface* pSurface, const DrawingRange& range, const Triangle* pTriangle, const Surface* pTexture);
	void rasterTextureColor(Surface* pSurface, const DrawingRange& range, const Triangle* pTriangle, const Surface* pTexture);
public:
	void drawTest(Surface* pSurface, const Triangle* pTriangle, const Color& col);
	void drawColor(Surface* pSurface, const Triangle* pTriangle);
	void drawTexture(Surface* pSurface, const Triangle* pTriangle, const Surface* pTexture);
	void drawTextureColor(Surface* pSurface, const Triangle* pTriangle, const Surface* pTexture);
};

struct Quad
{
	Vertex pnt[4];
};

class QuadRasterizer
{
protected:
	TriRasterizer m_triRasterizer;
	Triangle m_triangles[4];
	void generateTriangles(const Quad* pQuad);
public:
	void drawTest(Surface* pSurface, const Quad* pQuad, const Color& col)
	{
		if(pSurface == NULL || pQuad == NULL)
			return;
		generateTriangles(pQuad);
		for(int i = 0; i < 4; i++)
			m_triRasterizer.drawTest(pSurface, &m_triangles[i], col);
	}
	void drawColor(Surface* pSurface, const Quad* pQuad)
	{
		if(pSurface == NULL || pQuad == NULL)
			return;
		generateTriangles(pQuad);
		for(int i = 0; i < 4; i++)
			m_triRasterizer.drawColor(pSurface, &m_triangles[i]);
	}
	void drawTexture(Surface* pSurface, const Quad* pQuad, const Surface* pTexture)
	{
		if(pSurface == NULL || pQuad == NULL)
			return;
		generateTriangles(pQuad);
		for(int i = 0; i < 4; i++)
			m_triRasterizer.drawTexture(pSurface, &m_triangles[i], pTexture);
	}
	void drawTextureColor(Surface* pSurface, const Quad* pQuad, const Surface* pTexture)
	{
		if(pSurface == NULL || pQuad == NULL)
			return;
		generateTriangles(pQuad);
		for(int i = 0; i < 4; i++)
			m_triRasterizer.drawTextureColor(pSurface, &m_triangles[i], pTexture);
	}
};
