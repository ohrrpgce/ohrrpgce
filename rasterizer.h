//rasterizer.h
//by Jay Tennant 5/30/11
//exposes method of triangle rasterization on software surfaces

#ifndef RASTERIZER_H
#define RASTERIZER_H

#include "fpInt.h"
//#include "surface.h"
#include "gfx_newRenderPlan.h"
#include <queue>

//samples a surface; might want to expand this functionality later,
//such as setting sampler state (otherwise, what's the point of this class?)
class Tex2DSampler
{
public:
	//Color sample(const Surface* pTexture, FPInt u, FPInt v) const {return ( (pTexture->format == SF_8bit) ? sample8bit(pTexture, u, v) : sample32bit(pTexture, u, v) );}
	uint8_t sample8bit(const Surface* pTexture, FPInt u, FPInt v) const;
	Color sample32bit(const Surface* pTexture, FPInt u, FPInt v) const;
};

struct ClippingRectF
{
	float left, top, right, bottom;
	//FPInt left, top, right, bottom;
	//ClippingRect() : left(0), top(0), right(0), bottom(0) {}
};

template <class T_VertexType>
struct DrawingRange
{
	T_VertexType least, greatest;
	DrawingRange(const T_VertexType& Least, const T_VertexType& Greatest) : least(Least), greatest(Greatest) {}
};

class LineSegment
{
private:
	float m_slope;
	float m_dx;
	float m_dy;
	bool m_isFunctionOfX;
	union
	{
		float m_yIntercept;
		float m_xIntercept;
	};
	float m_leastX, m_leastY, m_greatestX, m_greatestY;
public:
	LineSegment() 
		: m_slope(0.0f), m_dx(0.0f), m_dy(0.0f), m_isFunctionOfX(false), m_yIntercept(0.0f), m_leastX(0.0f), m_leastY(0.0f), m_greatestX(0.0f), m_greatestY(0.0f) {}
	void calculateLineSegment(const Position& A, const Position& B);
	bool intersects(float* pIntersection, float YIntercept);

	float slope() const {return m_slope;}
	float dx() const {return m_dx;}
	float dy() const {return m_dy;}
	bool isFunctionOfX() const {return m_isFunctionOfX;}
	float xIntercept() const {return m_xIntercept;}
	float yIntercept() const {return m_yIntercept;}
	float leastX() const {return m_leastX;}
	float leastY() const {return m_leastY;}
	float greatestX() const {return m_greatestX;}
	float greatestY() const {return m_greatestY;}
};

class TriRasterizer
{
protected:
	Tex2DSampler m_sampler;
	template <class T_VertexType>
	void calculateTriangleRect(const T_VertexType* pTriangle, ClippingRectF& clipOut);
	template <class T_VertexType>
	void calculateRasterPixels(const Surface* pSurfaceDest, const T_VertexType* pTriangle, ClippingRectF& clipRgn, ClippingRectF& triangleRgn, std::queue< DrawingRange<T_VertexType> >& rasterLinesOut);
	void rasterColor(const DrawingRange<VertexPC>& range, Surface* pSurfaceDest);
	void rasterTexture(const DrawingRange<VertexPT>& range, const Surface* pTexture, const Palette* pPalette, Surface* pSurfaceDest);
	void rasterTextureWithColorKey0(const DrawingRange<VertexPT>& range, const Surface* pTexture, const Palette* pPalette, Surface* pSurfaceDest);
	void rasterTextureColor(const DrawingRange<VertexPTC>& range, const Surface* pTexture, const Palette* pPalette, Surface* pSurfaceDest);
	void rasterTextureColorWithColorKey0(const DrawingRange<VertexPTC>& range, const Surface* pTexture, const Palette* pPalette, Surface* pSurfaceDest);
public:
	void drawTriangleColor(VertexPC* pTriangle, Color argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest);
	void drawTriangleTexture(VertexPT* pTriangle, const Surface* pTexture, const Palette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest);
	void drawTriangleTextureColor(VertexPTC* pTriangle, const Surface* pTexture, const Palette* pPalette, int bUseColorKey0, Color argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest);
};

class QuadRasterizer : public TriRasterizer
{
protected:
	template <class T_VertexType>
	void generateTriangles(const T_VertexType* pQuad, T_VertexType* pTriangles);
public:
	void drawQuadColor(const VertexPC* pQuad, Color argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest);
	void drawQuadTexture(const VertexPT* pQuad, const Surface* pTexture, const Palette* pPalette, int bUseColorKey0, SurfaceRect* pRectDest, Surface* pSurfaceDest);
	void drawQuadTextureColor(const VertexPTC* pQuad, const Surface* pTexture, const Palette* pPalette, int bUseColorKey0, Color argbModifier, SurfaceRect* pRectDest, Surface* pSurfaceDest);
};

#endif
