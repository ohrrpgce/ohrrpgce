/* OHRRPGCE - software 3D rasterizer private API
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * By Jay Tennant 5/30/11
 * Triangle and quad rasterization on software surfaces.
 * See gfxRender.hpp for the public rasterizer API.
 */

#ifndef RASTERIZER_H
#define RASTERIZER_H

#include "fpInt.hpp"
#include "surface.h"
#include "gfxRender.hpp"
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
	typename T_VertexType::IncType least, greatest;
	DrawingRange(const typename T_VertexType::IncType& Least, const typename T_VertexType::IncType& Greatest) : least(Least), greatest(Greatest) {}
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
	template <class T_VertexType>
	bool drawSetup(T_VertexType *pTriangle, SurfaceRect *pRectDest, Surface *pSurfaceDest, std::queue< DrawingRange< T_VertexType > > &rasterLines, DrawOptions &opts, int &alpha);
	void rasterColor(const DrawingRange<VertexPC>& range, Surface* pSurfaceDest, DrawOptions* pOpts);
	void rasterTexture(const DrawingRange<VertexPT>& range, const Surface* pTexture, const RGBPalette* pPalette, Surface* pSurfaceDest, DrawOptions* pOpts, int alpha);
	void rasterTextureColor(const DrawingRange<VertexPTC>& range, const Surface* pTexture, const RGBPalette* pPalette, Surface* pSurfaceDest, DrawOptions* pOpts);
public:
	void drawTriangleColor(VertexPC* pTriangle, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts);
	void drawTriangleTexture(VertexPT* pTriangle, const Surface* pTexture, const RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts);
	void drawTriangleTextureColor(VertexPTC* pTriangle, const Surface* pTexture, const RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts);
};

// Note: QuadRasterizer should not have state, so that g_rasterizer global is threadsafe
class QuadRasterizer : public TriRasterizer
{
protected:
	template <class T_VertexType>
	void generateTriangles(const T_VertexType* pQuad, T_VertexType* pTriangles);
public:
	void drawQuadColor(const VertexPC* pQuad, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts);
	void drawQuadTexture(const VertexPT* pQuad, const Surface* pTexture, const RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts);
	void drawQuadTextureColor(const VertexPTC* pQuad, const Surface* pTexture, const RGBPalette* pPalette, SurfaceRect* pRectDest, Surface* pSurfaceDest, DrawOptions* pOpts);
};

#endif
