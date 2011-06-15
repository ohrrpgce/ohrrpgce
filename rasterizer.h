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
	Color sample(const Surface* pSurface, FPInt x, FPInt y) const;
};

template <class T_VertexType>
struct Triangle
{
	T_VertexType pnt[3];
};

struct ClippingRect
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
	//void calculateTriangleRect(ClippingRect& clipOut, const Triangle* pTriangle);
	//void interpolateTexCoord(TexCoord& texOut, const TexCoord& t1, const TexCoord& t2, float scale);
	//Color interpolateColor(Color c1, Color c2, float scale);
	template <class T_VertexType>
	void calculateRasterPixels(std::queue< DrawingRange<T_VertexType> >& rasterLinesOut, const Surface* pSurface, const Triangle<T_VertexType>* pTriangle, ClippingRect& clip);
	void rasterColor(Surface* pSurfaceDest, const DrawingRange<VertexC>& range, Color argbModifier);
	void rasterTexture(Surface* pSurfaceDest, const DrawingRange<VertexT>& range, const Surface* pTexture, const Palette* pPalette, Color argbModifier);
	void rasterTextureWithColorKey(Surface* pSurfaceDest, const DrawingRange<VertexT>& range, const Surface* pTexture, const Palette* pPalette, unsigned char colorKey, Color argbModifier);
public:
	void drawColor(Surface* pSurface, SurfaceRect* pRect, const Triangle<VertexC>* pTriangle, Color argbModifier);
	void drawTexture(Surface* pSurface, SurfaceRect* pRect, const Triangle<VertexT>* pTriangle, const Surface* pTexture, const Palette* pPalette, Color argbModifier);
	void drawTextureWithColorKey(Surface* pSurface, SurfaceRect* pRect, const Triangle<VertexT>* pTriangle, const Surface* pTexture, const Palette* pPalette, unsigned char colorKey, Color argbModifier);
};

class QuadRasterizer
{
protected:
	TriRasterizer m_triRasterizer;
	void generateTriangles(Triangle<VertexC>* pTriangles, const QuadC* pQuad);
	void generateTriangles(Triangle<VertexT>* pTriangles, const QuadT* pQuad);
public:
	void drawColor(Surface* pSurface, SurfaceRect* pRect, const QuadC* pQuad, Color col)
	{
		if(pSurface == NULL || pQuad == NULL)
			return;
		Triangle<VertexC> triangles[4];
		generateTriangles(triangles, pQuad);
		for(int i = 0; i < 4; i++)
			m_triRasterizer.drawColor(pSurface, pRect, &triangles[i], col);
	}
	void drawTexture(Surface* pSurface, SurfaceRect* pRect, const QuadT* pQuad, const Surface* pTexture, const Palette* pPalette, Color argbModifier)
	{
		if(pSurface == NULL || pQuad == NULL)
			return;
		Triangle<VertexT> triangles[4];
		generateTriangles(triangles, pQuad);
		for(int i = 0; i < 4; i++)
			m_triRasterizer.drawTexture(pSurface, pRect, &triangles[i], pTexture, pPalette, argbModifier);
	}
	void drawTextureWithColorKey(Surface* pSurface, SurfaceRect* pRect, const QuadT* pQuad, const Surface* pTexture, const Palette* pPalette, unsigned char colorKey, Color argbModifier)
	{
		if(pSurface == NULL || pQuad == NULL)
			return;
		Triangle<VertexT> triangles[4];
		generateTriangles(triangles, pQuad);
		for(int i = 0; i < 4; i++)
			m_triRasterizer.drawTextureWithColorKey(pSurface, pRect, &triangles[i], pTexture, pPalette, colorKey, argbModifier);
	}
};

#endif