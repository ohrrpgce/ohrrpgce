/* OHRRPGCE - software 3D rasterizer
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * This 3D triangle and quad rasterizer was written by Jay Tennant.
 */

#include <stdlib.h>
#include <cmath>
#include "rasterizer.hpp"
#include "errorlog.h"

using std::abs;
using std::max;
using std::min;


uint8_t Tex2DSampler::sample8bit(const Surface* pTexture, FPInt u, FPInt v) const
{
	FPInt minuteScale;
	minuteScale.fraction = 0xffff; //same as 65535/65536

	u *= /*0.99999f;*/minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	u.whole = 0; //remove all whole numbers and negative references, keeping fraction
	u *= pTexture->width;//FPInt(pTexture->width); //scale from (0.0)-(0.9999...) to (0)-(texture.width-1)

	v *= /*0.99999f;*/minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	v.whole = 0; //remove all whole numbers and negative references, keeping fraction
	v *= pTexture->height;//FPInt(pTexture->height); //scale from (0.0)-(0.9999...) to (0)-(texture.width-1)

	return pTexture->pPaletteData[v.whole * pTexture->pitch + u.whole];
}
Color Tex2DSampler::sample32bit(const Surface* pTexture, FPInt u, FPInt v) const
{
	FPInt minuteScale;
	minuteScale.fraction = 0xffff; //same as 65535/65536

	u *= /*0.99999f;*/minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	u.whole = 0; //remove all whole numbers and negative references, keeping fraction
	u *= pTexture->width;//FPInt(pTexture->width); //scale from (0.0)-(0.9999...) to (0)-(texture.width-1)

	v *= /*0.99999f;*/minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	v.whole = 0; //remove all whole numbers and negative references, keeping fraction
	v *= pTexture->height;//FPInt(pTexture->height); //scale from (0.0)-(0.9999...) to (0)-(texture.width-1)

	return pTexture->pColorData[v.whole * pTexture->pitch + u.whole];
}



void LineSegment::calculateLineSegment(const Position &A, const Position &B)
{
	m_dx = A.x - B.x;
	m_dy = A.y - B.y;

	if(abs(m_dx) > abs(m_dy))
	{
		m_isFunctionOfX = true;
		m_slope = m_dy / m_dx;
		m_yIntercept = A.y - m_slope * A.x;
	}
	else
	{
		m_isFunctionOfX = false;
		m_slope = m_dx / m_dy;
		m_xIntercept = A.x - m_slope * A.y;
	}
	m_leastX    = min(A.x, B.x);
	m_greatestX = max(A.x, B.x);
	m_leastY    = min(A.y, B.y);
	m_greatestY = max(A.y, B.y);
}

bool LineSegment::intersects(float *pIntersection, float YIntercept)
{
	if(YIntercept > m_greatestY || YIntercept < m_leastY)
		return false;

	if(pIntersection == 0)
		return true;

	if(m_isFunctionOfX)
	{
		if(m_slope == 0.0f || m_slope == -0.0f)
			*pIntersection = m_leastX;
		else
			*pIntersection = (YIntercept - m_yIntercept) / m_slope;
	}
	else
		*pIntersection = m_slope * YIntercept + m_xIntercept;

	return true;
}

// Calculate the bounding box
template <class T_VertexType>
void TriRasterizer::calculateTriangleRect(const T_VertexType* pTriangle, ClippingRectF &clipOut)
{
	clipOut.left   = min(min(pTriangle[0].pos.x, pTriangle[1].pos.x), pTriangle[2].pos.x);
	clipOut.top    = min(min(pTriangle[0].pos.y, pTriangle[1].pos.y), pTriangle[2].pos.y);
	clipOut.right  = max(max(pTriangle[0].pos.x, pTriangle[1].pos.x), pTriangle[2].pos.x);
	clipOut.bottom = max(max(pTriangle[0].pos.y, pTriangle[1].pos.y), pTriangle[2].pos.y);
}

template <class T_VertexType>
void TriRasterizer::calculateRasterPixels(const Surface* pSurfaceDest, const T_VertexType* pTriangle, ClippingRectF& clipRgn, ClippingRectF& triangleRgn, std::queue< DrawingRange<T_VertexType> >& rasterLinesOut)
{//done
	//calculate the edge lines of the triangle
	LineSegment segments[3];
	for (int i = 0; i < 3; i++)
		segments[i].calculateLineSegment(pTriangle[i].pos, pTriangle[(i+1)%3].pos);

	//find the left and right boundaries of each raster line
	float xIntersection[3];
	float leftMost, rightMost;
	int leftMostIndex, rightMostIndex;
	float scale;
	T_VertexType leftVertex, rightVertex;
	int row = max(clipRgn.top, triangleRgn.top);
	int rowEnd = min(clipRgn.bottom, triangleRgn.bottom);
	for (; row <= rowEnd; row++) {
		leftMost = triangleRgn.right + 1.0f;
		leftMostIndex = -1;
		rightMost = triangleRgn.left - 1.0f;
		rightMostIndex = -1;
		for (int i = 0; i < 3; i++) {
			if(!segments[i].intersects(&xIntersection[i], row))
				continue;
			if(xIntersection[i] < triangleRgn.left-.5f || xIntersection[i] > triangleRgn.right+.5f)
				continue;
			if(xIntersection[i] < leftMost)
			{
				leftMost = xIntersection[i];
				leftMostIndex = i;
			}
			if(xIntersection[i] > rightMost)
			{
				rightMost = xIntersection[i];
				rightMostIndex = i;
			}
		}

		if(leftMostIndex == -1 || rightMostIndex == -1)
			continue;

		//interpolate vertex data for each line
		if(segments[leftMostIndex].isFunctionOfX())
		{
			if(segments[leftMostIndex].dx() == 0.0f || segments[leftMostIndex].dx() == -0.0f)
				scale = 1.0f;
			else
				scale = (xIntersection[leftMostIndex] - pTriangle[(leftMostIndex+1)%3].pos.x) / segments[leftMostIndex].dx();
		}
		else
		{
			if(segments[leftMostIndex].dy() == 0.0f || segments[leftMostIndex].dy() == -0.0f)
				scale = 1.0f;
			else
				scale = (row - pTriangle[(leftMostIndex+1)%3].pos.y) / segments[leftMostIndex].dy();
		}
		leftVertex = pTriangle[leftMostIndex];
		leftVertex.interpolateComponents( pTriangle[(leftMostIndex+1)%3], scale );
		leftVertex.pos.x = xIntersection[leftMostIndex];
		leftVertex.pos.y = row;

		if(segments[rightMostIndex].isFunctionOfX())
		{
			if(segments[rightMostIndex].dx() == 0.0f || segments[rightMostIndex].dx() == -0.0f)
				scale = 1.0f;
			else
				scale = (xIntersection[rightMostIndex] - pTriangle[(rightMostIndex+1)%3].pos.x) / segments[rightMostIndex].dx();
		}
		else
		{
			if(segments[rightMostIndex].dy() == 0.0f || segments[rightMostIndex].dy() == -0.0f)
				scale = 1.0f;
			else
				scale = (row - pTriangle[(rightMostIndex+1)%3].pos.y) / segments[rightMostIndex].dy();
		}
		rightVertex = pTriangle[rightMostIndex];
		rightVertex.interpolateComponents( pTriangle[(rightMostIndex+1)%3], scale );
		rightVertex.pos.x = xIntersection[rightMostIndex];
		rightVertex.pos.y = row;

		//perform horizontal clipping
		if(leftVertex.pos.x > clipRgn.right || rightVertex.pos.x < clipRgn.left)
			continue;
		
		if(leftVertex.pos.x < clipRgn.left)
		{
			scale = (clipRgn.left - leftVertex.pos.x) / (rightVertex.pos.x - leftVertex.pos.x);
			leftVertex.interpolateComponents( rightVertex, 1-scale );
			leftVertex.pos.x = clipRgn.left;
			//leftVertex.pos.y = row;
		}
		if(rightVertex.pos.x > clipRgn.right)
		{
			scale = (clipRgn.right - rightVertex.pos.x) / (leftVertex.pos.x - rightVertex.pos.x);
			rightVertex.interpolateComponents( leftVertex, 1-scale );
			rightVertex.pos.x = clipRgn.right;
			//rightVertex.pos.y = row;
		}

		//push the data onto the raster queue
		rasterLinesOut.push( DrawingRange<T_VertexType>(leftVertex, rightVertex) );
	}
}

template <class T_VertexType>
inline TexCoord texel_coord(const DrawingRange<T_VertexType> &range, int x, float length, float &weightFirst)
{
	float weightSecond;
	weightFirst = (range.greatest.pos.x - x) / length;
	weightSecond = 1 - weightFirst;
	TexCoord texel;
	texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
	texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;
	return texel;
}

inline Color blend_colors(Color srcColor, Color destColor)
{
	Color finalColor;
	//floating point method of blending
	//red =	( (float)srcColor.r * (float)srcColor.a + (float)destColor.r * (255.0f - (float)srcColor.a) ) / 255.0f;
	//green =	( (float)srcColor.g * (float)srcColor.a + (float)destColor.g * (255.0f - (float)srcColor.a) ) / 255.0f;
	//blue =	( (float)srcColor.b * (float)srcColor.a + (float)destColor.b * (255.0f - (float)srcColor.a) ) / 255.0f;
	//finalColor.r = (int)red;
	//finalColor.g = (int)green;
	//finalColor.b = (int)blue;

	//integer method of blending
	finalColor.r = ( srcColor.r * srcColor.a + destColor.r * (255 - srcColor.a) ) / 255;//(int)red;
	finalColor.g = ( srcColor.g * srcColor.a + destColor.g * (255 - srcColor.a) ) / 255;//(int)green;
	finalColor.b = ( srcColor.b * srcColor.a + destColor.b * (255 - srcColor.a) ) / 255;//(int)blue;

	//failed attempt at combining integer method of blending
	//finalColor.dw = ( (srcColor.dw & 0x00ffffff) * srcColor.a + (destColor.dw & 0x00ffffff) * (255-srcColor.a) ) / 255;

	finalColor.a = (srcColor.a + destColor.a) > 255 ? 255 : (srcColor.a + destColor.a);
	return finalColor;
}

void TriRasterizer::rasterColor(const DrawingRange<VertexPC> &range, Surface *pSurfaceDest)
{//done
	float length = range.greatest.pos.x - range.least.pos.x + 1.0f;
	float weight;

	int start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	int finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor;
	uint32_t *pDest = &pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->pitch + start];

	for (int i = start; i <= finish; i++, pDest++) {
		weight = 255.0f*(finish-i) / length;
		srcColor = range.least.col;
		srcColor.scale(range.greatest.col, weight);
		destColor = *pDest;
		*pDest = blend_colors(srcColor, destColor);
	}
}

void TriRasterizer::rasterTexture(const DrawingRange<VertexPT> &range, const Surface *pTexture, const RGBPalette *pPalette, Surface *pSurfaceDest)
{//done
	//assumed that if source is 8bit, a palette was passed in

	float length = range.greatest.pos.x - range.least.pos.x + 1.0f;
	float weightFirst;

	int start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	int finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor;
	uint32_t *pDest = &pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->pitch + start];

	bool is_32bit = (pTexture->format == SF_32bit);

	for (int i = start; i <= finish; i++, pDest++) {
		TexCoord texel = texel_coord(range, i, length, weightFirst);

		if (is_32bit)
			srcColor = m_sampler.sample32bit(pTexture, texel.u, texel.v);
		else
			srcColor = Color(pPalette->col[ m_sampler.sample8bit(pTexture, texel.u, texel.v) ]);

		destColor = *pDest;
		*pDest = blend_colors(srcColor, destColor);
	}
}

void TriRasterizer::rasterTextureWithColorKey0(const DrawingRange<VertexPT> &range, const Surface *pTexture, const RGBPalette *pPalette, Surface *pSurfaceDest)
{//done
	//assumes that if source is 8bit, a palette was passed in
	if (pTexture->format == SF_32bit)
		return;  //BUG, should never be called

	float length = range.greatest.pos.x - range.least.pos.x + 1.0f;
	float weightFirst;

	int start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	int finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor;
	uint32_t *pDest = &pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->pitch + start];

	for (int i = start; i <= finish; i++, pDest++) {
		TexCoord texel = texel_coord(range, i, length, weightFirst);

		uint8_t colorKey = m_sampler.sample8bit(pTexture, texel.u, texel.v);
		if (!colorKey)
			continue;
		srcColor = Color(pPalette->col[colorKey]);

		destColor = *pDest;
		*pDest = blend_colors(srcColor, destColor);
	}
}

void TriRasterizer::rasterTextureColor(const DrawingRange<VertexPTC> &range, const Surface *pTexture, const RGBPalette *pPalette, Surface *pSurfaceDest)
{//done
	//assumed that if source is 8bit, a palette was passed in

	float length = range.greatest.pos.x - range.least.pos.x + 1.0f;
	float weightFirst;

	int start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	int finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor, vertexColor;
	uint32_t *pDest = &pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->pitch + start];

	bool is_32bit = (pTexture->format == SF_32bit);

	for (int i = start; i <= finish; i++, pDest++) {
		TexCoord texel = texel_coord(range, i, length, weightFirst);

		if (is_32bit)
			srcColor = m_sampler.sample32bit(pTexture, texel.u, texel.v);
		else
			srcColor = Color(pPalette->col[ m_sampler.sample8bit(pTexture, texel.u, texel.v) ]);

		vertexColor = range.least.col;
		vertexColor.scale(range.greatest.col, 255.0f * weightFirst);
		srcColor.scale(vertexColor);

		destColor = *pDest;
		*pDest = blend_colors(srcColor, destColor);
	}
}

void TriRasterizer::rasterTextureColorWithColorKey0(const DrawingRange<VertexPTC> &range, const Surface *pTexture, const RGBPalette *pPalette, Surface *pSurfaceDest)
{//done
	//assumed that if source is 8bit, a palette was passed in
	if (pTexture->format == SF_32bit)
		return;  //BUG, should never be called

	float length = range.greatest.pos.x - range.least.pos.x + 1.0f;
	float weightFirst;

	int start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	int finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor, vertexColor;
	uint32_t *pDest = &pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->pitch + start];

	for (int i = start; i <= finish; i++, pDest++) {
		TexCoord texel = texel_coord(range, i, length, weightFirst);

		uint8_t colorKey = m_sampler.sample8bit(pTexture, texel.u, texel.v);
		if (!colorKey)
			continue;
		srcColor = Color(pPalette->col[colorKey]);

		vertexColor = range.least.col;
		vertexColor.scale(range.greatest.col, 255.0f * weightFirst);
		srcColor.scale(vertexColor);

		destColor = *pDest;
		*pDest = blend_colors(srcColor, destColor);
	}
}


//Returns false if the entire draw is clipped
template <class T_VertexType>
bool TriRasterizer::drawSetup(T_VertexType *pTriangle, SurfaceRect *pRectDest, Surface *pSurfaceDest, std::queue< DrawingRange< T_VertexType > > &rasterLines)
{
	//determine rasterizing region
	ClippingRectF clipRgn = {(float)pRectDest->left, (float)pRectDest->top, (float)pRectDest->right, (float)pRectDest->bottom};
	if(clipRgn.left < 0.0f) clipRgn.left = 0.0f;
	if(clipRgn.top < 0.0f) clipRgn.top = 0.0f;
	if(clipRgn.right >= pSurfaceDest->width) clipRgn.right = pSurfaceDest->width-1;
	if(clipRgn.bottom >= pSurfaceDest->height) clipRgn.bottom = pSurfaceDest->width-1;

	ClippingRectF triangleRgn;
	calculateTriangleRect(pTriangle, triangleRgn);

	//test whether triangle is inside clipping region at all
	if(triangleRgn.left > clipRgn.right || triangleRgn.right < clipRgn.left || triangleRgn.top > clipRgn.bottom || triangleRgn.bottom < clipRgn.top)
		return false;

	calculateRasterPixels(pSurfaceDest, pTriangle, clipRgn, triangleRgn, rasterLines);

	return true;
}

void TriRasterizer::drawTriangleColor(VertexPC *pTriangle, Color argbModifier, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if(pSurfaceDest == NULL || pTriangle == NULL)
		return;

	std::queue< DrawingRange< VertexPC > > rasterLines;
	if (!drawSetup(pTriangle, pRectDest, pSurfaceDest, rasterLines))
		return;

	//apply color modifier
	for (int i = 0; i < 3; i++)
		pTriangle[i].col.scale(argbModifier);

	//rasterize the polygon
	while (!rasterLines.empty()) {
		rasterColor(rasterLines.front(), pSurfaceDest);
		rasterLines.pop();
	}
}

void TriRasterizer::drawTriangleTexture(VertexPT *pTriangle, const Surface *pTexture, const RGBPalette *pPalette, int bUseColorKey0, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if(pSurfaceDest == NULL || pTriangle == NULL || pTexture == NULL)
		return;

	//palette check
	if(pTexture->format == SF_8bit && !pPalette)
		return; //need a palette to convert

	if(pTexture->format == SF_32bit && bUseColorKey0)
		return; //Not supported

	std::queue< DrawingRange< VertexPT > > rasterLines;
	if (!drawSetup(pTriangle, pRectDest, pSurfaceDest, rasterLines))
		return;

	//rasterize the polygon
	while (!rasterLines.empty()) {
		if(bUseColorKey0)
			rasterTextureWithColorKey0(rasterLines.front(), pTexture, pPalette, pSurfaceDest);
		else
			rasterTexture(rasterLines.front(), pTexture, pPalette, pSurfaceDest);
		rasterLines.pop();
	}
}

void TriRasterizer::drawTriangleTextureColor(VertexPTC *pTriangle, const Surface *pTexture, const RGBPalette *pPalette, int bUseColorKey0, Color argbModifier, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if(pSurfaceDest == NULL || pTriangle == NULL || pTexture == NULL)
		return;

	//palette check
	if(pTexture->format == SF_8bit && !pPalette)
		return; //need a palette to convert

	if(pTexture->format == SF_32bit && bUseColorKey0)
		return; //Not supported

	std::queue< DrawingRange< VertexPTC > > rasterLines;
	if (!drawSetup(pTriangle, pRectDest, pSurfaceDest, rasterLines))
		return;

	//apply color modifier
	for (int i = 0; i < 3; i++)
		pTriangle[i].col.scale(argbModifier);

	//rasterize the polygon
	while (!rasterLines.empty()) {
		if(bUseColorKey0)
			rasterTextureColorWithColorKey0(rasterLines.front(), pTexture, pPalette, pSurfaceDest);
		else
			rasterTextureColor(rasterLines.front(), pTexture, pPalette, pSurfaceDest);
		rasterLines.pop();
	}
}



template <class T_VertexType>
void QuadRasterizer::generateTriangles(const T_VertexType* pQuad, T_VertexType* pTriangles)
{//done
	T_VertexType center1, center2;

	center1 = pQuad[0];
	center2 = pQuad[2];

	center1.interpolateComponents(pQuad[1], .5f);
	center2.interpolateComponents(pQuad[3], .5f);
	center1.interpolateComponents(center2, .5f);

	for (int i = 0; i < 4; i++) {
		pTriangles[i*3 + 0] = pQuad[i];
		pTriangles[i*3 + 1] = pQuad[(i+1)%4];
		pTriangles[i*3 + 2] = center1;
	}
}

void QuadRasterizer::drawQuadColor(const VertexPC *pQuad, Color argbModifier, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if( pQuad == NULL )
		return;
	VertexPC triangles[4*3];
	generateTriangles(pQuad, triangles);

	for (int i = 0; i < 4; i++)
		drawTriangleColor(&triangles[i*3], argbModifier, pRectDest, pSurfaceDest);
}

void QuadRasterizer::drawQuadTexture(const VertexPT *pQuad, const Surface *pTexture, const RGBPalette *pPalette, int bUseColorKey0, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if( pQuad == NULL )
		return;
	VertexPT triangles[4*3];
	generateTriangles(pQuad, triangles);

	for (int i = 0; i < 4; i++)
		drawTriangleTexture(&triangles[i*3], pTexture, pPalette, bUseColorKey0, pRectDest, pSurfaceDest);
}

void QuadRasterizer::drawQuadTextureColor(const VertexPTC *pQuad, const Surface *pTexture, const RGBPalette *pPalette, int bUseColorKey0, Color argbModifier, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if( pQuad == NULL )
		return;
	VertexPTC triangles[4*3];
	generateTriangles(pQuad, triangles);

	for (int i = 0; i < 4; i++)
		drawTriangleTextureColor(&triangles[i*3], pTexture, pPalette, bUseColorKey0, argbModifier, pRectDest, pSurfaceDest);
}
