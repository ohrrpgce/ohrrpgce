/* OHRRPGCE - software 3D rasterizer
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * This 3D triangle and quad rasterizer was written by Jay Tennant.
 */

#include <stdlib.h>
#include <cmath>
#include "rasterizer.hpp"
#include "blend.h"
#include "errorlog.h"

using std::abs;
using std::max;
using std::min;


uint8_t Tex2DSampler::sample8bit(const Surface* pTexture, FPInt u, FPInt v) const
{
	// Keep fractional part only; this tiles the texture across the polygon
	u.whole = 0;
	// Scale from real-valued [0, 1) to integer-valued [0-texture.width)
	u *= pTexture->width;
	v.whole = 0;
	v *= pTexture->height;
	return pTexture->pPaletteData[v.whole * pTexture->pitch + u.whole];
}
Color Tex2DSampler::sample32bit(const Surface* pTexture, FPInt u, FPInt v) const
{
	u.whole = 0;
	u *= pTexture->width;
	v.whole = 0;
	v *= pTexture->height;
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
	if(YIntercept >= m_greatestY || YIntercept < m_leastY)
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
{
	//calculate the edge lines of the triangle
	LineSegment segments[3];
	for (int i = 0; i < 3; i++)
		segments[i].calculateLineSegment(pTriangle[i].pos, pTriangle[(i+1)%3].pos);

	//find the left and right boundaries of each raster line
	float xIntersection[3];
	float leftMost, rightMost;
	int leftMostIndex, rightMostIndex;
	float scale;
	typename T_VertexType::IncType leftVertex, rightVertex;
	// Each integral y value in the half-open interval [top, bottom)
	int row = max(clipRgn.top, triangleRgn.top);
	int rowEnd = (int)ceil(min(clipRgn.bottom, triangleRgn.bottom)) - 1;
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
		// It's necessary that U/V 0.0 is the first texel in the texture but U/V 1.0 is the
		// last one rather than wrapping, because the polygon edge may fall directly on a pixel
		// center, so scale U/V by 0.9999. This seems like an unavoidable hack.
		leftVertex.scaleDownUV();

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
		rightVertex.scaleDownUV();

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
inline void calculateRangeIncrements(const DrawingRange<T_VertexType> &range, int &start, int &finish, typename T_VertexType::IncType &point, typename T_VertexType::IncType &pointInc)
{
	float length = range.greatest.pos.x - range.least.pos.x;
	if (length <= 0) length = 1.;

	// X range [start, finish] of pixels to render (inclusive), selecting
	// each if its center is in the half-open range [least, greatest).
	// Already clamped to the clip region, don't need to repeat that
	start = (int)ceil(range.least.pos.x - .5);
	finish = (int)ceil(range.greatest.pos.x - .5) - 1;

	// Note: point.pos won't actually be used for anything. Hopefully the compiler optimises it away
	float startCenter = (start + .5) - range.least.pos.x;
	point = range.greatest;
	point.interpolateComponents(range.least, startCenter / length);

	// Equivalent to pointInc = (range.greatest - range.least) / length
	pointInc = range.greatest - range.least;
	typename T_VertexType::IncType zero = {0};
	pointInc.interpolateComponents(zero, 1. / length);
}


void TriRasterizer::rasterColor(const DrawingRange<VertexPC> &range, Surface *pSurfaceDest, DrawOptions *pOpts)
{
	int start, finish;
	VertexPC::IncType point, pointInc;
	calculateRangeIncrements(range, start, finish, point, pointInc);
	uint32_t *pDest = &pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->pitch + start];

	for (int i = start; i <= finish; i++, pDest++, point += pointInc) {
		Color srcColor, destColor;
		srcColor = point.col;

		if (pOpts->with_blending) {
			destColor = *pDest;
			// alpha is always 256 because already multiplied into srcColor.a
			*pDest = alpha_blend(srcColor, destColor, 256, pOpts->blend_mode, pOpts->alpha_channel).col;
		} else {
			*pDest = srcColor;
		}
	}
}

//Assumes that if pTexture is 8bit a palette is passed in
void TriRasterizer::rasterTexture(const DrawingRange<VertexPT> &range, const Surface *pTexture, const RGBPalette *pPalette, Surface *pSurfaceDest, DrawOptions *pOpts, int alpha)
{
	int start, finish;
	VertexPT::IncType point, pointInc;
	calculateRangeIncrements(range, start, finish, point, pointInc);
	uint32_t *pDest = &pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->pitch + start];

	bool is_32bit = (pTexture->format == SF_32bit);
	int colorKey = pOpts->color_key0 ? 0 : -1;

	for (int i = start; i <= finish; i++, pDest++, point += pointInc) {
		Color srcColor, destColor;
		if (is_32bit)
			srcColor = m_sampler.sample32bit(pTexture, point.tex.u, point.tex.v);
		else {
			uint8_t index = m_sampler.sample8bit(pTexture, point.tex.u, point.tex.v);
			if (index == colorKey)
				continue;
			srcColor = Color(pPalette->col[index]);
		}

		if (pOpts->with_blending) {
			destColor = *pDest;
			*pDest = alpha_blend(srcColor, destColor, alpha, pOpts->blend_mode, pOpts->alpha_channel).col;
		} else {
			*pDest = srcColor;
		}
	}
}

//Assumes that if source is 8bit a palette is passed in
void TriRasterizer::rasterTextureColor(const DrawingRange<VertexPTC> &range, const Surface *pTexture, const RGBPalette *pPalette, Surface *pSurfaceDest, DrawOptions *pOpts)
{
	int start, finish;
	VertexPTC::IncType point, pointInc;
	calculateRangeIncrements(range, start, finish, point, pointInc);
	uint32_t *pDest = &pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->pitch + start];

	bool is_32bit = (pTexture->format == SF_32bit);
	int colorKey = pOpts->color_key0 ? 0 : -1;

	for (int i = start; i <= finish; i++, pDest++, point += pointInc) {
		Color srcColor, destColor;
		if (is_32bit)
			srcColor = m_sampler.sample32bit(pTexture, point.tex.u, point.tex.v);
		else {
			uint8_t index = m_sampler.sample8bit(pTexture, point.tex.u, point.tex.v);
			if (index == colorKey)
				continue;
			srcColor = Color(pPalette->col[index]);
		}

		srcColor.scale(point.col);

		if (pOpts->with_blending) {
			destColor = *pDest;
			// alpha is always 256 because the global alpha (pOpts->opacity) is already
			// multiplied into the vertex alphas which is multipled into srcColor
			*pDest = alpha_blend(srcColor, destColor, 256, pOpts->blend_mode, pOpts->alpha_channel).col;
		} else {
			*pDest = srcColor;
		}
	}
}


//Returns false if the entire draw is clipped
template <class T_VertexType>
bool TriRasterizer::drawSetup(T_VertexType *pTriangle, SurfaceRect *pRectDest, Surface *pSurfaceDest, std::queue< DrawingRange< T_VertexType > > &rasterLines, DrawOptions &opts, int &alpha)
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

	alpha = 256;
	//bool with_blending = opts.with_blending;
	if (opts.with_blending) {
		alpha = min(256, (int)(opts.opacity * 256));
		if (opts.opacity <= 0. || opts.argbModifier.a == 0)
			return false;  //TODO: remove this if write_mask is implemented!
		if (opts.opacity >= 1. && opts.blend_mode == blendModeNormal && !opts.alpha_channel)
			opts.with_blending = false;
	}

	calculateRasterPixels(pSurfaceDest, pTriangle, clipRgn, triangleRgn, rasterLines);

	return true;
}

void TriRasterizer::drawTriangleColor(VertexPC *pTriangle, SurfaceRect *pRectDest, Surface *pSurfaceDest, DrawOptions *pOpts)
{
	if(pSurfaceDest == NULL || pTriangle == NULL || pOpts == NULL)
		return;

	DrawOptions opts = *pOpts;  //Local modifications
	int alpha;
	std::queue< DrawingRange< VertexPC > > rasterLines;
	if (!drawSetup(pTriangle, pRectDest, pSurfaceDest, rasterLines, opts, alpha))
		return;

	//apply color modifier
	Color argbModifier = opts.argbModifier;
	argbModifier.a *= opts.opacity;  //Deal with the redundancy
	for (int i = 0; i < 3; i++)
		pTriangle[i].col.scale(argbModifier);

	if ((pTriangle[0].col.a & pTriangle[1].col.a & pTriangle[2].col.a) == 255)
		//No use of alpha channel, nor opts.opacity nor opts.argbModifier.a
		opts.alpha_channel = false;

	//rasterize the polygon
	while (!rasterLines.empty()) {
		rasterColor(rasterLines.front(), pSurfaceDest, &opts);
		rasterLines.pop();
	}
}

void TriRasterizer::drawTriangleTexture(VertexPT *pTriangle, const Surface *pTexture, const RGBPalette *pPalette, SurfaceRect *pRectDest, Surface *pSurfaceDest, DrawOptions *pOpts)
{
	if(pSurfaceDest == NULL || pTriangle == NULL || pTexture == NULL || pOpts == NULL)
		return;

	if(pTexture->format == SF_8bit && !pPalette)
		return; //need a palette to convert

	if(pTexture->format == SF_32bit && pOpts->color_key0)
		return; //Not supported

	DrawOptions opts = *pOpts;  //Local modifications
	int alpha;
	std::queue< DrawingRange< VertexPT > > rasterLines;
	if (!drawSetup(pTriangle, pRectDest, pSurfaceDest, rasterLines, opts, alpha))
		return;

	//opts.argbModifier is not supported, drawTriangleTextureColor must be used for it.

	//rasterize the polygon
	while (!rasterLines.empty()) {
		rasterTexture(rasterLines.front(), pTexture, pPalette, pSurfaceDest, &opts, alpha);
		rasterLines.pop();
	}
}

void TriRasterizer::drawTriangleTextureColor(VertexPTC *pTriangle, const Surface *pTexture, const RGBPalette *pPalette, SurfaceRect *pRectDest, Surface *pSurfaceDest, DrawOptions *pOpts)
{
	if(pSurfaceDest == NULL || pTriangle == NULL || pTexture == NULL || pOpts == NULL)
		return;

	if(pTexture->format == SF_8bit && !pPalette)
		return; //need a palette to convert

	if(pTexture->format == SF_32bit && pOpts->color_key0)
		return; //Not supported

	DrawOptions opts = *pOpts;  //Local modifications
	int alpha;
	std::queue< DrawingRange< VertexPTC > > rasterLines;
	if (!drawSetup(pTriangle, pRectDest, pSurfaceDest, rasterLines, opts, alpha))
		return;

	//apply color modifier
	Color argbModifier = opts.argbModifier;
	argbModifier.a *= opts.opacity;  //Deal with the redundancy
	for (int i = 0; i < 3; i++)
		pTriangle[i].col.scale(argbModifier);

	//rasterize the polygon
	while (!rasterLines.empty()) {
		rasterTextureColor(rasterLines.front(), pTexture, pPalette, pSurfaceDest, &opts);
		rasterLines.pop();
	}
}



template <class T_VertexType>
void QuadRasterizer::generateTriangles(const T_VertexType* pQuad, T_VertexType* pTriangles)
{
	typename T_VertexType::IncType center1, center2;

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

void QuadRasterizer::drawQuadColor(const VertexPC *pQuad, SurfaceRect *pRectDest, Surface *pSurfaceDest, DrawOptions *pOpts)
{
	if( pQuad == NULL )
		return;
	VertexPC triangles[4*3];
	generateTriangles(pQuad, triangles);

	for (int i = 0; i < 4; i++)
		drawTriangleColor(&triangles[i*3], pRectDest, pSurfaceDest, pOpts);
}

void QuadRasterizer::drawQuadTexture(const VertexPT *pQuad, const Surface *pTexture, const RGBPalette *pPalette, SurfaceRect *pRectDest, Surface *pSurfaceDest, DrawOptions *pOpts)
{
	if( pQuad == NULL )
		return;
	VertexPT triangles[4*3];
	generateTriangles(pQuad, triangles);

	for (int i = 0; i < 4; i++)
		drawTriangleTexture(&triangles[i*3], pTexture, pPalette, pRectDest, pSurfaceDest, pOpts);
}

void QuadRasterizer::drawQuadTextureColor(const VertexPTC *pQuad, const Surface *pTexture, const RGBPalette *pPalette, SurfaceRect *pRectDest, Surface *pSurfaceDest, DrawOptions *pOpts)
{
	if( pQuad == NULL )
		return;
	VertexPTC triangles[4*3];
	generateTriangles(pQuad, triangles);

	for (int i = 0; i < 4; i++)
		drawTriangleTextureColor(&triangles[i*3], pTexture, pPalette, pRectDest, pSurfaceDest, pOpts);
}
