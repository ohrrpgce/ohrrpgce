#include <stdlib.h>
#include "rasterizer.h"



Color Tex2DSampler::sample8bit(const Surface* pTexture, FPInt u, FPInt v) const
{
	FPInt minuteScale;
	minuteScale.fraction = 0xffff; //same as 65535/65536

	u *= /*0.99999f;*/minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	u.whole = 0; //remove all whole numbers and negative references, keeping fraction
	u *= pTexture->width;//FPInt(pTexture->width); //scale from (0.0)-(0.9999...) to (0)-(texture.width-1)

	v *= /*0.99999f;*/minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	v.whole = 0; //remove all whole numbers and negative references, keeping fraction
	v *= pTexture->height;//FPInt(pTexture->height); //scale from (0.0)-(0.9999...) to (0)-(texture.width-1)

	return pTexture->pPaletteData[v.whole * pTexture->width + u.whole];
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

	return pTexture->pColorData[v.whole * pTexture->width + u.whole];
}



void LineSegment::calculateLineSegment(const Position &A, const Position &B)
{
	m_dx = A.x - B.x;
	m_dy = A.y - B.y;

	if((m_dx < 0 ? -m_dx : m_dx) > (m_dy < 0 ? -m_dy : m_dy))
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
	m_leastX = A.x < B.x ? A.x : B.x;
	m_greatestX = A.x > B.x ? A.x : B.x;
	m_leastY = A.y < B.y ? A.y : B.y;
	m_greatestY = A.y > B.y ? A.y : B.y;
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


template <class T_VertexType>
void TriRasterizer::calculateTriangleRect(const T_VertexType* pTriangle, ClippingRectF &clipOut)
{
	clipOut.left = (pTriangle[0].pos.x < pTriangle[1].pos.x ? pTriangle[0].pos.x : pTriangle[1].pos.x);
	clipOut.left = (pTriangle[2].pos.x < (float)clipOut.left ? pTriangle[2].pos.x : clipOut.left);

	clipOut.right = (pTriangle[0].pos.x > pTriangle[1].pos.x ? pTriangle[0].pos.x : pTriangle[1].pos.x);
	clipOut.right = (pTriangle[2].pos.x > (float)clipOut.right ? pTriangle[2].pos.x : clipOut.right);

	clipOut.top = (pTriangle[0].pos.y < pTriangle[1].pos.y ? pTriangle[0].pos.y : pTriangle[1].pos.y);
	clipOut.top = (pTriangle[2].pos.y < (float)clipOut.top ? pTriangle[2].pos.y : clipOut.top);

	clipOut.bottom = (pTriangle[0].pos.y > pTriangle[1].pos.y ? pTriangle[0].pos.y : pTriangle[1].pos.y);
	clipOut.bottom = (pTriangle[2].pos.y > (float)clipOut.bottom ? pTriangle[2].pos.y : clipOut.bottom);
}

template <class T_VertexType>
void TriRasterizer::calculateRasterPixels(const Surface* pSurfaceDest, const T_VertexType* pTriangle, ClippingRectF& clipRgn, ClippingRectF& triangleRgn, std::queue< DrawingRange<T_VertexType> >& rasterLinesOut)
{//done
	//calculate the edge lines of the triangle
	LineSegment segments[3];
	for(int i = 0; i < 3; i++)
		segments[i].calculateLineSegment(pTriangle[i].pos, pTriangle[(i+1)%3].pos);

	//find the left and right boundaries of each raster line
	float xIntersection[3];
	float leftMost, rightMost;
	int leftMostIndex, rightMostIndex;
	float scale;
	T_VertexType leftVertex, rightVertex;
	int row = (clipRgn.top > triangleRgn.top ? clipRgn.top : triangleRgn.top);
	int rowEnd = (clipRgn.bottom < triangleRgn.bottom ? clipRgn.bottom : triangleRgn.bottom);
	for(; row <= rowEnd; row++)
	{
		leftMost = triangleRgn.right + 1.0f;
		leftMostIndex = -1;
		rightMost = triangleRgn.left - 1.0f;
		rightMostIndex = -1;
		for(int i = 0; i < 3; i++)
		{
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

void TriRasterizer::rasterColor(const DrawingRange<VertexPC> &range, Surface *pSurfaceDest)
{//done
	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color finalColor;
	float length(range.greatest.pos.x - range.least.pos.x+1), weight;

	for(int i = start; i <= finish; i++)
	{
		weight = 255.0f*(finish-i) / length;
		finalColor = range.least.col;
		finalColor.scale(range.greatest.col, weight);
		pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
	}
}

void TriRasterizer::rasterTexture(const DrawingRange<VertexPT> &range, const Surface *pTexture, const Palette *pPalette, Surface *pSurfaceDest)
{//done
	//assumed that if source is 8bit, a palette was passed in

	TexCoord texel;
	float length(range.greatest.pos.x - range.least.pos.x+1.0f);
	float weightFirst;
	float weightSecond;

	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor, finalColor;
	//float red, green, blue;

	if(pTexture->format == SF_32bit)
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;

			//alpha blending
			srcColor = m_sampler.sample32bit(pTexture, texel.u, texel.v);
			destColor = pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i];

			//srcColor.a = 64; //test value

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
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;/*( ((srcColor.dw & 0xffffff) * (srcColor.a)) + ((destColor.dw & 0xffffff) * (0xff-(srcColor.a))) ) / 0xff | 0xff000000;*/
		}
	}
	else //texture is 8bit
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;

			//alpha blending
			srcColor = pPalette->p[ m_sampler.sample8bit(pTexture, texel.u, texel.v) ];
			destColor = pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i];

			//integer method of blending
			finalColor.r = ( srcColor.r * srcColor.a + destColor.r * (255 - srcColor.a) ) / 255;//(int)red;
			finalColor.g = ( srcColor.g * srcColor.a + destColor.g * (255 - srcColor.a) ) / 255;//(int)green;
			finalColor.b = ( srcColor.b * srcColor.a + destColor.b * (255 - srcColor.a) ) / 255;//(int)blue;

			finalColor.a = (srcColor.a + destColor.a) > 255 ? 255 : (srcColor.a + destColor.a);
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
		}
	}
}

void TriRasterizer::rasterTextureWithColorKey0(const DrawingRange<VertexPT> &range, const Surface *pTexture, const Palette *pPalette, Surface *pSurfaceDest)
{//needs revision
	//assumed that if source is 8bit, a palette was passed in

	TexCoord texel;
	float length(range.greatest.pos.x - range.least.pos.x+1.0f);
	float weightFirst;
	float weightSecond;

	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor, finalColor;

	if(pTexture->format == SF_32bit)
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;

			//alpha blending
			srcColor = m_sampler.sample32bit(pTexture, texel.u, texel.v);
			destColor = pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i];

			//integer method of blending
			finalColor.r = ( srcColor.r * srcColor.a + destColor.r * (255 - srcColor.a) ) / 255;
			finalColor.g = ( srcColor.g * srcColor.a + destColor.g * (255 - srcColor.a) ) / 255;
			finalColor.b = ( srcColor.b * srcColor.a + destColor.b * (255 - srcColor.a) ) / 255;

			finalColor.a = (srcColor.a + destColor.a) > 255 ? 255 : (srcColor.a + destColor.a);
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
		}
	}
	else //texture is 8bit
	{
		uint8_t colorKey = 0;
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;

			//alpha blending
			colorKey = m_sampler.sample8bit(pTexture, texel.u, texel.v);
			if( !colorKey )
				continue;

			srcColor = pPalette->p[colorKey];
			destColor = pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i];

			//integer method of blending
			finalColor.r = ( srcColor.r * srcColor.a + destColor.r * (255 - srcColor.a) ) / 255;//(int)red;
			finalColor.g = ( srcColor.g * srcColor.a + destColor.g * (255 - srcColor.a) ) / 255;//(int)green;
			finalColor.b = ( srcColor.b * srcColor.a + destColor.b * (255 - srcColor.a) ) / 255;//(int)blue;

			finalColor.a = (srcColor.a + destColor.a) > 255 ? 255 : (srcColor.a + destColor.a);
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
		}
	}
}

void TriRasterizer::rasterTextureColor(const DrawingRange<VertexPTC> &range, const Surface *pTexture, const Palette *pPalette, Surface *pSurfaceDest)
{//done
	//assumed that if source is 8bit, a palette was passed in

	TexCoord texel;
	float length(range.greatest.pos.x - range.least.pos.x+1.0f);
	float weightFirst;
	float weightSecond;

	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor, finalColor, vertexColor;

	if(pTexture->format == SF_32bit)
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;
			vertexColor = range.least.col;
			vertexColor.scale(range.greatest.col, 255.0f * weightFirst);

			//alpha blending
			srcColor = m_sampler.sample32bit(pTexture, texel.u, texel.v);
			srcColor.scale(vertexColor);
			destColor = pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i];

			//integer method of blending
			finalColor.r = ( srcColor.r * srcColor.a + destColor.r * (255 - srcColor.a) ) / 255;
			finalColor.g = ( srcColor.g * srcColor.a + destColor.g * (255 - srcColor.a) ) / 255;
			finalColor.b = ( srcColor.b * srcColor.a + destColor.b * (255 - srcColor.a) ) / 255;

			finalColor.a = (srcColor.a + destColor.a) > 255 ? 255 : (srcColor.a + destColor.a);
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
		}
	}
	else //texture is 8bit 
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;
			vertexColor = range.least.col;
			vertexColor.scale(range.greatest.col, 255.0f * weightFirst);

			//alpha blending
			srcColor = pPalette->p[m_sampler.sample8bit(pTexture, texel.u, texel.v)];
			srcColor.scale(vertexColor);
			destColor = pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i];

			//integer method of blending
			finalColor.r = ( srcColor.r * srcColor.a + destColor.r * (255 - srcColor.a) ) / 255;//(int)red;
			finalColor.g = ( srcColor.g * srcColor.a + destColor.g * (255 - srcColor.a) ) / 255;//(int)green;
			finalColor.b = ( srcColor.b * srcColor.a + destColor.b * (255 - srcColor.a) ) / 255;//(int)blue;

			finalColor.a = (srcColor.a + destColor.a) > 255 ? 255 : (srcColor.a + destColor.a);
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
		}
	}
}

void TriRasterizer::rasterTextureColorWithColorKey0(const DrawingRange<VertexPTC> &range, const Surface *pTexture, const Palette *pPalette, Surface *pSurfaceDest)
{//done
	//assumed that if source is 8bit, a palette was passed in

	TexCoord texel;
	float length(range.greatest.pos.x - range.least.pos.x+1.0f);
	float weightFirst;
	float weightSecond;

	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x-.5f);

	Color srcColor, destColor, finalColor, vertexColor;

	if(pTexture->format == SF_32bit)
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;
			vertexColor = range.least.col;
			vertexColor.scale(range.greatest.col, 255.0f * weightFirst);

			//alpha blending
			srcColor = m_sampler.sample32bit(pTexture, texel.u, texel.v);
			srcColor.scale(vertexColor);
			destColor = pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i];

			//integer method of blending
			finalColor.r = ( srcColor.r * srcColor.a + destColor.r * (255 - srcColor.a) ) / 255;
			finalColor.g = ( srcColor.g * srcColor.a + destColor.g * (255 - srcColor.a) ) / 255;
			finalColor.b = ( srcColor.b * srcColor.a + destColor.b * (255 - srcColor.a) ) / 255;

			finalColor.a = (srcColor.a + destColor.a) > 255 ? 255 : (srcColor.a + destColor.a);
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
		}
	}
	else //texture is 8bit
	{
		uint8_t colorKey = 0;
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;
			vertexColor = range.least.col;
			vertexColor.scale(range.greatest.col, 255.0f * weightFirst);

			//alpha blending
			colorKey = m_sampler.sample8bit(pTexture, texel.u, texel.v);
			if( !colorKey )
				continue;

			srcColor = pPalette->p[colorKey];
			srcColor.scale(vertexColor);
			destColor = pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i];

			//integer method of blending
			finalColor.r = ( srcColor.r * srcColor.a + destColor.r * (255 - srcColor.a) ) / 255;//(int)red;
			finalColor.g = ( srcColor.g * srcColor.a + destColor.g * (255 - srcColor.a) ) / 255;//(int)green;
			finalColor.b = ( srcColor.b * srcColor.a + destColor.b * (255 - srcColor.a) ) / 255;//(int)blue;

			finalColor.a = (srcColor.a + destColor.a) > 255 ? 255 : (srcColor.a + destColor.a);
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
		}
	}
}

void TriRasterizer::drawTriangleColor(VertexPC *pTriangle, Color argbModifier, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if(pSurfaceDest == NULL || pTriangle == NULL)
		return;

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
		return;

	//apply color modifier
	for(int i = 0; i < 3; i++)
		pTriangle[i].col.scale(argbModifier);

	std::queue< DrawingRange< VertexPC > > rasterLines;
	calculateRasterPixels(pSurfaceDest, pTriangle, clipRgn, triangleRgn, rasterLines);

	//rasterize the polygon
	while(!rasterLines.empty())
	{
		rasterColor(rasterLines.front(), pSurfaceDest);
		rasterLines.pop();
	}
}

void TriRasterizer::drawTriangleTexture(VertexPT *pTriangle, const Surface *pTexture, const Palette *pPalette, int bUseColorKey0, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if(pSurfaceDest == NULL || pTriangle == NULL || pTexture == NULL)
		return;

	//palette check
	if(pTexture->format == SF_8bit)
		if( !pPalette )
			return; //need a palette to convert

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
		return;

	std::queue< DrawingRange< VertexPT > > rasterLines;
	calculateRasterPixels(pSurfaceDest, pTriangle, clipRgn, triangleRgn, rasterLines);

	//rasterize the polygon
	if(bUseColorKey0)
	{
		while(!rasterLines.empty())
		{
			rasterTextureWithColorKey0(rasterLines.front(), pTexture, pPalette, pSurfaceDest);
			rasterLines.pop();
		}
	}
	else
	{
		while(!rasterLines.empty())
		{
			rasterTexture(rasterLines.front(), pTexture, pPalette, pSurfaceDest);
			rasterLines.pop();
		}
	}
}

void TriRasterizer::drawTriangleTextureColor(VertexPTC *pTriangle, const Surface *pTexture, const Palette *pPalette, int bUseColorKey0, Color argbModifier, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if(pSurfaceDest == NULL || pTriangle == NULL || pTexture == NULL)
		return;

	//palette check
	if(pTexture->format == SF_8bit)
		if( !pPalette )
			return; //need a palette to convert

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
		return;

	//apply color modifier
	for(int i = 0; i < 3; i++)
		pTriangle[i].col.scale(argbModifier);

	std::queue< DrawingRange< VertexPTC > > rasterLines;
	calculateRasterPixels(pSurfaceDest, pTriangle, clipRgn, triangleRgn, rasterLines);

	//rasterize the polygon
	if(bUseColorKey0)
	{
		while(!rasterLines.empty())
		{
			rasterTextureColorWithColorKey0(rasterLines.front(), pTexture, pPalette, pSurfaceDest);
			rasterLines.pop();
		}
	}
	else
	{
		while(!rasterLines.empty())
		{
			rasterTextureColor(rasterLines.front(), pTexture, pPalette, pSurfaceDest);
			rasterLines.pop();
		}
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

	for(int i = 0; i < 4; i++)
	{
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

	for(int i = 0; i < 4; i++)
		drawTriangleColor(&triangles[i*3], argbModifier, pRectDest, pSurfaceDest);
}

void QuadRasterizer::drawQuadTexture(const VertexPT *pQuad, const Surface *pTexture, const Palette *pPalette, int bUseColorKey0, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if( pQuad == NULL )
		return;
	VertexPT triangles[4*3];
	generateTriangles(pQuad, triangles);

	for(int i = 0; i < 4; i++)
		drawTriangleTexture(&triangles[i*3], pTexture, pPalette, bUseColorKey0, pRectDest, pSurfaceDest);
}

void QuadRasterizer::drawQuadTextureColor(const VertexPTC *pQuad, const Surface *pTexture, const Palette *pPalette, int bUseColorKey0, Color argbModifier, SurfaceRect *pRectDest, Surface *pSurfaceDest)
{//done
	if( pQuad == NULL )
		return;
	VertexPTC triangles[4*3];
	generateTriangles(pQuad, triangles);

	for(int i = 0; i < 4; i++)
		drawTriangleTextureColor(&triangles[i*3], pTexture, pPalette, bUseColorKey0, argbModifier, pRectDest, pSurfaceDest);
}
