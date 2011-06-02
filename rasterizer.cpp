#include "rasterizer.h"

Color Tex2DSampler::sample(const Surface* pSurface, FPInt x, FPInt y) const
{
	FPInt minuteScale;
	minuteScale.fraction = 0xffff; //same as 65535/65536

	x *= minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	x.whole = 0; //remove all whole numbers and negative references, keeping fraction
	x *= FPInt(pSurface->width); //scale from (0.0)-(0.9999...) to (0)-(surface.width-1)

	y *= minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	y.whole = 0; //remove all whole numbers and negative references, keeping fraction
	y *= FPInt(pSurface->height); //scale from (0.0)-(0.9999...) to (0)-(surface.width-1)

	Color color;
	if(pSurface->format == SFMT_P8)
		color = pSurface->pPaletteData[y.whole * pSurface->width + x.whole] & 0xff;
	else if(pSurface->format == SFMT_A8R8G8B8)
		color = pSurface->pColorData[y.whole * pSurface->width + x.whole];

	return color;
}



void TriRasterizer::calculateTriangleRect(ClippingRect &clipOut, const Triangle* pTriangle)
{
	clipOut.left = (pTriangle->pnt[0].pos.x < pTriangle->pnt[1].pos.x ? pTriangle->pnt[0].pos.x : pTriangle->pnt[1].pos.x);
	clipOut.left = (pTriangle->pnt[2].pos.x < clipOut.left ? pTriangle->pnt[2].pos.x : clipOut.left);

	clipOut.right = (pTriangle->pnt[0].pos.x > pTriangle->pnt[1].pos.x ? pTriangle->pnt[0].pos.x : pTriangle->pnt[1].pos.x);
	clipOut.right = (pTriangle->pnt[2].pos.x > clipOut.right ? pTriangle->pnt[2].pos.x : clipOut.right);

	clipOut.top = (pTriangle->pnt[0].pos.y < pTriangle->pnt[1].pos.y ? pTriangle->pnt[0].pos.y : pTriangle->pnt[1].pos.y);
	clipOut.top = (pTriangle->pnt[2].pos.y < clipOut.top ? pTriangle->pnt[2].pos.y : clipOut.top);

	clipOut.bottom = (pTriangle->pnt[0].pos.y > pTriangle->pnt[1].pos.y ? pTriangle->pnt[0].pos.y : pTriangle->pnt[1].pos.y);
	clipOut.bottom = (pTriangle->pnt[2].pos.y > clipOut.bottom ? pTriangle->pnt[2].pos.y : clipOut.bottom);
}

void TriRasterizer::interpolateVertices(Vertex &vertexOut, const Vertex &v1, const Vertex &v2, FPInt scale)
{
	Vertex out;
	FPInt invScale(-scale + 1);

	out.pos.x = scale * v1.pos.x + invScale * v2.pos.x;
	out.pos.y = scale * v1.pos.y + invScale * v2.pos.y;
	out.tex.u = scale * v1.tex.u + invScale * v2.tex.u;
	out.tex.v = scale * v1.tex.v + invScale * v2.tex.v;
	out.col.a = scale * v1.col.a + invScale * v2.col.a;
	out.col.r = scale * v1.col.r + invScale * v2.col.r;
	out.col.g = scale * v1.col.g + invScale * v2.col.g;
	out.col.b = scale * v1.col.b + invScale * v2.col.b;

	vertexOut = out;
}

void TriRasterizer::calculateRasterPixels(unsigned int row, FPInt minimum, FPInt maximum, const Surface* pSurface, const Triangle *pTriangle)
{
	FPInt xIntercept[3];

	//figure all x-intercepts
	FPInt deltaX, deltaY, slope, yIntercept;
	Position a, b;

	for(int i = 0; i < 3; i++)
	{
		a = pTriangle->pnt[i].pos;
		b = pTriangle->pnt[(i+1)%3].pos;
		deltaX = a.x - b.x;
		deltaY = a.y - b.y;

		if(deltaX == 0) //no slope
		{
			xIntercept[i] = a.x;
		}
		else
		{
			slope = deltaY / deltaX;
			yIntercept = a.y - slope * a.x;

			if(slope == 0) //0 slope
			{
				if(yIntercept == row) //entire row is to be rasterized (line is parallel with this rasterizing line AND overlays it)
				{
					xIntercept[i] = a.x;
					xIntercept[(i+1)%3] = b.x;
					break;
				}
			}
			else
			{
				xIntercept[i] = (-yIntercept + row) / slope;
			}
		}
	}

	//figure leftmost and rightmost x-intercepts within the triangle minimum and maximum:
	//those are the boundaries of the raster line
	FPInt leftMost(maximum), rightMost(minimum);
	int leftIndex(0), rightIndex(1);
	for(int i = 0; i < 3; i++)
	{
		if(xIntercept[i] >= minimum && xIntercept[i] <= maximum)
		{
			if(xIntercept[i] < leftMost)
			{
				leftMost = xIntercept[i];
				leftIndex = i;
			}
			if(xIntercept[i] > rightMost)
			{
				rightMost = xIntercept[i];
				rightIndex = i;
			}
		}
	}

	//generate boundaries of raster line
	Vertex leftBoundary, rightBoundary;

	FPInt scale;
	deltaX = pTriangle->pnt[leftIndex].pos.x - pTriangle->pnt[(leftIndex+1)%3].pos.x;
	deltaY = pTriangle->pnt[leftIndex].pos.y - pTriangle->pnt[(leftIndex+1)%3].pos.y;

	if((deltaX < 0 ? -deltaX : deltaX) > (deltaY < 0 ? -deltaY : deltaY)) //ensure the larger scale is used
	{
		scale = (xIntercept[leftIndex] - pTriangle->pnt[(leftIndex+1)%3].pos.x) / deltaX;
	}
	else
	{
		scale = (FPInt(row) - pTriangle->pnt[(leftIndex+1)%3].pos.y) / deltaY;
	}

	interpolateVertices(leftBoundary, pTriangle->pnt[leftIndex], pTriangle->pnt[(leftIndex+1)%3], scale);

	deltaX = pTriangle->pnt[rightIndex].pos.x - pTriangle->pnt[(rightIndex+1)%3].pos.x;
	deltaY = pTriangle->pnt[rightIndex].pos.y - pTriangle->pnt[(rightIndex+1)%3].pos.y;

	if((deltaX < 0 ? -deltaX : deltaX) > (deltaY < 0 ? -deltaY : deltaY)) //ensure the larger scale is used
	{
		scale = (xIntercept[rightIndex] - pTriangle->pnt[(rightIndex+1)%3].pos.x) / deltaX;
	}
	else
	{
		scale = (FPInt(row) - pTriangle->pnt[(rightIndex+1)%3].pos.y) / deltaY;
	}

	interpolateVertices(rightBoundary, pTriangle->pnt[rightIndex], pTriangle->pnt[(rightIndex+1)%3], scale);

	//perform clipping interpolation
	if(leftBoundary.pos.x >= pSurface->width || rightBoundary.pos.x < 0)
		return; //completely outside of raster area

	if(leftBoundary.pos.x < 0)
	{
		scale = leftBoundary.pos.x / (leftBoundary.pos.x - rightBoundary.pos.x);
		interpolateVertices(leftBoundary, leftBoundary, rightBoundary, scale);
	}
	if(rightBoundary.pos.x >= pSurface->width)
	{
		scale = (FPInt(pSurface->width - 1) - rightBoundary.pos.x) / (leftBoundary.pos.x - rightBoundary.pos.x);
		interpolateVertices(rightBoundary, rightBoundary, leftBoundary, scale);
	}

	//post the raster line
	m_rasterLines.push( DrawingRange(leftBoundary, rightBoundary) );
}

void TriRasterizer::rasterColor(Surface *pSurface, const DrawingRange &range, const Triangle *pTriangle)
{
	Color color;
	FPInt length(range.greatest.pos.x - range.least.pos.x), 
		  weightFirst;

	for(int i = range.least.pos.x; i < range.greatest.pos.x.whole; i++)
	{
		weightFirst = (range.greatest.pos.x - i) / length;

		if(pSurface->format == SFMT_P8)
		{
			color.b = weightFirst * range.least.col.b + (-weightFirst + 1) * range.greatest.col.b;
			pSurface->pPaletteData[range.least.pos.y.whole * pSurface->width + i] = (SurfaceData8)color;
		}
		else
		{
			color.a = weightFirst * range.least.col.a + (-weightFirst + 1) * range.greatest.col.a;
			color.r = weightFirst * range.least.col.r + (-weightFirst + 1) * range.greatest.col.r;
			color.g = weightFirst * range.least.col.g + (-weightFirst + 1) * range.greatest.col.g;
			color.b = weightFirst * range.least.col.b + (-weightFirst + 1) * range.greatest.col.b;
			pSurface->pColorData[range.least.pos.y.whole * pSurface->width + i] = (SurfaceData32)color;
		}
	}
}

void TriRasterizer::rasterTexture(Surface *pSurface, const DrawingRange &range, const Triangle *pTriangle, const Surface *pTexture)
{
	TexCoord texel;
	FPInt length(range.greatest.pos.x - range.least.pos.x), 
		  weightFirst;

	for(int i = range.least.pos.x; i < range.greatest.pos.x.whole; i++)
	{
		weightFirst = (range.greatest.pos.x - i) / length;
		texel.u = weightFirst * range.least.tex.u + (-weightFirst + 1) * range.greatest.tex.u;
		texel.v = weightFirst * range.least.tex.v + (-weightFirst + 1) * range.greatest.tex.v;

		if(pSurface->format == SFMT_P8)
		{
			pSurface->pPaletteData[range.least.pos.y.whole * pSurface->width + i] = (SurfaceData8)m_sampler.sample(pTexture, texel.u, texel.v);
		}
		else
		{
			pSurface->pColorData[range.least.pos.y.whole * pSurface->width + i] = (SurfaceData32)m_sampler.sample(pTexture, texel.u, texel.v);
		}
	}
}

void TriRasterizer::rasterTextureColor(Surface *pSurface, const DrawingRange &range, const Triangle *pTriangle, const Surface *pTexture)
{
	TexCoord texel;
	Color texelColor;
	Color color;
	FPInt length(range.greatest.pos.x - range.least.pos.x), 
		  weightFirst;

	for(int i = range.least.pos.x; i < range.greatest.pos.x.whole; i++)
	{
		weightFirst = (range.greatest.pos.x - i) / length;
		texel.u = weightFirst * range.least.tex.u + (-weightFirst + 1) * range.greatest.tex.u;
		texel.v = weightFirst * range.least.tex.v + (-weightFirst + 1) * range.greatest.tex.v;

		if(pSurface->format == SFMT_P8) //no point for palettes to be affected by color weights
		{
			pSurface->pPaletteData[range.least.pos.y.whole * pSurface->width + i] = (SurfaceData8)m_sampler.sample(pTexture, texel.u, texel.v);
		}
		else
		{
			texelColor = m_sampler.sample(pTexture, texel.u, texel.v);
			color.a = ((weightFirst * range.least.col.a + (-weightFirst + 1) * range.greatest.col.a).whole * texelColor.a) >> 8;
			color.r = ((weightFirst * range.least.col.r + (-weightFirst + 1) * range.greatest.col.r).whole * texelColor.r) >> 8;
			color.g = ((weightFirst * range.least.col.g + (-weightFirst + 1) * range.greatest.col.g).whole * texelColor.g) >> 8;
			color.b = ((weightFirst * range.least.col.b + (-weightFirst + 1) * range.greatest.col.b).whole * texelColor.b) >> 8;
			pSurface->pColorData[range.least.pos.y.whole * pSurface->width + i] = (SurfaceData32)color;
		}
	}
}

void TriRasterizer::drawTest(Surface* pSurface, const Triangle* pTriangle, const Color &col)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip;
	calculateTriangleRect(clip, pTriangle);
	if(clip.top < 0) clip.top = 0;
	if(clip.bottom >= pSurface->height) clip.bottom = pSurface->height-1;
	for(int row = clip.top.whole; row < clip.bottom.whole; row++)
		calculateRasterPixels(row, clip.left, clip.right, pSurface, pTriangle);

	//rasterize the polygon
	if(pSurface->format == SFMT_P8)
	{
		while(!m_rasterLines.empty())
		{
			for(int i = m_rasterLines.front().least.pos.x.whole; i < m_rasterLines.front().greatest.pos.x.whole; i++)
			{
				pSurface->pPaletteData[m_rasterLines.front().least.pos.y.whole * pSurface->width + i] = (SurfaceData8)col;
				m_rasterLines.pop();
			}
		}
	}
	else if(pSurface->format == SFMT_A8R8G8B8)
	{
		while(!m_rasterLines.empty())
		{
			for(int i = m_rasterLines.front().least.pos.x.whole; i < m_rasterLines.front().greatest.pos.x.whole; i++)
			{
				pSurface->pColorData[m_rasterLines.front().least.pos.y.whole * pSurface->width + i] = (SurfaceData32)col;
			}
			m_rasterLines.pop();
		}
	}
}

void TriRasterizer::drawColor(Surface *pSurface, const Triangle *pTriangle)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip;
	calculateTriangleRect(clip, pTriangle);
	if(clip.top < 0) clip.top = 0;
	if(clip.bottom >= pSurface->height) clip.bottom = pSurface->height-1;

	for(int row = clip.top.whole; row < clip.bottom.whole; row++)
		calculateRasterPixels(row, clip.left, clip.right, pSurface, pTriangle);

	//rasterize the polygon
	while(!m_rasterLines.empty())
	{
		rasterColor(pSurface, m_rasterLines.front(), pTriangle);
		m_rasterLines.pop();
	}
}

void TriRasterizer::drawTexture(Surface *pSurface, const Triangle *pTriangle, const Surface* pTexture)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip;
	calculateTriangleRect(clip, pTriangle);
	if(clip.top < 0) clip.top = 0;
	if(clip.bottom >= pSurface->height) clip.bottom = pSurface->height-1;
	for(int row = clip.top.whole; row < clip.bottom.whole; row++)
		calculateRasterPixels(row, clip.left, clip.right, pSurface, pTriangle);

	//rasterize the polygon
	while(!m_rasterLines.empty())
	{
		rasterTexture(pSurface, m_rasterLines.front(), pTriangle, pTexture);
		m_rasterLines.pop();
	}
}

void TriRasterizer::drawTextureColor(Surface *pSurface, const Triangle *pTriangle, const Surface* pTexture)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip;
	calculateTriangleRect(clip, pTriangle);
	if(clip.top < 0) clip.top = 0;
	if(clip.bottom >= pSurface->height) clip.bottom = pSurface->height-1;
	for(int row = clip.top.whole; row < clip.bottom.whole; row++)
		calculateRasterPixels(row, clip.left, clip.right, pSurface, pTriangle);

	//rasterize the polygon
	while(!m_rasterLines.empty())
	{
		rasterTextureColor(pSurface, m_rasterLines.front(), pTriangle, pTexture);
		m_rasterLines.pop();
	}
}




void QuadRasterizer::generateTriangles(const Quad *pQuad)
{
	Vertex center;
	FPInt a,r,g,b;
	for(int i = 0; i < 4; i++)
	{
		center.pos.x += pQuad->pnt[i].pos.x;
		center.pos.y += pQuad->pnt[i].pos.y;
		center.tex.u += pQuad->pnt[i].tex.u;
		center.tex.v += pQuad->pnt[i].tex.v;
		a += pQuad->pnt[i].col.a;
		r += pQuad->pnt[i].col.r;
		g += pQuad->pnt[i].col.g;
		b += pQuad->pnt[i].col.b;
	}
	center.pos.x /= 4;
	center.pos.y /= 4;
	center.tex.u /= 4;
	center.tex.v /= 4;
	a /= 4;
	r /= 4;
	g /= 4;
	b /= 4;
	center.col.a = a.whole;
	center.col.r = r.whole;
	center.col.g = g.whole;
	center.col.b = b.whole;

	for(int i = 0; i < 4; i++)
	{
		m_triangles[i].pnt[0] = pQuad->pnt[i];
		m_triangles[i].pnt[1] = pQuad->pnt[(i+1)%4];
		m_triangles[i].pnt[2] = center;
	}
}