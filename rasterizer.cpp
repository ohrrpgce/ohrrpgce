#include "rasterizer.h"

Color Tex2DSampler::sample(const Surface* pSurface, FPInt u, FPInt v) const
{
	FPInt minuteScale;
	minuteScale.fraction = 0xffff; //same as 65535/65536

	u *= /*0.99999f;*/minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	u.whole = 0; //remove all whole numbers and negative references, keeping fraction
	u *= pSurface->width;//FPInt(pSurface->width); //scale from (0.0)-(0.9999...) to (0)-(surface.width-1)

	v *= /*0.99999f;*/minuteScale; //scale from (0.0)-(1.0) to (0.0)-(0.99999...)
	v.whole = 0; //remove all whole numbers and negative references, keeping fraction
	v *= pSurface->height;//FPInt(pSurface->height); //scale from (0.0)-(0.9999...) to (0)-(surface.width-1)

	Color color;
	if(pSurface->format == SFMT_P8)
		color = pSurface->pPaletteData[(int)v * pSurface->width + (int)u];
	else if(pSurface->format == SFMT_A8R8G8B8)
		color = pSurface->pColorData[(int)v * pSurface->width + (int)u];

	return color;
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



void TriRasterizer::calculateTriangleRect(ClippingRect &clipOut, const Triangle* pTriangle)
{
	clipOut.left = (pTriangle->pnt[0].pos.x < pTriangle->pnt[1].pos.x ? pTriangle->pnt[0].pos.x : pTriangle->pnt[1].pos.x);
	clipOut.left = (pTriangle->pnt[2].pos.x < (float)clipOut.left ? pTriangle->pnt[2].pos.x : clipOut.left);

	clipOut.right = (pTriangle->pnt[0].pos.x > pTriangle->pnt[1].pos.x ? pTriangle->pnt[0].pos.x : pTriangle->pnt[1].pos.x);
	clipOut.right = (pTriangle->pnt[2].pos.x > (float)clipOut.right ? pTriangle->pnt[2].pos.x : clipOut.right);

	clipOut.top = (pTriangle->pnt[0].pos.y < pTriangle->pnt[1].pos.y ? pTriangle->pnt[0].pos.y : pTriangle->pnt[1].pos.y);
	clipOut.top = (pTriangle->pnt[2].pos.y < (float)clipOut.top ? pTriangle->pnt[2].pos.y : clipOut.top);

	clipOut.bottom = (pTriangle->pnt[0].pos.y > pTriangle->pnt[1].pos.y ? pTriangle->pnt[0].pos.y : pTriangle->pnt[1].pos.y);
	clipOut.bottom = (pTriangle->pnt[2].pos.y > (float)clipOut.bottom ? pTriangle->pnt[2].pos.y : clipOut.bottom);
}

void TriRasterizer::interpolateTexCoord(TexCoord &texOut, const TexCoord &t1, const TexCoord &t2, float scale)
{
	TexCoord out;
	float invScale(-scale + 1);

	out.u = scale * t1.u + invScale * t2.u;
	out.v = scale * t1.v + invScale * t2.v;

	texOut = out;
}

void TriRasterizer::calculateRasterPixels(const Surface* pSurface, const Triangle *pTriangle, ClippingRect& clip)
{
	//check that the clipping rect is within the surface's boundaries
	if(clip.left < 0.0f) clip.left = 0.0f;
	if(clip.right > pSurface->width-1) clip.right = pSurface->width-1;
	if(clip.top < 0.0f) clip.top = 0.0f;
	if(clip.bottom > pSurface->height-1) clip.bottom = pSurface->height-1;

	//check if triangle is out of range of the clipping rect
	ClippingRect triangleRect = {pTriangle->pnt[0].pos.x, pTriangle->pnt[0].pos.y, pTriangle->pnt[0].pos.x, pTriangle->pnt[0].pos.y};
	for(int i = 1; i < 3; i++)
	{
		if(pTriangle->pnt[i].pos.x < triangleRect.left)
			triangleRect.left = pTriangle->pnt[i].pos.x;
		if(pTriangle->pnt[i].pos.x > triangleRect.right)
			triangleRect.right = pTriangle->pnt[i].pos.x;
		if(pTriangle->pnt[i].pos.y < triangleRect.top)
			triangleRect.top = pTriangle->pnt[i].pos.y;
		if(pTriangle->pnt[i].pos.y > triangleRect.bottom)
			triangleRect.bottom = pTriangle->pnt[i].pos.y;
	}

	if(triangleRect.left > clip.right || triangleRect.right < clip.left || triangleRect.top > clip.bottom || triangleRect.bottom < clip.top)
		return;

	//calculate the edge lines of the triangle
	LineSegment segments[3];
	for(int i = 0; i < 3; i++)
		segments[i].calculateLineSegment(pTriangle->pnt[i].pos, pTriangle->pnt[(i+1)%3].pos);

	//find the left and right boundaries of each raster line
	float xIntersection[3];
	float leftMost, rightMost;
	int leftMostIndex, rightMostIndex;
	float scale;
	Vertex leftVertex, rightVertex;
	for(int row = (clip.top > triangleRect.top ? clip.top : triangleRect.top),
		    rowEnd = (clip.bottom < triangleRect.bottom ? clip.bottom : triangleRect.bottom);
			row <= rowEnd; row++)
	{
		leftMost = triangleRect.right + 1.0f;
		leftMostIndex = -1;
		rightMost = triangleRect.left - 1.0f;
		rightMostIndex = -1;
		for(int i = 0; i < 3; i++)
		{
			if(!segments[i].intersects(&xIntersection[i], row))
				continue;
			if(xIntersection[i] < triangleRect.left || xIntersection[i] > triangleRect.right)
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
				scale = (xIntersection[leftMostIndex] - pTriangle->pnt[(leftMostIndex+1)%3].pos.x) / segments[leftMostIndex].dx();
		}
		else
		{
			if(segments[leftMostIndex].dy() == 0.0f || segments[leftMostIndex].dy() == -0.0f)
				scale = 1.0f;
			else
				scale = (row - pTriangle->pnt[(leftMostIndex+1)%3].pos.y) / segments[leftMostIndex].dy();
		}
		//interpolateVertices(leftVertex, pTriangle->pnt[leftMostIndex], pTriangle->pnt[(leftMostIndex+1)%3], scale);
		interpolateTexCoord(leftVertex.tex, pTriangle->pnt[leftMostIndex].tex, pTriangle->pnt[(leftMostIndex+1)%3].tex, scale);
		leftVertex.pos.x = xIntersection[leftMostIndex];
		leftVertex.pos.y = row;

		if(segments[rightMostIndex].isFunctionOfX())
		{
			if(segments[rightMostIndex].dx() == 0.0f || segments[rightMostIndex].dx() == -0.0f)
				scale = 1.0f;
			else
				scale = (xIntersection[rightMostIndex] - pTriangle->pnt[(rightMostIndex+1)%3].pos.x) / segments[rightMostIndex].dx();
		}
		else
		{
			if(segments[rightMostIndex].dy() == 0.0f || segments[rightMostIndex].dy() == -0.0f)
				scale = 1.0f;
			else
				scale = (row - pTriangle->pnt[(rightMostIndex+1)%3].pos.y) / segments[rightMostIndex].dy();
		}
		//interpolateVertices(rightVertex, pTriangle->pnt[rightMostIndex], pTriangle->pnt[(rightMostIndex+1)%3], scale);
		interpolateTexCoord(rightVertex.tex, pTriangle->pnt[rightMostIndex].tex, pTriangle->pnt[(rightMostIndex+1)%3].tex, scale);
		rightVertex.pos.x = xIntersection[rightMostIndex];
		rightVertex.pos.y = row;

		//perform horizontal clipping
		if(leftVertex.pos.x > clip.right || rightVertex.pos.x < clip.left)
			continue;
		
		if(leftVertex.pos.x < clip.left)
		{
			scale = (clip.left - leftVertex.pos.x) / (rightVertex.pos.x - leftVertex.pos.x);
			//interpolateVertices(leftVertex, leftVertex, rightVertex, 1-scale);
			interpolateTexCoord(leftVertex.tex, leftVertex.tex, rightVertex.tex, 1-scale);
			leftVertex.pos.x = clip.left;
			//leftVertex.pos.y = row;
		}
		if(rightVertex.pos.x > clip.right)
		{
			scale = (clip.right - rightVertex.pos.x) / (leftVertex.pos.x - rightVertex.pos.x);
			//interpolateVertices(rightVertex, rightVertex, leftVertex, 1-scale);
			interpolateTexCoord(rightVertex.tex, rightVertex.tex, leftVertex.tex, 1-scale);
			rightVertex.pos.x = clip.right;
			//rightVertex.pos.y = row;
		}

		//push the data onto the raster queue
		m_rasterLines.push( DrawingRange(leftVertex, rightVertex) );
	}
}

void TriRasterizer::rasterColor(Surface *pSurface, const DrawingRange &range, const Triangle *pTriangle, Color col)
{
	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x);
	finish = (range.greatest.pos.x >= pSurface->width ? pSurface->width-1 : range.greatest.pos.x);

	if(pSurface->format == SFMT_P8)
	{
		for(int i = start; i <= finish; i++)
		{
			pSurface->pPaletteData[(int)range.least.pos.y * pSurface->width + i] = (SurfaceData8)col;
		}
	}
	else
	{
		for(int i = start; i <= finish; i++)
		{
			pSurface->pColorData[(int)range.least.pos.y * pSurface->width + i] = (SurfaceData32)col;
		}
	}
}

void TriRasterizer::rasterTexture(Surface *pSurface, const DrawingRange &range, const Triangle *pTriangle, const Surface *pTexture)
{
	TexCoord texel;
	float length(range.greatest.pos.x - range.least.pos.x+1), 
		  weightFirst,
		  weightSecond;

	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x);
	finish = (range.greatest.pos.x >= pSurface->width ? pSurface->width-1 : range.greatest.pos.x);

	Color srcColor, destColor, finalColor;
	float red, green, blue;

	if(pSurface->format == SFMT_P8)
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;

			srcColor = (SurfaceData8)m_sampler.sample(pTexture, texel.u, texel.v);
			if(srcColor.dw == 0x0)//color key test
				continue;
			pSurface->pPaletteData[(int)range.least.pos.y * pSurface->width + i] = srcColor; 
		}
	}
	else
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;

			//alpha blending
			srcColor = (SurfaceData32)m_sampler.sample(pTexture, texel.u, texel.v);
			destColor = pSurface->pColorData[(int)range.least.pos.y * pSurface->width + i];

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
			pSurface->pColorData[(int)range.least.pos.y * pSurface->width + i] = finalColor;/*( ((srcColor.dw & 0xffffff) * (srcColor.a)) + ((destColor.dw & 0xffffff) * (0xff-(srcColor.a))) ) / 0xff | 0xff000000;*/
		}
	}
}

void TriRasterizer::drawColor(Surface *pSurface, const Triangle *pTriangle, Color col)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip = {0.0f, 0.0f, pSurface->width-1, pSurface->height-1};
	calculateRasterPixels(pSurface, pTriangle, clip);

	//rasterize the polygon
	while(!m_rasterLines.empty())
	{
		rasterColor(pSurface, m_rasterLines.front(), pTriangle, col);
		m_rasterLines.pop();
	}
}

void TriRasterizer::drawTexture(Surface *pSurface, const Triangle *pTriangle, const Surface* pTexture)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip = {0.0f, 0.0f, pSurface->width-1, pSurface->height-1};
	calculateRasterPixels(pSurface, pTriangle, clip);

	//rasterize the polygon
	while(!m_rasterLines.empty())
	{
		rasterTexture(pSurface, m_rasterLines.front(), pTriangle, pTexture);
		m_rasterLines.pop();
	}
}




void QuadRasterizer::generateTriangles(const Quad *pQuad)
{
	Vertex center;
	for(int i = 0; i < 4; i++)
	{
		center.pos.x += pQuad->pnt[i].pos.x;
		center.pos.y += pQuad->pnt[i].pos.y;
		center.tex.u += pQuad->pnt[i].tex.u;
		center.tex.v += pQuad->pnt[i].tex.v;
	}
	center.pos.x /= 4;
	center.pos.y /= 4;
	center.tex.u /= 4;
	center.tex.v /= 4;

	for(int i = 0; i < 4; i++)
	{
		m_triangles[i].pnt[0] = pQuad->pnt[i];
		m_triangles[i].pnt[1] = pQuad->pnt[(i+1)%4];
		m_triangles[i].pnt[2] = center;
	}
}