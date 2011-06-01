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



void TriRasterizer::calculateClippingRect(ClippingRect &clipOut, const Surface* pSurface, const Triangle* pTriangle)
{
	clipOut.left = (pTriangle->pnt[0].pos.x < pTriangle->pnt[1].pos.x ? pTriangle->pnt[0].pos.x : pTriangle->pnt[1].pos.x);
	clipOut.left = (pTriangle->pnt[2].pos.x < clipOut.left ? pTriangle->pnt[2].pos.x : clipOut.left);
	clipOut.left = (clipOut.left < 0 ? 0 : clipOut.left);

	clipOut.right = (pTriangle->pnt[0].pos.x > pTriangle->pnt[1].pos.x ? pTriangle->pnt[0].pos.x : pTriangle->pnt[1].pos.x);
	clipOut.right = (pTriangle->pnt[2].pos.x > clipOut.right ? pTriangle->pnt[2].pos.x : clipOut.right);
	clipOut.right = (clipOut.right > (pSurface->width-1) ? (pSurface->width-1) : clipOut.right);

	clipOut.top = (pTriangle->pnt[0].pos.y < pTriangle->pnt[1].pos.y ? pTriangle->pnt[0].pos.y : pTriangle->pnt[1].pos.y);
	clipOut.top = (pTriangle->pnt[2].pos.y < clipOut.top ? pTriangle->pnt[2].pos.y : clipOut.top);
	clipOut.top = (clipOut.top < 0 ? 0 : clipOut.top);

	clipOut.bottom = (pTriangle->pnt[0].pos.y > pTriangle->pnt[1].pos.y ? pTriangle->pnt[0].pos.y : pTriangle->pnt[1].pos.y);
	clipOut.bottom = (pTriangle->pnt[2].pos.y > clipOut.bottom ? pTriangle->pnt[2].pos.y : clipOut.bottom);
	clipOut.bottom = (clipOut.bottom > (pSurface->height-1) ? (pSurface->height-1) : clipOut.bottom);
}

void TriRasterizer::decidePixels(unsigned int row, unsigned int minimum, unsigned int maximum, const Triangle *pTriangle)
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

			if(yIntercept == 0) //0 slope
			{
				if(yIntercept == row) //entire row is to be rasterized (line is parallel with this rasterizing line AND overlays it)
				{
					xIntercept[0] = minimum;
					xIntercept[1] = maximum;
					break;
				}
			}
			else
			{
				xIntercept[i] = (-yIntercept + row) / slope;
			}
		}
	}

	//determine range candidates
	FPInt least(minimum), greatest(maximum);
	for(int i = 0; i < 3; i++)
	{
		if(xIntercept[i] >= minimum)
			least = (least < xIntercept[i] ? least : xIntercept[i]);
		if(xIntercept[i] <= maximum)
			greatest = (greatest > xIntercept[i] ? greatest : xIntercept[i]);
	}

	//create pixel range
	m_rasterLines.push(DrawingRange(row, least, greatest));
}

void TriRasterizer::interpolateColor(Color &colorOut, const Position &posIn, const Triangle *pTriangle)
{//needs work
	//interpolate between points A and B
	//interpolate between that and C
}

void TriRasterizer::interpolateTexCoord(TexCoord &texCoordOut, const Position &posIn, const Triangle *pTriangle)
{//needs work
}

void TriRasterizer::drawTest(const Surface* pSurface, const Triangle* pTriangle, const Color &col)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip;
	calculateClippingRect(clip, pSurface, pTriangle);
	for(int row = clip.top.whole; row < clip.bottom.whole; row++)
		decidePixels(row, clip.left.whole, clip.right.whole, pTriangle);

	//rasterize the polygon
	if(pSurface->format == SFMT_P8)
	{
		while(!m_rasterLines.empty())
		{
			for(unsigned int i = m_rasterLines.front().least; i < m_rasterLines.front().greatest; i++)
			{
				pSurface->pPaletteData[m_rasterLines.front().row * pSurface->width + i] = (SurfaceData8)col;
				m_rasterLines.pop();
			}
		}
	}
	else if(pSurface->format == SFMT_A8R8G8B8)
	{
		while(!m_rasterLines.empty())
		{
			for(unsigned int i = m_rasterLines.front().least; i < m_rasterLines.front().greatest; i++)
			{
				pSurface->pColorData[m_rasterLines.front().row * pSurface->width + i] = (SurfaceData32)col;
				m_rasterLines.pop();
			}
		}
	}
}

void TriRasterizer::drawColor(const Surface *pSurface, const Triangle *pTriangle)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip;
	calculateClippingRect(clip, pSurface, pTriangle);
	for(int row = clip.top.whole; row < clip.bottom.whole; row++)
		decidePixels(row, clip.left.whole, clip.right.whole, pTriangle);

	//rasterize the polygon
	Position pixelPos;
	Color pixelColor;
	if(pSurface->format == SFMT_P8)
	{
		while(!m_rasterLines.empty())
		{
			for(unsigned int i = m_rasterLines.front().least; i < m_rasterLines.front().greatest; i++)
			{
				pixelPos.x = i;
				pixelPos.y = m_rasterLines.front().row * pSurface->width;
				interpolateColor(pixelColor, pixelPos, pTriangle);

				pSurface->pPaletteData[m_rasterLines.front().row * pSurface->width + i] = (SurfaceData8)pixelColor;
				m_rasterLines.pop();
			}
		}
	}
	else if(pSurface->format == SFMT_A8R8G8B8)
	{
		while(!m_rasterLines.empty())
		{
			for(unsigned int i = m_rasterLines.front().least; i < m_rasterLines.front().greatest; i++)
			{
				pixelPos.x = i;
				pixelPos.y = m_rasterLines.front().row * pSurface->width;
				interpolateColor(pixelColor, pixelPos, pTriangle);

				pSurface->pColorData[m_rasterLines.front().row * pSurface->width + i] = (SurfaceData32)pixelColor;
				m_rasterLines.pop();
			}
		}
	}
}

void TriRasterizer::drawTexture(const Surface *pSurface, const Triangle *pTriangle)
{
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip;
	calculateClippingRect(clip, pSurface, pTriangle);
	for(int row = clip.top.whole; row < clip.bottom.whole; row++)
		decidePixels(row, clip.left.whole, clip.right.whole, pTriangle);

	//rasterize the polygon
	Position pixelPos;
	TexCoord pixelTex;
	Color pixelColor;
	if(pSurface->format == SFMT_P8)
	{
		while(!m_rasterLines.empty())
		{
			for(unsigned int i = m_rasterLines.front().least; i < m_rasterLines.front().greatest; i++)
			{
				pixelPos.x = i;
				pixelPos.y = m_rasterLines.front().row * pSurface->width;
				interpolateTexCoord(pixelTex, pixelPos, pTriangle);
				pixelColor = m_sampler.sample(pSurface, pixelTex.u, pixelTex.v);

				pSurface->pPaletteData[m_rasterLines.front().row * pSurface->width + i] = (SurfaceData8)pixelColor;
				m_rasterLines.pop();
			}
		}
	}
	else if(pSurface->format == SFMT_A8R8G8B8)
	{
		while(!m_rasterLines.empty())
		{
			for(unsigned int i = m_rasterLines.front().least; i < m_rasterLines.front().greatest; i++)
			{
				pixelPos.x = i;
				pixelPos.y = m_rasterLines.front().row * pSurface->width;
				interpolateTexCoord(pixelTex, pixelPos, pTriangle);
				pixelColor = m_sampler.sample(pSurface, pixelTex.u, pixelTex.v);

				pSurface->pColorData[m_rasterLines.front().row * pSurface->width + i] = (SurfaceData32)pixelColor;
				m_rasterLines.pop();
			}
		}
	}
}

void TriRasterizer::drawTextureColor(const Surface *pSurface, const Triangle *pTriangle)
{ //needs work (haven't combined pixelColorModifier and pixelColor from the surface yet)
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip;
	calculateClippingRect(clip, pSurface, pTriangle);
	for(int row = clip.top.whole; row < clip.bottom.whole; row++)
		decidePixels(row, clip.left.whole, clip.right.whole, pTriangle);

	//rasterize the polygon
	Position pixelPos;
	TexCoord pixelTex;
	Color pixelColorModifier, pixelColor;
	if(pSurface->format == SFMT_P8) //paletted surfaces don't get color modifiers on them
	{
		while(!m_rasterLines.empty())
		{
			for(unsigned int i = m_rasterLines.front().least; i < m_rasterLines.front().greatest; i++)
			{
				pixelPos.x = i;
				pixelPos.y = m_rasterLines.front().row * pSurface->width;
				interpolateTexCoord(pixelTex, pixelPos, pTriangle);
				pixelColor = m_sampler.sample(pSurface, pixelTex.u, pixelTex.v);

				pSurface->pPaletteData[m_rasterLines.front().row * pSurface->width + i] = (SurfaceData8)pixelColor;
				m_rasterLines.pop();
			}
		}
	}
	else if(pSurface->format == SFMT_A8R8G8B8)
	{
		while(!m_rasterLines.empty())
		{
			for(unsigned int i = m_rasterLines.front().least; i < m_rasterLines.front().greatest; i++)
			{
				pixelPos.x = i;
				pixelPos.y = m_rasterLines.front().row * pSurface->width;
				interpolateColor(pixelColorModifier, pixelPos, pTriangle);
				interpolateTexCoord(pixelTex, pixelPos, pTriangle);
				pixelColor = m_sampler.sample(pSurface, pixelTex.u, pixelTex.v);

				pSurface->pColorData[m_rasterLines.front().row * pSurface->width + i] = (SurfaceData32)pixelColor;
				m_rasterLines.pop();
			}
		}
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