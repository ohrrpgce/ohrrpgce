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
	if(pSurface->format == SF_8bit)
		color = pSurface->pPaletteData[(int)v * pSurface->width + (int)u];
	else
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



//void TriRasterizer::calculateTriangleRect(ClippingRect &clipOut, const Triangle* pTriangle)
//{
//	clipOut.left = (pTriangle->pnt[0].pos.x < pTriangle->pnt[1].pos.x ? pTriangle->pnt[0].pos.x : pTriangle->pnt[1].pos.x);
//	clipOut.left = (pTriangle->pnt[2].pos.x < (float)clipOut.left ? pTriangle->pnt[2].pos.x : clipOut.left);
//
//	clipOut.right = (pTriangle->pnt[0].pos.x > pTriangle->pnt[1].pos.x ? pTriangle->pnt[0].pos.x : pTriangle->pnt[1].pos.x);
//	clipOut.right = (pTriangle->pnt[2].pos.x > (float)clipOut.right ? pTriangle->pnt[2].pos.x : clipOut.right);
//
//	clipOut.top = (pTriangle->pnt[0].pos.y < pTriangle->pnt[1].pos.y ? pTriangle->pnt[0].pos.y : pTriangle->pnt[1].pos.y);
//	clipOut.top = (pTriangle->pnt[2].pos.y < (float)clipOut.top ? pTriangle->pnt[2].pos.y : clipOut.top);
//
//	clipOut.bottom = (pTriangle->pnt[0].pos.y > pTriangle->pnt[1].pos.y ? pTriangle->pnt[0].pos.y : pTriangle->pnt[1].pos.y);
//	clipOut.bottom = (pTriangle->pnt[2].pos.y > (float)clipOut.bottom ? pTriangle->pnt[2].pos.y : clipOut.bottom);
//}

//void TriRasterizer::interpolateTexCoord(TexCoord &texOut, const TexCoord &t1, const TexCoord &t2, float scale)
//{
//	TexCoord out;
//	float invScale(-scale + 1);
//
//	out.u = scale * t1.u + invScale * t2.u;
//	out.v = scale * t1.v + invScale * t2.v;
//
//	texOut = out;
//}
//
//Color TriRasterizer::interpolateColor(Color c1, Color c2, float scale)
//{
//	c1.scale(c2, scale * 255.0f);
//	return c1;
//}

template <class T_VertexType>
void TriRasterizer::calculateRasterPixels(std::queue< DrawingRange<T_VertexType> >& rasterLinesOut, const Surface* pSurface, const Triangle<T_VertexType>* pTriangle, ClippingRect& clip)
{//done?
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
	T_VertexType leftVertex, rightVertex;
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
		//interpolateTexCoord(leftVertex.tex, pTriangle->pnt[leftMostIndex].tex, pTriangle->pnt[(leftMostIndex+1)%3].tex, scale);
		leftVertex = pTriangle->pnt[leftMostIndex];
		leftVertex.interpolateComponents( pTriangle->pnt[(leftMostIndex+1)%3], scale );
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
		//interpolateTexCoord(rightVertex.tex, pTriangle->pnt[rightMostIndex].tex, pTriangle->pnt[(rightMostIndex+1)%3].tex, scale);
		rightVertex = pTriangle->pnt[rightMostIndex];
		rightVertex.interpolateComponents( pTriangle->pnt[(rightMostIndex+1)%3], scale );
		rightVertex.pos.x = xIntersection[rightMostIndex];
		rightVertex.pos.y = row;

		//perform horizontal clipping
		if(leftVertex.pos.x > clip.right || rightVertex.pos.x < clip.left)
			continue;
		
		if(leftVertex.pos.x < clip.left)
		{
			scale = (clip.left - leftVertex.pos.x) / (rightVertex.pos.x - leftVertex.pos.x);
			//interpolateVertices(leftVertex, leftVertex, rightVertex, 1-scale);
			//interpolateTexCoord(leftVertex.tex, leftVertex.tex, rightVertex.tex, 1-scale);
			leftVertex.interpolateComponents( rightVertex, 1-scale );
			leftVertex.pos.x = clip.left;
			//leftVertex.pos.y = row;
		}
		if(rightVertex.pos.x > clip.right)
		{
			scale = (clip.right - rightVertex.pos.x) / (leftVertex.pos.x - rightVertex.pos.x);
			//interpolateVertices(rightVertex, rightVertex, leftVertex, 1-scale);
			//interpolateTexCoord(rightVertex.tex, rightVertex.tex, leftVertex.tex, 1-scale);
			rightVertex.interpolateComponents( leftVertex, 1-scale );
			rightVertex.pos.x = clip.right;
			//rightVertex.pos.y = row;
		}

		//push the data onto the raster queue
		rasterLinesOut.push( DrawingRange<T_VertexType>(leftVertex, rightVertex) );
	}
}

void TriRasterizer::rasterColor(Surface* pSurfaceDest, const DrawingRange<VertexC>& range, Color argbModifier)
{//done
	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x);

	if(pSurfaceDest->format == SF_8bit)
	{
		for(int i = start; i <= finish; i++)
		{
			pSurfaceDest->pPaletteData[(int)range.least.pos.y * pSurfaceDest->width + i] = argbModifier;
		}
	}
	else
	{
		Color finalColor;
		float length(range.greatest.pos.x - range.least.pos.x+1), weight;

		for(int i = start; i <= finish; i++)
		{
			weight = 255.0f*(finish-i) / length;
			finalColor = range.least.col;
			finalColor.scale(range.greatest.col, weight);
			finalColor.scale(argbModifier);
			pSurfaceDest->pColorData[(int)range.least.pos.y * pSurfaceDest->width + i] = finalColor;
		}
	}
}

void TriRasterizer::rasterTexture(Surface* pSurfaceDest, const DrawingRange<VertexT>& range, const Surface* pTexture, const Palette* pPalette, Color argbModifier)
{//done
	//assumed that surface dest is not 8bit if source is 32bit
	//also assumed that if dest is 32bit and source is 8bit, a palette was passed in

	TexCoord texel;
	float length(range.greatest.pos.x - range.least.pos.x+.5f), 
		  weightFirst,
		  weightSecond;

	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x);

	Color srcColor, destColor, finalColor, vertexColor;
	//float red, green, blue;

	if(pSurfaceDest->format == SF_8bit) //both surfaces are 8bit
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;

			srcColor = m_sampler.sample(pTexture, texel.u, texel.v);
			//if(srcColor.dw == 0x0)//color key test
			//	continue;
			pSurfaceDest->pPaletteData[(int)range.least.pos.y * pSurfaceDest->width + i] = srcColor; 
		}
	}
	else if(pTexture->format == SF_32bit) //both surfaces are 32bit
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
			srcColor = m_sampler.sample(pTexture, texel.u, texel.v);
			srcColor.scale(argbModifier);
			srcColor.scale(vertexColor);
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
	else //texture is 8bit, destination is 32bit
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
			srcColor = pPalette->p[ m_sampler.sample(pTexture, texel.u, texel.v) ];
			srcColor.scale(argbModifier);
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

void TriRasterizer::rasterTextureWithColorKey(Surface *pSurfaceDest, const DrawingRange<VertexT> &range, const Surface *pTexture, const Palette *pPalette, uint8_t colorKey, Color argbModifier)
{//done
	//assumed that surface source is 8bit
	//also assumed that if dest is 32bit, a palette was passed in

	TexCoord texel;
	float length(range.greatest.pos.x - range.least.pos.x+.5f), 
		  weightFirst,
		  weightSecond;

	int start = 0, finish = 0;

	start = (range.least.pos.x < 0 ? 0 : range.least.pos.x+.5f); //add .5f to help with rounding trouble
	finish = (range.greatest.pos.x >= pSurfaceDest->width ? pSurfaceDest->width-1 : range.greatest.pos.x);

	Color srcColor, destColor, finalColor, vertexColor;

	if(pSurfaceDest->format == SF_8bit) //both surfaces are 8bit
	{
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;

			srcColor = m_sampler.sample(pTexture, texel.u, texel.v);
			if(srcColor.dw == 0x0)//color key test
				continue;
			pSurfaceDest->pPaletteData[(int)range.least.pos.y * pSurfaceDest->width + i] = srcColor; 
		}
	}
	else //texture is 8bit, destination is 32bit
	{
		uint8_t value = 0;
		for(int i = start; i <= finish; i++)
		{
			weightFirst = (range.greatest.pos.x - i) / (float)length;
			weightSecond = 1 - weightFirst;
			texel.u = weightFirst * range.least.tex.u + weightSecond * range.greatest.tex.u;
			texel.v = weightFirst * range.least.tex.v + weightSecond * range.greatest.tex.v;
			vertexColor = range.least.col;
			vertexColor.scale(range.greatest.col, 255.0f * weightFirst);

			//alpha blending
			value = m_sampler.sample(pTexture, texel.u, texel.v);
			if(value == colorKey)
				continue;
			srcColor = pPalette->p[ value ];
			srcColor.scale(argbModifier);
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

void TriRasterizer::drawColor(Surface* pSurface, SurfaceRect* pRect, const Triangle<VertexC>* pTriangle, Color argbModifier)
{//done
	if(pSurface == NULL || pTriangle == NULL)
		return;

	//determine rasterizing region
	ClippingRect clip = {(float)pRect->left, (float)pRect->top, (float)pRect->right, (float)pRect->bottom};
	if(clip.left < 0.0f) clip.left = 0.0f;
	if(clip.top < 0.0f) clip.top = 0.0f;
	if(clip.right >= pSurface->width) clip.right = pSurface->width-1;
	if(clip.bottom >= pSurface->height) clip.bottom = pSurface->width-1;
	std::queue< DrawingRange< VertexC > > rasterLines;
	calculateRasterPixels(rasterLines, pSurface, pTriangle, clip);

	//rasterize the polygon
	while(!rasterLines.empty())
	{
		rasterColor(pSurface, rasterLines.front(), argbModifier);
		rasterLines.pop();
	}
}

void TriRasterizer::drawTexture(Surface* pSurface, SurfaceRect* pRect, const Triangle<VertexT>* pTriangle, const Surface* pTexture, const Palette* pPalette, Color argbModifier)
{//done
	if(pSurface == NULL || pTriangle == NULL || pTexture == NULL)
		return;

	if(pSurface->format == SF_8bit && pTexture->format == SF_32bit)
		return; //invalid 32bit to 8bit conversion
	if(pSurface->format == SF_32bit && pTexture->format == SF_8bit)
		if( !pPalette )
			return; //need a palette to convert

	//determine rasterizing region
	ClippingRect clip = {(float)pRect->left, (float)pRect->top, (float)pRect->right, (float)pRect->bottom};
	if(clip.left < 0.0f) clip.left = 0.0f;
	if(clip.top < 0.0f) clip.top = 0.0f;
	if(clip.right >= pSurface->width) clip.right = pSurface->width-1;
	if(clip.bottom >= pSurface->height) clip.bottom = pSurface->width-1;
	std::queue< DrawingRange<VertexT> > rasterLines;
	calculateRasterPixels(rasterLines, pSurface, pTriangle, clip);

	//rasterize the polygon
	while(!rasterLines.empty())
	{
		rasterTexture(pSurface, rasterLines.front(), pTexture, pPalette, argbModifier);
		rasterLines.pop();
	}
}

void TriRasterizer::drawTextureWithColorKey(Surface *pSurface, SurfaceRect *pRect, const Triangle<VertexT> *pTriangle, const Surface *pTexture, const Palette *pPalette, uint8_t colorKey, Color argbModifier)
{//done
	if(pSurface == NULL || pTriangle == NULL || pTexture == NULL)
		return;

	if(pTexture->format == SF_32bit)
		return; //invalid, it must be a palette-type for color key checking
	if(pSurface->format == SF_32bit && pTexture->format == SF_8bit)
		if( !pPalette )
			return; //need a palette to convert

	//determine rasterizing region
	ClippingRect clip = {(float)pRect->left, (float)pRect->top, (float)pRect->right, (float)pRect->bottom};
	if(clip.left < 0.0f) clip.left = 0.0f;
	if(clip.top < 0.0f) clip.top = 0.0f;
	if(clip.right >= pSurface->width) clip.right = pSurface->width-1;
	if(clip.bottom >= pSurface->height) clip.bottom = pSurface->width-1;
	std::queue< DrawingRange<VertexT> > rasterLines;
	calculateRasterPixels(rasterLines, pSurface, pTriangle, clip);

	//rasterize the polygon
	while(!rasterLines.empty())
	{
		rasterTextureWithColorKey(pSurface, rasterLines.front(), pTexture, pPalette, colorKey, argbModifier);
		rasterLines.pop();
	}
}




void QuadRasterizer::generateTriangles(Triangle<VertexC>* pTriangles, const QuadC* pQuad)
{//done
	VertexC center;
	uint16_t a=0,r=0,g=0,b=0;
	for(int i = 0; i < 4; i++)
	{
		center.pos.x += pQuad->pnt[i].pos.x;
		center.pos.y += pQuad->pnt[i].pos.y;
		a += pQuad->pnt[i].col.a;
		r += pQuad->pnt[i].col.r;
		g += pQuad->pnt[i].col.g;
		b += pQuad->pnt[i].col.b;
	}
	center.pos.x /= 4;
	center.pos.y /= 4;
	a /= 4;
	r /= 4;
	g /= 4;
	b /= 4;
	center.col.a = a;
	center.col.r = r;
	center.col.g = g;
	center.col.b = b;

	for(int i = 0; i < 4; i++)
	{
		pTriangles[i].pnt[0] = pQuad->pnt[i];
		pTriangles[i].pnt[1] = pQuad->pnt[(i+1)%4];
		pTriangles[i].pnt[2] = center;
	}
}

void QuadRasterizer::generateTriangles(Triangle<VertexT> *pTriangles, const QuadT *pQuad)
{//done
	VertexT center;
	uint16_t a=0,r=0,g=0,b=0;
	for(int i = 0; i < 4; i++)
	{
		center.pos.x += pQuad->pnt[i].pos.x;
		center.pos.y += pQuad->pnt[i].pos.y;
		a += pQuad->pnt[i].col.a;
		r += pQuad->pnt[i].col.r;
		g += pQuad->pnt[i].col.g;
		b += pQuad->pnt[i].col.b;
		center.tex.u += pQuad->pnt[i].tex.u;
		center.tex.v += pQuad->pnt[i].tex.v;
	}
	center.pos.x /= 4;
	center.pos.y /= 4;
	a /= 4;
	r /= 4;
	g /= 4;
	b /= 4;
	center.col.a = a;
	center.col.r = r;
	center.col.g = g;
	center.col.b = b;
	center.tex.u /= 4;
	center.tex.v /= 4;

	for(int i = 0; i < 4; i++)
	{
		pTriangles[i].pnt[0] = pQuad->pnt[i];
		pTriangles[i].pnt[1] = pQuad->pnt[(i+1)%4];
		pTriangles[i].pnt[2] = center;
	}
}