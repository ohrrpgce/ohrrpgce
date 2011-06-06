#include "rasterizer.h"
#include "matrixMath.h"
#include "allmodex.h"

extern "C" {
	void frame_draw_transformed(Frame *dest, Frame *src, float3 vertices[4]);
}

void frame_draw_transformed(Frame *dest, Frame *src, float3 vertices[4]) {
	QuadRasterizer rasterizer;
	Quad quad;
	quad.pnt[0].tex = TexCoord(0, 0);
	quad.pnt[1].tex = TexCoord(1, 0);
	quad.pnt[2].tex = TexCoord(1, 1);
	quad.pnt[3].tex = TexCoord(0, 1);
	for (int i = 0; i < 4; i++) {
		quad.pnt[i].pos.x = vertices[i].x;
		quad.pnt[i].pos.y = vertices[i].y;
	}
	Surface srcsurf, destsurf;
	srcsurf.width = src->w;
	srcsurf.height = src->h;
	srcsurf.format = SFMT_P8;
	srcsurf.pPaletteData = src->image;
	destsurf.width = dest->w;
	destsurf.height = dest->h;
	destsurf.format = SFMT_P8;
	destsurf.pPaletteData = dest->image;
	rasterizer.drawTexture(&destsurf, &quad, &srcsurf);
}
