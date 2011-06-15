#include "gfx_newRenderPlan.h"
#include "matrixMath.h"
#include "allmodex.h"

extern "C" {
	void frame_draw_transformed(Frame *dest, Frame *src, float3 vertices[4]);
}

void frame_draw_transformed(Frame *dest, Frame *src, float3 vertices[4]) {
	QuadT quad;
	quad.pnt[0].tex = TexCoord(0, 0);
	quad.pnt[1].tex = TexCoord(1, 0);
	quad.pnt[2].tex = TexCoord(1, 1);
	quad.pnt[3].tex = TexCoord(0, 1);
	for (int i = 0; i < 4; i++) {
		quad.pnt[i].col = 0xffffffff;
		quad.pnt[i].pos.x = vertices[i].x;
		quad.pnt[i].pos.y = vertices[i].y;
	}
	Surface srcsurf, destsurf;
	srcsurf.width = src->w;
	srcsurf.height = src->h;
	srcsurf.format = SF_8bit;
	srcsurf.usage = SU_Source;
	srcsurf.pPaletteData = src->image;
	destsurf.width = dest->w;
	destsurf.height = dest->h;
	destsurf.format = SF_8bit;
	destsurf.usage = SF_RenderTarget;
	destsurf.pPaletteData = dest->image;
	gfx_renderQuadTexture( &quad, &srcsurf, 0, 0xffffffff, 0, &destsurf );
}
