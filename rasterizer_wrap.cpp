#include "gfx_newRenderPlan.h"
#include "matrixMath.h"
#include "allmodex.h"

extern "C" {
	void frame_draw_transformed(Frame *dest, Frame *src, float3 vertices[4]);
}

void frame_draw_transformed(Frame *dest, Frame *src, float3 vertices[4]) {
	VertexPT quad[4];
	quad[0].tex = TexCoord(0, 0);
	quad[1].tex = TexCoord(1, 0);
	quad[2].tex = TexCoord(1, 1);
	quad[3].tex = TexCoord(0, 1);
	for (int i = 0; i < 4; i++) {
		quad[i].col = 0xffffffff;
		quad[i].pos.x = vertices[i].x;
		quad[i].pos.y = vertices[i].y;
	}
	Surface srcsurf, destsurf;
	srcsurf.width = src->w;
	srcsurf.height = src->h;
	srcsurf.format = SF_8bit;
	srcsurf.usage = SU_Source;
	srcsurf.pPaletteData = src->image;
	destsurf.width = dest->w;
	destsurf.height = dest->h;
	destsurf.format = SF_8bit; //this is going to fail now in the render call
	destsurf.usage = SU_RenderTarget;
	destsurf.pPaletteData = dest->image;
	gfx_renderQuadTexture( quad, &srcsurf, 0, 0x0, 0xffffffff, 0, &destsurf );
}
