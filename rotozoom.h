#ifndef ROTOZOOM_H
#define ROTOZOOM_H

#ifdef __cplusplus
extern "C" {
#endif

SDL_Surface *rotozoomSurfaceXY
        (SDL_Surface * src, double angle, double zoomx, double zoomy, int smooth);

void rotozoomSurfaceSizeXY
        (int width, int height, double angle, double zoomx, double zoomy, int *dstwidth, int *dstheight);

void zoomSurfaceSize(int width, int height, double zoomx, double zoomy, int *dstwidth, int *dstheight);

SDL_Surface* rotateSurface90Degrees(SDL_Surface* src, int numClockwiseTurns);

#ifdef __cplusplus
}
#endif

#endif
