//slices.h
//2/28/12
//translates important components of slices.bi for cross-language interfacing

#ifndef SLICES_H
#define SLICES_H

extern "C" {

typedef void* SlicePtr;

SlicePtr NewSlice( SlicePtr parent = 0 );
void DeleteSlice( SlicePtr* ps, int debugme = 0 );
void DrawSlice( SlicePtr s, int page );
void OrphanSlice( SlicePtr sl );
void SetSliceParent( SlicePtr sl, SlicePtr parent );
void InsertSliceBefore( SlicePtr sl, SlicePtr newsl );
void SwapSiblingSlices( SlicePtr sl1, SlicePtr sl2 );
SlicePtr FindSliceAtPoint( SlicePtr parent, int x, int y, int& num, int descend );

SlicePtr sliceGetParent( SlicePtr s );
SlicePtr sliceGetFirstChild( SlicePtr s );
SlicePtr sliceGetNextSibling( SlicePtr s );
SlicePtr sliceGetPrevSibling( SlicePtr s );
int sliceGetNumChildren( SlicePtr s );
int sliceGetX( SlicePtr s );
int sliceGetY( SlicePtr s );
int sliceGetScreenX( SlicePtr s );
int sliceGetScreenY( SlicePtr s );
int sliceGetWidth( SlicePtr s );
int sliceGetHeight( SlicePtr s );
int sliceIsVisible( SlicePtr s );
int sliceIsMobile( SlicePtr s );
int sliceIsClipping( SlicePtr s );

void sliceSetX( int x, SlicePtr s );
void sliceSetY( int y, SlicePtr s );
void sliceSetScreenX( int x, SlicePtr s );
void sliceSetScreenY( int y, SlicePtr s );
void sliceSetWidth( int w, SlicePtr s );
void sliceSetHeight( int h, SlicePtr s );
void sliceSetVisibility( int b, SlicePtr s );
void sliceSetMobility( int b, SlicePtr s );
void sliceSetClipping( int b, SlicePtr s );

}

#endif