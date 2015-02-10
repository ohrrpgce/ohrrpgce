//slices.h
//2/28/12
//translates important components of slices.bi for cross-language interfacing

#ifndef SLICES_H
#define SLICES_H

#ifdef __cplusplus
extern "C" {
#endif

typedef void* SlicePtr;

SlicePtr NewSlice( SlicePtr parent = 0 );
void DeleteSlice( SlicePtr* ps, int debugme = 0 );
void DrawSlice( SlicePtr s, int page );
void OrphanSlice( SlicePtr sl );
void SetSliceParent( SlicePtr sl, SlicePtr parent );
void InsertSliceBefore( SlicePtr sl, SlicePtr newsl );
void SwapSiblingSlices( SlicePtr sl1, SlicePtr sl2 );
SlicePtr FindSliceAtPoint( SlicePtr parent, int x, int y, int& num, int descend );
SlicePtr LookupSlice( int lookup_code, SlicePtr start_sl = 0 );
void RefreshSliceScreenPos( SlicePtr sl );

SlicePtr SliceGetParent( SlicePtr s );
SlicePtr SliceGetFirstChild( SlicePtr s );
SlicePtr SliceGetLastChild( SlicePtr s );
SlicePtr SliceGetNextSibling( SlicePtr s );
SlicePtr SliceGetPrevSibling( SlicePtr s );
int SliceGetNumChildren( SlicePtr s );
int SliceGetX( SlicePtr s );
int SliceGetY( SlicePtr s );
int SliceGetScreenX( SlicePtr s );
int SliceGetScreenY( SlicePtr s );
int SliceGetWidth( SlicePtr s );
int SliceGetHeight( SlicePtr s );
int SliceIsVisible( SlicePtr s );
int SliceIsMobile( SlicePtr s );
int SliceIsClipping( SlicePtr s );

void SliceSetX( SlicePtr s, int x );
void SliceSetY( SlicePtr s, int y );
void SliceSetWidth( SlicePtr s, int w );
void SliceSetHeight( SlicePtr s, int h );
void SliceSetVisibility( SlicePtr s, int b );
void SliceSetMobility( SlicePtr s, int b );
void SliceSetClipping( SlicePtr s, int b );

#ifdef __cplusplus
}
#endif

#endif
