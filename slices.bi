#ifndef SLICES_BI
#define SLICES_BI
'OHRRPGCE GAME - Slice related functionality
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't very crappy

#include "udts.bi"

DECLARE Sub SetupGameSlices
DECLARE Sub DestroyGameSlices
DECLARE Function NewSlice(Byval parent as Slice ptr = 0) as Slice Ptr
DECLARE Sub DeleteSlice(Byval s as Slice ptr ptr)

TYPE SliceTable_
  root AS Slice Ptr
  map  AS Slice Ptr
  scriptsprite AS Slice Ptr
  textbox AS Slice Ptr
  menu AS Slice Ptr
  scriptstring AS Slice Ptr
END TYPE

TYPE RectangleSliceData
 fgcol as integer
 bgcol as integer
 transparent as integer
 border as integer
 
END TYPE

EXTERN Slices() as Slice ptr
EXTERN AS SliceTable_ SliceTable

#endif