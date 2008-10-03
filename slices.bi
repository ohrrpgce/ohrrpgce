#ifndef SLICES_BI
#define SLICES_BI
'OHRRPGCE GAME - Slice related functionality
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't very crappy

#include "udts.bi"

Enum SliceTypes
 slSpecial = 0
 slRectangle = 1
 slSprite = 2
 slText
End Enum

Type SliceFwd as Slice
Type SliceDraw as Sub(Byval as SliceFwd ptr, byval stupidPage as integer)
Type SliceDispose as Sub(Byval as SliceFwd ptr)

TYPE Slice
  Parent as Slice Ptr
  FirstChild as Slice Ptr
  NextSibling as Slice Ptr
  PrevSibling as Slice Ptr
  NumChildren as Integer
  
  X as integer
  Y as integer
  Width as integer
  Height as integer
  Visible as integer
  
  Draw as SliceDraw
  Dispose as SliceDispose
  SliceData as any ptr
  SliceType as SliceTypes
  
  'whatever else
  
END TYPE

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
 'Declare constructor (byval bgcol as integer, byval transparent as integer = YES, byval fgcol as integer = -1, byval border as integer = -1)
END TYPE

Type TextSliceData
 col as integer
 outline as integer
 s as String
 'Declare constructor(byval st as string, byval col as integer = -1, byval ol as integer = YES)
End Type


DECLARE Sub SetupGameSlices
DECLARE Sub DestroyGameSlices
DECLARE Function NewSlice(Byval parent as Slice ptr = 0) as Slice Ptr
DECLARE Sub DeleteSlice(Byval s as Slice ptr ptr)
DECLARE Sub DrawSlice(byval s as slice ptr, byval page as integer)

DECLARE Function NewRectangleSlice(byval parent as Slice ptr, byref dat as RectangleSliceData) as slice ptr

EXTERN Slices() as Slice ptr
EXTERN AS SliceTable_ SliceTable

#endif