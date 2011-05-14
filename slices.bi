#ifndef SLICES_BI
#define SLICES_BI
'OHRRPGCE GAME - Slice related functionality
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't very crappy

#include "udts.bi"
#include "common.bi"
#include "reload.bi"

CONST ENABLE_SLICE_DEBUG = NO

'These constants also need to be updated in slices.bas and plotscr.hsd
'and plotdict.xml. You may choose to just update them here and then run
' misc/sl_lookup.py
'to update the other files. Be aware that once you have picked a number
'for one of these, you should not change it, because it could get used
'as a constant in someone's script, and changing it would break any
'non-recompiled script that used it.

'<SLICE LOOKUP CODES>
CONST SL_TEXTBOX_TEXT     = -100001
CONST SL_TEXTBOX_PORTRAIT = -100002
CONST SL_TEXTBOX_CHOICE0  = -100003
CONST SL_TEXTBOX_CHOICE1  = -100004
CONST SL_SCRIPT_LAYER     = -100005
CONST SL_TEXTBOX_LAYER    = -100006
CONST SL_STRING_LAYER     = -100007
CONST SL_MAPROOT          = -100008
CONST SL_OBSOLETE_OVERHEAD = -100009
CONST SL_MAP_LAYER0       = -101000
CONST SL_MAP_LAYER1       = -101001
CONST SL_MAP_LAYER2       = -101002
CONST SL_MAP_LAYER3       = -101003
CONST SL_MAP_LAYER4       = -101004
CONST SL_MAP_LAYER5       = -101005
CONST SL_MAP_LAYER6       = -101006
CONST SL_MAP_LAYER7       = -101007
'</SLICE LOOKUP CODES>

Enum SliceTypes
 slRoot
 slSpecial
 slContainer
 slRectangle
 slSprite
 slText
 slMenu
 slMenuItem
 slMap
 slGrid
End Enum

Enum AttachTypes
 slSlice
 slScreen
End Enum

Type SliceFwd as Slice
Type SliceDraw as Sub(Byval as SliceFwd ptr, byval stupidPage as integer)
Type SliceDispose as Sub(Byval as SliceFwd ptr)
Type SliceClone as Sub(Byval as SliceFwd ptr, byval as SliceFwd ptr)
Type SliceSave as Sub(Byval as SliceFwd ptr, byval node as Reload.Nodeptr)
Type SliceLoad as Sub(Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
Type SliceChildRefresh as Sub(Byval par as SliceFwd ptr, Byval ch as SliceFwd ptr)
Type SliceChildDraw as Sub(Byval s as SliceFwd ptr, Byval page as integer)

TYPE Slice
  Parent as Slice Ptr
  FirstChild as Slice Ptr
  NextSibling as Slice Ptr
  PrevSibling as Slice Ptr
  NumChildren as Integer
  
  X as integer 'the X,Y relative to whatever the slice is attached to
  Y as integer
  ScreenX as integer 'the actual X,Y, updated every frame
  ScreenY as integer
  Width as integer
  Height as integer
  Visible as integer
  Mobile as integer
  Clip as integer
  
  'moving at a constant pixels-per-tick speed (direct setting should cancel targ)
  Velocity as XYPair
  'limit the number of cycles to apply velocity before auto-clearing it (-1 means forever, 0 clears instantly)
  VelTicks as XYPair

  'moving to a destination in constant time. (replaces velocity)
  Targ as XYPair
  TargResidue_X as double
  TargResidue_Y as double
  TargTicks as integer

  TableSlot as integer 'which slot in plotslices() holds a reference to this slice, or 0 for none
  Lookup As integer

  Sorter as integer 'Only used by CustomSortChildSlices
  Extra(2) as integer
  
  AlignHoriz as integer 'Relative to parent. 0,1,2=Left,Mid,Right. Only used when .Fill = NO
  AlignVert as integer  'Relative to parent. 0,1,2=Top,Mid,Bottom. Only used when .Fill = NO
  AnchorHoriz as integer 'Relative to self. 0,1,2=Left,Mid,Right. Only used when .Fill = NO
  AnchorVert as integer  'Relative to self. 0,1,2=Top,Mid,Bottom. Only used when .Fill = NO
  
  as integer PaddingTop, PaddingLeft, PaddingRight, PaddingBottom
  
  Fill as integer
  
  Attach as AttachTypes
  Union
   Attached as Slice ptr
  End Union
  
  Draw as SliceDraw
  Dispose as SliceDispose
  Clone as SliceClone
  Save as SliceSave
  Load as SliceLoad
  ChildRefresh as SliceChildRefresh
  ChildDraw as SliceChildDraw
  
  SliceData as any ptr
  SliceType as SliceTypes
  
  'whatever else
  
END TYPE

TYPE SliceTable_
  root AS Slice Ptr
  maproot AS Slice Ptr
  maplayer(maplayerMax) AS Slice Ptr
  obsoleteoverhead AS Slice Ptr
  scriptsprite AS Slice Ptr
  textbox AS Slice Ptr
  menu AS Slice Ptr
  scriptstring AS Slice Ptr
END TYPE

'--Data containers for various slice types

TYPE RectangleSliceData
 'If any of fgcol, bgcol or border are manually changed, set style=-1 and style_loaded=0
 fgcol as integer
 border as integer 'Should default to -1
 translucent as RectTransTypes
 fuzzfactor as integer 'Should default to 50
 bgcol as integer
 'if style is changed then set style_loaded = NO
 style as integer 'Should default to -1
 style_loaded as integer 'Used internally flag whether a change of style has been applied to fgcol, bgcol, or border
 'Declare constructor (byval style as integer = -1, byval bgcol as integer=0, byval translucent as integer = NO, byval fgcol as integer = -1, byval border as integer = -1)
END TYPE

Type TextSliceData
 col as integer
 bgcol as integer
 outline as integer
 s as String
 'lines() as string
 wrap as integer
 'Declare constructor(st as string, byval col as integer = -1, byval ol as integer = YES)
 insert as integer 'char offset of insertion pointer
 show_insert as integer ' set to YES to display insertion point
 insert_tog as integer 'flashing
 first_line as integer 'used in scrolling
 line_limit as integer 'use to stop wrapping text from flowing too far down. 0 is no limit
                       '-1 can be used to hide all lines
 line_count as integer 'automatically populated when the slice changes
End Type

CONST sprTypeMXS = 9   'can't change this
CONST sprTypeLast = 9   'last sprite type selectable in slice editor
CONST sprTypeFrame = 10  'free to change this later (never saved)

'FIXME: Support for modifying sprites and flipping is pretty tacked on; generalise!
Type SpriteSliceData
 spritetype AS INTEGER 'PT0 thru PT8, 9=MXS, 10=loaded from Frame
 record AS INTEGER     'meaningless if spritetype is sptTypeFrame
 paletted AS INTEGER   'UNSAVED: YES: 4-bit, NO: 8-bit  (could remove this when 256-colour palettes added, or change meaning)
 pal AS INTEGER     '(UNSAVED if unpaletted) Set pal to -1 for the default. Ignored for unpaletted
 trans AS INTEGER   'Draw transparently?
 frame AS INTEGER   'Currently displaying frame
 flipHoriz AS INTEGER  'NO normal, YES horizontally flipped
 flipVert AS INTEGER   'NO normal, YES horizontally flipped
 loaded AS INTEGER  'UNSAVED: Set to NO to force a re-load on the next draw
 img AS GraphicPair 'UNSAVED: No need to manually populate this, done in draw (.pal = NULL for unpaletted)
End Type

'Shows the currently loaded map at the given slice pos
'Doesn't yet have the ability to load other non-current maps
Type MapSliceData
 'FIXME: Should I even use this at all in this early
 'incarnation? maybe not yet. (It certainly was a huge hassle when rewriting up tilemap stuff)
 map AS INTEGER 'Currently read-only informational
 transparent AS INTEGER 'Whether or not color 0 is transparent
 overlay AS INTEGER 'For backcompat with layers that observe the old overlay feature.
 tileset as TilesetData ptr 'NOTE: ptr to the same memory pointed to by the ptrs in the tilesets() array in game.bas (Not owned!)
 tiles as TileMap ptr 'NOTE: ptr to one of maptiles() in game.bas (Not owned!)
 pass as TileMap ptr 'NOTE: ptr to pass in game.bas (Not owned!) May be NULL for non-overhead layers
End Type

Type MenuSliceData
 selected as integer
 tog as integer
End Type

Type MenuItemSliceData
 ordinal as integer
 caption as string
 disabled as integer
End Type

Type GridSliceData
 show as integer
 rows as integer
 cols as integer
End Type

DECLARE Sub SetupGameSlices
DECLARE Sub DestroyGameSlices(Byval dumpdebug AS INTEGER=0)
DECLARE Function NewSlice(Byval parent as Slice ptr = 0) as Slice Ptr
DECLARE Sub DeleteSlice(Byval s as Slice ptr ptr, byval debugme as integer = 0)
DECLARE Sub DeleteSliceChildren(Byval s as Slice ptr)
DECLARE Sub DrawSlice(byval s as slice ptr, byval page as integer)
DECLARE Sub AdvanceSlice(byval s as slice ptr)
DECLARE Sub OrphanSlice(byval sl as slice ptr)
DECLARE Sub SetSliceParent(byval sl as slice ptr, byval parent as slice ptr)
DECLARE Sub ReplaceSliceType(byval sl as slice ptr, byref newsl as slice ptr)
DECLARE Sub InsertSliceBefore(byval sl as slice ptr, byval newsl as slice ptr)
DECLARE Sub SwapSiblingSlices(byval sl1 as slice ptr, byval sl2 as slice ptr)
DECLARE Function LookupSlice OVERLOAD (byval lookup_code as integer) as slice ptr
DECLARE Function LookupSlice OVERLOAD (byval lookup_code as integer, byval start_sl as slice ptr) as slice ptr
DECLARE Function LastChild(byval parent as slice ptr) as slice ptr
DECLARE Function verifySliceLineage(byval sl as slice ptr, parent as slice ptr) as integer
DECLARE FUNCTION SliceTypeName OVERLOAD (sl AS Slice Ptr) AS STRING
DECLARE FUNCTION SliceTypeName OVERLOAD (t AS SliceTypes) AS STRING
DECLARE FUNCTION SliceLookupCodename OVERLOAD (sl AS Slice Ptr) AS STRING
DECLARE FUNCTION SliceLookupCodename OVERLOAD (BYVAL code AS INTEGER) AS STRING
DECLARE Sub RefreshSliceScreenPos(byval sl as slice ptr)
DECLARE Function SliceXAnchor(BYVAL sl AS Slice Ptr) AS INTEGER
DECLARE Function SliceYAnchor(BYVAL sl AS Slice Ptr) AS INTEGER
DECLARE Function SliceEdgeX(BYVAL sl AS Slice Ptr, BYVAL edge AS INTEGER) AS INTEGER
DECLARE Function SliceEdgeY(BYVAL sl AS Slice Ptr, BYVAL edge AS INTEGER) AS INTEGER
DECLARE Function SliceCollide(byval sl1 as Slice Ptr, sl2 as Slice Ptr) as integer
DECLARE Function SliceCollidePoint(byval sl as Slice Ptr, byval x as integer, byval y as integer) as integer
DECLARE Function SliceContains(byval sl1 as Slice Ptr, byval sl2 as Slice Ptr) as integer
DECLARE Function FindSliceCollision(byval parent as Slice Ptr, byval sl as Slice Ptr, byref num as integer, byval descend as integer) as Slice Ptr
DECLARE Function FindSliceAtPoint(byval parent as Slice Ptr, byval x as integer, byval y as integer, byref num as integer, byval descend as integer) as Slice Ptr
DECLARE Sub SliceClamp(byval sl1 as Slice Ptr, byval sl2 as Slice Ptr)
DECLARE Sub YSortChildSlices(byval parent as slice ptr)
DECLARE Sub CustomSortChildSlices(byval parent as slice ptr, byval wipevals as integer)
DECLARE Function CloneSliceTree(byval sl as slice ptr) as slice ptr
DECLARE Sub SetSliceTarg(byval s as slice ptr, byval x as integer, byval y as integer, byval ticks as integer)

DECLARE FUNCTION NewSliceOfType (BYVAL t AS SliceTypes, BYVAL parent AS Slice Ptr=0, BYVAL lookup_code AS INTEGER=0) AS Slice Ptr

DECLARE SUB SliceDebugRemember(sl AS Slice Ptr)
DECLARE SUB SliceDebugForget(sl AS Slice Ptr)
DECLARE SUB SliceDebugDump(noisy AS INTEGER = NO)
DECLARE SUB SliceDebugDumpTree(sl as Slice Ptr, indent as integer = 0)
DECLARE FUNCTION SliceDebugCheck(sl as Slice Ptr) AS INTEGER

DECLARE Function NewRectangleSlice(byval parent as Slice ptr, byref dat as RectangleSliceData) as slice ptr
DECLARE Sub ChangeRectangleSlice(byval sl as slice ptr,_
                      byval style as integer=-2,_
                      byval bgcol as integer=-1,_
                      byval fgcol as integer=-1,_
                      byval border as integer=-3,_
                      byval translucent as RectTransTypes=transUndef,_
                      byval fuzzfactor as integer=0)

DECLARE Function NewTextSlice(byval parent as Slice ptr, byref dat as TextSliceData) as slice ptr
DECLARE Function NewMenuSlice(byval parent as Slice ptr, byref dat as MenuSliceData) as slice ptr
DECLARE Function NewMenuItemSlice(byval parent as Slice ptr, byref dat as MenuItemSliceData) as slice ptr
DECLARE Sub UpdateTextSlice(byval sl as slice ptr)
DECLARE Sub ChangeTextSlice(byval sl as slice ptr,_
                      s as string=CHR(1) & CHR(255),_
                      byval col as integer=-1,_
                      byval outline as integer=-2,_
                      byval wrap as integer=-2,_
                      byval bgcol as integer=-1)

DECLARE Sub DisposeSpriteSlice(byval sl as slice ptr)
DECLARE Sub DrawSpriteSlice(byval sl as slice ptr, byval p as integer)
DECLARE Function GetSpriteSliceData(byval sl as slice ptr) as SpriteSliceData ptr
DECLARE Sub SetSpriteToFrame(byval sl as slice ptr, byval fr as Frame ptr, byval pal as integer)
DECLARE Function NewSpriteSlice(byval parent as Slice ptr, byref dat as SpriteSliceData) as slice ptr
DECLARE Sub ChangeSpriteSlice(byval sl as slice ptr,_
                      byval spritetype as integer=-1,_
                      byval record as integer=-1,_
                      byval pal as integer = -2,_
                      byval frame as integer = -1,_
                      byval fliph as integer = -2,_
                      byval flipv as integer = -2,_
                      byval trans as integer = -2)  ' All arguments default to no change

DECLARE Sub DisposeMapSlice(byval sl as slice ptr)
DECLARE Sub DrawMapSlice(byval sl as slice ptr, byval p as integer)
DECLARE Function GetMapSliceData(byval sl as slice ptr) as MapSliceData ptr
DECLARE Function NewMapSlice(byval parent as Slice ptr, byref dat as MapSliceData) as slice ptr
DECLARE Sub ChangeMapSliceTileset (byval sl as slice ptr, byval tileset as TilesetData ptr)
DECLARE Sub ChangeMapSlice (byval sl as slice ptr,_
                   byval tiles as TileMap ptr=cast(TileMap ptr, 1),_
                   byval pass as TileMap ptr=cast(TileMap ptr, 1),_
                   byval transparent as integer=-2,_
                   byval overlay as integer=-1) ' All arguments default to no change (explaining weird tiles default)

DECLARE Function NewGridSlice(byval parent as Slice ptr, byref dat as GridSliceData) as slice ptr
DECLARE Sub ChangeGridSlice(byval sl as slice ptr,_
                      byval rows as integer=0,_
                      byval cols as integer=0)

'--Saving and loading slices
DECLARE Sub SliceSaveToNode(BYVAL sl AS Slice Ptr, node AS Reload.Nodeptr)
DECLARE Sub SliceSaveToFile(BYVAL sl AS Slice Ptr, filename AS STRING)
DECLARE Sub SliceLoadFromNode(BYVAL sl AS Slice Ptr, node AS Reload.Nodeptr)
DECLARE Sub SliceLoadFromFile(BYVAL sl AS Slice Ptr, filename AS STRING)


EXTERN AS SliceTable_ SliceTable

'NEW SLICE TYPE TEMPLATE
'INSTRUCTIONS: Copy the following block into Slices.bas.
' Then, select the block, and use Find and Replace to switch
' <TYPENAME> with whatever name you need. Then, add the drawing code to
' Draw<TYPENAME>Slice.
/'
'==START OF <TYPENAME>SLICEDATA
Sub Dispose<TYPENAME>Slice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as <TYPENAME>SliceData ptr = cptr(<TYPENAME>SliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub Draw<TYPENAME>Slice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as <TYPENAME>SliceData ptr = cptr(<TYPENAME>SliceData ptr, sl->SliceData)

 '''DRAWING CODE GOES HERE!
end sub

Function Get<TYPENAME>SliceData(byval sl as slice ptr) as <TYPENAME>SliceData ptr
 return sl->SliceData
End Function

Sub Save<TYPENAME>Slice(byval sl as slice ptr, byref f as SliceFileWrite)
 DIM dat AS <TYPENAME>SliceData Ptr
 dat = sl->SliceData
 'WriteSliceFileVal f, "keyname", dat->datamember
End Sub

Function Load<TYPENAME>Slice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 dim dat AS <TYPENAME>SliceData Ptr
 dat = sl->SliceData
 select case key
  'case "keyname": dat->datamember = n
  case else: return NO
 end select
 return YES
End Function

Function New<TYPENAME>Slice(byval parent as Slice ptr, byref dat as <TYPENAME>SliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as <TYPENAME>SliceData ptr = new <TYPENAME>SliceData
 *d = dat
 
 ret->SliceType = sl<TYPENAME>
 ret->SliceData = d
 ret->Draw = @Draw<TYPENAME>Slice
 ret->Dispose = @Dispose<TYPENAME>Slice
 ret->Clone = @Clone<TYPENAME>Slice
 ret->Save = @Save<TYPENAME>Slice
 ret->Load = @Load<TYPENAME>Slice
 
 return ret
end function
'==END OF <TYPENAME>SLICEDATA
'/


#endif
