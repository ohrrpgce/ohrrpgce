'OHRRPGCE GAME - Slice related functionality
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't very crappy
'
#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"

#include "slices.bi"

#ifdef IS_GAME
extern plotslices() as slice ptr
#endif

'==============================================================================

'Reload helper functions used by saving/loading
DECLARE Sub SaveProp OVERLOAD (node as Reload.Nodeptr, propname as string, byval value as integer)
DECLARE Sub SaveProp OVERLOAD (node as Reload.Nodeptr, propname as string, s as string)

EXTERN "C"

DECLARE Function LoadPropStr(node as Reload.Nodeptr, propname as string, defaultval as string="") as string
DECLARE Function LoadProp(node as Reload.Nodeptr, propname as string, byval defaultval as integer=0) as integer
DECLARE Function LoadPropBool(node as Reload.Nodeptr, propname as string, byval defaultval as integer=NO) as integer

'Other local subs and functions
DECLARE Function SliceXAlign(byval sl as Slice Ptr, byval alignTo as Slice Ptr) as integer
DECLARE Function SliceYAlign(byval sl as Slice Ptr, byval alignTo as Slice Ptr) as integer
DECLARE Sub ApplySliceVelocity(byval s as slice ptr)
DECLARE Sub SeekSliceTarg(byval s as slice ptr)

END EXTERN

'==============================================================================

Dim SliceTable as SliceTable_

ReDim Shared SliceDebug(50) as Slice Ptr

'add other slice tables here

'ScreenSlice is used by other slices with ->Attach = slScreen
DIM SHARED ScreenSlice as Slice Ptr
ScreenSlice = NewSlice()
SliceDebugForget ScreenSlice '--screen slice is magical, ignore it for debugging purposes
WITH *ScreenSlice
 'Note that .Attach is NOT set to slScreen here. slScreen uses this, not the othetr way around
 .X = 0
 .Y = 0
 .ScreenX = 0
 .ScreenY = 0
 .Width = 320
 .Height = 200
END WITH

'frame_new_view changes the position of the origin. This is the transform needed to translate
'a slice's ScreenX/Y position to X/Y position on the current view slice. It starts at 0,0 when
'drawing a slice tree, and is modified whenever recursing to the children of a clipping slice.
Dim Shared GlobalCoordOffset as XYPair

EXTERN "C"


'==General slice code==========================================================

'stub functions:
Sub DrawNullSlice(byval s as slice ptr, byval p as integer) : end sub
Sub DisposeNullSlice(byval s as slice ptr) : end sub
Sub CloneNullSlice(byval s as slice ptr, byval cl as slice ptr) : end sub
Sub SaveNullSlice(byval s as slice ptr, byval node as Reload.Nodeptr) : end sub
Sub LoadNullSlice(Byval s as slice ptr, byval node as Reload.Nodeptr) : end sub
Sub DefaultChildRefresh(Byval par as Slice ptr, Byval ch as Slice ptr)
 if ch = 0 then debug "DefaultChildRefresh null ptr": exit sub
 with *ch
  if .Fill then
   .ScreenX = par->ScreenX + par->paddingLeft
   .ScreenY = par->ScreenY + par->paddingTop
   .Width = par->Width - par->paddingLeft - par->paddingRight
   .height = par->Height - par->paddingTop - par->paddingBottom
  else ' Not fill
   .ScreenX = .X + SliceXAlign(ch, par) - SliceXAnchor(ch)
   .ScreenY = .Y + SliceYAlign(ch, par) - SliceYAnchor(ch)
  end if
 end with
End sub

Sub DefaultChildDraw(Byval s as Slice Ptr, byval page as integer)
 'NOTE: we don't bother to null check s here because this sub is only
 '      ever called from DrawSlice which does null check it.
 dim clippos as XYPair = any
 with *s
  if .Clip then
   dim clipview as Frame ptr
   clippos.X = .ScreenX + .paddingLeft + GlobalCoordOffset.X
   clippos.Y = .ScreenY + .paddingTop + GlobalCoordOffset.Y
   clipview = frame_new_view(vpages(page), _
                             clippos.X, _
                             clippos.Y, _
                             .Width - .paddingLeft - .paddingRight, _
                             .Height - .paddingTop - .paddingBottom)
   page = registerpage(clipview)
   frame_unload @clipview

   'frame_new_view doesn't move the origin past the edges of the screen
   '(we don't need to check for going off the bottom or right edges because that's always a zero-size view)
   GlobalCoordOffset.X -= large(clippos.X, 0)
   GlobalCoordOffset.Y -= large(clippos.Y, 0)
  end if

  'draw the slice's children
  dim ch as slice ptr = .FirstChild
  do while ch <> 0
   DrawSlice(ch, page)
   ch = ch->NextSibling
  Loop

  if .Clip then
   freepage page
   GlobalCoordOffset.X += large(clippos.X, 0)
   GlobalCoordOffset.Y += large(clippos.Y, 0)
  end if

 end with
End sub

Sub SetupGameSlices
 SliceTable.Root = NewSliceOfType(slRoot, NULL, SL_ROOT)
 
 SliceTable.MapRoot = NewSliceOfType(slContainer, SliceTable.Root, SL_MAPROOT)
 SliceTable.MapRoot->Protect = YES
 
 SetupMapSlices maplayerMax

 SliceTable.Backdrop = NewSliceOfType(slSprite, SliceTable.Root, SL_BACKDROP)
 SliceTable.Backdrop->Protect = YES
 ChangeSpriteSlice SliceTable.Backdrop, sprTypeMXS

 SliceTable.ScriptSprite = NewSliceOfType(slSpecial, SliceTable.Root, SL_SCRIPT_LAYER)
 SliceTable.ScriptSprite->Fill = YES
 RefreshSliceScreenPos(SliceTable.ScriptSprite)
 
 SliceTable.TextBox = NewSliceOfType(slSpecial, SliceTable.Root, SL_TEXTBOX_LAYER)
 SliceTable.TextBox->Fill = YES
 RefreshSliceScreenPos(SliceTable.TextBox)
 
 'Not used yet, so don't create it!
 'SliceTable.Menu = NewSliceOfType(slSpecial, SliceTable.Root)

 'Not used yet either, actually 
 SliceTable.ScriptString = NewSliceOfType(slSpecial, SliceTable.Root, SL_STRING_LAYER)

End Sub

Sub SetupMapSlices(byval to_max as integer)
 FOR i as integer = 0 TO to_max
  SliceTable.MapLayer(i) = NewSliceOfType(slMap, SliceTable.MapRoot, SL_MAP_LAYER0 - i)
  ChangeMapSlice SliceTable.MapLayer(i), , , (i > 0), 0   'maybe transparent, not overhead
 NEXT
 
 SliceTable.ObsoleteOverhead = NewSliceOfType(slMap, SliceTable.MapRoot, SL_OBSOLETE_OVERHEAD)
 ChangeMapSlice SliceTable.ObsoleteOverhead, , , 0, 2   'non-transparent, overhead

 'Note: the order of this slice in relation to the .MapLayer siblings will change each time a map is loaded
 SliceTable.Walkabout = NewSliceOfType(slContainer, SliceTable.MapRoot, SL_WALKABOUT_LAYER)
 SliceTable.Walkabout->Fill = YES
 SliceTable.Walkabout->Protect = YES
 SliceTable.HeroLayer = NewSliceOfType(slContainer, SliceTable.Walkabout, SL_HERO_LAYER)
 SliceTable.HeroLayer->Fill = YES
 SliceTable.HeroLayer->Protect = YES
 SliceTable.HeroLayer->AutoSort = slAutoSortY
 SliceTable.NPCLayer = NewSliceOfType(slContainer, SliceTable.Walkabout, SL_NPC_LAYER)
 SliceTable.NPCLayer->Fill = YES
 SliceTable.NPCLayer->Protect = YES
 SliceTable.NPCLayer->AutoSort = slAutoSortCustom
End Sub

Sub DestroyGameSlices (Byval dumpdebug as integer=0)

 DeleteSlice(@SliceTable.Root, ABS(SGN(dumpdebug)))
 '--after deleting root, all other slices should be gone, but the pointers
 '--in SliceTable still need zeroing
 SliceTable.MapRoot = 0
 FOR i as integer = 0 TO maplayerMax
  SliceTable.MapLayer(i) = 0
 NEXT
 SliceTable.ObsoleteOverhead = 0
 SliceTable.Backdrop = 0
 SliceTable.ScriptSprite = 0
 SliceTable.TextBox = 0
 SliceTable.Menu = 0
 SliceTable.ScriptString = 0
End Sub

FUNCTION SliceTypeByName (s as string) as SliceTypes
 SELECT CASE s
  CASE "Root":           RETURN slRoot
  CASE "Special":        RETURN slSpecial
  CASE "Container":      RETURN slContainer
  CASE "Rectangle":      RETURN slRectangle
  CASE "Sprite":         RETURN slSprite
  CASE "Text":           RETURN slText
  CASE "Menu":           RETURN slMenu
  CASE "MenuItem":       RETURN slMenuItem
  CASE "Map":            RETURN slMap
  CASE "Grid":           RETURN slGrid
  CASE "Ellipse":        RETURN slEllipse
 END SELECT
 debug "Unrecognized slice name """ & s & """"
END FUNCTION

END EXTERN

FUNCTION SliceTypeName (sl as Slice Ptr) as string
 IF sl = 0 THEN debug "SliceTypeName null ptr": RETURN "<null ptr>"
 RETURN SliceTypeName(sl->SliceType)
END FUNCTION

FUNCTION SliceTypeName (t as SliceTypes) as string
 SELECT CASE t
  CASE slRoot:           RETURN "Root"
  CASE slSpecial:        RETURN "Special"
  CASE slContainer:      RETURN "Container"
  CASE slRectangle:      RETURN "Rectangle"
  CASE slSprite:         RETURN "Sprite"
  CASE slText:           RETURN "Text"
  CASE slMenu:           RETURN "Menu"
  CASE slMenuItem:       RETURN "MenuItem"
  CASE slMap:            RETURN "Map"
  CASE slGrid:           RETURN "Grid"
  CASE slEllipse:        RETURN "Ellipse"
 END SELECT
 RETURN "Unknown"
END FUNCTION

FUNCTION SliceLookupCodename (sl as Slice Ptr) as string
 '--Used for debugging
 IF sl = 0 THEN RETURN "[null]"
 RETURN SliceLookupCodename(sl->Lookup)
END FUNCTION

FUNCTION SliceLookupCodename (byval code as integer) as string
 SELECT CASE code
  CASE 0: RETURN ""
'--the following is updated from slices.bi using the misc/sl_lookup.py script
'<SLICE LOOKUP NAMES>
  CASE SL_ROOT: RETURN "root"
  CASE SL_TEXTBOX_TEXT: RETURN "textbox_text"
  CASE SL_TEXTBOX_PORTRAIT: RETURN "textbox_portrait"
  CASE SL_TEXTBOX_CHOICE0: RETURN "textbox_choice0"
  CASE SL_TEXTBOX_CHOICE1: RETURN "textbox_choice1"
  CASE SL_TEXTBOX_BOX: RETURN "textbox_box"
  CASE SL_TEXTBOX_PORTRAIT_BOX: RETURN "textbox_portrait_box"
  CASE SL_TEXTBOX_CHOICE_BOX: RETURN "textbox_choice_box"
  CASE SL_TEXTBOX_ROOT: RETURN "textbox_root"
  CASE SL_SCRIPT_LAYER: RETURN "script_layer"
  CASE SL_TEXTBOX_LAYER: RETURN "textbox_layer"
  CASE SL_STRING_LAYER: RETURN "string_layer"
  CASE SL_MAPROOT: RETURN "maproot"
  CASE SL_OBSOLETE_OVERHEAD: RETURN "obsolete_overhead"
  CASE SL_WALKABOUT_LAYER: RETURN "walkabout_layer"
  CASE SL_HERO_LAYER: RETURN "hero_layer"
  CASE SL_NPC_LAYER: RETURN "npc_layer"
  CASE SL_WALKABOUT_SPRITE_COMPONENT: RETURN "walkabout_sprite_component"
  CASE SL_WALKABOUT_SHADOW_COMPONENT: RETURN "walkabout_shadow_component"
  CASE SL_BACKDROP: RETURN "backdrop"
  CASE SL_MAP_LAYER0: RETURN "map_layer0"
  CASE SL_MAP_LAYER1: RETURN "map_layer1"
  CASE SL_MAP_LAYER2: RETURN "map_layer2"
  CASE SL_MAP_LAYER3: RETURN "map_layer3"
  CASE SL_MAP_LAYER4: RETURN "map_layer4"
  CASE SL_MAP_LAYER5: RETURN "map_layer5"
  CASE SL_MAP_LAYER6: RETURN "map_layer6"
  CASE SL_MAP_LAYER7: RETURN "map_layer7"
'</SLICE LOOKUP NAMES>
  CASE ELSE
   RETURN STR(code)
 END SELECT
 RETURN ""
END FUNCTION

EXTERN "C"

FUNCTION NewSliceOfType (byval t as SliceTypes, byval parent as Slice Ptr=0, byval lookup_code as integer=0) as Slice Ptr
 DIM newsl as Slice Ptr
 SELECT CASE t
  CASE slRoot:
   newsl = NewSlice(parent)
   WITH *newsl
    .SliceType = slRoot
    .Attach = slScreen
    .SliceType = slRoot
    .Protect = YES
    'We manually set these here so that Root will have the correct
    'size even if DrawSlice has not been called on it yet. This
    'is needed to make second-level roots .Fill=YES work correctly
    'in the transitional phase when root is not yet drawn
    .Width = 320
    .Height = 200
   END WITH
  CASE slSpecial:
   newsl = NewSlice(parent)
   newsl->SliceType = slSpecial
   newsl->Protect = YES
  CASE slContainer:
   newsl = NewSlice(parent)
   newsl->SliceType = slContainer
  CASE slRectangle:
   DIM dat as RectangleSliceData
   newsl = NewRectangleSlice(parent, dat)
  CASE slSprite:
   DIM dat as SpriteSliceData
   newsl = NewSpriteSlice(parent, dat)
  CASE slText
   DIM dat as TextSliceData
   newsl = NewTextSlice(parent, dat)
  CASE slMenu:
   DIM dat as MenuSliceData
   newsl = NewMenuSlice(parent, dat)
  CASE slMenuItem:
   DIM dat as MenuItemSliceData
   newsl = NewMenuItemSlice(parent, dat)
  CASE slMap:
   DIM dat as MapSliceData
   newsl = NewMapSlice(parent, dat)
   newsl->Protect = YES
  CASE slGrid:
   DIM dat as GridSliceData
   newsl = NewGridSlice(parent, dat)
  CASE slEllipse:
   DIM dat as EllipseSliceData
   newsl = NewEllipseSlice(parent, dat)
  CASE ELSE
   debug "NewSliceByType: Warning! type " & t & " is invalid"
   newsl = NewSlice(parent)
 END SELECT
 newsl->Lookup = lookup_code
 RETURN newsl
END FUNCTION

'Creates a new Slice object, and optionally, adds it to the heirarchy somewhere
Function NewSlice(Byval parent as Slice ptr = 0) as Slice Ptr
 dim ret as Slice Ptr
 ret = new Slice
 
 setSliceParent(ret, parent)
 
 ret->SliceType = slSpecial
 ret->Visible = YES
 ret->Mobile = YES
 ret->Attached = 0
 ret->Attach = slSlice
 
 ret->Draw = @DrawNullSlice
 ret->Dispose = @DisposeNullSlice
 ret->Clone = @CloneNullSlice
 ret->Save = @SaveNullSlice
 ret->Load = @LoadNullSlice
 ret->ChildRefresh = @DefaultChildRefresh
 ret->ChildDraw = @DefaultChildDraw

 SliceDebugRemember ret
 
 return ret
End Function

'Deletes a slice, and any children (and their children (and their...))
Sub DeleteSlice(Byval s as Slice ptr ptr, Byval debugme as integer=0)
 '-- if debugme is true, dump some debug info about the slice being freed and all its children

 if s = 0 then exit sub  'can't do anything
 if *s = 0 then exit sub 'already freed

 dim sl as slice ptr = *s

 if debugme = -1 then debugme = 1
 if debugme > 0 then
  debug string(debugme - 1, " ") & SliceTypeName(sl) & " " & SliceLookupCodename(sl)
  debugme += 1
 end if
 
#ifdef IS_GAME
 'unlink this slice from the table of handles
 if sl->TableSlot > 0 then
  if sl->TableSlot <= ubound(plotslices) then
   if plotslices(sl->TableSlot) = sl then
    '--zero out the reference to this slice from the table
    plotslices(sl->TableSlot) = 0
   else
    reporterr "DeleteSlice: TableSlot mismatch! Slice " & sl & " slot is " & sl->TableSlot & " which has " & plotslices(sl->TableSlot), serrBug
   end if
  else
   reporterr "DeleteSlice: TableSlot for " & sl & " is invalid: " & sl->TableSlot, serrBug
  end if
 end if
#endif
 
 'Call the slice's type-specific Dispose function
 if sl->Dispose <> 0 then sl->Dispose(sl)
 
 dim as slice ptr nxt, prv, par, ch
 nxt = sl->NextSibling
 prv = sl->PrevSibling
 par = sl->Parent
 ch = sl->FirstChild
 
 if nxt then
  nxt->PrevSibling = prv
 end if
 if prv then
  prv->NextSibling = nxt
 end if
 if par then
  if par->FirstChild = sl then
   par->FirstChild = nxt
  end if
  par->NumChildren -= 1
 end if
 
 'next, delete our children
 do while ch <> 0
  nxt = ch->NextSibling
  DeleteSlice(@ch, debugme)
  ch = nxt
 loop

 SliceDebugForget sl
 
 delete sl
 *s = 0
End Sub

'Deletes a slice's children but not itself
Sub DeleteSliceChildren(Byval sl as Slice ptr)
 if sl = 0 then debug "DeleteSliceChildren null ptr": exit sub
 dim ch as slice ptr
 ch = sl->FirstChild
 do while ch
  DeleteSlice @ch
  ch = sl->FirstChild
 loop
End Sub

Sub OrphanSlice(byval sl as slice ptr)
 '-- Remove a slice from its current parent cleanly,
 '-- adjusting siblings, and leaving itself parentless.
 if sl = 0 then debug "OrphanSlice null ptr": exit sub
 
 dim as slice ptr nxt, prv, par
 nxt = sl->NextSibling
 prv = sl->PrevSibling
 par = sl->Parent
 
 if nxt then
  nxt->PrevSibling = prv
 end if
 if prv then
  prv->NextSibling = nxt
 end if
 if par then
  if par->FirstChild = sl then
   par->FirstChild = nxt
  end if
  par->NumChildren -= 1
 end if
 
 sl->NextSibling = 0
 sl->PrevSibling = 0
 sl->Parent = 0
end sub

Sub SetSliceParent(byval sl as slice ptr, byval parent as slice ptr)
 'Note: might be reparenting a slice to its parent, to make it the last child
 if sl = 0 then debug "SetSliceParent null ptr": exit sub

 if parent andalso verifySliceLineage(sl, parent) = 0 then
  reporterr "Attempted to parent a slice to itself or descendents!", serrBadOp
  exit sub
 end if

 'first, remove the slice from its existing parent
 OrphanSlice sl
 
 'then, add ourselves to the new parent
 if parent then
  if parent->FirstChild = 0 then
   parent->FirstChild = sl
  else
   dim s as slice ptr
   s = parent->FirstChild
   do while s->NextSibling <> 0
    s = s->NextSibling
   loop
   s->NextSibling = sl
   sl->PrevSibling = s
  end if
   
  parent->NumChildren += 1
  sl->parent = parent
 end if
 
end sub

Sub AutoSortChildren(byval s as Slice Ptr)
 if s = 0 then debug "AutoSortChildren: null ptr": exit sub
 select case s->AutoSort
  case slAutoSortCustom:
   CustomSortChildSlices s, NO
  case slAutoSortY:
   YSortChildSlices s
  case slAutoSortTopY:
   EdgeYSortChildSlices s, 0
  case slAutoSortCenterY:
   EdgeYSortChildSlices s, 1
  case slAutoSortBottomY:
   EdgeYSortChildSlices s, 2
 end select
End sub

Sub UnlinkChildren(byval parent as Slice Ptr, slice_list() as slice ptr)
 if parent = 0 then debug "UnlinkChildren: null ptr"
 dim temp_sl as slice ptr = parent->FirstChild
 dim i as integer
 'Convert the children into an unlinked list
 for i = 0 to ubound(slice_list)
  slice_list(i) = temp_sl
  temp_sl = temp_sl->NextSibling
  slice_list(i)->PrevSibling = 0
  slice_list(i)->NextSibling = 0
 next i
end sub

Sub RelinkChildren(byval parent as Slice Ptr, slice_list() as slice ptr)
 if parent = 0 then debug "RelinkChildren: null ptr"
 dim i as integer
 parent->FirstChild = slice_list(0)
 'Convert back to a doubly linked list
 for i = 1 to ubound(slice_list)
  slice_list(i - 1)->NextSibling = slice_list(i)
  slice_list(i)->PrevSibling = slice_list(i - 1)
 next i 
end sub

Sub SwapSiblingSlices(byval sl1 as slice ptr, byval sl2 as slice ptr)
 'Only intended for use by siblings of the same parent
 if sl1 = 0 or sl2 = 0 then EXIT SUB ' Exit quietly when an arg is null. Valid use case for attempted swap at the beginning or end of a list
 if sl1 = sl2 then EXIT SUB ' Ignore attempts to swap a slice with itself
 if sl1->Parent <> sl2->Parent then reporterr "SwapSiblingSlices: slices are not siblings": EXIT SUB
 dim parent as slice ptr = sl1->Parent
 dim slice_list(parent->NumChildren - 1) as slice ptr
 UnlinkChildren parent, slice_list()
 'Swap the two siblings
 for i as integer = 0 to ubound(slice_list)
  if slice_list(i) = sl1 then
   slice_list(i) = sl2
  elseif slice_list(i) = sl2 then
   slice_list(i) = sl1
  end if
 next i
 RelinkChildren parent, slice_list()
end sub

Sub YSortChildSlices(byval parent as slice ptr)
 if parent = 0 then debug "YSortChildSlices: null ptr" : exit sub
 if parent->NumChildren = 0 then exit sub
 dim slice_list(parent->NumChildren - 1) as slice ptr
 UnlinkChildren parent, slice_list()
 'Sort the siblings by Y
 dim temp as slice ptr
 dim i as integer
 for j as integer = 1 to ubound(slice_list)
  temp = slice_list(j)
  for i = j - 1 to 0 step -1
   if slice_list(i)->Y <= temp->Y then exit for
   slice_list(i + 1) = slice_list(i)
  next i
  slice_list(i + 1) = temp
 next j
 RelinkChildren parent, slice_list()
end sub

Sub CustomSortChildSlices(byval parent as slice ptr, byval wipevals as integer)
 if parent = 0 then debug "CustomSortChildSlices: null ptr" : exit sub
 if parent->NumChildren = 0 then exit sub
 dim slice_list(parent->NumChildren - 1) as slice ptr
 UnlinkChildren parent, slice_list()
 'Sort the siblings by Sorter
 dim temp as slice ptr
 dim i as integer
 for j as integer = 1 to ubound(slice_list)
  temp = slice_list(j)
  for i = j - 1 to 0 step -1
   if slice_list(i)->Sorter <= temp->Sorter then exit for
   slice_list(i + 1) = slice_list(i)
  next i
  slice_list(i + 1) = temp
 next j
 if wipevals then
  for j as integer = 0 to ubound(slice_list)
   slice_list(j)->Sorter = 0
  next
 end if
 RelinkChildren parent, slice_list()
End sub

Sub EdgeYSortChildSlices(byval parent as slice ptr, byval edge as integer)
 if parent = 0 then debug "EdgeYSortChildSlices: null ptr" : exit sub
 if parent->NumChildren = 0 then exit sub
 dim slice_list(parent->NumChildren - 1) as slice ptr
 UnlinkChildren parent, slice_list()
 'Sort the siblings all by the same edge/corner
 dim temp as slice ptr
 dim i as integer
 for j as integer = 1 to ubound(slice_list)
  temp = slice_list(j)
  for i = j - 1 to 0 step -1
   if slice_list(i)->Y - SliceYAnchor(slice_list(i)) + SliceEdgeY(slice_list(i), edge) <= temp->Y - SliceYAnchor(temp) + SliceEdgeY(temp, edge) then exit for
   slice_list(i + 1) = slice_list(i)
  next i
  slice_list(i + 1) = temp
 next j
 RelinkChildren parent, slice_list()
end sub

Sub InsertSliceBefore(byval sl as slice ptr, byval newsl as slice ptr)
 'newsl will be removed from its current parent (if any) and attached to the same
 'parent as sl as the child before sl
 if sl = 0 then debug "InsertSliceBefore: null sl": EXIT SUB
 if newsl = 0 then debug "InsertSliceBefore: null newsl": EXIT SUB
 if sl = newsl then EXIT SUB ' Fail quietly when trying to insert a slice as a sibling
                             ' of itself because this is normal if you are using this function
                             ' to move a slice to the beginning of its sibling list when it is
                             ' already the first sibling
 if sl->PrevSibling = newsl then EXIT SUB 'already done
 if sl->Parent = 0 then reporterr "InsertSliceBefore: Root shouldn't have siblings" : EXIT SUB

 'Verify the family
 if verifySliceLineage(newsl, sl->Parent) = NO then
  reporterr "InsertSliceBefore: attempted to parent a slice to itself or descendents"
  EXIT SUB
 end if

 if newsl->Parent <> 0 then OrphanSlice newsl

 'Tell the new sibling about its parent
 newsl->Parent = sl->Parent

 'If this new sibling is an eldest child, tell the parent 
 if sl->Parent->FirstChild = sl then
  sl->Parent->FirstChild = newsl
 end if

 'Tell previous siblings that it has a new sibling.
 if sl->PrevSibling <> 0 then
  sl->PrevSibling->NextSibling = newsl
 end if
 
 'Tell new sibling about its adjacent siblings
 newsl->PrevSibling = sl->PrevSibling
 newsl->NextSibling = sl

 'Tell the supplanted sibling that the new one precedes it
 sl->PrevSibling = newsl

 'One more mouth to feed...
 newsl->Parent->NumChildren += 1
 
end sub

Sub ReplaceSliceType(byval sl as slice ptr, byref newsl as slice ptr)
 'This takes a new slice (normally from one of the New*Slice functions)
 'and copies its type and type-specific data over an existing tree member.
 'Newsl gets Deleted to prevent it from being used afterwards!
 'Also, this fails if newsl is part of a tree. It must be parentless
 if sl = 0 then debug "ReplaceSliceType null ptr": exit sub
 if newsl = 0 then debug "ReplaceSliceType newsl null ptr": exit sub
 WITH *newsl
  'Make sure that newsl is an orphan already
  IF .Parent <> 0 THEN debug "ReplaceSliceType: Only works with orphaned slices" : EXIT SUB
  'Dispose of any old Slice Type specific data that is about to be replaced
  IF sl->SliceData <> 0 THEN sl->Dispose(sl)
  'Copy over slice identity
  sl->SliceType = .SliceType
  sl->Draw      = .Draw
  sl->Dispose   = .Dispose
  sl->Clone     = .Clone
  sl->Save      = .Save
  sl->Load      = .Load
  sl->ChildRefresh = .ChildRefresh
  sl->ChildDraw = .ChildDraw
  sl->SliceData = .SliceData
  sl->SliceType = .SliceType
  'Break slice connection to data
  .SliceData = 0
  'Now destroy newsl
  DeleteSlice @newsl
 END WITH
End Sub

Function LookupSlice(byval lookup_code as integer, byval start_sl as slice ptr = NULL) as slice ptr
  IF start_sl = 0 THEN start_sl = SliceTable.root
  IF lookup_code = 0 THEN RETURN 0 '--fail searching for a zero lookup code
  IF start_sl->Lookup = lookup_code THEN RETURN start_sl '--found it!
  DIM child as Slice Ptr
  child = start_sl->FirstChild
  DIM result as Slice Ptr
  WHILE child
   result = LookupSlice(lookup_code, child)
   IF result THEN RETURN result '--found in recursion, pass the result back
   child = child->NextSibling
  WEND
End Function

Function LastChild(byval parent as slice ptr) as slice ptr
 IF parent = 0 THEN RETURN 0
 DIM sl as Slice Ptr
 sl = parent->FirstChild
 IF sl = 0 THEN RETURN 0
 DIM nextsib as Slice ptr
 WHILE sl
  nextsib = sl->NextSibling
  IF nextsib = 0 THEN RETURN sl
  sl = nextsib
 WEND
 RETURN 0
End function

'this function ensures that we can't set a slice to be a child of itself (or, a child of a child of itself, etc)
Function verifySliceLineage(byval sl as slice ptr, parent as slice ptr) as integer
 dim s as slice ptr
 if sl = 0 then return no
 s = parent
 do while s <> 0
  if s = sl then return no
  s = s->parent
 loop
 return yes
end function

Function IndexAmongSiblings(byref sl as Slice Ptr) as integer
 '--Returns the 0-based index of this slice among is siblings.
 'FIXME: slow for large families
 if sl = 0 then return 0
 if sl->parent = 0 then return 0
 dim sib as Slice Ptr = sl->parent->FirstChild
 for i as integer = 0 TO sl->parent->NumChildren - 1
  if sib = 0 then exit for
  if sib = sl then return i
  sib = sib->NextSibling
 next i
 return 0
End function

'slice accessors
Function SliceGetParent( byval s as Slice ptr ) as Slice ptr
 return s->Parent
End Function
Function SliceGetFirstChild( byval s as Slice ptr ) as Slice ptr
 return s->FirstChild
End Function
Function SliceGetNextSibling( byval s as Slice ptr ) as Slice ptr
 return s->NextSibling
End Function
Function SliceGetPrevSibling( byval s as Slice ptr ) as Slice ptr
 return s->PrevSibling
End Function
Function SliceGetNumChildren( byval s as Slice ptr ) as integer
 return s->NumChildren
End Function
Function SliceGetX( byval s as Slice ptr ) as integer
 return s->X
End Function
Function SliceGetY( byval s as Slice ptr ) as integer
 return s->Y
End Function
Function SliceGetScreenX( byval s as Slice ptr ) as integer
 return s->ScreenX
End Function
Function SliceGetScreenY( byval s as Slice ptr ) as integer
 return s->ScreenY
End Function
Function SliceGetWidth( byval s as Slice ptr ) as integer
 return s->Width
End Function
Function SliceGetHeight( byval s as Slice ptr ) as integer
 return s->Height
End Function
Function SliceIsVisible( byval s as Slice ptr ) as integer
 return s->Visible
End Function
Function SliceIsMobile( byval s as Slice ptr ) as integer
 return s->Mobile
End Function
Function SliceIsClipping( byval s as Slice ptr ) as integer
 return s->Clip
End Function
'slice mutators
Sub SliceSetX( byval s as Slice ptr, byval x as integer )
 s->X = x
End Sub
Sub SliceSetY( byval s as Slice ptr, byval y as integer )
 s->Y = y
End Sub
Sub SliceSetWidth( byval s as Slice ptr, byval w as integer )
 s->Width = w
End Sub
Sub SliceSetHeight( byval s as Slice ptr, byval h as integer )
 s->Height = h
End Sub
Sub SliceSetVisibility( byval s as Slice ptr, byval b as integer )
 s->Visible = b
End Sub
Sub SliceSetMobility( byval s as Slice ptr, byval b as integer )
 s->Mobile = b
End Sub
Sub SliceSetClipping( byval s as Slice ptr, byval b as integer )
 s->Clip = b
End Sub

'==Special slice types=========================================================

'--Rectangle--------------------------------------------------------------
Sub DisposeRectangleSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as RectangleSliceData ptr = cptr(RectangleSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub UpdateRectangleSliceStyle(byval dat as RectangleSliceData ptr)
 IF dat->style >= -1 ANDALSO dat->style <= 14 THEN
  dat->bgcol = boxlook(dat->style).bgcol
  dat->fgcol = boxlook(dat->style).edgecol
  dat->border = dat->style
 ELSE
  debug "bad rect style " & dat->style
 END IF
 dat->style_loaded = YES
end sub

Sub DrawRectangleSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as RectangleSliceData ptr = cptr(RectangleSliceData ptr, sl->SliceData)

 if dat->style >= 0 and dat->style_loaded = NO then
  UpdateRectangleSliceStyle dat
 end if

 edgebox sl->screenx, sl->screeny, sl->width, sl->height, dat->bgcol, dat->fgcol, p, dat->translucent, dat->border, dat->fuzzfactor
end sub

Sub CloneRectangleSlice(byval sl as slice ptr, byval cl as slice ptr)
 if sl = 0 or cl = 0 then debug "CloneRectangleSlice null ptr": exit sub
 dim dat as RectangleSliceData Ptr
 dat = sl->SliceData
 dim clonedat as RectangleSliceData Ptr
 clonedat = cl->SliceData
 with *clonedat
  .style       = dat->style
  .fgcol       = dat->fgcol
  .bgcol       = dat->bgcol
  .translucent = dat->translucent
  .border      = dat->border
  .fuzzfactor  = dat->fuzzfactor
 end with
end sub

Sub SaveRectangleSlice(byval sl as slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveRectangleSlice null ptr": exit sub
 DIM dat as RectangleSliceData Ptr
 dat = sl->SliceData
 SaveProp node, "style", dat->style
 SaveProp node, "fg", dat->fgcol
 SaveProp node, "bg", dat->bgcol
 SaveProp node, "trans", dat->translucent
 SaveProp node, "border", dat->border
 SaveProp node, "fuzzfactor", dat->fuzzfactor
End Sub

Sub LoadRectangleSlice (Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadRectangleSlice null ptr": exit sub
 dim dat as RectangleSliceData Ptr
 dat = sl->SliceData
 dat->style = LoadProp(node, "style", -1)
 dat->fgcol = LoadProp(node, "fg")
 dat->bgcol = LoadProp(node, "bg")
 dat->translucent = LoadProp(node, "trans")
 dat->border = LoadProp(node, "border", -1)
 dat->fuzzfactor = LoadProp(node, "fuzzfactor", 50)
End Sub

Function NewRectangleSlice(byval parent as Slice ptr, byref dat as RectangleSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as RectangleSliceData ptr = new RectangleSliceData
 *d = dat
 '--Set non-zero defaults here
 d->border = -1
 d->style = -1
 d->fuzzfactor = 50
 
 ret->SliceType = slRectangle
 ret->SliceData = d
 ret->Draw = @DrawRectangleSlice
 ret->Dispose = @DisposeRectangleSlice
 ret->Clone = @CloneRectangleSlice
 ret->Save = @SaveRectangleSlice
 ret->Load = @LoadRectangleSlice
 
 return ret
end function

Function GetRectangleSliceData(byval sl as slice ptr) as RectangleSliceData ptr
 if sl = 0 then debug "GetRectangleSliceData null ptr": return 0
 return sl->SliceData
End Function

'All arguments default to no-change
Sub ChangeRectangleSlice(byval sl as slice ptr,_
                      byval style as integer=-2,_
                      byval bgcol as integer=-1,_
                      byval fgcol as integer=-1,_
                      byval border as integer=-3,_
                      byval translucent as RectTransTypes=transUndef,_
                      byval fuzzfactor as integer=0)
 if sl = 0 then debug "ChangeRectangleSlice null ptr" : exit sub
 if sl->SliceType <> slRectangle then reporterr "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a rectangle" : exit sub
 if style > -2 andalso border > -3 then
  debug "WARNING: attempted to simultaneously set style and border on a rectangle slice"
 end if
 dim dat as RectangleSliceData Ptr = sl->SliceData
 with *dat
  if bgcol >= 0 then
   .bgcol = bgcol
   .style = -1
   .style_loaded = NO
  end if
  if fgcol >= 0 then
   .fgcol = fgcol
   .style = -1
   .style_loaded = NO
  end if
  if border > -3 then
   .border = border
   .style = -1
   .style_loaded = NO
  end if
  if style > -2 then
   .style = style
   .style_loaded = NO
  end if
  if translucent <> transUndef then .translucent = translucent
  if fuzzfactor > 0 then
   .fuzzfactor = fuzzfactor
  end if
 end with
 if dat->style >= 0 and dat->style_loaded = NO then
  UpdateRectangleSliceStyle dat
 end if
end sub

'--Text-------------------------------------------------------------------
Sub DisposeTextSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as TextSliceData ptr = cptr(TextSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub WrapTextSlice(byval sl as slice ptr, lines() as string)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 dim dat as TextSliceData ptr = cptr(TextSliceData ptr, sl->SliceData)
 dim d as string
 if dat->wrap AND sl->width > 7 then
  d = wordwrap(dat->s, int(sl->width / 8))
 elseif dat->wrap AND sl->width <= 7 then
  d = wordwrap(dat->s, int((320 - sl->X) / 8))
 else
  d = dat->s
 end if

 split(d, lines())

 '--set line count based on the current wrapped size
 dat->line_count = UBOUND(lines) + 1
End sub

Sub DrawTextSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub

 dim dat as TextSliceData ptr = cptr(TextSliceData ptr, sl->SliceData)
 
 dim lines() as string
 WrapTextSlice sl, lines()

 dim col as integer = dat->col
 if col = 0 then col = uilook(uiText)
 dim chars as integer = 0
 dat->insert_tog = dat->insert_tog xor 1
 dim insert_size as integer = 8
 if dat->outline then insert_size = 9
 dim last_line as integer = ubound(lines)
 if dat->line_limit <> 0 then last_line = small(last_line, dat->first_line + dat->line_limit - 1)
 dim ypos as integer
 if dat->show_insert then
  '--advance the insert cursor for off-screen lines
  for i as integer = 0 to small(dat->first_line - 1, ubound(lines))
   chars += len(lines(i)) + 1
  next i
 end if
 for i as integer = dat->first_line to last_line
  ypos = (i - dat->first_line) * 10
  if dat->show_insert then
   if dat->insert >= chars and dat->insert <= chars + len(lines(i)) then
    rectangle sl->screenx + (dat->insert - chars) * 8, sl->screeny + ypos, insert_size, insert_size, uilook(uiHighlight + dat->insert_tog), p
   end if
   chars += len(lines(i)) + 1
  end if
  if dat->outline then
   edgeprint lines(i), sl->screenx, sl->screeny + ypos, col, p
  else
   textcolor col, dat->bgcol
   printstr lines(i), sl->screenx, sl->screeny + ypos, p
  end if
 next
end sub

Sub UpdateTextSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as TextSliceData ptr = cptr(TextSliceData ptr, sl->SliceData)
 
 '--Note that automatic setting of wrapped text height doesn't matter if this slice is set ->Fill = YES the parent fill height will override
 dim lines() as string
 WrapTextSlice sl, lines()
 dim high as integer
 high = dat->line_count
 if dat->line_limit > 0 then
  high = small(high, dat->line_limit)
 end if
 sl->Height = high * 10
 
 if dat->Wrap = NO then
  sl->Width = textWidth(dat->s)
 else
  '--Wrapped text does not change the slice width. Do that manually (or by setting ->Fill = YES)
 end if
end sub

Function GetTextSliceData(byval sl as slice ptr) as TextSliceData ptr
 if sl = 0 then debug "GetTextSliceData null ptr": return 0
 return sl->SliceData
End Function

Sub CloneTextSlice(byval sl as slice ptr, byval cl as slice ptr)
 if sl = 0 or cl = 0 then debug "CloneTextSlice null ptr": exit sub
 dim dat as TextSliceData Ptr
 dat = sl->SliceData
 dim clonedat as TextSliceData Ptr
 clonedat = cl->SliceData
 with *clonedat
  .s       = dat->s
  .col     = dat->col
  .outline = dat->outline
  .wrap    = dat->wrap
  .bgcol   = dat->bgcol
 end with
end sub

Sub SaveTextSlice(byval sl as slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveTextSlice null ptr": exit sub
 DIM dat as TextSliceData Ptr
 dat = sl->SliceData
 SaveProp node, "s", dat->s
 SaveProp node, "col", dat->col
 SaveProp node, "outline", dat->outline
 SaveProp node, "wrap", dat->wrap
 SaveProp node, "bgcol", dat->bgcol
End Sub

Sub LoadTextSlice (Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadTextSlice null ptr": exit sub
 dim dat as TextSliceData Ptr
 dat = sl->SliceData
 dat->s       = LoadPropStr(node, "s")
 dat->col     = LoadProp(node, "col")
 dat->outline = LoadPropBool(node, "outline")
 dat->wrap    = LoadPropBool(node, "wrap")
 dat->bgcol   = LoadProp(node, "bgcol")
End Sub

Function NewTextSlice(byval parent as Slice ptr, byref dat as TextSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as TextSliceData ptr = new TextSliceData
 *d = dat
 
 ret->SliceType = slText
 ret->SliceData = d
 ret->Draw = @DrawTextSlice
 ret->Dispose = @DisposeTextSlice
 ret->Clone = @CloneTextSlice
 ret->Save = @SaveTextSlice
 ret->Load = @LoadTextSlice

 ret->Width = textwidth(d->s)
 'split(d->s, d->lines())
 
 return ret
end function

'All arguments default to no-change
Sub ChangeTextSlice(byval sl as slice ptr,_
                      s as string=CHR(1) & CHR(255),_
                      byval col as integer=-1,_
                      byval outline as integer=-2,_
                      byval wrap as integer=-2,_
                      byval bgcol as integer=-1)
 if sl = 0 then debug "ChangeTextSlice null ptr" : exit sub
 if sl->SliceType <> slText then reporterr "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as text" : exit sub
 dim dat as TextSliceData Ptr = sl->SliceData
 with *dat
  if s <> CHR(1) & CHR(255) then
   .s = s
  end if
  if col >= 0 then
   .col = col
  end if
  if bgcol >= 0 then
   .bgcol = bgcol
  end if
  if outline > -2 then
   .outline = outline <> 0
  end if
  if wrap > -2 then
   .wrap = wrap <> 0
  end if
 end with
 UpdateTextSlice sl
end sub

Function GetTextSliceString(byval sl as slice ptr) as string
 if sl = 0 then debug "GetTextSliceString null ptr" : return ""
 if sl->SliceType <> slText then reporterr "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as text" : return ""
 dim dat as TextSliceData Ptr = sl->SliceData
 with *dat
  return .s
 end with 
End Function

'--Sprite-----------------------------------------------------------------

Sub DisposeSpriteSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as SpriteSliceData ptr = cptr(SpriteSliceData ptr, sl->SliceData)
 unload_sprite_and_pal dat->img
 delete dat
 sl->SliceData = 0
end sub

Sub DrawSpriteSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as SpriteSliceData ptr = cptr(SpriteSliceData ptr, sl->SliceData)

 with *dat
 
  if .loaded = NO then
   load_sprite_and_pal .img, .spritetype, .record, .pal
   sl->Width = sprite_sizes(.spritetype).size.x
   sl->Height = sprite_sizes(.spritetype).size.y
   .loaded = YES
  end if

  dim spr as Frame ptr
  dim have_copy as integer = NO
  spr = .img.sprite
  if spr = 0 then
   reporterr "null sprite ptr for slice " & sl, serrBug
   exit sub
  end if
  if .frame >= sprite_sizes(.spritetype).frames or .frame < 0 then
   reporterr "out of range frame " & .frame & " for slice " & sl, serrBug
   .frame = 0
  end if
  
  spr += .frame

  'some redesign needed to prevent this continous flipping
  if .flipHoriz then
   if have_copy = NO THEN spr = frame_duplicate(spr)
   have_copy = YES
   frame_flip_horiz(spr)
  end if
  if .flipVert then
   if have_copy = NO THEN spr = frame_duplicate(spr)
   have_copy = YES
   frame_flip_vert(spr)
  end if
 
  frame_draw spr, .img.pal, sl->screenX, sl->screenY, , .trans, p
  
  if have_copy then
   frame_unload(@spr)
  end if
 end with
end sub

Function GetSpriteSliceData(byval sl as slice ptr) as SpriteSliceData ptr
 if sl = 0 then debug "GetSpriteSliceData null ptr": return 0
 return sl->SliceData
End Function

'Make no mistake, this is just a hack currently
'(and it only accepts 4 bit graphics). Default palettes not allowed.
Sub SetSpriteToFrame(byval sl as slice ptr, byval fr as Frame ptr, byval pal as integer)
 if sl = 0 then debug "SetSpriteToFrame null ptr": exit sub
 dim dat as SpriteSliceData ptr = cptr(SpriteSliceData ptr, sl->SliceData)

 if pal < 0 then showerror "SetSpriteToFrame: default palettes verboten!"

 with *dat
  'Should not matter whether the sprite is loaded; however if we set .loaded=YES, have to have a palette
  'if this is 4-bit.
  frame_unload(@.img.sprite)
  .img.sprite = fr  'frame_reference(fr)
  palette16_unload(@.img.pal)
  .img.pal = palette16_load(pal)

  sl->Width = fr->w
  sl->Height = fr->h
  .loaded = YES

  .spritetype = sprTypeFrame
  .paletted = YES
 end with
End Sub

Sub CloneSpriteSlice(byval sl as slice ptr, byval cl as slice ptr)
 if sl = 0 or cl = 0 then debug "CloneSpriteSlice null ptr": exit sub
 dim dat as SpriteSliceData Ptr
 dat = sl->SliceData
 dim clonedat as SpriteSliceData Ptr
 clonedat = cl->SliceData
 with *clonedat
  .spritetype = dat->spritetype
  .record     = dat->record
  .paletted   = dat->paletted
  .pal        = dat->pal
  .frame      = dat->frame
  .flipHoriz  = dat->flipHoriz
  .flipVert   = dat->flipVert
  .trans      = dat->trans
  '.img and .loaded remain NULLs, NO  (for no reason. FIXME: what about Frame sprites?)
 end with
end sub

Sub SaveSpriteSlice(byval sl as slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveSpriteSlice null ptr": exit sub
 DIM dat as SpriteSliceData Ptr
 dat = sl->SliceData
 if dat->spritetype = sprTypeFrame then showerror "SaveSpriteSlice: tried to save Frame sprite": exit sub  'programmer error
 SaveProp node, "sprtype", dat->spritetype
 SaveProp node, "rec", dat->record
 if dat->paletted then
  SaveProp node, "pal", dat->pal
 end if
 SaveProp node, "frame", dat->frame
 SaveProp node, "fliph", dat->flipHoriz
 SaveProp node, "flipv", dat->flipVert
 SaveProp node, "trans", dat->trans
end sub

Sub LoadSpriteSlice (Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadSpriteSlice null ptr": exit sub
 dim dat as SpriteSliceData Ptr
 dat = sl->SliceData
 dat->spritetype = LoadProp(node, "sprtype")
 dat->record     = LoadProp(node, "rec")
 dat->pal        = LoadProp(node, "pal", -1)
 dat->frame      = LoadProp(node, "frame")
 dat->flipHoriz  = LoadProp(node, "fliph")
 dat->flipVert   = LoadProp(node, "flipv")
 dat->trans      = LoadProp(node, "trans", 1)
 dat->paletted   = (dat->spritetype <> sprTypeMXS)
End Sub

Function NewSpriteSlice(byval parent as Slice ptr, byref dat as SpriteSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as SpriteSliceData ptr = new SpriteSliceData
 *d = dat

 'Set non-zero defaults
 d->pal = -1
 d->trans = YES
 d->paletted = YES
 
 ret->SliceType = slSprite
 ret->SliceData = d
 ret->Draw = @DrawSpriteSlice
 ret->Dispose = @DisposeSpriteSlice
 ret->Clone = @CloneSpriteSlice
 ret->Save = @SaveSpriteSlice
 ret->Load = @LoadSpriteSlice
 
 return ret
end function

'All arguments default to no-change
Sub ChangeSpriteSlice(byval sl as slice ptr,_
                      byval spritetype as SpriteType = sprTypeInvalid,_
                      byval record as integer=-1,_
                      byval pal as integer = -2,_
                      byval frame as integer = -1,_
                      byval fliph as integer = -2,_
                      byval flipv as integer = -2,_
                      byval trans as integer = -2)
 if sl = 0 then debug "ChangeSpriteSlice null ptr" : exit sub
 if sl->SliceType <> slSprite then reporterr "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a sprite" : exit sub
 dim dat as SpriteSliceData Ptr = sl->SliceData
 with *dat
  if spritetype <> sprTypeInvalid then
   .spritetype = spritetype
   .paletted = (spritetype <> sprTypeMXS)
   .loaded = NO
   sl->Width = sprite_sizes(.spritetype).size.x
   sl->Height = sprite_sizes(.spritetype).size.y
  end if
  if record >= 0 then
   .record = record
   .loaded = NO
  end if
  if pal >= -1 then
   if .paletted = NO then
    reporterr "Attempt to set a palette (" & pal & ") on an unpaletted " & sprite_sizes(.spritetype).name & " sprite slice"
   else
    .pal = pal
    .loaded = NO
   end if
  end if
  if frame >= 0 then
   if frame >= sprite_sizes(.spritetype).frames then
    reporterr "Sprite frame " & frame & " is out of range for " & sprite_sizes(.spritetype).name & " sprites; valid range 0 to " & sprite_sizes(.spritetype).frames - 1
   else
    .frame = frame
   end if
  end if
  if fliph > -2 then .flipHoriz = (fliph <> 0)
  if flipv > -2 then .flipVert = (flipv <> 0)
  if trans > -2 then .trans = (trans <> 0)
 end with
end sub

'--Map-----------------------------------------------------------------

Sub DisposeMapSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as MapSliceData ptr = cptr(MapSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub DrawMapSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as MapSliceData ptr = cptr(MapSliceData ptr, sl->SliceData)

 with *dat
  if .tiles = 0 then exit sub 'tilemap ptr null if the layer doesn't exist. This slice probably shouldn't either.
  if .tileset = 0 then exit sub 'quit silently on a null tileset ptr
  '2nd, 3rd arguments to drawmap are "camera position" of upper left of the screen.
  drawmap *.tiles, sl->ScreenX * -1, sl->ScreenY * -1, .tileset, p, .transparent, .overlay, .pass
 end with
end sub

Function GetMapSliceData(byval sl as slice ptr) as MapSliceData ptr
 if sl = 0 then debug "GetMapSliceData null ptr": return 0
 return sl->SliceData
End Function

Sub SaveMapSlice(byval sl as slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveMapSlice null ptr": exit sub
 DIM dat as SpriteSliceData Ptr
 dat = sl->SliceData
 'FIXME: current MapSlice impl. has no savable properties
end sub

Sub LoadMapSlice (Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadMapSlice null ptr": exit sub
 dim dat as SpriteSliceData Ptr
 dat = sl->SliceData
 'FIXME: current MapSlice impl. has no savable properties
End Sub

Function NewMapSlice(byval parent as Slice ptr, byref dat as MapSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as MapSliceData ptr = new MapSliceData
 *d = dat

 ret->SliceType = slMap
 ret->SliceData = d
 ret->Draw = @DrawMapSlice
 ret->Dispose = @DisposeMapSlice
 '--No cloning support for Map slice yet
 'ret->Clone = @CloneMapSlice
 ret->Save = @SaveMapSlice
 ret->Load = @LoadMapSlice
 
 return ret
end function

Sub ChangeMapSliceTileset(byval sl as slice ptr, byval tileset as TilesetData ptr)
 if sl = 0 then debug "ChangeMapSliceTileset null ptr" : exit sub
 if sl->SliceType <> slMap then reporterr "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a map" : exit sub
 dim dat as MapSliceData Ptr = sl->SliceData
 dat->tileset = tileset 'NOTE: *shiver* pointers make me cringe.
end sub

Sub ChangeMapSlice(byval sl as slice ptr,_
                   byval tiles as TileMap ptr = cast(TileMap ptr, 1),_
                   byval pass as TileMap ptr = cast(TileMap ptr, 1),_
                   byval transparent as integer=-2,_
                   byval overlay as integer=-1)
 if sl = 0 then debug "ChangeMapSlice null ptr" : exit sub
 if sl->SliceType <> slMap then reporterr "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a map" : exit sub
 dim dat as MapSliceData Ptr = sl->SliceData
 with *dat
  if tiles <> cast(TileMap ptr, 1) then
   .tiles = tiles
   if tiles = NULL then
    sl->Width = 0
    sl->Height = 0
   else
    sl->Width = tiles->wide * 20
    sl->Height = tiles->high * 20
   end if
  end if
  if tiles <> cast(TileMap ptr, 1) then
   '--passmap. If this slice doesn't draw overhead tiles, can set this to NULL
   .pass = pass
  end if
  if transparent >= -1 then
   .transparent = (transparent <> 0) 'boolean
  end if
  if overlay >= 0 and overlay <= 2 then
   '--used for backcompat with overhead tiles on layer 0
   .overlay = overlay 'valid values 0, 1, 2
  end if
 end with
end sub

'--Grid-------------------------------------------------------------------
Sub DisposeGridSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as GridSliceData ptr = cptr(GridSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub DrawGridSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as GridSliceData ptr = cptr(GridSliceData ptr, sl->SliceData)

 
 if dat->show then
  drawbox sl->screenx, sl->screeny, sl->width, sl->height, uilook(uiText), 1, p
  dim w as integer = sl->width \ large(1, dat->cols)
  dim h as integer = sl->height \ large(1, dat->rows)
  for row as integer = 0 to dat->rows - 1
   for col as integer = 0 to dat->cols - 1
    'drawbox sl->screenx + col * w, sl->screeny + row * h, w, h, uilook(uiText), 1, p
    rectangle sl->screenx + col * w, sl->screeny + row * h, w, 1, uilook(uiText), p
    rectangle sl->screenx + col * w, sl->screeny + row * h, 1, h, uilook(uiText), p
   next col
  next row
 end if
end sub

Sub CloneGridSlice(byval sl as slice ptr, byval cl as slice ptr)
 if sl = 0 or cl = 0 then debug "CloneGridSlice null ptr": exit sub
 dim dat as GridSliceData Ptr
 dat = sl->SliceData
 dim clonedat as GridSliceData Ptr
 clonedat = cl->SliceData
 with *clonedat
  .cols = dat->cols
  .rows = dat->rows
  .show = dat->show
 end with
end sub

Sub SaveGridSlice(byval sl as slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveGridSlice null ptr": exit sub
 DIM dat as GridSliceData Ptr
 dat = sl->SliceData
 SaveProp node, "cols", dat->cols
 SaveProp node, "rows", dat->rows
 SaveProp node, "show", dat->show
End Sub

Sub LoadGridSlice (Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadGridSlice null ptr": exit sub
 dim dat as GridSliceData Ptr
 dat = sl->SliceData
 dat->cols = large(1, LoadProp(node, "cols", 1))
 dat->rows = large(1, LoadProp(node, "rows", 1))
 dat->show = LoadPropBool(node, "show")
End Sub

Function GridSliceXAlign(byval sl as Slice Ptr, byval alignTo as Slice Ptr, byval w as integer) as integer
 if sl = 0 then debug "GridSliceXAlign null ptr": Return 0
 SELECT CASE sl->AlignHoriz
  CASE 0: RETURN alignTo->ScreenX + alignTo->paddingLeft
  CASE 1: RETURN alignTo->ScreenX + alignTo->paddingLeft + (w - alignTo->paddingLeft - alignTo->paddingRight) \ 2
  CASE 2: RETURN alignTo->ScreenX + w - alignTo->paddingRight
 END SELECT
End Function

Function GridSliceYAlign(byval sl as Slice Ptr, byval alignTo as Slice Ptr, byval h as integer) as integer
 if sl = 0 then debug "GridSliceYAlign null ptr": Return 0
 SELECT CASE sl->AlignVert
  CASE 0: RETURN alignTo->ScreenY + alignTo->paddingTop
  CASE 1: RETURN alignTo->ScreenY + alignTo->paddingTop + (h - alignTo->paddingTop - alignTo->paddingBottom) \ 2
  CASE 2: RETURN alignTo->ScreenY + h - alignTo->paddingBottom
 END SELECT
End Function

Sub GridChildRefresh(byval par as slice ptr, byval ch as slice ptr)
 if ch = 0 then debug "GridChildRefresh null ptr": exit sub
 
 '--get grid data
 dim dat as GridSliceData ptr
 dat = par->SliceData
 dim w as integer = par->Width \ large(1, dat->cols)
 dim h as integer = par->Height \ large(1, dat->rows)
 '--Figure out which child this is
 dim slot as integer = IndexAmongSiblings(ch)
 dim xslot as integer = slot mod large(1, dat->cols)
 dim yslot as integer = slot \ large(1, dat->cols)
 
 with *ch
  if .Fill then
   .ScreenX = par->ScreenX + xslot * w + par->paddingLeft
   .ScreenY = par->ScreenY + yslot * h + par->paddingTop
   .Width = w - par->paddingLeft - par->paddingRight
   .height = h - par->paddingTop - par->paddingBottom
  else ' Not fill
   .ScreenX = .X + GridSliceXAlign(ch, par, w) - SliceXAnchor(ch) + xslot * w
   .ScreenY = .Y + GridSliceYAlign(ch, par, h) - SliceYAnchor(ch) + yslot * h
  end if
 end with
End sub

Sub GridChildDraw(Byval s as Slice Ptr, byval page as integer)
 'NOTE: this Sub only handles the clipping of the children of a Grid slice which
 '      is set to clip. It might seem the logical place to position the children
 '      too, but that's in GridChildRefresh. Which is probably correct: drawing
 '      and calculating position are independent.
 'NOTE: we don't bother to null check s here because this sub is only
 '      ever called from DrawSlice which does null check it.

 if s->SliceType <> slGrid then debug "GridChildDraw illegal slice type": exit sub

 if s->Clip = NO then
  'no special behaviour
  DefaultChildDraw s, page
  exit sub
 end if
 
 with *s
  '--get grid data
  dim dat as GridSliceData ptr
  dat = .SliceData
  dim w as integer = .Width \ large(1, dat->cols)
  dim h as integer = .Height \ large(1, dat->rows)

  dim clippos as XYPair
  dim clipview as Frame ptr
  dim childpage as integer

  'draw the slice's children
  dim ch as slice ptr = .FirstChild
  for yslot as integer = 0 to dat->rows - 1
   for xslot as integer = 0 to dat->cols - 1
    if ch = 0 then exit for, for

    clippos.X = .ScreenX + xslot * w + .paddingLeft + GlobalCoordOffset.X
    clippos.Y = .ScreenY + yslot * h + .paddingTop + GlobalCoordOffset.Y

    clipview = frame_new_view(vpages(page), _
                              clippos.X, _
                              clippos.Y, _
                              w - .paddingLeft - .paddingRight, _
                              h - .paddingTop - .paddingBottom)
    childpage = registerpage(clipview)
    frame_unload @clipview

    'frame_new_view doesn't move the origin past the edges of the screen
    '(we don't need to check for going off the bottom or right edges because that's always a zero-size view)
    GlobalCoordOffset.X -= large(clippos.X, 0)
    GlobalCoordOffset.Y -= large(clippos.Y, 0)

    DrawSlice(ch, childpage)

    freepage childpage
    GlobalCoordOffset.X += large(clippos.X, 0)
    GlobalCoordOffset.Y += large(clippos.Y, 0)

    ch = ch->NextSibling
   next
  next
 end with
End Sub

Function NewGridSlice(byval parent as Slice ptr, byref dat as GridSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as GridSliceData ptr = new GridSliceData
 *d = dat
 '--Set non-zero defaults here
 d->cols = 1
 d->rows = 1
 
 ret->SliceType = slGrid
 ret->SliceData = d
 ret->Draw = @DrawGridSlice
 ret->Dispose = @DisposeGridSlice
 ret->Clone = @CloneGridSlice
 ret->Save = @SaveGridSlice
 ret->Load = @LoadGridSlice
 ret->ChildRefresh = @GridChildRefresh
 ret->ChildDraw = @GridChildDraw
 
 return ret
end function

Function GetGridSliceData(byval sl as slice ptr) as GridSliceData ptr
 if sl = 0 then debug "GetGridSliceData null ptr": return 0
 return sl->SliceData
End Function

'All arguments default to no-change
Sub ChangeGridSlice(byval sl as slice ptr,_
                      byval rows as integer=0,_
                      byval cols as integer=0)
 if sl = 0 then debug "ChangeGridSlice null ptr" : exit sub
 if sl->SliceType <> slGrid then reporterr "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a grid" : exit sub
 dim dat as GridSliceData Ptr = sl->SliceData
 if rows > 0 then
  dat->rows = rows
 end if
 if cols > 0 then
  dat->cols = cols
 end if
end sub

'--Ellipse----------------------------------------------------------------

Sub DisposeEllipseSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as EllipseSliceData ptr = cptr(EllipseSliceData ptr, sl->SliceData)
 frame_unload @dat->frame
 delete dat
 sl->SliceData = 0
end sub

Sub DrawEllipseSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as EllipseSliceData ptr = cptr(EllipseSliceData ptr, sl->SliceData)

 with *dat
 
  dim w as integer = ABS(sl->Width)
  dim h as integer = ABS(sl->Height)
  if .frame = 0 _
     ORELSE .last_draw_size.X <> w _
     ORELSE .last_draw_size.Y <> h _
     ORELSE .last_draw_bordercol <> .bordercol _
     ORELSE .last_draw_fillcol <> .fillcol then
   if sl->Width = 0 ORELSE sl->Height = 0 then exit sub
   frame_unload @.frame
   'debug "create new ellipse frame " & w & "x" & h
   .frame = frame_new(w, h, , YES)
   'fuzzyrect .frame, 0, 0, w, h, dat->fillcol, 37
   dim fillcol as integer = dat->fillcol
   if fillcol = 0 then fillcol = -1
   ellipse .frame, w / 2 - 0.5, h / 2 - 0.5 , w / 2 - 0.5, dat->bordercol, fillcol, h / 2 - 0.5
   .last_draw_size.X = w
   .last_draw_size.Y = h
   .last_draw_bordercol = .bordercol
   .last_draw_fillcol = .fillcol
  end if

  if .frame = 0 then
   reporterr "null frame ptr for ellipse slice " & sl, serrBug
   exit sub
  end if

  frame_draw .frame, , small(sl->screenX, sl->screenX + sl->Width), small(sl->screenY, sl->screenY + sl->Height), , , p

' ellipse vpages(p), small(sl->screenX, sl->screenX + sl->Width) + w / 2 - 0.5, small(sl->screenY, sl->screenY + sl->Height) + h / 2 - 0.5 , w / 2 - 0.5, dat->bordercol, fillcol, h / 2 - 0.5
 end with
end sub

Sub CloneEllipseSlice(byval sl as slice ptr, byval cl as slice ptr)
 if sl = 0 or cl = 0 then debug "CloneEllipseSlice null ptr": exit sub
 dim dat as EllipseSliceData Ptr
 dat = sl->SliceData
 dim clonedat as EllipseSliceData Ptr
 clonedat = cl->SliceData
 with *clonedat
  .bordercol  = dat->bordercol
  .fillcol    = dat->fillcol
  '.last_draw_* left at zero to force a redraw
  '.frame will be populated on next draw
 end with
end sub

Sub SaveEllipseSlice(byval sl as slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveEllipseSlice null ptr": exit sub
 DIM dat as EllipseSliceData Ptr
 dat = sl->SliceData
 SaveProp node, "bordercol", dat->bordercol
 SaveProp node, "fillcol", dat->fillcol
end sub

Sub LoadEllipseSlice (Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadEllipseSlice null ptr": exit sub
 dim dat as EllipseSliceData Ptr
 dat = sl->SliceData
 dat->bordercol = LoadProp(node, "bordercol")
 dat->fillcol   = LoadProp(node, "fillcol")
End Sub

Function NewEllipseSlice(byval parent as Slice ptr, byref dat as EllipseSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as EllipseSliceData ptr = new EllipseSliceData
 *d = dat

 'Set defaults
 d->bordercol = 0
 d->fillcol = 0
 
 ret->SliceType = slEllipse
 ret->SliceData = d
 ret->Draw = @DrawEllipseSlice
 ret->Dispose = @DisposeEllipseSlice
 ret->Clone = @CloneEllipseSlice
 ret->Save = @SaveEllipseSlice
 ret->Load = @LoadEllipseSlice
 
 return ret
end function

'All arguments default to no-change
Sub ChangeEllipseSlice(byval sl as slice ptr,_
                      byval bordercol as integer=-1,_
                      byval fillcol as integer=-1)
 if sl = 0 then debug "ChangeEllipseSlice null ptr" : exit sub
 if sl->SliceType <> slEllipse then reporterr "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as an ellipse" : exit sub
 dim dat as EllipseSliceData Ptr = sl->SliceData
 with *dat
  if bordercol >= 0 then
   .bordercol = bordercol
  end if
  if fillcol >= 0 then
   .fillcol = fillcol
  end if
 end with
end sub

'--Menu-------------------------------------------------------------------
Sub DisposeMenuSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as MenuSliceData ptr = cptr(MenuSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub DrawMenuSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as MenuSliceData ptr = cptr(MenuSliceData ptr, sl->SliceData)

 dat->tog = dat->tog xor 1
end sub

Function GetMenuSliceData(byval sl as slice ptr) as MenuSliceData ptr
 if sl = 0 then debug "GetMenuSliceData null ptr": return 0
 return sl->SliceData
End Function

Sub SaveMenuSlice(byval sl as slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "SaveMenuSlice null ptr": exit sub
 DIM dat as MenuSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "SaveMenuSlice not implemented"
end sub

Sub LoadMenuSlice (Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadMenuSlice null ptr": exit sub
 dim dat as MenuSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "LoadMenuSlice not implemented"
End Sub

Function NewMenuSlice(byval parent as Slice ptr, byref dat as MenuSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as MenuSliceData ptr = new MenuSliceData
 *d = dat
 
 ret->SliceType = slMenu
 ret->SliceData = d
 ret->Draw = @DrawMenuSlice
 ret->Dispose = @DisposeMenuSlice
 ret->Save = @SaveMenuSlice
 ret->Load = @LoadMenuSlice
 
 return ret
end function

'--MenuItem---------------------------------------------------------------
Sub DisposeMenuItemSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as MenuItemSliceData ptr = cptr(MenuItemSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub DrawMenuItemSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as MenuItemSliceData ptr = cptr(MenuItemSliceData ptr, sl->SliceData)

 with *(GetMenuSliceData(sl->parent))
  dim c as integer
  if dat->disabled = NO then
   c = uiText
  else
   c = uiDisabledItem
  end if
  
  if .selected = dat->ordinal then
   edgeprint dat->caption, sl->screenx, sl->screeny, uilook(.tog + uiSelectedItem), p
  else
   edgeprint dat->caption, sl->screenx, sl->screeny, uilook(c), p
  end if
 end with
end sub

Sub UpdateMenuItemSlice(byval sl as slice ptr)
 if sl = 0 then debug "UpdateMenuItemSlice null ptr": exit sub
 dim dat as MenuItemSliceData ptr = cptr(MenuItemSliceData ptr, sl->SliceData)
 
 sl->Width = textWidth(dat->caption)
end sub

Function GetMenuItemSliceData(byval sl as slice ptr) as MenuItemSliceData ptr
 if sl = 0 then debug "GetMenuItemSliceData null ptr": return 0
 return sl->SliceData
End Function

Sub SaveMenuItemSlice(byval sl as slice ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "GetMenuItemSliceData null ptr": exit sub
 DIM dat as MenuItemSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "SaveMenuItemSlice not implemented"
end sub

Sub LoadMenuItemSlice (Byval sl as SliceFwd ptr, byval node as Reload.Nodeptr)
 if sl = 0 or node = 0 then debug "LoadMenuItemSlice null ptr": exit sub
 dim dat as MenuItemSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "LoadMenuItemSlice not implemented"
End Sub

Function NewMenuItemSlice(byval parent as Slice ptr, byref dat as MenuItemSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as MenuItemSliceData ptr = new MenuItemSliceData
 *d = dat
 
  d->ordinal = ret->parent->NumChildren - 1
 
 ret->SliceType = slMenuItem
 ret->SliceData = d
 ret->Draw = @DrawMenuItemSlice
 ret->Dispose = @DisposeMenuItemSlice
 ret->Save = @SaveMenuItemSlice
 ret->Load = @LoadMenuItemSlice
 
 return ret
end function

'==General slice display=======================================================

Function GetSliceDrawAttachParent(byval sl as Slice Ptr) as Slice Ptr
 if sl = 0 then debug "GetSliceDrawAttachParent null ptr": return 0
 WITH *sl
  SELECT CASE .Attach
   case slSlice
    if .Attached then
     RETURN .Attached
    elseif .parent then
     RETURN .parent
    else
     'Fall through, use screen
    end if
   case slScreen
    'Fall through, use screen
  END SELECT
 END WITH
 '--When no attached slice is found (or when we are explicitly attached to the screen)
 RETURN ScreenSlice
End Function

Function SliceXAlign(byval sl as Slice Ptr, byval alignTo as Slice Ptr) as integer
 if sl = 0 then debug "SliceXAlign null ptr": Return 0
 SELECT CASE sl->AlignHoriz
  CASE 0: RETURN alignTo->ScreenX + alignTo->paddingLeft
  CASE 1: RETURN alignTo->ScreenX + alignTo->paddingLeft + (alignTo->Width - alignTo->paddingLeft - alignTo->paddingRight) \ 2
  CASE 2: RETURN alignTo->ScreenX + alignTo->Width - alignTo->paddingRight
 END SELECT
End Function

Function SliceYAlign(byval sl as Slice Ptr, byval alignTo as Slice Ptr) as integer
 if sl = 0 then debug "SliceYAlign null ptr": Return 0
 SELECT CASE sl->AlignVert
  CASE 0: RETURN alignTo->ScreenY + alignTo->paddingTop
  CASE 1: RETURN alignTo->ScreenY + alignTo->paddingTop + (alignTo->Height - alignTo->paddingTop - alignTo->paddingBottom) \ 2
  CASE 2: RETURN alignTo->ScreenY + alignTo->Height - alignTo->paddingBottom
 END SELECT
End Function

Function SliceXAnchor(byval sl as Slice Ptr) as integer
 if sl = 0 then debug "SliceXAnchor null ptr": Return 0
 SELECT CASE sl->AnchorHoriz
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Width \ 2
  CASE 2: RETURN sl->Width
 END SELECT
End Function

Function SliceYAnchor(byval sl as Slice Ptr) as integer
 if sl = 0 then debug "SliceYAnchor null ptr": Return 0
 SELECT CASE sl->AnchorVert
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Height \ 2
  CASE 2: RETURN sl->Height
 END SELECT
End Function

Function SliceEdgeX(byval sl as Slice Ptr, byval edge as integer) as integer
 if sl = 0 then debug "SliceEdgeX null ptr": Return 0
 SELECT CASE edge
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Width \ 2
  CASE 2: RETURN sl->Width
 END SELECT
End Function

Function SliceEdgeY(byval sl as Slice Ptr, byval edge as integer) as integer
 if sl = 0 then debug "SliceEdgeY null ptr": Return 0
 SELECT CASE edge
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Height \ 2
  CASE 2: RETURN sl->Height
 END SELECT
End Function

Sub SetSliceTarg(byval s as slice ptr, byval x as integer, byval y as integer, byval ticks as integer)
 if s = 0 then debug "SetSliceTarg null ptr": exit sub
 with *s
  .TargResidue_X = 0.0
  .TargResidue_Y = 0.0
  .Targ.X = x
  .Targ.Y = y
  .TargTicks = ticks
  'cancel velocity
  .Velocity.X = 0
  .Velocity.Y = 0
  .VelTicks.X = 0
  .VelTicks.Y = 0
 end with
end sub

Sub AdvanceSlice(byval s as slice ptr)
 if s = 0 then debug "AdvanceSlice null ptr": exit sub
 if s->Mobile then
  SeekSliceTarg s
  ApplySliceVelocity s
  'advance the slice's children
  dim ch as slice ptr = s->FirstChild
  do while ch <> 0
   AdvanceSlice(ch)
   ch = ch->NextSibling
  Loop
 end if
end sub

Sub SeekSliceTarg(byval s as slice ptr)
 'no null check because this is only called from AdvanceSlice
 with *s
  if .TargTicks > 0 then
    dim as double temp
    dim as integer movestep
    temp = s->TargResidue_X + (s->Targ.X - .X) / s->TargTicks
    movestep = temp
    s->TargResidue_X = temp - movestep
    .X += movestep
    temp = s->TargResidue_Y + (s->Targ.Y - .Y) / s->TargTicks
    movestep = temp
    s->TargResidue_Y = temp - movestep
    .Y += movestep

   .TargTicks -= 1
   if .TargTicks = 0 then
    .X = .Targ.X
    .Y = .Targ.Y
   end if
  end if
 end with
end sub

Sub ApplySliceVelocity(byval s as slice ptr)
 'no null check because this is only called from AdvanceSlice
 if s->VelTicks.X <> 0 then s->X += s->Velocity.X
 if s->VelTicks.X > 0 then
  s->VelTicks.X -= 1
  if s->VelTicks.X = 0 then s->Velocity.X = 0
 end if
 if s->VelTicks.Y <> 0 then s->Y += s->Velocity.Y
 if s->VelTicks.Y > 0 then
  s->VelTicks.Y -= 1
  if s->VelTicks.Y = 0 then s->Velocity.Y = 0
 end if
end sub

Sub DrawSlice(byval s as slice ptr, byval page as integer)
 if s = 0 then debug "DrawSlice null ptr": exit sub
 'first, draw this slice
 if s->Visible then
  'calc the slice's X,Y

  DIM attach as Slice Ptr
  attach = GetSliceDrawAttachParent(s)
  if attach then attach->ChildRefresh(attach, s)
  if s->Draw then
   'translate screenX/Y by the position difference between page (due to it
   'potentially being a view on the screen) and the screen.
   s->ScreenX += GlobalCoordOffset.X
   s->ScreenY += GlobalCoordOffset.Y
   s->Draw(s, page)
   s->ScreenX -= GlobalCoordOffset.X
   s->ScreenY -= GlobalCoordOffset.Y
  end if
  AutoSortChildren(s)
  s->ChildDraw(s, page)
 end if
end sub

Sub DrawSliceAt(byval s as slice ptr, byval x as integer, byval y as integer, byval w as integer = 100, byval h as integer = 100, byval page as integer, byval ignore_offset as integer = NO)
 'ignore_offset causes the slice's offset from its parent to be ignored

 if s = 0 then debug "DrawSliceAt null ptr": exit sub
 if s->Visible then
  'calc the slice's X,Y

  'Is this actually necessary? I guess it should always be 0,0 when not inside a DrawSlice call...
  'GlobalCoordOffset.X = 0
  'GlobalCoordOffset.Y = 0

  DIM dummyparent as Slice Ptr
  dummyparent = NewSliceOfType(slContainer)
  dummyparent->ScreenX = x
  dummyparent->ScreenY = y
  dummyparent->Width = w
  dummyparent->Height = h
  DIM oldpos as XYPair
  if ignore_offset then
   oldpos.X = s->X
   oldpos.Y = s->Y
   s->X = 0
   s->Y = 0
  end if
  DefaultChildRefresh(dummyparent, s)

  if s->Draw then
   s->Draw(s, page)
  end if
  AutoSortChildren(s)
  s->ChildDraw(s, page)

  if ignore_offset then
   s->X = oldpos.X
   s->Y = oldpos.Y
  end if
  DeleteSlice @dummyparent
 end if
end sub

Function UpdateScreenSlice(byval page as integer) as integer
 'Returns true if the size changed
 dim changed as integer = NO
 updatepagesize page
 with *ScreenSlice
  changed = (.Width <> vpages(page)->w) or (.Height <> vpages(page)->h)
  .Width = vpages(page)->w
  .Height = vpages(page)->h
 end with
 return changed
end function

Sub RefreshSliceScreenPos(byval s as slice ptr)
 'This sub quickly updates a slice's ScreenX and ScreenY
 'without needing to do a full DrawSlice of the whole tree
 'and without respect to the .Visible property
 if s = 0 then exit sub
 DIM attach as Slice Ptr
 attach = GetSliceDrawAttachParent(s)
 if attach = 0 then exit sub
 if attach = ScreenSlice then exit sub
 RefreshSliceScreenPos attach
 attach->ChildRefresh(attach, s)
end sub

Function SliceCollide(byval sl1 as Slice Ptr, sl2 as Slice Ptr) as integer
 'Check for a screen-position collision between slice 1 and slice 2 (regardless of parentage)
 'Note RefreshSliceScreenPos not called here
 if sl1 = 0 or sl2 = 0 then return 0
 'AABB collision test
 if sl1->Width + sl2->Width <= abs(2 * sl1->ScreenX + sl1->Width - 2 * sl2->ScreenX - sl2->Width) then return NO
 if sl1->Height + sl2->Height <= abs(2 * sl1->ScreenY + sl1->Height - 2 * sl2->ScreenY - sl2->Height) then return NO
 return YES
end function

Function SliceCollidePoint(byval sl as Slice Ptr, byval x as integer, byval y as integer) as integer
 'Check if a point collides with a slice's screen position
 'Note RefreshSliceScreenPos not called here
 if sl = 0 then return 0
 if x >= sl->ScreenX and x < sl->ScreenX + sl->Width then
  if y >= sl->ScreenY and y < sl->ScreenY + sl->Height then
   return YES
  end if
 end if
 return NO
end function

Function SliceContains(byval sl1 as Slice Ptr, byval sl2 as Slice Ptr) as integer
 'Check if sl2 is completely contained inside sl1
 if sl1 = 0 or sl2 = 0 then return 0
 RefreshSliceScreenPos(sl1)
 RefreshSliceScreenPos(sl2)
 if SliceCollidePoint(sl1, sl2->ScreenX, sl2->ScreenY) then
  if SliceCollidePoint(sl1, sl2->ScreenX + sl2->Width-1, sl2->ScreenY + sl2->Height-1) then
   'no nonrectangular slices (yet)
   'if SliceCollidePoint(sl1, sl2->ScreenX + sl2->Width-1, sl2->ScreenY) then
    'if SliceCollidePoint(sl1, sl2->ScreenX, sl2->ScreenY + sl2->Height-1) then
    'end if
   'end if
   return YES
  end if
 end if
 return NO
end function

Function FindSliceCollision(byval parent as Slice Ptr, byval sl as Slice Ptr, byref num as integer, byval descend as integer) as Slice Ptr
 'We don't call RefreshSliceScreenPos for efficiency; we expect the calling code to do that
 if parent = 0 or sl = 0 then debug "FindSliceCollision null ptr": return 0
 DIM as Slice Ptr s, temp
 s = parent->FirstChild
 while s
  if s <> sl then
   with *s
    parent->ChildRefresh(parent, s)
 
    if .SliceType <> slSpecial and SliceCollide(s, sl) then  '--impossible to encounter the root
     if num = 0 then return s
     num -= 1
    end if
 
    if descend then
     temp = FindSliceCollision(s, sl, num, YES)
     if temp then return temp
    end if
   end with
  end if
  s = s->NextSibling
 wend
 return NULL
end function

Function FindSliceAtPoint(byval parent as Slice Ptr, byval x as integer, byval y as integer, byref num as integer, byval descend as integer) as Slice Ptr
 'We don't call RefreshSliceScreenPos for efficiency; we expect the calling code to do that
 if parent = 0 then debug "FindSliceAtPoint null ptr": return 0
 DIM as Slice Ptr s, temp
 s = parent->FirstChild
 while s
  with *s
   parent->ChildRefresh(parent, s)

   if .SliceType <> slSpecial and SliceCollidePoint(s, x, y) then  '--impossible to encounter the root
    if num = 0 then return s
    num -= 1
   end if

   if descend then
    temp = FindSliceAtPoint(s, x, y, num, YES)
    if temp then return temp
   end if
  end with
  s = s->NextSibling
 wend
 return NULL
end function

Sub SliceClamp(byval sl1 as Slice Ptr, byval sl2 as Slice Ptr)
 'Don't confuse this with a slice's .Fill member. This is a one-shot attempt
 'to fit sl2 inside sl1 without doing any resizing.
 if sl1 = 0 or sl2 = 0 then exit sub
 if sl2->Fill then reporterr "SliceClamp cannot move slices with .Fill=ON" : exit sub
 RefreshSliceScreenPos(sl1)
 RefreshSliceScreenPos(sl2)
 dim diff as integer
 diff = sl2->ScreenX - sl1->ScreenX
 '--Horizontal clamp
 if diff < 0 then
  sl2->X += abs(diff)
 else
  diff = (sl2->ScreenX + sl2->Width) - (sl1->ScreenX + sl1->Width)
  if diff > 0 then sl2->X -= abs(diff)
 end if
 '--Verical clamp
 diff = sl2->ScreenY - sl1->ScreenY
 if diff < 0 then
  sl2->Y += abs(diff)
 else
  diff = (sl2->ScreenY + sl2->Height) - (sl1->ScreenY + sl1->Height)
  if diff > 0 then sl2->Y -= abs(diff)
 end if
end sub

'==Slice cloning===============================================================

Function CloneSliceTree(byval sl as slice ptr) as slice ptr
 'clone a duplicate of a slice and all its children.
 'only saveable properties are cloned.
 'The resulting clone is parentless
 dim clone as Slice Ptr
 '--Create another slice of the same type
 clone = NewSliceOfType(sl->SliceType)
 '--Clone all standard properties
 with *clone
  .lookup = sl->lookup
  .x = sl->x
  .y = sl->y
  .Width = sl->Width
  .Height = sl->Height
  .Visible = sl->Visible
  .Mobile = sl->Mobile
  .Clip = sl->Clip
  .Velocity.X = sl->Velocity.X
  .Velocity.Y = sl->Velocity.Y
  .VelTicks.X = sl->VelTicks.X
  .VelTicks.Y = sl->VelTicks.Y
  .Targ.X = sl->Targ.X
  .Targ.Y = sl->Targ.Y
  .TargResidue_X = sl->TargResidue_X
  .TargResidue_Y = sl->TargResidue_Y
  .TargTicks = sl->TargTicks
  .AlignHoriz = sl->AlignHoriz
  .AlignVert = sl->AlignVert
  .AnchorHoriz = sl->AnchorHoriz
  .AnchorVert = sl->AnchorVert
  .PaddingTop = sl->PaddingTop
  .PaddingLeft = sl->PaddingLeft
  .PaddingRight = sl->PaddingRight
  .PaddingBottom = sl->PaddingBottom
  .Fill = sl->Fill
 end with
 '--clone special properties for this slice type
 sl->Clone(sl, clone)
 '--Now clone all the children
 dim ch_slice as Slice Ptr = sl->FirstChild
 dim ch_clone as Slice Ptr
 do while ch_slice <> 0
  ch_clone = CloneSliceTree(ch_slice)
  SetSliceParent ch_clone, clone
  ch_slice = ch_slice->NextSibling
 loop
 '--return the clone
 return clone
end function

'==Slice saving and loading====================================================

'--saving----------------------------------------------------------------------

End Extern

Sub SaveProp(node as Reload.Nodeptr, propname as string, byval value as integer)
 if node = 0 then debug "SaveProp null node ptr": Exit Sub
 Reload.SetChildNode(node, propname, CLNGINT(value))
End Sub

Sub SaveProp(node as Reload.Nodeptr, propname as string, s as string)
 if node = 0 then debug "SaveProp null node ptr": Exit Sub
 Reload.SetChildNode(node, propname, s)
End Sub

Extern "C"

Sub SliceSaveToNode(byval sl as Slice Ptr, node as Reload.Nodeptr)
 if sl = 0 then debug "SliceSaveToNode null slice ptr": Exit Sub
 if node = 0 then debug "SliceSaveToNode null node ptr": Exit Sub
 if Reload.NumChildren(node) <> 0 then debug "SliceSaveToNode non-empty node has " & Reload.NumChildren(node) & " children"
 '--Save standard slice properties
 if sl->lookup <> 0 then
  SaveProp node, "lookup", sl->lookup
 end if
 SaveProp node, "x", sl->x
 SaveProp node, "y", sl->Y
 SaveProp node, "w", sl->Width
 SaveProp node, "h", sl->Height
 SaveProp node, "vis", sl->Visible
 SaveProp node, "mobile", sl->Mobile
 SaveProp node, "clip", sl->Clip
 SaveProp node, "vx", sl->Velocity.X
 SaveProp node, "vy", sl->Velocity.Y
 SaveProp node, "vtickx", sl->VelTicks.X
 SaveProp node, "vticky", sl->VelTicks.Y
 if sl->TargTicks > 0 then
  SaveProp node, "tx", sl->Targ.X
  SaveProp node, "ty", sl->Targ.Y
  SaveProp node, "ttick", sl->TargTicks
  'No need to save TargResidue
 end if
 SaveProp node, "alignh", sl->AlignHoriz
 SaveProp node, "alignv", sl->AlignVert
 SaveProp node, "anchorh", sl->AnchorHoriz
 SaveProp node, "anchorv", sl->AnchorVert
 SaveProp node, "padt", sl->PaddingTop
 SaveProp node, "padl", sl->PaddingLeft
 SaveProp node, "padr", sl->PaddingRight
 SaveProp node, "padb", sl->PaddingBottom
 SaveProp node, "fill", sl->Fill
 if sl->Sorter <> 0 then
  SaveProp node, "sort", sl->Sorter
 end if
 if sl->AutoSort <> 0 then
  SaveProp node, "autosort", sl->AutoSort
 end if
 SaveProp node, "extra0", sl->Extra(0)
 SaveProp node, "extra1", sl->Extra(1)
 SaveProp node, "extra2", sl->Extra(2)
 SaveProp node, "type", SliceTypeName(sl)
 '--Save properties specific to this slice type
 sl->Save(sl, node)
 '--Now save all the children
 if sl->NumChildren > 0 then
  '--make a container node for all the child nodes
  dim children as Reload.NodePtr
  children = Reload.CreateNode(node, "children")
  Reload.AddChild(node, children)
  'now loop through the children of this slice and create a new node for each one
  dim ch_node as Reload.NodePtr
  dim ch_slice as Slice Ptr = sl->FirstChild
  do while ch_slice <> 0
   ch_node = Reload.CreateNode(children, "")
   Reload.AddChild(children, ch_node)
   SliceSaveToNode ch_slice, ch_node
   ch_slice = ch_slice->NextSibling
  loop
 end if
End sub

Sub SliceSaveToFile(byval sl as Slice Ptr, filename as string)
 
 'First create a reload document
 dim doc as Reload.DocPtr
 doc = Reload.CreateDocument()
 if doc = null then
   debug "Reload.CreateDocument failed in SliceSaveToFile"
   exit sub
 end if
 
 'Create a node, and save the slice tree into it
 dim node as Reload.Nodeptr
 node = Reload.CreateNode(doc, "")
 Reload.SetRootNode(doc, node)
 SliceSaveToNode sl, node
 
 'Write the reload document to the file
 Reload.SerializeBin filename, doc
 
 Reload.FreeDocument(doc)

End sub

'--loading---------------------------------------------------------------------

Function LoadPropStr(node as Reload.Nodeptr, propname as string, defaultval as string="") as string
 if node = 0 then debug "LoadPropStr null node ptr": return defaultval
 return Reload.GetChildNodeStr(node, propname, defaultval)
End function

Function LoadProp(node as Reload.Nodeptr, propname as string, byval defaultval as integer=0) as integer
 if node = 0 then debug "LoadProp null node ptr": return defaultval
 return Reload.GetChildNodeInt(node, propname, CLNGINT(defaultval))
End function

Function LoadPropBool(node as Reload.Nodeptr, propname as string, byval defaultval as integer=NO) as integer
 if node = 0 then debug "LoadPropBool null node ptr": return defaultval
 return Reload.GetChildNodeBool(node, propname, defaultval)
End function

Sub SliceLoadFromNode(byval sl as Slice Ptr, node as Reload.Nodeptr)
 if sl = 0 then debug "SliceLoadFromNode null slice ptr": Exit Sub
 if node = 0 then debug "SliceLoadFromNode null node ptr": Exit Sub
 if sl->NumChildren > 0 then debug "SliceLoadFromNode slice already has " & sl->numChildren & " children"
 '--Load standard slice properties
 sl->lookup = LoadProp(node, "lookup")
 sl->x = LoadProp(node, "x")
 sl->y = LoadProp(node, "y")
 sl->Width = LoadProp(node, "w")
 sl->Height = LoadProp(node, "h")
 sl->Visible = LoadPropBool(node, "vis")
 sl->Mobile = LoadPropBool(node, "mobile", YES)
 sl->Clip = LoadPropBool(node, "clip")
 sl->Velocity.X = LoadProp(node, "vx")
 sl->Velocity.Y = LoadProp(node, "vy")
 sl->VelTicks.X = LoadProp(node, "vtickx")
 sl->VelTicks.Y = LoadProp(node, "vticky")
 sl->Targ.X = LoadProp(node, "tx")
 sl->Targ.Y = LoadProp(node, "ty")
 sl->TargTicks = LoadProp(node, "ttick")
 sl->AlignHoriz = LoadProp(node, "alignh")
 sl->AlignVert = LoadProp(node, "alignv")
 sl->AnchorHoriz = LoadProp(node, "anchorh")
 sl->AnchorVert = LoadProp(node, "anchorv")
 sl->PaddingTop = LoadProp(node, "padt")
 sl->PaddingLeft = LoadProp(node, "padl")
 sl->PaddingRight = LoadProp(node, "padr")
 sl->PaddingBottom = LoadProp(node, "padb")
 sl->Fill = LoadPropBool(node, "fill")
 sl->Sorter = LoadProp(node, "sort")
 sl->AutoSort = LoadProp(node, "autosort")
 sl->Extra(0) = LoadProp(node, "extra0")
 sl->Extra(1) = LoadProp(node, "extra1")
 sl->Extra(2) = LoadProp(node, "extra2")
 'now update the type
 dim typestr as string = LoadPropStr(node, "type")
 if typestr = "" then
  debug "Bad type while loading slice from node"
 else
  dim typenum as integer = SliceTypeByName(typestr)
  dim newsl as Slice Ptr = NewSliceOfType(typenum)
  ReplaceSliceType sl, newsl
  '--Load properties specific to this slice type
  sl->Load(sl, node)
 end if
 '--Now load all the children
 dim children as Reload.NodePtr
 children = Reload.GetChildByName(node, "children")
 if children then
  'now loop through the children of this node and create a new slice for each one
  dim ch_slice as Slice Ptr
  dim ch_node as Reload.NodePtr = Reload.FirstChild(children)
  do while ch_node <> 0
   ch_slice = NewSlice(sl)
   SliceLoadFromNode ch_slice, ch_node
   ch_node = Reload.NextSibling(ch_node)
  loop
 end if
End sub

Sub SliceLoadFromFile(byval sl as Slice Ptr, filename as string)
 
 'First create a reload document
 dim doc as Reload.DocPtr
 doc = Reload.LoadDocument(filename, optNoDelay)
 if doc = null then 'the root node will never be null -- Mike
   debug "Reload.LoadDocument failed in SliceLoadFromFile"
   exit sub
 end if
 
 'Populate the slice tree with data from the reload tree
 dim node as Reload.Nodeptr
 node = Reload.DocumentRoot(doc)
 SliceLoadFromNode sl, node
 
 Reload.FreeDocument(doc)

End sub

'--slice debug stuff

SUB SliceDebugRemember(sl as Slice Ptr)
 if ENABLE_SLICE_DEBUG = NO then exit sub
 if sl = 0 then debug "SliceDebugRemember null ptr": exit sub
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) = 0 then
   '--found an empty slot in the slice debug table...
   SliceDebug(i) = sl
   exit sub
  end if
 next i
 '--no more room in the slice debug table
 dim newsize as integer = ubound(SliceDebug) + 50
 debuginfo "enlarge slice debug table to " & newsize
 redim preserve SliceDebug(newsize) as Slice Ptr
 SliceDebugRemember sl
END SUB

SUB SliceDebugForget(sl as Slice Ptr)
 if ENABLE_SLICE_DEBUG = NO then exit sub
 if sl = 0 then debug "SliceDebugForget null ptr": exit sub
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) = sl then
   '--found the slice to forget
   SliceDebug(i) = 0
   exit sub
  end if
 next i
 debug "WARNING: tried to delete slice " & sl & " without any record of creating it!"
END SUB

SUB SliceDebugDump(byval noisy as integer = NO)
 if ENABLE_SLICE_DEBUG = NO then exit sub
 debug "===SLICE DEBUG DUMP==="
 dim count as integer = 0
 dim sl as Slice Ptr
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) <> 0 then
   sl = SliceDebug(i)
   debug "[" & i & " Slice " & sl & " " & SliceTypeName(sl) & " " & SliceLookupCodename(sl) & "]"
   if noisy then
    debug "parent " & sl->parent
    SliceDebugDumpTree sl
   end if
   count += 1
  end if
 next i
 debug count & " slices found in the slice debug table"
END SUB

SUB SliceDebugDumpTree(sl as Slice Ptr, byval indent as integer = 0)
 if sl = 0 then exit sub
 dim s as string
 s = string(indent, " ") & SliceTypeName(sl)
 if sl->Protect then
  s = s & " (P)"
 end if
 s = s & " lookup:" & SliceLookupCodename(sl) & " handle:" & sl->TableSlot & " pos:" & sl->X & "," & sl->Y & " size:" & sl->Width & "x" & sl->Height
 debug s
 SliceDebugDumpTree sl->FirstChild, indent + 1
 SliceDebugDumpTree sl->NextSibling, indent
END SUB

FUNCTION SliceDebugCheck(sl as Slice Ptr) as integer
 if ENABLE_SLICE_DEBUG = NO then debug "SliceDebugCheck not enabled" : RETURN NO
 if sl = 0 then RETURN NO
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) = sl then RETURN YES
 next i
 RETURN NO
END FUNCTION

End Extern
