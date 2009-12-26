'OHRRPGCE GAME - Slice related functionality
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't very crappy
'

#ifdef __FB_LANG__
  #if __FB_LANG__ <> "fb"
'$DYNAMIC
    Option Explicit
  #endif
#endif

#ifdef IS_GAME
extern plotslices() as integer
#endif

#include "allmodex.bi"
#include "common.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"

#include "slices.bi"

'==============================================================================

'String manipulation functions used by save/load
DECLARE Function StripQuotes (s AS STRING) AS STRING
DECLARE Function EscapeChar (s AS STRING, char AS STRING, escaper AS STRING="\") AS STRING
DECLARE Function FindUnquotedChar (s AS STRING, char AS STRING) AS INTEGER

'==============================================================================

Dim SliceTable as SliceTable_

ReDim Shared SliceDebug(50) AS Slice Ptr

'add other slice tables here

'ScreenSlice is used by other slices with ->Attach = slScreen
DIM SHARED ScreenSlice AS Slice Ptr
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

'==General slice code==========================================================

'stub functions:
Sub DrawNullSlice(byval s as slice ptr, byval p as integer) : end sub
Sub DisposeNullSlice(byval s as slice ptr) : end sub
Sub UpdateNullSlice(byval s as slice ptr) : end sub
Sub SaveNullSlice(byval s as slice ptr, byref f as SliceFileWrite) : end sub
Function LoadNullSlice(Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer : return NO: end function

Sub SetupGameSlices
 SliceTable.Root = NewSliceOfType(slRoot)
 
 SliceTable.MapRoot = NewSliceOfType(slContainer, SliceTable.Root, SL_MAPROOT)
 FOR i AS INTEGER = 0 TO maplayerMax
  SliceTable.MapLayer(i) = NewSliceOfType(slMap, SliceTable.MapRoot, SL_MAP_LAYER0 + i)
 NEXT
 
 SliceTable.ScriptSprite = NewSliceOfType(slSpecial, SliceTable.Root, SL_SCRIPT_LAYER)
 SliceTable.ScriptSprite->Fill = YES
 RefreshSliceScreenPos(SliceTable.ScriptSprite)
 
 SliceTable.TextBox = NewSliceOfType(slSpecial, SliceTable.Root, SL_TEXTBOX_LAYER)
 SliceTable.TextBox->Fill = YES
 RefreshSliceScreenPos(SliceTable.TextBox)
 
 'Not used yet, so don't create it!
 'SliceTable.Menu = NewSliceOfType(slSpecial, SliceTable.Root)
 
 SliceTable.ScriptString = NewSliceOfType(slSpecial, SliceTable.Root, SL_STRING_LAYER)

End Sub

Sub DestroyGameSlices (Byval dumpdebug AS INTEGER=0)
 DeleteSlice(@SliceTable.Root, ABS(SGN(dumpdebug)))
 '--after deleting root, all other slices should be gone, but the pointers
 '--in SliceTable still need zeroing
 SliceTable.MapRoot = 0
 FOR i AS INTEGER = 0 TO maplayerMax
  SliceTable.MapLayer(i) = 0
 NEXT
 SliceTable.ScriptSprite = 0
 SliceTable.TextBox = 0
 SliceTable.Menu = 0
 SliceTable.ScriptString = 0
End Sub

FUNCTION SliceTypeName (sl AS Slice Ptr) AS STRING
 IF sl = 0 THEN debug "SliceTypeName null ptr": RETURN "<null ptr>"
 RETURN SliceTypeName(sl->SliceType)
END FUNCTION

FUNCTION SliceTypeName (t AS SliceTypes) AS STRING
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
 END SELECT
 RETURN "Unknown"
END FUNCTION

FUNCTION SliceTypeByName (s AS STRING) AS SliceTypes
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
 END SELECT
END FUNCTION

FUNCTION SliceLookupCodename (sl AS Slice Ptr) AS STRING
 '--Used for debugging
 IF sl = 0 THEN RETURN "[null]"
 SELECT CASE sl->Lookup
  CASE 0: RETURN ""
'--the following is updated from slices.bi using the misc/sl_lookup.py script
'<SLICE LOOKUP NAMES>
  CASE SL_TEXTBOX_TEXT: RETURN "textbox_text"
  CASE SL_TEXTBOX_PORTRAIT: RETURN "textbox_portrait"
  CASE SL_TEXTBOX_CHOICE0: RETURN "textbox_choice0"
  CASE SL_TEXTBOX_CHOICE1: RETURN "textbox_choice1"
  CASE SL_SCRIPT_LAYER: RETURN "script_layer"
  CASE SL_TEXTBOX_LAYER: RETURN "textbox_layer"
  CASE SL_STRING_LAYER: RETURN "string_layer"
  CASE SL_MAPROOT: RETURN "maproot"
  CASE SL_MAP_LAYER0: RETURN "map_layer0"
  CASE SL_MAP_LAYER1: RETURN "map_layer1"
  CASE SL_MAP_LAYER2: RETURN "map_layer2"
'</SLICE LOOKUP NAMES>
  CASE ELSE
   RETURN STR(sl->Lookup)
 END SELECT
 RETURN ""
END FUNCTION

FUNCTION NewSliceOfType (BYVAL t AS SliceTypes, BYVAL parent AS Slice Ptr=0, BYVAL lookup_code AS INTEGER=0) AS Slice Ptr
 DIM newsl AS Slice Ptr
 SELECT CASE t
  CASE slRoot:
   newsl = NewSlice(parent)
   WITH *newsl
    .SliceType = slRoot
    .Attach = slScreen
    .SliceType = slRoot
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
  CASE slContainer:
   newsl = NewSlice(parent)
   newsl->SliceType = slContainer
  CASE slRectangle:
   DIM dat AS RectangleSliceData
   newsl = NewRectangleSlice(parent, dat)
  CASE slSprite:
   DIM dat AS SpriteSliceData
   newsl = NewSpriteSlice(parent, dat)
  CASE slText
   DIM dat AS TextSliceData
   newsl = NewTextSlice(parent, dat)
  CASE slMenu:
   DIM dat AS MenuSliceData
   newsl = NewMenuSlice(parent, dat)
  CASE slMenuItem:
   DIM dat AS MenuItemSliceData
   newsl = NewMenuItemSlice(parent, dat)
  CASE slMap:
   DIM dat AS MapSliceData
   newsl = NewMapSlice(parent, dat)
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
 ret->Attached = 0
 ret->Attach = slSlice
 
 ret->Draw = @DrawNullSlice
 ret->Dispose = @DisposeNullSlice
 ret->Update = @UpdateNullSlice
 ret->Save = @SaveNullSlice
 ret->Load = @LoadNullSlice

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
    debug "DeleteSlice: TableSlot mismatch! Slice " & sl & " slot is " & sl->TableSlot & " which has " & plotslices(sl->TableSlot)
   end if
  else
   debug "DeleteSlice: TableSlot for " & sl & " is invalid: " & sl->TableSlot
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
 if sl = 0 then debug "SetSliceParent null ptr": exit sub

 'first, remove the slice from its existing parent
 OrphanSlice sl
 
 'then, add ourselves to the new parent
 if parent then
  if verifySliceLineage(sl, parent) then
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
  else
   debug "Detected inbreeding in slice system!"
  end if
 end if
 
end sub

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
 if sl1->Parent <> sl2->Parent then debug "SwapSiblingSlices: slices are not siblings": EXIT SUB
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

Sub InsertSiblingSlice(byval sl as slice ptr, byval newsl as slice ptr)
 'newsl will be removed from its current parent (if any) and attached to the same
 'parent as sl
 if sl = 0 then debug "InsertSiblingSlice: null sl": EXIT SUB
 if newsl = 0 then debug "InsertSiblingSlice: null newsl": EXIT SUB
 if sl = newsl then EXIT SUB ' Fail quietly when trying to insert a slice as a sibling
                             ' of itself because this is normal if you are using this function
                             ' to move a slice to the beginning of its sibling list when it is
                             ' already the first sibling
 if sl->Parent = 0 then debug "InsertSiblingSlice: Root shouldn't have siblings": EXIT SUB
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
 
 'Verify the family
 if verifySliceLineage(newsl, sl->Parent) = NO then debug "slice inbreeding detected"
end sub

Sub ReplaceSliceType(byval sl as slice ptr, byref newsl as slice ptr)
 'This takes a new slice (normally from one of the New*Slice functions)
 'and copies its type and type-specific data over an existing tree member.
 'Newsl gets Deleted to prevent it from being used afterwards!
 'Also, this fails if newsl is part of a tree. It must be parentless
 if sl = 0 then debug "ReplaceSliceType null ptr": exit sub
 if newsl = 0 then debug "DrawSlice newsl null ptr": exit sub
 WITH *newsl
  'Make sure that newsl is an orphan already
  IF .Parent <> 0 THEN debug "ReplaceSliceType: Only works with orphaned slices" : EXIT SUB
  'Dispose of any old Slice Type specific data that is about to be replaced
  IF sl->SliceData <> 0 THEN sl->Dispose(sl)
  'Copy over slice identity
  sl->SliceType = .SliceType
  sl->Draw      = .Draw
  sl->Dispose   = .Dispose
  sl->Update    = .Update
  sl->Save      = .Save
  sl->Load      = .Load
  sl->SliceData = .SliceData
  sl->SliceType = .SliceType
  'Break slice connection to data
  .SliceData = 0
  'Now destroy newsl
  DeleteSlice @newsl
 END WITH
End Sub

Function LookupSlice(byval lookup_code as integer) as slice ptr
 RETURN LookupSlice(lookup_code, SliceTable.root)
End Function

Function LookupSlice(byval lookup_code as integer, byval start_sl as slice ptr) as slice ptr
  IF start_sl = 0 THEN RETURN 0 '--fail searching under an invalid slice
  IF lookup_code = 0 THEN RETURN 0 '--fail searching for a zero lookup code
  IF start_sl->Lookup = lookup_code THEN RETURN start_sl '--found it!
  DIM child AS Slice Ptr
  child = start_sl->FirstChild
  DIM result AS Slice Ptr
  WHILE child
   result = LookupSlice(lookup_code, child)
   IF result THEN RETURN result '--found in recursion, pass the result back
   child = child->NextSibling
  WEND
End Function

Function LastChild(byval parent as slice ptr) as slice ptr
 IF parent = 0 THEN RETURN 0
 DIM sl AS Slice Ptr
 sl = parent->FirstChild
 IF sl = 0 THEN RETURN 0
 DIM nextsib AS Slice ptr
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
 dat->bgcol = uiLook(uiTextbox + dat->style * 2)
 dat->fgcol = uiLook(uiTextbox + dat->style * 2 + 1)
 dat->border = dat->style
 dat->style_loaded = YES
end sub

Sub DrawRectangleSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as RectangleSliceData ptr = cptr(RectangleSliceData ptr, sl->SliceData)

 if dat->style >= 0 and dat->style_loaded = NO then
  UpdateRectangleSliceStyle dat
 end if

 edgebox sl->screenx, sl->screeny, sl->width, sl->height, dat->bgcol, dat->fgcol, p, dat->translucent, dat->border
end sub

Sub SaveRectangleSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 if sl = 0 then debug "SaveRectangleSlice null ptr": exit sub
 DIM dat AS RectangleSliceData Ptr
 dat = sl->SliceData
 WriteSliceFileVal f, "style", dat->style, -1
 WriteSliceFileVal f, "fg", dat->fgcol
 WriteSliceFileVal f, "bg", dat->bgcol
 WriteSliceFileBool f, "trans", dat->translucent
 WriteSliceFileVal f, "border", dat->border, -1
End Sub

Function LoadRectangleSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 if sl = 0 then debug "LoadRectangleSlice null ptr": return 0
 dim dat AS RectangleSliceData Ptr
 dat = sl->SliceData
 select case key
  case "style": dat->style = n
  case "fg": dat->fgcol = n
  case "bg": dat->bgcol = n
  case "trans": dat->translucent = n
  case "border": dat->border = n
  case else: return NO
 end select
 return YES
End Function

Function NewRectangleSlice(byval parent as Slice ptr, byref dat as RectangleSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as RectangleSliceData ptr = new RectangleSliceData
 *d = dat
 '--Set defaults here since we have no constructors yet
 d->border = -1
 d->style = -1
 
 ret->SliceType = slRectangle
 ret->SliceData = d
 ret->Draw = @DrawRectangleSlice
 ret->Dispose = @DisposeRectangleSlice
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
                      byval translucent as RectTransTypes=transUndef)
 if sl = 0 then debug "ChangeRectangleSlice null ptr" : exit sub
 if sl->SliceType <> slRectangle then debug "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a rectangle" : exit sub
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

Sub SaveTextSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 if sl = 0 then debug "SaveTextSlice null ptr": exit sub
 DIM dat AS TextSliceData Ptr
 dat = sl->SliceData
 WriteSliceFileVal f, "s", dat->s
 WriteSliceFileVal f, "col", dat->col
 WriteSliceFileBool f, "outline", dat->outline
 WriteSliceFileBool f, "wrap", dat->wrap
 WriteSliceFileVal f, "bgcol", dat->bgcol
End Sub

Function LoadTextSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 if sl = 0 then debug "LoadTextSlice null ptr": return 0
 dim dat AS TextSliceData Ptr
 dat = sl->SliceData
 select case key
  case "s": dat->s = StripQuotes(valstr) : checkn = NO
  case "col": dat->col = n
  case "outline": dat->outline = n
  case "wrap": dat->wrap = n
  case "bgcol": dat->bgcol = n
  case else: return NO
 end select
 return YES
End Function

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
 ret->Update = @UpdateTextSlice
 ret->Save = @SaveTextSlice
 ret->Load = @LoadTextSlice

 ret->Width = textwidth(d->s)
 'split(d->s, d->lines())
 
 return ret
end function

'All arguments default to no-change
Sub ChangeTextSlice(byval sl as slice ptr,_
                      byval s as string=CHR(1) & CHR(255),_
                      byval col as integer=-1,_
                      byval outline as integer=-2,_
                      byval wrap as integer=-2,_
                      byval bgcol as integer=-1)
 if sl = 0 then debug "ChangeTextSlice null ptr" : exit sub
 if sl->SliceType <> slText then debug "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as text" : exit sub
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
   debug "null sprite ptr for slice " & sl
   exit sub
  end if
  if .frame >= sprite_sizes(.spritetype).frames or .frame < 0 then
   debug "out of range frame " & .frame & " for slice " & sl
   .frame = 0
  end if
  
  spr += .frame

  'some redesign needed to prevent this continous flipping
  if .flipHoriz then
   if have_copy = NO THEN spr = sprite_duplicate(spr)
   have_copy = YES
   sprite_flip_horiz(spr)
  end if
  if .flipVert then
   if have_copy = NO THEN spr = sprite_duplicate(spr)
   have_copy = YES
   sprite_flip_vert(spr)
  end if
 
  sprite_draw spr, .img.pal, sl->screenX, sl->screenY, , ,dpage
  
  if have_copy then
   sprite_unload(@spr)
  end if
 end with
end sub

Function GetSpriteSliceData(byval sl as slice ptr) as SpriteSliceData ptr
 if sl = 0 then debug "GetSpriteSliceData null ptr": return 0
 return sl->SliceData
End Function

'Make no mistake, this is just an unproven hack
'(and it only accepts 4 bit graphics, without palettes!!)
Sub SetSpriteFrame(byval sl as slice ptr, byval fr as Frame ptr)
 if sl = 0 then debug "SetSpriteFrame null ptr": exit sub
 dim dat as SpriteSliceData ptr = cptr(SpriteSliceData ptr, sl->SliceData)

 with *dat
  'Should not matter whether the sprite is loaded; however if we set .loaded=YES, have to have a palette
  '(since this is 4-bit). Where do we get the palette from? O:
  sprite_unload(@.img.sprite)
  if .img.pal = 0 then palette16_load(.pal, .spritetype, .record)
  .img.sprite = fr  'sprite_reference(fr)

  sl->Width = fr->w
  sl->Height = fr->h
  .loaded = YES

  .spritetype = 0
  .record = -1
 end with
End Sub

Sub SaveSpriteSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 if sl = 0 then debug "SaveSpriteSlice null ptr": exit sub
 DIM dat AS SpriteSliceData Ptr
 dat = sl->SliceData
 WriteSliceFileVal f, "sprtype", dat->spritetype
 WriteSliceFileVal f, "rec", dat->record
 WriteSliceFileVal f, "pal", dat->pal, -1
 WriteSliceFileVal f, "frame", dat->frame
 WriteSliceFileBool f, "fliph", dat->flipHoriz
 WriteSliceFileBool f, "flipv", dat->flipVert
end sub

Function LoadSpriteSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 if sl = 0 then debug "LoadSpriteSlice null ptr": return 0
 dim dat AS SpriteSliceData Ptr
 dat = sl->SliceData
 select case key
  case "sprtype": dat->spritetype = n
  case "rec": dat->record = n
  case "pal": dat->pal = n
  case "frame": dat->frame = n
  case "fliph": dat->flipHoriz = n
  case "flipv": dat->flipVert = n
  case else: return NO
 end select
 return YES
End Function

Function NewSpriteSlice(byval parent as Slice ptr, byref dat as SpriteSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as SpriteSliceData ptr = new SpriteSliceData
 *d = dat

 d->pal = -1 'FIXME: Hack to make up for the lack of constructors
 
 ret->SliceType = slSprite
 ret->SliceData = d
 ret->Draw = @DrawSpriteSlice
 ret->Dispose = @DisposeSpriteSlice
 ret->Save = @SaveSpriteSlice
 ret->Load = @LoadSpriteSlice
 
 return ret
end function

'All arguments default to no-change
Sub ChangeSpriteSlice(byval sl as slice ptr,_
                      byval spritetype as integer=-1,_
                      byval record as integer=-1,_
                      byval pal as integer = -2,_
                      byval frame as integer = -1,_
                      byval fliph as integer = -2,_
                      byval flipv as integer = -2)
 if sl = 0 then debug "ChangeSpriteSlice null ptr" : exit sub
 if sl->SliceType <> slSprite then debug "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a sprite" : exit sub
 dim dat as SpriteSliceData Ptr = sl->SliceData
 with *dat
  if spritetype >= 0 then
   .spritetype = spritetype
   .loaded = NO
   sl->Width = sprite_sizes(.spritetype).size.x
   sl->Height = sprite_sizes(.spritetype).size.y
  end if
  if record >= 0 then
   .record = record
   .loaded = NO
  end if
  if pal >= -1 then
   .pal = pal
   .loaded = NO
  end if
  if frame >= 0 then
   if frame >= sprite_sizes(.spritetype).frames then
    debug "Sprite frame " & frame & " is out of range for " & sprite_sizes(.spritetype).name & " sprites, valid range 0 to " & sprite_sizes(.spritetype).frames - 1
   else
    .frame = frame
   end if
  end if
  if fliph > -2 then .flipHoriz = (fliph <> 0) : .loaded = NO
  if flipv > -2 then .flipVert = (flipv <> 0) : .loaded = NO
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
  drawmap *.tiles, sl->ScreenX * -1, sl->ScreenY * -1, .overlay, .tileset, p, .transparent
 end with
end sub

Function GetMapSliceData(byval sl as slice ptr) as MapSliceData ptr
 if sl = 0 then debug "GetMapSliceData null ptr": return 0
 return sl->SliceData
End Function

Sub SaveMapSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 if sl = 0 then debug "SaveMapSlice null ptr": exit sub
 DIM dat AS SpriteSliceData Ptr
 dat = sl->SliceData
 'FIXME: current MapSlice impl. has no savable properties
end sub

Function LoadMapSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 if sl = 0 then debug "LoadMapSlice null ptr": return 0
 dim dat AS SpriteSliceData Ptr
 dat = sl->SliceData
 'FIXME: current MapSlice impl. has no savable properties
 select case key
  'case "name": dat->name = n
  case else: return NO
 end select
 return YES
End Function

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
 ret->Save = @SaveMapSlice
 ret->Load = @LoadMapSlice
 
 return ret
end function

Sub ChangeMapSliceTileset(byval sl as slice ptr, byval tileset as TilesetData ptr)
 if sl = 0 then debug "ChangeMapSliceTileset null ptr" : exit sub
 if sl->SliceType <> slMap then debug "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a map" : exit sub
 dim dat as MapSliceData Ptr = sl->SliceData
 dat->tileset = tileset 'NOTE: *shiver* pointers make me cringe.
end sub

Sub ChangeMapSlice(byval sl as slice ptr,_
                   byval tiles as TileMap ptr=cast(TileMap ptr, 1),_
                   byval transparent as integer=-2,_
                   byval overlay as integer=-1)
 if sl = 0 then debug "ChangeMapSlice null ptr" : exit sub
 if sl->SliceType <> slMap then debug "Attempt to use " & SliceTypeName(sl) & " slice " & sl & " as a map" : exit sub
 dim dat as MapSliceData Ptr = sl->SliceData
 with *dat
  if tiles <> 1 then
   .tiles = tiles
   if tiles = NULL then
    sl->Width = 0
    sl->Height = 0
   else
    sl->Width = tiles->wide * 20
    sl->Height = tiles->high * 20
   end if
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

Sub SaveMenuSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 if sl = 0 then debug "SaveMenuSlice null ptr": exit sub
 DIM dat AS MenuSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "SaveMenuSlice not implemented"
end sub

Function LoadMenuSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 if sl = 0 then debug "LoadMenuSlice null ptr": return 0
 dim dat AS MenuSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "LoadMenuSlice not implemented"
 select case key
  'case "keyname": dat->datamember = n
  case else: return NO
 end select
 return YES
End Function

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

Sub SaveMenuItemSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 if sl = 0 then debug "GetMenuItemSliceData null ptr": exit sub
 DIM dat AS MenuItemSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "SaveMenuItemSlice not implemented"
end sub

Function LoadMenuItemSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 if sl = 0 then debug "LoadMenuItemSlice null ptr": return 0
 dim dat AS MenuItemSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "LoadMenuItemSlice not implemented"
 select case key
  'case "keyname": dat->datamember = n
  case else: return NO
 end select
 return YES
End Function

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

Function GetSliceDrawAttachParent(BYVAL sl AS Slice Ptr) AS Slice Ptr
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

Function SliceXAlign(BYVAL sl AS Slice Ptr, BYVAL alignTo AS Slice Ptr) AS INTEGER
 if sl = 0 then debug "SliceXAlign null ptr": Return 0
 SELECT CASE sl->AlignHoriz
  CASE 0: RETURN alignTo->ScreenX + alignTo->paddingLeft
  CASE 1: RETURN alignTo->ScreenX + alignTo->paddingLeft + (alignTo->Width - alignTo->paddingLeft - alignTo->paddingRight) \ 2
  CASE 2: RETURN alignTo->ScreenX + alignTo->Width - alignTo->paddingRight
 END SELECT
End Function

Function SliceYAlign(BYVAL sl AS Slice Ptr, BYVAL alignTo AS Slice Ptr) AS INTEGER
 if sl = 0 then debug "SliceYAlign null ptr": Return 0
 SELECT CASE sl->AlignVert
  CASE 0: RETURN alignTo->ScreenY + alignTo->paddingTop
  CASE 1: RETURN alignTo->ScreenY + alignTo->paddingTop + (alignTo->Height - alignTo->paddingTop - alignTo->paddingBottom) \ 2
  CASE 2: RETURN alignTo->ScreenY + alignTo->Height - alignTo->paddingBottom
 END SELECT
End Function

Function SliceXAnchor(BYVAL sl AS Slice Ptr) AS INTEGER
 if sl = 0 then debug "SliceXAnchor null ptr": Return 0
 SELECT CASE sl->AnchorHoriz
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Width \ 2
  CASE 2: RETURN sl->Width
 END SELECT
End Function

Function SliceYAnchor(BYVAL sl AS Slice Ptr) AS INTEGER
 if sl = 0 then debug "SliceYAnchor null ptr": Return 0
 SELECT CASE sl->AnchorVert
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Height \ 2
  CASE 2: RETURN sl->Height
 END SELECT
End Function

Function SliceEdgeX(BYVAL sl AS Slice Ptr, BYVAL edge AS INTEGER) AS INTEGER
 if sl = 0 then debug "SliceEdgeX null ptr": Return 0
 SELECT CASE edge
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Width \ 2
  CASE 2: RETURN sl->Width
 END SELECT
End Function

Function SliceEdgeY(BYVAL sl AS Slice Ptr, BYVAL edge AS INTEGER) AS INTEGER
 if sl = 0 then debug "SliceEdgeY null ptr": Return 0
 SELECT CASE edge
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Height \ 2
  CASE 2: RETURN sl->Height
 END SELECT
End Function

Sub LocalRefreshSliceScreenPos(byval s as slice ptr, byval attach as slice ptr)
 if s = 0 then debug "LocalRefreshSliceScreenPos null ptr": exit sub
 with *s
  if .Fill then
   .ScreenX = attach->ScreenX + attach->paddingLeft
   .ScreenY = attach->ScreenY + attach->paddingTop
   .Width = attach->Width - attach->paddingLeft - attach->paddingRight
   .height = attach->Height - attach->paddingTop - attach->paddingBottom
  else ' Not fill
   .ScreenX = .X + SliceXAlign(s, attach) - SliceXAnchor(s)
   .ScreenY = .Y + SliceYAlign(s, attach) - SliceYAnchor(s)
  end if
 end with
end sub

Sub DrawSlice(byval s as slice ptr, byval page as integer)
 if s = 0 then debug "DrawSlice null ptr": exit sub
 'first, draw this slice
 if s->Visible then
  'calc it's X,Y
  DIM attach AS Slice Ptr
  attach = GetSliceDrawAttachParent(s)
  with *s
   LocalRefreshSliceScreenPos s, attach
   
   if .Draw <> 0 THEN .Draw(s, page)
   'draw its children
   dim ch as slice ptr = .FirstChild
   do while ch <> 0
    DrawSlice(ch, page)
    ch = ch->NextSibling
   Loop
  end with
 end if
end sub

Sub RefreshSliceScreenPos(byval s as slice ptr)
 'This sub quickly updates a slice's ScreenX and ScreenY
 'without needing to do a full DrawSlice of the whole tree
 'and without respect to the .Visible property
 if s = 0 then exit sub
 DIM attach AS Slice Ptr
 attach = GetSliceDrawAttachParent(s)
 if attach = 0 then exit sub
 if attach = ScreenSlice then exit sub
 RefreshSliceScreenPos attach
 LocalRefreshSliceScreenPos s, attach
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
    LocalRefreshSliceScreenPos s, parent
 
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
   LocalRefreshSliceScreenPos s, parent

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
 if sl2->Fill then debug "SliceClamp cannot move slices with .Fill=ON" : exit sub
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

'==Slice saving and loading====================================================

'--String manupilation functions used by saving/loading------------------------

Function FindUnquotedChar (s AS STRING, char AS STRING) AS INTEGER
 'Returns the index of the first occurance of a char that is not inside a double-quote
 DIM mode AS INTEGER = 0
 FOR i AS INTEGER = 0 TO LEN(s) - 1
  SELECT CASE mode
   CASE 0'--Looking
    IF s[i] = ASC("""") THEN mode = 1
    IF s[i] = ASC(char) THEN RETURN i
   CASE 1'--"Found opening doublequote, seek another
    IF s[i] = ASC("""") THEN mode = 0
    IF s[i] = ASC("\") THEN mode = 2
   CASE 2'--Ignoring any backslash escaped chars inside a string
    mode = 1 ' Go back to searching for an ending quote
  END SELECT
 NEXT i
 RETURN -1
End Function

Function EscapeChar (s AS STRING, char AS STRING, escaper AS STRING="\") AS STRING
 DIM result AS STRING = ""
 FOR i AS INTEGER = 0 TO LEN(s) - 1
  IF s[i] = ASC(char) THEN result &= escaper
  result &= CHR(s[i])
 NEXT i
 RETURN result
End Function

Function StripQuotes (s AS STRING) AS STRING
 IF LEFT(s, 1) <> """" OR RIGHT(s, 1) <> """" THEN debug "StripQuotes: unmatched quotes": debug s
 DIM result AS STRING = ""
 FOR i AS INTEGER = 1 TO LEN(s) - 2
  IF s[i] = ASC("\") AND s[i+1] = ASC("""") THEN CONTINUE FOR
  result &= CHR(s[i])
 NEXT i
 RETURN result
End Function

'--saving----------------------------------------------------------------------

Sub OpenSliceFileWrite (BYREF f AS SliceFileWrite, filename AS STRING)
 f.name = filename
 f.indent = 0
 f.handle = FREEFILE
 OPEN f.name FOR OUTPUT AS f.handle
 WriteSliceFileLine f, "#OHR SliceTree - This format has not yet been finalized"
End Sub

Sub CloseSliceFileWrite (BYREF f AS SliceFileWrite)
 IF f.indent <> 0 THEN debug "SliceFileWrite indent check fail " & f.indent & " " & f.name
 CLOSE f.handle
End Sub

Sub WriteSliceFileLine (BYREF f AS SliceFileWrite, s AS STRING)
 PRINT # f.handle, STRING(f.indent, " ") & s
End sub

Sub WriteSliceFileVal (BYREF f AS SliceFileWrite, nam AS STRING, s AS STRING, quotes AS INTEGER=YES, default AS STRING="", BYVAL skipdefault AS INTEGER=YES)
 IF skipdefault THEN
  IF s = default THEN EXIT SUB
 END IF
 DIM valstring AS STRING = s
 IF quotes THEN
  valstring = """" & EscapeChar(s, """") & """"
 END IF
 WriteSliceFileLine f, LCASE(nam) & ":" & valstring
End Sub

Sub WriteSliceFileVal (BYREF f AS SliceFileWrite, nam AS STRING, n AS INTEGER, default AS INTEGER=0, BYVAL skipdefault AS INTEGER=YES)
 IF skipdefault THEN
  IF n = default THEN EXIT SUB
 END IF
 WriteSliceFileLine f, LCASE(nam) & ":" & n
End Sub

Sub WriteSliceFileBool (BYREF f AS SliceFileWrite, nam AS STRING, b AS INTEGER, default AS INTEGER=NO, BYVAL skipdefault AS INTEGER=YES)
 IF skipdefault THEN
  IF b = default THEN EXIT SUB
 END IF
 WriteSliceFileVal f, nam, yesorno(b, "true", "false"), NO
End Sub

Sub SaveSlice (BYREF f AS SliceFileWrite, BYVAL sl AS Slice Ptr)
 if sl = 0 then debug "SaveSlice null ptr": Exit Sub
 WriteSliceFileLine f, "{"
 f.indent += 1
 WriteSliceFileVal f, "x", sl->X
 WriteSliceFileVal f, "y", sl->Y
 WriteSliceFileVal f, "w", sl->Width
 WriteSliceFileVal f, "h", sl->Height
 WriteSliceFileBool f, "vis", sl->Visible
 WriteSliceFileVal f, "alh", sl->AlignHoriz
 WriteSliceFileVal f, "alv", sl->AlignVert
 WriteSliceFileVal f, "anh", sl->AnchorHoriz
 WriteSliceFileVal f, "anv", sl->AnchorVert
 WriteSliceFileVal f, "padt", sl->PaddingTop
 WriteSliceFileVal f, "padl", sl->PaddingLeft
 WriteSliceFileVal f, "padr", sl->PaddingRight
 WriteSliceFileVal f, "padb", sl->PaddingBottom
 WriteSliceFileBool f, "fill", sl->Fill
 'WriteSliceFileVal f, "attach", sl->Attach 'FIXME: should probably store this as a string?
 'WriteSliceFileVal f, "attached", "" 'this should definitely NOT be the pointer. Instead need some other scheme for storing this
 WriteSliceFileVal f, "type", SliceTypeName(sl), , , NO 'Never omit type, even if it something crazy like ""
 'Now save all properties specific to this type of slice
 sl->Save(sl, f)
 IF sl->NumChildren > 0 THEN
  'Now save all children
  WriteSliceFileLine f, "child:["
  f.indent += 1
  DIM child AS Slice Ptr = sl->FirstChild
  DO WHILE child <> 0
   SaveSlice f, child
   child = child->NextSibling
  LOOP
  f.indent -= 1
  WriteSliceFileLine f, "]"
 END IF
 f.indent -= 1
 WriteSliceFileLine f, "}"
End sub

'--loading---------------------------------------------------------------------

Sub OpenSliceFileRead (BYREF f AS SliceFileRead, filename AS STRING)
 f.name = filename
 f.handle = FREEFILE
 OPEN f.name FOR INPUT AS f.handle
End Sub

Sub CloseSliceFileRead (BYREF f AS SliceFileRead)
 CLOSE f.handle
End Sub

Function ReadSliceFileLine (BYREF f AS SliceFileRead) AS STRING
 DIM s AS STRING
 LINE INPUT # f.handle, s
 f.linenum += 1
 RETURN s
End Function

Function CleanSliceFileLine (s AS STRING) AS STRING
 DIM result AS STRING
 result = TRIM(s)
 DIM commentmark AS INTEGER
 commentmark = FindUnquotedChar(result, "#")
 IF commentmark >= 0 THEN
  '--Strip out any comments
  result = MID(result, 1, commentmark)
 END IF
 RETURN result
End Function

Function LoadSliceSplitPair (BYREF s AS STRING, BYREF key AS STRING, BYREF valstr AS STRING)
 'Returns NO on failure
 DIM colon AS INTEGER
 colon = FindUnquotedChar(s, ":")
 IF colon < 0 THEN RETURN NO
 key = MID(s, 1, colon)
 valstr = MID(s, colon + 2)
 RETURN YES
End Function

Function LoadSliceConvertInt(BYREF s AS STRING) AS INTEGER
 IF s = "true" THEN RETURN -1
 IF s = "false" THEN RETURN 0
 RETURN VALINT(s)
End Function

ENUM LoadSliceMode
 lsmBegin
 lsmReading
 lsmNextChild
END ENUM

Sub LoadSlice (BYREF f AS SliceFileRead, BYVAL sl AS Slice Ptr, BYVAL skip_to_read AS INTEGER=NO)
 'sl should be a new empty slice. Its data will get overwritten.
 if sl = 0 then debug "LoadSlice null ptr": Exit Sub
 DIM mode AS LoadSliceMode
 mode = lsmBegin
 IF skip_to_read THEN mode = lsmReading
 DIM rawline AS STRING
 DIM s AS STRING
 DIM key AS STRING
 DIM valstr AS STRING
 DIM n AS INTEGER
 DIM checkn AS INTEGER
 DIM clean_exit AS INTEGER = NO
 DIM typestr AS STRING
 DIM typenum AS SliceTypes
 DIM newsl AS Slice Ptr
 DO
  rawline = ReadSliceFileLine(f)
  s = CleanSliceFileLine(rawline)
  IF s = "" THEN CONTINUE DO '--Ignore blank lines
  SELECT CASE mode
   CASE lsmBegin
    IF s = "{" THEN '--start a new slice
     mode = lsmReading
    ELSE
     debug "LoadSlice expected { in line " & f.linenum & " but found:"
     debug rawline 
    END IF
   CASE lsmReading
    IF s = "}" THEN
     clean_exit = YES
     EXIT DO
    END IF
    IF LoadSliceSplitPair(s, key, valstr) = NO THEN
     debug "LoadSliceSplitPair failed on line " & f.linenum
     debug rawline
    END IF
    n = LoadSliceConvertInt(valstr)
    checkn = YES
    SELECT CASE key
     CASE "x": sl->X = n
     CASE "y": sl->Y = n
     CASE "w": sl->Width = n
     CASE "h": sl->Height = n
     CASE "vis": sl->Visible = n
     CASE "alh": sl->AlignHoriz = n
     CASE "alv": sl->AlignVert = n
     CASE "anh": sl->AnchorHoriz = n
     CASE "anv": sl->AnchorVert = n
     CASE "padt": sl->PaddingTop = n
     CASE "padl": sl->PaddingLeft = n
     CASE "padr": sl->PaddingRight = n
     CASE "padb": sl->PaddingBottom = n
     CASE "fill": sl->Fill = n
     CASE "type"
      checkn = NO
      typestr = StripQuotes(valstr)
      typenum = SliceTypeByName(typestr)
      newsl = NewSliceOfType(typenum)
      ReplaceSliceType sl, newsl
     CASE "child"
      checkn = NO
      IF valstr <> "[" THEN
       debug "LoadSlice expected [ in line " & f.linenum
       debug rawline
      END IF
      'Append a new child slice, then recurse to populate it
      newsl = NewSlice(sl)
      LoadSlice f, newsl
      mode = lsmNextChild
     CASE ELSE
      '--This key was not understood as a genereric data member, check to see if
      '--it is understood as a data member specific to this type
      IF sl->Load(sl, key, valstr, n, checkn) = NO THEN
       debug "LoadSlice ignored key """ & key & """ at line " & f.linenum
       debug rawline
      END IF
    END SELECT
    IF checkn THEN
     IF STR(n) = valstr OR (n = -1 AND valstr = "true") OR (n = 0 AND valstr = "false") THEN
     ELSE
      debug "LoadSlice integer conversion mismatch " & n & "<>" & valstr & " in line " & f.linenum
      debug rawline 
     END IF
    END IF
   CASE lsmNextChild
    IF s = "]" THEN
     mode = lsmReading
    ELSEIF s = "{" THEN
     'Append another child, then recurse to populate it
     newsl = NewSlice(sl)
     LoadSlice f, newsl, YES
    ELSE
     debug "LoadSlice: Expected ] or { in line " & f.linenum & " but found:"
     debug rawline
    END IF
  END SELECT
 LOOP UNTIL EOF(f.handle)
 IF clean_exit = NO THEN
  debug "LoadSlice: File ended mid-slice on line " & f.linenum
  debug rawline
 END IF
End sub

SUB SliceDebugRemember(sl AS Slice Ptr)
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

SUB SliceDebugForget(sl AS Slice Ptr)
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

SUB SliceDebugDump(noisy AS INTEGER = NO)
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

SUB SliceDebugDumpTree(sl as Slice Ptr, indent as integer = 0)
 if sl = 0 then exit sub
 debug string(indent, " ") & SliceTypeName(sl) & " " & SliceLookupCodename(sl)
 SliceDebugDumpTree sl->FirstChild, indent + 1
 SliceDebugDumpTree sl->NextSibling, indent
END SUB

FUNCTION SliceDebugCheck(sl as Slice Ptr) AS INTEGER
 if ENABLE_SLICE_DEBUG = NO then debug "SliceDebugCheck not enabled" : RETURN NO
 if sl = 0 then RETURN NO
 for i as integer = 0 to ubound(SliceDebug)
  if SliceDebug(i) = sl then RETURN YES
 next i
 RETURN NO
END FUNCTION

'==Epic prophecy of the construcinator=========================================
/'

AND SO THE PROPHECY WAS SPOKEN:

WHEN SO THE SOURCE IS COMPILED WITH -LANG FB, THEN THE LEGENDARY CONSTRUCTORS SHALL BE BORN
Constructor RectangleSliceData (byval bg as integer = -1, byval tr as RectTransType = rectFuzzy, byval fg as integer = -1, byval bor as integer = -1)
 with this
  .bgcol = bg
  if fgcol = -1 then
   .fgcol = uilook(uiTextBoxFrame)
  else
   .fgcol = fg
  end if
  if bgcol = -1 then
   .bgcol = uilook(uiTextBox)
  else
   .bgcol = fg
  end if
  .border = bor
  .translucent = tr
 end with
End Constructor
'/
