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

#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
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

REDIM Slices(100) as Slice Ptr

Dim SliceTable as SliceTable_

'add other slice tables here

'ScreenSlice is used by other slices with ->Attach = slScreen
DIM SHARED ScreenSlice AS Slice Ptr
ScreenSlice = NewSlice()
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
 SliceTable.Root = NewSlice
 SliceTable.Root->Attach = slScreen
 SliceTable.Root->SliceType = slRoot
 
 SliceTable.Map = NewSlice(SliceTable.Root)
 
 SliceTable.ScriptSprite = NewSlice(SliceTable.Root)
 
 SliceTable.TextBox = NewSlice(SliceTable.Root)
 
 SliceTable.Menu = NewSlice(SliceTable.Root)
 
 SliceTable.ScriptString = NewSlice(SliceTable.Root)

End Sub

Sub DestroyGameSlices
 if SliceTable.Root then
  DeleteSlice(@SliceTable.Map)
  DeleteSlice(@SliceTable.ScriptSprite)
  DeleteSlice(@SliceTable.TextBox)
  DeleteSlice(@SliceTable.Menu)
  DeleteSlice(@SliceTable.ScriptString)
 
  DeleteSlice(@SliceTable.Root)
 end if
 
End Sub

FUNCTION SliceTypeName (sl AS Slice Ptr) AS STRING
 RETURN SliceTypeName(sl->SliceType)
END FUNCTION

FUNCTION SliceTypeName (t AS SliceTypes) AS STRING
 SELECT CASE t
  CASE slRoot:           RETURN "Root"
  CASE slSpecial:        RETURN "Special"
  CASE slRectangle:      RETURN "Rectangle"
  CASE slStyleRectangle: RETURN "Styled Rect"
  CASE slSprite:         RETURN "Sprite"
  CASE slText:           RETURN "Text"
  CASE slMenu:           RETURN "Menu"
  CASE slMenuItem:       RETURN "MenuItem"
 END SELECT
 RETURN "Unknown"
END FUNCTION

FUNCTION SliceTypeByName (s AS STRING) AS SliceTypes
 SELECT CASE s
  CASE "Root":           RETURN slRoot
  CASE "Special":        RETURN slSpecial
  CASE "Rectangle":      RETURN slRectangle
  CASE "Styled Rect":    RETURN slStyleRectangle
  CASE "Sprite":         RETURN slSprite
  CASE "Text":           RETURN slText
  CASE "Menu":           RETURN slMenu
  CASE "MenuItem":       RETURN slMenuItem
 END SELECT
END FUNCTION

FUNCTION NewSliceOfType (BYVAL t AS SliceTypes, BYVAL parent AS Slice Ptr=0) AS Slice Ptr
 SELECT CASE t
  CASE slRoot:
   DIM newsl AS Slice Ptr
   newsl = NewSlice(parent)
   newsl->SliceType = slRoot
   RETURN newsl
  CASE slSpecial:
   DIM newsl AS Slice Ptr
   newsl = NewSlice(parent)
   newsl->SliceType = slSpecial
   RETURN newsl
  CASE slRectangle:
   DIM dat AS RectangleSliceData
   RETURN NewRectangleSlice(parent, dat)
  CASE slStyleRectangle:
   DIM dat AS StyleRectangleSliceData
   RETURN NewStyleRectangleSlice(parent, dat)
  CASE slSprite:
   DIM dat AS SpriteSliceData
   dat.pal = -1 'FIXME: Hack to make up for the lack of constructors
   RETURN NewSpriteSlice(parent, dat)
  CASE slText
   DIM dat AS TextSliceData
   RETURN NewTextSlice(parent, dat)
  CASE slMenu:
   DIM dat AS MenuSliceData
   RETURN NewMenuSlice(parent, dat)
  CASE slMenuItem:
   DIM dat AS MenuItemSliceData
   RETURN NewMenuItemSlice(parent, dat)
 END SELECT
 debug "NewSliceByType: Warning! type " & t & " is invalid"
 RETURN NewSlice(parent)
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
 
 return ret
End Function

'Deletes a slice, and any children (and their children (and their...))
Sub DeleteSlice(Byval s as Slice ptr ptr)
 if s = 0 then exit sub  'can't do anything
 if *s = 0 then exit sub 'already freed
 
 dim sl as slice ptr = *s
 
 'first thing's first.
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
  nxt->NextSibling = nxt
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
  DeleteSlice(@ch)
  ch = nxt
 loop
 
 'finally, we need to remove ourself from the global slice table
 for i as integer = lbound(Slices) to ubound(Slices)
  if Slices(i) = sl then
   Slices(i) = 0
   exit for 'if it's possible for us to be in the table more than once,
            'we need to get rid of this exit for.
  end if
 next
 
 delete sl
 *s = 0
End Sub

Sub SetSliceParent(byval sl as slice ptr, byval parent as slice ptr)
 'first, remove the slice from its existing parent
 dim as slice ptr nxt, prv, par, ch
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

Sub SwapSiblingSlices(byval sl1 as slice ptr, byval sl2 as slice ptr)
 'Only intended for use by siblings of the same parent
 if sl1 = 0 or sl2 = 0 then EXIT SUB ' Exit quietly when an arg is null. Valid use case for attempted swap at the beginning or end of a list
 if sl1 = sl2 then EXIT SUB ' Ignore attempts to swap a slice with itself
 if sl1->Parent <> sl2->Parent then debug "SwapSiblingSlices: slices are not siblings": EXIT SUB
 dim parent as slice ptr = sl1->Parent
 dim slice_list(parent->NumChildren - 1) as slice ptr
 dim temp_sl as slice ptr = parent->FirstChild
 dim i as integer
 'Convert the children into an unlinked list
 for i = 0 to ubound(slice_list)
  slice_list(i) = temp_sl
  temp_sl = temp_sl->NextSibling
  slice_list(i)->PrevSibling = 0
  slice_list(i)->NextSibling = 0
 next i
 'Swap the two siblings
 for i = 0 to ubound(slice_list)
  if slice_list(i) = sl1 then
   slice_list(i) = sl2
  elseif slice_list(i) = sl2 then
   slice_list(i) = sl1
  end if
 next i
 'Convert back to a doubly linked list
 parent->FirstChild = slice_list(0)
 for i = 1 to ubound(slice_list)
  slice_list(i - 1)->NextSibling = slice_list(i)
  slice_list(i)->PrevSibling = slice_list(i - 1)
 next i 
end sub

Sub InsertSiblingSlice(byval sl as slice ptr, byval newsl as slice ptr)
 'Intended for use when newsl is a newly created orphan such as a New*Slice result
 'FIXME: maybe this could probably use some more safety checks?
 if newsl->Parent <> 0 then debug "InsertSiblingSlice: Only inserts orphans": EXIT SUB
 if sl->Parent = 0 then debug "InsertSiblingSlice: Root shouldn't have siblings": EXIT SUB

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

Sub DrawRectangleSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as RectangleSliceData ptr = cptr(RectangleSliceData ptr, sl->SliceData)
 edgebox sl->screenx, sl->screeny, sl->width, sl->height, dat->bgcol , dat->fgcol, p, dat->transparent, NOT dat->border
end sub

Sub SaveRectangleSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 DIM dat AS RectangleSliceData Ptr
 dat = sl->SliceData
 WriteSliceFileVal f, "fg", dat->fgcol
 WriteSliceFileVal f, "bg", dat->bgcol
 WriteSliceFileBool f, "trans", dat->transparent
 WriteSliceFileBool f, "border", dat->border
End Sub

Function LoadRectangleSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 dim dat AS RectangleSliceData Ptr
 dat = sl->SliceData
 select case key
  case "fg": dat->fgcol = n
  case "bg": dat->bgcol = n
  case "trans": dat->transparent = n
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
 
 ret->SliceType = slRectangle
 ret->SliceData = d
 ret->Draw = @DrawRectangleSlice
 ret->Dispose = @DisposeRectangleSlice
 ret->Save = @SaveRectangleSlice
 ret->Load = @LoadRectangleSlice
 
 return ret
end function

Function GetRectangleSliceData(byval sl as slice ptr) as RectangleSliceData ptr
 return sl->SliceData
End Function

'--StyleRectangle---------------------------------------------------------
Sub DisposeStyleRectangleSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as StyleRectangleSliceData ptr = cptr(StyleRectangleSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub DrawStyleRectangleSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as StyleRectangleSliceData ptr = cptr(StyleRectangleSliceData ptr, sl->SliceData)
 edgeboxstyle sl->screenx, sl->screeny, sl->width, sl->height, dat->style , p, dat->transparent, dat->hideborder
end sub

Function GetStyleRectangleSliceData(byval sl as slice ptr) as StyleRectangleSliceData ptr
 return sl->SliceData
End Function

Sub SaveStyleRectangleSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 DIM dat AS StyleRectangleSliceData Ptr
 dat = sl->SliceData
 WriteSliceFileVal f, "style", dat->style
 WriteSliceFileBool f, "trans", dat->transparent
 WriteSliceFileBool f, "hideb", dat->hideborder
End Sub

Function LoadStyleRectangleSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 dim dat AS StyleRectangleSliceData Ptr
 dat = sl->SliceData
 select case key
  case "style": dat->style = n
  case "trans": dat->transparent = n
  case "hideb": dat->hideborder = n
  case else: return NO
 end select
 return YES
End Function

Function NewStyleRectangleSlice(byval parent as Slice ptr, byref dat as StyleRectangleSliceData) as slice ptr
 dim ret as Slice ptr
 ret = NewSlice(parent)
 if ret = 0 then 
  debug "Out of memory?!"
  return 0
 end if
 
 dim d as StyleRectangleSliceData ptr = new StyleRectangleSliceData
 *d = dat
 
 ret->SliceType = slStyleRectangle
 ret->SliceData = d
 ret->Draw = @DrawStyleRectangleSlice
 ret->Dispose = @DisposeStyleRectangleSlice
 ret->Save = @SaveStyleRectangleSlice
 ret->Load = @LoadStyleRectangleSlice
 
 return ret
end function

'--Text-------------------------------------------------------------------
Sub DisposeTextSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as TextSliceData ptr = cptr(TextSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

Sub DrawTextSlice(byval sl as slice ptr, byval p as integer)
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
 'this ugly hack is because printstr doesn't do new lines :@
 dim lines() as string
 split(d, lines())
 dim col as integer = dat->col
 if col = 0 then col = uilook(uiText)
 if dat->outline then 
  for i as integer = 0 to ubound(lines)
   edgeprint lines(i), sl->screenx, sl->screeny + i * 10, col, p
  next
 else
  textcolor col, 0
  for i as integer = 0 to ubound(lines)
   printstr lines(i), sl->screenx, sl->screeny + i * 10, p
  next
 end if
end sub

Sub UpdateTextSlice(byval sl as slice ptr)
 dim dat as TextSliceData ptr = cptr(TextSliceData ptr, sl->SliceData)
 
 if dat->Wrap = NO then
  sl->Width = textWidth(dat->s)
 else
  'split(wordwrap(dat->s, sl->Width / 8), dat->lines())
 end if
end sub

Function GetTextSliceData(byval sl as slice ptr) as TextSliceData ptr
 return sl->SliceData
End Function

Sub SaveTextSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 DIM dat AS TextSliceData Ptr
 dat = sl->SliceData
 WriteSliceFileVal f, "s", dat->s
 WriteSliceFileVal f, "col", dat->col
 WriteSliceFileBool f, "outline", dat->outline
 WriteSliceFileBool f, "wrap", dat->wrap
End Sub

Function LoadTextSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
 dim dat AS TextSliceData Ptr
 dat = sl->SliceData
 select case key
  case "s": dat->s = StripQuotes(valstr) : checkn = NO
  case "col": dat->col = n
  case "outline": dat->outline = n
  case "wrap": dat->wrap = n
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
   if .flipHoriz then
    .img.sprite = sprite_flip_horiz(.img.sprite, YES)
   end if
   if .flipVert then
    .img.sprite = sprite_flip_vert(.img.sprite, YES)
   end if
   .loaded = YES
  end if
 
  sprite_draw .img.sprite + .frame, .img.pal, sl->screenX, sl->screenY, , ,dpage
 end with
end sub

Function GetSpriteSliceData(byval sl as slice ptr) as SpriteSliceData ptr
 return sl->SliceData
End Function

Sub SaveSpriteSlice(byval sl as slice ptr, byref f as SliceFileWrite)
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
 if sl->SliceType <> slSprite then debug "Attempt to use non-sprite slice " & sl & " as a sprite" : exit sub
 dim dat as SpriteSliceData Ptr = sl->SliceData
 with *dat
  if spritetype >= 0 then
   .spritetype = spritetype
   .loaded = NO
  end if
  if record >= 0 then
   .record = record
   .loaded = NO
  end if
  if pal >= -1 then
   .pal = pal
   .loaded = NO
  end if
  if frame >= 0 then .frame = frame
  if fliph > -2 then .flipHoriz = (fliph <> 0)
  if flipv > -2 then .flipVert = (flipv <> 0)
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
 return sl->SliceData
End Function

Sub SaveMenuSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 DIM dat AS MenuSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "SaveMenuSlice not implemented"
end sub

Function LoadMenuSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
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
 dim dat as MenuItemSliceData ptr = cptr(MenuItemSliceData ptr, sl->SliceData)
 
 sl->Width = textWidth(dat->caption)
end sub

Function GetMenuItemSliceData(byval sl as slice ptr) as MenuItemSliceData ptr
 return sl->SliceData
End Function

Sub SaveMenuItemSlice(byval sl as slice ptr, byref f as SliceFileWrite)
 DIM dat AS MenuItemSliceData Ptr
 dat = sl->SliceData
 'FIXME: Implement me!
 debug "SaveMenuItemSlice not implemented"
end sub

Function LoadMenuItemSlice (Byval sl as SliceFwd ptr, key as string, valstr as string, byval n as integer, byref checkn as integer) as integer
 'Return value is YES if the key is understood, NO if ignored
 'set checkn=NO if you read a string. checkn defaults to YES which causes integer/boolean checking to happen afterwards
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

'==Epic prophecy of the construcinator=========================================
/'

AND SO THE PROPHECY WAS SPOKEN:

WHEN SO THE SOURCE IS COMPILED WITH -LANG FB, THEN THE LEGENDARY CONSTRUCTORS SHALL BE BORN
Constructor RectangleSliceData (byval bg as integer = -1, byval tr as integer = YES, byval fg as integer = -1, byval bor as integer = 0)
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
  .transparent = tr
 end with
End Constructor
'/

'==General slice display=======================================================

Function GetSliceDrawAttachParent(BYVAL sl AS Slice Ptr) AS Slice Ptr
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
 SELECT CASE sl->AlignHoriz
  CASE 0: RETURN alignTo->ScreenX + alignTo->paddingLeft
  CASE 1: RETURN alignTo->ScreenX + alignTo->paddingLeft + (alignTo->Width - alignTo->paddingLeft - alignTo->paddingRight) \ 2
  CASE 2: RETURN alignTo->ScreenX + alignTo->Width - alignTo->paddingRight
 END SELECT
End Function

Function SliceYAlign(BYVAL sl AS Slice Ptr, BYVAL alignTo AS Slice Ptr) AS INTEGER
 SELECT CASE sl->AlignVert
  CASE 0: RETURN alignTo->ScreenY + alignTo->paddingTop
  CASE 1: RETURN alignTo->ScreenY + alignTo->paddingTop + (alignTo->Height - alignTo->paddingTop - alignTo->paddingBottom) \ 2
  CASE 2: RETURN alignTo->ScreenY + alignTo->Height - alignTo->paddingBottom
 END SELECT
End Function

Function SliceXAnchor(BYVAL sl AS Slice Ptr) AS INTEGER
 SELECT CASE sl->AnchorHoriz
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Width \ 2
  CASE 2: RETURN sl->Width
 END SELECT
End Function

Function SliceYAnchor(BYVAL sl AS Slice Ptr) AS INTEGER
 SELECT CASE sl->AnchorVert
  CASE 0: RETURN 0
  CASE 1: RETURN sl->Height \ 2
  CASE 2: RETURN sl->Height
 END SELECT
End Function

Sub DrawSlice(byval s as slice ptr, byval page as integer)
 'first, draw this slice
 if s->Visible then
  'calc it's X,Y
  DIM attach AS Slice Ptr
  attach = GetSliceDrawAttachParent(s)
  with *s
   IF .Fill then
    .ScreenX = attach->ScreenX + attach->paddingLeft
    .ScreenY = attach->ScreenY + attach->paddingTop
    .Width = attach->Width - attach->paddingLeft - attach->paddingRight
    .height = attach->Height - attach->paddingTop - attach->paddingBottom
   ELSE ' Not fill
    .ScreenX = .X + SliceXAlign(s, attach) - SliceXAnchor(s)
    .ScreenY = .Y + SliceYAlign(s, attach) - SliceYAnchor(s)
   END IF
   
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

