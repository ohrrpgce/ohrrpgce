'OHRRPGCE GAME - Slice related functionality
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't very crappy
'
'$DYNAMIC

option explicit

#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"

#include "slices.bi"

DIM Slices(100) as Slice Ptr

Dim SliceTable as SliceTable_

'add other slice tables here

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

'Creates a new Slice object, and optionally, adds it to the heirarchy somewhere
Function NewSlice(Byval parent as Slice ptr = 0) as Slice Ptr
 dim ret as Slice Ptr
 ret = new Slice
 
 if parent then
  if parent->FirstChild = 0 then
   parent->FirstChild = ret
  else
   dim s as slice ptr
   s = parent->FirstChild
   do while s->NextSibling <> 0
    s = s->NextSibling
   loop
   s->NextSibling = ret
   ret->PrevSibling = s
  end if
  
  parent->NumChildren += 1
  ret->parent = parent
 end if
 
 ret->SliceType = slSpecial
 ret->Visible = YES
 
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

Sub DrawRectangleSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim rect as RectangleSliceData ptr = cptr(RectangleSliceData ptr, sl->SliceData)
 
 edgebox sl->x, sl->y, sl->width, sl->height, rect->bgcol , rect->fgcol, p, rect->transparent, rect->border
end sub

Sub DisposeRectangleSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim rect as RectangleSliceData ptr = cptr(RectangleSliceData ptr, sl->SliceData)
 delete rect
 sl->SliceData = 0
end sub

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
 
 return ret
end function

Sub DrawTextSlice(byval sl as slice ptr, byval p as integer)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 
 dim dat as TextSliceData ptr = cptr(TextSliceData ptr, sl->SliceData)
 dim d as string
 if dat->wrap then
  d = wordwrap(dat->s, int(sl->width / 8) - 1)
 else
  d = dat->s
 end if
 'this ugly hack is because printstr doesn't do new lines :@
 dim lines() as string
 split(d, lines())
 if dat->outline then 
  for i as integer = 0 to ubound(lines)
   edgeprint lines(i), sl->screenx, sl->screeny + i * 10, dat->col, p
  next
 else
  textcolor dat->col, 0
  for i as integer = 0 to ubound(lines)
   printstr lines(i), sl->screenx, sl->screeny + i * 10, p
  next
 end if
 exit sub
end sub

Sub DisposeTextSlice(byval sl as slice ptr)
 if sl = 0 then exit sub
 if sl->SliceData = 0 then exit sub
 dim dat as TextSliceData ptr = cptr(TextSliceData ptr, sl->SliceData)
 delete dat
 sl->SliceData = 0
end sub

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
 
 return ret
end function

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

Sub DrawSlice(byval s as slice ptr, byval page as integer)
 'first, draw this slice
 if s->Visible then
  'calc it's X,Y
  with *s
   IF .Fill then
    SELECT CASE .Attach
     CASE slParent
      .ScreenX = .X + .parent->ScreenX + .parent->PaddingLeft
      .ScreenY = .Y + .parent->ScreenY + .parent->PaddingTop
      .Width = .parent->Width - .parent->paddingleft - .parent->paddingRight
      .height = .parent->height - .parent->paddingtop - .parent->paddingbottom
     case slScreen
      .ScreenX = .X
      .ScreenY = .Y
      .Width = 320
      .height = 200
     case slSlice
      .ScreenX = .X + .Attached->ScreenX 
      .ScreenY = .Y + .Attached->ScreenY
      .Width = .Attached->Width - .Attached->paddingleft - .Attached->paddingRight
      .height = .Attached->height - .Attached->paddingtop - .Attached->paddingbottom
     case else '???
      .ScreenX = .X
      .ScreenY = .Y
      .Width = 320
      .height = 200
    END SELECT
   ELSE
    SELECT CASE .Attach
     CASE slParent
      .ScreenX = .X + .parent->ScreenX + .parent->PaddingLeft
      .ScreenY = .Y + .parent->ScreenY + .parent->PaddingTop
     case slScreen
      .ScreenX = .X
      .ScreenY = .Y
     case slSlice
      .ScreenX = .X + .Attached->ScreenX 
      .ScreenY = .Y + .Attached->ScreenY
     case else '???
      .ScreenX = .X
      .ScreenY = .Y
    END SELECT
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
