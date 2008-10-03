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

'==============================================================================

DIM Slices(100) as Slice Ptr

Dim SliceTable as SliceTable_

'add other slice tables here

'==General slice code==========================================================

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
 
 setSliceParent(ret, parent)
 
 ret->SliceType = slSpecial
 ret->Visible = YES
 ret->Attached = parent
 ret->Attach = slSlice
 
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
  nxt->NextSibling = nxt
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
 edgebox sl->screenx, sl->screeny, sl->width, sl->height, dat->bgcol , dat->fgcol, p, dat->transparent, dat->border
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




Function GetRectangleSliceData(byval sl as slice ptr) as RectangleSliceData ptr
 return sl->SliceData
End Function


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
 edgeboxstyle sl->screenx, sl->screeny, sl->width, sl->height, dat->style , p, dat->transparent, dat->border
end sub

Function GetStyleRectangleSliceData(byval sl as slice ptr) as StyleRectangleSliceData ptr
 return sl->SliceData
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
 
 return ret
end function

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
end sub

Function GetTextSliceData(byval sl as slice ptr) as TextSliceData ptr
 return sl->SliceData
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
 
 return ret
end function

'==START OF MenuSLICEDATA
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
 
 return ret
end function
'==END OF MenuSLICEDATA

'==START OF MENUITEMSLICEDATA
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

Function GetMenuItemSliceData(byval sl as slice ptr) as MenuItemSliceData ptr
 return sl->SliceData
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
 
 return ret
end function
'==END OF MENUITEMSLICEDATA


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

Sub DrawSlice(byval s as slice ptr, byval page as integer)
 'first, draw this slice
 if s->Visible then
  'calc it's X,Y
  with *s
   IF .Fill then
    SELECT CASE .Attach
     case slScreen
      .ScreenX = 0
      .ScreenY = 0
      .Width = 320
      .height = 200
     case slSlice
      if .Attached then
       .ScreenX = .Attached->ScreenX + .Attached->paddingleft
       .ScreenY = .Attached->ScreenY + .Attached->paddingtop
       .Width = .Attached->Width - .Attached->paddingleft - .Attached->paddingRight
       .height = .Attached->height - .Attached->paddingtop - .Attached->paddingbottom
      elseif .parent then
       .Attached = .parent
       .ScreenX = .Parent->ScreenX + .Parent->paddingleft
       .ScreenY = .Parent->ScreenY + .Parent->paddingtop
       .Width = .Parent->Width - .Parent->paddingleft - .Parent->paddingRight
       .height = .Parent->height - .Parent->paddingtop - .Parent->paddingbottom
      else
      .ScreenX = 0
      .ScreenY = 0
      .Width = 320
      .height = 200
      end if
    END SELECT
   ELSE
    SELECT CASE .Attach
     case slScreen
      .ScreenX = .X
      .ScreenY = .Y
     case slSlice
      if .Attached then
       .ScreenX = .X + .Attached->ScreenX + .Attached->paddingleft
       .ScreenY = .Y + .Attached->ScreenY + .Attached->paddingtop
      elseif .parent then
       .Attached = .parent
       .ScreenX = .X + .parent->ScreenX + .Parent->paddingleft
       .ScreenY = .Y + .parent->ScreenY + .Parent->paddingtop
      else
       .ScreenX = .X
       .ScreenY = .Y
      end if
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
