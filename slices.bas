'OHRRPGCE GAME - Slice related functionality
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't very crappy
'
'$DYNAMIC


#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"

#include "slices.bi"

DIM Slices(100) as Slice Ptr
DIM AS Slice Ptr RootSlice, MapSlice, ScriptSpriteSlice, TextboxSlice, MenuSlice, ScriptStringSlice
'add other slice tables here

Sub SetupGameSlices
 RootSlice = NewSlice
 
 MapSlice = NewSlice(RootSlice)
 ScriptSpriteSlice = NewSlice(RootSlice)
 TextBoxSlice = NewSlice(RootSlice)
 MenuSlice = NewSlice(RootSlice)
 ScriptStringSlice = NewSlice(RootSlice)

 'if RootSlice->FirstChild = MapSlice AND _
 '   RootSlice->FirstChild->NextSibling = ScriptSpriteSlice AND _
 '   RootSlice->FirstChild->NextSibling->NextSibling = TextBoxSlice AND _
 '   RootSlice->FirstChild->NextSibling->NextSibling->NextSibling = MenuSlice AND _
 '   RootSlice->FirstChild->NextSibling->NextSibling->NextSibling->NextSibling = ScriptStringSlice THEN
 ' debug "Sanity checks, passed!"
 'end if
End Sub

Sub DestroyGameSlices
 if RootSlice then
  DeleteSlice(@MapSlice)
  DeleteSlice(@ScriptSpriteSlice)
  DeleteSlice(@TextBoxSlice)
  DeleteSlice(@MenuSlice)
  DeleteSlice(@ScriptStringSlice)
 
  DeleteSlice(@RootSlice)
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
  
 end if
 
 return ret
End Function

'Deletes a slice, and any children (and their children (and their...))
Sub DeleteSlice(Byval s as Slice ptr ptr)
 if s = 0 then exit sub  'can't do anything
 if *s = 0 then exit sub 'already freed
 
 dim sl as slice ptr = *s
 
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
