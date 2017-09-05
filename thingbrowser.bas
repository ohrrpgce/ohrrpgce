'OHRRPGCE GAME & CUSTOM - Classes for browsing various kinds of things ant getting an ID number
'(C) Copyright 2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't especially crappy
'
#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "const.bi"
#include "uiconst.bi"
#include "reloadext.bi"
#include "slices.bi"
#include "sliceedit.bi"

#include "thingbrowser.bi"

'-----------------------------------------------------------------------

Function ThingBrowser.browse(byref start_id as integer=0) as integer
 dim result as integer = start_id

 dim holdscreen as integer = allocatepage
 copypage vpage, holdscreen

 root = NewSliceOfType(slRoot)
 with *root
  .Fill = YES
  .paddingLeft = 8
  .paddingRight = 8
  .paddingTop = 8
  .paddingBottom = 8
 end with
 
 dim box as Slice ptr
 box = NewSliceOfType(slRectangle, root)
 with *box
  .Fill = YES
  .paddingLeft = 4
  .paddingRight = 4
  .paddingTop = 4
  .paddingBottom = 4
 end with
 ChangeRectangleSlice box, 1

 dim scroll as Slice Ptr
 scroll = NewSliceofType(slScroll, box)
 scroll->Fill = YES

 dim grid as Slice Ptr
 grid = NewSliceOfType(slGrid, scroll)
 WITH *grid
  .Fill = YES
  .FillMode = sliceFillHoriz
  .Lookup = SL_PLANK_HOLDER
 END WITH

 RefreshSliceScreenPos grid

 build_thing_list()
 do
  setwait 55
  setkeys YES
  if keyval(scEsc) > 1 then
   'cancel out of the browser
   result = start_id
   exit do
  end if
  if keyval(scF6) > 1 then slice_editor(root)
  if len(helpkey) andalso keyval(scF1) > 1 then show_help helpkey

  copypage holdscreen, vpage
  DrawSlice root, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 freepage holdscreen
 return result
End Function

Function ThingBrowser.init_helpkey() as string
 return ""
End Function

Sub ThingBrowser.build_thing_list()
 dim grid as slice ptr
 grid = LookupSlice(SL_PLANK_HOLDER, root)
 dim plank as slice ptr
 for id as integer = lowest_id() to highest_id()
  plank = create_thing_plank(id)
  SetSliceParent(plank, grid)
  plank->Lookup = SL_PLANK_MENU_SELECTABLE
  plank_size.x = large(plank_size.x, plank->Width)
  plank_size.y = large(plank_size.y, plank->Height)
  grid->Height = plank_size.y
  debug id & " " & grid->Width / plank_size.x
  ChangeGridSlice grid, , grid->Width / plank_size.x
 next id
End Sub

Function ThingBrowser.lowest_id() as integer
 return 0
End Function

Function ThingBrowser.highest_id() as integer
 return -1
End Function

Function ThingBrowser.create_thing_plank(byval id as integer) as Slice Ptr
 dim plank as Slice ptr
 plank = NewSliceOfType(slContainer)
 plank->size = XY(32, 10)
 dim txt as Slice ptr
 txt = NewSliceOfType(slText, plank)
 ChangeTextSlice txt, str(id), uilook(uiMenuItem), YES
 return plank
End Function

'-----------------------------------------------------------------------

Function ItemBrowser.init_helpkey() as string
 return "item_browser"
End Function

Function ItemBrowser.highest_id() as integer
 return gen(genMaxItem)
End Function

Function ItemBrowser.create_thing_plank(byval id as integer) as Slice Ptr
 dim plank as Slice ptr
 plank = NewSliceOfType(slContainer)
 plank->size = XY(90, 10)
 dim txt as Slice ptr
 txt = NewSliceOfType(slText, plank)
 ChangeTextSlice txt, id & " " & readitemname(id), uilook(uiMenuItem), YES
 return plank
End Function
