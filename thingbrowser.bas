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
#include "scriptcommands.bi"
#include "plankmenu.bi"

#include "thingbrowser.bi"

'-----------------------------------------------------------------------

Function ThingBrowser.browse(byref start_id as integer=0) as integer
 dim result as integer = start_id
 
 helpkey = init_helpkey()

 dim holdscreen as integer = allocatepage
 copypage vpage, holdscreen

 root = NewSliceOfType(slContainer)
 SliceLoadFromFile root, finddatafile("thingbrowser.slice")

 dim grid as Slice Ptr
 grid = LookupSlice(SL_THINGBROWSER_GRID, root) 
 RefreshSliceScreenPos grid
 build_thing_list()
 DrawSlice root, vpage
 
 dim ps as PlankState
 ps.m = root
 ps.cur = top_left_plank(ps)
 dim orig_cur as slice ptr = 0
 if focus_plank_by_extra_id(ps, start_id, grid) then
  orig_cur = ps.cur
 end if

 dim hover as Slice Ptr = 0
 
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

  'Clear selection indicators
  if ps.cur then set_plank_state ps, ps.cur, plankNORMAL
  if hover then set_plank_state ps, hover, plankNORMAL
  if orig_cur then set_plank_state ps, orig_cur, plankNORMAL
  
  dim moved as bool = NO
  if plank_menu_arrows(ps, grid) then
   'Give priority to the grid
   moved = YES
  elseif plank_menu_arrows(ps) then
   'Only if no movement happened in the grid do we consider outside the grid
   moved = YES
  end if
  IF plank_menu_mouse_wheel(ps) then
   moved = YES
  END IF
  hover = find_plank_at_screen_pos(ps, readmouse.pos)
  if hover andalso (readmouse.clicks AND mouseLeft) then
   moved = ps.cur <> hover
   ps.cur = hover
  end if

  if moved then
   'Yep, a move happened. We would update selection detail display here if that was a thing
  end if

  if enter_or_space() orelse ((readmouse.release AND mouseLeft) andalso hover=ps.cur) then
   if IsAncestor(ps.cur, grid) then
    'Selected a thing from the grid
    result = ps.cur->Extra(0)
    exit do
   else
    'Cancel out of the browser
    result = start_id
    exit do
   end if
  end if

  'Set selection indicators
  if orig_cur then set_plank_state ps, orig_cur, plankSPECIAL
  if hover then set_plank_state ps, hover, plankMOUSEHOVER
  if ps.cur then
   set_plank_state ps, ps.cur, plankSEL
   if ps.cur = orig_cur then set_plank_state ps, ps.cur, plankSELSPECIAL
  end if

  ChangeGridSlice grid, , grid->Width / plank_size.x
  update_plank_scrolling ps

  copypage holdscreen, vpage
  DrawSlice root, vpage
  setvispage vpage
  dowait
 loop
 setkeys
 freepage holdscreen
 DeleteSlice @(root)
 return result
End Function

Function ThingBrowser.init_helpkey() as string
 return ""
End Function

Sub ThingBrowser.build_thing_list()
 dim grid as slice ptr
 grid = LookupSlice(SL_THINGBROWSER_GRID, root)
 dim plank as slice ptr
 for id as integer = lowest_id() to highest_id()
  plank = create_thing_plank(id)
  SetSliceParent(plank, grid)
  plank->Lookup = SL_PLANK_HOLDER
  plank->Extra(0) = id
  plank_size.x = large(plank_size.x, plank->Width)
  plank_size.y = large(plank_size.y, plank->Height)
  grid->Height = plank_size.y
 next id
End Sub

Function ThingBrowser.lowest_id() as integer
 return 0
End Function

Function ThingBrowser.highest_id() as integer
 return -1
End Function

Function ThingBrowser.create_thing_plank(byval id as integer) as Slice Ptr
 dim plank as Slice Ptr
 plank = NewSliceOfType(slContainer, , SL_PLANK_HOLDER) ' SL_PLANK_HOLDER will be re-applied by the caller
 dim box as Slice Ptr
 box = NewSliceOfType(slRectangle, plank, SL_PLANK_MENU_SELECTABLE)
 box->Fill = YES
 box->Visible = NO
 ChangeRectangleSlice box, , , , -2
 dim txt as Slice Ptr
 txt = NewSliceOfType(slText, plank, SL_PLANK_MENU_SELECTABLE)
 ChangeTextSlice txt, thing_text_for_id(id), uilook(uiMenuItem), YES
 plank->size = txt->size + XY(2, 0) ' Plank is 2 pixels wider than the text
 return plank
End Function

Function ThingBrowser.thing_text_for_id(byval id as integer) as string
 return str(id)
End Function

'-----------------------------------------------------------------------

Function ItemBrowser.init_helpkey() as string
 return "item_browser"
End Function

Function ItemBrowser.highest_id() as integer
 return gen(genMaxItem)
End Function

Function ItemBrowser.thing_text_for_id(byval id as integer) as string
 dim digits as integer = len(str(highest_id()))
 if id = -1 then
  return lpad("", " ", digits) & " " & rpad("NO ITEM", " ", 8)
 end if
 return lpad(str(id), " ", digits) & " " & rpad(readitemname(id), " ", 8)
End Function

'-----------------------------------------------------------------------

Function ItemBrowserOrNone.lowest_id() as integer
 return -1 ' 0 is the lowest item id, but -1 represents "NO ITEM"
End Function

'-----------------------------------------------------------------------

Function ShopBrowser.init_helpkey() as string
 return "shop_browser"
End Function

Function ShopBrowser.highest_id() as integer
 return gen(genMaxShop)
End Function

Function ShopBrowser.thing_text_for_id(byval id as integer) as string
 dim digits as integer = len(str(highest_id()))
 if id = -1 then
  return lpad("", " ", digits) & " " & rpad("NO SHOP", " ", 16)
 end if
 return lpad(str(id), " ", digits) & " " & rpad(readshopname(id), " ", 16)
End Function

'-----------------------------------------------------------------------

Function ShopBrowserOrNone.lowest_id() as integer
 return -1 ' 0 is the lowest item id, but -1 represents "NO ITEM"
End Function

'-----------------------------------------------------------------------
