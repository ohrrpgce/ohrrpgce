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

Function ThingBrowser.browse(byref start_id as integer=0, byval or_none as bool=NO, editor_func as FnThingBrowserEditor=0) as integer
 dim result as integer = start_id
 this.or_none = or_none
 
 helpkey = init_helpkey()

 dim holdscreen as integer = allocatepage
 copypage vpage, holdscreen

 root = NewSliceOfType(slContainer)
 SliceLoadFromFile root, finddatafile("thingbrowser.slice")

 enter_browser

 dim mode_indicator as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_MODE_INDICATOR, root)
 ChangeTextSlice mode_indicator, "Browsing " & thing_kind_name()
 if editor_func <> 0 then ChangeTextSlice mode_indicator, "Editing " & thing_kind_name()

 dim back_holder as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_BACK_HOLDER, root)

 dim grid as Slice Ptr
 grid = LookupSlice(SL_EDITOR_THINGBROWSER_GRID, root) 
 RefreshSliceScreenPos grid
 build_thing_list()
 
 dim ps as PlankState
 ps.m = root
 ps.cur = top_left_plank(ps)
 dim orig_cur as slice ptr = 0
 if focus_plank_by_extra_id(ps, start_id, grid) then
  orig_cur = ps.cur
 end if
 DrawSlice root, vpage
 update_plank_scrolling ps

 dim hover as Slice Ptr = 0
 dim cursor_moved as bool = YES
 
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
  
  if plank_menu_arrows(ps, grid) then
   'Give priority to the grid
   cursor_moved = YES
  elseif plank_menu_arrows(ps) then
   'Only if no movement happened in the grid do we consider outside the grid
   cursor_moved = YES
  end if
  plank_menu_mouse_wheel(ps)
  hover = find_plank_at_screen_pos(ps, readmouse.pos)
  if hover andalso (readmouse.clicks AND mouseLeft) then
   cursor_moved = ps.cur <> hover
   ps.cur = hover
  end if
  if readmouse.buttons AND mouseRight then
   'Holding down right click can change cursor selection
   cursor_moved = ps.cur <> hover
   ps.cur = hover
  end if

  if enter_or_space() orelse ((readmouse.release AND mouseLeft) andalso hover=ps.cur) then
   if IsAncestor(ps.cur, grid) then
    if editor_func = 0 then
     'Selected a thing
     result = ps.cur->Extra(0)
     exit do
    else
     'Editing a thing
     dim editor as FnThingBrowserEditor = editor_func
     editor(ps.cur->Extra(0))
     save_plank_selection ps
     build_thing_list()
     restore_plank_selection ps
     hover = 0
     orig_cur = find_plank_by_extra_id(ps, start_id, grid)
    end if
   elseif IsAncestor(ps.cur, back_holder) then
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

  'Iterate over all the planks to run their each_tick sub  
  REDIM planks(any) as Slice Ptr
  find_all_planks ps, ps.m, planks()
  for i as integer = 0 to ubound(planks)
   each_tick_each_plank planks(i)
  next i
  'Then run the each-tick sub for the selected plank
  if ps.cur then each_tick_selected_plank ps.cur

  ChangeGridSlice grid, , grid->Width \ plank_size.x
  if cursor_moved then
   'Yep, a move happened. We would update selection detail display here if that was a thing
   update_plank_scrolling ps
  end if
  cursor_moved = NO

  copypage holdscreen, vpage
  DrawSlice root, vpage
  setvispage vpage
  dowait
 loop
 leave_browser
 setkeys
 freepage holdscreen
 DeleteSlice @(root)
 return result
End Function

Sub ThingBrowser.enter_browser()
 'Special initialisation
End Sub

Sub ThingBrowser.leave_browser()
 'Special cleanup
End Sub

Sub ThingBrowser.each_tick_each_plank(byval plank as Slice Ptr)
 'Nothing needs to happen here, if you don't want continous animation
End Sub

Sub ThingBrowser.each_tick_selected_plank(byval plank as Slice Ptr)
 'Nothing needs to happen here, if you don't want extra selection cursor animation
 '(the SL_PLANK_MENU_SELECTABLE animation of TextSlice and RectangleSlice color happens automatically even without this sub)
End Sub

Sub ThingBrowser.loop_sprite_helper(byval plank as Slice Ptr, byval min as integer, byval max as integer, byval delay as integer=1)
 'A crude and simple animation helper for sprites in planks.
 'Uses the Extra(1) slot to manage the animation speed.
 'FIXME: rip this all out and replace it when the new animation system is ready
 dim spr as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 if spr then
  spr->Extra(1) = loopvar(spr->Extra(1), 0, delay)
  if spr->Extra(1) = 0 then
   dim dat as SpriteSliceData Ptr = spr->SliceData
   dat->frame = loopvar(dat->frame, min, max)
  end if
 end if
End Sub

Function ThingBrowser.thing_kind_name() as string
 'Should be plural
 return "Things"
End Function

Function ThingBrowser.init_helpkey() as string
 return ""
End Function

Sub ThingBrowser.build_thing_list()
 plank_menu_clear root, SL_EDITOR_THINGBROWSER_GRID
 dim grid as slice ptr
 grid = LookupSlice(SL_EDITOR_THINGBROWSER_GRID, root)
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
 ChangeGridSlice grid, , grid->Width \ plank_size.x
 DrawSlice root, vpage 'refresh screen positions
End Sub

Function ThingBrowser.lowest_id() as integer
 if or_none then return -1
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
 ChangeRectangleSlice box, , , , borderNone
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

Function ItemBrowser.thing_kind_name() as string
 return "Items"
End Function

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

Function ShopBrowser.thing_kind_name() as string
 return "Shops"
End Function

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

Function ConstantListBrowser.thing_kind_name() as string
 return "Values"
End Function

Sub ConstantListBrowser.enter_browser()
 for i as integer = lbound(list) to ubound(list)
  longest = large(longest, len(list(i)))
 next i
End Sub

Function ConstantListBrowser.lowest_id() as integer
 if or_none then return lbound(list) - 1
 return lbound(list)
End Function

Function ConstantListBrowser.highest_id() as integer
 return ubound(list)
End Function

Function ConstantListBrowser.thing_text_for_id(byval id as integer) as string
 dim text as string
 if id >= lbound(list) andalso id <= ubound(list) then
  text = list(id)
 else
  text = "NOVALUE"
 end if
 return rpad(text, " ", longest)
End Function

'-----------------------------------------------------------------------

Constructor ArrayBrowser(array() as string, thing_name as string)
 set_list array()
 thing_name_override = thing_name
End Constructor

Function ArrayBrowser.thing_kind_name() as string
 if thing_name_override <> "" then return thing_name_override
 return "Values"
End Function

Sub ArrayBrowser.set_list(array() as string)
 str_array_copy array(), list()
End Sub

'-----------------------------------------------------------------------

Sub FlexmenuCaptionBrowser.set_list_from_flexmenu(caption() as string, byval caption_code as integer, byval min as integer, byval max as integer)
 dim capindex as integer
 dim show_id as bool = NO
 select case caption_code
  case 1000 to 1999 ' caption with id at the beginning
   capindex = caption_code - 1000
   show_id = YES
  case 2000 to 2999 ' caption only
   capindex = caption_code - 2000
  case else
   visible_debug "set_list_from_flexmenu: caption_code " & caption_code & " is not in the expected range of 1000 to 2999"
   exit sub
 end select
 redim list(min to max) as string
 for i as integer = min to max
  list(i) = caption(capindex + i)
 next i
End Sub

'-----------------------------------------------------------------------

Function SpriteBrowser.thing_kind_name() as string
 return sprite_sizes(sprite_kind()).name & " Sprites"
End Function

Function SpriteBrowser.sprite_kind() as integer
 'This should be overridden by a child class
 return sprTypeInvalid
End Function

Function SpriteBrowser.sprite_frame() as integer
 return 0
End Function

Function SpriteBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim plank as Slice Ptr
 plank = NewSliceOfType(slContainer, , SL_PLANK_HOLDER) ' SL_PLANK_HOLDER will be re-applied by the caller
 dim box as Slice Ptr
 box = NewSliceOfType(slRectangle, plank, SL_PLANK_MENU_SELECTABLE)
 box->Fill = YES
 box->Visible = NO
 ChangeRectangleSlice box, , , , borderNone
 dim spr as Slice Ptr
 spr = NewSliceOfType(slSprite, plank, SL_EDITOR_THINGBROWSER_PLANK_SPRITE)
 ChangeSpriteSlice spr, sprite_kind(), id, , sprite_frame()
 dim txt as Slice Ptr
 txt = NewSliceOfType(slText, plank, SL_PLANK_MENU_SELECTABLE)
 txt->AlignVert = alignBottom
 txt->AnchorVert = alignBottom
 ChangeTextSlice txt, thing_text_for_id(id), uilook(uiMenuItem), YES
 if id = -1 then ChangeTextSlice txt, "NONE"
 plank->size = spr->size
 return plank
End Function

'-----------------------------------------------------------------------

'HERO
Function HeroSpriteBrowser.highest_id() as integer
 return gen(genMaxHeroPic)
End Function

Function HeroSpriteBrowser.sprite_kind() as integer
 return sprTypeHero
End Function

Sub HeroSpriteBrowser.each_tick_selected_plank(byval plank as Slice Ptr)
 loop_sprite_helper plank, 0, 1
End Sub

'WALKABOUT
Function WalkaboutSpriteBrowser.highest_id() as integer
 return gen(genMaxNPCPic)
End Function

Function WalkaboutSpriteBrowser.sprite_kind() as integer
 return sprTypeWalkabout
End Function

Function WalkaboutSpriteBrowser.sprite_frame() as integer
 return 4
End Function

Sub WalkaboutSpriteBrowser.each_tick_selected_plank(byval plank as Slice Ptr)
 loop_sprite_helper plank, 4, 5
End Sub

'PORTRAIT
Function PortraitSpriteBrowser.highest_id() as integer
 return gen(genMaxPortrait)
End Function

Function PortraitSpriteBrowser.sprite_kind() as integer
 return sprTypePortrait
End Function

'ENEMY
Function EnemySpriteBrowser.highest_id() as integer
 select case size_group
  case 0: return gen(genMaxEnemy1Pic)
  case 1: return gen(genMaxEnemy2Pic)
  case 2: return gen(genMaxEnemy3Pic)
 end select
 debug "EnemySpriteBrowser.highest_id(): size_group " & size_group & " is not valid"
 return 0
End Function

Function EnemySpriteBrowser.sprite_kind() as integer
 select case size_group
  case 0: return sprTypeSmallEnemy
  case 1: return sprTypeMediumEnemy
  case 2: return sprTypeLargeEnemy
 end select
 debug "EnemySpriteBrowser.sprite_kind: size_group " & size_group & " is not valid"
 return sprTypeInvalid
End Function

'ATTACK
Function AttackSpriteBrowser.highest_id() as integer
 return gen(genMaxAttackPic)
End Function

Function AttackSpriteBrowser.sprite_kind() as integer
 return sprTypeAttack
End Function

Sub AttackSpriteBrowser.each_tick_each_plank(byval plank as Slice Ptr)
 loop_sprite_helper plank, 0, 2
End Sub

'WEAPON
Function WeaponSpriteBrowser.highest_id() as integer
 return gen(genMaxWeaponPic)
End Function

Function WeaponSpriteBrowser.sprite_kind() as integer
 return sprTypeWeapon
End Function

Sub WeaponSpriteBrowser.each_tick_selected_plank(byval plank as Slice Ptr)
 loop_sprite_helper plank, 0, 1
End Sub

'BACKDROP
Sub BackdropSpriteBrowser.enter_browser()
 switch_to_32bit_vpages
End Sub

Sub BackdropSpriteBrowser.leave_browser()
 switch_to_8bit_vpages
End Sub

Function BackdropSpriteBrowser.highest_id() as integer
 return gen(genNumBackdrops) - 1
End Function

Function BackdropSpriteBrowser.sprite_kind() as integer
 return sprTypeBackdrop
End Function

Function BackdropSpriteBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim plank as Slice Ptr
 plank = Base.create_thing_plank(id)
 plank->size = XY(98, 62)
 dim spr as Slice Ptr
 spr = LookupSlice(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 if id = -1 then
  DeleteSlice @spr
 end if
 if spr then
  spr->AlignVert = alignBottom
  spr->AlignHoriz = AlignCenter
  spr->AnchorVert = alignBottom
  spr->AnchorHoriz = alignCenter
  spr->y = -1
  ScaleSpriteSlice spr, plank->size - XY(2,2)
 end if
 return plank
End Function

'BOX BORDER
Function BoxborderSpriteBrowser.highest_id() as integer
 return gen(genMaxBoxBorder)
End Function

Function BoxborderSpriteBrowser.sprite_kind() as integer
 return sprTypeBoxBorder
End Function

Function BoxborderSpriteBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim plank as Slice Ptr
 plank = NewSliceOfType(slContainer, , SL_PLANK_HOLDER) ' SL_PLANK_HOLDER will be re-applied by the caller
 dim box as Slice Ptr
 box = NewSliceOfType(slRectangle, plank, SL_PLANK_MENU_SELECTABLE)
 box->Fill = YES
 box->Visible = NO
 ChangeRectangleSlice box, , , , borderNone
 if id >= 0 then
  dim box2 as Slice Ptr
  box2 = NewSliceOfType(slRectangle, plank)
  ChangeRectangleSlice box2, , , , , , , id
  box2->AlignVert = alignCenter
  box2->AnchorVert = alignCenter
  box2->AlignHoriz = alignCenter
  box2->AnchorHoriz = alignCenter
  box2->size = XY(34, 34)
 end if
 dim txt as Slice Ptr
 txt = NewSliceOfType(slText, plank, SL_PLANK_MENU_SELECTABLE)
 txt->AlignVert = alignCenter
 txt->AnchorVert = alignCenter
 txt->AlignHoriz = alignCenter
 txt->AnchorHoriz = alignCenter
 ChangeTextSlice txt, thing_text_for_id(id), uilook(uiMenuItem), YES
 if id = -1 then ChangeTextSlice txt, "NONE"
 plank->size = XY(50, 50)
 return plank
End Function


'-----------------------------------------------------------------------

Function SpriteOfTypeBrowser.browse(byref start_id as integer=0, byval or_none as bool=NO, byval spr_type as spriteType) as integer
 select case spr_type
  case sprTypeHero
   dim br as HeroSpriteBrowser
   return br.browse(start_id, or_none)
  case sprTypeSmallEnemy
   dim br as EnemySpriteBrowser
   br.size_group = 0
   return br.browse(start_id, or_none)
  case sprTypeMediumEnemy
   dim br as EnemySpriteBrowser
   br.size_group = 1
   return br.browse(start_id, or_none)
  case sprTypeLargeEnemy
   dim br as EnemySpriteBrowser
   br.size_group = 2
   return br.browse(start_id, or_none)
  case sprTypeWalkabout
   dim br as WalkaboutSpriteBrowser
   return br.browse(start_id, or_none)
  case sprTypeWeapon
   dim br as WeaponSpriteBrowser
   return br.browse(start_id, or_none)
  case sprTypeAttack
   dim br as AttackSpriteBrowser
   return br.browse(start_id, or_none)
  case sprTypePortrait
   dim br as PortraitSpriteBrowser
   return br.browse(start_id, or_none)
  case sprTypeBackdrop
   dim br as BackdropSpriteBrowser
   return br.browse(start_id, or_none)
  case sprTypeBoxBorder
   dim br as BoxborderSpriteBrowser
   return br.browse(start_id, or_none)
  case else
   visible_debug "No sprite browser available for sprite type " & spr_type
 end select
 return start_id
End Function

'-----------------------------------------------------------------------
