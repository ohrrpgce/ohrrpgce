'OHRRPGCE GAME & CUSTOM - Classes for browsing various kinds of things and getting an ID number
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
#include "loading.bi"
#include "bcommon.bi"

#ifdef IS_CUSTOM
#include "custom.bi"
#endif

#include "thingbrowser.bi"

'-----------------------------------------------------------------------

Function ThingBrowser.browse(byref start_id as integer=0, byval or_none as bool=NO, editor_func as FnThingBrowserEditor=0, byval edit_by_default as bool=YES, byval skip_zero as bool=NO) as integer
 dim result as integer = start_id
 this.or_none = or_none
 this.skip_zero = skip_zero
 
 dim holdscreen as integer = allocatepage
 copypage vpage, holdscreen

 root = NewSliceOfType(slContainer)
 SliceLoadFromFile root, finddatafile("thingbrowser.slice")
 
 can_edit = (editor_func <> 0)
 helpkey = init_helpkey() 'Do this after we know if editing is available

 set_up_sub_buttons
 enter_browser

 dim mode_indicator as Slice Ptr = LookupSliceSafe(SL_EDITOR_THINGBROWSER_MODE_INDICATOR, root)
 ChangeTextSlice mode_indicator, "Browsing " & thing_kind_name()
 if can_edit andalso edit_by_default then ChangeTextSlice mode_indicator, "Editing " & thing_kind_name()

 dim noscroll_area as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_NOSCROLL_AREA, root)
 dim back_holder as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_BACK_HOLDER, root)
 dim new_holder as Slice Ptr = LookupSliceSafe(SL_EDITOR_THINGBROWSER_NEW_HOLDER, root)
 if not can_edit then new_holder->Visible = NO
 dim filter_holder as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_FILTER_HOLDER, root)
 dim type_query_sl as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_TYPE_QUERY, root)
 dim filter_text_sl as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_FILTER_TEXT, root)

 thinglist = LookupSlice(SL_EDITOR_THINGBROWSER_THINGLIST, root)
 RefreshSliceScreenPos thinglist
 build_thing_list()

 dim ps as PlankState
 ps.m = root
 ps.cur = top_left_plank(ps)
 dim orig_cur as slice ptr = 0
 if focus_plank_by_extra_id(ps, , start_id, thinglist) then
  orig_cur = ps.cur
 end if
 DrawSlice root, vpage
 update_plank_scrolling ps

 dim hover as Slice Ptr = 0
 dim cursor_moved as bool = YES

 dim selectst as SelectTypeState
 
 dim do_add as bool = NO
 dim do_edit as bool = NO
 dim do_filter as bool = NO
 dim quit_if_add_cancelled as bool = NO
 
 'If start_id is > highest id then .browse should try to add a new item, and return -1 if cancelled
 if start_id > highest_id() then
  do_add = YES
  quit_if_add_cancelled = NO
 end if
 
 do
  setwait 55
  setkeys YES

  if keyval(ccCancel) > 1 then
   'cancel out of the browser
   result = start_id
   exit do
  end if
  if keyval(scF6) > 1 then slice_editor(root, SL_COLLECT_EDITOR, "thingbrowser.slice", , YES)
  if keyval(scCtrl) > 0 andalso keyval(scF) > 1 then do_filter = YES
  if len(helpkey) andalso keyval(scF1) > 1 then show_help helpkey

  'Clear selection indicators
  if ps.cur then set_plank_state ps, ps.cur, plankNORMAL
  if hover then set_plank_state ps, hover, plankNORMAL
  if orig_cur then set_plank_state ps, orig_cur, plankNORMAL
  
  if IsAncestor(ps.cur, thinglist) then
   'Things that only happen when the selection is in the thinglist
   if keyval(scHome) > 1 then
    'special handling of the home button
    if ps.cur->PrevSibling = 0 then
     'already on the first
     plank_menu_home(ps)
     cursor_moved = YES
    else
     'select the first
     dim first_thing as Slice Ptr
     first_thing = find_plank_by_extra_id(ps, , lowest_id(), thinglist)
     if first_thing then
      ps.cur = first_thing
      cursor_moved = YES
     end if
    end if
   elseif plank_menu_arrows(ps, thinglist) then
    'Give priority to the thinglist
    cursor_moved = YES
   end if
   if not cursor_moved andalso cropafter_keycombo() then
    handle_cropafter(ps.cur->Extra(0))
    save_plank_selection ps
    build_thing_list()
    restore_plank_selection ps
    hover = 0
    orig_cur = find_plank_by_extra_id(ps, , start_id, thinglist)
    cursor_moved = YES
   end if
  elseif IsAncestor(ps.cur, noscroll_area) then
   dim start_from as Slice Ptr = noscroll_area
   if keyval(scPageUp) > 1 orelse keyval(scPageDown) > 1 then start_from = thinglist
   if plank_menu_arrows(ps, start_from) then
    cursor_moved = YES
   end if
  end if
  if not cursor_moved then
   if plank_menu_arrows(ps) then
   'Only if no movement happened in one of the areas do we consider global movement
    cursor_moved = YES
   end if
  end if
  plank_menu_mouse_wheel(ps)
  if select_by_typing(selectst) then
   if selectst.query <> " " then
    if plank_select_by_string(ps, selectst.query) then cursor_moved = YES
   end if
  end if
  hover = find_plank_at_screen_pos(ps, readmouse.pos)
  if hover andalso (readmouse.clicks AND mouseLeft) then
   'FIXME: clicking on the Play button of a music/sfx plank results in the song/sfx getting
   'played twice, once for the button-down moving the selection, and a second time for
   'the button-release activating the play button
   cursor_moved = ps.cur <> hover
   ps.cur = hover
  end if
  if hover andalso (readmouse.buttons AND mouseRight) then
   'Holding down right click changes cursor selection
   cursor_moved = ps.cur <> hover
   ps.cur = hover
  end if

  dim edit_record as integer

  if readmouse.release AND mouseRight then
   'When edit mode is available, but not default, then
   ' right-clicking pops up a context menu
   if ps.cur andalso can_edit andalso not edit_by_default then
    dim selected_id as integer = ps.cur->Extra(0)
    dim options(1) as string
    dim thing_and_id as string = thing_kind_name_singular() & " " & selected_id
    options(0) = "Pick " & thing_and_id
    options(1) = "Edit " & thing_and_id
    select case multichoice("", options())
     case 0: 
      result = selected_id
      exit do
     case 1:
      edit_record = selected_id
      do_edit = YES
    end select
   end if
  end if

  'Activating menu items.
  'The hover=ps.cur test here is to ensure that clicking and dragging off the selection does not activate it.
  if ps.cur andalso enter_or_space() orelse _
     ((readmouse.release AND mouseLeft) andalso hover = ps.cur andalso confirm_plank_click(hover)) then
   if IsAncestor(ps.cur, thinglist) then
    if can_edit = NO orelse edit_by_default = NO then
     'Selected a thing
     result = ps.cur->Extra(0)
     exit do
    else
     'Editing a thing
     edit_record = ps.cur->Extra(0)
     do_edit = YES
    end if
   elseif IsAncestor(ps.cur, back_holder) then
    'Cancel out of the browser
    result = start_id
    exit do
   elseif IsAncestor(ps.cur, filter_holder) then
    'Open the Filter window
    do_filter = YES
   elseif can_edit andalso isAncestor(ps.cur, new_holder) then
    do_add = YES
   end if
  end if

  if can_edit andalso (keyval(scCtrl) > 0 andalso keyval(scE) > 1) then
   if ps.cur andalso IsAncestor(ps.cur, thinglist) then
    'Editing a thing
    edit_record = ps.cur->Extra(0)
    do_edit = YES
   end if
  end if
  
  if can_edit andalso (keyval(scCtrl) > 0 andalso keyval(scN) > 1) then
   do_add = YES
  end if

  if do_add then
   do_add = NO
   'Add a new thing
   if highest_id() + 1 > highest_possible_id() then
    visible_debug "There are already " & highest_possible_id() & " " & thing_kind_name() & ", which is the most " & thing_kind_name() & " you can have."
    if quit_if_add_cancelled then result = -1: exit do
   else
    edit_record = highest_id() + 1
    do_edit = YES
   end if
  end if

  if do_edit then
   do_edit = NO
   dim ed_ret as integer = editor_func(edit_record)
   if ed_ret = -1 andalso quit_if_add_cancelled then result = -1 : exit do
   save_plank_selection ps
   build_thing_list()
   restore_plank_selection ps
   if ed_ret >= 0 then
    focus_plank_by_extra_id(ps, , ed_ret, thinglist)
   end if
   hover = 0
   orig_cur = find_plank_by_extra_id(ps, , start_id, thinglist)
  elseif do_filter then
   do_filter = NO
   if prompt_for_string(filter_text, "Find/Filter " & thing_kind_name()) then
    save_plank_selection ps
    build_thing_list()
    restore_plank_selection ps
    hover = 0
    orig_cur = find_plank_by_extra_id(ps, , start_id, thinglist)
    cursor_moved = YES
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

  if thinglist->SliceType = slGrid then ChangeGridSlice thinglist, , thinglist->Width \ plank_size.x
  if cursor_moved then
   'Yep, a move happened. We would update selection detail display here if that was a thing
   update_plank_scrolling ps
   dim id as integer = -1
   if ps.cur andalso IsAncestor(ps.cur, thinglist) then id = ps.cur->Extra(0)
   on_cursor_moved id, ps.cur
  end if
  cursor_moved = NO
  
  ChangeTextSlice filter_text_sl, IIF(filter_text <> "", "Showing Only: *" & filter_text & "*", "")
  ChangeTextSlice type_query_sl, selectst.query

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

Sub ThingBrowser.on_cursor_moved(byval id as integer, byval plank as Slice Ptr)
 'React to selecting a new plank with the keyboard or mouse
End Sub

Sub ThingBrowser.each_tick_each_plank(byval plank as Slice Ptr)
 'Nothing needs to happen here, if you don't want continous animation
End Sub

Sub ThingBrowser.each_tick_selected_plank(byval plank as Slice Ptr)
 'Nothing needs to happen here, if you don't want extra selection cursor animation
 'or special key handling
 '(the SL_PLANK_MENU_SELECTABLE animation of TextSlice and RectangleSlice color happens automatically even without this sub)
End Sub

Function ThingBrowser.confirm_plank_click (byval plank as Slice Ptr) as bool
 'This function is called if a click has happened with readmouse.pos colliding with the plank
 'return YES to select/pick the plank, or return NO if the plank should just be focused, not activated
 dim id as integer = plank->Extra(0)
 if not IsAncestor(plank, thinglist) then id = INT_MIN
 dim btn as Slice Ptr
 for i as integer = 0 to ubound(sub_buttons)
  dim code as integer = sub_buttons(i)
  if code <> 0 then
   btn = LookupSlice(code, plank)
   if btn andalso SliceCollidePoint(btn, readmouse.pos) then
    return on_sub_button_click(code, id, plank)
   end if
  end if
 next i
 return YES
End Function

Sub ThingBrowser.set_up_sub_buttons()
 'Sub classes can resize this array and fill it with slice lookup codes to make them behave as buttons
 'see also on_sub_button_click()
 redim sub_buttons(0) as integer
 sub_buttons(0) = 0
 
 'For example, if you wanted special handling when clicking on the sprite, you would do:
 'sub_buttons(0) = SL_EDITOR_THINGBROWSER_PLANK_SPRITE
End Sub

Function ThingBrowser.on_sub_button_click(byval button_lookup as integer,byval id as integer, byval plank as Slice Ptr) as bool
 'This is a handler for the special sub-buttons set up in set_up_sub_buttons()
 'It should return YES if this click should activate the plank as normal, or
 'return NO if the plank should just be focused without activating
 return NO
End Function

Sub ThingBrowser.loop_sprite_helper(byval plank as Slice Ptr, byval min as integer, byval max as integer, byval delay as integer=1)
 'A crude and simple animation helper for sprites in planks.
 'Uses the Extra(1) slot to manage the animation speed.
 'FIXME: rip this all out and replace it when the new animation system is ready
 dim spr as Slice Ptr = LookupSlice(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 if spr then
  loopvar spr->Extra(1), 0, delay
  if spr->Extra(1) = 0 then
   dim dat as SpriteSliceData Ptr = spr->SliceData
   loopvar dat->frame, min, max
  end if
 end if
End Sub

Function ThingBrowser.thing_kind_name() as string
 'Should be plural
 return "Things"
End Function

Function ThingBrowser.thing_kind_name_singular() as string
 'Strip the "s" off the end of the plural... override this function when that is wrong
 return rtrim(thing_kind_name(), ANY "s")
End Function

Function ThingBrowser.init_helpkey() as string
 return ""
End Function

Sub ThingBrowser.build_thing_list()
 dim timing as double = TIMER
 plank_menu_clear root, SL_EDITOR_THINGBROWSER_THINGLIST
 dim thinglist as slice ptr
 thinglist = LookupSliceSafe(SL_EDITOR_THINGBROWSER_THINGLIST, root)
 plank_size = XY(1,1)  'Avoid divide-by-zero
 dim plank as slice ptr
 for id as integer = lowest_id() to highest_id()
  plank = create_thing_plank(id)
  if plank = NULL then
   showbug "create_thing_plank returned NULL!"
   exit for  'Probably something is really broken
  end if
  if check_plank_filter(plank) then
   SetSliceParent(plank, thinglist)
   plank->Lookup = SL_PLANK_HOLDER
   plank->Extra(0) = id
   plank_size.x = large(plank_size.x, plank->Width)
   plank_size.y = large(plank_size.y, plank->Height)
  else
   'Don't use this one because it was filtered out
   DeleteSlice @(plank)
  end if
 next id
 thinglist->Height = plank_size.y  'Only needed if a Grid: Height of one row
 if thinglist->SliceType = slGrid then ChangeGridSlice thinglist, , thinglist->Width \ plank_size.x
 DrawSlice root, vpage 'refresh screen positions
 timing = TIMER - timing
 if timing > 0.2 then
  debuginfo thing_kind_name() & ": build_thing_list() took " & cint(timing * 1000) & "ms"
 end if
End Sub

Function ThingBrowser.check_plank_filter(byval sl as Slice Ptr) as bool
 'Returns YES if this plank is okay to display according to the text filter check.
 'Returns NO if the plank should be hidden
 
 'If there is no filter active, succeed immediately
 if filter_text = "" then return YES
 
 if FindTextSliceStringRecursively(sl, filter_text) <> 0 then return YES
 'No text was found that matches the filter text
 return NO
End Function

Function ThingBrowser.lowest_id() as integer
 dim ret as integer = 0
 if skip_zero then ret += 1
 if or_none then ret -= 1
 return ret
End Function

Function ThingBrowser.highest_id() as integer
 return -1
End Function

Function ThingBrowser.highest_possible_id() as integer
 return 32767
End Function

'The ID which is the NONE option. Irrelevant if or_none = NO (but mustn't be a valid ID)
Function ThingBrowser.none_id() as integer
 if or_none then
  return lowest_id()
 else
  return INT_MIN  'No None
 end if
End Function

Function ThingBrowser.create_thing_plank(byval id as integer) as Slice Ptr
 'Override this for complex planks.
 'For simple plain-text planks, just override thing_text_for_id instead
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
 'Override this for plain text planks.
 'For more complex planks, override create_thing_plank instead
 return str(id)
End Function

Sub ThingBrowser.handle_cropafter(byval id as integer)
 visible_debug("No support for deleting " & thing_kind_name() & " after the selected one.")
End Sub

'-----------------------------------------------------------------------

Function ItemBrowser.thing_kind_name() as string
 return "Items"
End Function

Function ItemBrowser.init_helpkey() as string
 return "item_editor_browser"
End Function

Function ItemBrowser.highest_id() as integer
 return gen(genMaxItem)
End Function

Function ItemBrowser.highest_possible_id() as integer
 return maxMaxItems
End Function

Function ItemBrowser.thing_text_for_id(byval id as integer) as string
 dim digits as integer = len(str(highest_id()))
 if id = none_id then
  return SPACE(digits) & " " & rpad("NO ITEM", , 8)
 end if
 return lpad(str(id), " ", digits) & " " & rpad(readitemname(id), " ", 8)
End Function

Sub ItemBrowser.handle_cropafter(byval id as integer)
 cropafter id, gen(genMaxItem), 0, game & ".itm", getbinsize(binITM)
 load_special_tag_caches
End Sub

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

Function ShopBrowser.highest_possible_id() as integer
 return maxMaxShop
End Function

Function ShopBrowser.thing_text_for_id(byval id as integer) as string
 dim digits as integer = len(str(highest_id()))
 if id = none_id then
  return SPACE(digits) & " " & rpad("NO SHOP", , 16)
 end if
 return lpad(str(id), " ", digits) & " " & rpad(readshopname(id), " ", 16)
End Function

'-----------------------------------------------------------------------

Constructor ShopStuffBrowser(byval shop_id as integer)
 this.shop_id = shop_id
End Constructor

Function ShopStuffBrowser.thing_kind_name() as string
 return "Shop Stuff"
End Function

Function ShopStuffBrowser.init_helpkey() as string
 return "shop_stuff_browser"
End Function

Function ShopStuffBrowser.highest_id() as integer
 dim shopbuf(20) as integer
 loadrecord shopbuf(), game & ".sho", 40 \ 2, shop_id
 return shopbuf(16)
End Function

Function ShopStuffBrowser.highest_possible_id() as integer
 return 49
End Function

Function ShopStuffBrowser.thing_text_for_id(byval id as integer) as string
 dim digits as integer = len(str(highest_id()))
 if id = none_id then
  return SPACE(digits) & " " & rpad("NOTHING", " ", 16)  'I think this is never used
 end if
 dim stufbuf(curbinsize(binSTF) \ 2 - 1) as integer
 loadrecord stufbuf(), game & ".stf", getbinsize(binSTF) \ 2, shop_id * 50 + id
 dim kind as string = iif(stufbuf(17) = 1, "(HERO)", "(ITEM)")
 dim thing_name as string = readbadbinstring(stufbuf(), 0, 16, 0)
 dim stock_info as string
 if stufbuf(19) = -1 then  'Infinite stock
  stock_info = "*  "
 elseif stufbuf(19) = 0 then  'No stock
  stock_info = "-  "
 else
  stock_info = rpad(STR(stufbuf(19)), " ", 3)
 end if
 return lpad(str(id), " ", digits) & " " & rpad(thing_name, " ", 16) & " " & stock_info & " " & kind
End Function

'-----------------------------------------------------------------------

Function AttackBrowser.thing_kind_name() as string
 return "Attacks"
End Function

Function AttackBrowser.init_helpkey() as string
 return "attack_editor_browser"
End Function

Function AttackBrowser.highest_id() as integer
 return gen(genMaxAttack)
End Function

Function AttackBrowser.highest_possible_id() as integer
 return maxMaxAttacks
End Function

Function AttackBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim attack as AttackData
 loadattackdata attack, id

 if plank_template = 0 then
  plank_template = load_plank_from_file(finddatafile("attack_browser_plank.slice"))
 end if
 dim plank as Slice Ptr
 plank = CloneSliceTree(plank_template)
 
 dim spr as Slice Ptr
 spr = LookupSliceSafe(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 ChangeSpriteSlice spr, sprTypeAttack, attack.picture, attack.pal, 0
 if id = none_id then
  spr->Visible = NO
 end if
 dim txt as Slice Ptr
 txt = LookupSliceSafe(SL_PLANK_MENU_SELECTABLE, plank, slText)
 ChangeTextSlice txt, id & !"\n" & attack.name
 if id = none_id then ChangeTextSlice txt, "NONE"
 return plank
End Function

Sub AttackBrowser.handle_cropafter(byval id as integer)
 cropafter id, gen(genMaxAttack), 0, game & ".dt6", 80
 '--this is a hack to detect if it is safe to erase the extended data
 '--in the second file
 IF id = gen(genMaxAttack) THEN
  '--delete the end of attack.bin without the need to prompt
  cropafter id, gen(genMaxAttack), 0, workingdir & SLASH & "attack.bin", getbinsize(binATTACK), NO
 END IF
End Sub

'-----------------------------------------------------------------------

Function EnemyBrowser.thing_kind_name() as string
 return "Enemies"
End Function

Function EnemyBrowser.thing_kind_name_singular() as string
 return "Enemy"
End Function

Function EnemyBrowser.init_helpkey() as string
 return "enemy_editor_browser"
End Function

Function EnemyBrowser.highest_id() as integer
 return gen(genMaxEnemy)
End Function

Function EnemyBrowser.highest_possible_id() as integer
 return maxMaxAttacks
End Function

Function EnemyBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim enemy as EnemyDef
 loadenemydata enemy, id

 if plank_template = 0 then
  plank_template = load_plank_from_file(finddatafile("enemy_browser_plank.slice"))
 end if
 dim plank as Slice Ptr
 plank = CloneSliceTree(plank_template)
 
 dim spr as Slice Ptr
 spr = LookupSliceSafe(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 dim spr_kind as SpriteType
 select case enemy.size
  case 0: spr_kind = sprTypeSmallEnemy
  case 1: spr_kind = sprTypeMediumEnemy
  case 2: spr_kind = sprTypeLargeEnemy
  'FIXME: switch this to sprTypeEnemy when EnemyDef supports it
 end select
 ChangeSpriteSlice spr, spr_kind, enemy.pic, enemy.pal, 0
 if id = none_id then
  spr->Visible = NO
 end if
 dim txt as Slice Ptr
 txt = LookupSliceSafe(SL_PLANK_MENU_SELECTABLE, plank, slText)
 ChangeTextSlice txt, id & !"\n" & enemy.name
 if id = none_id then ChangeTextSlice txt, "NONE"
 return plank
End Function

Sub EnemyBrowser.handle_cropafter(byval id as integer)
 cropafter id, gen(genMaxEnemy), 0, game & ".dt1", getbinsize(binDT1)
End Sub

'-----------------------------------------------------------------------

Function FormationBrowser.thing_kind_name() as string
 return "Formations"
End Function

Function FormationBrowser.thing_kind_name_singular() as string
 return "Formation"
End Function

Function FormationBrowser.init_helpkey() as string
 return "formation_editor_browser"
End Function

Function FormationBrowser.highest_id() as integer
 return gen(genMaxFormation)
End Function

Function FormationBrowser.highest_possible_id() as integer
 return maxMaxFormation
End Function

Function FormationBrowser.thing_text_for_id(byval id as integer) as string
 dim digits as integer = len(str(highest_id()))
 if id = none_id then
  return SPACE(digits) & " " & rpad("NOTHING", " ", 16)
 end if
 dim form as Formation
 LoadFormation form, game & ".for", id
 dim cap as string
 cap = lpad(str(id), " ", digits) & " " & describe_formation(form)
 'Make a guess of about how many characters to padd the string to so they are all single-screen sized
 dim w_chars as integer = (vpages(vpage)->w - 16) / 8
 return rpad(cap, " ", w_chars)
End Function

Sub FormationBrowser.handle_cropafter(byval id as integer)
 cropafter id, gen(genMaxFormation), 0, game & ".for", 80
End Sub

'-----------------------------------------------------------------------

Function HeroBrowser.thing_kind_name() as string
 return "Heroes"
End Function

Function HeroBrowser.thing_kind_name_singular() as string
 return "Hero"
End Function

Function HeroBrowser.init_helpkey() as string
 return "hero_editor_browser"
End Function

Function HeroBrowser.highest_id() as integer
 return gen(genMaxHero)
End Function

Function HeroBrowser.highest_possible_id() as integer
 return maxMaxHero
End Function

Function HeroBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim hero as HeroDef
 loadherodata hero, id

 if plank_template = 0 then
  plank_template = load_plank_from_file(finddatafile("hero_browser_plank.slice"))
 end if
 dim plank as Slice Ptr
 plank = CloneSliceTree(plank_template)
 
 dim spr as Slice Ptr
 spr = LookupSliceSafe(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 ChangeSpriteSlice spr, sprTypeHero, hero.sprite, hero.sprite_pal, 0
 if id = none_id then
  spr->Visible = NO
 end if
 dim txt as Slice Ptr
 txt = LookupSliceSafe(SL_PLANK_MENU_SELECTABLE, plank, slText)
 ChangeTextSlice txt, id & !"\n" & hero.name
 if id = none_id then ChangeTextSlice txt, "NONE"
 return plank
End Function

Sub HeroBrowser.handle_cropafter(byval id as integer)
 'FIXME: this only clears the old .dt0 file, not the new heroes.reld file
 cropafter id, gen(genMaxHero), 0, game & ".dt0", getbinsize(binDT0)
 load_special_tag_caches
End Sub

'-----------------------------------------------------------------------

Function SfxBrowser.thing_kind_name() as string
 return "Sound Effects"
End Function

Function SfxBrowser.init_helpkey() as string
 return "sfx_editor_browser"
End Function

Function SfxBrowser.highest_id() as integer
 return gen(genMaxSFX)
End Function

Function SfxBrowser.highest_possible_id() as integer
 return maxMaxSFX
End Function

Sub SfxBrowser.on_cursor_moved(byval id as integer, byval plank as Slice Ptr)
 if id >= 0 then
  playsfx id, 0
 end if
End Sub

Sub SfxBrowser.set_up_sub_buttons()
 redim sub_buttons(0) as integer
 sub_buttons(0) = SL_EDITOR_THINGBROWSER_PLANK_SPRITE
End Sub

Function SfxBrowser.on_sub_button_click(byval button_lookup as integer,byval id as integer, byval plank as Slice Ptr) as bool
 select case button_lookup
  case SL_EDITOR_THINGBROWSER_PLANK_SPRITE:
   if id >= 0 then
    playsfx id, 0
   end if
   return NO
 end select
 return YES
End Function

Sub SfxBrowser.each_tick_selected_plank(byval plank as Slice Ptr)
#ifdef IS_CUSTOM
 if keyval(scF2) > 1 then Custom_volume_menu
#endif
End Sub

Sub SfxBrowser.build_thing_list()
 'Find the list of all sfx that actually exist, so we only show Play icons for them
 redim imported_sfx(highest_id())
 list_of_imported_songs_or_sfx(imported_sfx(), YES)

 base.build_thing_list()
End Sub

Function SfxBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim sfxname as string = getsfxname(id)

 if plank_template = 0 then
  plank_template = load_plank_from_file(finddatafile("sfx_browser_plank.slice"))
 end if
 dim plank as Slice Ptr
 plank = CloneSliceTree(plank_template)

 dim spr as Slice Ptr
 spr = LookupSliceSafe(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 if id = none_id orelse imported_sfx(id) = NO then
  spr->Visible = NO
 end if
 dim txt as Slice Ptr
 txt = LookupSliceSafe(SL_PLANK_MENU_SELECTABLE, plank, slText)
 ChangeTextSlice txt, id & !"\n" & sfxname
 if id = none_id then ChangeTextSlice txt, "NONE"
 return plank
End Function

'-----------------------------------------------------------------------

Function SongBrowser.thing_kind_name() as string
 return "Songs"
End Function

Function SongBrowser.init_helpkey() as string
 return "song_editor_browser"
End Function

Function SongBrowser.highest_id() as integer
 return gen(genMaxSong)
End Function

Function SongBrowser.highest_possible_id() as integer
 return maxMaxSong
End Function

Sub SongBrowser.on_cursor_moved(byval id as integer, byval plank as Slice Ptr)
 if id >= 0 then
  'playsongnum doesn't stop the music if you play a nonexistent song.
  if imported_songs(id) then
   playsongnum id
  else
   music_stop
  end if
 else
  music_stop
 end if
End Sub

Sub SongBrowser.set_up_sub_buttons()
 redim sub_buttons(0) as integer
 sub_buttons(0) = SL_EDITOR_THINGBROWSER_PLANK_SPRITE
End Sub

Function SongBrowser.on_sub_button_click(byval button_lookup as integer,byval id as integer, byval plank as Slice Ptr) as bool
 select case button_lookup
  case SL_EDITOR_THINGBROWSER_PLANK_SPRITE:
   on_cursor_moved id, plank  'Play the song
   return NO
 end select
 return YES
End Function

Sub SongBrowser.each_tick_selected_plank(byval plank as Slice Ptr)
#ifdef IS_CUSTOM
 if keyval(scF2) > 1 then Custom_volume_menu
#endif
End Sub

Sub SongBrowser.build_thing_list()
 'Find the list of all songs that actually exist, so we only show Play icons for them
 redim imported_songs(highest_id())
 list_of_imported_songs_or_sfx(imported_songs(), NO)

 base.build_thing_list()
End Sub

Function SongBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim songname as string = getsongname(id)

 if plank_template = 0 then
  plank_template = load_plank_from_file(finddatafile("song_browser_plank.slice"))
 end if
 dim plank as Slice Ptr
 plank = CloneSliceTree(plank_template)
 
 dim spr as Slice Ptr
 spr = LookupSliceSafe(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 if id = none_id orelse imported_songs(id) = NO then
  spr->Visible = NO
 end if
 dim txt as Slice Ptr
 txt = LookupSliceSafe(SL_PLANK_MENU_SELECTABLE, plank, slText)
 ChangeTextSlice txt, id & !"\n" & songname
 if id = none_id then ChangeTextSlice txt, "NONE"
 return plank
End Function

'-----------------------------------------------------------------------

Function TextboxBrowser.thing_kind_name() as string
 return "Textboxes"
End Function

Function TextboxBrowser.thing_kind_name_singular() as string
 return "Textbox"
End Function

Function TextboxBrowser.init_helpkey() as string
 return "textbox_browser"
End Function

Function TextboxBrowser.highest_id() as integer
 return gen(genMaxTextBox)
End Function

Function TextboxBrowser.highest_possible_id() as integer
 return maxMaxTextbox
End Function

Function TextboxBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim box as TextBox
 LoadTextBox box, id

 'Most of the text will not be visible, but we still do want to use it for search and filter
 dim text as string = textbox_preview_line(box, 800)

 if plank_template = 0 then
  plank_template = load_plank_from_file(finddatafile("textbox_browser_plank.slice"))
 end if
 dim plank as Slice Ptr
 plank = CloneSliceTree(plank_template)
 
 dim txt as Slice Ptr
 txt = LookupSliceSafe(SL_PLANK_MENU_SELECTABLE, plank, slText)
 dim caption as string
 if id = none_id then
  caption = "NONE"
 elseif id = 0 then
  caption = id & " [template] " & text
 else
  caption = id & " " & text
 end if
 ChangeTextSlice txt, caption

 'show the text box color
 dim rect as Slice Ptr
 rect = LookupSlice(SL_TEXTBOX_BOX, plank)
 if box.no_box then
  'Invisible box
  ChangeRectangleSlice rect, , , , borderNone, transHollow
 else
  ChangeRectangleSlice rect, , boxlook(box.boxstyle).bgcol, , borderNone, iif(box.opaque, transOpaque, transFuzzy)
 end if

 return plank
End Function

Sub TextboxBrowser.handle_cropafter(byval id as integer)
 cropafter id, gen(genMaxTextBox), 0, game & ".say", getbinsize(binSAY)
End Sub

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

Sub ConstantListBrowser.handle_cropafter(byval id as integer)
 'Silently do nothing
End Sub

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
 a_copy array(), list()
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
   showbug "set_list_from_flexmenu: caption_code " & caption_code & " is not in the expected range of 1000 to 2999"
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

Function SpriteBrowser.sprite_kind() as SpriteType
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
 if id <> none_id then
  dim spr as Slice Ptr
  spr = NewSliceOfType(slSprite, plank, SL_EDITOR_THINGBROWSER_PLANK_SPRITE)
  ChangeSpriteSlice spr, sprite_kind(), id, , sprite_frame()
  plank->size = spr->size
 else
  plank->size = XY(40, sprite_sizes(sprite_kind()).size.h)
 end if
 dim txt as Slice Ptr
 txt = NewSliceOfType(slText, plank, SL_PLANK_MENU_SELECTABLE)
 txt->AlignVert = alignBottom
 txt->AnchorVert = alignBottom
 ChangeTextSlice txt, iif(id = none_id, "NONE", thing_text_for_id(id)), uilook(uiMenuItem), YES
 return plank
End Function

'-----------------------------------------------------------------------

'HERO
Function HeroSpriteBrowser.highest_id() as integer
 return gen(genMaxHeroPic)
End Function

Function HeroSpriteBrowser.sprite_kind() as SpriteType
 return sprTypeHero
End Function

Sub HeroSpriteBrowser.each_tick_selected_plank(byval plank as Slice Ptr)
 loop_sprite_helper plank, 0, 1
End Sub

'WALKABOUT
Function WalkaboutSpriteBrowser.highest_id() as integer
 return gen(genMaxNPCPic)
End Function

Function WalkaboutSpriteBrowser.sprite_kind() as SpriteType
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

Function PortraitSpriteBrowser.sprite_kind() as SpriteType
 return sprTypePortrait
End Function

'ENEMY
Function EnemySpriteBrowser.highest_id() as integer
 select case size_group
  case 0: return gen(genMaxEnemy1Pic)
  case 1: return gen(genMaxEnemy2Pic)
  case 2: return gen(genMaxEnemy3Pic)
 end select
 showbug "EnemySpriteBrowser.highest_id(): size_group " & size_group & " is not valid"
 return 0
End Function

Function EnemySpriteBrowser.sprite_kind() as SpriteType
 select case size_group
  case 0: return sprTypeSmallEnemy
  case 1: return sprTypeMediumEnemy
  case 2: return sprTypeLargeEnemy
 end select
 showbug "EnemySpriteBrowser.sprite_kind: size_group " & size_group & " is not valid"
 return sprTypeInvalid
End Function

'ATTACK
Function AttackSpriteBrowser.highest_id() as integer
 return gen(genMaxAttackPic)
End Function

Function AttackSpriteBrowser.sprite_kind() as SpriteType
 return sprTypeAttack
End Function

Sub AttackSpriteBrowser.each_tick_each_plank(byval plank as Slice Ptr)
 loop_sprite_helper plank, 0, 2
End Sub

'WEAPON
Function WeaponSpriteBrowser.highest_id() as integer
 return gen(genMaxWeaponPic)
End Function

Function WeaponSpriteBrowser.sprite_kind() as SpriteType
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

Function BackdropSpriteBrowser.sprite_kind() as SpriteType
 return sprTypeBackdrop
End Function

Function BackdropSpriteBrowser.create_thing_plank(byval id as integer) as Slice ptr
 dim plank as Slice Ptr
 plank = Base.create_thing_plank(id)
 plank->size = XY(98, 63)
 dim spr as Slice Ptr
 spr = LookupSlice(SL_EDITOR_THINGBROWSER_PLANK_SPRITE, plank)
 if id = none_id then
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

Function BoxborderSpriteBrowser.sprite_kind() as SpriteType
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
 if id = none_id then ChangeTextSlice txt, "NONE"
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
   showbug "No sprite browser available for sprite type " & spr_type
 end select
 return start_id
End Function

'-----------------------------------------------------------------------
