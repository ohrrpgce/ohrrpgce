'OHRRPGCE CUSTOM - Menu Editor (not to be confused with menu slices editors)
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "loading.bi"
#include "cglobals.bi"
#include "scrconst.bi"
#include "sliceedit.bi"
#include "custom.bi"

'--Local SUBs
DECLARE SUB update_menu_editor_menu(byval record as integer, edmenu as MenuDef, menu as MenuDef)
DECLARE SUB update_edited_menu(menudata as MenuDef)
DECLARE SUB update_detail_menu(detail as MenuDef, menudata as MenuDef, mi as MenuDefItem)
DECLARE SUB menu_editor_keys (state as MenuState, mstate as MenuState, menudata as MenuDef, byref record as integer, menu_set as MenuSet, viewport_page as integer)
DECLARE SUB menu_editor_menu_keys (mstate as MenuState, dstate as MenuState, menudata as MenuDef, byval record as integer)
DECLARE SUB menu_editor_detail_keys(dstate as MenuState, mstate as MenuState, detail as MenuDef, mi as MenuDefItem)
DECLARE SUB preview_menu(menu as MenuDef, mstate as MenuState, viewport_page as integer, destpage as integer = -1)
DECLARE SUB edit_menu_bits (menu as MenuDef)
DECLARE SUB edit_menu_item_bits (mi as MenuDefItem)
DECLARE SUB reposition_menu (menu as MenuDef, mstate as MenuState, viewport_page as integer)
DECLARE SUB reposition_anchor (menu as MenuDef, mstate as MenuState, viewport_page as integer)


SUB menu_editor ()

DIM menu_set as MenuSet
menu_set.menufile = workingdir & SLASH & "menus.bin"
menu_set.itemfile = workingdir & SLASH & "menuitem.bin"

DIM record as integer = 0

DIM state as MenuState 'top level
state.active = YES
state.need_update = YES
DIM mstate as MenuState  'menu being edited
mstate.active = NO
mstate.need_update = YES
DIM dstate as MenuState 'detail state
dstate.active = NO

DIM edmenu as MenuDef  'top level
WITH edmenu
 .textalign = alignLeft
 .alignhoriz = alignLeft
 .alignvert = alignTop
 .anchorhoriz = alignLeft
 .anchorvert = alignTop
 .boxstyle = 3
 .translucent = YES
 .min_chars = 38
END WITH
DIM menudata as MenuDef  'menu being edited
LoadMenuData menu_set, menudata, record
DIM detail as MenuDef  'detail editing menu
WITH detail
 .textalign = alignLeft
 .anchorhoriz = alignLeft
 .anchorvert = alignBottom
 .offset.x = -152
 .offset.y = 92
 .min_chars = 36
END WITH

DIM box_preview as string = ""

DIM viewport_page as integer = gameres_page()

setkeys YES
DO
 setwait 55
 setkeys YES
 
 IF state.active = NO THEN EXIT DO
 IF mstate.active = YES THEN
  menu_editor_menu_keys mstate, dstate, menudata, record
 ELSEIF dstate.active = YES THEN
  menu_editor_detail_keys dstate, mstate, detail, *menudata.items[mstate.pt]
 ELSE
  menu_editor_keys state, mstate, menudata, record, menu_set, viewport_page
 END IF
 
 IF state.need_update THEN
  state.need_update = NO
  update_menu_editor_menu record, edmenu, menudata
  init_menu_state state, edmenu
  init_menu_state mstate, menudata
 END IF
 IF dstate.need_update THEN
  dstate.need_update = NO
  update_detail_menu detail, menudata, *menudata.items[mstate.pt]
  init_menu_state dstate, detail
  WITH *menudata.items[mstate.pt]
   IF .t = mtypeTextBox THEN
    box_preview = textbox_preview_line(.sub_t)
   END IF
  END WITH
  'If any menu item data changed, the menu itself also needs updating
  mstate.need_update = YES
 END IF
 IF mstate.need_update THEN
  mstate.need_update = NO
  update_edited_menu menudata
  init_menu_state mstate, menudata
 END IF
 
 clearpage dpage
 preview_menu menudata, mstate, viewport_page, dpage

 IF mstate.active THEN
  edgeprint "ENTER to edit, Shift+Arrows to re-order", 0, pBottom, uilook(uiDisabledItem), dpage
  IF record = 0 THEN
   edgeprint "CTRL+R to reload default", 0, pBottom - 10, uilook(uiDisabledItem), dpage
  END IF
 END IF
 IF NOT mstate.active AND NOT dstate.active THEN draw_menu edmenu, state, dpage
 IF dstate.active THEN
  draw_menu detail, dstate, dpage
  IF menudata.items[mstate.pt]->t = mtypeTextBox THEN
   edgeprint box_preview, 0, pBottom, uilook(uiText), dpage
  END IF
 END IF
 
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
SaveMenuData menu_set, menudata, record
freepage viewport_page

END SUB

SUB menu_editor_keys (state as MenuState, mstate as MenuState, menudata as MenuDef, byref record as integer, menu_set as MenuSet, viewport_page as integer)
 IF keyval(ccCancel) > 1 THEN state.active = NO
 IF keyval(scF1) > 1 THEN show_help "menu_editor_main"
 
 usemenu state
 
 SELECT CASE state.pt
  CASE 0
   IF enter_space_click(state) THEN
    state.active = NO
   END IF
  CASE 1
   DIM saverecord as integer = record
   IF intgrabber_with_addset(record, 0, gen(genMaxMenu), 32767, "menu") THEN
    IF record > gen(genMaxMenu) THEN gen(genMaxMenu) = record
    SaveMenuData menu_set, menudata, saverecord
    LoadMenuData menu_set, menudata, record
    state.need_update = YES
    mstate.need_update = YES
   END IF
  CASE 2
   IF strgrabber(menudata.name, 20) THEN state.need_update = YES
  CASE 3
   IF enter_space_click(state) THEN
    mstate.active = YES
    mstate.need_update = YES
    menudata.edit_mode = YES
    append_menu_item menudata, "[NEW MENU ITEM]"
   END IF
  CASE 4
   IF intgrabber(menudata.boxstyle, 0, 14) THEN state.need_update = YES
  CASE 5
   IF intgrabber(menudata.textcolor, LowColorCode(), 255) THEN state.need_update = YES
   IF enter_space_click(state) THEN
    menudata.textcolor = color_browser_256(menudata.textcolor)
    state.need_update = YES
   END IF
  CASE 6
   IF intgrabber(menudata.disabled_textcolor, LowColorCode(), 255) THEN state.need_update = YES
   IF enter_space_click(state) THEN
    menudata.disabled_textcolor = color_browser_256(menudata.disabled_textcolor)
    state.need_update = YES
   END IF
  CASE 7
   IF intgrabber(menudata.maxrows, 0, 100) THEN state.need_update = YES
  CASE 8
   IF enter_space_click(state) THEN
    edit_menu_bits menudata
    state.need_update = YES
   END IF
  CASE 9
   IF enter_space_click(state) THEN
    reposition_menu menudata, mstate, viewport_page
   END IF
  CASE 10
   IF enter_space_click(state) THEN
    reposition_anchor menudata, mstate, viewport_page
   END IF
  CASE 11 ' text align
   IF intgrabber(menudata.textalign, alignLeft, alignRight) THEN state.need_update = YES
  CASE 12 ' Minimum width in chars
   IF intgrabber(menudata.min_chars, 0, 200) THEN state.need_update = YES
  CASE 13 ' Maximum width in chars
   IF intgrabber(menudata.max_chars, 0, 200) THEN state.need_update = YES
  CASE 14 ' border size
   IF intgrabber(menudata.bordersize, -100, 100) THEN state.need_update = YES
  CASE 15 ' item spacing
   IF intgrabber(menudata.itemspacing, -10, 100) THEN state.need_update = YES
  CASE 16: ' on-close script
   IF enter_space_click(state) THEN
    scriptbrowse menudata.on_close, plottrigger, "menu on-close plotscript"
    state.need_update = YES
   END IF
   IF scrintgrabber(menudata.on_close, 0, 0, ccLeft, ccRight, 1, plottrigger) THEN state.need_update = YES
  CASE 17: ' esc menu
   IF zintgrabber(menudata.esc_menu, -1, gen(genMaxMenu)) THEN state.need_update = YES
 END SELECT
END SUB

SUB menu_editor_menu_keys (mstate as MenuState, dstate as MenuState, menudata as MenuDef, byval record as integer)
 DIM i as integer
 DIM elem as integer

 IF keyval(ccCancel) > 1 THEN
  mstate.active = NO
  menudata.edit_mode = NO
  mstate.need_update = YES
  'remove [NEW MENU ITEM]
  remove_menu_item menudata, menudata.last
  EXIT SUB
 END IF
 IF keyval(scF1) > 1 THEN show_help "menu_editor_items"

 usemenu mstate
 IF mstate.pt >= 0 AND mstate.pt < menudata.numitems THEN
 WITH *menudata.items[mstate.pt]
  IF NOT (menudata.edit_mode = YES AND .trueorder.next = NULL) THEN  'not the last item, "NEW MENU ITEM"
   IF strgrabber(.caption, 38) THEN
    'Space should not enter the menu
   ELSEIF enter_space_click(mstate) THEN
    mstate.active = NO
    dstate.active = YES
    dstate.need_update = YES
   END IF
   IF keyval(scDelete) > 1 THEN
    IF yesno("Delete this menu item?", NO) THEN
     remove_menu_item menudata, mstate.pt
     mstate.need_update = YES
    END IF
   END IF
   IF keyval(scShift) > 0 THEN
    IF keyval(ccUp) > 1 AND mstate.pt < mstate.last - 1 THEN ' just went up
     'NOTE: Cursor will have already moved because of usemenu call above
     swap_menu_items menudata, mstate.pt, menudata, mstate.pt + 1
     mstate.need_update = YES
    END IF
    IF keyval(ccDown) > 1 AND mstate.pt > mstate.first THEN ' just went down
     'NOTE: Cursor will have already moved because of usemenu call above
     swap_menu_items menudata, mstate.pt, menudata, mstate.pt - 1
     mstate.need_update = YES
    END IF
   END IF
  ELSE
   IF menudata.edit_mode = YES THEN
    'Selecting the item that appends new items
    IF enter_space_click(mstate) THEN
     menudata.last->caption = ""
     append_menu_item menudata, "[NEW MENU ITEM]"
     mstate.active = NO
     mstate.need_update = YES
     dstate.active = YES
     dstate.need_update = YES
    END IF
   END IF
  END IF
 END WITH
 END IF' above block only runs with a valid mstate.pt

 IF record = 0 THEN
  IF keyval(scCtrl) > 0 AND keyval(scR) > 1 THEN
   IF yesno("Reload the default main menu?") THEN
    ClearMenuData menudata
    create_default_menu menudata
    append_menu_item menudata, "[NEW MENU ITEM]"
    mstate.need_update = YES
   END IF
  END IF
 END IF
 
END SUB

SUB menu_editor_detail_keys(dstate as MenuState, mstate as MenuState, detail as MenuDef, mi as MenuDefItem)
 DIM max as integer

 IF keyval(ccCancel) > 1 THEN
  dstate.active = NO
  mstate.active = YES
  EXIT SUB
 END IF
 IF keyval(scF1) > 1 THEN show_help "menu_editor_item_details"

 usemenu dstate

 DIM editaction as integer = detail.items[dstate.pt]->sub_t
 SELECT CASE editaction
  CASE 0: 'go back
   IF enter_space_click(dstate) THEN
    dstate.active = NO
    mstate.active = YES
    EXIT SUB
   END IF
  CASE 1: 'caption
   IF strgrabber(mi.caption, 38) THEN
    dstate.need_update = YES
   END IF
  CASE 2: 'type
   IF intgrabber(mi.t, 0, mtypeLAST) THEN
    mi.sub_t = 0
    dstate.need_update = YES
   END IF
  CASE 3: 'subtype
   SELECT CASE mi.t
    CASE mtypeLabel
     max = lbLAST
    CASE mtypeSpecial
     max = spLAST
    CASE mtypeMenu
     max = gen(genMaxMenu)
    CASE mtypeTextBox
     max = gen(genMaxTextBox)
   END SELECT
   IF mi.t = mtypeScript THEN
    IF scrintgrabber(mi.sub_t, 0, 0, ccLeft, ccRight, 1, plottrigger) THEN dstate.need_update = YES
    IF enter_space_click(dstate) THEN
     scriptbrowse mi.sub_t, plottrigger, "Menu Item Script"
     dstate.need_update = YES
    END IF
   ELSEIF mi.t = mtypeTextBox THEN
    IF textboxgrabber(mi.sub_t, dstate) THEN dstate.need_update = YES
   ELSE
    IF intgrabber(mi.sub_t, 0, max) THEN dstate.need_update = YES
   END IF
  CASE 4: 'conditional tag1
   IF tag_grabber(mi.tag1, dstate) THEN dstate.need_update = YES
  CASE 5: 'conditional tag2
   IF tag_grabber(mi.tag2, dstate) THEN dstate.need_update = YES
  CASE 6: 'set tag
   IF tag_set_grabber(mi.settag, dstate) THEN dstate.need_update = YES
  CASE 7: 'toggle tag
   IF tag_id_grabber(mi.togtag, dstate) THEN dstate.need_update = YES
  CASE 8: ' bitsets
   IF enter_space_click(dstate) THEN
    edit_menu_item_bits mi
    dstate.need_update = YES
   END IF
  CASE 9 TO 11: 'extra data
   IF intgrabber(mi.extra(editaction - 9), -32768, 32767) THEN dstate.need_update = YES
  CASE 12: 'color
   IF intgrabber(mi.col, LowColorCode(), 255) THEN dstate.need_update = YES
   IF enter_space_click(dstate) THEN
    mi.col = color_browser_256(mi.col)
    dstate.need_update = YES
   END IF
  CASE 13: 'disabled color
   IF intgrabber(mi.disabled_col, LowColorCode(), 255) THEN dstate.need_update = YES
   IF enter_space_click(dstate) THEN
    mi.disabled_col = color_browser_256(mi.disabled_col)
    dstate.need_update = YES
   END IF

 END SELECT

END SUB

SUB update_menu_editor_menu(byval record as integer, edmenu as MenuDef, menu as MenuDef)
 DIM cap as string
 DeleteMenuItems edmenu
 
 append_menu_item edmenu, "Previous Menu"
 
 cap = "Menu " & record
 IF record = 0 THEN cap = cap & " (MAIN MENU)"
 append_menu_item edmenu, cap
 
 append_menu_item edmenu, "Name: " & menu.name
 append_menu_item edmenu, "Edit items..."
 append_menu_item edmenu, "Box style: " & menu.boxstyle
 append_menu_item edmenu, "Text color: " & slice_color_caption(menu.textcolor, "Default UI color")
 append_menu_item edmenu, "Disabled color: " & slice_color_caption(menu.disabled_textcolor, "Default UI color")
 append_menu_item edmenu, "Max rows to display: " & zero_default(menu.maxrows)
 append_menu_item edmenu, "Edit bitsets..."
 append_menu_item edmenu, "Reposition menu..."
 append_menu_item edmenu, "Change anchor point..."
 append_menu_item edmenu, "Text align: " & HorizCaptions(menu.textalign)
 append_menu_item edmenu, "Minimum width: " & zero_default(menu.min_chars, "Automatic")
 append_menu_item edmenu, "Maximum width: " & zero_default(menu.max_chars, "None")
 append_menu_item edmenu, "Border size: " & zero_default(menu.bordersize)
 append_menu_item edmenu, "Item spacing: " & zero_default(menu.itemspacing)
 append_menu_item edmenu, "On-close script: " & scriptname(menu.on_close)
 IF menu.esc_menu = 0 THEN
  cap = "just closes this menu"
 ELSE
  cap = "switch to menu " & menu.esc_menu - 1 & " " & getmenuname(menu.esc_menu - 1)
 END IF
 IF menu.no_close THEN cap = "disabled by bitset"
 append_menu_item edmenu, "Cancel button: " & cap
END SUB

'Update the user-defined menu itself, for tags.
'Note: MenuDefItem.disabled and .unselectable are not saved to disk, hence they can be safely modified.
SUB update_edited_menu(menudata as MenuDef)
 FOR idx as integer = 0 TO menudata.numitems - 1
  WITH *menudata.items[idx]
   .disabled = NO
   IF .t = mtypeLabel AND .sub_t = lbDisabled THEN .disabled = YES
   'Check only for tags conditions set to 'Never'
   IF .tag1 = 1 OR .tag2 = 1 THEN .disabled = YES
   'Make sure menu items don't disappear, or we can't edit them!
   '(This is NOT saved)
   .override_hide = menudata.edit_mode
  END WITH
 NEXT
END SUB

SUB update_detail_menu(detail as MenuDef, menudata as MenuDef, mi as MenuDefItem)
 DIM i as integer
 DIM cap as string
 DIM index as integer
 DeleteMenuItems detail

 ' Set .sub_t of each menu item to indicate what menu_editor_detail_keys should do

 append_menu_item detail, "Go Back", , 0
 
 cap = mi.caption
 IF LEN(cap) = 0 THEN cap = "[DEFAULT]"
 append_menu_item detail, "Caption: " & cap, , 1
 
 append_menu_item detail, "Type", , 2
 WITH *detail.last
  SELECT CASE mi.t
   CASE mtypeLabel
    .caption = "Type: " & mi.t & " Label"
   CASE mtypeSpecial
    .caption = "Type: " & mi.t & " Special screen"
   CASE mtypeMenu
    .caption = "Type: " & mi.t & " Go to Menu"
   CASE mtypeTextBox
    .caption = "Type: " & mi.t & " Show text box"
   CASE mtypeScript
    .caption = "Type: " & mi.t & " Run script"
  END SELECT
 END WITH
 
 append_menu_item detail, "Subtype: " & mi.sub_t, , 3
 WITH *detail.last
  SELECT CASE mi.t
   CASE mtypeLabel
    SELECT CASE mi.sub_t
     CASE lbSelectable:   .caption &= " Selectable"
     CASE lbDisabled:     .caption &= " Disabled"
     CASE lbUnselectable: .caption &= " Can't be selected"
    END SELECT
   CASE mtypeSpecial
    .caption = .caption & " " & get_special_menu_caption(mi.sub_t)
   CASE mtypeMenu
    .caption = .caption & " " & getmenuname(mi.sub_t)
   CASE mtypeScript
    .caption = "Subtype: " & scriptname(mi.sub_t)
    IF mi.sub_t THEN
     ' Indicate which script arguments are passed
     ' (It's safe to call append_menu_item, because WITH saves a reference to *detail.last)
     DIM argsinfo as string = "Args: "
     IF menudata.allow_gameplay THEN
      '0 is passed instead of the menu item handle if it would be invalid
      argsinfo &= IIF(mi.close_when_activated, "0, ", "handle, ")
     ELSE
      'Sadly, for back-compatibility, leave out the handle instead of passing zero.
     END IF
     argsinfo &= "extra0, extra1, extra2"
     append_menu_item detail, argsinfo, , -1   'sub_t: does nothing
     detail.last->disabled = YES
     detail.last->unselectable = YES  'Does nothing, yet
    END IF
   CASE ELSE
    .caption = "Subtype: " & mi.sub_t
  END SELECT
  .caption &= get_menu_item_editing_annotation(mi)
 END WITH

 append_menu_item detail, "Color: " & slice_color_caption(mi.col, "Default"), , 12
 append_menu_item detail, "Disabled color: " & slice_color_caption(mi.disabled_col, "Default"), , 13

 append_menu_item detail, tag_condition_caption(mi.tag1, "Enable if tag", "Always"), , 4
 append_menu_item detail, tag_condition_caption(mi.tag2, " and also tag", "Always"), , 5
 IF menu_item_is_activatable(mi) THEN
  append_menu_item detail, tag_set_caption(mi.settag, "Set tag"), , 6
  append_menu_item detail, tag_toggle_caption(mi.togtag), , 7
 END IF
 append_menu_item detail, "Edit Bitsets...", , 8
 FOR i = 0 TO 2
  append_menu_item detail, "Extra data " & i & ": " & mi.extra(i), , 9 + i
 NEXT i
END SUB


SUB edit_menu_bits (menu as MenuDef)
 DIM bitname(10) as string
 DIM bits(0) as integer
 
 bitname(0) = "Translucent box"
 bitname(1) = "Never show scrollbar"
 bitname(2) = "Allow gameplay & scripts"
 bitname(3) = "Suspend player even if gameplay allowed"
 bitname(4) = "No box"
 bitname(5) = "Disable player closing menu"
 bitname(6) = "Disable player control of menu"
 bitname(7) = "Prevent main menu activation"
 bitname(8) = "Advance text box when menu closes"
 bitname(9) = "Highlight selection background"
 bitname(10) = "Remember selection when reopening"

 MenuBitsToArray menu, bits()
 editbitset bits(), 0, bitname(), "menu_editor_bitsets"
 MenuBitsFromArray menu, bits()  
END SUB

SUB edit_menu_item_bits (mi as MenuDefItem)
 DIM bitname(2) as string
 DIM bits(0) as integer

 bitname(0) = "Hide if disabled"
 IF menu_item_is_activatable(mi) THEN
  bitname(1) = "Close menu when activated"
 END IF
 bitname(2) = "Don't run on-close script"

 MenuItemBitsToArray mi, bits()
 editbitset bits(), 0, bitname(), "menu_editor_item_bitsets"
 MenuItemBitsFromArray mi, bits()  
END SUB

SUB reposition_menu (menu as MenuDef, mstate as MenuState, viewport_page as integer)
 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "reposition_menu"

  DIM speed as integer = IIF(keyval(scShift) > 0, 10, 1)
  WITH menu.offset
   IF slowkey(ccUp, 100)  THEN .y -= speed
   IF slowkey(ccDown, 100)  THEN .y += speed
   IF slowkey(ccLeft, 100)  THEN .x -= speed
   IF slowkey(ccRight, 100)  THEN .x += speed
  END WITH

  clearpage dpage
  preview_menu menu, mstate, viewport_page, dpage
  edgeprint "Offset=" & menu.offset, 0, 0, uilook(uiMenuItem), dpage
  edgeprint "Arrows to re-position, SHIFT for speed, ESC to exit", 0, pBottom, uilook(uiMenuItem), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

'Draw the menu, to the bottom-right corner of the screen onto a background rect of the size of the game window
SUB preview_menu(menu as MenuDef, mstate as MenuState, viewport_page as integer, destpage as integer = -1)
 draw_textured_background viewport_page
 draw_menu menu, mstate, viewport_page
 IF destpage > -1 THEN draw_viewport_page viewport_page, destpage
END SUB

SUB reposition_anchor (menu as MenuDef, mstate as MenuState, viewport_page as integer)
 DIM tog as integer = 0
 DIM x as integer
 DIM y as integer
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
 
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "reposition_anchor"
  
  WITH menu
   IF keyval(ccUp) > 1 THEN .anchorvert = bound(.anchorvert - 1, alignTop, alignBottom)
   IF keyval(ccDown) > 1 THEN .anchorvert = bound(.anchorvert + 1, alignTop, alignBottom)
   IF keyval(ccLeft) > 1 THEN .anchorhoriz = bound(.anchorhoriz - 1, alignLeft, alignRight)
   IF keyval(ccRight) > 1 THEN .anchorhoriz = bound(.anchorhoriz + 1, alignLeft, alignRight)
  END WITH
 
  clearpage dpage
  preview_menu menu, mstate, viewport_page
  WITH menu
   x = .rect.x - 2 + anchor_point(.anchorhoriz, .rect.wide)
   y = .rect.y - 2 + anchor_point(.anchorvert, .rect.high)
   rectangle x, y, 5, 5, 2 + tog, viewport_page
  END WITH
  draw_viewport_page viewport_page, dpage

  edgeprint "Arrows to re-position, ESC to exit", 0, pBottom, uilook(uiMenuItem), dpage
  
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB
