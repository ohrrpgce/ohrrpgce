'OHRRPGCE CUSTOM - Textbox editor
'(C) Copyright 1997-2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#include "config.bi"
#include "udts.bi"
#include "custom_udts.bi"
#include "const.bi"
#include "common.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "loading.bi"
#include "scrconst.bi"
#include "custom.bi"
#include "thingbrowser.bi"

'--Local subs and functions
DECLARE FUNCTION textbox_condition_caption(tag as integer, prefix as string = "") as string
DECLARE FUNCTION textbox_condition_short_caption(tag as integer) as string
DECLARE SUB write_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer, num as integer)
DECLARE FUNCTION read_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer) as integer
DECLARE FUNCTION box_conditional_type_by_menu_index(menuindex as integer) as integer
DECLARE SUB update_textbox_editor_main_menu (byref box as TextBox, menu() as string)
DECLARE SUB textbox_edit_load (byref box as TextBox, byref st as TextboxEditState, menu() as string)
DECLARE SUB textbox_edit_preview (byref box as TextBox, byref st as TextboxEditState, page as integer, override_y as integer=-1, suppress_text as bool=NO)
DECLARE SUB textbox_draw_with_background(byref box as TextBox, byref st as TextboxEditState, backdrop as Frame ptr, page as integer)
DECLARE SUB textbox_appearance_editor (byref box as TextBox, byref st as TextboxEditState, parent_menu() as string)
DECLARE SUB update_textbox_appearance_editor_menu (byref menu as SimpleMenuItem vector, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_position_portrait (byref box as TextBox, byref st as TextboxEditState, backdrop as Frame ptr)
DECLARE SUB textbox_seek(byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_create_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_create_from_box_and_load (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState, menu() as string)
DECLARE SUB textbox_link_to_new_box_and_load (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState, menu() as string)
DECLARE SUB textbox_line_editor (byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_set_after_textbox (byref box as TextBox, after_textbox as integer)
DECLARE SUB textbox_copy_style_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_connections(byref box as TextBox, byref st as TextboxEditState, menu() as string)
DECLARE SUB textbox_connection_captions(byref node as TextboxConnectNode, id as integer, tag as integer, box as TextBox, topcation as string, use_tag as bool = YES)
DECLARE SUB textbox_connection_draw_node(byref node as TextboxConnectNode, x as integer, y as integer, selected as integer)
DECLARE SUB textbox_choice_editor (byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_conditionals(byref box as TextBox)
DECLARE SUB textbox_update_conditional_menu(byref box as TextBox, menu() as string)
DECLARE FUNCTION textbox_conditional_textbox_or_script_picker(num as integer, state as MenuState) as integer
DECLARE FUNCTION textbox_conditional_hero_picker(byval num as integer, state as MenuState) as integer
DECLARE SUB textbox_edit_importer ()
DECLARE SUB textbox_edit_exporter ()
DECLARE SUB import_textboxes_warn (byref warn as string, s as string)
DECLARE FUNCTION export_textboxes (filename as string, metadata() as bool) as bool
DECLARE FUNCTION import_textboxes (filename as string, byref warn as string) as bool



'These are used in the TextBox conditional editor
CONST condEXIT       = -1
CONST condTAG        = 0
CONST condBATTLE     = 1
CONST condSHOP       = 2
CONST condHERO       = 3
CONST condGAMEDELETE = 4
CONST condGAMESAVE   = 5
CONST condGAMELOAD   = 6
CONST condMONEY      = 7
CONST condDOOR       = 8
CONST condITEM       = 9
CONST condBOXorSCRIPT = 10
CONST condMENU       = 11
CONST condSETTAG     = 12

DIM SHARED browse_default as string


SUB textbox_editor_main ()
 DIM b as TextboxBrowser
 b.browse(-1, , @text_box_editor)
END SUB

FUNCTION textbox_picker (recindex as integer = -1) as integer
 DIM b as TextboxBrowser
 RETURN b.browse(recindex, , @text_box_editor, NO)
END FUNCTION

'skip_zero: textbox 0 is replaced with the None option, rather than use -1 for None
FUNCTION textbox_picker_or_none (recindex as integer = -1, skip_zero as bool = NO) as integer
 DIM offset as integer = IIF(skip_zero, 0, 1)
 DIM b as TextboxBrowser
 RETURN b.browse(recindex - offset, YES, @text_box_editor, NO, skip_zero) + offset
END FUNCTION

'whichbox is the box to edit, -1 for default, or past last textbox to add a new
'one. Returns -1 if cancelled add-new, or else last box edited.
'(See also FnEditor)
FUNCTION text_box_editor(whichbox as integer = -1) as integer
 DIM box as TextBox
 DIM st as TextboxEditState
 WITH st
  .search = ""
 END WITH

 'Set st.id: textbox to edit
 STATIC remember_box_id as integer = 1
 IF whichbox <= -1 THEN
  st.id = small(remember_box_id, gen(genMaxTextBox))
 ELSE
  st.id = small(whichbox, gen(genMaxTextBox) + 1)
  IF st.id > gen(genMaxTextBox) THEN
   IF yesno("Add new text box?") THEN
    gen(genMaxTextBox) = st.id
    textbox_create_from_box 0, box, st
   ELSE
    RETURN -1
   END IF
  END IF
 END IF

 DIM menu(10) as string
 menu(0) = "Return to Previous Menu"
 menu(1) = "Text Box"
 menu(2) = "Edit Text"
 menu(3) = "Edit Conditionals"
 menu(4) = "Edit Choices"
 menu(5) = "Box Appearance & Sounds"
 menu(6) = "After:"
 menu(7) = "Text Search:"
 menu(8) = "Connected Boxes..."
 menu(9) = "Export text boxes..."
 menu(10) = "Import text boxes..."

 DIM state as MenuState  'State of the toplevel menu
 state.pt = 1
 state.last = UBOUND(menu)
 state.size = 24
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 menuopts.itemspacing = -1

 DIM style_clip as integer = 0

 textbox_edit_load box, st, menu()
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_main"
  IF cropafter_keycombo(state.pt = 1) THEN
   cropafter st.id, gen(genMaxTextBox), 0, game & ".say", curbinsize(binSAY)
   textbox_edit_load box, st, menu()
  END IF
  usemenu state

  '--Editing
  '--NOTE: On this top-level menu, the textbox must be saved immediately after
  '--any data changes (e.g. after a submenu) so we can assume no unsaved changes.

  SELECT CASE state.pt
   CASE 7'textsearch
    strgrabber st.search, 36
   CASE 6'quickchainer
    IF keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1 THEN
     IF yesno("Create and link to new a textbox like this one?") THEN
      textbox_link_to_new_box_and_load st.id, box, st, menu()
     END IF
    ELSEIF keyval(scInsert) > 1 ANDALSO yesno("Create and link to a new textbox?") THEN
     textbox_link_to_new_box_and_load 0,     box, st, menu()
    ELSEIF keyval(scAlt) = 0 THEN  'Ignore alt+left/right keypresses
     ' Ctrl+Left/Right links to previous/next box. We actually let scrintgrabber
     ' handle that, by starting at box.after. So continuing to press Ctrl+Left/Right works.
     IF keyval(scCtrl) > 0 AND box.after = 0 THEN
      IF keyval(ccLeft) > 1 OR keyval(ccRight) > 1 THEN box.after = st.id
     END IF
     IF scrintgrabber(box.after, 0, gen(genMaxTextbox), ccLeft, ccRight, -1, plottrigger) THEN
      textbox_set_after_textbox box, box.after
      SaveTextBox box, st.id
      update_textbox_editor_main_menu box, menu()
     END IF
    END IF
   CASE ELSE '--not using the quick textbox chainer nor the search
    IF keyval(scAlt) > 0 AND keyval(scC) > 1 THEN style_clip = st.id
    IF keyval(scAlt) > 0 AND keyval(scV) > 1 THEN
     IF yesno("Copy box " & style_clip & "'s style to this box?") THEN
      textbox_copy_style_from_box style_clip, box, st
      SaveTextBox box, st.id
      textbox_edit_load box, st, menu()
     END IF
    END IF
    IF (keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1) AND gen(genMaxTextBox) < maxMaxTextbox THEN
     IF yesno("Create a textbox like this one?") THEN
      textbox_create_from_box_and_load st.id, box, st, menu()
     END IF
    END IF
  END SELECT

  'Navigate textboxes
  'Alt+left/right is only needed when the quickchainer ('After') is selected
  IF (state.pt <> 6 ANDALSO state.pt <> 7) ORELSE keyval(scAlt) > 0 THEN  'not quickchainer
   IF intgrabber_with_addset(st.id, 0, gen(genMaxTextBox), maxMaxTextbox, "text box") THEN
    IF st.id > gen(genMaxTextBox) THEN
     gen(genMaxTextBox) = st.id
     textbox_create_from_box 0, box, st
    END IF
    textbox_edit_load box, st, menu()
   END IF
  END IF

  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 1 THEN EXIT DO
   IF state.pt = 2 THEN textbox_line_editor box, st
   IF state.pt = 3 THEN
    textbox_conditionals box
    update_textbox_editor_main_menu box, menu()
   END IF
   IF state.pt = 4 THEN textbox_choice_editor box, st
   IF state.pt = 5 THEN
    textbox_appearance_editor box, st, menu()
    '--re-update the menu after the appearance editor in case we switched records
    update_textbox_editor_main_menu box, menu()
   END IF
   IF state.pt = 6 THEN
    DIM want_scriptbrowse as bool = NO
    IF box.after > 0 THEN
     '--Go to Next textbox
     st.id = box.after
     textbox_edit_load box, st, menu()
    ELSEIF box.after < 0 THEN
     want_scriptbrowse = YES
    ELSE
     DIM choices(...) as string = { _
      "A new text box with default style", "A new text box with this style", "A script" _
     }
     DIM choice as integer = multichoice("Link to what after this text box?", choices())
     IF choice = 0 THEN textbox_link_to_new_box_and_load 0,     box, st, menu()
     IF choice = 1 THEN textbox_link_to_new_box_and_load st.id, box, st, menu()
     IF choice = 2 THEN want_scriptbrowse = YES
    END IF
    IF want_scriptbrowse THEN
     DIM temptrig as integer = ABS(box.after)
     scriptbrowse temptrig, plottrigger, "textbox plotscript"
     textbox_set_after_textbox box, -temptrig
     update_textbox_editor_main_menu box, menu()
    END IF
   END IF
   IF state.pt = 7 AND keyval(scEnter) > 1 THEN
    textbox_seek box, st
    textbox_edit_load box, st, menu()
   END IF
   IF state.pt = 8 THEN
    textbox_connections box, st, menu()
   END IF
   IF state.pt = 9 THEN '--Export textboxes to a .TXT file
    textbox_edit_exporter
   END IF
   IF state.pt = 10 THEN '-- Import text boxes from a .TXT file
    textbox_edit_importer
    LoadTextBox box, st.id
   END IF

   '--Save box after visiting any submenu
   SaveTextBox box, st.id
  END IF '--end of enter_space_click

  '--Draw screen
  IF st.id = 0 THEN
    menu(1) = CHR(27) & "Text Box 0 [template]" & CHR(26)
  ELSE
    menu(1) = CHR(27) & "Text Box " & st.id & CHR(26)
  END IF

  menu(7) = "Text Search:"
  IF state.pt = 7 THEN menu(7) &= st.search
 
  '--Draw box
  clearpage dpage
  textbox_edit_preview box, st, dpage, 96

  textcolor uilook(uiText), uilook(uiHighlight)
  printstr "+ to copy", 248, 0, dpage
  printstr "ALT+C copy style", 192, 8, dpage
  IF style_clip > 0 THEN printstr "ALT+V paste style", 184, 16, dpage
  IF state.pt = 6 THEN
   DIM tooltip as string
   IF box.after THEN
    tooltip = textbox_condition_short_caption(box.after_tag)
   ELSE
    tooltip = "+/INSERT/ENTER/Ctrl" & CHR(27,ASC("/"),26) & ": link to textbox"
   END IF
   edgeprint tooltip, 0, pBottom, uilook(uiDisabledItem), dpage
  END IF
  standardmenu menu(), state, 0, 0, dpage, menuopts

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 unload_sprite_and_pal st.portrait
 remember_box_id = st.id
 RETURN st.id
END FUNCTION


'========================= Textbox Conditionals Editor ========================


SUB textbox_conditionals(byref box as TextBox)
 DIM menu(-1 TO 26) as string
 
 DIM state as MenuState
 state.top = -1
 state.pt = 0
 state.first = -1
 state.last = 26
 state.spacing = 9  'Necessary because not using standardmenu
 state.autosize = YES

 DIM grey(-1 TO 26) as integer
 'This array tells which rows in the conditional editor are grey
 grey(-1) = NO
 grey(0) = YES
 grey(1) = NO
 grey(2) = YES
 grey(3) = NO
 grey(4) = NO
 grey(5) = YES
 grey(6) = NO
 grey(7) = YES
 grey(8) = NO
 grey(9) = YES
 grey(10) = NO
 grey(11) = YES
 grey(12) = NO
 grey(13) = YES
 grey(14) = NO
 grey(15) = NO
 grey(16) = NO
 grey(17) = YES
 grey(18) = NO
 grey(19) = YES
 grey(20) = NO
 grey(21) = YES
 grey(22) = NO
 grey(23) = NO
 grey(24) = NO
 grey(25) = YES 'Menu Tag
 grey(26) = NO

 DIM num as integer
 DIM c as integer

 textbox_update_conditional_menu box, menu()
 init_menu_state state, menu()
 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_conditions"
  usemenu state
  IF keyval(scDelete) > 1 THEN ' Pressed the delete key
   write_box_conditional_by_menu_index box, state.pt, 0
   textbox_update_conditional_menu box, menu()
  END IF

  num = read_box_conditional_by_menu_index(box, state.pt)
  SELECT CASE box_conditional_type_by_menu_index(state.pt)
   CASE condEXIT
    IF enter_space_click(state) THEN EXIT DO
   CASE condTAG
    tag_grabber num, state, , YES  'always_choice=YES
   CASE condSETTAG
    tag_set_grabber num, state
   CASE condBATTLE
    intgrabber num, 0, gen(genMaxFormation)
   CASE condSHOP
    xintgrabber num, 0, gen(genMaxShop), -1, -32000  'Negative values are Inn cost
    IF enter_space_click(state) THEN
     num = shop_picker_or_none(num)
    END IF
   CASE condHERO
    'From -gen(genMaxHero) - 1 to  gen(genMaxHero) + 1
    xintgrabber num, 0, gen(genMaxHero), 0, -gen(genMaxHero)
    IF enter_space_click(state) THEN
     num = textbox_conditional_hero_picker(num, state)
    END IF
   CASE condGAMEDELETE
    'Don't cap to gen(genSaveSlotCount): should be able to access hidden slots, just like script commands
    intgrabber num, -1, maxSaveSlotCount
   CASE condGAMESAVE
    intgrabber num, -2, maxSaveSlotCount
   CASE condGAMELOAD
    intgrabber num, -3, maxSaveSlotCount
   CASE condMONEY
    intgrabber num, -32000, 32000
   CASE condDOOR
    intgrabber num, 0, maxDoorsPerMap
   CASE condITEM
    xintgrabber num, 0, gen(genMaxItem), 0, -gen(genMaxItem)
    IF enter_space_click(state) THEN
     DIM orig_num as integer = num
     DIM add_or_del as integer = SGN(num)
     num = item_picker_or_none(abs(num))
     IF num > 0 THEN
      DIM add_or_del_choices(1) as string = {"Add " & readitemname(num - 1), "Remove " & readitemname(num - 1)} 
      SELECT CASE multichoice("Add or remove item?", add_or_del_choices(), IIF(add_or_del = -1, 1, 0), -1)
       CASE 1
        num = num * -1
       CASE -1
        'Cancel
        num = orig_num
      END SELECT
     END IF
    END IF
   CASE condBOXorSCRIPT
    scrintgrabber num, 0, gen(genMaxTextbox), ccLeft, ccRight, -1, plottrigger
    IF enter_space_click(state) THEN
     num = textbox_conditional_textbox_or_script_picker(num, state)
    END IF
   CASE condMENU
    intgrabber num, 0, gen(genMaxMenu)
  END SELECT
  IF num <> read_box_conditional_by_menu_index(box, state.pt) THEN
   'The value has changed
   write_box_conditional_by_menu_index(box, state.pt, num)
   textbox_update_conditional_menu box, menu()
  END IF

  clearpage dpage
  FOR i as integer = state.top TO state.top + state.size
   IF i > state.last THEN CONTINUE FOR
   DIM drawy as integer = (i - state.top) * state.spacing
   textcolor uilook(uiMenuItem), 0
   IF grey(i) = YES THEN
    c = uilook(uiSelectedDisabled)
    SELECT CASE read_box_conditional_by_menu_index(box, i)
     CASE -1 ' Check tag 1=OFF always true
      c = uilook(uiHighlight)
     CASE 0, 1 ' Disabled or check tag 1=ON never true
      c = uilook(uiDisabledItem)
    END SELECT
    rectangle 0, drawy, 312, state.spacing - 1, c, dpage
   ELSE
    'Display items that do nothing greyed out
    'FIXME: not correct: shop 0, formation 0, door 0 and menu 0 still do something!
    IF read_box_conditional_by_menu_index(box, i) = 0 THEN textcolor uilook(uiDisabledItem), 0
   END IF
   IF i = state.hover THEN textcolor uilook(uiMouseHoverItem), 0 
   IF i = state.pt THEN textcolor uilook(uiSelectedItem + state.tog), 0
   printstr menu(i), 0, drawy, dpage
  NEXT i
  draw_fullscreen_scrollbar state, , dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

FUNCTION textbox_conditional_textbox_or_script_picker(num as integer, state as MenuState) as integer
 DIM whichbrowser as integer = -1
 IF num > 0 THEN whichbrowser = 0
 IF num < 0 THEN whichbrowser = 1
 IF num = 0 THEN
  DIM choices(1) as string = {"Textbox", "Script"}
  whichbrowser = multichoice("Go to a textbox, or run a script?", choices())
 END IF
 IF whichbrowser = 0 THEN
  'num = textbox_picker_or_none(num, YES)
  'Don't allow reentering textbox editor
  DIM b as TextboxBrowser
  num = b.browse(num, YES, , NO, YES)  'skip_zero = YES
 ELSEIF whichbrowser = 1 THEN
  DIM temptrigger as integer = -num
  scriptbrowse temptrigger, plottrigger, IIF(state.pt = 1, "instead of", "after") & " textbox plotscript"
  num = -temptrigger
 END IF
 RETURN num
END FUNCTION

FUNCTION textbox_conditional_hero_picker(byval num as integer, state as MenuState) as integer
 DIM cur_hero as integer = ABS(num)
 DIM picked_hero as integer = hero_picker_or_none(cur_hero)
 IF picked_hero = -1 THEN
  RETURN num 'Cancelled
 END IF
 IF picked_hero = 0 THEN
  RETURN 0 'None
 END IF
 ' picked a hero, must clarify action
 DIM choices(1) as string
 SELECT CASE state.pt
  CASE 14: choices(0) = "Add" : choices(1) = "Remove"
  CASE 15: choices(0) = "Swap In" : choices(1) = "Swap Out"
  CASE 16: choices(0) = "Unlock" : choices(1) = "Lock"
 END SELECT
 DIM heroname as string = getheroname(picked_hero - 1)
 choices(0) &= " " & heroname
 choices(1) &= " " & heroname
 SELECT CASE multichoice("Which action?", choices())
  CASE 0: RETURN picked_hero
  CASE 1: RETURN picked_hero * -1
 END SELECT
 'No changes
 RETURN num
END FUNCTION

SUB textbox_update_conditional_menu(byref box as TextBox, menu() as string)
 menu(-1) = "Go Back"
 menu(0) = textbox_condition_caption(box.instead_tag, "INSTEAD")
 SELECT CASE box.instead
  CASE 0
   menu(1) = " use [text box or script] instead"
  CASE IS < 0
   menu(1) = " run " & scriptname(-box.instead) & " instead"
  CASE IS > 0
   menu(1) = " jump to text box " & box.instead & " instead"
 END SELECT
 menu(2) = textbox_condition_caption(box.settag_tag, "SETTAG")
 menu(3) = tag_set_caption(box.settag1, " set tag")
 menu(4) = tag_set_caption(box.settag2, " set tag")
 menu(5) = textbox_condition_caption(box.money_tag, "MONEY")
 IF box.money < 0 THEN
  menu(6) = " lose " & ABS(box.money) & "$"
 ELSE
  menu(6) = " gain " & ABS(box.money) & "$"
 END IF
 menu(7) = textbox_condition_caption(box.battle_tag, "BATTLE")
 menu(8) = " fight enemy formation " & box.battle
 menu(9) = textbox_condition_caption(box.item_tag, "ITEM")
 SELECT CASE box.item
  CASE 0 :      menu(10) = " do not add/remove items"
  CASE IS > 0 : menu(10) = " add one " & load_item_name(ABS(box.item), 0, 0)
  CASE IS < 0 : menu(10) = " remove one " & load_item_name(ABS(box.item), 0, 0)
 END SELECT
 menu(11) = textbox_condition_caption(box.shop_tag, "SHOP")
 SELECT CASE box.shop
  CASE IS > 0 : menu(12) = " go to shop " & box.shop - 1 & " " & readshopname(box.shop - 1)
  CASE IS < 0 : menu(12) = " go to an Inn that costs " & -box.shop & "$"
  CASE 0 :      menu(12) = " restore Hp and Mp [select shop here]"
 END SELECT
 menu(13) = textbox_condition_caption(box.hero_tag, "HEROES")
 SELECT CASE box.hero_addrem
  CASE 0 :      menu(14) = " do not add/remove heros"
  CASE IS > 0 : menu(14) = " add " & getheroname(ABS(box.hero_addrem) - 1) & " to party"
  CASE IS < 0 : menu(14) = " remove " & getheroname(ABS(box.hero_addrem) - 1) & " from party"
 END SELECT
 SELECT CASE box.hero_swap
  CASE 0 :      menu(15) = " do not swap in/out heros"
  CASE IS > 0 : menu(15) = " swap in " & getheroname(ABS(box.hero_swap) - 1)
  CASE IS < 0 : menu(15) = " swap out " & getheroname(ABS(box.hero_swap) - 1)
 END SELECT
 SELECT CASE box.hero_lock
  CASE 0 :      menu(16) = " do not unlock/lock heros"
  CASE IS > 0 : menu(16) = " unlock " & getheroname(ABS(box.hero_lock) - 1)
  CASE IS < 0 : menu(16) = " lock " & getheroname(ABS(box.hero_lock) - 1)
 END SELECT
 menu(17) = textbox_condition_caption(box.door_tag, "DOOR")
 menu(18) = " instantly use door " & box.door
 menu(19) = textbox_condition_caption(box.menu_tag, "MENU")
 menu(20) = " open menu " & box.menu & " " & getmenuname(box.menu)
 menu(21) = textbox_condition_caption(box.game_tag, "GAME")
 SELECT CASE box.game_delete
  CASE 0 :      menu(22) = " don't delete save slot"
  CASE -1 :     menu(22) = " delete active save slot"
  CASE IS > 0 : menu(22) = " delete save slot " & box.game_delete
 END SELECT
 SELECT CASE box.game_save
  CASE 0 :      menu(23) = " don't save game"
  CASE -1 :     menu(23) = " save to active slot"
  CASE -2 :     menu(23) = " show save menu"
  CASE IS > 0 : menu(23) = " save to slot " & box.game_save
 END SELECT
 SELECT CASE box.game_load
  CASE 0 :      menu(24) = " don't load/end game"
  CASE -1 :     menu(24) = " load from active slot"
  CASE -2 :     menu(24) = " show load menu"
  CASE -3 :     menu(24) = " game over"
  CASE IS > 0 : menu(24) = " load from slot " & box.game_load
 END SELECT
 menu(25) = textbox_condition_caption(box.after_tag, "AFTER")
 SELECT CASE box.after
  CASE 0 :      menu(26) = " use [text box or script] next"
  CASE IS < 0 : menu(26) = " run " & scriptname(-box.after) & " next"
  CASE IS > 0 : menu(26) = " jump to text box " & box.after & " next"
 END SELECT
END SUB


'========================== Textbox Display/Preview ===========================


' Draw the textbox without backdrop, optionally without text or at other y position.
SUB textbox_edit_preview (byref box as TextBox, byref st as TextboxEditState, page as integer, override_y as integer=-1, suppress_text as bool=NO)
 DIM ypos as integer
 IF override_y >= 0 THEN
  ypos = override_y
 ELSE
  ypos = 4 + box.vertical_offset * 4
 END IF
 IF box.no_box = NO THEN
  edgeboxstyle 4, ypos, 312, get_text_box_height(box), box.boxstyle, page, (box.opaque = NO)
 END IF
 IF suppress_text = NO THEN
  DIM col as integer
  col = IIF(box.textcolor > 0, box.textcolor, uilook(uiText))
  FOR i as integer = 0 TO UBOUND(box.text)
   edgeprint box.text(i), 8, 3 + ypos + i * 10, col, page
  NEXT i
 END IF
 ' Don't draw box if portrait type is NONE
 IF box.portrait_box ANDALSO box.portrait_type <> portraitNONE THEN
  edgeboxstyle 4 + box.portrait_pos.x, ypos  + box.portrait_pos.y, 50, 50, box.boxstyle, page, YES
 END IF
 WITH st.portrait
  IF .sprite THEN frame_draw .sprite, .pal, 4 + box.portrait_pos.x, ypos + box.portrait_pos.y, , , page
 END WITH
END SUB

' Preview the textbox as it will appear in-game, portraying it with in-game window size
SUB textbox_draw_with_background(byref box as TextBox, byref st as TextboxEditState, backdrop as Frame ptr, page as integer)
 clearpage page
 draw_background vpages(page), uilook(uiBackground)

 ' Draw the textbox
 DIM fr as Frame ptr = vpages(page)
 DIM viewport as Frame ptr
 viewport = frame_new_view(fr, large(0, fr->w - gen(genResolutionX)), large(0, fr->h - gen(genResolutionY)), gen(genResolutionX), gen(genResolutionY))
 draw_background viewport, bgChequer
 fuzzyrect viewport, 0, 0, , , uilook(uiBackground), 50  'Make the chequer less glaring underneath text
 IF backdrop THEN frame_draw backdrop, , 0, 0, , box.backdrop_trans, viewport
 DIM viewport_page as integer = registerpage(viewport)
 textbox_edit_preview box, st, viewport_page
 freepage viewport_page
 frame_unload @viewport
END SUB

SUB textbox_edit_load (byref box as TextBox, byref st as TextboxEditState, menu() as string)
 LoadTextBox box, st.id
 update_textbox_editor_main_menu box, menu()
 load_text_box_portrait box, st.portrait
END SUB

'Set the next textbox, and auto-adjust the 'After' condition tag if appropriate.
SUB textbox_set_after_textbox (byref box as TextBox, after_textbox as integer)
 box.after = after_textbox
 IF box.after = 0 THEN
  box.after_tag = 0
 ELSE
  IF box.after_tag = 0 THEN box.after_tag = -1 ' Set "After" text box conditional to "Always"
 END IF
END SUB

SUB update_textbox_editor_main_menu (byref box as TextBox, menu() as string)
 SELECT CASE box.after_tag
  CASE 0
   menu(6) = "After: None Selected..."
  CASE -1
   IF box.after >= 0 THEN
    menu(6) = "After: Box " & box.after
   ELSE
    menu(6) = "After: script " & scriptname(ABS(box.after))
   END IF
  CASE ELSE
   IF box.after >= 0 THEN
    menu(6) = "After: Box " & box.after & " (conditional)"
   ELSE
    menu(6) = "After: script " & scriptname(ABS(box.after)) & " (conditional)"
   END IF
 END SELECT
END SUB


'==============================================================================


'One of the tag conditional headers in the Conditionals menu.
FUNCTION textbox_condition_caption(tag as integer, prefix as string = "") as string
 DIM prefix2 as string
 IF LEN(prefix) > 0 THEN prefix2 = prefix & ": "
 IF tag = 0 THEN RETURN prefix2 & "Never do the following"
 IF tag = 1 THEN RETURN prefix2 & "If tag 1 = ON [Never]"
 IF tag = -1 THEN RETURN prefix2 & "Always do the following"
 RETURN prefix2 & "If tag " & ABS(tag) & " = " + onoroff(tag) & " (" & load_tag_name(tag) & ")"
END FUNCTION

'Note that this is similar to tag_condition_caption and describe_tag_condition. Ugh!
FUNCTION textbox_condition_short_caption(tag as integer) as string
 IF tag = 0 THEN RETURN "NEVER"
 IF tag = 1 THEN RETURN "NEVER"
 IF tag = -1 THEN RETURN "ALWAYS"
 RETURN "IF TAG " & ABS(tag) & "=" + onoroff(tag)
END FUNCTION

SUB write_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer, num as integer)
 WITH box
  SELECT CASE menuindex
   CASE 0:  .instead_tag = num
   CASE 1:  .instead     = num
   CASE 2:  .settag_tag  = num
   CASE 3:  .settag1     = num
   CASE 4:  .settag2     = num
   CASE 5:  .money_tag   = num
   CASE 6:  .money       = num
   CASE 7:  .battle_tag  = num
   CASE 8:  .battle      = num
   CASE 9:  .item_tag    = num
   CASE 10: .item        = num
   CASE 11: .shop_tag    = num
   CASE 12: .shop        = num
   CASE 13: .hero_tag    = num
   CASE 14: .hero_addrem = num
   CASE 15: .hero_swap   = num
   CASE 16: .hero_lock   = num
   CASE 17: .door_tag    = num
   CASE 18: .door        = num
   CASE 19: .menu_tag    = num
   CASE 20: .menu        = num
   CASE 21: .game_tag    = num
   CASE 22: .game_delete = num
   CASE 23: .game_save   = num
   CASE 24: .game_load   = num
   CASE 25: .after_tag   = num
   CASE 26: .after       = num
  END SELECT
 END WITH
END SUB

FUNCTION read_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer) as integer
 WITH box
  SELECT CASE menuindex
   CASE 0:  RETURN .instead_tag
   CASE 1:  RETURN .instead
   CASE 2:  RETURN .settag_tag
   CASE 3:  RETURN .settag1
   CASE 4:  RETURN .settag2
   CASE 5:  RETURN .money_tag
   CASE 6:  RETURN .money
   CASE 7:  RETURN .battle_tag
   CASE 8:  RETURN .battle
   CASE 9:  RETURN .item_tag
   CASE 10: RETURN .item
   CASE 11: RETURN .shop_tag
   CASE 12: RETURN .shop
   CASE 13: RETURN .hero_tag
   CASE 14: RETURN .hero_addrem
   CASE 15: RETURN .hero_swap
   CASE 16: RETURN .hero_lock
   CASE 17: RETURN .door_tag
   CASE 18: RETURN .door
   CASE 19: RETURN .menu_tag
   CASE 20: RETURN .menu
   CASE 21: RETURN .game_tag
   CASE 22: RETURN .game_delete
   CASE 23: RETURN .game_save
   CASE 24: RETURN .game_load
   CASE 25: RETURN .after_tag
   CASE 26: RETURN .after
  END SELECT
 END WITH
END FUNCTION

FUNCTION box_conditional_type_by_menu_index(menuindex as integer) as integer
 SELECT CASE menuindex
  CASE -1      : RETURN condEXIT
  CASE 1, 26   : RETURN condBOXorSCRIPT
  CASE 3, 4    : RETURN condSETTAG
  CASE 6       : RETURN condMONEY
  CASE 8       : RETURN condBATTLE
  CASE 10      : RETURN condITEM
  CASE 12      : RETURN condSHOP
  CASE 14,15,16: RETURN condHERO
  CASE 18      : RETURN condDOOR
  CASE 20      : RETURN condMENU
  CASE 22      : RETURN condGAMEDELETE
  CASE 23      : RETURN condGAMESAVE
  CASE 24      : RETURN condGAMELOAD
  CASE ELSE    : RETURN condTAG
 END SELECT
END FUNCTION


'============================== Appearance Editor =============================


SUB textbox_position_portrait (byref box as TextBox, byref st as TextboxEditState, backdrop as Frame ptr)
 DIM tog as integer = 0
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_position_portrait"
  IF enter_or_space() THEN EXIT DO
  DIM as integer speed = IIF(keyval(scShift), 10, 1)
  DIM as integer delay = IIF(keyval(scShift), 55, 90)
  IF slowkey(ccLeft, delay)  THEN box.portrait_pos.x -= speed
  IF slowkey(ccRight, delay) THEN box.portrait_pos.x += speed
  IF slowkey(ccUp, delay)    THEN box.portrait_pos.y -= speed
  IF slowkey(ccDown, delay)  THEN box.portrait_pos.y += speed

  textbox_draw_with_background box, st, backdrop, dpage
  edgeprint "Arrow keys to move, space to confirm", 0, 0, uilook(uiSelectedItem + tog), dpage
  edgeprint "Offset " & box.portrait_pos, pLeft, pBottom, uilook(uiText), dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB textbox_appearance_editor (byref box as TextBox, byref st as TextboxEditState, parent_menu() as string)
 DIM menu as SimpleMenuItem vector
 update_textbox_appearance_editor_menu menu, box, st

 DIM state as MenuState
 init_menu_state state, cast(BasicMenuItem vector, menu)
 state.autosize = YES
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 menuopts.itemspacing = -1

 'Show backdrop
 DIM backdrop as Frame ptr
 IF box.backdrop > 0 THEN
  backdrop = frame_load(sprTypeBackdrop, box.backdrop - 1)
 END IF

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_appearance"
  usemenu state, cast(BasicMenuItem vector, menu)
  IF enter_space_click(state) THEN
   SELECT CASE menu[state.pt].dat
    CASE 0: EXIT DO ' Exit the appearance menu
    CASE 3: box.textcolor = color_browser_256(box.textcolor)
    CASE 5:
      DIM backdropb as BackdropSpriteBrowser
      box.backdrop = backdropb.browse(box.backdrop - 1, YES) + 1
      state.need_update = YES
      frame_unload @backdrop
      IF box.backdrop > 0 THEN
       backdrop = frame_load(sprTypeBackdrop, box.backdrop - 1)
      END IF
    CASE 6:
     box.music = song_picker_or_none(box.music)
     IF box.music > -1 THEN playsongnum box.music - 1
    CASE 7: box.no_box = (NOT box.no_box)
    CASE 8: box.opaque = (NOT box.opaque)
    CASE 9: box.restore_music = (NOT box.restore_music)
    CASE 16: box.stop_sound_after = (NOT box.stop_sound_after)
    CASE 18: box.backdrop_trans = (NOT box.backdrop_trans)
    CASE 11:
     IF box.portrait_type = portraitSPRITESET THEN
      DIM portraitb as PortraitSpriteBrowser
      box.portrait_id = portraitb.browse(box.portrait_id)
      state.need_update = YES
     END IF
    CASE 12:
     IF box.portrait_type = portraitSPRITESET THEN
      box.portrait_pal = pal16browse(box.portrait_pal, sprTypePortrait, box.portrait_id, YES)
     END IF
    CASE 13: box.portrait_box = (NOT box.portrait_box)
    CASE 14:
     IF box.portrait_type <> portraitNONE THEN  'If portrait type is NONE, then the portrait+box aren't visible
      textbox_position_portrait box, st, backdrop
     END IF
    CASE 15:
     box.sound_effect = sfx_picker_or_none(box.sound_effect)
    CASE 17:
     box.line_sound = sfx_picker_or_none(box.line_sound)
     IF box.line_sound = 0 ANDALSO gen(genTextboxLine) > 0 THEN
      playsfx gen(genTextboxLine) - 1
     END IF
   END SELECT
   state.need_update = YES
  END IF
  IF keyval(scAlt) = 0 THEN
   'Not holding ALT
   IF keyval(ccLeft) > 1 OR keyval(ccRight) > 1 THEN
    SELECT CASE menu[state.pt].dat
     CASE 7: box.no_box = (NOT box.no_box)
     CASE 8: box.opaque = (NOT box.opaque)
     CASE 9: box.restore_music = (NOT box.restore_music)
     CASE 13: box.portrait_box = (NOT box.portrait_box)
     CASE 16: box.stop_sound_after = (NOT box.stop_sound_after)
     CASE 18: box.backdrop_trans = (NOT box.backdrop_trans)
    END SELECT
    state.need_update = YES
   END IF
   SELECT CASE menu[state.pt].dat
    CASE 1: state.need_update OR= intgrabber(box.vertical_offset, 0, gen(genResolutionX) \ 4 - 1)
    CASE 2: state.need_update OR= intgrabber(box.shrink, -1, 21)
    CASE 3: state.need_update OR= intgrabber(box.textcolor, 0, 255)
    CASE 4: state.need_update OR= intgrabber(box.boxstyle, 0, 14)
    CASE 5:
     IF zintgrabber(box.backdrop, -1, gen(genNumBackdrops) - 1) THEN
      state.need_update = YES
      frame_unload @backdrop
      IF box.backdrop > 0 THEN
       backdrop = frame_load(sprTypeBackdrop, box.backdrop - 1)
      END IF
     END IF
    CASE 6:
     IF zintgrabber(box.music, -2, gen(genMaxSong)) THEN
      state.need_update = YES
      music_stop
     END IF
    CASE 10:
     state.need_update OR= intgrabber(box.portrait_type, 0, portraitLAST)
    CASE 11:
     SELECT CASE box.portrait_type
      CASE portraitSPRITESET: state.need_update OR= intgrabber(box.portrait_id, 0, gen(genMaxPortrait))
      CASE portraitPARTYRANK: state.need_update OR= intgrabber(box.portrait_id, 0, sizeActiveParty - 1)
      CASE portraitPARTYSLOT: state.need_update OR= intgrabber(box.portrait_id, 0, sizeParty - 1)
      CASE portraitHEROID:    state.need_update OR= intgrabber(box.portrait_id, 0, gen(genMaxHero))
     END SELECT
    CASE 12:
     IF box.portrait_type = portraitSPRITESET THEN
      state.need_update OR= intgrabber(box.portrait_pal, -1, gen(genMaxPal))
     END IF
    CASE 15:
     IF zintgrabber(box.sound_effect, -1, gen(genMaxSFX)) THEN
      state.need_update = YES
      resetsfx
     END IF
    CASE 17:
     IF zintgrabber(box.line_sound, -2, gen(genMaxSFX)) THEN
      state.need_update = YES
      resetsfx
     END IF
   END SELECT
  ELSE '-- holding ALT
   DIM remptr as integer = st.id
   IF intgrabber(st.id, 0, gen(genMaxTextBox)) THEN
    SaveTextBox box, remptr
    textbox_edit_load box, st, parent_menu()
    frame_unload @backdrop
    music_stop
    IF box.backdrop > 0 THEN
     backdrop = frame_load(sprTypeBackdrop, box.backdrop - 1)
    END IF
    state.need_update = YES
   END IF
  END IF
  IF state.need_update THEN
   state.need_update = NO
   update_textbox_appearance_editor_menu menu, box, st
   init_menu_state state, cast(BasicMenuItem vector, menu)
  END IF

  textbox_draw_with_background box, st, backdrop, dpage
  standardmenu cast(BasicMenuItem vector, menu), state, 0, 0, dpage, menuopts

  IF keyval(scAlt) > 0 THEN
   textcolor uilook(uiText), uilook(uiHighlight)
   printstr "Box " & st.id, pRight, 0, dpage
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 v_free menu
 frame_unload @backdrop
 resetsfx
 music_stop
END SUB

' Append a menu item; first argument is menu item type
PRIVATE SUB menuitem(itemdata as integer, byref menu as SimpleMenuItem vector, caption as zstring ptr, unselectable as bool = NO, col as integer = 0)
 IF itemdata = -1 THEN unselectable = YES : col = uilook(eduiHeading)
 append_simplemenu_item menu, caption, unselectable, col, itemdata
END SUB

STATIC SHARED portrait_type_names(portraitLAST) as zstring ptr = {_
    @"NONE", _
    @"Fixed", _
    @"Hero (by caterpillar order)", _
    @"Hero (by party order)", _
    @"Hero (by ID)" _
}

SUB update_textbox_appearance_editor_menu (byref menu as SimpleMenuItem vector, byref box as TextBox, byref st as TextboxEditState)
 DIM menutemp as string
 v_new menu
 menuitem 0, menu, "Go Back"
 menuitem 1, menu, "Position: " & box.vertical_offset
 menuitem 3, menu, "Text Color: " & box.textcolor

 menuitem -1, menu, ""
 menuitem -1, menu, " Box"
 menuitem 7, menu, "Show Box: " & yesorno(NOT box.no_box)
 IF box.no_box = NO THEN
  menuitem 8, menu, "Translucent: " & yesorno(NOT box.opaque)
  menuitem 4, menu, "Box Style: " & box.boxstyle
  menuitem 2, menu, "Shrink: " & IIF(box.shrink = -1, "Auto", STR(box.shrink))
 END IF

 menuitem -1, menu, ""
 menuitem -1, menu, " Backdrop"
 menuitem 5, menu, "Backdrop: " & IIF(box.backdrop, STR(box.backdrop - 1), "NONE")
 IF box.backdrop > 0 THEN
  menuitem 18, menu, "Transparent: " & yesorno(box.backdrop_trans)
 END IF

 menuitem -1, menu, ""
 menuitem -1, menu, " Portrait"
 menuitem 10, menu, "Type: " & safe_captionz(portrait_type_names(), box.portrait_type, "type")

 IF box.portrait_type <> portraitNONE THEN
  menutemp = STR(box.portrait_id)
  SELECT CASE box.portrait_type
   CASE portraitNONE: menutemp = "(N/A)"
   CASE portraitPARTYRANK: IF box.portrait_id = 0 THEN menutemp &= " (Leader)"
   CASE portraitPARTYSLOT: IF box.portrait_id > 3 THEN menutemp &= " (Reserve)"
   CASE portraitHEROID: menutemp &= " (" & getheroname(box.portrait_id) & ")"
  END SELECT
  menuitem 11, menu, "ID: " & menutemp
  menutemp = defaultint(box.portrait_pal)
  SELECT CASE box.portrait_type
   CASE portraitNONE: menutemp = "(N/A)"
   CASE portraitSPRITESET:
   CASE ELSE: menutemp &= " (N/A, see hero editor)"
  END SELECT
  menuitem 12, menu, "Palette: " & menutemp
  menuitem 13, menu, "Box: " & yesorno(box.portrait_box)
  menuitem 14, menu, "Position Portrait..."
 END IF

 menuitem -1, menu, ""
 menuitem -1, menu, " Audio"
 IF box.music < 0 THEN
  menutemp = "SILENCE"
 ELSEIF box.music = 0 THEN
  menutemp = "NONE"
 ELSE
  menutemp = (box.music - 1) & " " & getsongname(box.music - 1)
 END IF
 menuitem 6, menu, "Music: " & menutemp
 menuitem 9, menu, "Restore Map Music Afterwards: " & yesorno(box.restore_music)

 IF box.sound_effect = 0 THEN
  menutemp = "NONE"
 ELSE
  menutemp = (box.sound_effect - 1) & " " & getsfxname(box.sound_effect - 1)
 END IF
 menuitem 15, menu, "Sound Effect: " & menutemp
 IF box.sound_effect > 0 THEN
  menuitem 16, menu, "Stop Sound Afterwards: " & yesorno(box.stop_sound_after)
 END IF
 IF box.line_sound < 0 THEN
  menutemp = "NONE"
 ELSEIF box.line_sound = 0 THEN
  IF gen(genTextboxLine) <= 0 THEN
   menutemp = "default (NONE)"
  ELSE
   menutemp = "default (" & (gen(genTextboxLine) - 1) & " " & getsfxname(gen(genTextboxLine) - 1) & ")"
  END IF
 ELSE
  menutemp = (box.line_sound - 1) & " " & getsfxname(box.line_sound - 1)
 END IF
 menuitem 17, menu, "Line Sound: " & menutemp

 load_text_box_portrait box, st.portrait
END SUB

SUB textbox_seek(byref box as TextBox, byref st as TextboxEditState)
 DIM remember_id as integer = st.id
 st.id += 1
 DIM foundstr as bool = NO
 DO
  IF st.id > gen(genMaxTextBox) THEN st.id = 0
  IF st.id = remember_id THEN
   edgeboxstyle 115, 90, 100, 20, 0, vpage
   edgeprint "Not found.", 120, 95, uilook(uiText), vpage
   setvispage vpage
   waitforanykey
   EXIT DO
  END IF
  LoadTextBox box, st.id
  foundstr = NO
  FOR i as integer = 0 TO UBOUND(box.text)
   IF INSTR(UCASE(box.text(i)), UCASE(st.search)) > 0 THEN foundstr = YES
  NEXT i
  IF foundstr THEN EXIT DO
  st.id += 1
 LOOP
END SUB


'==============================================================================


SUB textbox_create_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
 '--this inits and saves a new text box, copying in values from another box for defaults
 ClearTextBox box
 textbox_copy_style_from_box template_box_id, box, st
 SaveTextBox box, st.id
END SUB

SUB textbox_create_from_box_and_load (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState, menu() as string)
 gen(genMaxTextBox) += 1
 st.id = gen(genMaxTextBox)
 textbox_create_from_box template_box_id, box, st
 textbox_edit_load box, st, menu()
END SUB

SUB textbox_link_to_new_box_and_load (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState, menu() as string)
 textbox_set_after_textbox box, gen(genMaxTextBox) + 1
 SaveTextBox box, st.id
 textbox_create_from_box_and_load template_box_id, box, st, menu()
END SUB

SUB textbox_copy_style_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
 '--copies in styles values from another box for defaults
 DIM boxcopier as TextBox
 LoadTextBox boxcopier, template_box_id
 WITH box
  .no_box          = boxcopier.no_box
  .opaque          = boxcopier.opaque
  .vertical_offset = boxcopier.vertical_offset
  .shrink          = boxcopier.shrink
  .textcolor       = boxcopier.textcolor
  .boxstyle        = boxcopier.boxstyle
  'Copy backdrop as it's often used as a portrait
  .backdrop        = boxcopier.backdrop
  .backdrop_trans  = boxcopier.backdrop_trans

  .portrait_box    = boxcopier.portrait_box
  .portrait_type   = boxcopier.portrait_type
  .portrait_id     = boxcopier.portrait_id
  .portrait_pal    = boxcopier.portrait_pal
  .portrait_pos    = boxcopier.portrait_pos

  .sound_effect    = boxcopier.sound_effect
  .stop_sound_after= boxcopier.stop_sound_after
  .line_sound      = boxcopier.line_sound
  'But don't copy music, as in a sequence of textboxes you wouldn't give them the same setting.
  '.restore_music   = boxcopier.restore_music
  '.music           = boxcopier.music
 END WITH
END SUB


'============================ Textbox Text Editor =============================


'Wrap and split up a string and stuff it into box.text, possibly editing 'text'
'by trimming unneeded whitespace at the end if needed to meet the line limit.
'(Opposite of textbox_lines_to_string.)
'Returns true if it did fit, and does nothing and returns false if it didn't.
FUNCTION textbox_string_to_lines(byref box as TextBox, byref text as string) as bool
 DIM lines() as string
 DIM wrappedtext as string = wordwrap(text, 38)
 split(wrappedtext, lines())

 IF UBOUND(lines) + 1 > maxTextboxLines THEN
  'Trim whitespace lines past the end so that stuffing doesn't fail unnecessarily
  FOR idx as integer = maxTextboxLines TO UBOUND(lines)
   IF LEN(TRIM(lines(idx))) > 0 THEN RETURN NO  'Can't trim! Failure
  NEXT
  REDIM PRESERVE lines(maxTextboxLines - 1)
  'Also trim those extra lines from the input string.
  DIM line_starts() as integer
  split_line_positions text, lines(), line_starts()
  text = LEFT(text, line_starts(maxTextboxLines) - 1)
 END IF
 a_copy lines(), box.text()
 RETURN YES
END FUNCTION

SUB textbox_line_editor (byref box as TextBox, byref st as TextboxEditState)
 DIM text as string = textbox_lines_to_string(box)

 DIM textslice as Slice Ptr
 textslice = NewSliceOfType(slText)
 WITH *textslice
  .x = 8
  .y = 7
  .width = 38 * 8
 END WITH
 DIM txtdata as TextSliceData Ptr = textslice->SliceData
 WITH *txtdata
  .line_limit = maxTextboxLines
  .insert = -1  'End of text
  .show_insert = YES
  .outline = YES
  .wrap = YES
  IF box.textcolor > 0 THEN
   .col = box.textcolor
  ELSE
   .col = uilook(uiText)
  END IF
 END WITH
 
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_line_editor"

  DIM newtext as string = text
  DIM newinsert as integer = txtdata->insert
  stredit(newtext, newinsert, 9999, maxTextboxLines, 38)
  IF textbox_string_to_lines(box, newtext) THEN
   'Accepted: the new text did fit in the box
   text = newtext
   txtdata->insert = newinsert  'May be past end, because newtext got trimmed
   ChangeTextSlice textslice, text
  END IF
  
  'Display the textbox minus the text
  clearpage dpage
  textbox_edit_preview box, st, dpage, 4, YES
  'Display the lines in the box
  DrawSlice textslice, dpage
  textcolor uilook(uiText), 0
  printstr "Text Box " & st.id, 0, 100, dpage
  printstr "${C0} = Leader's name", 0, 120, dpage
  printstr "${C#} = Hero name at caterpillar slot #", 0, 128, dpage
  printstr "${P#} = Hero name at party slot #", 0, 136, dpage
  printstr "${H#} = Name of hero ID #", 0, 144, dpage
  printstr "${V#} = Global Plotscript Variable ID #", 0, 152, dpage
  printstr "${S#} = Insert String Variable with ID #", 0, 160, dpage
  printstr "${B#} = Platform-specific button name #", 0, 168, dpage
  printstr "CTRL+SPACE: choose an extended character", 0, 184, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB


'========================== Textbox Choice Editor =============================


SUB textbox_choice_editor (byref box as TextBox, byref st as TextboxEditState)
 'tchoice:
 DIM state as MenuState
 WITH state
  .last = 5
  .size = 24
 END WITH
 DIM menu(5) as string
 menu(0) = "Go Back"
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT SUB
  IF keyval(scF1) > 1 THEN show_help "textbox_choice_editor"
  usemenu state
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT SUB
   IF state.pt = 1 THEN box.choice_enabled = (NOT box.choice_enabled)
  END IF
  IF state.pt = 1 THEN
   IF keyval(ccLeft) > 1 OR keyval(ccRight) > 1 THEN box.choice_enabled = (NOT box.choice_enabled)
  END IF
  FOR i as integer = 0 TO 1
   IF state.pt = 2 + (i * 2) THEN strgrabber box.choice(i), 15
   IF state.pt = 3 + (i * 2) THEN tag_set_grabber box.choice_tag(i), state
  NEXT i
  IF box.choice_enabled THEN menu(1) = "Choice = Enabled" ELSE menu(1) = "Choice = Disabled"
  FOR i as integer = 0 TO 1
   menu(2 + (i * 2)) = "Option " & i & " text:" + box.choice(i)
   menu(3 + (i * 2)) = tag_set_caption(box.choice_tag(i))
  NEXT i
 
  clearpage dpage
  standardmenu menu(), state, 0, 8, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB


'========================== Textbox Connections Viewer ========================


SUB textbox_connections(byref box as TextBox, byref st as TextboxEditState, menu() as string)
'TODO: menu() should be moved to become a member of st, then we wouldn't have to pass it around
 DIM do_search as bool = YES
 REDIM prev(5) as TextboxConnectNode
 DIM current as TextboxConnectNode
 REDIM nxt(2) as TextboxConnectNode

 DIM column as integer = 1
 DIM col_limit_left as integer = 1
 DIM col_limit_right as integer = 1
 
 DIM state as MenuState
 state.size = 6
 DIM nxt_state as MenuState
 nxt_state.size = 2
 DIM searchbox as TextBox
 
 DIM nxt_add_type as integer
 DIM remember_insert as integer
 DIM remember_insert_tag as integer
 
 DIM y as integer

 setkeys
 DO
  setwait 55
  setkeys
  IF do_search THEN
   column = 1
   col_limit_left = 1
   col_limit_right = 2
   '--Current box
   textbox_connection_captions current, st.id, 0, box, "BOX", NO
   '--Next boxes
   nxt_state.last = -1
   nxt_state.pt = 0
   nxt_state.top = 0
   REDIM nxt(2) as TextboxConnectNode
   IF box.instead_tag <> 0 THEN
    nxt_state.last += 1
    LoadTextBox searchbox, box.instead
    textbox_connection_captions nxt(nxt_state.last), box.instead, box.instead_tag, searchbox, "INSTEAD"
   END IF
   IF box.after_tag <> 0 THEN
    nxt_state.last += 1
    LoadTextBox searchbox, box.after
    textbox_connection_captions nxt(nxt_state.last), box.after, box.after_tag, searchbox, "AFTER"
   END IF
   nxt_state.last += 1
   WITH nxt(nxt_state.last)
    .add = YES
    .lines(0) = " INSERT A"
    .lines(1) = " NEW BOX"
    .style = 1
   END WITH
   '--Previous boxes
   state.last = -1
   state.pt = 0
   state.top = 0
   FOR i as integer = 0 TO gen(genMaxTextBox)
    LoadTextBox searchbox, i
    WITH searchbox
     IF .instead = st.id AND .instead_tag <> 0 THEN
      state.last += 1
      IF UBOUND(prev) < state.last THEN
       REDIM PRESERVE prev(UBOUND(prev) + 10)
      END IF
      textbox_connection_captions prev(state.last), i, .instead_tag, searchbox, "REPLACES"
      col_limit_left = 0
     END IF
     IF .after = st.id AND .after_tag <> 0 THEN
      state.last += 1
      IF UBOUND(prev) < state.last THEN
       REDIM PRESERVE prev(UBOUND(prev) + 10)
      END IF
      textbox_connection_captions prev(state.last), i, .after_tag, searchbox, "BEFORE"
      col_limit_left = 0
     END IF
    END WITH
   NEXT i
   do_search = NO
  END IF
  IF keyval(ccCancel) > 1 THEN EXIT SUB
  IF keyval(scF1) > 1 THEN show_help "textbox_connections"
  '--Horizontal column navigation
  IF keyval(ccLeft) > 1 THEN loopvar column, col_limit_left, col_limit_right, -1
  IF keyval(ccRight) > 1 THEN loopvar column, col_limit_left, col_limit_right, 1
  '--Vertical navigation within selected column
  SELECT CASE column
   CASE 0 'Previous
    usemenu state
    IF enter_space_click(state) THEN
     IF prev(state.pt).id >= 0 THEN
      st.id = prev(state.pt).id
      textbox_edit_load box, st, menu()
      do_search = YES
     END IF
    END IF
   CASE 1 'Current
    IF enter_space_click(state) THEN EXIT SUB
   CASE 2 'Next
    usemenu nxt_state
    IF enter_space_click(nxt_state) THEN
     IF nxt(nxt_state.pt).add THEN
      'Add a box
      nxt_add_type = twochoice("Add a new box?", "After this box", "Instead of this box", 0, -1)
      IF nxt_add_type >= 0 THEN
       '--Add a box
       gen(genMaxTextbox) += 1
       IF nxt_add_type = 0 THEN
        '--an after box
        remember_insert = box.after
        remember_insert_tag = box.after_tag
        box.after_tag = -1
        box.after = gen(genMaxTextbox)
       ELSEIF nxt_add_type = 1 THEN
        '--an instead box
        remember_insert = box.instead
        remember_insert_tag = box.instead_tag
        box.instead_tag = -1
        box.instead = gen(genMaxTextbox)
       END IF
       SaveTextBox box, st.id
       st.id = gen(genMaxTextBox)
       textbox_create_from_box 0, box, st
       'Having added the new box, now we insert it into the existing chain
       IF nxt_add_type = 0 THEN
        box.after = remember_insert
        box.after_tag = remember_insert_tag
       ELSEIF nxt_add_type = 1 THEN
        box.instead = remember_insert
        box.instead_tag = remember_insert_tag
       END IF
       SaveTextBox box, st.id
       textbox_edit_load box, st, menu()
       do_search = YES
      END IF
     ELSE
      'Navigate to a box
      IF nxt(nxt_state.pt).id >= 0 THEN
       st.id = nxt(nxt_state.pt).id
       textbox_edit_load box, st, menu()
       do_search = YES
      END IF
     END IF
    END IF
  END SELECT

  '--Draw box preview
  clearpage dpage
  textbox_edit_preview box, st, dpage, 96
  '--Draw previous
  IF state.last >= 0 THEN
   FOR i as integer = state.top TO small(state.last, state.top + state.size)
    y = (i - state.top) * 25
    textbox_connection_draw_node prev(i), 0, y, (column = 0 AND state.pt = i)
   NEXT i
  ELSE
   edgeprint "No Previous", 0, 0, uilook(uiMenuItem), dpage
  END IF
  '--Draw current
  y = 10 * large(nxt_state.last, 0)
  textbox_connection_draw_node current, 106, y, (column = 1)
  '--Draw next
  IF nxt_state.last >= 0 THEN
   FOR i as integer = nxt_state.top TO small(nxt_state.last, nxt_state.top + nxt_state.size)
    y = (i - nxt_state.top) * 25
    textbox_connection_draw_node nxt(i), 212, y, (column = 2 AND nxt_state.pt = i)
   NEXT i
  ELSE
   edgeprint "No Next", 212, 0, uilook(uiMenuItem), dpage
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB textbox_connection_captions(byref node as TextboxConnectNode, id as integer, tag as integer, box as TextBox, topcation as string, use_tag as bool = YES)
 DIM preview_line as string
 IF id >= 0 THEN
  node.lines(0) = topcation & " " & id
  preview_line = textbox_preview_line(box)
  IF use_tag THEN
   node.lines(1) = textbox_condition_short_caption(tag)
   node.lines(2) = LEFT(textbox_preview_line(box), 13)
  ELSE
   node.lines(1) = LEFT(preview_line, 13)
   node.lines(2) = MID(preview_line, 13, 13)
  END IF
 ELSE
  node.lines(0) = topcation & " " & "SCRIPT"
  preview_line = scriptname(ABS(id))
  node.lines(1) = LEFT(preview_line, 13)
  node.lines(2) = MID(preview_line, 13, 13)
 END IF
 node.id = id
END SUB

SUB textbox_connection_draw_node(byref node as TextboxConnectNode, x as integer, y as integer, selected as integer)
 STATIC tog as integer = 0
 edgeboxstyle x, y, 104, 26, node.style, dpage, (NOT selected), YES
 textcolor uilook(uiMenuItem), 0
 IF selected THEN
  textcolor uilook(uiSelectedItem + tog), 0
  tog = tog XOR 1
 END IF
 FOR i as integer = 0 TO 2
  printstr node.lines(i), x + 1, y + i * 8 + 1, dpage
 NEXT i
END SUB


'============================== Import Textboxes ==============================


SUB textbox_edit_importer ()
 IF yesno("Are you sure? Boxes will be overwritten", NO) = NO THEN EXIT SUB

 DIM box_text_file as string
 box_text_file = browse(browseAny, browse_default, "*.txt", "browse_import_textbox")
 IF LEN(box_text_file) = 0 THEN EXIT SUB
 clearpage vpage

 '--Make a backup copy of the .say lump
 DIM backup_say as string = tmpdir & "backup-textbox-lump.say"
 copyfile game & ".say", backup_say
 IF NOT isfile(backup_say) THEN
  visible_debug "Unable to save a backup copy of the text box data to " & backup_say
  EXIT SUB
 END IF

 '--Backup was successful, okay to proceed
 DIM remember_boxcount as integer = gen(genMaxTextbox)
 DIM import_warn as string = ""
 IF import_textboxes(box_text_file, import_warn) THEN
  notification "Successfully imported """ & decode_filename(box_text_file) & """. " & import_warn
 ELSE
  'Failure! Reset, revert, abort, run-away!
  gen(genMaxTextBox) = remember_boxcount
  copyfile backup_say, game & ".say"
  notification "Import failed, restoring backup. " & import_warn
 END IF
END SUB

SUB import_textboxes_warn (byref warn as string, s as string)
 debug "import_textboxes: " & s
 IF warn <> "" THEN warn = warn & " "
 warn = warn & s
END SUB

FUNCTION import_textboxes (filename as string, byref warn as string) as bool
 DIM fh as integer = FREEFILE
 IF OPEN(filename FOR INPUT as #fh) THEN
  import_textboxes_warn warn, "Failed to open """ & decode_filename(filename) & """."
  RETURN NO
 END IF
 DIM warn_length as integer = 0
 DIM warn_skip as integer = 0
 DIM warn_append as integer = 0
 DIM show_encoding_warnings as bool = YES
 DIM box as TextBox
 DIM index as integer = 0
 DIM getindex as integer = 0 
 DIM mode as integer = 0
 DIM s as string
 DIM firstline as bool = YES
 DIM line_number as integer = 0
 DIM i as integer
 DO WHILE NOT EOF(fh)
  line_number += 1
  LINE INPUT #fh, s
  s = decode_backslash_codes(s, "Line " & line_number & ":", show_encoding_warnings)
  IF firstline THEN
   IF RTRIM(s) <> STRING(38, "=") THEN
    import_textboxes_warn warn, decode_filename(filename) & " is not a valid text box file. Expected header row, found """ & s & """."
    CLOSE #fh
    RETURN NO
   END IF
   firstline = NO
   CONTINUE DO
  END IF
  SELECT CASE mode
   CASE 0 '--Seek box number
    IF LEFT(s, 4) = "Box " THEN
     getindex = VALINT(MID(s, 5))
     IF getindex > index THEN
      warn_skip += 1
      debug "import_textboxes: line " & line_number & ": box ID " & index & " is not in the txt file"
     END IF
     IF getindex < index THEN
      debug "import_textboxes: line " & line_number & ": box ID numbers out-of-order. Expected " & index & ", but found " & getindex
     END IF
     index = getindex
     LoadTextBox box, index
     mode = 1
    ELSE
     import_textboxes_warn warn, "line " & line_number & ": expected Box # but found """ & s & """."
     CLOSE #fh
     RETURN NO
    END IF
   CASE 1 '--Seek divider
    IF RTRIM(s) = STRING(38, "-") THEN
     '--Beginning of text. Erase old text.
     ERASE box.text
     mode = 2
    ELSEIF RTRIM(s) = STRING(38, "=") THEN
     '--No text. Don't touch this box's text; save and prepare for the next box.
     IF index > gen(genMaxTextbox) THEN
      warn_append += index - gen(genMaxTextbox)
      gen(genMaxTextbox) = index
     END IF
     SaveTextBox box, index
     index += 1
     mode = 0
    ELSE
     IF INSTR(s, ":") THEN '--metadata, probably
      DIM t as string, valline as string, v as string
      t = LCASE(LEFT(s, INSTR(s, ":") - 1))
      valline = LTRIM(MID(s, INSTR(s, ":") + 1))
      'v is valline with any comment appearing after the value removed
      v = valline
      IF INSTR(v, " ") THEN v = MID(v, 1, INSTR(v, " ") - 1)
      SELECT CASE t
       CASE "size"
        IF LCASE(v) = "auto" THEN
         box.shrink = -1
        ELSEIF VALINT(v) > 21 THEN
         debug "Box size too large, capping"
         box.shrink = 0
        ELSE
         box.shrink = 21 - VALINT(v)
        END IF
       CASE "portrait box"
        box.portrait_box = str2bool(v, NO)
       CASE "portrait type"
        box.portrait_type = VALINT(v)
       CASE "portrait id"
        box.portrait_id = VALINT(v)
       CASE "portrait x"
        box.portrait_pos.x = VALINT(v)
       CASE "portrait y"
        box.portrait_pos.y = VALINT(v)
       CASE "portrait palette"
        box.portrait_pal = VALINT(v)
       CASE "instead tag"
        box.instead_tag = VALINT(v)
       CASE "instead box"
        box.instead = VALINT(v)
       CASE "set tag"
        box.settag_tag = VALINT(v)
       CASE "set tag 1"
        box.settag1 = VALINT(v)
       CASE "set tag 2"
        box.settag2 = VALINT(v)
       CASE "battle tag"
        box.battle_tag = VALINT(v)
       CASE "battle"
        box.battle = VALINT(v)
       CASE "shop tag"
        box.shop_tag = VALINT(v)
       CASE "shop"
        box.shop = VALINT(v)
       CASE "item tag"
        box.item_tag = VALINT(v)
       CASE "item"
        box.item = VALINT(v)
       CASE "money tag"
        box.money_tag = VALINT(v)
       CASE "money"
        box.money = VALINT(v)
       CASE "door tag"
        box.door_tag = VALINT(v)
       CASE "door"
        box.door = VALINT(v)
       CASE "hero tag"
        box.hero_tag = VALINT(v)
       CASE "hero add"
        box.hero_addrem = VALINT(v)
       CASE "hero swap"
        box.hero_swap = VALINT(v)
       CASE "hero lock"
        box.hero_lock = VALINT(v)
       CASE "menu tag"
        box.menu_tag = VALINT(v)
       CASE "menu"
        box.menu = VALINT(v)
       CASE "game tag"
        box.game_tag = VALINT(v)
       CASE "game delete"
        box.game_delete = VALINT(v)
       CASE "game save"
        box.game_save = VALINT(v)
       CASE "game load"
        box.game_load = VALINT(v)
       CASE "next tag"
        box.after_tag = VALINT(v)
       CASE "next box"
        box.after = VALINT(v)
       CASE "choice enabled"
        box.choice_enabled = str2bool(v)
       CASE "choice 1"
        box.choice(0) = TRIM(valline)
       CASE "choice 2"
        box.choice(1) = TRIM(valline)
       CASE "choice 1 tag"
        box.choice_tag(0) = VALINT(v)
       CASE "choice 2 tag"
        box.choice_tag(1) = VALINT(v)
       CASE "position"
        box.vertical_offset = VALINT(v)
       CASE "text color"
        box.textcolor = VALINT(v)
       CASE "border color"
        box.boxstyle = VALINT(v)
       CASE "backdrop"
        box.backdrop = VALINT(v)
       CASE "music"
        box.music = VALINT(v)
       CASE "restore music"
        box.restore_music = str2bool(v)
       CASE "sound effect"
        box.sound_effect = VALINT(v)
       CASE "stop sound after box"
        box.stop_sound_after = str2bool(v)
       CASE "line sound"
        box.line_sound = VALINT(v)
       CASE "show box"
        box.no_box = str2bool(v) XOR YES
       CASE "translucent"
        box.opaque = str2bool(v) XOR YES

       CASE ELSE
        import_textboxes_warn warn, "line " & line_number & ": expected divider line but found """ & s & """."
        CLOSE #fh
        RETURN NO
      END SELECT
     END IF
    END IF
   CASE 2 '--Text lines
    IF RTRIM(s) = STRING(38, "=") THEN
     'End of this textbox, save and start the next one.
     IF index > gen(genMaxTextbox) THEN
      warn_append += index - gen(genMaxTextbox)
      gen(genMaxTextbox) = index
     END IF
     SaveTextBox box, index
     index += 1
     mode = 0
    ELSE
     IF UBOUND(box.text) + 1 = maxTextboxLines THEN
      import_textboxes_warn warn, "line " & line_number & ": too many lines in box " & index & ". Overflowed with """ & s & """."
      CLOSE #fh
      RETURN NO
     END IF
     IF LEN(s) > 38 THEN '--this should be down here
      warn_length += 1
      debug "import_textboxes: line " & line_number & ": line too long (" & LEN(s) & ")"
      s = LEFT(s, 38)
     END IF
     a_append box.text(), s
    END IF
  END SELECT
 LOOP
 IF mode = 2 THEN'--Save the last box
  IF index > gen(genMaxTextbox) THEN
   warn_append += index - gen(genMaxTextbox)
   gen(genMaxTextbox) = index
  END IF
  SaveTextBox box, index
 ELSEIF mode = 0 THEN '--this... is not good
  import_textboxes_warn warn, "line " & line_number & ": txt file ended unexpectedly."
  CLOSE #fh
  RETURN NO
 END IF
 IF warn_length > 0 THEN import_textboxes_warn warn, warn_length & " lines were too long."
 IF warn_skip > 0   THEN import_textboxes_warn warn, warn_skip & " box ID numbers were not in the txt file."
 IF warn_append > 0 THEN import_textboxes_warn warn, warn_append & " new boxes were appended."
 CLOSE #fh
 RETURN YES
END FUNCTION


'============================== Export Texboxes ===============================

SUB textbox_edit_exporter ()
 STATIC metadata(3) as bool
 DIM metadatalabels(3) as string
 metadatalabels(0) = "Text"
 metadata(0) = YES '--by default, export text
 metadatalabels(1) = "Conditionals"
 metadatalabels(2) = "Choices"
 metadatalabels(3) = "Appearance"
 IF editbools(metadata(), metadatalabels(), "textbox_export_askwhatmetadata", _
              , , "Choose which metadata to include", "Done") = edbitCancelled THEN EXIT SUB

 DIM box_text_file as string
 box_text_file = inputfilename("Filename for TextBox Export?", ".txt", browse_default, "input_file_export_textbox")
 IF box_text_file = "" THEN EXIT SUB
 box_text_file &= ".txt"

 IF export_textboxes(box_text_file, metadata()) THEN
  notification "Successfully exported " & decode_filename(box_text_file)
 ELSE
  notification "Failed to export " & decode_filename(box_text_file)
 END IF
END SUB

FUNCTION export_textboxes (filename as string, metadata() as bool) as bool
 DIM fh as integer
 IF OPENFILE(filename, FOR_OUTPUT + OR_ERROR, fh) THEN RETURN NO
 DIM box as TextBox
 DIM as integer i, j, k
 FOR i = 0 TO gen(genMaxTextBox)
  LoadTextBox box, i
  '--Write the header guide
  PRINT #fh, "======================================"
  '--Write the box number and metadata
  PRINT #fh, "Box " & i
    
  IF metadata(1) THEN '--box conditionals
   IF box.instead_tag <> 0 THEN
    PRINT #fh, "Instead Tag: " & box.instead_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.instead_tag, , "Never")) & ")"
    PRINT #fh, "Instead Box: " & box.instead;
    IF box.instead < 0 THEN
     PRINT #fh, " (Plotscript " & scriptname(box.instead * -1) & ")"
    ELSE
     PRINT #fh, " (Textbox)"
    END IF
   END IF
   IF box.after_tag <> 0 THEN
    PRINT #fh, "Next Tag: " & box.after_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.after_tag, , "Never")) & ")"
    PRINT #fh, "Next Box: " & box.after;
    IF box.after < 0 THEN
     PRINT #fh, " (Plotscript " & scriptname(box.after * -1) & ")"
    ELSE
     PRINT #fh, " (Textbox)"
    END IF
   END IF
   
   IF box.settag_tag <> 0 THEN
    PRINT #fh, "Set Tag: " & box.settag_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.settag_tag, , "Never")) & ")"
    IF box.settag1 <> 0 THEN PRINT #fh, "Set Tag 1: " & box.settag1 & " (" & escape_nonprintable_ascii(tag_set_caption(box.settag1)) & ")"
    IF box.settag2 <> 0 THEN PRINT #fh, "Set Tag 2: " & box.settag2 & " (" & escape_nonprintable_ascii(tag_set_caption(box.settag2)) & ")"
   END IF
   IF box.battle_tag <> 0 THEN
    PRINT #fh, "Battle Tag: " & box.battle_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.battle_tag, , "Never")) & ")"
    PRINT #fh, "Battle: " & box.battle
   END IF
   IF box.shop_tag <> 0 THEN
    PRINT #fh, "Shop Tag: " & box.shop_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.shop_tag, , "Never")) & ")"
    PRINT #fh, "Shop: " & box.shop;
    IF box.shop = 0 THEN PRINT #fh, " (Restore HP/MP)"
    IF box.shop < 0 THEN PRINT #fh, " (Inn for $" & (box.shop * -1) & ")"
    IF box.shop > 0 THEN PRINT #fh, " (" & escape_nonprintable_ascii(readshopname(box.shop - 1)) & ")"
   END IF
   IF box.hero_tag <> 0 THEN
    PRINT #fh, "Hero Tag: " & box.hero_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.hero_tag, , "Never")) & ")"
    
    IF box.hero_addrem <> 0 THEN
     PRINT #fh, "Hero Add: " & box.hero_addrem;
     IF box.hero_addrem < 0 THEN
      PRINT #fh, " (Remove " & escape_nonprintable_ascii(getheroname((box.hero_addrem * -1) - 1)) & ")"
     ELSE
      PRINT #fh, " (Add " & escape_nonprintable_ascii(getheroname(box.hero_addrem - 1)) & ")"
     END IF
    END IF
    
    IF box.hero_swap <> 0 THEN
     PRINT #fh, "Hero Swap: " & box.hero_swap;
     IF box.hero_swap < 0 THEN
      PRINT #fh, " (Swap Out " & escape_nonprintable_ascii(getheroname((box.hero_swap * -1) - 1)) & ")"
     ELSE
      PRINT #fh, " (Swap In " & escape_nonprintable_ascii(getheroname(box.hero_swap - 1)) & ")"
     END IF
    END IF
    
    IF box.hero_lock <> 0 THEN
     PRINT #fh, "Hero Lock: " & box.hero_lock;
     IF box.hero_lock < 0 THEN
      PRINT #fh, " (Lock " & escape_nonprintable_ascii(getheroname((box.hero_lock * -1) - 1)) & ")"
     ELSE
      PRINT #fh, " (Unlock " & escape_nonprintable_ascii(getheroname(box.hero_lock - 1)) & ")"
     END IF
    END IF
    
   END IF
   
   IF box.game_tag <> 0 THEN
    PRINT #fh, "Game Tag: " & box.game_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.game_tag, , "Never")) & ")"
    
    IF box.game_delete <> 0 THEN
     PRINT #fh, "Game Delete: " & box.game_delete;
     IF box.game_delete = -1 THEN
      PRINT #fh, " (Delete active save slot)"
     ELSE
      PRINT #fh, " (Delete save slot " & box.game_delete & ")"
     END IF
    END IF
    
    IF box.game_save <> 0 THEN
     PRINT #fh, "Game Save: " & box.game_save;
     IF box.game_save = -1 THEN
      PRINT #fh, " (Save to active slot)"
     ELSEIF box.game_save = -2 THEN
      PRINT #fh, " (Show save menu)"
     ELSE
      PRINT #fh, " (Save to slot " & box.game_save & ")"
     END IF
    END IF
    
    IF box.game_load <> 0 THEN
     PRINT #fh, "Game Load: " & box.game_load;
     IF box.game_load = -1 THEN
      PRINT #fh, " (Load from active slot)"
     ELSEIF box.game_load = -2 THEN
      PRINT #fh, " (Show load menu)"
     ELSEIF box.game_load = -3 THEN
      PRINT #fh, " (Game over)"
     ELSE
      PRINT #fh, " (Load from slot " & box.game_load & ")"
     END IF
    END IF
    
   END IF
   
   IF box.money_tag <> 0 THEN
    PRINT #fh, "Money Tag: " & box.money_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.money_tag, , "Never")) & ")"
    PRINT #fh, "Money: " & box.money
   END IF
   
   IF box.door_tag <> 0 THEN
    PRINT #fh, "Door Tag: " & box.door_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.door_tag, , "Never")) & ")"
    PRINT #fh, "Door: " & box.door
   END IF
   
   IF box.item_tag <> 0 THEN
    PRINT #fh, "Item Tag: " & box.item_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.item_tag, , "Never")) & ")"
    PRINT #fh, "Item: " & box.item;
    IF box.item < 0 THEN
     PRINT #fh, " (Remove " & escape_nonprintable_ascii(readitemname((box.item * -1) - 1)) & ")"
    ELSE
     PRINT #fh, " (Add " & escape_nonprintable_ascii(readitemname(box.item - 1)) & ")"
    END IF
   END IF
  END IF
  
  IF box.menu_tag <> 0 THEN
    PRINT #fh, "Menu Tag: " & box.menu_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.menu_tag, , "Never")) & ")"
    PRINT #fh, "Menu: " & box.menu
   END IF
   
  IF metadata(2) THEN '--choices
   IF box.choice_enabled THEN
    PRINT #fh, "Choice Enabled: YES"
    PRINT #fh, "Choice 1: " & escape_nonprintable_ascii(box.choice(0))
    PRINT #fh, "Choice 1 " & escape_nonprintable_ascii(tag_set_caption(box.choice_tag(0)))
    PRINT #fh, "Choice 2: " & escape_nonprintable_ascii(box.choice(1))
    PRINT #fh, "Choice 2 " & escape_nonprintable_ascii(tag_set_caption(box.choice_tag(1)))
    
   END IF
  END IF
  
  IF metadata(3) THEN '--box appearance
   IF box.shrink = -1 THEN
    PRINT #fh, "Size: auto"
   ELSE
    PRINT #fh, "Size: " & (21 - box.shrink)
   END IF
   PRINT #fh, "Position: " & box.vertical_offset
   PRINT #fh, "Text Color: " & box.textcolor '--AARGH.
   PRINT #fh, "Border Color: " & box.boxstyle '--AARGH AGAIN.
   PRINT #fh, "Backdrop: " & box.backdrop
   IF box.music > 0 THEN
    PRINT #fh, "Music: " & box.music & " (" & escape_nonprintable_ascii(getsongname(box.music - 1)) & ")"
   ELSEIF box.music = 0 THEN
    PRINT #fh, "Music: " & box.music & " (None)"
   ELSEIF box.music < 0 THEN
    PRINT #fh, "Music: " & box.music & " (Silence)"
   END IF
   PRINT #fh, "Restore Music: " & yesorno(box.restore_music)
   IF box.sound_effect > 0 THEN
    PRINT #fh, "Sound Effect: " & box.sound_effect & " (" & escape_nonprintable_ascii(getsfxname(box.sound_effect - 1)) & ")"
   ELSE
    PRINT #fh, "Sound Effect: " & box.sound_effect & " (None)"
   END IF
   IF box.sound_effect > 0 THEN
    PRINT #fh, "Stop Sound After Box: " & yesorno(box.stop_sound_after)
   END IF
   IF box.line_sound > 0 THEN
    PRINT #fh, "Line Sound: " & box.line_sound & " (" & escape_nonprintable_ascii(getsfxname(box.line_sound - 1)) & ")"
   ELSEIF box.line_sound = 0 THEN
    PRINT #fh, "Line Sound: " & box.line_sound & " (Default)"
   ELSEIF box.line_sound < 0 THEN
    PRINT #fh, "Line Sound: " & box.line_sound & " (None)"
   END IF
   PRINT #fh, "Show Box: " & yesorno(NOT box.no_box) '--argh, double negatives
   PRINT #fh, "Translucent: " & yesorno(NOT box.opaque) '--  "       "      "

   IF box.portrait_box <> NO ORELSE box.portrait_type <> portraitNONE THEN
    PRINT #fh, "Portrait Box: " & yesorno(box.portrait_box)
   END IF
   IF box.portrait_type <> portraitNONE THEN
    PRINT #fh, "Portrait Type: " & box.portrait_type & " (" & _
               safe_captionz(portrait_type_names(), box.portrait_type, "type") & ")"
    PRINT #fh, "Portrait ID: " & box.portrait_id
    IF box.portrait_pal <> -1 THEN PRINT #fh, "Portrait Palette: " & box.portrait_pal
    PRINT #fh, "Portrait X: " & box.portrait_pos.X
    PRINT #fh, "Portrait Y: " & box.portrait_pos.Y
   END IF
  END IF


  IF metadata(0) THEN '--box text
   '--Write the separator
   PRINT #fh, "--------------------------------------"
   FOR j = 0 TO text_box_last_line(box)
    PRINT #fh, escape_nonprintable_ascii(box.text(j))
   NEXT j
  END IF
 NEXT i
 CLOSE #fh
 RETURN YES
END FUNCTION
