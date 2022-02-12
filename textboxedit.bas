'OHRRPGCE CUSTOM - Textbox editor
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "udts.bi"
#include "custom_udts.bi"
#include "const.bi"
#include "common.bi"
#include "allmodex.bi"
#include "bcommon.bi"
#include "customsubs.bi"
#include "loading.bi"
#include "scrconst.bi"
#include "custom.bi"
#include "thingbrowser.bi"
#include "sliceedit.bi"
#include "editorkit.bi"

'--Local subs and functions
DECLARE FUNCTION textbox_condition_caption(tag as integer, prefix as string = "") as string
DECLARE FUNCTION textbox_condition_short_caption(tag as integer) as string
DECLARE SUB write_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer, num as integer)
DECLARE FUNCTION read_box_conditional_by_menu_index(byref box as TextBox, menuindex as integer) as integer
DECLARE FUNCTION box_conditional_type_by_menu_index(menuindex as integer) as integer
DECLARE FUNCTION box_conditional_tag_by_menu_index(byref box as TextBox, menuindex as integer) as integer
DECLARE FUNCTION box_conditional_is_enabled(byref box as TextBox, menuindex as integer) as bool
DECLARE SUB update_textbox_editor_main_menu (byref box as TextBox, menu() as string)
DECLARE SUB textbox_edit_load (byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_edit_preview (byref box as TextBox, byref st as TextboxEditState, page as integer, override_y as integer=-1, for_editing as bool=NO)
DECLARE SUB textbox_draw_with_background(byref box as TextBox, byref st as TextboxEditState, backdrop as Frame ptr, page as integer)
DECLARE SUB textbox_appearance_editor (byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB update_textbox_appearance_editor_menu (byref menu as SimpleMenuItem vector, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_position_portrait (byref box as TextBox, byref st as TextboxEditState, backdrop as Frame ptr)
DECLARE SUB textbox_seek(byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_create_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_create_from_box_and_load (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_link_to_new_box_and_load (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_line_editor (byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_set_after_textbox (byref box as TextBox, after_textbox as integer)
DECLARE SUB textbox_copy_style_from_box (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
DECLARE SUB textbox_connections(byref box as TextBox, byref st as TextboxEditState)
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

STATIC SHARED portrait_type_names(portraitLAST) as zstring ptr = {_
    @"None", _
    @"Fixed", _
    @"Hero (by caterpillar order)", _
    @"Hero (by party slot)", _
    @"Hero (by ID)" _
}

DIM SHARED browse_default as string


SUB textbox_editor_main ()
 IF read_config_bool("thingbrowser.enable_top_level", YES) THEN
  DIM b as TextboxBrowser
  b.browse(-1, , @text_box_editor)
 ELSE
  text_box_editor 0
 END IF
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
  .rootsl = NewSlice()
  .rootsl->Width = gen(genResolutionX)
  .rootsl->Height = gen(genResolutionY)
  .viewport_page = gameres_page()
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

 st.menu(0) = "Return to Previous Menu"
 st.menu(1) = "Text Box"
 st.menu(2) = "Edit Text"
 st.menu(3) = "Edit Conditionals"
 st.menu(4) = "Edit Choices"
 st.menu(5) = "Box Appearance & Sounds"
 st.menu(6) = "After:"
 st.menu(7) = "Text Search:"
 st.menu(8) = "Connected Boxes..."
 st.menu(9) = "Export text boxes..."
 st.menu(10) = "Import text boxes..."

 DIM state as MenuState  'State of the toplevel menu
 state.pt = 1
 state.last = UBOUND(st.menu)
 state.size = 24
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 menuopts.itemspacing = -1

 STATIC style_clip as integer = 0

 textbox_edit_load box, st
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "textbox_main"
  IF cropafter_keycombo(state.pt = 1) THEN
   cropafter st.id, gen(genMaxTextBox), game & ".say", curbinsize(binSAY)
   textbox_edit_load box, st
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
      textbox_link_to_new_box_and_load st.id, box, st
     END IF
    ELSEIF keyval(scInsert) > 1 ANDALSO yesno("Create and link to a new textbox?") THEN
     textbox_link_to_new_box_and_load 0,     box, st
    ELSEIF keyval(scAlt) = 0 THEN  'Ignore alt+left/right keypresses
     ' Ctrl+Left/Right links to previous/next box. We actually let scrintgrabber
     ' handle that, by starting at box.after. So continuing to press Ctrl+Left/Right works.
     IF keyval(scCtrl) > 0 AND box.after = 0 THEN
      IF keyval(ccLeft) > 1 OR keyval(ccRight) > 1 THEN box.after = st.id
     END IF
     IF scrintgrabber(box.after, 0, gen(genMaxTextbox), ccLeft, ccRight, -1, plottrigger) THEN
      textbox_set_after_textbox box, box.after
      SaveTextBox box, st.id
      update_textbox_editor_main_menu box, st.menu()
     END IF
    END IF
   CASE ELSE '--not using the quick textbox chainer nor the search
    IF keyval(scAlt) > 0 AND keyval(scC) > 1 THEN style_clip = st.id
    IF keyval(scAlt) > 0 AND keyval(scV) > 1 THEN
     IF style_clip > gen(genMaxTextBox) THEN
      visible_debug "Oops! Text box " & style_clip & " doesn't exist, so we can't paste its style"
      style_clip = 0
     ELSE
      IF yesno("Copy box " & style_clip & "'s style to this box?") THEN
       textbox_copy_style_from_box style_clip, box, st
       SaveTextBox box, st.id
       textbox_edit_load box, st
      END IF
     END IF
    END IF
    IF (keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1) AND gen(genMaxTextBox) < maxMaxTextbox THEN
     IF yesno("Create a textbox like this one?") THEN
      textbox_create_from_box_and_load st.id, box, st
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
    textbox_edit_load box, st
   END IF
  END IF

  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 1 THEN EXIT DO
   IF state.pt = 2 THEN textbox_line_editor box, st
   IF state.pt = 3 THEN
    textbox_conditionals box
    update_textbox_editor_main_menu box, st.menu()
   END IF
   IF state.pt = 4 THEN textbox_choice_editor box, st
   IF state.pt = 5 THEN
    textbox_appearance_editor box, st
    '--re-update the menu after the appearance editor in case we switched records
    update_textbox_editor_main_menu box, st.menu()
   END IF
   IF state.pt = 6 THEN
    DIM want_scriptbrowse as bool = NO
    IF box.after > 0 THEN
     '--Go to Next textbox
     st.id = box.after
     textbox_edit_load box, st
    ELSEIF box.after < 0 THEN
     want_scriptbrowse = YES
    ELSE
     DIM choices(...) as string = { _
      "A new text box with default style", "A new text box with this style", "A script" _
     }
     DIM choice as integer = multichoice("Link to what after this text box?", choices())
     IF choice = 0 THEN textbox_link_to_new_box_and_load 0,     box, st
     IF choice = 1 THEN textbox_link_to_new_box_and_load st.id, box, st
     IF choice = 2 THEN want_scriptbrowse = YES
    END IF
    IF want_scriptbrowse THEN
     DIM temptrig as integer = ABS(box.after)
     scriptbrowse temptrig, plottrigger, "textbox plotscript"
     textbox_set_after_textbox box, -temptrig
     update_textbox_editor_main_menu box, st.menu()
    END IF
   END IF
   IF state.pt = 7 AND keyval(scEnter) > 1 THEN
    textbox_seek box, st
    textbox_edit_load box, st
   END IF
   IF state.pt = 8 THEN
    textbox_connections box, st
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
    st.menu(1) = CHR(27) & "Text Box 0 [template]" & CHR(26)
  ELSE
    st.menu(1) = CHR(27) & "Text Box " & st.id & CHR(26)
  END IF

  st.menu(7) = "Text Search:"
  IF state.pt = 7 THEN st.menu(7) &= st.search
 
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
  standardmenu st.menu(), state, 0, 0, dpage, menuopts

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 DeleteSlice @st.rootsl
 freepage st.viewport_page
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
    IF enter_space_click(state) THEN
     num = formation_picker(num)
    END IF
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
   DIM disabled as bool
   IF grey(i) = YES THEN  'Section heading
    DIM bgcol as integer
    SELECT CASE read_box_conditional_by_menu_index(box, i)
     CASE -1   ' ALWAYS: Check tag 1=OFF
      bgcol = uilook(uiHighlight)
     CASE 0, 1 ' NEVER: Disabled or check tag 1=ON
      bgcol = uilook(uiDisabledItem)
     CASE ELSE ' Tag check
      bgcol = uilook(uiSelectedDisabled)
    END SELECT
    rectangle 0, drawy, rWidth, state.spacing - 1, bgcol, dpage
    disabled = NO
   ELSE
    'Display items that do nothing greyed out
    disabled = (box_conditional_is_enabled(box, i) = NO)
   END IF
   textcolor menu_item_color(state, i, disabled), 0
   printstr menu(i), 0, drawy, dpage
  NEXT i
  rectangle pRight, 0, 8, rHeight, uilook(uiBackground), dpage  'Background rect for the scrollbar
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
 IF box.battle_tag = 0 THEN
  menu(8) = " fight enemy formation"
 ELSE
  menu(8) = " fight enemy formation " & box.battle & " " & describe_formation_by_id(box.battle)
 END IF
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


' Draw the textbox slices (without backdrop)
' override_y: optionally overrides y position, and also override x to 0, and the choicebox is
'    always placed underneath the text box, instead of potentially appearing above it.
' for_editing: hide the choicebox and raise the text above the portrait slices
SUB textbox_edit_preview (byref box as TextBox, byref st as TextboxEditState, page as integer, override_y as integer=-1, for_editing as bool=NO)

 DIM text_sl as Slice ptr = LookupSliceSafe(SL_TEXTBOX_TEXT, st.rootsl)
 IF for_editing ANDALSO text_sl->Parent THEN
  InsertSliceAfter text_sl->Parent->LastChild, text_sl
 END IF

 IF override_y >= 0 THEN
  DIM box_sl as Slice ptr = LookupSliceSafe(SL_TEXTBOX_BOX, st.rootsl)
  DIM choice_box_sl as Slice ptr = LookupSlice(SL_TEXTBOX_CHOICE_BOX, st.rootsl)
  DrawSliceAt box_sl, 0, override_y, 320, 200, page, YES
  IF choice_box_sl ANDALSO for_editing = NO THEN
   DrawSliceAt choice_box_sl, 0, override_y + box_sl->Height + 12, 320, 200, page, YES
  END IF
 ELSE
  DrawSlice st.rootsl, page
 END IF

 IF for_editing ANDALSO text_sl->Parent THEN
  InsertSliceBefore text_sl->Parent->FirstChild, text_sl
 END IF
END SUB

' Preview the textbox as it will appear in-game, portraying it with in-game window size
SUB textbox_draw_with_background(byref box as TextBox, byref st as TextboxEditState, backdrop as Frame ptr, page as integer)
 clearpage page
 draw_background vpages(page), uilook(uiBackground)
 ' Draw the textbox over a textured background
 draw_textured_background st.viewport_page
 IF backdrop THEN frame_draw backdrop, , 0, 0, box.backdrop_trans, st.viewport_page
 textbox_edit_preview box, st, st.viewport_page
 ' Draw it in the corner of the screen
 draw_viewport_page st.viewport_page, page
END SUB

SUB textbox_edit_load (byref box as TextBox, byref st as TextboxEditState)
 LoadTextBox box, st.id
 update_textbox_editor_main_menu box, st.menu()
 init_text_box_slices st.textbox_sl, box, st.rootsl, YES
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

'Returns the value (istag() arg) of the tag condition for a conditional, given its menu index.
'Eg. for the .door ("use door") conditional, returns .door_tag
FUNCTION box_conditional_tag_by_menu_index(byref box as TextBox, menuindex as integer) as integer
 'This works by simply scanning up the menu until we hit a tag condition
 WHILE menuindex >= 0
  IF box_conditional_type_by_menu_index(menuindex) = condTAG THEN
   RETURN read_box_conditional_by_menu_index(box, menuindex)
  END IF
  menuindex -= 1
 WEND
END FUNCTION

'Whether a conditional-action does anything.
'Returns false if a conditional is disabled by a NEVER condition,
'or by being set to zero, if zero means do nothing.
'Not intended to be called a menu index for a section header (tag condition).
FUNCTION box_conditional_is_enabled(byref box as TextBox, menuindex as integer) as bool
 IF menuindex = -1 THEN RETURN YES  'Previous menu
 SELECT CASE box_conditional_tag_by_menu_index(box, menuindex)
  CASE 0, 1 ' NEVER: Disabled or check tag 1=ON
   RETURN NO
 END SELECT
 SELECT CASE box_conditional_type_by_menu_index(menuindex)
  CASE condSHOP, condBATTLE, condDOOR, condMENU
   'Shop 0, formation 0, door 0 and menu 0 can all be activated with conditionals
   RETURN YES
  CASE ELSE
   '0 does nothing
   RETURN read_box_conditional_by_menu_index(box, menuindex) <> 0
 END SELECT
END FUNCTION

'============================== Appearance Editor =============================


SUB textbox_position_portrait (byref box as TextBox, byref st as TextboxEditState, backdrop as Frame ptr)
 DIM img_box as Slice ptr = LookupSliceSafe(SL_TEXTBOX_PORTRAIT_BOX, st.rootsl)

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
  img_box->X = box.portrait_pos.x - 4  'Duplicated from init_text_box_slices
  img_box->Y = box.portrait_pos.y - 3

  textbox_draw_with_background box, st, backdrop, dpage
  wrapprintbg "Arrow keys to move, space to confirm", 0, 0, uilook(uiText), dpage
  wrapprintbg "Offset " & box.portrait_pos, pLeft, pBottom, uilook(uiText), dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB



'==============================================================================


TYPE TextboxAppearanceEditor EXTENDS EditorKit
 DECLARE SUB define_items()
 DECLARE SUB load()
 DECLARE SUB save()
 DECLARE SUB draw_underlays()
 backdrop as Frame ptr
 boxp as TextBox ptr
 st as TextboxEditState ptr
END TYPE

SUB textbox_appearance_editor (byref box as TextBox, byref st as TextboxEditState)
 DIM editor as TextboxAppearanceEditor
 editor.helpkey = "textbox_appearance"
 editor.boxp = @box
 editor.st = @st
 editor.menuopts.drawbg = YES
 editor.menuopts.edged = YES
 editor.setup_record_switching st.id, gen(genMaxTextBox), , "Text Box"
 editor.run()
 frame_unload @editor.backdrop
END SUB

SUB TextboxAppearanceEditor.save()
 SaveTextBox *boxp, st->id
END SUB

SUB TextboxAppearanceEditor.load()
 ' This also loads the slices, but we don't rely on that: the backdrop
 ' and slices are (instead) loaded in define_items
 textbox_edit_load *boxp, *st
 'music_stop
END SUB

SUB TextboxAppearanceEditor.draw_underlays()
 textbox_draw_with_background *boxp, *st, backdrop, vpage
END SUB

SUB TextboxAppearanceEditor.define_items()
 DIM byref box as TextBox = *boxp

 defint "Position:", box.vertical_offset, 0, gen(genResolutionX) \ 4 - 1
 defint "Text Color:", box.textcolor, 0, 255
 IF activate THEN
  value = color_browser_256(value)
  edited = YES
 END IF

 section "Box"
 defitem "Show Box:"
 edit_bool invert_bool(box.no_box)
 IF box.no_box = NO THEN
  defitem "Translucent:"
  edit_bool invert_bool(box.opaque)
 END IF
 IF box.no_box = NO ORELSE box.choice_enabled THEN  'Box Style and shrink affect the choicebox
  defint "Box Style:", box.boxstyle, 0, 14
  defitem "Shrink:"
  edit_zint box.shrink, -1, 21
  IF value = -1 THEN set_caption "Auto"
 END IF

 section "Backdrop"
 defitem "Backdrop:"
 edit_as_spriteset(offset_int(-1, box.backdrop), sprTypeBackdrop, Or_None)
 IF box.backdrop > 0 THEN
  defbool "Transparent:", box.backdrop_trans
 END IF

 section "Portrait"
 defint "Type:", box.portrait_type, 0, portraitLAST
 captionsz portrait_type_names(), "type"
 IF box.portrait_type <> portraitNONE THEN
  defitem "ID:"
  val_int box.portrait_id
  SELECT CASE box.portrait_type
   CASE portraitSPRITESET: edit_as_spriteset value, sprTypePortrait
   CASE portraitPARTYRANK: edit_int value, 0, sizeActiveParty - 1
   CASE portraitPARTYSLOT: edit_int value, 0, sizeParty - 1
   CASE portraitHEROID:    edit_int value, 0, gen(genMaxHero)
  END SELECT
  SELECT CASE box.portrait_type
   CASE portraitPARTYRANK: IF value = 0 THEN set_caption value & " (Leader)"
   CASE portraitPARTYSLOT: IF value >= sizeActiveParty THEN set_caption value & " (Reserve)"
   CASE portraitHEROID:    set_caption value & " (" & getheroname(value) & ")"
  END SELECT

  defitem "Palette:"
  SELECT CASE box.portrait_type
   CASE portraitSPRITESET: edit_as_palette box.portrait_pal, sprTypePortrait, box.portrait_id
   CASE ELSE:              set_caption "(N/A, see hero editor)"
  END SELECT

  defbool "Box:", box.portrait_box

  IF defitem_act("Position Portrait...") THEN
   IF box.portrait_type <> portraitNONE THEN  'If portrait type is NONE, then the portrait+box aren't visible
    textbox_position_portrait box, *st, backdrop
   END IF
  END IF
  'IF box.portrait_type = portraitNONE THEN set_disabled  'Not implemented
 END IF

 section "Audio"
 defitem "Music:"
 edit_as_song offset_int(-1, box.music), -2, 'Preview_Audio
 IF value = -2 THEN
  set_caption "Silence"
 ELSEIF value = -1 THEN
  set_caption "None"
 END IF
 defbool "Restore Map Music Afterwards:", box.restore_music

 defitem "Sound Effect:"
 edit_as_sfx offset_int(-1, box.sound_effect), , 'Preview_Audio
 IF box.sound_effect > 0 THEN
  defbool "Stop Sound Afterwards:", box.stop_sound_after
 END IF

 defitem "Line Sound:"
 default_effective_value -1, gen(genTextboxLine) - 1
 edit_as_sfx offset_int(-1, box.line_sound), -2, 'Preview_Audio
 IF value = -2 THEN set_caption "None"

 'Update textbox preview (refresh happens when state.need_update is true)
 IF phase = Phases.Refreshing THEN
  init_text_box_slices st->textbox_sl, box, st->rootsl, YES
  frame_unload @backdrop
  IF boxp->backdrop > 0 THEN
   backdrop = frame_load(sprTypeBackdrop, boxp->backdrop - 1)
  END IF
 END IF
END SUB

'==============================================================================


SUB textbox_seek(byref box as TextBox, byref st as TextboxEditState)
 DIM remember_id as integer = st.id
 st.id += 1
 DIM foundstr as bool = NO
 DO
  IF st.id > gen(genMaxTextBox) THEN st.id = 0
  IF st.id = remember_id THEN
   notification "Not found.", YES  'shrink=YES
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

SUB textbox_create_from_box_and_load (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
 gen(genMaxTextBox) += 1
 st.id = gen(genMaxTextBox)
 textbox_create_from_box template_box_id, box, st
 textbox_edit_load box, st
END SUB

SUB textbox_link_to_new_box_and_load (byval template_box_id as integer=0, byref box as TextBox, byref st as TextboxEditState)
 textbox_set_after_textbox box, gen(genMaxTextBox) + 1
 SaveTextBox box, st.id
 textbox_create_from_box_and_load template_box_id, box, st
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
  '(Delay REDIM'ing lines(), split_line_positions doesn't like it)
  'Also trim those extra lines from the input string.
  DIM line_starts() as integer
  split_line_positions text, lines(), line_starts()
  text = LEFT(text, line_starts(maxTextboxLines) - 1)
  'Now trim lines()
  REDIM PRESERVE lines(maxTextboxLines - 1)
 END IF
 a_copy lines(), box.text()
 RETURN YES
END FUNCTION

SUB textbox_line_editor (byref box as TextBox, byref st as TextboxEditState)
 DIM text as string = textbox_lines_to_string(box)

 DIM boxslice as Slice ptr = LookupSliceSafe(SL_TEXTBOX_BOX, st.rootsl)
 DIM textslice as Slice ptr = LookupSliceSafe(SL_TEXTBOX_TEXT, st.rootsl, slText)
 DIM txtdata as TextSliceData Ptr = textslice->TextData
 WITH *txtdata
  .line_limit = maxTextboxLines  'No longer actually used?
  .insert = LEN(text)  'End of text
  .show_insert = YES
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
   txtdata->insert = small(newinsert, LEN(newtext))  'May be past end if newtext got trimmed
   IF newtext <> text THEN
    text = newtext
    ChangeTextSlice textslice, text
    boxslice->Height = get_text_box_height(box)
    'The choicebox position would need updating too, but it's hidden
   END IF
  END IF

  clearpage dpage
  'Display the textbox with the text raised above the portrait (not really sure if necessary)
  textbox_edit_preview box, st, dpage, 4, YES
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

 txtdata->show_insert = NO
END SUB


'========================== Textbox Choice Editor =============================


TYPE ChoiceEditor EXTENDS EditorKit
 DECLARE SUB define_items()
 boxp as TextBox ptr
END TYPE

SUB textbox_choice_editor (byref box as TextBox, byref st as TextboxEditState)
 DIM editor as ChoiceEditor
 editor.boxp = @box
 editor.helpkey = "textbox_choice_editor"
 editor.run()
 init_text_box_slices st.textbox_sl, box, st.rootsl, YES  'Update preview
END SUB

SUB ChoiceEditor.define_items()
 DIM byref box as TextBox = *boxp
 defbool "Choice:", box.choice_enabled
 captions_bool "Disabled", "Enabled"
 IF value = NO THEN EXIT SUB
 FOR i as integer = 0 TO 1
  defstr "Option " & i & " text:", box.choice(i), 15
  defitem ""
  edit_as_set_tag box.choice_tag(i)
 NEXT i
END SUB


'========================== Textbox Connections Viewer ========================


SUB textbox_connections(byref box as TextBox, byref st as TextboxEditState)
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
 DIM nxt_add_tag as integer
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
      textbox_edit_load box, st
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
      SELECT CASE twochoice("With a condition?", "Always", "Check a tag", 0, -1)
       CASE -1: nxt_add_type = -1 'Cancelled
       CASE 0: nxt_add_tag = -1 'Always
       CASE 1:
        nxt_add_tag = tags_menu(0, YES, YES, YES)
        IF nxt_add_tag = 0 THEN nxt_add_type = -1 'Cancelled
      END SELECT
      IF nxt_add_type >= 0 THEN
       '--Add a box
       gen(genMaxTextbox) += 1
       IF nxt_add_type = 0 THEN
        '--an after box
        remember_insert = box.after
        remember_insert_tag = box.after_tag
        box.after_tag = nxt_add_tag
        box.after = gen(genMaxTextbox)
       ELSEIF nxt_add_type = 1 THEN
        '--an instead box
        remember_insert = box.instead
        remember_insert_tag = box.instead_tag
        box.instead_tag = nxt_add_tag
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
       textbox_edit_load box, st
       do_search = YES
      END IF
     ELSE
      'Navigate to a box
      IF nxt(nxt_state.pt).id >= 0 THEN
       st.id = nxt(nxt_state.pt).id
       textbox_edit_load box, st
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
 warn &= !"\n" & s
END SUB

SUB import_save_textbox(box as TextBox, byref index as integer, byref warn_append as integer)
 IF index > gen(genMaxTextbox) THEN
  warn_append += index - gen(genMaxTextbox)
  gen(genMaxTextbox) = index
 END IF
 SaveTextBox box, index
 index += 1
END SUB

FUNCTION import_textboxes (filename as string, byref warn as string) as bool
 DIM fh as integer
 IF OPENFILE(filename, FOR_INPUT, fh) THEN
  import_textboxes_warn warn, "Failed to open """ & decode_filename(filename) & """."
  RETURN NO
 END IF
 DIM warn_length as integer = 0
 DIM warn_skip as integer = 0
 DIM warn_append as integer = 0
 DIM show_encoding_warnings as bool = YES
 DIM box as TextBox
 DIM index as integer = 0
 DIM mode as integer = 0
 DIM s as string
 DIM firstline as bool = YES
 DIM line_number as integer = 0
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
     DIM getindex as integer = VALINT(MID(s, 5))
     IF getindex > index THEN
      warn_skip += 1
      debug "import_textboxes: line " & line_number & ": box ID " & index & " is not in the txt file"
     END IF
     IF getindex < index THEN
      debug "import_textboxes: line " & line_number & ": box ID numbers out-of-order. Expected " & index & ", but found " & getindex
     END IF
     index = getindex
     IF index <= gen(genMaxTextBox) THEN
      LoadTextBox box, index
     ELSE
      LoadTextBox box, 0  'Copy style from the template
     END IF
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
     import_save_textbox(box, index, warn_append)
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
       CASE "box style", "border color"    'Obsolete name from previously exported files
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
     import_save_textbox(box, index, warn_append)
     mode = 0
    ELSE
     IF UBOUND(box.text) + 1 = maxTextboxLines THEN
      import_textboxes_warn warn, "line " & line_number & ": too many lines in box " & index & ". Overflowed with """ & s & """."
      CLOSE #fh
      RETURN NO
     END IF
     IF LEN(s) > 38 THEN '--this should be down here
      warn_length += 1
      debug "import_textboxes: line " & line_number & " too long: """ & s & """"
      s = LEFT(s, 38)
     END IF
     a_append box.text(), s
    END IF
  END SELECT
 LOOP
 IF mode = 2 THEN'--Save the last box
  import_save_textbox(box, index, warn_append)
 ELSEIF mode = 0 THEN '--this... is not good
  import_textboxes_warn warn, "line " & line_number & ": txt file ended unexpectedly."
  CLOSE #fh
  RETURN NO
 END IF
 IF warn_length > 0 THEN import_textboxes_warn warn, warn_length & " lines were too long. See the debug log (c_debug.txt) for a list of trimmed lines."
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
   PRINT #fh, "Text Color: " & box.textcolor
   PRINT #fh, "Box Style: " & box.boxstyle  'Used to be exported as "Border Color"
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
