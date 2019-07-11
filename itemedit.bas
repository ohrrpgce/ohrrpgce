'OHRRPGCE CUSTOM - Item Editor
'(C) Copyright 1997-2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
#include "config.bi"
#include "const.bi"
#include "udts.bi"
#include "custom.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "thingbrowser.bi"
#include "cglobals.bi"


'--Local SUBs
DECLARE FUNCTION item_attack_name(n as integer) as string
DECLARE SUB generate_item_edit_menu (menu() as string, shaded() as bool, itembuf() as integer, item_name as string, info_string as string, equip_types() as string, byref box_preview as string)

DECLARE SUB item_editor_equipbits(itembuf() as integer, itemname as string)
DECLARE SUB item_editor_elementals(itembuf() as integer)
DECLARE SUB item_editor_init_new(itembuf() as integer)
DECLARE SUB item_editor_stat_bonuses(itembuf() as integer)

SUB item_editor ()
 DIM itemb as ItemBrowser
 itemb.browse(-1, , @individual_item_editor)
END SUB

' an FnEditor
FUNCTION item_picker (recindex as integer = -1) as integer
 DIM itemb as ItemBrowser
 RETURN itemb.browse(recindex, , @individual_item_editor, NO)
END FUNCTION

FUNCTION item_picker_or_none (recindex as integer = -1) as integer
 DIM itemb as ItemBrowser
 RETURN itemb.browse(recindex - 1, YES , @individual_item_editor, NO) + 1
END FUNCTION

PRIVATE SUB read_item_strings(itembuf() as integer, byref item_name as string, byref info as string)
 item_name = readbadbinstring(itembuf(), 0, 8)
 info = readbadbinstring(itembuf(), 9, 36)
END SUB

PRIVATE SUB write_item_strings(itembuf() as integer, item_name as string, info as string)
 writebadbinstring item_name, itembuf(), 0, 8
 writebadbinstring info, itembuf(), 9, 36
END SUB

FUNCTION individual_item_editor(item_id as integer) as integer
'Return value is the item_id (for thingbrowser)

 STATIC clipboard_used as bool  'There's something in clipboard_buf
 STATIC clipboard_buf(dimbinsize(binITM)) as integer  'For copy/pasting
 DIM undo_available as bool  'There's something in undobuf
 DIM undobuf(dimbinsize(binITM)) as integer  'Just to undo pasting

 DIM itembuf(dimbinsize(binITM)) as integer

 'Add new item
 IF item_id > maxMaxItems THEN
  visible_debug "Can't edit item id > " & maxMaxItems
  RETURN -1
 END IF
 IF item_id > gen(genMaxItem) THEN
  gen(genMaxItem) += 1
  item_id = gen(genMaxItem)
  item_editor_init_new itembuf()
  saveitemdata itembuf(), item_id
 END IF
 IF item_id > UBOUND(itemtags) THEN
  load_special_tag_caches
 END IF

 CONST menusize as integer = 21
 DIM menu(menusize) as string
 DIM menu_display(menusize) as string
 DIM shaded(menusize) as bool
 DIM wep_img as GraphicPair
 DIM box_preview as string = ""

 DIM eqst(4) as string
 eqst(0) = readglobalstring(38, "Weapon", 10)
 FOR i as integer = 0 TO 3
  eqst(i + 1) = readglobalstring(25 + i, "Armor" & i+1)
 NEXT i

 loaditemdata itembuf(), item_id
 DIM item_name as string, info as string
 'item_name and info are written to itembuf() after any change
 read_item_strings itembuf(), item_name, info

 'Indexed by menu index, not by .itm/itembuf() index
 DIM max(menusize) as integer
 DIM min(menusize) as integer
 max(3) = 32767
 max(4) = 99
 max(5) = 5
 max(6) = gen(genMaxAttack)  'These are not used or kept up-to-date!
 max(7) = gen(genMaxAttack)
 max(8) = gen(genMaxAttack)
 max(9) = gen(genMaxAttack)
 max(10) = 2
 max(11) = max_tag()
 max(12) = max_tag()
 max(13) = max_tag()
 max(14) = max_tag()
 max(15) = gen(genMaxWeaponPic)
 min(16) = -1
 max(16) = 32767

 'Map from menu() indices to itembuf() indices
 DIM iidx(menusize) as integer
 iidx(3)  = 46 'value
 iidx(4)  = 210 'stack size
 'iidx(5)  = 49 'equippable as (obsolete, replaced by bitsets in int 239)
 iidx(6)  = 47 'in battle use
 iidx(7)  = 48 'weapon use
 iidx(8)  = 50 'teach spell
 iidx(9)  = 51 'out of battle use
 iidx(10) = 73 'consumption
 iidx(11) = 74 'own item tag
 iidx(12) = 75 'in inventory tag
 iidx(13) = 76 'equipped tag
 iidx(14) = 77 'actively equipped tag
 iidx(15) = 52 'weapon pic
 iidx(16) = 53 'weapon pal

 DIM elementnames() as string
 getelementnames elementnames()

 DIM bitnames(0 TO 47) as string
 FOR i as integer = 0 TO 7
  'Lots of obsolete elemental bits
  DIM elname as string
  elname = IIF(i <= UBOUND(elementnames), elementnames(i), "element" & i)
  bitnames(i) = "##Weak against " & elname & " (obsolete)"
  bitnames(i + 8) = "##Strong against " & elname & " (obsolete)"
  bitnames(i + 16) = "##Absorbs " & elname & " (obsolete)"
 NEXT

 DIM selectst as SelectTypeState
 DIM enable_strgrabber as bool
 DIM state as MenuState
 state.last = menusize
 state.need_update = YES
 state.autosize = YES
 state.autosize_ignore_lines = 2
 state.autosize_ignore_pixels = 4
 DIM menuopts as MenuOptions
 menuopts.scrollbar = YES  'FIXME: this really needs to be the default!

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "item_editor"
  usemenu state
  enable_strgrabber = NO
  IF LEN(selectst.query) = 0 AND (state.pt = 1 OR state.pt = 2) THEN
   enable_strgrabber = YES
  END IF
  IF enable_strgrabber = NO ANDALSO keyval(scAlt) > 0 THEN
   'Alt-C/V to copy/paste. Not available when a text field
   'selected, because Alt-C/V input characters.
   IF keyval(scC) > 1 THEN
    a_copy itembuf(), clipboard_buf()
    clipboard_used = YES
    show_overlay_message "Copied item", 0.75
   END IF
   IF clipboard_used ANDALSO keyval(scV) > 1 THEN
    a_copy itembuf(), undobuf()
    undo_available = YES
    a_copy clipboard_buf(), itembuf()
    read_item_strings itembuf(), item_name, info
    state.need_update = YES
    show_overlay_message "Pasted item (Ctrl-Z to undo)", 1.1
   END IF
  END IF
  IF undo_available ANDALSO keyval(scCtrl) > 0 ANDALSO keyval(scZ) > 1 THEN
   'Ctrl-Z to undo paste
   a_copy undobuf(), itembuf()
   read_item_strings itembuf(), item_name, info
   state.need_update = YES
   show_overlay_message "Undid paste", 0.75
  END IF
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF item_is_equippable_in_slot(itembuf(), 0) THEN
    IF state.pt = 17 THEN
     xy_position_on_sprite wep_img, itembuf(80), itembuf(81), 0, "Weapon handle position", "xy_weapon_handle"
     state.need_update = YES
    END IF
    IF state.pt = 18 THEN
     xy_position_on_sprite wep_img, itembuf(78), itembuf(79), 1, "Weapon handle position", "xy_weapon_handle"
     state.need_update = YES
    END IF
   END IF
   IF item_is_equippable(itembuf()) THEN
    IF state.pt = 19 THEN
     item_editor_stat_bonuses itembuf()
     state.need_update = YES
    END IF
    IF state.pt = 20 THEN
     item_editor_elementals itembuf()
    END IF
    IF state.pt = 21 THEN
     item_editor_equipbits itembuf(), item_name
     state.need_update = YES
    END IF
   END IF
   IF state.pt = 15 THEN 'sprite browser
    DIM weaponb as WeaponSpriteBrowser
    itembuf(52) = weaponb.browse(itembuf(52))
    state.need_update = YES
   END IF
   IF state.pt = 16 THEN '--palette picker
    itembuf(53) = pal16browse(itembuf(53), sprTypeWeapon, itembuf(52), YES)
    state.need_update = YES
   END IF
   IF state.pt = 5 THEN
    editbitset itembuf(), 239, eqst()
    state.need_update = YES
   END IF
   IF state.pt = 10 THEN
    DIM captions(2) as string = {"Unlimited Use", "Consumed By Use", "Cannot be Sold/Dropped"}
    DIM b as ArrayBrowser = ArrayBrowser(captions(), "Consumability")
    itembuf(73) = b.browse(itembuf(73))
    state.need_update = YES
   END IF
  END IF
  SELECT CASE state.pt
   CASE 1
    IF enable_strgrabber ANDALSO strgrabber(item_name, 8) THEN
     state.need_update = YES
    END IF
   CASE 2
    IF enable_strgrabber ANDALSO strgrabber(info, 36) THEN
     state.need_update = YES
    END IF
   CASE 3, 4, 10, 15, 16
    IF intgrabber(itembuf(iidx(state.pt)), min(state.pt), max(state.pt)) THEN
     state.need_update = YES
    END IF
   CASE 6, 7, 8  'Attacks
    IF attackgrabber(itembuf(iidx(state.pt)), state, 1, 0) THEN
     state.need_update = YES
    END IF
   CASE 9
    'Out-of-battle use can be either an attack or a textbox
    'We want attack/textboxgrabber to handle +/ins and enter/space/click when an
    'attack/textbox is selected, and xintgrabber to handle other keys.
    DIM boxnum as integer = -itembuf(51)
    IF keyval(scPlus) = 0 ANDALSO xintgrabber(itembuf(51), 0, gen(genMaxAttack), -1, gen(genMaxTextbox) * -1) THEN
     state.need_update = YES
    ELSEIF itembuf(51) < 0 ANDALSO textboxgrabber(boxnum, state, 0, , NO) THEN  'intgrab=NO
     itembuf(51) = -boxnum
     state.need_update = YES
    ELSEIF itembuf(51) > 0 ANDALSO attackgrabber(itembuf(51), state, 1, , NO) THEN  'intgrab=NO
     state.need_update = YES
    ELSEIF enter_or_add_new(state) THEN
     'Yikes... never cram multiple data types in the same option!!
     '(OK, this is overkill, since James will hopefully come along and replace this soon!)
     DIM wantnew as bool = NOT enter_space_click(state)
     SELECT CASE twochoice("Link to what?", "A textbox", "An attack")
      CASE 0: itembuf(51) = textbox_picker(IIF(wantnew, 999999, 1)) * -1
      CASE 1: itembuf(51) = attack_picker_or_none(IIF(wantnew, 999999, 0))
     END SELECT
     state.need_update = YES
    END IF
   CASE 11 TO 14
    IF tag_id_grabber(itembuf(74 + (state.pt - 11)), state) THEN
     state.need_update = YES
     'Update itemtags() cache
     item_tags_from_buf itemtags(item_id), itembuf()
    END IF
  END SELECT
  IF keyval(scCtrl) > 0 ANDALSO keyval(scB) > 1 THEN  'Ctrl+B debug key: edit all bits... except equipability
   editbitset itembuf(), 70, bitnames(), , , , , , YES, YES 'show_index = show_all = YES
   state.need_update = YES
  END IF

  IF state.need_update THEN
   state.need_update = NO
   write_item_strings itembuf(), item_name, info
   generate_item_edit_menu menu(), shaded(), itembuf(), item_name, info, eqst(), box_preview
   load_sprite_and_pal wep_img, sprTypeWeapon, itembuf(52), itembuf(53)
  END IF
  IF enable_strgrabber = NO ANDALSO keyval(scAlt) = 0 ANDALSO select_by_typing(selectst, NO) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, shaded(), 0, 0, dpage, menuopts
  IF item_is_equippable_in_slot(itembuf(), 0) THEN
   'Is a weapon
   DIM frame as integer = 0
   IF state.pt = 17 THEN frame = 1
   frame_draw wep_img.sprite + 1 - frame, wep_img.pal, 280, 160, , , dpage
   DIM handle as XYPair
   handle.x = 280 + itembuf(78 + frame * 2)
   handle.y = 160 + itembuf(79 + frame * 2)
   DIM col as integer = uilook(uiSelectedItem) + state.tog
   drawline handle.x - 2, handle.y    , handle.x - 1, handle.y    , col, dpage
   drawline handle.x    , handle.y - 2, handle.x    , handle.y - 1, col, dpage
   drawline handle.x + 1, handle.y    , handle.x + 2, handle.y    , col, dpage
   drawline handle.x    , handle.y + 1, handle.x    , handle.y + 2, col, dpage
  END IF

  'Tooltip and textbox preview
  DIM tooltipy as RelPos = pBottom
  IF itembuf(51) < 0 THEN
   edgeprint box_preview, 0, pBottom, uilook(uiText), dpage
   tooltipy -= 10
  END IF
  DIM tooltip as string
  IF state.pt >= 6 AND state.pt <= 9 THEN 'Editing a textbox/attack ID
   tooltip = THINGGRABBER_TOOLTIP
  ELSEIF state.pt = 0 THEN
   'These keys work when many other menu items, aside from "Back to Item Menu",
   'are selected too, but I think it's bad to show tooltips not specific to the current menu item,
   'so only show it in one place (so the keys are discoverable).
   IF clipboard_used THEN
    tooltip = "Alt-C/V to copy/paste item definition"
   ELSE
    tooltip = "Alt-C to copy item definition"
   END IF
  END IF
  edgeprint tooltip, 0, tooltipy, uilook(uiDisabledItem), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 unload_sprite_and_pal wep_img
 saveitemdata itembuf(), item_id
 RETURN item_id
END FUNCTION

SUB generate_item_edit_menu (menu() as string, shaded() as bool, itembuf() as integer, item_name as string, info_string as string, equip_types() as string, byref box_preview as string)
 DIM weapon as string = readglobalstring(38, "Weapon", 10)
 menu(0) = "Back to Item Menu"
 menu(1) = "Name:" & item_name
 menu(2) = "Info:" & info_string
 menu(3) = "Value: " & itembuf(46)
 menu(4) = "Maximum stack size: " & defaultint(itembuf(210), "Default (" & gen(genItemStackSize) & ")", 0)

 DIM is_equippable as bool = NO
 DIM is_weapon as bool = item_is_equippable_in_slot(itembuf(), 0)
 menu(5) = "Equippable as:"
 DIM sep as string = " "
 FOR i as integer = 0 TO 4
  IF item_is_equippable_in_slot(itembuf(), i) THEN
   menu(5) &= sep & equip_types(i)
   sep = "/"
   is_equippable = YES
  END IF
 NEXT i
 IF NOT is_equippable THEN menu(5) &= " NEVER EQUIPPED"
 
 menu(6) = "When used in battle: " & item_attack_name(itembuf(47))
 menu(7) = "When used as a " & weapon & ": " & item_attack_name(itembuf(48))
 menu(8) = "Teach Spell: " & item_attack_name(itembuf(50))
 IF itembuf(51) > 0 THEN
  menu(9) = "When used out of battle: " & item_attack_name(itembuf(51))
  box_preview = ""
 ELSEIF itembuf(51) = 0 THEN
  menu(9) = "When used out of battle: NO ATTACK/TEXTBOX"
  box_preview = ""
 ELSE
  menu(9) = "When used out of battle: Text " & ABS(itembuf(51))
  box_preview = textbox_preview_line(ABS(itembuf(51)))
 END IF
 menu(10) = "Unlimited Use"
 IF itembuf(73) = 1 THEN menu(10) = "Consumed By Use"
 IF itembuf(73) = 2 THEN menu(10) = "Cannot be Sold/Dropped"
 menu(11) = "Own item Tag " & itembuf(74) & " " & load_tag_name(itembuf(74))
 menu(12) = "Is in inventory Tag " & itembuf(75) & " " & load_tag_name(itembuf(75))
 menu(13) = "Is equipped Tag " & itembuf(76) & " " & load_tag_name(itembuf(76))
 menu(14) = "Equipped by active hero Tag " & itembuf(77) & " " & load_tag_name(itembuf(77))
 menu(15) = "Weapon Picture: " & itembuf(52)
 menu(16) = "Weapon Palette: " & defaultint(itembuf(53))
 menu(17) = "Handle position A..."
 menu(18) = "Handle position B..."
 menu(19) = "Stat Bonuses..."
 menu(20) = "Elemental Resists..."
 'menu(20) = "Equipment Bits..."
 menu(21) = "Who Can Equip?..."

 FOR i as integer = 0 TO UBOUND(shaded)
  shaded(i) = NO
 NEXT
 IF NOT is_weapon THEN
  menu(7) = "When used as a " & weapon & ": N/A"
  shaded(7) = YES
  menu(15) = "Weapon Picture: N/A"
  menu(16) = "Weapon Palette: N/A"
  shaded(15) = YES
  shaded(16) = YES
  shaded(17) = YES
  shaded(18) = YES
 END IF
 IF NOT is_equippable THEN
  'Don't N/A the tags, because they still take effect
  shaded(13) = YES
  shaded(14) = YES
  shaded(19) = YES
  shaded(20) = YES
  shaded(21) = YES
 END IF

END SUB

FUNCTION item_attack_name(n as integer) as string
 IF n <= 0 THEN RETURN "NOTHING"
 RETURN n - 1 & " " & readattackname(n - 1)
END FUNCTION

' Who Can Equip? menu
SUB item_editor_equipbits(itembuf() as integer, itemname as string)
 DIM hero_id as integer
 ' The equippable bits are discontinuous
 DIM combined_bits(maxMaxHero \ 16) as integer
 DIM bitnames(-1 TO maxMaxHero) as string
 FOR hero_id = 0 TO gen(genMaxHero)
  bitnames(hero_id) = getheroname(hero_id)
  IF LEN(bitnames(hero_id)) = 0 THEN bitnames(hero_id) = "Hero " & hero_id
  setbit combined_bits(), 0, hero_id, item_read_equipbit(itembuf(), hero_id)
 NEXT
 editbitset combined_bits(), 0, bitnames(), , , , itemname & " is equippable by..."
 FOR hero_id = 0 TO gen(genMaxHero)
  item_write_equipbit(itembuf(), hero_id, xreadbit(combined_bits(), hero_id))
 NEXT
END SUB

'This elemental resistance editor is shared by the hero and item editors
SUB common_elementals_editor(elementals() as single, helpfile as string, byval showsign as integer = 0)
 DIM elementnames() as string
 getelementnames elementnames()
 DIM float_reprs(gen(genNumElements) - 1) as string
 DIM menu(1 + gen(genNumElements) - 1) as string
 DIM menu_display(UBOUND(menu)) as string
 DIM selectst as SelectTypeState
 DIM st as MenuState
 st.last = UBOUND(menu)
 st.autosize = YES
 st.need_update = YES
 DIM menuopts as MenuOptions
 menuopts.scrollbar = YES

 FOR i as integer = 0 TO gen(genNumElements) - 1
  float_reprs(i) = format_percent(elementals(i))
  elementnames(i) = rpad(elementnames(i), " ", 15, clipRight)
 NEXT

 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help helpfile
  IF st.pt = 0 THEN
   IF enter_space_click(st) THEN EXIT DO
  ELSE
   IF percent_grabber(elementals(st.pt - 1), float_reprs(st.pt - 1), -1000, 1000) THEN st.need_update = YES
  END IF
  usemenu st

  IF st.need_update THEN
   st.need_update = NO
   menu(0) = "Previous Menu"
   FOR i as integer = 0 TO gen(genNumElements) - 1
    menu(i + 1) = "Damage from " + elementnames(i) + ": "
    IF showsign THEN
     'positive values get explicit + prefix
     IF LEFT(float_reprs(i), 1) <> "-" THEN menu(i + 1) += "+"
    END IF
    menu(i + 1) += float_reprs(i)
   NEXT
  END IF
  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary menu(), selectst, st
  END IF

  clearpage vpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, st
  standardmenu menu_display(), st, 0, 0, vpage, menuopts
  setvispage vpage
  dowait
 LOOP
 setkeys
END SUB

SUB item_editor_elementals(itembuf() as integer)
 DIM elementals(gen(genNumElements) - 1) as single
 FOR i as integer = 0 TO gen(genNumElements) - 1
  elementals(i) = DeSerSingle(itembuf(), 82 + i * 2)
  IF gen(genEquipMergeFormula) = 2 THEN  'additive merging
   elementals(i) -= 1.0
  END IF
 NEXT
 common_elementals_editor elementals(), "item_elementals", (gen(genEquipMergeFormula) = 2)
 FOR i as integer = 0 TO gen(genNumElements) - 1
  IF gen(genEquipMergeFormula) = 2 THEN  'additive merging
   elementals(i) += 1.0
  END IF
  SerSingle itembuf(), 82 + i * 2, elementals(i)
 NEXT
END SUB

SUB item_editor_init_new(itembuf() as integer)
 flusharray itembuf(), dimbinsize(binITM), 0
 FOR i as integer = 0 TO 63
  SerSingle itembuf(), 82 + i * 2, 1.0
 NEXT i
END SUB

SUB item_editor_stat_bonuses(itembuf() as integer)
 DIM menu(-1 TO statLast) as string
 DIM menu_display(-1 TO statLast) as string
 menu(-1) = "Previous Menu"
 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.first = -1
 state.top = -1
 state.last = UBOUND(menu)
 state.size = 24
 state.need_update = YES

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "equipment_stat_bonuses"
  usemenu state
  IF enter_space_click(state) THEN
   IF state.pt = -1 THEN EXIT DO
  END IF
  IF state.pt >= 0 THEN
   IF intgrabber(itembuf(54 + state.pt), -32768, 32767) THEN
    state.need_update = YES
   END IF
  END IF

  IF state.need_update THEN
   state.need_update = NO
   FOR i as integer = 0 TO statLast
    menu(i) = statnames(i) + " Bonus: " & itembuf(54 + i)
    DIM cap as integer = gen(genStatCap + i)
    IF cap > 0 ANDALSO itembuf(54 + i) > cap THEN
     menu(i) &= " [stat capped to " & cap & "]"
    END IF
   NEXT
  END IF
  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB
