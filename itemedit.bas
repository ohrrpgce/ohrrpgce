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

DECLARE SUB individual_item_editor(item_id as integer)
DECLARE SUB item_editor_equipbits(itembuf() as integer)
DECLARE SUB item_editor_elementals(itembuf() as integer)
DECLARE SUB item_editor_init_new(itembuf() as integer)
DECLARE SUB item_editor_stat_bonuses(itembuf() as integer)


SUB item_editor
 DIM menu() as string
 DIM menu_display() as string
 DIM shaded() as bool

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.autosize = YES
 state.size = 20  'Temp to avoid glitch when size=0
 state.first = -1
 state.top = -1
 state.last = gen(genMaxItem) + 1
 state.need_update = YES

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "item_editor_pickitem"
  IF cropafter_keycombo(YES) AND state.pt >= 0 THEN
   cropafter state.pt, gen(genMaxItem), 0, game + ".itm", getbinsize(binITM)
   load_special_tag_caches
   state.need_update = YES
  END IF
  usemenu state
  IF enter_space_click(state) THEN
   IF state.pt = -1 THEN EXIT DO
   IF state.pt = gen(genMaxItem) + 1 THEN
    'Add new item
    IF gen(genMaxItem) < maxMaxItems THEN
     gen(genMaxItem) += 1
     DIM itembuf(dimbinsize(binITM)) as integer
     item_editor_init_new itembuf()
     saveitemdata itembuf(), state.pt
     state.need_update = YES
    END IF
   END IF
   IF state.pt <= gen(genMaxItem) THEN
    individual_item_editor state.pt
    state.need_update = YES
   END IF
  END IF

  IF state.need_update THEN
   state.need_update = NO
   state.last = gen(genMaxItem) + 1

   REDIM shaded(-1 TO state.last)
   FOR i as integer = LBOUND(shaded) TO UBOUND(shaded)
    shaded(i) = NO
   NEXT

   REDIM menu(-1 TO state.last)
   REDIM menu_display(-1 TO state.last)
   menu(-1) = "Return to Main Menu"
   FOR i as integer = 0 TO gen(genMaxItem)
    menu(i) = i & " " & readitemname(i)
   NEXT
   IF gen(genMaxItem) < maxMaxItems THEN
    menu(UBOUND(menu)) = "Add a new item"
   ELSE
    menu(UBOUND(menu)) = "No more items can be added"
    shaded(UBOUND(menu)) = YES
   END IF
  END IF
  IF select_by_typing(selectst) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, shaded(), 0, 0, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB individual_item_editor(item_id as integer)
 DIM itembuf(dimbinsize(binITM)) as integer
 CONST menusize as integer = 21
 DIM menu(menusize) as string
 DIM menu_display(menusize) as string
 DIM shaded(menusize) as bool
 DIM wep_img as GraphicPair
 DIM box_preview as string = ""

 DIM eqst(5) as string
 eqst(0) = "NEVER EQUIPPED"
 eqst(1) = readglobalstring(38, "Weapon", 10)
 FOR i as integer = 0 TO 3
  eqst(i + 2) = readglobalstring(25 + i, "Armor" & i+1)
 NEXT i

 loaditemdata itembuf(), item_id
 DIM item_name as string, info as string
 'item_name and info aren't rewritten to itembuf() after changes until the menu is quit
 item_name = readbadbinstring(itembuf(), 0, 8)
 info = readbadbinstring(itembuf(), 9, 36)

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
 iidx(5)  = 49 'equippable as
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

 DIM selectst as SelectTypeState
 DIM enable_strgrabber as bool
 DIM state as MenuState
 state.last = menusize
 state.need_update = YES
 state.autosize = YES
 state.autosize_ignore_pixels = 12

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "item_editor"
  usemenu state
  enable_strgrabber = NO
  IF LEN(selectst.query) = 0 AND (state.pt = 1 OR state.pt = 2) THEN
   enable_strgrabber = YES
  END IF
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF itembuf(49) = 1 THEN
    IF state.pt = 17 THEN
     xy_position_on_sprite wep_img, itembuf(80), itembuf(81), 0, 24, 24, "weapon handle position", "xy_weapon_handle"
     state.need_update = YES
    END IF
    IF state.pt = 18 THEN
     xy_position_on_sprite wep_img, itembuf(78), itembuf(79), 1, 24, 24, "weapon handle position", "xy_weapon_handle"
     state.need_update = YES
    END IF
   END IF
   IF itembuf(49) > 0 THEN
    IF state.pt = 19 THEN
     item_editor_stat_bonuses itembuf()
     state.need_update = YES
    END IF
    IF state.pt = 20 THEN
     item_editor_elementals itembuf()
    END IF
    IF state.pt = 21 THEN
     item_editor_equipbits itembuf()
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
   CASE 3, 4, 5, 10, 15, 16
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
     DIM addwhat as integer = twochoice("Link to what?", "A textbox", "An attack")
     IF addwhat = 0 THEN itembuf(51) = -large(0, text_box_editor(IIF(wantnew, 999999, 1)))
     IF addwhat = 1 THEN itembuf(51) = 1 + attack_editor(IIF(wantnew, 999999, 0))
     state.need_update = YES
    END IF
   CASE 11 TO 14
    IF tag_set_grabber(itembuf(74 + (state.pt - 11)), state, 0) THEN
     state.need_update = YES
     'Update cache
     itemtags(item_id).have_tag = itembuf(74)
     itemtags(item_id).in_inventory_tag = itembuf(75)
     itemtags(item_id).is_equipped_tag = itembuf(76)
     itemtags(item_id).is_actively_equipped_tag = itembuf(77)
    END IF
  END SELECT
  IF state.need_update THEN
   state.need_update = NO
   generate_item_edit_menu menu(), shaded(), itembuf(), item_name, info, eqst(), box_preview
   load_sprite_and_pal wep_img, sprTypeWeapon, itembuf(52), itembuf(53)
  END IF
  IF enable_strgrabber = NO ANDALSO select_by_typing(selectst, NO) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, shaded(), 0, 0, dpage
  IF itembuf(49) = 1 THEN
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

  IF state.pt >= 6 AND state.pt <= 9 THEN
   'Editing a textbox/attack ID. Move it up if a textbox is being previewed
   DIM y as RelPos = pBottom
   IF itembuf(51) < 0 THEN y -= 10
   edgeprint "ENTER to edit, + or INSERT to add new", 0, y, uilook(uiDisabledItem), dpage
  END IF
  IF itembuf(51) < 0 THEN
   edgeprint box_preview, 0, pBottom, uilook(uiText), dpage
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 unload_sprite_and_pal wep_img
 writebadbinstring item_name, itembuf(), 0, 8
 writebadbinstring info, itembuf(), 9, 36
 saveitemdata itembuf(), item_id
END SUB

SUB generate_item_edit_menu (menu() as string, shaded() as bool, itembuf() as integer, item_name as string, info_string as string, equip_types() as string, byref box_preview as string)
 DIM weapon as string = readglobalstring(38, "Weapon", 10)
 menu(0) = "Back to Item Menu"
 menu(1) = "Name:" & item_name
 menu(2) = "Info:" & info_string
 menu(3) = "Value: " & itembuf(46)
 menu(4) = "Maximum stack size: " & defaultint(itembuf(210), "Default (" & gen(genItemStackSize) & ")", 0)
 menu(5) = "Equippable as: " & equip_types(bound(itembuf(49), 0, 5))
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
 IF itembuf(49) <> 1 THEN  'Not a weapon
  menu(7) = "When used as a " & weapon & ": N/A"
  shaded(7) = YES
  menu(15) = "Weapon Picture: N/A"
  menu(16) = "Weapon Palette: N/A"
  shaded(15) = YES
  shaded(16) = YES
  shaded(17) = YES
  shaded(18) = YES
 END IF
 IF itembuf(49) = 0 THEN  'Not equipable
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
SUB item_editor_equipbits(itembuf() as integer)
 DIM hero_id as integer
 ' The equippable bits are discontinuous
 DIM combined_bits(maxMaxHero \ 16) as integer
 DIM bitnames(-1 TO maxMaxHero) as string
 FOR hero_id = 0 TO gen(genMaxHero)
  bitnames(hero_id) = "Equippable by " & getheroname(hero_id)
  setbit combined_bits(), 0, hero_id, item_read_equipbit(itembuf(), hero_id)
 NEXT
 editbitset combined_bits(), 0, gen(genMaxHero), bitnames()
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
  elementnames(i) = rpad(elementnames(i), " ", 15)
 NEXT

 DO
  setwait 55
  setkeys YES
  IF keyval(scEsc) > 1 THEN EXIT DO
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
 FOR i as integer = 0 TO maxElements - 1
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

 DIM sbmax(statLast) as integer
 FOR i as integer = 0 TO 1
  sbmax(i) = 9999
 NEXT i
 FOR i as integer = 2 TO 8
  sbmax(i) = 999
 NEXT i
 FOR i as integer = 9 TO 10
  sbmax(i) = 100
 NEXT i
 sbmax(11) = 10

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "equipment_stat_bonuses"
  usemenu state
  IF enter_space_click(state) THEN
   IF state.pt = -1 THEN EXIT DO
  END IF
  IF state.pt >= 0 THEN
   IF intgrabber(itembuf(54 + state.pt), sbmax(state.pt) * -1, sbmax(state.pt)) THEN
    state.need_update = YES
   END IF
  END IF

  IF state.need_update THEN
   state.need_update = NO
   FOR i as integer = 0 TO statLast
    menu(i) = statnames(i) + " Bonus: " & itembuf(54 + i)
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
