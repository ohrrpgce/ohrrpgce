'OHRRPGCE CUSTOM - Item Editor
'(C) Copyright 1997-2025 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
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
#include "editorkit.bi"


'--Local SUBs
DECLARE SUB item_editor_equipbits(item as ItemDef)
DECLARE SUB item_editor_elementals(item as ItemDef)

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

'ITEMFIXME remove this
LOCAL SUB read_item_strings(itembuf() as integer, byref item_name as string, byref info as string)
 item_name = readbadbinstring(itembuf(), 0, 8)
 info = readbadbinstring(itembuf(), 9, 36)
END SUB

'ITEMFIXME remove this
LOCAL SUB write_item_strings(itembuf() as integer, item_name as string, info as string)
 writebadbinstring item_name, itembuf(), 0, 8
 writebadbinstring info, itembuf(), 9, 36
END SUB

SUB populate_eq_slot_names(eq_slot_names() as string)
 eq_slot_names(0) = readglobalstring(38, "Weapon", 10)
 FOR i as integer = 0 TO 3
  eq_slot_names(i + 1) = readglobalstring(25 + i, "Armor" & i+1)
 NEXT i
 FOR i as integer = 0 TO UBOUND(eq_slot_names)
  IF LEN(eq_slot_names(i)) = 0 THEN eq_slot_names(i) = "Equip slot " & i
 NEXT
END SUB

FUNCTION summarize_item_equipability(item as ItemDef) as string
 DIM eq_slot_names(4) as string
 populate_eq_slot_names eq_slot_names()
 DIM summary as string
 DIM sep as string = ""
 FOR i as integer = 0 TO 4
  IF item.eqslots(i) THEN
   summary &= sep & eq_slot_names(i)
   sep = "/"
  END IF
 NEXT i
 IF summary = "" THEN summary = "NEVER EQUIPPED"
 RETURN summary
END FUNCTION

'-----------------------------------------------------------------------

TYPE ItemEditor EXTENDS EditorKit
 DECLARE CONSTRUCTOR(item_id as integer)
 DECLARE DESTRUCTOR
 DECLARE SUB define_items()
 DECLARE SUB load()
 DECLARE SUB save()
 DECLARE SUB save_new()
 DECLARE SUB draw_underlays()
 DECLARE SUB reload_sprite()
 id as integer
 item as ItemDef
 eq_slot_names(4) as string
 underlay as Slice Ptr
 wep_sl as Slice Ptr
 handle_pos_sl as Slice ptr
 preview_wep_frame as integer
 STATIC clipboard_item as ItemDef ptr  'For copy/pasting, NULL if nothing copied
 undo_item as ItemDef ptr  'Just to undo pasting. NULL if nothing
 can_copy_and_paste as bool
END TYPE
DIM ItemEditor.clipboard_item as ItemDef ptr

CONSTRUCTOR ItemEditor(item_id as integer)
 populate_eq_slot_names eq_slot_names()

 'Add a new item if an out-of-range item_id was requested
 IF item_id > gen(genMaxItem) THEN
  id = gen(genMaxItem) + 1
 ELSE
  id = item_id
 ENd IF

 exit_menu_text = "Previous Menu"
 exit_submenu_text = "Back to Item Menu"

 setup_record_switching id, 0, gen(genMaxItem), , "Item", maxMaxItems
 
 'Set up the weapon preview underlay
 underlay = NewSliceOfType(slContainer)
 underlay->Fill = YES
 wep_sl = NewSliceOfType(slSprite)
 SetSliceParent wep_sl, underlay
 ReAlignSlice wep_sl, alignRight, alignCenter, alignRight, alignCenter
 wep_sl->X = -20
 preview_wep_frame = 1
 handle_pos_sl = NewSliceOfType(slRectangle)
 SetSliceParent handle_pos_sl, wep_sl
 handle_pos_sl->x = -1
 handle_pos_sl->y = -1
 handle_pos_sl->Width = 3
 handle_pos_sl->Height = 3
 ReAlignSlice handle_pos_sl, alignLeft, alignLeft, alignCenter, alignCenter
 ChangeRectangleSlice handle_pos_sl, , , uiSelectedItem2 * -1 - 1, borderLine, transHollow
END CONSTRUCTOR

DESTRUCTOR ItemEditor()
 DeleteSlice @underlay
 IF undo_item THEN DELETE undo_item
END DESTRUCTOR

SUB ItemEditor.load()
 IF id > gen(genMaxItem) THEN save_new
 loaditemdata item, id
 reload_sprite
END SUB

SUB ItemEditor.save_new()
 DIM new_item as ItemDef = ItemDef()
 saveitemdata new_item, id
 gen(genMaxItem) = id
 'REDIMs itemtags
 load_special_tag_caches
END SUB

SUB ItemEditor.save()
 saveitemdata item, id
END SUB

SUB ItemEditor.define_items()

 '----------------------------
 IF submenu = "statbonus" THEN
 
 helpkey = "equipment_stat_bonuses"

 FOR i as integer = 0 TO statLast
  defint statnames(i) + " Bonus:", item.stat_bonuses.sta(i), -32768, 32767
  DIM cap as integer = gen(genStatCap + i)
  IF cap > 0 ANDALSO item.stat_bonuses.sta(i) > cap THEN
   set_caption item.stat_bonuses.sta(i) & " [stat capped to " & cap & "]"
  END IF
 NEXT

 '----------------------------
 ELSE '--main menu

 helpkey = "item_editor"

 can_copy_and_paste = YES

 def_record_switcher
 'Only do copy-pasting on the main menu. Not in sub-menus
 '(We don't want to create the false impression that only the contents of the sub-menu would be pasted)
 'The copy-paste is implemented at the end of the main menu definition
 IF selected THEN
  IF clipboard_item THEN
   set_tooltip "Alt-C/V to copy/paste item definition"
  ELSE
   set_tooltip "Alt-C to copy item definition"
  END IF
 END IF
 
 defstr "Name:", item.name, 8
 IF selected THEN can_copy_and_paste = NO
 defstr "Info:", item.info
 IF selected THEN can_copy_and_paste = NO
 IF activate THEN
  item.info = multiline_string_editor(item.info, "multiline_item_description_editor", NO)
  state.need_update = YES
 END IF
 defint "Value:", item.buy_price, 0, 32767

 defint "Maximum stack size:", item.stacksize, 0, 99
 caption_default_or_int 0, "Default (99)"

 'We can split these apart after the switch from ITM to items.reld (if we wish)
 defitem "Consumability:"
 DIM consumability as integer = 0
 IF item.consumed_by_use THEN consumability = 1
 IF item.cannot_be_sold_or_dropped THEN consumability = 2
 DIM usability_captions(...) as string = {"Unlimited Use", "Consumed By Use", "Cannot be Sold/Dropped"}
 edit_int_enum consumability, usability_captions()
 item.consumed_by_use = (consumability = 1)
 item.cannot_be_sold_or_dropped = (consumability = 2)

 IF defitem_act("Equippable as...:") THEN
  editbools item.eqslots(), eq_slot_names()
  reload_sprite
 END IF
 IF refresh THEN set_caption summarize_item_equipability(item)
 
 defitem "When used as an item in battle:"
 edit_as_attack item.battle_items_menu_attack, Or_None
 IF value = -1 THEN set_caption "NOTHING"
 set_tooltip THINGGRABBER_TOOLTIP

 IF item.eqslots(0) THEN
  section "As a weapon"
  preview_wep_frame = 1
  
  defitem "When used as a Weapon:"
  edit_as_attack item.battle_weapon_attack, Or_None
  IF value = -1 THEN set_caption "NOTHING"
  set_tooltip THINGGRABBER_TOOLTIP
  
  defitem "Weapon Picture:"
  IF edit_as_spriteset(item.wep_pic, sprTypeWeapon) THEN
  END IF

  defitem "Weapon Palette:"
  IF edit_as_palette(item.wep_pal, sprTypeWeapon, item.wep_pic) THEN
  END IF
 
  IF defitem_act("Handle position (A)...") THEN
   ChangeSpriteSlice wep_sl, , , , 0
   xy_position_on_sprite_slice wep_sl, item.wep_handle(0).x, item.wep_handle(0).y, "Weapon handle position", "xy_weapon_handle"
  END IF
  IF selected THEN preview_wep_frame = 0

  IF defitem_act("Handle position (B)...") THEN
   ChangeSpriteSlice wep_sl, , , , 1
   xy_position_on_sprite_slice wep_sl, item.wep_handle(1).x, item.wep_handle(1).y, "Weapon handle position", "xy_weapon_handle"
  END IF
  
  reload_sprite
 END IF

 IF item_is_equippable(item) THEN
  IF defitem_act("Stat Bonuses...") THEN 
   enter_submenu "statbonus"
  END IF

  IF defitem_act("Elemental Resists...") THEN
   item_editor_elementals item
  END IF
  
  IF defitem_act("Who Can Equip?...") THEN
   item_editor_equipbits item
  END IF
 END IF
 
 section "When used out of battle"

 defitem "Cure Attack:"
 IF item.text_box >= 0 ORELSE item.teach_spell >= 0 THEN
  item.oob_attack = -1
  set_disabled
 ELSE
  edit_as_attack item.oob_attack, Or_None
  IF value = -1 THEN set_caption "NOTHING"
 END IF
 set_tooltip THINGGRABBER_TOOLTIP

 defitem "Text Box:"
 IF item.oob_attack >= 0 ORELSE item.teach_spell >= 0 THEN
  item.text_box = -1
  set_disabled
 ELSE
  edit_as_textbox item.text_box, Or_None
  IF value = -1 THEN set_caption "NOTHING"
  IF value = 0 THEN set_caption "(Box 0 not supported here)"
 END IF
 set_tooltip THINGGRABBER_TOOLTIP

 defitem "Teach Spell:"
 IF item.oob_attack >= 0 ORELSE item.text_box >= 0 THEN
  item.teach_spell = -1
  set_disabled
 ELSE
  edit_as_attack item.teach_spell, Or_None
  IF value = -1 THEN set_caption "NOTHING"
 END IF
 set_tooltip THINGGRABBER_TOOLTIP

 section "Automatically set tags"

 defitem "Own item:"
 edit_as_tag_id item.tags.have_tag
 IF edited THEN itemtags(id) = item.tags

 defitem "Is in inventory:"
 edit_as_tag_id item.tags.in_inventory_tag
 IF edited THEN itemtags(id) = item.tags

 IF item_is_equippable(item) THEN
  defitem "Equipped by any hero:"
  edit_as_tag_id item.tags.is_equipped_tag
  IF edited THEN itemtags(id) = item.tags
  
  defitem "Equipped by hero in active party:"
  edit_as_tag_id item.tags.is_actively_equipped_tag
  IF edited THEN itemtags(id) = item.tags
 END IF

 IF phase = processing THEN
  IF can_copy_and_paste THEN
   IF keyval(scAlt) > 0 ANDALSO keyval(scC) > 1 THEN
    IF clipboard_item THEN DELETE clipboard_item
    clipboard_item = NEW ItemDef(item)
    show_overlay_message "Copied item", 0.75
   END IF
   IF clipboard_item ANDALSO keyval(scAlt) > 0 ANDALSO keyval(scV) > 1 THEN
    IF undo_item THEN DELETE undo_item
    undo_item = NEW ItemDef(item)
    item = *clipboard_item
    state.need_update = YES
    show_overlay_message "Pasted item (Ctrl-Z to undo)", 1.1
   END IF
  END IF

  IF undo_item ANDALSO keyval(scCtrl) > 0 ANDALSO keyval(scZ) > 1 THEN
   item = *undo_item
   'DELETE undo_item
   'undo_item = NULL
   state.need_update = YES
   show_overlay_message "Undid paste", 0.75
  END IF
 END IF

 END IF '--End of main menu
 '----------------------------

END SUB

SUB ItemEditor.reload_sprite()
 ChangeSpriteSlice wep_sl, sprTypeWeapon, item.wep_pic, item.wep_pal, preview_wep_frame
 handle_pos_sl->x = item.wep_handle(preview_wep_frame).x
 handle_pos_sl->y = item.wep_handle(preview_wep_frame).y
 wep_sl->Visible = IIF(item.eqslots(0), YES, NO)
END SUB

SUB ItemEditor.draw_underlays ()
 DrawSlice underlay, vpage
END SUB

'-----------------------------------------------------------------------

SUB item_editor_elementals(item as ItemDef)
 DIM elementals(gen(genNumElements) - 1) as single
 FOR i as integer = 0 TO gen(genNumElements) - 1
  elementals(i) = item.elemental_resist(i)
  IF gen(genEquipMergeFormula) = 2 THEN  'additive merging
   elementals(i) -= 1.0
  END IF
 NEXT
 common_elementals_editor elementals(), "item_elementals", (gen(genEquipMergeFormula) = 2)
 FOR i as integer = 0 TO gen(genNumElements) - 1
  IF gen(genEquipMergeFormula) = 2 THEN  'additive merging
   elementals(i) += 1.0
  END IF
  item.elemental_resist(i) = elementals(i)
 NEXT
END SUB

' Who Can Equip? menu
SUB item_editor_equipbits(item as ItemDef)
 DIM hero_id as integer
 DIM bitnames(-1 TO maxMaxHero) as string
 FOR hero_id = 0 TO gen(genMaxHero)
  bitnames(hero_id) = getheroname(hero_id)
  IF LEN(bitnames(hero_id)) = 0 THEN bitnames(hero_id) = "Hero " & hero_id
 NEXT
 editbitset item.equip_by_bits(), 0, bitnames(), , , , item.name & " is equippable by..."
END SUB

'-----------------------------------------------------------------------


FUNCTION individual_item_editor(item_id as integer) as integer

 IF item_id > maxMaxItems THEN
  visible_debug "Can't edit item id > " & maxMaxItems
  RETURN -1
 END IF

 DIM editor as ItemEditor = ItemEditor(item_id)
 editor.run()
 
 RETURN editor.id
END FUNCTION

'-----------------------------------------------------------------------


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
  standardmenu menu_display(), st, , , vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
END SUB

'-----------------------------------------------------------------------

SUB ExpandTextItemScreenPreview (code as string, result as string, byval arg0 as ANY ptr=0, byval arg1 as ANY ptr=0, byval arg2 as ANY ptr=0)
 SELECT CASE UCASE(code)
  CASE "EXIT": result = readglobalstring(35, "DONE", 10)
  CASE "SORT": result = readglobalstring(36, "AUTOSORT", 10)
  CASE "TRASH": result = readglobalstring(37, "TRASH", 10)
  CASE "ITEM":
   'Only support empty item right now. Later we might want to add a fake item stack with arg0
   result = ""
  CASE "DESC":
   IF TIMER MOD 6 < 3 THEN
    result = "Lorem ipsum dolor sit amet is the crest masterfully enscribed upon this beautiful sword."
   ELSE
    result = "A simple sword with no inscription."
   END IF
 END SELECT
END SUB

'-----------------------------------------------------------------------
