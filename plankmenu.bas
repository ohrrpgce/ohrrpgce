'OHRRPGCE GAME - Plank-Slice based menus
'(C) Copyright 2014 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "allmodex.bi"
#include "common.bi" 
#include "loading.bi"
#include "gglobals.bi"
#include "const.bi"
#include "uiconst.bi"
#include "game_udts.bi"
#include "slices.bi"
#include "sliceedit.bi"

#include "yetmore.bi"

#include "plankmenu.bi"


FUNCTION plank_menu_move_cursor (byref cur as Slice Ptr, byval m as Slice Ptr, byval axis as integer, byval d as integer) as bool
 RETURN NO
END FUNCTION

FUNCTION plank_menu_arrows (byref cur as Slice Ptr, byval m as Slice Ptr) as bool
 DIM result as bool = NO
 'IF keyval(scA) > 1 THEN slice_editor m
 IF carray(ccLeft) > 1  THEN IF plank_menu_move_cursor(cur, m, 0, -1) THEN result = YES
 IF carray(ccRight) > 1 THEN IF plank_menu_move_cursor(cur, m, 0, 1)  THEN result = YES
 IF carray(ccUp) > 1    THEN IF plank_menu_move_cursor(cur, m, 1, -1) THEN result = YES
 IF carray(ccDown) > 1  THEN IF plank_menu_move_cursor(cur, m, 1, 1)  THEN result = YES
 RETURN result
END FUNCTION

SUB set_plank_state (byval sl as Slice Ptr, byval state as integer=plankNORMAL)
 IF sl = 0 THEN debug "set_plank_state: null slice ptr": EXIT SUB
 'First evaluate the current slice
 IF sl->Lookup = SL_PLANK_MENU_SELECTABLE THEN
  SELECT CASE sl->SliceType
   CASE slText:
    SELECT CASE state
     CASE plankNORMAL:         ChangeTextSlice sl, , uiMenuItem * -1 - 1
     CASE plankSEL:            ChangeTextSlice sl, , uiSelectedItem2 * -1 - 1
     CASE plankDISABLE:        ChangeTextSlice sl, , uiDisabledItem * -1 - 1
     CASE plankSELDISABLE:     ChangeTextSlice sl, , uiSelectedDisabled2 * -1 - 1
     CASE plankSPECIAL:        ChangeTextSlice sl, , uiSpecialItem * -1 - 1
     CASE plankSELSPECIAL:     ChangeTextSlice sl, , uiSelectedSpecial2 * -1 - 1
     CASE plankITEMSEL:        ChangeTextSlice sl, , uiItemScreenSelected * -1 - 1
     CASE plankITEMSELDISABLE: ChangeTextSlice sl, , uiItemScreenSelectedDisabled * -1 - 1
     CASE plankITEMSELSPECIAL: ChangeTextSlice sl, , uiItemScreenSelectedSpecial * -1 - 1
    END SELECT
  END SELECT
 END IF
 
 'Now repeat for each child
 DIM ch as Slice Ptr = sl->FirstChild
 DO WHILE ch
  set_plank_state ch, state
  ch = ch->NextSibling
 LOOP
 
END SUB

FUNCTION plank_menu_append (byval sl as slice ptr, byval lookup as integer, byval collection_kind as integer, byval callback as ANY ptr=0, byval arg0 as integer=0, byval arg1 as integer=0, byval arg2 as integer=0) as Slice Ptr
 DIM collection as Slice Ptr = NewSliceOfType(slRoot)
 load_slice_collection collection, collection_kind
 IF collection = 0 THEN debug "plank_menu_append: plank collection not found " & collection_kind : RETURN 0
 DIM result as Slice Ptr
 result = plank_menu_append(sl, lookup, collection, callback, arg0, arg1, arg2)
 DeleteSlice @collection
 RETURN result
END FUNCTION

FUNCTION plank_menu_append (byval sl as slice ptr, byval lookup as integer, byval collection as Slice Ptr, byval callback as ANY ptr=0, byval arg0 as integer=0, byval arg1 as integer=0, byval arg2 as integer=0) as Slice Ptr
 IF sl = 0 THEN debug "plank_menu_append: null slice ptr": RETURN 0
 DIM m as Slice ptr = LookupSlice(lookup, sl)
 IF m = 0 THEN debug "plank_menu_append: menu not found " & lookup : RETURN 0
 IF collection = 0 THEN debug "plank_menu_append: plank collection null ptr" : RETURN 0
 DIM holder as Slice Ptr
 holder = LookupSlice(SL_PLANK_HOLDER, collection)
 DIM cl as Slice Ptr
 IF holder <> 0 THEN
  'Found a holder, use only it
  cl = CloneSliceTree(holder)
  cl->Fill = YES
 ELSE
  'No holder, use the whole collection
  cl = CloneSliceTree(collection)
 END IF
 
 SetSliceParent cl, m
 
 expand_slice_text_insert_codes cl, callback, arg0, arg1, arg2
 
 RETURN cl
 
END FUNCTION

SUB plank_menu_clear (byval sl as Slice Ptr, byval lookup as integer)
 IF sl = 0 THEN debug "plank_menu_clear: null slice ptr": EXIT SUB
 DIM m as Slice ptr = LookupSlice(lookup, sl)
 IF m = 0 THEN
  debug "plank_menu_clear: menu not found " & lookup
  EXIT SUB
 END IF
 DeleteSliceChildren m
END SUB

SUB expand_slice_text_insert_codes (byval sl as Slice ptr, byval callback as ANY ptr=0, byval arg0 as integer=0, byval arg1 as integer=0, byval arg2 as integer=0)
 'Starting with children of the given container slice, iterate through
 ' all children and expand any ${} codes found in any TextSlice
 IF sl = 0 THEN debug "expand_slice_text_insert_codes: null slice ptr": EXIT SUB
 DIM ch as Slice Ptr = sl->FirstChild
 DIM dat as TextSliceData Ptr
 DO WHILE ch <> 0
  IF ch->SliceType = slText THEN
   dat = ch->SliceData
   IF dat->s_orig = "" THEN dat->s_orig = dat->s
   ChangeTextSlice ch, embed_text_codes(dat->s_orig, callback, arg0, arg1, arg2)
  END IF
  expand_slice_text_insert_codes ch, callback, arg0, arg1, arg2
  ch = ch->NextSibling
 LOOP
END SUB

SUB hide_slices_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval cond as bool)
 'Starting with children of the given container slice, iterate through
 ' all children and toggle the visibility of any slices with a specific lookup code
 IF sl = 0 THEN debug "hide_slices_by_lookup_code: null slice ptr": EXIT SUB
 DIM ch as Slice Ptr = sl->FirstChild
 DO WHILE ch <> 0
  IF ch->Lookup = lookup THEN
   ch->Visible = NOT cond
  END IF
  hide_slices_by_lookup_code ch, lookup, cond
  ch = ch->NextSibling
 LOOP
END SUB

SUB set_sprites_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval sprtype as SpriteType, byval picnum as integer, byval palnum as integer=-1)
 'Starting with children of the given container slice, iterate through
 ' all children and change any sprites matching the lookup code
 IF sl = 0 THEN debug "set_sprites_by_lookup_code: null slice ptr": EXIT SUB
 DIM ch as Slice Ptr = sl->FirstChild
 DO WHILE ch <> 0
  IF ch->Lookup = lookup THEN
   IF ch->SliceType = slSprite THEN
    ChangeSpriteSlice ch, sprtype, picnum, palnum
   END IF
  END IF
  set_sprites_by_lookup_code ch, lookup, sprtype, picnum, palnum
  ch = ch->NextSibling
 LOOP
END SUB
