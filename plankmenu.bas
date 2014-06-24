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

FUNCTION plank_menu_move_cursor (byref ps as PlankState, byval axis as integer, byval d as integer) as bool

 IF ps.cur = 0 THEN
  'No cursor yet, guess a default one
  ps.cur = top_left_plank(ps)
  RETURN YES
 END IF

 DIM result as integer = NO

 ps.planks_found = 0
 REDIM planks(10) as Slice Ptr
 find_all_planks ps, ps.m, planks()
 
 DIM old as XYPair
 old.x = ps.cur->ScreenX
 old.y = ps.cur->ScreenY

 DIM best as integer = 2000000000
 DIM p as XYPair
 DIM dist as integer
 
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO ps.planks_found - 1
  sl = planks(i)
  p.x = sl->ScreenX
  p.y = sl->ScreenY
  IF (d = 1 ANDALSO p.n(axis) > old.n(axis)) ORELSE (d = -1 ANDALSO p.n(axis) < old.n(axis)) THEN
   dist = (old.x - p.x) ^ 2 + (old.y - p.y) ^ 2
   IF dist < best THEN
    best = dist
    ps.cur = sl
    result = YES
   END IF
  END IF
 NEXT i
 
 RETURN result
END FUNCTION

FUNCTION plank_menu_arrows (byref ps as PlankState) as bool
 DIM result as bool = NO
 'IF keyval(scA) > 1 THEN slice_editor m
 IF carray(ccLeft) > 1  THEN IF plank_menu_move_cursor(ps, 0, -1) THEN result = YES
 IF carray(ccRight) > 1 THEN IF plank_menu_move_cursor(ps, 0, 1)  THEN result = YES
 IF carray(ccUp) > 1    THEN IF plank_menu_move_cursor(ps, 1, -1) THEN result = YES
 IF carray(ccDown) > 1  THEN IF plank_menu_move_cursor(ps, 1, 1)  THEN result = YES
 RETURN result
END FUNCTION

FUNCTION top_left_plank(byref ps as PlankState) as Slice Ptr

 ps.planks_found = 0
 REDIM planks(10) as Slice Ptr
 find_all_planks ps, ps.m, planks()

 IF ps.planks_found = 0 THEN RETURN 0

 DIM best as Slice Ptr = planks(0)
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO ps.planks_found - 1
  sl = planks(i)
  IF sl->ScreenX <= best->ScreenX ANDALSO sl->ScreenY <= best->ScreenY THEN
   best = sl
  END IF
 NEXT i
 
 RETURN best
END FUNCTION

SUB find_all_planks(byref ps as PlankState, byval m as Slice Ptr, planks() as Slice Ptr)
 IF m = 0 THEN debug "plank_menu_move_cursor: null m ptr" : EXIT SUB

 DIM plank_checker as FUNCTION (byval s as Slice Ptr) as bool
 plank_checker = ps.is_plank_callback
 IF plank_checker = 0 THEN plank_checker = @is_plank

 DIM sl as Slice Ptr
 sl = m->FirstChild
 DO WHILE sl
  IF plank_checker(sl) THEN
   'This is a plank.
   IF ps.planks_found > UBOUND(planks) THEN
    REDIM PRESERVE planks(UBOUND(planks) + 10) as Slice Ptr
   END IF
   planks(ps.planks_found) = sl
   ps.planks_found += 1
  END IF
  find_all_planks ps, sl, planks()
  sl = sl->NextSibling
 LOOP
 
END SUB

SUB set_plank_state_default_callback (byval sl as Slice Ptr, byval state as integer)
 SELECT CASE sl->SliceType
  CASE slText:
   SELECT CASE state
    CASE plankNORMAL:          ChangeTextSlice sl, , uiMenuItem * -1 - 1
    CASE plankSEL:             ChangeTextSlice sl, , uiSelectedItem2 * -1 - 1
    CASE plankDISABLE:         ChangeTextSlice sl, , uiDisabledItem * -1 - 1
    CASE plankSELDISABLE:      ChangeTextSlice sl, , uiSelectedDisabled2 * -1 - 1
    CASE plankSPECIAL:         ChangeTextSlice sl, , uiSpecialItem * -1 - 1
    CASE plankSELSPECIAL:      ChangeTextSlice sl, , uiSelectedSpecial2 * -1 - 1
   END SELECT
  CASE slRectangle:
   sl->Visible = YES
   SELECT CASE state
    CASE plankNORMAL:          sl->Visible = NO
    CASE plankSEL:             ChangeRectangleSlice sl, , uiHighlight * -1 - 1
    CASE plankDISABLE:         sl->Visible = NO
    CASE plankSELDISABLE:      ChangeRectangleSlice sl, , uiHighlight * -1 - 1
    CASE plankSPECIAL:         sl->Visible = NO
    CASE plankSELSPECIAL:      ChangeRectangleSlice sl, , uiHighlight * -1 - 1
   END SELECT
 END SELECT
END SUB

SUB set_plank_state (byref ps as PlankState, byval sl as Slice Ptr, byval state as integer=plankNORMAL)
 IF sl = 0 THEN debug "set_plank_state: null slice ptr": EXIT SUB
 'First evaluate the current slice
 IF sl->Lookup = SL_PLANK_MENU_SELECTABLE THEN
  DIM runner as SUB (byval sl as Slice Ptr, byval state as integer)
  runner = ps.state_callback
  IF runner = 0 THEN runner = @set_plank_state_default_callback
  runner(sl, state)
 END IF
 
 'Now repeat for each child
 DIM ch as Slice Ptr = sl->FirstChild
 DO WHILE ch
  set_plank_state ps, ch, state
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

FUNCTION is_plank(byval sl as Slice Ptr) as bool
 IF sl = 0 THEN debug "is_item_plank: null slice ptr" : RETURN NO
 IF sl->Lookup = SL_PLANK_HOLDER THEN RETURN YES
 RETURN NO 
END FUNCTION

FUNCTION find_plank_scroll (byval sl as Slice Ptr) as slice ptr
 IF sl = 0 THEN debug "find_plank_scroll: null slice ptr" : RETURN 0
 IF sl->SliceType = slScroll THEN RETURN sl

 DIM result as Slice Ptr
 DIM ch as slice ptr = sl->FirstChild
 DO WHILE ch
  result = find_plank_scroll(ch)
  IF result <> 0 THEN RETURN result
  ch = ch->NextSibling
 LOOP

 RETURN 0
END FUNCTION

SUB update_plank_scrolling (byref ps as PlankState)
 IF ps.m = 0 THEN debug "update_plank_scrolling: null m slice ptr" : EXIT SUB

 DIM scroll as slice ptr = find_plank_scroll(ps.m)
 IF scroll ANDALSO ps.cur THEN
  ScrollToChild scroll, ps.cur
 END IF
 
END SUB
