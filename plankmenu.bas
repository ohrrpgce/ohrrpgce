'OHRRPGCE GAME & CUSTOM - Plank-Slice based menus
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

#include "scriptcommands.bi"

#include "plankmenu.bi"

'Local subs and functions
DECLARE FUNCTION plank_menu_move_cursor (byref ps as PlankState, byval axis as integer, byval d as integer, byval start_parent as Slice Ptr=0) as bool
DECLARE SUB plank_menu_scroll_page (byref ps as PlankState, byval scrolldir as integer, byval start_parent as Slice Ptr=0)

'-----------------------------------------------------------------------

'A note about planks: A plank should have the lookup code SL_PLANK_HOLDER whatever its type.
'Child slices of types slText, slRectangle or slSelect can have lookup code SL_PLANK_MENU_SELECTABLE
'if their styles should change depending on selection state

'-----------------------------------------------------------------------

'If operating on a grid arrangement of plank slices, the planks should be of uniform size for best results
'because this goes by plank center points
FUNCTION plank_menu_move_cursor (byref ps as PlankState, byval axis as integer, byval d as integer, byval start_parent as Slice Ptr=0) as bool

 IF ps.cur = 0 THEN
  'No cursor yet, guess a default one
  ps.cur = top_left_plank(ps)
  RETURN YES
 END IF
 
 DIM old_cur as Slice Ptr = ps.cur

 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()
 
 DIM old as XYPair
 old.x = ps.cur->ScreenX + ps.cur->Width / 2
 old.y = ps.cur->ScreenY + ps.cur->Height / 2

 DIM best as integer = INT_MAX
 DIM p as XYPair
 DIM dist as integer
 
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO UBOUND(planks)
  sl = planks(i)
  p.x = sl->ScreenX + sl->Width / 2
  p.y = sl->ScreenY + sl->Height / 2
  IF (d = 1 ANDALSO p.n(axis) > old.n(axis)) ORELSE (d = -1 ANDALSO p.n(axis) < old.n(axis)) THEN
   dist = (old.x - p.x) ^ 2 + (old.y - p.y) ^ 2
   IF dist < best THEN
    best = dist
    ps.cur = sl
   END IF
  END IF
 NEXT i
 
 RETURN ps.cur <> old_cur
END FUNCTION

SUB plank_menu_scroll_page (byref ps as PlankState, byval scrolldir as integer, byval start_parent as Slice Ptr=0)

 IF ps.cur = 0 THEN
  'No cursor yet, guess a default one
  ps.cur = top_left_plank(ps)
  IF ps.cur = 0 THEN debug "plank_menu_scroll_page: No cursor, and can't find one" : EXIT SUB
 END IF

 DIM scroll as Slice ptr = find_plank_scroll(ps.m)

 DIM targpos as XYPair
 targpos = ps.cur->ScreenPos + ps.cur->Size / 2
 IF scroll THEN targpos.y += scroll->Height * scrolldir

 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()

 IF UBOUND(planks) < 0 THEN EXIT SUB

 DIM best_sl as Slice Ptr = ps.cur
 DIM best as integer = (best_sl->ScreenX + best_sl->Width / 2 - targpos.x) ^ 2 + (best_sl->ScreenY + best_sl->Height / 2 - targpos.y) ^ 2
 DIM dist as integer
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO UBOUND(planks)
  sl = planks(i)
  dist = (sl->ScreenX + sl->Width / 2 - targpos.x) ^ 2 + (sl->ScreenY + sl->Height / 2 - targpos.y) ^ 2
  IF dist < best THEN
   best = dist
   best_sl = sl
  END IF
 NEXT i

 ps.cur = best_sl
END SUB

FUNCTION plank_menu_arrows (byref ps as PlankState, byval start_parent as Slice Ptr=0) as bool
 DIM result as bool = NO
 'IF keyval(scA) > 1 THEN slice_editor m
 IF start_parent = 0 THEN start_parent = ps.m
#IFDEF IS_GAME
 IF carray(ccLeft) > 1  ANDALSO plank_menu_move_cursor(ps, 0, -1, start_parent) THEN result = YES
 IF carray(ccRight) > 1 ANDALSO plank_menu_move_cursor(ps, 0, 1, start_parent)  THEN result = YES
 IF carray(ccUp) > 1    ANDALSO plank_menu_move_cursor(ps, 1, -1, start_parent) THEN result = YES
 IF carray(ccDown) > 1  ANDALSO plank_menu_move_cursor(ps, 1, 1, start_parent)  THEN result = YES
#ELSE
 IF keyval(scLeft) > 1  ANDALSO plank_menu_move_cursor(ps, 0, -1, start_parent) THEN result = YES
 IF keyval(scRight) > 1 ANDALSO plank_menu_move_cursor(ps, 0, 1, start_parent)  THEN result = YES
 IF keyval(scUp) > 1    ANDALSO plank_menu_move_cursor(ps, 1, -1, start_parent) THEN result = YES
 IF keyval(scDown) > 1  ANDALSO plank_menu_move_cursor(ps, 1, 1, start_parent)  THEN result = YES
#ENDIF
 IF keyval(scPageUp) > 1 THEN plank_menu_scroll_page ps, -1, start_parent : result = YES
 IF keyval(scPageDown) > 1 THEN plank_menu_scroll_page ps, 1, start_parent : result = YES
 RETURN result
END FUNCTION

FUNCTION find_plank_nearest_screen_pos(byref ps as PlankState, byval targpos as XYPair, byval start_parent as Slice Ptr=0) as Slice Ptr
 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()

 DIM best_sl as Slice Ptr = 0
 DIM best as integer = 2000000000
 DIM p as XYPair
 DIM dist as integer
 
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO UBOUND(planks)
  sl = planks(i)
  p.x = sl->ScreenX + sl->Width / 2
  p.y = sl->ScreenY + sl->Height / 2
  dist = (targpos.x - p.x) ^ 2 + (targpos.y - p.y) ^ 2
  IF dist < best THEN
   best = dist
   best_sl = sl
  END IF
 NEXT i
 
 RETURN best_sl
END FUNCTION

FUNCTION top_left_plank(byref ps as PlankState) as Slice Ptr
 REDIM planks(any) as Slice Ptr
 find_all_planks ps, ps.m, planks()

 IF UBOUND(planks) < 0 THEN RETURN 0

 DIM best as Slice Ptr = planks(0)
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO UBOUND(planks)
  sl = planks(i)
  IF sl->ScreenX <= best->ScreenX ANDALSO sl->ScreenY <= best->ScreenY THEN
   best = sl
  END IF
 NEXT i
 
 RETURN best
END FUNCTION

FUNCTION default_is_plank(byval sl as Slice Ptr) as bool
 IF sl = 0 THEN debug "default_is_plank: null slice ptr" : RETURN NO
 RETURN sl->Lookup = SL_PLANK_HOLDER
END FUNCTION

' Fill planks() with all descendents of m that are planks (according to the callback)
SUB find_all_planks(byref ps as PlankState, byval m as Slice Ptr, planks() as Slice Ptr)
 IF m = 0 THEN debug "find_all_planks: null m ptr" : EXIT SUB

 DIM plank_checker as FnIsPlank
 plank_checker = ps.is_plank_callback
 IF plank_checker = 0 THEN plank_checker = @default_is_plank

 REDIM planks(-1 TO -1)
 DIM planks_found as integer = 0
 DIM desc as Slice ptr = m->FirstChild
 DO WHILE desc
  IF plank_checker(desc) THEN
   'This is a plank.
   IF planks_found > UBOUND(planks) THEN
    REDIM PRESERVE planks(-1 TO UBOUND(planks) + 10)
   END IF
   planks(planks_found) = desc
   planks_found += 1
  END IF
  desc = NextDescendent(desc, m)
 LOOP
 REDIM PRESERVE planks(-1 TO planks_found - 1)
END SUB

SUB set_plank_state_default_callback (byval sl as Slice Ptr, byval state as PlankItemState)
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

' Set the state (using the callback) of each descendent of sl with lookup=SL_PLANK_MENU_SELECTABLE
SUB set_plank_state (byref ps as PlankState, byval sl as Slice Ptr, byval state as PlankItemState = plankNORMAL)
 IF sl = 0 THEN debug "set_plank_state: null slice ptr": EXIT SUB

 DIM desc as Slice ptr = sl
 DO WHILE desc
  IF desc->Lookup = SL_PLANK_MENU_SELECTABLE THEN
   IF ps.state_callback THEN
    ps.state_callback(desc, state)
   ELSE
    set_plank_state_default_callback(desc, state)
   END IF
  END IF
  desc = NextDescendent(desc, sl)
 LOOP
END SUB

FUNCTION plank_menu_append (byval sl as slice ptr, byval lookup as integer, byval collection_kind as integer, byval callback as FnEmbedCode=0, byval arg0 as any ptr=0, byval arg1 as any ptr=0, byval arg2 as any ptr=0) as Slice Ptr
 DIM collection as Slice Ptr = NewSliceOfType(slRoot)
 load_slice_collection collection, collection_kind
 IF collection = 0 THEN debug "plank_menu_append: plank collection not found " & collection_kind : RETURN 0
 DIM result as Slice Ptr
 result = plank_menu_append(sl, lookup, collection, callback, arg0, arg1, arg2)
 DeleteSlice @collection
 RETURN result
END FUNCTION

FUNCTION plank_menu_append (byval sl as slice ptr, byval lookup as integer, byval collection as Slice Ptr, byval callback as FnEmbedCode=0, byval arg0 as any ptr=0, byval arg1 as any ptr=0, byval arg2 as any ptr=0) as Slice Ptr
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
  cl->Lookup = SL_PLANK_HOLDER
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

SUB expand_slice_text_insert_codes (byval sl as Slice ptr, byval callback as FnEmbedCode=0, byval arg0 as any ptr=0, byval arg1 as any ptr=0, byval arg2 as any ptr=0)
 'Starting with children of the given container slice, iterate through
 ' all children and expand any ${} codes found in any TextSlice
 ' Do not descend into child slices marked with SL_PLANK_HOLDER because planks are responsible for their own text codes
 IF sl = 0 THEN debug "expand_slice_text_insert_codes: null slice ptr": EXIT SUB
 DIM ch as Slice Ptr = sl->FirstChild
 DIM dat as TextSliceData Ptr
 DO WHILE ch <> 0
  IF ch->Lookup <> SL_PLANK_HOLDER THEN
   IF ch->SliceType = slText THEN
    dat = ch->SliceData
    IF dat->s_orig = "" THEN dat->s_orig = dat->s
#IFDEF IS_GAME
    ChangeTextSlice ch, embed_text_codes(dat->s_orig, callback, arg0, arg1, arg2)
#ENDIF
   END IF
   expand_slice_text_insert_codes ch, callback, arg0, arg1, arg2
  END IF
  ch = ch->NextSibling
 LOOP
END SUB

SUB hide_slices_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval hide as bool)
 ' Starting with a given container slice, iterate through
 ' all descendents and set the visibility of any slices with a specific lookup code
 ' (Note: the argument is whether to set hidden, opposite of Slice.Visible)
 IF sl = 0 THEN debug "hide_slices_by_lookup_code: null slice ptr": EXIT SUB

 DIM desc as Slice ptr = sl
 DO WHILE desc
  IF desc->Lookup = lookup THEN desc->Visible = NOT hide
  desc = NextDescendent(desc, sl)
 LOOP
END SUB

SUB set_sprites_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval sprtype as SpriteType, byval picnum as integer, byval palnum as integer=-1)
 'Starting with children of the given container slice, iterate through
 ' all children and change any sprites matching the lookup code
 IF sl = 0 THEN debug "set_sprites_by_lookup_code: null slice ptr": EXIT SUB

 DIM desc as Slice ptr = sl
 DO WHILE desc
  IF desc->Lookup = lookup AND desc->SliceType = slSprite THEN
   ChangeSpriteSlice desc, sprtype, picnum, palnum
  END IF
  desc = NextDescendent(desc, sl)
 LOOP
END SUB

FUNCTION find_plank_scroll (byval sl as Slice Ptr) as slice ptr
 IF sl = 0 THEN debug "find_plank_scroll: null slice ptr" : RETURN 0

 DIM desc as Slice ptr = sl
 DO WHILE desc
  IF desc->SliceType = slScroll THEN RETURN desc
  desc = NextDescendent(desc, sl)
 LOOP
 RETURN 0
END FUNCTION

SUB update_plank_scrolling (byref ps as PlankState)
 IF ps.m = 0 THEN debug "update_plank_scrolling: null m slice ptr" : EXIT SUB

 DIM scroll as slice ptr = find_plank_scroll(ps.m)
 IF scroll ANDALSO ps.cur ANDALSO IsAncestor(ps.cur, scroll) THEN
  ScrollToChild scroll, ps.cur
 END IF
END SUB

SUB save_plank_selection (byref ps as PlankState)
 'Attempt to save the current selection and scroll position without any slice references
 ps.selection_saved = NO
 IF ps.cur = 0 THEN EXIT SUB
 ps._saved_pos.x = ps.cur->ScreenX + ps.cur->Width / 2
 ps._saved_pos.y = ps.cur->ScreenY + ps.cur->Height / 2
 ps.selection_saved = YES
END SUB

SUB restore_plank_selection (byref ps as PlankState)
 'Attempt to restore selection previously saved by save_plank_selection
 ps.cur = 0
 IF ps.selection_saved = NO THEN EXIT SUB
 ps.cur = find_plank_nearest_screen_pos(ps, ps._saved_pos)
 ps.selection_saved = NO
END SUB

FUNCTION focus_plank_by_extra_id(byref ps as PlankState, byval id as integer, byval start_parent as Slice Ptr = 0) as bool
 DIM old_cur as Slice Ptr = ps.cur

 DIM new_cur as Slice Ptr
 new_cur = find_plank_by_extra_id(ps, id, start_parent)
 IF new_cur THEN
  ps.cur = new_cur
  update_plank_scrolling ps
 END IF
 
 RETURN ps.cur <> old_cur
END FUNCTION

FUNCTION find_plank_by_extra_id(byref ps as PlankState, byval id as integer, byval start_parent as Slice Ptr = 0) as Slice Ptr
 'If more than one plank has the same Extra(0) id number, just return the first one.

 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()
 
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO UBOUND(planks)
  sl = planks(i)
  IF sl->Extra(0) = id THEN RETURN sl
 NEXT i

 RETURN 0
END FUNCTION

