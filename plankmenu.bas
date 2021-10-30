'OHRRPGCE - Plank-slice-based menus
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "const.bi"
#include "uiconst.bi"
#include "slices.bi"
#include "sliceedit.bi"
#include "plankmenu.bi"

#ifdef IS_GAME
#include "gglobals.bi"
#include "scriptcommands.bi"  'For embed_text_codes
#endif

'-----------------------------------------------------------------------

'Plank menus are slice-based menus, where the menu item slices are called "planks".
'There are few requirements in how the slices are organised into a tree. There of lots
'of slices with different roles:
'
'1. A slice ps.m which is the root of the subtree for the menu; arbitrary other slices
'can exist in this tree aside from the ones below.
'
'2. An optional Scroll slice, which is scrolled automatically to the selected plank if
'it's a descendent. Planks can exist outside of it, as extra buttons outside the
'scrollable list.  The first Scroll slice in the tree is used.
'
'3. Zero or more 'list' parent slices for new planks (can be the root). Should normally be
'a Grid or Layout slice, so its childen are positioned automatically. This is specified
'by lookup code to plank_menu_append and plank_menu_clear; also typically passed as a
'pointer to functions such as plank_menu_arrows.
'These is just a place that new plank slices are created; planks don't have to all
'share this parent and can even be reparented out of it afterwards.
'
'4. Planks are the actual menu items. A plank should have the lookup code SL_PLANK_HOLDER
'whatever its type. Planks can be anywhere in the slice tree below ps.m (except a
'descendent of another plank!) Planks may be either created manually (e.g. for fixed
'extra selectable buttons like "New Game", or by thingbrowser's create_thing_plank()
'method), or created by calling plank_menu_append.
'
'5. Plank 'prototypes', which are cloned to create new planks if using plank_menu_append.
'Plank prototypes can be either stored in a separate file, loaded with load_plank_from_file,
'or just any old slice.
'
'6. Descendants of a plank slice can have lookup code SL_PLANK_MENU_SELECTABLE if their
'styles should change depending on selection state. Call set_plank_state to call the
'state_callback on all SL_PLANK_MENU_SELECTABLE slices.  The default callback can modify
'Text and Rectangle slices.
'
'This module doesn't create any of these slices except planks from prototypes.
'You need to supply all the other slices yourself.

'-----------------------------------------------------------------------

'This is a helper function which is useful if you aren't using plank_menu_append, and
'aren't using plank Template slices.
FUNCTION load_plank_from_file(filename as string) as Slice Ptr
 DIM plank as Slice ptr
 DIM col as Slice ptr = NewSliceOfType(slSpecial)
 IF SliceLoadFromFile(col, filename) THEN  'Shows error on failure
  DIM sl as Slice ptr = LookupSlice(SL_PLANK_HOLDER, col)
  IF sl = 0 THEN
   showerror "load_plank_from_file: could not find plank holder"
  ELSE
   plank = CloneTemplate(sl)
   OrphanSlice plank
  END IF
 END IF
 DeleteSlice @col
 RETURN plank
END FUNCTION

'fwd (forwards): a coordinate increasing in the direction we're trying to move
'side (sidewards): the orthogonal coordinate to the fwd one
'
'Eg, for axis = 1, d = 1:
'    +-------+     side
'    |pv C   |^   ----->
'    +---+---+|
'        .    |D
'        .    |
'        .    |  E
'       L.    v<--->
'        .     +---+
' |      .  S  |pl |
' |fwd   .<--->|   |
' v      .     +---+
'        .<-------->
'             F
'
' C: center of prev
' L: line from center of prev, in fwd direction
' S: pl_closest_side distance
' F: pl_far_side distance
' D: pl_dist_fwd
' E: pl_edgelen
'
' Note: there's heaps of dead code in this function, computing quantities
' which might be useful for changing the scoring function (eg extra penalties)
' but which can be removed once this is finalised.

'Find the position of an edge of a slice given by axis & d. See below for
'meaning of fwd and side.
'axis:  0 for left/right, 1 for up/down
'd:     1 for right or bottom edge, -1 for left or top edge
'rearedge: use the edge opposite the forward direction rather than the forward one
LOCAL SUB slice_forward_edge(sl as Slice ptr, axis as integer, d as integer, rearedge as bool, byref pos as FwdSide, byref edgelen as integer)
 DIM right_or_bottom_edge as bool = (d = 1) XOR rearedge
 pos.fwd = sl->ScreenPos.n(axis) + IIF(right_or_bottom_edge, sl->Size.n(axis), 0)
 pos.fwd *= d
 pos.side = sl->ScreenPos.n(axis XOR 1)
 edgelen = large(1, sl->Size.n(axis XOR 1))
END SUB

LOCAL SUB slice_forward_center(sl as Slice ptr, axis as integer, d as integer, byref center as FwdSide)
 center.fwd = (sl->ScreenPos + sl->Size \ 2).n(axis)
 center.fwd *= d
 center.side = (sl->ScreenPos + sl->Size \ 2).n(axis XOR 1)
END SUB

CONSTRUCTOR PlankViewpoint(prev as Slice ptr, axis as integer, d as integer)
 this.prev = prev
 this.axis = axis
 this.d = d

 slice_forward_center(prev, axis, d, prev_center)
 slice_forward_edge(prev, axis, d, NO, prev_front, prev_edgelen)
 prev_front.fwd -= prev_center.fwd
 prev_front.side -= prev_center.side
END CONSTRUCTOR

FUNCTION PlankViewpoint.plank_effective_pos(byref ret as FwdSide, pl as Slice ptr) as bool
 'Get the edge of this plank facing the old one.
 DIM pl_front as FwdSide, pl_edgelen as integer
 slice_forward_edge(pl, axis, d, YES, pl_front, pl_edgelen)

 DIM pl_center as FwdSide
 slice_forward_center(pl, axis, d, pl_center)

 pl_center.fwd -= prev_center.fwd
 pl_center.side -= prev_center.side
 pl_front.fwd -= prev_center.fwd
 pl_front.side -= prev_center.side

 'Shift everything relative to prev_center, which is at 0,0

 'First, ignore any plank that isn't at least fwd of the center of
 'the old plank. Use the center rather than the fwd edge of the old plank
 'so that they can overlap a bit.
 IF pl_center.fwd <= 0 THEN RETURN NO

 'Unless the plank overlaps with this one, ignore if it's not forward of our front edge
 IF pl_center.fwd < prev_front.fwd ANDALSO SliceCollide(prev, pl) = NO THEN RETURN NO

 'Find the 'side' distance between the edge and the line from the center of the prev slice
 DIM pl_closest_side as integer
 DIM pl_far_side as integer

 IF pl_front.side <= 0 AND pl_front.side + pl_edgelen >= 0 THEN
  'Line L intersects the plank
  pl_closest_side = 0
 ELSE
  DIM other_side as integer = pl_front.side + pl_edgelen
  IF ABS(pl_front.side) > ABS(other_side) THEN
   pl_closest_side = other_side
  ELSE
   pl_closest_side = pl_front.side
  END IF
  'pl_far_side = pl_closest_side + f
 END IF

 'Penalise slices that stick out by moving the closest point up to 4px
 'towards the center of the slice.
 ' IF pl_closest_side > 0 THEN
 '  pl_closest_side += small(pl_edgelen, 6) \ 2
 ' END IF
 '  pl_closest_side = small(pl_closest_side + 3, ABS(pl_center.side))

 '  pl_closest_side = (pl_closest_side + small(pl_center.fwd, ABS(pl_center.side))) / 2

 ' pl_far_side = large(ABS(pl_front.side), ABS(pl_front.side + pl_edgelen))


 'If the front edge of the plank is actually behind us but it sticks out far ahead,
 'then pl_front.fwd is negative, but we should still consider it.
 '  DIM pl_dist_fwd as integer = pl_fwd
 DIM pl_dist_fwd as integer = large(1, pl_front.fwd)
 ' pl_dist_fwd = bound(directedness * pl_closest_side, pl_front.fwd, pl_center.fwd)   'Or pl_center.side to penalise large slices
 ' pl_dist_fwd = large(1, pl_dist_fwd)

 ret.fwd = pl_dist_fwd
 ret.side = pl_closest_side
 RETURN YES
END FUNCTION

'axis:  0 for left/right, 1 for up/down
'd:     1 for right or down, -1 for left or up
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

 DIM as PlankViewpoint viewpoint = PlankViewpoint(ps.cur, axis, d)

 DIM best as double = DBL_MAX  'INT_MAX

 DIM as double directedness = 3.

 FOR i as integer = 0 TO UBOUND(planks)
  DIM pl as Slice ptr = planks(i)

  DIM pnt as FwdSide
  IF viewpoint.plank_effective_pos(pnt, pl) = NO THEN CONTINUE FOR

  DIM score as double
  score = pnt.fwd + (directedness * pnt.side) ^ 2 / pnt.fwd

  ''Penalise planks which are wide and stick out a long way to the side
  ''score += 0.5 * large(0, pl_far_side - prev_edgelen \ 2)

  'Ignore slices far away at a very high angle
  IF score > 1e4 THEN CONTINUE FOR

  IF score < best THEN
   best = score
   ps.cur = pl
  END IF
 NEXT i

 RETURN ps.cur <> old_cur
END FUNCTION

LOCAL FUNCTION plankscroll_dist(sl as Slice ptr, targpos as XYPair) as double
 DIM xdist as integer  'Min distance between targpos.x and any point inside sl
 xdist = large(0, large(sl->ScreenX - targpos.x, targpos.x - (sl->ScreenX + sl->Width)))
 RETURN xdist ^ 2 + (sl->ScreenY + sl->Height / 2 - targpos.y) ^ 2
END FUNCTION

SUB plank_menu_scroll_page (byref ps as PlankState, byval scrolldir as integer, byval start_parent as Slice Ptr=0)

 IF ps.cur = NULL THEN
  'No cursor yet, guess a default one
  ps.cur = top_left_plank(ps)
  FAIL_IF(ps.cur = NULL, "No cursor, and can't find one")
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
 DIM best as double = plankscroll_dist(best_sl, targpos)
 FOR i as integer = 0 TO UBOUND(planks)
  DIM sl as Slice ptr = planks(i)
  DIM dist as double = plankscroll_dist(sl, targpos)
  IF dist < best THEN
   best = dist
   best_sl = sl
  END IF
 NEXT i

 ps.cur = best_sl
END SUB

FUNCTION plank_menu_arrows (byref ps as PlankState, byval start_parent as Slice Ptr=0, byval linear_left_right as bool = NO) as bool
 DIM result as bool = NO
 'IF keyval(scA) > 1 THEN slice_editor m
 IF start_parent = 0 THEN start_parent = ps.m

 DIM left_right as integer
 IF carray(ccLeft) > 1 THEN left_right = -1
 IF carray(ccRight) > 1 THEN left_right = 1
 IF left_right THEN
  'Try moving by position (unless disabled), otherwise try moving forward/back in tree traversal order
  IF linear_left_right = NO THEN result = plank_menu_move_cursor(ps, 0, left_right, start_parent)
  IF result = NO THEN result = plank_menu_select_prev_next(ps, left_right)
 END IF

 IF carray(ccUp) > 1    ANDALSO plank_menu_move_cursor(ps, 1, -1, start_parent) THEN result = YES
 IF carray(ccDown) > 1  ANDALSO plank_menu_move_cursor(ps, 1, 1, start_parent)  THEN result = YES
 IF keyval(scPageUp) > 1 THEN plank_menu_scroll_page ps, -1, start_parent : result = YES
 IF keyval(scPageDown) > 1 THEN plank_menu_scroll_page ps, 1, start_parent : result = YES
 IF keyval(scHome) > 1 THEN IF plank_menu_home(ps) THEN result = YES
 IF keyval(scEnd) > 1 THEN IF plank_menu_end(ps) THEN result = YES
 IF result THEN reset_menu_edit_state
 RETURN result
END FUNCTION

FUNCTION plank_menu_drag_scroll(byref ps as PlankState, byval which_button as MouseButton=mouseLeft, byval min_threshold as integer=10) as bool
 IF (readmouse.dragging AND which_button) THEN
  IF readmouse.drag_dist > min_threshold THEN
   DIM amount as integer = readmouse.pos.y - readmouse.lastpos.y
   RETURN plank_menu_scroll(ps, amount)
  END IF
 END IF

 RETURN NO
END FUNCTION

FUNCTION plank_menu_mouse_wheel(byref ps as PlankState, byval dist as integer=30) as bool
 '30 is a reasonable default number of pixels, I guess?
 DIM scroll_move as integer = dist * -1 * readmouse.wheel_delta / 120
 RETURN plank_menu_scroll(ps, scroll_move)
END FUNCTION

FUNCTION plank_menu_scroll(byref ps as PlankState, byval scroll_move as integer, byval mouse_must_be_in_scroll as bool=YES) as bool
 DIM result as bool = NO
 DIM scroll as Slice Ptr
 scroll = find_plank_scroll(ps.m)
 IF scroll = 0 THEN RETURN NO
 IF mouse_must_be_in_scroll THEN
  IF NOT SliceCollidePoint(scroll, readmouse.pos) THEN RETURN NO
 END IF

 DIM topy as integer = scroll->ScreenY
 DIM boty as integer = scroll->ScreenY + scroll->Height
 DIM as XYPair min, max
 CalcSliceContentsSize scroll, min, max, 0
 DO WHILE min.y + scroll_move > topy : scroll_move -= 1 : LOOP
 DO WHILE max.y + scroll_move < boty : scroll_move += 1 : LOOP
 ScrollAllChildren scroll, 0, scroll_move

 RETURN YES
END FUNCTION

FUNCTION plank_menu_home(byref ps as PlankState) as bool
 DIM old_cur as Slice ptr = ps.cur
 ps.cur = top_left_plank(ps)
 RETURN ps.cur <> old_cur
END FUNCTION

FUNCTION plank_menu_end(byref ps as PlankState) as bool
 DIM old_cur as Slice ptr = ps.cur
 ps.cur = bottom_right_plank(ps)
 RETURN ps.cur <> old_cur
END FUNCTION

FUNCTION plank_menu_update_hover(byref ps as PlankState) as bool
 'Returns YES if the hover has changed
 DIM oldhover as Slice Ptr = ps.hover
 ps.hover = find_plank_at_screen_pos(ps, readmouse.pos)
 RETURN ps.hover <> oldhover
END FUNCTION

FUNCTION find_plank_nearest_screen_pos(byref ps as PlankState, byval targpos as XYPair, byval start_parent as Slice Ptr=0) as Slice Ptr
 'Given a target screen pos, find the closest plank, even if it does not overlap the target pos
 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()

 DIM best_sl as Slice Ptr = 0
 DIM best as integer = INT_MAX
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

FUNCTION find_plank_at_screen_pos(byref ps as PlankState, byval targpos as XYPair, byval start_parent as Slice Ptr=0) as Slice Ptr
 'Given a target screen pos, find the first colliding plank that is not invisible.
 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()
 
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO UBOUND(planks)
  sl = planks(i)
  IF SliceIsInvisibleOrClipped(sl) THEN CONTINUE FOR
  IF SliceCollidePoint(sl, targpos) THEN RETURN sl
 NEXT i
 
 RETURN 0
END FUNCTION

FUNCTION top_left_plank(byref ps as PlankState, byval start_parent as Slice Ptr=0) as Slice Ptr
 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()

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

FUNCTION bottom_right_plank(byref ps as PlankState, byval start_parent as Slice Ptr=0) as Slice Ptr
 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()

 IF UBOUND(planks) < 0 THEN RETURN 0

 DIM best as Slice Ptr = planks(UBOUND(planks))
 DIM sl as Slice Ptr
 FOR i as integer = UBOUND(planks) to 0 STEP -1
  sl = planks(i)
  IF sl->ScreenX >= best->ScreenX ANDALSO sl->ScreenY >= best->ScreenY THEN
   best = sl
  END IF
 NEXT i
 
 RETURN best
END FUNCTION

FUNCTION default_is_plank(byval sl as Slice Ptr) as bool
 BUG_IF(sl = NULL, "null slice ptr", NO)
 RETURN sl->Lookup = SL_PLANK_HOLDER ANDALSO SliceIsInvisible(sl) = NO
END FUNCTION

'offset=1: select next plank in slice-tree-traversal order, offset=-1: select previous plank
FUNCTION plank_menu_select_prev_next(byref ps as PlankState, offset as integer) as bool
 IF ps.cur = NULL THEN RETURN NO

 REDIM planks(any) as Slice Ptr
 find_all_planks ps, ps.m, planks()

 DIM idx as integer = -1
 FOR idx = 0 TO UBOUND(planks)
  IF planks(idx) = ps.cur THEN EXIT FOR
 NEXT
 IF idx = -1 THEN RETURN NO
 idx += offset
 IF idx < 0 ORELSE idx > UBOUND(planks) THEN RETURN NO
 ps.cur = planks(idx)
 RETURN YES
END FUNCTION

' Fill planks() with all descendents of m that are planks (according to the callback)
' The default callback excludes invisible planks (also planks with invisible parents)
' Templates are never planks.
SUB find_all_planks(byref ps as PlankState, byval m as Slice Ptr, planks() as Slice Ptr)
 BUG_IF(m = NULL, "null m ptr")

 DIM plank_checker as FnIsPlank
 plank_checker = ps.is_plank_callback
 IF plank_checker = NULL THEN plank_checker = @default_is_plank

 REDIM planks(-1 TO -1)
 DIM planks_found as integer = 0
 DIM desc as Slice ptr = m->FirstChild
 DO WHILE desc
  IF desc->Template = NO ANDALSO plank_checker(desc) THEN
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
    CASE plankMOUSEHOVER:      ChangeTextSlice sl, , uiMouseHoverItem * -1 -1
   END SELECT
  CASE slRectangle:
   sl->Visible = YES
   'Change the bgcol
   SELECT CASE state
    CASE plankNORMAL:          sl->Visible = NO
    CASE plankSEL:             ChangeRectangleSlice sl, , uiHighlight * -1 - 1
    CASE plankDISABLE:         sl->Visible = NO
    CASE plankSELDISABLE:      ChangeRectangleSlice sl, , uiHighlight * -1 - 1
    CASE plankSPECIAL:         sl->Visible = NO
    CASE plankSELSPECIAL:      ChangeRectangleSlice sl, , uiHighlight * -1 - 1
    CASE plankMOUSEHOVER:      sl->Visible = NO
   END SELECT
 END SELECT
END SUB

' Set the state (using the callback) of each descendent of sl with lookup=SL_PLANK_MENU_SELECTABLE
SUB set_plank_state (byref ps as PlankState, byval sl as Slice Ptr, byval state as PlankItemState = plankNORMAL)
 BUG_IF(sl = NULL, "null slice ptr")

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

'This is used only for plankmenus that are builtin (in future) customizable in-game menus.
'collection_kind should be a SL_COLLECT_* constant.
FUNCTION plank_menu_append (sl as slice ptr, lookup as integer, collection_kind as integer, callback as FnEmbedCode=0, arg0 as any ptr=0, arg1 as any ptr=0, arg2 as any ptr=0) as Slice ptr
 DIM collection as Slice ptr
 collection = LoadSliceCollection(collection_kind)
 IF collection = NULL THEN RETURN NULL  'Already showed an error
 DIM result as Slice ptr
 result = plank_menu_append(sl, lookup, collection, callback, arg0, arg1, arg2)
 DeleteSlice @collection
 RETURN result
END FUNCTION

'Add a new plank child, copied from 'prototype' to a list parent slice, which is 'LookupSlice(lookup, sl)'.
'prototype should usually be either:
'-a loaded collection containing a SL_PLANK_HOLDER to clone
'-a slice to clone directly (usually a template slice which is a child of the list parent,
' in which case you can call the other plank_menu_append overload and omit sl and lookup)
'-NULL: look for a Template child of the list parent to use
'Other args: passed to expand_slice_text_insert_codes
FUNCTION plank_menu_append (sl as Slice ptr, lookup as integer, prototype as Slice ptr = NULL, callback as FnEmbedCode=0, arg0 as any ptr=0, arg1 as any ptr=0, arg2 as any ptr=0) as Slice ptr
 BUG_IF(sl = NULL, "null menu slice ptr", NULL)
 DIM list_parent as Slice ptr = LookupSliceOrError(lookup, sl, , "plankmenu list parent")
 IF list_parent = NULL THEN RETURN NULL

 'Get holder or prototype
 DIM holder as Slice ptr
 IF prototype = NULL THEN
  'Look for a template child, from the end because that's where it normally is
  prototype = list_parent->LastChild
  WHILE prototype
   IF prototype->Template THEN EXIT WHILE
   prototype = prototype->PrevSibling
  WEND
  ERROR_IF(prototype = NULL, "Couldn't find a template child of " & SliceLookupCodename(lookup), NULL)
 END IF

 holder = LookupSlice(SL_PLANK_HOLDER, prototype)
 IF holder <> 0 THEN
  'Found a holder, use only it
  prototype = holder
  'cl->Fill = YES
 ELSE
  'No holder, use the whole collection
 END IF

 RETURN plank_menu_append(list_parent, prototype, callback, arg0, arg1, arg2)
END FUNCTION

'Add a new plank child, copied from 'prototype' to a child of list_parent slice, or leave
'it parented to prototype's parent by default.
FUNCTION plank_menu_append (list_parent as Slice ptr = NULL, prototype as Slice ptr, callback as FnEmbedCode=0, arg0 as any ptr=0, arg1 as any ptr=0, arg2 as any ptr=0) as Slice ptr
 BUG_IF(list_parent = NULL ANDALSO prototype->Parent = NULL, "Need a list_parent", NULL)

 DIM cl as Slice ptr
 cl = CloneTemplate(prototype)
 BUG_IF(cl = NULL, "unclonable", NULL)
 IF list_parent ANDALSO cl->Parent <> list_parent THEN  'Don't move it if prototype was a child
  SetSliceParent cl, list_parent
 END IF

 'This likely overwrites a lookup code that's used to find a Template prototype
 'slice, which is a good thing
 cl->Lookup = SL_PLANK_HOLDER

 expand_slice_text_insert_codes cl, callback, arg0, arg1, arg2

 RETURN cl
END FUNCTION

'Delete all the non-template children of 'lookup'
'Note this is different to all other plankmenu functions in that this doesn't
'call find_all_planks, but operates on children of a particular slice.
'That's intentional, for example thingbrowser has extra planks outside the
'thinglist parent slice.
SUB plank_menu_clear (byval sl as Slice Ptr, byval lookup as integer)
 BUG_IF(sl = NULL, "null slice ptr")
 DIM list_parent as Slice ptr = LookupSliceOrError(lookup, sl, , "plankmenu list parent")
 IF list_parent = NULL THEN EXIT SUB
 DIM as Slice ptr ch = list_parent->FirstChild, nextch
 DO WHILE ch
  nextch = ch->NextSibling
  IF ch->Template = NO THEN DeleteSlice @ch
  ch = nextch
 LOOP
END SUB

SUB expand_slice_text_insert_codes (byval sl as Slice ptr, byval callback as FnEmbedCode=0, byval arg0 as any ptr=0, byval arg1 as any ptr=0, byval arg2 as any ptr=0)
 'Starting with children of the given container slice, iterate through
 ' all children and expand any ${} codes found in any TextSlice
 ' Do not descend into child slices marked with SL_PLANK_HOLDER because planks are responsible for their own text codes
 BUG_IF(sl = NULL, "null slice ptr")
 DIM ch as Slice Ptr = sl->FirstChild
 DIM dat as TextSliceData Ptr
 DO WHILE ch <> 0
  IF ch->Lookup <> SL_PLANK_HOLDER THEN
   IF ch->SliceType = slText THEN
    dat = ch->SliceData
    IF dat->s_orig = "" THEN dat->s_orig = dat->s
#IFDEF IS_GAME
    ChangeTextSlice ch, embed_text_codes(dat->s_orig, -1, callback, arg0, arg1, arg2)
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
 BUG_IF(sl = NULL, "null slice ptr")

 DIM desc as Slice ptr = sl
 DO WHILE desc
  IF desc->Lookup = lookup THEN desc->Visible = NOT hide
  desc = NextDescendent(desc, sl)
 LOOP
END SUB

SUB set_sprites_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval sprtype as SpriteType, byval picnum as integer, byval palnum as integer=-1)
 'Starting with children of the given container slice, iterate through
 ' all children and change any sprites matching the lookup code
 BUG_IF(sl = NULL, "null slice ptr")

 DIM desc as Slice ptr = sl
 DO WHILE desc
  IF desc->Lookup = lookup AND desc->SliceType = slSprite THEN
   ChangeSpriteSlice desc, sprtype, picnum, palnum
  END IF
  desc = NextDescendent(desc, sl)
 LOOP
END SUB

'Find the first scroll slice.
'It's not a bug if there is none.
FUNCTION find_plank_scroll (byval sl as Slice Ptr) as Slice ptr
 BUG_IF(sl = NULL, "null slice ptr", NULL)

 DIM desc as Slice ptr = sl
 DO WHILE desc
  IF desc->Template ORELSE desc->Lookup = SL_PLANK_HOLDER THEN
   desc = desc->NextSibling
   CONTINUE DO
  END IF
  IF desc->SliceType = slScroll THEN RETURN desc
  desc = NextDescendent(desc, sl)
 LOOP
 RETURN 0
END FUNCTION

'Scroll to the selected plank, if it's a descendent of the scroll slice.
SUB update_plank_scrolling (byref ps as PlankState)
 BUG_IF(ps.m = NULL, "null m slice ptr")

 DIM scroll as slice ptr = find_plank_scroll(ps.m)
 IF scroll ANDALSO ps.cur ANDALSO IsAncestor(ps.cur, scroll) THEN
  ScrollToChild scroll, ps.cur
 END IF
END SUB

SUB save_plank_selection (byref ps as PlankState)
 'Attempt to save the current selection and scroll position without any slice references
 'NOTE: save/restore_plank_selection shouldn't be used if the window might be resized in-between
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
 reset_menu_edit_state
 ps.selection_saved = NO
END SUB

FUNCTION focus_plank_by_extra_id(byref ps as PlankState, byval extra_idx as integer = 0, byval id as integer, byval start_parent as Slice Ptr = 0) as bool
 DIM old_cur as Slice Ptr = ps.cur

 DIM new_cur as Slice Ptr
 new_cur = find_plank_by_extra_id(ps, extra_idx, id, start_parent)
 IF new_cur THEN
  ps.cur = new_cur
  reset_menu_edit_state
  update_plank_scrolling ps
 END IF
 
 RETURN ps.cur <> old_cur
END FUNCTION

FUNCTION find_plank_by_extra_id(byref ps as PlankState, byval extra_idx as integer = 0, byval id as integer, byval start_parent as Slice Ptr = 0) as Slice Ptr
 'If more than one plank has the same Extra(0) id number, just return the first one.

 REDIM planks(any) as Slice Ptr
 IF start_parent = 0 THEN start_parent = ps.m
 find_all_planks ps, start_parent, planks()
 
 DIM sl as Slice Ptr
 FOR i as integer = 0 TO UBOUND(planks)
  sl = planks(i)
  IF sl->Extra(extra_idx) = id THEN RETURN sl
 NEXT i

 RETURN 0
END FUNCTION

Function plank_select_by_string(byref ps as PlankState, query as string) as bool
 IF ps.cur = 0 THEN
  'No cursor yet, guess a default one
  ps.cur = top_left_plank(ps)
 END IF
 
 DIM old_cur as Slice Ptr = ps.cur

 'get a list of all of the planks.
 REDIM planks(any) as Slice Ptr
 find_all_planks ps, ps.m, planks()

 'Find the current plank's index in the list
 DIM start_i as integer = -1
 FOR i as integer = 0 TO UBOUND(planks)
  IF planks(i) = ps.cur THEN start_i = i
 NEXT i
 IF start_i = -1 THEN
  start_i = 0
 END IF
 
 'Loop through the rest of the planks searching for the string in any text child
 DIM found_it as bool = NO
 FOR i as integer = start_i TO UBOUND(planks)
  IF FindTextSliceStringRecursively(planks(i), query) THEN
   ps.cur = planks(i)
   found_it = YES
   EXIT FOR
  END IF
 NEXT i
 IF NOT found_it THEN
  FOR i as integer = 0 TO start_i - 1
   IF FindTextSliceStringRecursively(planks(i), query) THEN
    ps.cur = planks(i)
    found_it = YES
    EXIT FOR
   END IF
  NEXT i
 END IF

 IF ps.cur <> old_cur THEN
  reset_menu_edit_state
  RETURN YES
 END IF
END FUNCTION


