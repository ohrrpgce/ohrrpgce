'OHRRPGCE - Plank-slice-based menus
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef PLANKMENU_BI
#define PLANKMENU_BI

' This is a state that a menu item of a plank menu could be in,
' although other special-purpose values maybe be allowed
ENUM PlankItemState
 plankNORMAL = 0
 plankSEL = 1
 plankDISABLE = 2
 plankSELDISABLE = 3
 plankSPECIAL = 4
 plankSELSPECIAL = 5
 plankITEMSWAP = 6
 plankITEMSWAPDISABLE = 7
 plankITEMSWAPSPECIAL = 8
 plankMOUSEHOVER = 9
END ENUM

TYPE FnIsPlank as Function(byval sl as Slice Ptr) as bool
TYPE FnPlankSetState as Sub(byval sl as Slice Ptr, byval state as PlankItemState)

DECLARE SUB set_plank_state_default_callback (byval sl as Slice Ptr, byval state as PlankItemState)

' Used by embed_text_codes to try to expand unknown codes
TYPE FnEmbedCode as Sub(code as string, result as string, arg0 as any ptr, arg1 as any ptr, arg2 as any ptr)

TYPE PlankState
 m as Slice Ptr                    'Container for the whole plank menu
 cur as Slice Ptr                  'Currently selected plank
 hover as Slice Ptr                'The plank the mouse is hovering over (only updated if you call plank_menu_update_hover)
 is_plank_callback as FnIsPlank
 state_callback as FnPlankSetState
 selection_saved as bool           'Position has been saved by save_plank_selection()
 _saved_pos as XYPair              'Saved by save_plank_selection()
 '_saved_scroll as XYPair
END TYPE

'Load a collection and return the first plank found inside
DECLARE FUNCTION load_plank_from_file(filename as string) as Slice Ptr

'Call plank_menu_clear before refreshing the menu
DECLARE SUB plank_menu_clear (byval sl as Slice Ptr, byval lookup as integer)

'plank_menu_append is called multiple times to rebuild the plank menu
DECLARE FUNCTION plank_menu_append OVERLOAD (sl as Slice ptr, lookup as integer, collection_kind as integer, callback as FnEmbedCode=0, arg0 as any ptr=0, arg1 as any ptr=0, arg2 as any ptr=0) as Slice ptr
DECLARE FUNCTION plank_menu_append OVERLOAD (sl as Slice ptr, lookup as integer, prototype as Slice ptr=NULL, callback as FnEmbedCode=0, arg0 as any ptr=0, arg1 as any ptr=0, arg2 as any ptr=0) as Slice ptr
DECLARE FUNCTION plank_menu_append OVERLOAD (list_parent as Slice ptr=NULL, prototype as Slice ptr, callback as FnEmbedCode=0, arg0 as any ptr=0, arg1 as any ptr=0, arg2 as any ptr=0) as Slice ptr

'Updates a plank's visual state to constants like plankNORMAL or plankSEL (and others)
'See set_plank_state_default_callback() for details on what changes are applied.
'Specific behavior can be overridden by giving the PlankState a .state_callback
DECLARE SUB set_plank_state (byref ps as PlankState, byval sl as Slice Ptr, byval state as PlankItemState = plankNORMAL)

TYPE FwdSide
 fwd as integer
 side as integer
END TYPE

'
TYPE PlankViewpoint 'PlankCursorComparator
 prev as Slice ptr
 axis as integer
 d as integer
 prev_center as FwdSide
 prev_front as FwdSide
 prev_edgelen as integer

 DECLARE CONSTRUCTOR (prev as Slice ptr, axis as integer, d as integer)
 DECLARE FUNCTION plank_effective_pos (byref ret as FwdSide, pl as Slice ptr) as bool
END TYPE

'plank_menu_arrows handles arrow key movement, and updates PlankState.cur and returns true .cur has changed, so you can update the visuals
DECLARE FUNCTION plank_menu_arrows (byref ps as PlankState, byval start_parent as Slice Ptr=0, byval linear_left_right as bool = NO) as bool

'Scrolls the scrollable area of the plank menu by a given amount
DECLARE FUNCTION plank_menu_scroll(byref ps as PlankState, byval scroll_move as integer, byval mouse_must_be_in_scroll as bool=YES) as bool
'Handles mouse wheel scrolling, updates PlankState.cur, and returns true if changed
DECLARE FUNCTION plank_menu_mouse_wheel(byref ps as PlankState, byval dist as integer=30) as bool
'Handles right-drag scrolling and/or two-finger-touch scrolling
DECLARE FUNCTION plank_menu_drag_scroll(byref ps as PlankState, byval which_button as MouseButton=mouseLeft, byval min_threshold as integer=10) as bool
'Search for the matching string in all text children of each plank, and return YES if the cursor changed

DECLARE FUNCTION plank_select_by_string(byref ps as PlankState, query as string) as bool
'Update the mouse hover property of the PlankState
DECLARE FUNCTION plank_menu_update_hover(byref ps as PlankState) as bool

'This expands the insert codes in the slice collection,using an optional callback for special ones
DECLARE SUB expand_slice_text_insert_codes (byval sl as Slice ptr, byval callback as FnEmbedCode=0, byval arg0 as any ptr=0, byval arg1 as any ptr=0, byval arg2 as any ptr=0)

DECLARE SUB hide_slices_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval hide as bool)
DECLARE SUB set_sprites_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval sprtype as SpriteType, byval picnum as integer, byval palnum as integer=-1)
DECLARE FUNCTION default_is_plank(byval sl as Slice Ptr) as bool
DECLARE SUB find_all_planks(byref ps as PlankState, byval m as Slice Ptr, planks() as Slice Ptr)
DECLARE FUNCTION top_left_plank(byref ps as PlankState, byval start_parent as Slice Ptr=0) as Slice Ptr
DECLARE FUNCTION bottom_right_plank(byref ps as PlankState, byval start_parent as Slice Ptr=0) as Slice Ptr
DECLARE SUB update_plank_scrolling (byref ps as PlankState)
DECLARE FUNCTION find_plank_scroll (byval sl as Slice Ptr) as slice ptr
DECLARE SUB save_plank_selection (byref ps as PlankState)
DECLARE SUB restore_plank_selection (byref ps as PlankState)
DECLARE FUNCTION plank_menu_move_cursor (byref ps as PlankState, byval axis as integer, byval d as integer, byval start_parent as Slice Ptr=0) as bool
DECLARE SUB plank_menu_scroll_page (byref ps as PlankState, byval scrolldir as integer, byval start_parent as Slice Ptr=0)
DECLARE FUNCTION plank_menu_home(byref ps as PlankState) as bool
DECLARE FUNCTION plank_menu_end(byref ps as PlankState) as bool
DeCLARE FUNCTION plank_menu_select_prev_next(byref ps as PlankState, offset as integer) as bool

'Find the best plank at a specific screen location (like a mouse click)
DECLARE FUNCTION find_plank_at_screen_pos(byref ps as PlankState, byval targpos as XYPair, byval start_parent as Slice Ptr=0) as Slice Ptr
'Find the closest plank to a screen location, for example when a selected plank vanishes, and you want to make the selection jump to the next closest one
DECLARE FUNCTION find_plank_nearest_screen_pos(byref ps as PlankState, byval targpos as XYPair, byval start_parent as Slice Ptr=0) as Slice Ptr

'Search for a plank with the given ID number in its ->Extra(0) slot
DECLARE FUNCTION find_plank_by_extra_id(byref ps as PlankState, byval extra_idx as integer = 0, byval id as integer, byval start_parent as Slice Ptr = 0) as Slice Ptr
'Same as find_plank_by_extra_id() except also selects it, and updates scrolling, and returns YES if the selection has changed
DECLARE FUNCTION focus_plank_by_extra_id(byref ps as PlankState, byval extra_idx as integer = 0, byval id as integer, byval start_parent as Slice Ptr = 0) as bool

#endif
