#ifndef PLANKMENU_BI
#define PLANKMENU_BI

TYPE PlankState
 m as Slice Ptr 'Container for the whole plank menu
 cur as Slice Ptr 'currently selected plank
 is_plank_callback as ANY Ptr 'Function () as bool
END TYPE

CONST plankNORMAL = 0
CONST plankSEL = 1
CONST plankDISABLE = 2
CONST plankSELDISABLE = 3
CONST plankSPECIAL = 4
CONST plankSELSPECIAL = 5
CONST plankITEMSEL = 6
CONST plankITEMSELDISABLE = 7
CONST plankITEMSELSPECIAL = 8

DECLARE SUB plank_menu_clear (byval sl as Slice Ptr, byval lookup as integer)
DECLARE FUNCTION plank_menu_append OVERLOAD (byval sl as slice ptr, byval lookup as integer, byval collection_kind as integer, byval callback as ANY ptr=0, byval arg0 as integer=0, byval arg1 as integer=0, byval arg2 as integer=0) as Slice Ptr
DECLARE FUNCTION plank_menu_append OVERLOAD (byval sl as slice ptr, byval lookup as integer, byval collection as Slice Ptr, byval callback as ANY ptr=0, byval arg0 as integer=0, byval arg1 as integer=0, byval arg2 as integer=0) as Slice Ptr
DECLARE SUB set_plank_state (byval sl as Slice Ptr, byval state as integer=plankNORMAL)
DECLARE FUNCTION plank_menu_arrows (byref ps as PlankState) as bool
DECLARE SUB expand_slice_text_insert_codes (byval sl as Slice ptr, byval callback as ANY ptr=0, byval arg0 as integer=0, byval arg1 as integer=0, byval arg2 as integer=0)
DECLARE SUB hide_slices_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval cond as bool)
DECLARE SUB set_sprites_by_lookup_code (byval sl as Slice ptr, byval lookup as integer, byval sprtype as SpriteType, byval picnum as integer, byval palnum as integer=-1)

#endif
