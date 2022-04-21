'OHRRPGCE GAME - Script command implementations
'(C) Copyright 1997-2022 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef SCRIPTCOMMANDS_BI
#define SCRIPTCOMMANDS_BI

#include "slices.bi"
#include "udts.bi"

'''' Script handles

' Handles are 32-bit ints where the top 5 bits are the HandleType, the rest is
' "payload" (arbitrary data), so that different types of handles (eg slices,
' menu items) are in separate ranges.

'There can be up to 31 different types of handles (exluding 0), with possible type values -16 to 15
enum HandleType explicit
 Zone      = -2    ' Zone ID
 NPC       = -1    ' Range -&h7FFFFFF to -1, but only -300 to -1 are used
 None      = 0     ' Range 1 to &h7FFFFFF. Context-dependent, might be some kind of ID
 'Menu      = 1
 'MenuItem  = 2
 Slice     = 12    ' Slice handles are spread over four types, to get 2 extra SLICE_HANDLE_CTR_MASK
                   ' bits, but decode_handle only returns Slice.
 Slice2    = 13
 Slice3    = 14
 Slice4    = 15
 Error     = 16
end enum

#define HANDLE_TYPE_BITS        5   '2^5 = 32 different types
#define HANDLE_TYPE_SHIFT       (32 - HANDLE_TYPE_BITS)
#define HANDLE_TYPE_MASK        &hF8000000  '31 shl HANDLE_TYPE_SHIFT
#define HANDLE_PAYLOAD_MASK     &h07FFFFFF  '(1 shl HANDLE_TYPE_SHIFT) - 1
'Only the bottom 27 bits of payload are used, it can be either an integer in the range 0 to &h07FFFFFF
'or a signed integer in range -&h04000000 to &h03FFFFFF.
#define make_handle(type, payload) ((type SHL HANDLE_TYPE_SHIFT) OR (payload AND HANDLE_PAYLOAD_MASK))
'The handle type is a signed integer -16 to 15
#define get_handle_type(handle)    (CAST(integer, handle) SHR HANDLE_TYPE_SHIFT)
'Returns the payload as an unsigned value 0 to &h07FFFFFF
#define get_handle_payload(handle) (CAST(uinteger, handle) AND HANDLE_PAYLOAD_MASK)
'Returns the payload as a signed value -&h04000000 to &h03FFFFFF
#define get_handle_signed_payload(handle) ((CAST(integer, handle) SHL HANDLE_TYPE_BITS) SHR HANDLE_TYPE_BITS)

'Slice handles point to a slot of plotslices() and have a counter that's incremented every time
'the slot is reused, so that stale slice handles can be detected with high confidence.
'Slot 0 is never used.
'Note that slice handles loaded from old .rsav files count from 1, which is equivalent
'to HandleType = 0 and CTR = 0 but they still work everywhere because we check against .handle in
'plotslices() rather than checking the type mask.
#define SLICE_HANDLE_CTR_SHIFT  21
#define SLICE_HANDLE_CTR_MASK   &h1FE00000  '255 shl CTR_SHIFT. Includes lower 2 bits of HANDLE_TYPE_MASK!
#define SLICE_HANDLE_SLOT_MASK  &h001FFFFF  '(1 shr CTR_SHIFT) - 1. Max 2.1 million slice handles

'An element of plotslices()
TYPE SliceHandleSlot
  sl as Slice ptr     '0 if this slot is unused
  handle as integer   'The slice handle. If the slot is used then always
                      '(.handle AND SLICE_HANDLE_SLOT_MASK) = .sl->TableSlot (the plotslices() index).
                      'If this slot is unused then (.handle AND SLICE_HANDLE_CTR_MASK) is kept as
                      'the previous ctr value to be incremented, the rest is considered garbage.
END TYPE


DECLARE FUNCTION checksaveslot (slot as integer) as integer
DECLARE SUB erasesaveslot (slot as integer)

DECLARE SUB embedtext (text as string, byval limit as integer = 0, byval saveslot as integer=-1)
DECLARE FUNCTION embed_text_codes (text_in as string, byval saveslot as integer=-1, byval callback as FnEmbedCode=0, byval arg0 as any ptr=0, byval arg1 as any ptr=0, byval arg2 as any ptr=0) as string
DECLARE FUNCTION standard_embed_codes(act as string, byval arg as integer) as string
DECLARE FUNCTION saveslot_embed_codes(byval saveslot as integer, act as string, byval arg as integer) as string

DECLARE FUNCTION herobyrank (byval rank as integer) as integer
DECLARE FUNCTION rank_to_party_slot (byval rank as integer) as integer
DECLARE FUNCTION party_slot_to_rank (byval slot as integer) as integer
DECLARE FUNCTION rankincaterpillar (byval heroid as integer) as integer

DECLARE SUB trigger_onkeypress_script ()
DECLARE SUB process_wait_conditions ()
DECLARE SUB script_functions (cmdid as integer)

DECLARE SUB wrappedsong (byval songnumber as integer)
DECLARE SUB stopsong
DECLARE FUNCTION backcompat_sound_id (byval id as integer) as integer

DECLARE FUNCTION decode_handle(byref ret as any ptr, handle as integer, errlvl as scriptErrEnum = serrBadOp) as HandleType
DECLARE FUNCTION get_handle_extravec(handle as integer) as integer vector ptr

DECLARE FUNCTION getnpcref (byval seekid as NPCScriptref, byval copynum as integer, byval pool as integer=0) as NPCIndex
DECLARE FUNCTION get_valid_npc (byval seekid as NPCScriptref, byval errlvl as scriptErrEnum = serrBadOp, byval pool as integer=0) as NPCIndex
DECLARE FUNCTION get_valid_npc_id_pool (seekid as NPCScriptref, pool as integer=-1, byref retid as NPCTypeID, byref retpool as integer) as bool

DECLARE FUNCTION get_handle_slice(byval handle as integer, byval errlvl as scriptErrEnum = serrBadOp) as Slice ptr
DECLARE FUNCTION get_handle_typed_slice(byval handle as integer, byval sltype as SliceTypes, byval errlvl as scriptErrEnum = serrBadOp) as Slice ptr
'DECLARE FUNCTION get_arg_slice(byval argno as integer, byval errlvl as scriptErrEnum = serrBadOp) as Slice ptr
'DECLARE FUNCTION get_arg_typed_slice(byval argno as integer, byval sltype as SliceTypes, byval errlvl as scriptErrEnum = serrBadOp) as Slice ptr
#DEFINE get_arg_slice(argno, errlvl...)  get_handle_slice(retvals(argno), errlvl)
#DEFINE get_arg_typed_slice(argno, sltype, errlvl...)  get_handle_typed_slice(retvals(argno), sltype, errlvl)

#DEFINE get_arg_containersl(argno)   get_arg_typed_slice(argno, slContainer)
#DEFINE get_arg_spritesl(argno)      get_arg_typed_slice(argno, slSprite)
#DEFINE get_arg_textsl(argno)        get_arg_typed_slice(argno, slText)
#DEFINE get_arg_rectsl(argno)        get_arg_typed_slice(argno, slRectangle)
#DEFINE get_arg_linesl(argno)        get_arg_typed_slice(argno, slLine)
#DEFINE get_arg_ellipsesl(argno)     get_arg_typed_slice(argno, slEllipse)
#DEFINE get_arg_mapsl(argno)         get_arg_typed_slice(argno, slMap)
#DEFINE get_arg_gridsl(argno)        get_arg_typed_slice(argno, slGrid)
#DEFINE get_arg_scrollsl(argno)      get_arg_typed_slice(argno, slScroll)
#DEFINE get_arg_selectsl(argno)      get_arg_typed_slice(argno, slSelect)
#DEFINE get_arg_panelsl(argno)       get_arg_typed_slice(argno, slPanel)
#DEFINE get_arg_layoutsl(argno)      get_arg_typed_slice(argno, slLayout)

#DEFINE get_arg_extravec(argno)      get_handle_extravec(retvals(argno))

DECLARE SUB slice_bad_op(sl as Slice ptr, message as zstring ptr, errlev as scriptErrEnum = serrBadOp)
DECLARE FUNCTION get_arg_resizeable_slice(byval argno as integer, byval horiz_fill_ok as bool=NO, byval vert_fill_ok as bool=NO) as Slice ptr
DECLARE FUNCTION get_slice_drawopts(sl as Slice ptr, required as bool = YES) as DrawOptions ptr

DECLARE FUNCTION create_plotslice_handle(byval sl as Slice Ptr) as integer
DECLARE FUNCTION find_plotslice_handle(byval sl as Slice Ptr) as integer
DECLARE SUB restore_saved_plotslice_handle(byval sl as Slice Ptr, handle as integer)

DECLARE FUNCTION find_menu_id (byval id as integer) as integer
DECLARE FUNCTION find_menu_handle (byval handle as integer) as integer
DECLARE FUNCTION valid_menu_handle (handle as integer, byref menuslot as integer) as bool
DECLARE FUNCTION find_menu_item_handle (byval handle as integer, byref found_in_menuslot as integer) as integer
DECLARE FUNCTION valid_menu_item_handle (handle as integer, byref found_in_menuslot as integer, byref found_in_mislot as integer = 0) as bool
DECLARE FUNCTION valid_menu_item_handle_ptr (handle as integer, byref mi as MenuDefItem ptr, byref found_in_menuslot as integer = 0, byref found_in_mislot as integer = 0) as bool
DECLARE FUNCTION assign_menu_item_handle (byref mi as menudefitem) as integer
DECLARE FUNCTION assign_menu_handles (byref menu as menudef) as integer
DECLARE FUNCTION menu_item_handle_by_slot(byval menuslot as integer, byval mislot as integer, byval visible_only as bool=YES) as integer
DECLARE FUNCTION find_menu_item_slot_by_string(byval menuslot as integer, s as string, byval mislot as integer=0, byval visible_only as bool=YES) as integer

DECLARE FUNCTION valid_player_num(byval player as integer) as bool
DECLARE FUNCTION valid_item_slot(byval item_slot as integer) as bool
DECLARE FUNCTION valid_item(byval itemid as integer) as bool
DECLARE FUNCTION valid_hero_caterpillar_rank(who as integer) as bool
DECLARE FUNCTION valid_hero_party(byval who as integer, byval minimum as integer=0) as bool
DECLARE FUNCTION really_valid_hero_party(byval who as integer, byval maxslot as integer=40, byval errlvl as scriptErrEnum = serrBadOp) as bool
DECLARE FUNCTION valid_stat(byval statid as integer) as bool
DECLARE FUNCTION valid_plotstr(byval n as integer, byval errlvl as scriptErrEnum = serrBound) as bool
DECLARE FUNCTION valid_enemy(byval id as integer) as bool
DECLARE FUNCTION valid_attack(byval id_plus_1 as integer) as bool
DECLARE FUNCTION valid_formation(byval form as integer) as bool
DECLARE FUNCTION valid_formation_slot(byval form as integer, byval slot as integer) as bool
DECLARE FUNCTION valid_zone(byval id as integer) as bool
DECLARE FUNCTION valid_door OVERLOAD (byval id as integer) as bool
DECLARE FUNCTION valid_door(thisdoor as door, byval id as integer=-1) as bool
DECLARE FUNCTION valid_map(map_id as integer) as bool
DECLARE FUNCTION valid_map_layer(layer as integer, byval errlvl as scriptErrEnum = serrBadOp) as bool
DECLARE FUNCTION valid_tile_pos(byval x as integer, byval y as integer) as bool
DECLARE FUNCTION valid_save_slot(slot as integer) as bool
DECLARE FUNCTION valid_color(index as integer) as bool
DECLARE FUNCTION valid_box_style(index as integer) as bool
DECLARE FUNCTION valid_spriteset(spritetype as SpriteType, record as integer) as bool

DECLARE SUB greyscalepal
DECLARE SUB tweakpalette (byval r as integer, byval g as integer, byval b as integer, byval first as integer = 0, byval last as integer = 255)
DECLARE SUB write_checkpoint ()

#endif
