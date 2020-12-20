'OHRRPGCE GAME - Saving and loading games
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef SAVEGAME_BI
#define SAVEGAME_BI

TYPE SaveSlotPreview
 valid as integer    'Whether this slot is not empty
 cur_map as integer
 hero(3) as HeroState
 hero_id(3) as integer  'Hero ID numbers +1 (0 indicates empty slot)
 playtime as string
 leader_name as string
 leader_lev as integer
END TYPE

DECLARE SUB init_save_system()

DECLARE SUB savegame (byval slot as integer, prefix as string="")
DECLARE SUB saveglobalvars (byval slot as integer, byval first as integer, byval last as integer)
DECLARE SUB loadgame (byval slot as integer, prefix as string="")
DECLARE SUB loadglobalvars (byval slot as integer, byval first as integer, byval last as integer)

DECLARE SUB get_save_slot_preview(byval slot as integer, pv as SaveSlotPreview)

DECLARE FUNCTION save_slot_used (byval slot as integer, prefix as string="") as integer
DECLARE SUB erase_save_slot (byval slot as integer)
DECLARE FUNCTION count_used_save_slots() as integer

'Helper functions to quickly read stuff from save slots without loading the whole save
DECLARE FUNCTION saveslot_quick_root_node(byval saveslot as integer) as NodePtr
DECLARE FUNCTION saveslot_findhero(byval saveroot as NodePtr, byval id as integer) as integer
DECLARE FUNCTION saveslot_rank_to_party_slot (byval saveroot as NodePtr, byval rank as integer) as integer
DECLARE FUNCTION saveslot_hero_name_by_slot(byval saveroot as NodePtr, byval slot as integer) as string
DECLARE FUNCTION saveslot_hero_id_by_slot(byval saveroot as NodePtr, byval slot as integer) as integer
DECLARE FUNCTION saveslot_global(byval saveroot as NodePtr, byval global_id as integer) as integer
DECLARE FUNCTION saveslot_plotstr(byval saveroot as NodePtr, byval string_id as integer) as string

#endif

