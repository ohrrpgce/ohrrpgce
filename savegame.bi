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

DECLARE SUB savegame (byval slot as integer)
DECLARE SUB saveglobalvars (byval slot as integer, byval first as integer, byval last as integer)
DECLARE SUB loadgame (byval slot as integer)
DECLARE SUB loadglobalvars (byval slot as integer, byval first as integer, byval last as integer)

DECLARE SUB get_save_slot_preview(byval slot as integer, pv as SaveSlotPreview)

DECLARE FUNCTION save_slot_used (byval slot as integer) as integer
DECLARE SUB erase_save_slot (byval slot as integer)
DECLARE FUNCTION count_used_save_slots() as integer

#endif

