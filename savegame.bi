#ifndef SAVEGAME_BI
#define SAVEGAME_BI

TYPE SaveSlotPreview
 valid AS INTEGER
 cur_map AS INTEGER
 hero(3) AS HeroState
 hero_id(3) AS INTEGER
 playtime AS STRING
 leader_name AS STRING
 leader_lev AS INTEGER
 use_saved_pics AS INTEGER
END TYPE

DECLARE SUB init_save_system()
DECLARE FUNCTION count_used_save_slots() AS INTEGER

DECLARE FUNCTION save_slot_used (BYVAL slot AS INTEGER) AS INTEGER
DECLARE SUB erase_save_slot (BYVAL slot AS INTEGER)

DECLARE SUB savegame (BYVAL slot AS INTEGER)
DECLARE SUB saveglobalvars (BYVAL slot AS INTEGER, BYVAL first AS INTEGER, BYVAL last AS INTEGER)
DECLARE SUB loadgame (BYVAL slot AS INTEGER)
DECLARE SUB loadglobalvars (BYVAL slot AS INTEGER, BYVAL first AS INTEGER, BYVAL last AS INTEGER)

DECLARE SUB get_save_slot_preview(BYVAL slot AS INTEGER, pv AS SaveSlotPreview)

#endif

