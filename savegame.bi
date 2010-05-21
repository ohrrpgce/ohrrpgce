#ifndef SAVEGAME_BI
#define SAVEGAME_BI

DECLARE SUB init_save_system()
DECLARE FUNCTION count_used_save_slots() AS INTEGER

DECLARE SUB savegame (slot as integer)
DECLARE SUB saveglobalvars (slot as integer, first as integer, last as integer)
DECLARE SUB loadgame (slot as integer)
DECLARE SUB loadglobalvars (slot as integer, first as integer, last as integer)

#endif

