#ifndef SAVEGAME_BI
#define SAVEGAME_BI

DECLARE SUB savegame (slot as integer, stat() as integer)
DECLARE SUB saveglobalvars (slot as integer, first as integer, last as integer)
DECLARE SUB loadgame (slot as integer, stat() as integer)
DECLARE SUB loadglobalvars (slot as integer, first as integer, last as integer)

#endif

