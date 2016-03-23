
#ifndef COMMON_MENUS_BI
#define COMMON_MENUS_BI

DECLARE FUNCTION editbitset (array() as integer, byval wof as integer, byval last as integer, names() as string, helpkey as string="editbitset", byref remem_pt as integer = -2, byval immediate_quit as integer = NO) as integer

DECLARE SUB edit_general_bitsets()
DECLARE SUB edit_backcompat_bitsets()
DECLARE SUB edit_active_time_battle_bitsets()

#endif