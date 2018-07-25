
#ifndef COMMON_MENUS_BI
#define COMMON_MENUS_BI

#include "udts.bi"

' A function which returns true to quit the menu
TYPE FnMenuLogic as FUNCTION (menu as MenuDef, state as MenuState, dataptr as any ptr) as bool

DECLARE SUB run_MenuDef(menu as MenuDef, each_tick as FnMenuLogic, dataptr as any ptr = NULL)


DECLARE FUNCTION editbitset (array() as integer, byval wof as integer, byval last as integer, names() as string, helpkey as string="editbitset", byref remem_pt as integer = -2, byval immediate_quit as bool = NO) as bool

DECLARE SUB edit_general_bitsets()
DECLARE SUB edit_backcompat_bitsets()
DECLARE SUB edit_active_time_battle_bitsets()
DECLARE SUB edit_mouse_options ()

'Globals
EXTERN npc_movetypes() as string
EXTERN npc_pushtypes() as string
EXTERN npc_usetypes() as string
EXTERN npc_facetypes() as string

#endif
