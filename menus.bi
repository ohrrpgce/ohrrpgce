'OHRRPGCE COMMON - Game/Custom shared menu code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef MENUS_BI
#define MENUS_BI

'*** Requires construction (with ClearMenuData or LoadMenuData) + destruction (with DeleteMenuItems) ***

'' Generic MenuState Stuff
DECLARE SUB init_menu_state OVERLOAD (BYREF state AS MenuState, menu() AS SimpleMenu, BYVAL pickenabled AS INTEGER = YES)
DECLARE SUB clamp_menu_state (BYREF state AS MenuState)
DECLARE SUB append_simplemenu_item (menu() as SimpleMenu, caption as string, BYVAL enabled as integer = YES, BYVAL col as integer = -1, BYVAL dat as integer = 0, BYVAL where as integer = -1)
DECLARE FUNCTION usemenu OVERLOAD (state AS MenuState, deckey = scUp, inckey = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (pt, top, first, last, size, deckey = scUp, inckey = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, menudata() as SimpleMenu, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, enabled() as INTEGER, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE FUNCTION scrollmenu (state AS MenuState, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE SUB standardmenu OVERLOAD (menu() AS STRING, state AS MenuState, x AS INTEGER, y AS INTEGER, page AS INTEGER, edge AS INTEGER=NO, hidecursor AS INTEGER=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, toggle=YES)
DECLARE SUB standardmenu OVERLOAD (menu() AS STRING, state AS MenuState, shaded() AS INTEGER, x AS INTEGER, y AS INTEGER, page AS INTEGER, edge AS INTEGER=NO, hidecursor AS INTEGER=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, toggle=YES)
DECLARE SUB standardmenu OVERLOAD (menu() AS STRING, size, vis, pt, top, x, y, page, edge=NO, wide=999, highlight=NO, shaded AS INTEGER PTR=NULL, toggle=YES)

'' MenuDef
DECLARE SUB ClearMenuData(dat AS MenuDef)
DECLARE SUB DeleteMenuItems(menu AS MenuDef)
DECLARE SUB ClearMenuItem(mi AS MenuDefItem)
DECLARE SUB SortMenuItems(menu AS MenuDef)
DECLARE FUNCTION getmenuname(record AS INTEGER) AS STRING
DECLARE SUB init_menu_state OVERLOAD (BYREF state AS MenuState, menu AS MenuDef)
DECLARE FUNCTION append_menu_item(BYREF menu AS MenuDef, caption AS STRING, t AS INTEGER=0, sub_t AS INTEGER=0) as integer
DECLARE SUB remove_menu_item OVERLOAD (BYREF menu AS MenuDef, BYVAL mi AS MenuDefItem ptr)
DECLARE SUB remove_menu_item OVERLOAD (BYREF menu AS MenuDef, BYVAL mislot AS INTEGER)
DECLARE SUB swap_menu_items(BYREF menu1 AS MenuDef, BYVAL mislot1 AS INTEGER, BYREF menu2 AS MenuDef, BYVAL mislot2 AS INTEGER)

'' Saving/Loading/(De)serializing MenuDefs
DECLARE SUB LoadMenuData(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER, ignore_items AS INTEGER=NO)
DECLARE SUB SaveMenuData(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER)
DECLARE SUB MenuBitsToArray (menu AS MenuDef, bits() AS INTEGER)
DECLARE SUB MenuBitsFromArray (menu AS MenuDef, bits() AS INTEGER)
DECLARE SUB MenuItemBitsToArray (mi AS MenuDefItem, bits() AS INTEGER)
DECLARE SUB MenuItemBitsFromArray (mi AS MenuDefItem, bits() AS INTEGER)
DECLARE FUNCTION read_menu_int (menu AS MenuDef, intoffset AS INTEGER) as integer
DECLARE SUB write_menu_int (menu AS MenuDef, intoffset AS INTEGER, n AS INTEGER)
DECLARE FUNCTION read_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER) as integer
DECLARE SUB write_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER, n AS INTEGER)

'' Drawing MenuDefs
DECLARE SUB draw_menu (menu AS MenuDef, state AS MenuState, page AS INTEGER)
DECLARE SUB position_menu_item (menu AS MenuDef, cap AS STRING, i AS INTEGER, BYREF where AS XYPair)
DECLARE SUB position_menu (menu AS MenuDef, page AS INTEGER)
DECLARE FUNCTION anchor_point(anchor AS INTEGER, size AS INTEGER) AS INTEGER
DECLARE FUNCTION count_menu_items (menu AS MenuDef) as integer
DECLARE FUNCTION get_menu_item_caption (mi AS MenuDefItem, menu AS MenuDef) AS STRING
DECLARE FUNCTION get_special_menu_caption(subtype AS INTEGER, edit_mode AS INTEGER= NO) AS STRING

'' Scrollbars!
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, menu AS MenuDef, page AS INTEGER)
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, rect AS RectType, boxstyle AS INTEGER=0, page AS INTEGER)
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, rect AS RectType, count AS INTEGER, boxstyle AS INTEGER=0, page AS INTEGER)
DECLARE SUB draw_fullscreen_scrollbar(state AS MenuState, boxstyle AS INTEGER=0, page AS INTEGER)


#endif