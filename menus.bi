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
DECLARE FUNCTION usemenu OVERLOAD (state AS MenuState, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (BYREF pt as integer, BYREF top as integer, BYVAL first as integer, BYVAL last as integer, BYVAL size as integer, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, menudata() as SimpleMenu, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, enabled() as INTEGER, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE FUNCTION scrollmenu (state AS MenuState, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE SUB standardmenu OVERLOAD (menu() AS STRING, state AS MenuState, BYVAL x AS INTEGER, BYVAL y AS INTEGER, BYVAL page AS INTEGER, BYVAL edge AS INTEGER=NO, BYVAL hidecursor AS INTEGER=NO, BYVAL wide AS INTEGER=999, BYVAL highlight AS INTEGER=NO, BYVAL toggle AS INTEGER=YES)
DECLARE SUB standardmenu OVERLOAD (menu() AS STRING, state AS MenuState, shaded() AS INTEGER, BYVAL x AS INTEGER, BYVAL y AS INTEGER, BYVAL page AS INTEGER, BYVAL edge AS INTEGER=NO, BYVAL hidecursor AS INTEGER=NO, BYVAL wide AS INTEGER=999, BYVAL highlight AS INTEGER=NO, BYVAL toggle AS INTEGER=YES)
DECLARE SUB standardmenu OVERLOAD (menu() AS STRING, BYVAL size AS INTEGER, BYVAL vis AS INTEGER, BYVAL pt AS INTEGER, BYVAL top AS INTEGER, BYVAL x AS INTEGER, BYVAL y AS INTEGER, BYVAL page AS INTEGER, BYVAL edge AS INTEGER=NO, BYVAL wide AS INTEGER=999, BYVAL highlight AS INTEGER=NO, BYVAL shaded AS INTEGER PTR=NULL, BYVAL toggle AS INTEGER=YES)

'' MenuDef
DECLARE SUB ClearMenuData(dat AS MenuDef)
DECLARE SUB DeleteMenuItems(menu AS MenuDef)
DECLARE SUB ClearMenuItem(mi AS MenuDefItem)
DECLARE SUB SortMenuItems(menu AS MenuDef)
DECLARE FUNCTION getmenuname(BYVAL record AS INTEGER) AS STRING
DECLARE SUB init_menu_state OVERLOAD (BYREF state AS MenuState, menu AS MenuDef)
DECLARE FUNCTION append_menu_item(BYREF menu AS MenuDef, caption AS STRING, BYVAL t AS INTEGER=0, BYVAL sub_t AS INTEGER=0) as integer
DECLARE SUB remove_menu_item OVERLOAD (BYREF menu AS MenuDef, BYVAL mi AS MenuDefItem ptr)
DECLARE SUB remove_menu_item OVERLOAD (BYREF menu AS MenuDef, BYVAL mislot AS INTEGER)
DECLARE SUB swap_menu_items(BYREF menu1 AS MenuDef, BYVAL mislot1 AS INTEGER, BYREF menu2 AS MenuDef, BYVAL mislot2 AS INTEGER)

'' Saving/Loading/(De)serializing MenuDefs
DECLARE SUB LoadMenuData(menu_set AS MenuSet, dat AS MenuDef, BYVAL record AS INTEGER, BYVAL ignore_items AS INTEGER=NO)
DECLARE SUB SaveMenuData(menu_set AS MenuSet, dat AS MenuDef, BYVAL record AS INTEGER)
DECLARE SUB MenuBitsToArray (menu AS MenuDef, bits() AS INTEGER)
DECLARE SUB MenuBitsFromArray (menu AS MenuDef, bits() AS INTEGER)
DECLARE SUB MenuItemBitsToArray (mi AS MenuDefItem, bits() AS INTEGER)
DECLARE SUB MenuItemBitsFromArray (mi AS MenuDefItem, bits() AS INTEGER)
DECLARE FUNCTION read_menu_int (menu AS MenuDef, BYVAL intoffset AS INTEGER) as integer
DECLARE SUB write_menu_int (menu AS MenuDef, BYVAL intoffset AS INTEGER, BYVAL n AS INTEGER)
DECLARE FUNCTION read_menu_item_int (mi AS MenuDefItem, BYVAL intoffset AS INTEGER) as integer
DECLARE SUB write_menu_item_int (mi AS MenuDefItem, BYVAL intoffset AS INTEGER, BYVAL n AS INTEGER)

'' Drawing MenuDefs
DECLARE SUB draw_menu (menu AS MenuDef, state AS MenuState, BYVAL page AS INTEGER)
DECLARE SUB position_menu_item (menu AS MenuDef, cap AS STRING, BYVAL i AS INTEGER, BYREF where AS XYPair)
DECLARE SUB position_menu (menu AS MenuDef, BYVAL page AS INTEGER)
DECLARE FUNCTION anchor_point(BYVAL anchor AS INTEGER, BYVAL size AS INTEGER) AS INTEGER
DECLARE FUNCTION count_menu_items (menu AS MenuDef) as integer
DECLARE FUNCTION get_menu_item_caption (mi AS MenuDefItem, menu AS MenuDef) AS STRING
DECLARE FUNCTION get_special_menu_caption(BYVAL subtype AS INTEGER, BYVAL edit_mode AS INTEGER= NO) AS STRING

'' Scrollbars!
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, menu AS MenuDef, BYVAL page AS INTEGER)
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, rect AS RectType, BYVAL boxstyle AS INTEGER=0, BYVAL page AS INTEGER)
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, rect AS RectType, BYVAL count AS INTEGER, BYVAL boxstyle AS INTEGER=0, BYVAL page AS INTEGER)
DECLARE SUB draw_fullscreen_scrollbar(state AS MenuState, BYVAL boxstyle AS INTEGER=0, BYVAL page AS INTEGER)


#endif
