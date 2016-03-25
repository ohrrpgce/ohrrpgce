'OHRRPGCE COMMON - Game/Custom shared menu code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef MENUS_BI
#define MENUS_BI

'*** Requires construction (with ClearMenuData or LoadMenuData) ***

'' Generic MenuState Stuff
DECLARE SUB init_menu_state OVERLOAD (byref state as MenuState, menu() as SimpleMenuItem)
DECLARE SUB init_menu_state OVERLOAD (byref state as MenuState, byval menu as BasicMenuItem vector)
DECLARE SUB init_menu_state OVERLOAD (byref state as MenuState, menu() as string)
DECLARE SUB clamp_menu_state (byref state as MenuState)
DECLARE SUB append_simplemenu_item (byref menu as SimpleMenuItem vector, caption as string, byval unselectable as bool = NO, byval col as integer = 0, byval dat as integer = 0, byval where as integer = -1)
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, byval deckey as integer = scUp, byval inckey as integer = scDown) as bool
DECLARE FUNCTION usemenu OVERLOAD (byref pt as integer, byref top as integer, byval first as integer, byval last as integer, byval size as integer, byval deckey as integer = scUp, byval inckey as integer = scDown) as bool
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, byval menudata as BasicMenuItem vector, byval deckey as integer = scUp, byval inckey as integer = scDown) as bool
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, selectable() as bool, byval deckey as integer = scUp, byval inckey as integer = scDown) as bool
DECLARE FUNCTION scrollmenu (state as MenuState, byval deckey as integer = scUp, byval inckey as integer = scDown) as bool
DECLARE SUB standard_to_basic_menu (menu() as string, byref state as MenuState, byref basicmenu as BasicMenuItem vector, byval shaded as bool ptr = NULL)
DECLARE SUB standardmenu OVERLOAD (menu() as string, state as MenuState, byval x as integer, byval y as integer, byval page as integer, menuopts as MenuOptions = MenuOptions())
DECLARE SUB standardmenu OVERLOAD (menu() as string, state as MenuState, shaded() as bool, byval x as integer, byval y as integer, byval page as integer, menuopts as MenuOptions = MenuOptions())
DECLARE SUB standardmenu OVERLOAD (byval menu as BasicMenuItem vector, state as MenuState, byval x as integer, byval y as integer, byval page as integer, menuopts as MenuOptions = MenuOptions())

'' Selection by typing
DECLARE FUNCTION select_by_typing (selectst as SelectTypeState, byval allow_numbers as bool = YES) as bool
DECLARE SUB select_clear (selectst as SelectTypeState)
DECLARE SUB highlight_menu_typing_selection OVERLOAD (menu() as string, menu_display() as string, selectst as SelectTypeState, state as MenuState)
DECLARE SUB highlight_menu_typing_selection OVERLOAD (byref menu as BasicMenuItem vector, byref menu_display as BasicMenuItem vector, selectst as SelectTypeState, state as MenuState)
DECLARE FUNCTION highlight_menu_typing_selection_string(z as string, selectst as SelectTypeState) as string
DECLARE SUB select_on_word_boundary_excluding OVERLOAD (menu() as string, selectst as SelectTypeState, state as MenuState, excludeword as string)
DECLARE SUB select_on_word_boundary_excluding OVERLOAD (byval menu as BasicMenuItem vector, selectst as SelectTypeState, state as MenuState, excludeword as string)
DECLARE SUB select_on_word_boundary OVERLOAD (menu() as string, selectst as SelectTypeState, state as MenuState)
DECLARE SUB select_on_word_boundary OVERLOAD (byval menu as BasicMenuItem vector, selectst as SelectTypeState, state as MenuState)
DECLARE SUB select_instr(menu() as string, selectst as SelectTypeState, state as MenuState)

'' MenuDef
DECLARE SUB ClearMenuData(dat as MenuDef)
DECLARE SUB DeleteMenuItems(menu as MenuDef)
DECLARE SUB SortMenuItems(menu as MenuDef)
DECLARE FUNCTION getmenuname(byval record as integer) as string
DECLARE SUB init_menu_state OVERLOAD (byref state as MenuState, menu as MenuDef)
DECLARE FUNCTION append_menu_item(byref menu as MenuDef, caption as string, byval t as integer=0, byval sub_t as integer=0) as integer
DECLARE SUB remove_menu_item OVERLOAD (byref menu as MenuDef, byval mi as MenuDefItem ptr)
DECLARE SUB remove_menu_item OVERLOAD (byref menu as MenuDef, byval mislot as integer)
DECLARE SUB swap_menu_items(byref menu1 as MenuDef, byval mislot1 as integer, byref menu2 as MenuDef, byval mislot2 as integer)

'' Saving/Loading/(De)serializing MenuDefs
DECLARE SUB LoadMenuData(menu_set as MenuSet, dat as MenuDef, byval record as integer, byval ignore_items as integer=NO)
DECLARE SUB SaveMenuData(menu_set as MenuSet, dat as MenuDef, byval record as integer)
DECLARE SUB MenuBitsToArray (menu as MenuDef, bits() as integer)
DECLARE SUB MenuBitsFromArray (menu as MenuDef, bits() as integer)
DECLARE SUB MenuItemBitsToArray (mi as MenuDefItem, bits() as integer)
DECLARE SUB MenuItemBitsFromArray (mi as MenuDefItem, bits() as integer)
DECLARE FUNCTION read_menu_int (menu as MenuDef, byval intoffset as integer) as integer
DECLARE SUB write_menu_int (menu as MenuDef, byval intoffset as integer, byval n as integer)
DECLARE FUNCTION read_menu_item_int (mi as MenuDefItem, byval intoffset as integer) as integer
DECLARE SUB write_menu_item_int (mi as MenuDefItem, byval intoffset as integer, byval n as integer)

'' Drawing MenuDefs
DECLARE SUB draw_menu (menu as MenuDef, state as MenuState, byval page as integer)
DECLARE SUB position_menu_item (menu as MenuDef, cap as string, byval i as integer, byref where as XYPair)
DECLARE SUB position_menu (menu as MenuDef, byval page as integer)
DECLARE FUNCTION anchor_point(byval anchor as integer, byval size as integer) as integer
DECLARE FUNCTION count_menu_items (menu as MenuDef) as integer
DECLARE FUNCTION get_menu_item_caption (mi as MenuDefItem, menu as MenuDef) as string
DECLARE FUNCTION get_special_menu_caption(byval subtype as integer) as string
DECLARE FUNCTION get_menu_item_editing_annotation (mi as MenuDefItem) as string

'' Scrollbars!
DECLARE SUB draw_scrollbar OVERLOAD (state as MenuState, menu as MenuDef, byval page as integer)
DECLARE SUB draw_scrollbar OVERLOAD (state as MenuState, rect as RectType, byval boxstyle as integer=0, byval page as integer)
DECLARE SUB draw_scrollbar OVERLOAD (state as MenuState, rect as RectType, byval count as integer, byval boxstyle as integer=0, byval page as integer)
DECLARE SUB draw_fullscreen_scrollbar(state as MenuState, byval boxstyle as integer=0, byval page as integer)


#endif
