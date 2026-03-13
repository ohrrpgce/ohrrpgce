'OHRRPGCE GAME - Various builtin menus
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef MENUSTUF_BI
#define MENUSTUF_BI

DECLARE SUB buystuff (byval shop_id as integer, byval shoptype as integer, storebuf() as integer)
DECLARE FUNCTION chkOOBtarg (byval attacker as integer, byval target as integer, byval atk as integer) as bool
DECLARE SUB update_hero_max_and_cur_stats (byval who as integer)
DECLARE FUNCTION doequip (toequip as integer, who as integer, where as integer) as bool
DECLARE FUNCTION unequip (who as integer, where as integer, resetdefwep as bool = YES, force as bool = YES) as bool
DECLARE SUB equip_menu (who as integer, allow_switch as bool = YES)
DECLARE SUB itemmenuswap (invent() as InventSlot, iuse() as integer, permask() as integer, byval it1 as integer, byval it2 as integer)
DECLARE FUNCTION item_screen () as integer
DECLARE FUNCTION use_item_by_id(byval item_id as integer, byref trigger_box as integer, name_override as string="") as bool
DECLARE FUNCTION use_item_in_slot(byval slot as integer, byref trigger_box as integer, byref consumed as bool=NO) as bool
DECLARE SUB update_inventory_caption (byval i as integer)
DECLARE FUNCTION oobcure (byval attacker as integer, byval target as integer, byval atk as integer, byval target_count as integer, byref was_full as bool=NO) as bool
DECLARE SUB patcharray (array() as integer, n as string)
DECLARE FUNCTION picksave () as integer
DECLARE FUNCTION pickload (newgame_opt as bool = YES, beep_if_no_saves as bool = NO) as integer
DECLARE SUB sellstuff (byval id as integer, storebuf() as integer)
DECLARE SUB old_spells_menu (byval who as integer)
DECLARE SUB status_screen (byval slot as integer)
DECLARE SUB spell_screen (byval slot as integer)
DECLARE FUNCTION trylearn (byval who as integer, byval atk as integer) as bool
DECLARE SUB loadshopstuf (array() as integer, byval id as integer)
DECLARE FUNCTION count_available_spells(byval who as integer, byval list as integer) as integer
DECLARE FUNCTION outside_battle_cure (byval atk as integer, byval target as integer, byval attacker as integer, byval spread as bool, byref was_full as bool=NO) as bool
DECLARE FUNCTION item_can_be_discarded (byval item_id as integer) as bool
DECLARE FUNCTION item_can_be_used_bits (byval item_id as integer) as integer
DECLARE SUB inventory_autosort()
DECLARE FUNCTION touch_virtual_keyboard (default_str as string, max_length as integer=-1, prompt as string="") as string
DECLARE FUNCTION hero_uses_lmp (byval hero_slot as integer) as bool
DECLARE FUNCTION is_shop_empty(byval shop_id as integer, byval shoptype as integer) as bool
DECLARE FUNCTION is_item_plank(byval sl as Slice Ptr) as bool
DECLARE SUB set_item_plank_state (byval sl as Slice Ptr, byval state as PlankItemState)
DECLARE SUB item_screen_refresh(byref st as ItemScreenState)
DECLARE SUB ExpandTextItemScreen (code as string, result as string, byval arg0 as ANY ptr=0, byval arg1 as ANY ptr=0, byval arg2 as ANY ptr=0)
DECLARE SUB item_screen_mouse_handler(byref st as ItemScreenState)

DECLARE FUNCTION game_check_use_key() as bool
DECLARE FUNCTION game_battle_check_use_key() as bool
DECLARE FUNCTION game_check_cancel_key() as bool
DECLARE FUNCTION game_check_menu_key() as bool

#endif
