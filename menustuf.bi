'OHRRPGCE GAME - Various builtin menus
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef MENUSTUF_BI
#define MENUSTUF_BI

DECLARE SUB buystuff (byval shop_id as integer, byval shoptype as integer, storebuf() as integer)
DECLARE FUNCTION chkOOBtarg (byval attacker as integer, byval target as integer, byval atk as integer) as bool
DECLARE SUB update_hero_max_and_cur_stats (byval who as integer)
DECLARE SUB doequip (toequip as integer, who as integer, where as integer)
DECLARE FUNCTION unequip (who as integer, where as integer, resetdefwep as bool = YES, force as bool = YES) as bool
DECLARE SUB equip_menu (who as integer, allow_switch as bool = YES)
DECLARE SUB itemmenuswap (invent() as InventSlot, iuse() as integer, permask() as integer, byval it1 as integer, byval it2 as integer)
DECLARE FUNCTION item_screen () as integer
DECLARE FUNCTION use_item_by_id(byval item_id as integer, byref trigger_box as integer, name_override as string="") as bool
DECLARE FUNCTION use_item_in_slot(byval slot as integer, byref trigger_box as integer, byref consumed as bool=NO) as bool
DECLARE SUB update_inventory_caption (byval i as integer)
DECLARE SUB oobcure (byval attacker as integer, byval target as integer, byval atk as integer, byval target_count as integer)
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
DECLARE FUNCTION outside_battle_cure (byval atk as integer, byval target as integer, byval attacker as integer, byval spread as bool) as bool
DECLARE FUNCTION item_can_be_discarded (byval item_id as integer) as bool
DECLARE FUNCTION item_can_be_used_bits (byval item_id as integer) as integer
DECLARE SUB inventory_autosort()
DECLARE FUNCTION touch_virtual_keyboard (default_str as string, max_length as integer=-1, prompt as string="") as string
DECLARE FUNCTION hero_uses_lmp (byval hero_slot as integer) as bool
DECLARE FUNCTION is_shop_empty(byval shop_id as integer, byval shoptype as integer) as bool

DECLARE FUNCTION game_check_use_key() as bool
DECLARE FUNCTION game_battle_check_use_key() as bool
DECLARE FUNCTION game_check_cancel_key() as bool
DECLARE FUNCTION game_check_menu_key() as bool

#endif
