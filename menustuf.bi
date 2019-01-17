#ifndef MENUSTUF_BI
#define MENUSTUF_BI
DECLARE SUB buystuff (byval shop_id as integer, byval shoptype as integer, storebuf() as integer)
DECLARE FUNCTION chkOOBtarg (byval target as integer, byval atk as integer) as bool
DECLARE SUB update_hero_max_and_cur_stats (byval who as integer)
DECLARE SUB doequip (toequip as integer, who as integer, where as integer)
DECLARE SUB unequip (who as integer, where as integer, resetdefwep as bool = YES)
DECLARE SUB equip_menu (who as integer, allow_switch as bool = YES)
DECLARE FUNCTION getOOBtarg (byval search_direction as integer, byref target as integer, byval atk as integer, byval recheck as bool=NO) as bool
DECLARE SUB itemmenuswap (invent() as InventSlot, iuse() as integer, permask() as integer, byval it1 as integer, byval it2 as integer)
DECLARE FUNCTION item_screen () as integer
DECLARE FUNCTION use_item_by_id(byval item_id as integer, byref trigger_box as integer, name_override as string="") as integer
DECLARE FUNCTION use_item_in_slot(byval slot as integer, byref trigger_box as integer, byref consumed as bool=NO) as integer
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
DECLARE FUNCTION outside_battle_cure (byval atk as integer, byref target as integer, byval attacker as integer, byval spread as bool) as bool
DECLARE FUNCTION item_can_be_discarded (byval item_id as integer) as bool
DECLARE FUNCTION item_can_be_used_bits (byval item_id as integer) as integer
DECLARE SUB inventory_autosort()
DECLARE FUNCTION touch_virtual_keyboard (default_str as string, max_length as integer=-1, prompt as string="") as string

#endif
