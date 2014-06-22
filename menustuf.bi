#ifndef MENUSTUF_BI
#define MENUSTUF_BI
DECLARE SUB buystuff (byval shop_id as integer, byval shoptype as integer, storebuf() as integer)
DECLARE FUNCTION chkOOBtarg (byval target as integer, byval atk as integer) as integer
DECLARE SUB update_hero_max_and_cur_stats (byval who as integer)
DECLARE SUB doequip (byval toequip as integer, byval who as integer, byval where as integer, byval defwep as integer)
DECLARE SUB equip (byval who as integer)
DECLARE FUNCTION getOOBtarg (byval search_direction as integer, byref target as integer, byval atk as integer, byval recheck as integer=NO) as integer
DECLARE SUB itemmenuswap (invent() as InventSlot, iuse() as integer, permask() as integer, byval it1 as integer, byval it2 as integer)
DECLARE FUNCTION item_screen () as integer
DECLARE FUNCTION old_items_menu () as integer
DECLARE FUNCTION use_item_by_id(byval item_id as integer, byref trigger_box as integer, name_override as string="") as integer
DECLARE FUNCTION use_item_in_slot(byval slot as integer, byref trigger_box as integer, byref consumed as bool=NO) as integer
DECLARE SUB update_inventory_caption (byval i as integer)
DECLARE SUB oobcure (byval attacker as integer, byval target as integer, byval atk as integer, byval target_count as integer)
DECLARE SUB patcharray (array() as integer, n as string)
DECLARE FUNCTION picksave (byval loading as integer, byval newgame_opt as bool = YES, byval beep_if_no_saves as bool = NO) as integer
DECLARE SUB sellstuff (byval id as integer, storebuf() as integer)
DECLARE SUB spells_menu (byval who as integer)
DECLARE SUB status_screen (byval slot as integer)
DECLARE SUB old_status (byval pt as integer)
DECLARE FUNCTION trylearn (byval who as integer, byval atk as integer) as bool
DECLARE SUB unequip (byval who as integer, byval where as integer, byval defwep as integer, byval resetdw as integer)
DECLARE SUB loadshopstuf (array() as integer, byval id as integer)
DECLARE FUNCTION count_available_spells(byval who as integer, byval list as integer) as integer
DECLARE FUNCTION outside_battle_cure (byval atk as integer, byref target as integer, byval attacker as integer, byval spread as integer) as integer

CONST plankNORMAL = 0
CONST plankSEL = 1
CONST plankDISABLE = 2
CONST plankSELDISABLE = 3
CONST plankSPECIAL = 4
CONST plankSELSPECIAL = 5
CONST plankITEMSEL = 6
CONST plankITEMSELDISABLE = 7
CONST plankITEMSELSPECIAL = 8

DECLARE SUB plank_menu_clear (byval sl as Slice Ptr, byval lookup as integer)
DECLARE FUNCTION plank_menu_append OVERLOAD (byval sl as slice ptr, byval lookup as integer, byval collection_kind as integer, byval callback as ANY ptr=0, byval arg0 as integer=0, byval arg1 as integer=0, byval arg2 as integer=0) as Slice Ptr
DECLARE FUNCTION plank_menu_append OVERLOAD (byval sl as slice ptr, byval lookup as integer, byval collection as Slice Ptr, byval callback as ANY ptr=0, byval arg0 as integer=0, byval arg1 as integer=0, byval arg2 as integer=0) as Slice Ptr
DECLARE SUB set_plank_state (byval sl as Slice Ptr, byval state as integer=plankNORMAL)

#endif
