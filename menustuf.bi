#ifndef MENUSTUF_BI
#define MENUSTUF_BI
DECLARE SUB buystuff (byval id as integer, byval shoptype as integer, storebuf() as integer)
DECLARE FUNCTION chkOOBtarg (target as integer, atk as integer) as integer
DECLARE SUB doequip (byval toequip as integer, byval who as integer, byval where as integer, byval defwep as integer)
DECLARE SUB equip (byval who as integer)
DECLARE SUB getitem (byval getit as integer, byval num as integer)
DECLARE FUNCTION getOOBtarg (search_direction as integer, byref target as integer, atk as integer, recheck as integer=NO) as integer
DECLARE SUB itemmenuswap (invent() as InventSlot, iuse() as integer, permask() as integer, byval it1 as integer, byval it2 as integer)
DECLARE FUNCTION items_menu () as integer
DECLARE FUNCTION use_item_by_id(byval item_id as integer, byref trigger_box as integer, name_override as STRING="") as integer
DECLARE FUNCTION use_item_in_slot(byval slot as integer, byref trigger_box as integer, byref consumed as integer) as integer
DECLARE SUB update_inventory_caption (byval i as integer)
DECLARE SUB oobcure (byval w as integer, byval t as integer, byval atk as integer, byval spred as integer)
DECLARE SUB patcharray (array() as integer, n as string)
DECLARE FUNCTION picksave (byval loading as integer) as integer
DECLARE SUB sellstuff (byval id as integer, storebuf() as integer)
DECLARE SUB spells_menu (who as integer)
DECLARE SUB status (byval pt as integer)
DECLARE FUNCTION trylearn (who as integer, atk as integer, learntype as integer) as integer
DECLARE SUB unequip (byval who as integer, byval where as integer, byval defwep as integer, byval resetdw as integer)
DECLARE SUB loadshopstuf (array() as integer, byval id as integer)
DECLARE FUNCTION count_available_spells(who as integer, list as integer) as integer
DECLARE FUNCTION outside_battle_cure (atk as integer, target as integer, attacker as integer, spread as integer) as integer
#endif
