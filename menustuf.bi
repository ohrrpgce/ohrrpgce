#ifndef MENUSTUF_BI
#define MENUSTUF_BI
DECLARE SUB buystuff (id as integer, shoptype as integer, storebuf() as integer, stat() as integer)
DECLARE FUNCTION chkOOBtarg (target AS INTEGER, atk AS INTEGER, stat() AS INTEGER) AS INTEGER
DECLARE SUB doequip (toequip as integer, who as integer, where as integer, defwep as integer, stat() as integer)
DECLARE SUB equip (pt as integer, stat() as integer)
DECLARE SUB getitem (getit as integer, num as integer)
DECLARE FUNCTION getOOBtarg (search_direction AS INTEGER, BYREF target AS INTEGER, atk AS INTEGER, stat() AS INTEGER, recheck AS INTEGER=NO) AS INTEGER
DECLARE SUB itemmenuswap (invent() AS InventSlot, iuse() as integer, permask() as integer, i as integer, o as integer)
DECLARE FUNCTION items_menu (stat() as integer) as integer
DECLARE SUB update_inventory_caption (i as integer)
DECLARE SUB oobcure (w as integer, t as integer, atk as integer, spred as integer, stat() as integer)
DECLARE SUB patcharray (array() as integer, n as string)
DECLARE FUNCTION picksave (loading as integer) as integer
DECLARE SUB sellstuff (id as integer, storebuf() as integer, stat() as integer)
DECLARE SUB spells_menu (who as integer)
DECLARE SUB status (pt as integer, stat() as integer)
DECLARE FUNCTION trylearn (who as integer, atk as integer, learntype as integer) as integer
DECLARE SUB unequip (who as integer, where as integer, defwep as integer, stat() as integer, resetdw as integer)
DECLARE SUB loadshopstuf (array() as integer, byval id as integer)
DECLARE FUNCTION count_available_spells(who AS INTEGER, list AS INTEGER) AS INTEGER
DECLARE FUNCTION outside_battle_cure (atk AS INTEGER, target AS INTEGER, attacker AS INTEGER, stat() AS INTEGER, spread AS INTEGER) AS INTEGER
#endif
