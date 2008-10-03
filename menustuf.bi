#ifndef MENUSTUF_BI
#define MENUSTUF_BI
DECLARE SUB buystuff (id, shoptype, storebuf(), stat())
DECLARE SUB setshopstock (id, recordsize, storebuf(), stufbuf())
DECLARE SUB loadtrades(index, tradestf(), b(), recordsize)
DECLARE FUNCTION chkOOBtarg (target AS INTEGER, atk AS INTEGER, stat() AS INTEGER) AS INTEGER
DECLARE SUB doequip (toequip, who, where, defwep, stat())
DECLARE SUB equip (pt, stat())
DECLARE SUB getitem (getit, num)
DECLARE FUNCTION getOOBtarg (search_direction AS INTEGER, BYREF target AS INTEGER, atk AS INTEGER, stat() AS INTEGER, recheck AS INTEGER=NO) AS INTEGER
DECLARE SUB itemmenuswap (invent() AS InventSlot, atkIDs() AS INTEGER, iuse(), permask(), i, o)
DECLARE FUNCTION items (stat())
DECLARE SUB itstr (i)
DECLARE SUB oobcure (w, t, atk, spred, stat())
DECLARE SUB patcharray (array(), n$)
DECLARE FUNCTION picksave (loading)
DECLARE SUB sellstuff (id, storebuf(), stat())
DECLARE SUB spells (pt, stat())
DECLARE SUB status (pt, stat())
DECLARE FUNCTION trylearn (who, atk, learntype)
DECLARE SUB unequip (who, where, defwep, stat(), resetdw)
DECLARE SUB loadshopstuf (array(), id)
DECLARE FUNCTION count_available_spells(who AS INTEGER, list AS INTEGER) AS INTEGER
DECLARE FUNCTION outside_battle_cure (atk AS INTEGER, target AS INTEGER, attacker AS INTEGER, stat() AS INTEGER, spread AS INTEGER) AS INTEGER
#endif