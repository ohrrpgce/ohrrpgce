'OHRRPGCE GAME - Mostly user-interface related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi" 
#include "loading.bi"
#include "gglobals.bi"
#include "const.bi"
#include "uiconst.bi"
#include "game_udts.bi"

#include "game.bi"
#include "yetmore.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "bmodsubs.bi"
#include "bmod.bi"

'--SUBs and FUNCTIONS only used locally
DECLARE SUB loadtrades(index as integer, tradestf() as integer, b() as integer, recordsize as integer)
DECLARE SUB setshopstock (id as integer, recordsize as integer, storebuf() as integer, stufbuf() as integer)
DECLARE SUB equip_menu_setup (BYREF st AS EquipMenuState, menu$())
DECLARE SUB equip_menu_do_equip(BYVAL item AS INTEGER, BYREF st AS EquipMenuState, menu$())
DECLARE SUB equip_menu_back_to_menu(BYREF st AS EquipMenuState, menu$())
DECLARE SUB equip_menu_stat_bonus(BYREF st AS EquipMenuState)
DECLARE SUB items_menu_infostr(state AS ItemsMenuState, permask() AS INTEGER)


REM $STATIC


SUB buystuff (id, shoptype, storebuf(), stat())
DIM b(dimbinsize(binSTF) * 50), stuf$(50), vmask(5), emask(5), buytype$(5, 1), wbuf(100), walks(15), tradestf(3, 1)
DIM is_equipable AS INTEGER
DIM itembuf(99) AS INTEGER
DIM hiresprite AS Frame PTR
DIM hirepal AS Palette16 PTR
DIM herosprite(3) AS Frame PTR
DIM heropal(3) AS Palette16 PTR
DIM heroframe AS INTEGER
DIM heropos AS XYPair
recordsize = curbinsize(binSTF) / 2 ' get size in INTs

'--Preserve background for display beneath the buy menu
holdscreen = allocatepage
copypage vpage, holdscreen

buytype$(0, 0) = readglobalstring$(85, "Trade for", 20) + " "
buytype$(0, 1) = readglobalstring$(87, "Joins for", 20) + " "
buytype$(1, 0) = readglobalstring$(89, "Cannot Afford", 20) + " "
buytype$(1, 1) = readglobalstring$(91, "Cannot Hire", 20) + " "
wepslot$ = readglobalstring$(38, "Weapon", 10)
purchased$ = readglobalstring$(93, "Purchased", 20)
joined$ = readglobalstring$(95, "Joined!", 20)
instock$ = readglobalstring$(97, "in stock", 20)
anda$ = readglobalstring$(81, "and a", 10)
andsome$ = readglobalstring$(153, "and", 10)
eqprefix$ = readglobalstring$(99, "Equip:", 10)
noroom$ = readglobalstring$(100, "No Room in Party", 20)

FOR i = 0 TO 3
 herosprite(i) = sprite_load(0, stat(i, 0, 14))
 IF herosprite(i) = 0 THEN debug "Couldn't load hero sprite: " & game & ".pt0#" & stat(i,0,14)
 heropal(i) = palette16_load(stat(i, 0, 15), 0, stat(i, 0, 14))
 IF heropal(i) = 0 THEN debug "Failed to load palette for hero (#" & i & ")"
NEXT i

FOR i = 0 TO 10 STEP 2
 walks(i) = 1
NEXT i
walks(11) = 2
walks(12) = 2
walks(13) = 3
walks(14) = 3

loadshopstuf b(), id
FOR o = 0 TO storebuf(16)
 stuf$(o) = ""
 FOR i = 1 TO small(b(o * recordsize + 0), 16)
  IF b(o * recordsize + i) >= 0 AND b(o * recordsize + i) < 256 THEN stuf$(o) = stuf$(o) + CHR$(b(o * recordsize + i))
 NEXT i
NEXT o

total = 0
setshopstock id, recordsize, storebuf(), b()
GOSUB stufmask
IF total = 0 THEN GOTO cleanupquit

price$ = "": xtralines = 0: price2$ = ""
info1$ = "": info2$ = ""
eqinfo$ = ""
showhero = 0
tradingitems = 0
pt = 0: top = 0
DO UNTIL readbit(vmask(), 0, pt) = 0
 pt = pt + 1
LOOP
GOSUB curinfo

menusound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 IF tog THEN walk = loopvar(walk, 0, 15, 1)
 playtimer
 control
 IF carray(ccUp) > 1 THEN
  menusound gen(genCursorSFX)
  DO
   pt = pt - 1
   IF pt < 0 THEN
    DO
     pt = pt + 1
    LOOP UNTIL readbit(vmask(), 0, pt) = 0 OR pt > storebuf(16)
    EXIT DO
   END IF
  LOOP UNTIL readbit(vmask(), 0, pt) = 0
  top = small(pt, top)
  GOSUB curinfo
 END IF
 IF carray(ccDown) > 1 THEN
  menusound gen(genCursorSFX)
  DO
   pt = pt + 1
   IF pt > storebuf(16) THEN
    DO
     pt = pt - 1
    LOOP UNTIL readbit(vmask(), 0, pt) = 0 OR pt < 0
    EXIT DO
   END IF
  LOOP UNTIL readbit(vmask(), 0, pt) = 0
  GOSUB curinfo
 END IF
 IF carray(ccMenu) > 1 THEN EXIT DO
 IF carray(ccUse) > 1 THEN '---PRESS ENTER---------------------
  IF readbit(emask(), 0, pt) = 0 THEN '---CHECK TO SEE IF YOU CAN AFFORD IT---
   IF gam.stock(id, pt) > 1 THEN gam.stock(id, pt) = gam.stock(id, pt) - 1
   IF b(pt * recordsize + 22) THEN setbit tag(), 0, ABS(b(pt * recordsize + 22)), SGN(SGN(b(pt * recordsize + 22)) + 1)
   gold = gold - b(pt * recordsize + 24)
   IF tradingitems THEN '---TRADE IN ITEMS----------
    FOR i = 0 TO 3
     IF tradestf(i, 0) > -1 THEN
      delitem tradestf(i, 0) + 1, tradestf(i, 1)
     END IF
    NEXT
   END IF '-------END TRADE IN ITEM----------------------------
   IF b(pt * recordsize + 17) = 0 THEN '---BUY ITEM-------------------
    menusound gen(genBuySFX)
    getitem b(pt * recordsize + 18) + 1, 1
    acol = 4
    alert = 10
    alert$ = purchased$ + " " + stuf$(pt)
   END IF '-------END IF ITEM-------------------------------------
   IF b(pt * recordsize + 17) = 1 THEN '---HIRE HERO------------------
    menusound gen(genHireSFX)
    FOR i = 37 TO 0 STEP -1
     IF hero(i) = 0 THEN slot = i
    NEXT i
    addhero b(pt * recordsize + 18) + 1, slot, stat(), b(pt * recordsize + 26)
    acol = 4
    alert = 10
    alert$ = stuf$(pt) + " " + joined$
   END IF '-------END IF HERO-------------------------------------
   'the last thing to do is re-eval the item and hero tags in case
   'stuff changed
   evalherotag stat()
   evalitemtag
  ELSE ' WHEN CANNOT AFFORD------------------------------------
   menusound gen(genCantBuySFX)
   acol = 3
   alert = 10
   alert$ = buytype$(1, shoptype) + stuf$(pt)
  END IF '--------END BUY THING------------
  GOSUB stufmask
  DO WHILE readbit(vmask(), 0, pt) = 1
   pt = pt - 1
   IF pt < 0 THEN
    pt = 0
    DO WHILE readbit(vmask(), 0, pt) = 1
     pt = pt + 1
     IF pt > storebuf(16) THEN GOTO cleanupquit
    LOOP
    EXIT DO
   END IF
  LOOP
  GOSUB curinfo
 END IF '---------END TRY BUY THING--------

 centerbox 80, 94, 150, 168, 1, dpage
 centerbox 240, 94, 150, 168, 1, dpage
 '-----RIGHT PANEL------------------------------------------
 temp$ = gold & " " & readglobalstring(32, "Money")
 centerbox 240, 19, LEN(temp$) * 8 + 8, 14, 4, dpage
 edgeprint temp$, xstring(temp$, 240), 14, uilook(uiText), dpage
 o = 0
 edgeprint stuf$(pt), xstring(stuf$(pt), 240), 30 + o * 10, uilook(uiMenuItem), dpage: o = o + 1
 IF info1$ <> "" THEN edgeprint info1$, xstring(info1$, 240), 30 + o * 10, uilook(uiDisabledItem), dpage: o = o + 1
 IF info2$ <> "" THEN edgeprint info2$, xstring(info2$, 240), 30 + o * 10, uilook(uiDisabledItem), dpage: o = o + 1
 IF eqinfo$ <> "" THEN edgeprint eqinfo$, xstring(eqinfo$, 240), 30 + o * 10, uilook(uiMenuItem), dpage: o = o + 1
 IF gam.stock(id, pt) > 1 THEN
  edgeprint (gam.stock(id, pt) - 1) & " " & instock$ & " ", xstring((gam.stock(id, pt) - 1) & " " & instock$ & " ", 240), 30 + o * 10, uilook(uiMenuItem), dpage: o = o + 1
 END IF
 IF showhero > -1 THEN
  'This happens only if a hireable hero is selected
  centerbox 240, 130, 36, 44, 4, dpage
  sprite_draw(hiresprite + walks(walk), hirepal, 224, 110, 1, -1, dpage)
 END IF
 IF is_equipable THEN
  FOR i = 0 TO 3
   heropos.x = 170 + i * 36
   heropos.y = 130
   heroframe = 0
   col = 0
   IF hero(i) > 0 THEN
    'If there is a hero in this slot
    IF readbit(itembuf(), 66, hero(i) - 1) <> 0 THEN
     '-- animation heroes when this item is equipable
     heroframe = walks(walk)
     col = 3
    END IF
   END IF
   edgeboxstyle heropos.x - 1, heropos.y - 2, 34, 44, col, dpage, , YES
   IF hero(i) > 0 THEN
    'If there is a hero in this slot
    sprite_draw(herosprite(i) + heroframe, heropal(i), heropos.x, heropos.y, 1, -1, dpage)
   END IF
  NEXT i
 END IF
 '-----LEFT PANEL-------------------------------------------
 o = 0
 FOR i = top TO storebuf(16)
  IF readbit(vmask(), 0, i) = 0 THEN
   c = uilook(uiMenuItem): IF pt = i THEN c = uilook(uiSelectedItem + tog)
   IF readbit(emask(), 0, i) THEN c = uilook(uiDisabledItem): IF pt = i THEN c = uilook(uiMenuItem + tog)
   edgeprint stuf$(i), 10, 15 + o * 10, c, dpage
   o = o + 1
   IF o > 14 THEN
    IF pt > i THEN
     DO
      top = top + 1
     LOOP UNTIL readbit(vmask(), 0, top) = 0
    END IF
    EXIT FOR
   END IF
  END IF
 NEXT i
 IF price$ <> "" THEN
  centerbox 160, 187, LEN(price$) * 8 + 8, 14 + xtralines * 10, 1, dpage
  edgeprint price$, xstring(price$, 160), 182 - xtralines * 5, uilook(uiText), dpage
  IF xtralines >= 1 THEN edgeprint price2$, xstring(price2$, 160), 187, uilook(uiText), dpage
 END IF
 IF alert THEN
  alert = alert - 1
  centerbox 160, 178, LEN(alert$) * 8 + 8, 14, acol, dpage
  edgeprint alert$, xstring(alert$, 160), 173, uilook(uiSelectedItem + tog), dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 dowait
LOOP

cleanupquit:
'Unload the sprites used to display the heroes
FOR i = 0 TO 3
 sprite_unload(@herosprite(i))
 palette16_unload(@heropal(i))
NEXT i
sprite_unload(@hiresprite)
palette16_unload(@hirepal)
freepage holdscreen
menusound gen(genCancelSFX)
vishero stat()
EXIT SUB

stufmask:
total = 0
FOR i = 0 TO 5
 vmask(i) = 0
 emask(i) = 0
NEXT i
eslot = 4
FOR i = 0 TO 3
 eslot = eslot - SGN(hero(i))
NEXT i
FOR i = 0 TO storebuf(16)
 '--for each shop-thing
 IF gam.stock(id, i) = 1 THEN setbit vmask(), 0, i, 1
 IF b(i * recordsize + 17) = (shoptype XOR 1) THEN setbit vmask(), 0, i, 1
 IF NOT istag(b(i * recordsize + 20), -1) THEN setbit vmask(), 0, i, 1
 IF b(i * recordsize + 24) > gold THEN setbit emask(), 0, i, 1
 loadtrades i, tradestf(), b(), recordsize
 FOR j = 0 TO 3
  IF tradestf(j, 0) > -1 THEN
   IF countitem(tradestf(j, 0) + 1) < tradestf(j, 1) THEN setbit emask(), 0, i, 1
  END IF
 NEXT
 '---PREVENT PARTY OVERFLOW
 IF b(i * recordsize + 17) = 1 AND eslot = 0 THEN setbit emask(), 0, i, 1
 IF readbit(vmask(), 0, i) = 0 THEN total = total + 1
NEXT i
RETRACE

curinfo:
tradingitems = 0
xtralines = 0
showhero = -1
is_equipable = NO
price$ = ""
price2$ = ""
eqinfo$ = ""
info1$ = ""
info2$ = ""
IF b(pt * recordsize + 24) > 0 THEN price$ = b(pt * recordsize + 24) & " " & readglobalstring(32, "Money")
'--load must trade in item types+amounts
loadtrades pt, tradestf(), b(), recordsize
FOR i = 0 TO 3
 IF tradestf(i, 0) > -1 THEN
  tradingitems = 1
  IF price$ = "" THEN
   price$ = buytype$(0, shoptype)
  ELSE
   IF tradestf(i, 1) = 1 THEN
    price$ = price$ + " " + anda$ + " "
   ELSE
    price$ = price$ + " " + andsome$ + " "
   END IF
  END IF
  IF tradestf(i, 1) = 1 THEN
   price$ = price$ + readitemname$(tradestf(i, 0))
  ELSE
   price$ = price$ + STR$(tradestf(i, 1)) + " " + readitemname$(tradestf(i, 0))
  END IF
 END IF
NEXT
IF LEN(price$) > 38 THEN
 '--have to split in 2! ARGH
 i = 38
 WHILE i > 19 AND MID$(price$, i, 1) <> " ": i = i - 1: WEND
 price2$ = MID$(price$, i + 1)
 price$ = LEFT$(price$, i - 1)
 xtralines = 1
END IF
IF b(pt * recordsize + 17) = 0 THEN
 'This is an item
 loaditemdata itembuf(), b(pt * recordsize + 18)
 'The itembuf remains and is used later to show equipability.
 IF itembuf(49) = 1 THEN eqinfo$ = eqprefix$ & " " & wepslot$
 IF itembuf(49) > 1 THEN eqinfo$ = eqprefix$ & " " & readglobalstring(23 + itembuf(49), "Armor" & itembuf(49)-1)
 info1$ = readbadbinstring$(itembuf(), 9, 35, 0)
 IF LEN(info1$) > 17 THEN
  FOR o = 18 TO 1 STEP -1
   IF MID$(info1$, o, 1) = " " OR MID$(info1$, o, 1) = "-" OR MID$(info1$, o, 1) = "," OR MID$(info1$, o, 1) = "." THEN EXIT FOR
  NEXT o
  IF o > 1 THEN
   info2$ = RIGHT$(info1$, LEN(info1$) - o)
   info1$ = LEFT$(info1$, o)
  END IF
  IF RIGHT$(info1$, 1) = " " THEN info1$ = LEFT$(info1$, LEN(info1$) - 1)
  info1$ = LEFT$(info1$, 18)
 END IF
 IF itembuf(49) > 0 THEN
  'This item is equippable
  is_equipable = YES
 END IF
END IF
IF b(pt * recordsize + 17) = 1 THEN
 'hire
 dim her as herodef
 loadherodata @her, b(pt * recordsize + 18)
 loaditemdata wbuf(), her.def_weapon
 IF her.def_level < 0 THEN her.def_level = averagelev(stat())
 eqinfo$ = (atlevel(her.def_level, her.lev0.hp, her.lev99.hp) + wbuf(54 + 0)) & " " & statnames(statHP)
 showhero = her.sprite
 
 'Load the sprite for the hireable hero
 sprite_unload @hiresprite
 hiresprite = sprite_load(0, showhero)
 IF hiresprite = 0 THEN debug "Couldn't load hero sprite: " & game & ".pt0#" & showhero
 palette16_unload @hirepal
 hirepal = palette16_load(her.sprite_pal, 0, showhero)
 IF hirepal = 0 THEN debug "Failed to load palette for hireable hero (#" & her.sprite_pal & ")"

 IF eslot = 0 THEN info1$ = noroom$
END IF
RETRACE
END SUB

SUB setshopstock (id, recordsize, storebuf(), stufbuf())
DIM i AS INTEGER
FOR i = 0 TO storebuf(16)
 '--for each shop-stuff
 IF gam.stock(id, i) = 0 THEN
  '--if unloaded, reload stock
  gam.stock(id, i) = stufbuf(i * recordsize + 19)
  '--zero means unloaded, 1 means no-stock, 2+n means 1+n in stock
  IF gam.stock(id, i) > -1 THEN gam.stock(id, i) = gam.stock(id, i) + 1
 END IF
NEXT i
END SUB

SUB loadtrades(index, tradestf(), b(), recordsize)
tradestf(0, 0) = b(index * recordsize + 25) - 1
tradestf(0, 1) = b(index * recordsize + 30) + 1
FOR i = 1 TO 3
 tradestf(i, 0) = b(index * recordsize + i * 2 + 29) - 1
 tradestf(i, 1) = b(index * recordsize + i * 2 + 30) + 1
NEXT i
END SUB

FUNCTION chkOOBtarg (target AS INTEGER, atk AS INTEGER, stat() AS INTEGER) AS INTEGER
'true if valid, false if not valid
 IF target < 0 OR target > 40 THEN RETURN NO
 IF hero(target) = 0 THEN RETURN NO
 IF atk < -1 OR atk > gen(genMaxAttack) THEN RETURN NO

 DIM hp AS INTEGER
 hp = stat(target, 0, statHP)

 IF atk >= 0 THEN
  DIM atktemp(40 + dimbinsize(binATTACK)) AS INTEGER
  loadattackdata atktemp(), atk
  IF hp = 0 AND (atktemp(3) = 4 OR atktemp(3) = 10) THEN RETURN YES
  IF hp > 0 AND atktemp(3) = 10 THEN RETURN NO
 END IF

 IF hp = 0 THEN RETURN NO

 RETURN YES
END FUNCTION

SUB doequip (toequip, who, where, defwep, stat())

'--load the item data for this equipment
loaditemdata buffer(), toequip -1

'--apply the stat bonuses
FOR i = 0 TO 11
 'stat bonuses
 stat(who, 1, i) = stat(who, 1, i) + buffer(54 + i)
 IF i > 1 THEN stat(who, 0, i) = stat(who, 1, i)
 stat(who, 0, i) = small(stat(who, 0, i), stat(who, 1, i))
 IF gen(genStatCap + i) > 0 THEN
 	stat(who, 0, i) = small(stat(who, 0, i),gen(genStatCap + i))
 	stat(who, 1, i) = small(stat(who, 1, i),gen(genStatCap + i))
 END IF
NEXT i

'--special handling for weapons
IF where = 0 THEN
 stat(who, 0, 13) = buffer(52) 'remember weapon pic
 stat(who, 1, 13) = buffer(53) 'remember weapon pal
 bmenu(who, 0) = large(buffer(48), 1) 'put weapon attack in battle menu
END IF

'--set equipment
eqstuf(who, where) = toequip

'--equipping the default weapon does not delete it from inventory
IF toequip = defwep AND where = 0 THEN
ELSE
 '--delete the item from inventory
 delitem toequip, 1
END IF

END SUB

SUB getitem (getit, num)

numitems = num

FOR i = 0 TO last_inv_slot()
 ' Loop through all inventory slots looking for a slot that already
 ' contains the item we are adding. If found increment that slot
 room = 99 - inventory(i).num
 IF inventory(i).used AND getit - 1 = inventory(i).id AND room > 0 THEN
  IF room < numitems THEN
   inventory(i).num = 99
   update_inventory_caption i
   numitems -= room
  ELSE
   inventory(i).num += numitems
   update_inventory_caption i
   EXIT SUB
  END IF
 END IF
NEXT
FOR i = 0 TO last_inv_slot()
 'loop through each inventory slot looking for an empty slot to populate 
 IF inventory(i).used = 0 THEN
  inventory(i).used = -1
  inventory(i).id = getit - 1
  inventory(i).num = small(numitems, 99)
  numitems -= inventory(i).num
  update_inventory_caption i
  IF numitems = 0 THEN EXIT SUB
 END IF
NEXT
END SUB

FUNCTION getOOBtarg (search_direction AS INTEGER, BYREF target AS INTEGER, atk AS INTEGER, stat() AS INTEGER, recheck AS INTEGER=NO) AS INTEGER
 '--return true on success, false on failure
 IF recheck THEN target -= 1 ' For a re-check, back the cursor up so if the current target is still valid, it won't change
 DIM safety AS INTEGER = 0
 DO
  target = loopvar(target, 0, 3, search_direction)
  IF chkOOBtarg(target, atk, stat()) THEN RETURN YES
  safety += 1
  IF safety >= 4 THEN EXIT DO
 LOOP
 'Failure
 target = -1
 RETURN NO
END FUNCTION

SUB itemmenuswap (invent() AS InventSlot, atkIDs() AS INTEGER, iuse(), permask(), i, o)
'this sub called from items()
SWAP invent(i), invent(o)
SWAP atkIDs(i), atkIDs(o)

t1 = readbit(iuse(), 0, 3 + i)
t2 = readbit(iuse(), 0, 3 + o)
setbit iuse(), 0, 3 + i, t2
setbit iuse(), 0, 3 + o, t1
t1 = readbit(permask(), 0, 3 + i)
t2 = readbit(permask(), 0, 3 + o)
setbit permask(), 0, 3 + i, t2
setbit permask(), 0, 3 + o, t1
END SUB

FUNCTION items_menu (stat() as integer) as integer
DIM istate AS ItemsMenuState
DIM itemdata(100) AS INTEGER
DIM itemtemp(100) AS INTEGER
DIM atktemp(40 + dimbinsize(binATTACK)) AS INTEGER
DIM learn_attack AS AttackData 'FIXME: used only when learning an attack, move to a sub when that code is subified
DIM iuse((inventoryMax + 3) / 16) 'bit 0 of iuse, permask, correspond to item -3
DIM permask((inventoryMax + 3) / 16) AS INTEGER
DIM atkIDs(inventoryMax)
DIM special$(-3 TO -1)
DIM autosort_changed AS INTEGER = 0


special$(-3) = rpad(readglobalstring$(35, "DONE", 10), " ", 11)
special$(-2) = rpad(readglobalstring$(36, "AUTOSORT", 10), " ", 11)
special$(-1) = rpad(readglobalstring$(37, "TRASH", 10), " ", 11)

REMEMBERSTATE

'--Preserve background for display beneath the item menu
holdscreen = allocatepage
copypage vpage, holdscreen

FOR i = 0 TO 2
 setbit iuse(), 0, i, 1
NEXT i
FOR i = 0 TO last_inv_slot()
 atkIDs(i) = -1
 IF inventory(i).used THEN
  loaditemdata itemtemp(), inventory(i).id
  IF itemtemp(73) = 2 THEN setbit permask(), 0, 3 + i, 1
  IF itemtemp(50) > 0 THEN '--teach spell
   setbit iuse(), 0, 3 + i, 1
  ELSEIF itemtemp(51) > 0 THEN
   setbit iuse(), 0, 3 + i, 1
   atkIDs(i) = itemtemp(51) - 1
  ELSEIF itemtemp(51) < 0 THEN
   setbit iuse(), 0, 3 + i, 1
  END IF
 END IF
NEXT i
istate.cursor = -3
top = -3
istate.sel = -4
wptr = 0
spred = 0
pick = 0
istate.info = ""

DIM rect AS RectType
WITH rect
 .x = 8
 .y = 5
 .wide = 304
 .high = small(180, 12 + (INT((last_inv_slot() + 1) / 3) + 1) * 8)
END WITH
DIM scrollrect AS RectType
WITH scrollrect
 .x = 20
 .y = 12
 .wide = 287
 .high = 168
END WITH
DIM state AS MenuState
WITH state
 .first = -1
 .last = INT(last_inv_slot() / 3)
 .size = 20
END WITH

items_menu_infostr istate, permask()
setkeys
quit = 0
wtogl = 0
menusound gen(genAcceptSFX)

DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 wtogl = loopvar(wtogl, 0, 3, 1)
 playtimer
 control
 GOSUB itcontrol
 IF quit THEN EXIT DO
 edgeboxstyle rect.x, rect.y, rect.wide, rect.high, 0, dpage
 FOR i = top TO small(top + 62, last_inv_slot())
  textcolor uilook(uiDisabledItem), 0
  IF readbit(iuse(), 0, 3 + i) = 1 THEN textcolor uilook(uiMenuItem), 0
  IF readbit(permask(), 0, 3 + i) THEN textcolor uilook(uiSelectedDisabled), 0
  IF istate.cursor = i THEN
   textcolor uilook(uiMenuItem), uilook(uiHighlight2)
   IF readbit(iuse(), 0, 3 + i) = 1 THEN textcolor uilook(uiText), uilook(uiHighlight2)
   IF readbit(permask(), 0, 3 + i) THEN textcolor uilook(uiGold), uilook(uiHighlight2)
  END IF
  IF istate.sel = i THEN
   textcolor uilook(uiMenuItem), uilook(uiHighlight + tog)
   IF istate.cursor = i THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight + tog)
  END IF
  IF i >= 0 THEN
   display$ = inventory(i).text
  ELSE
   display$ = special$(i)
  END IF
  printstr display$, 20 + 96 * ((i + 3) MOD 3), 12 + 8 * ((i - top) \ 3), dpage
 NEXT i
 centerfuz 160, 192, 312, 16, 4, dpage
 edgeprint istate.info, xstring(istate.info, 160), 187, uilook(uiText), dpage
 IF pick = 1 THEN
  centerbox 160, 47, 160, 88, 2, dpage
  IF spred = 0 AND wptr >= 0 THEN
   rectangle 84, 8 + wptr * 20, 152, 20, uilook(uiHighlight2), dpage
  ELSEIF spred <> 0 THEN
   rectangle 84, 8, 152, 80, uilook(uiHighlight2 * tog), dpage
  END IF
  o = 0
  FOR i = 0 TO 3
   IF hero(i) > 0 THEN
    wt = 0: IF wptr = i THEN wt = INT(wtogl / 2)
    sprite_draw herow(o).sprite + (2 * 2) + wt, herow(o).pal, 89, 8 + i * 20, 1, -1, dpage
    col = uilook(uiMenuItem): IF i = wptr THEN col = uilook(uiSelectedItem + tog)
    tstat = atktemp(18)
    IF tstat = 0 or tstat = 1 THEN
     temp$ = STR$(ABS(stat(i, 0, tstat))) & "/" & STR$(ABS(stat(i, 1, tstat)))
    ELSE
     temp$ = STR$(ABS(stat(i, 0, tstat)))
    END IF
    edgeprint temp$, 119, 16 + i * 20, col, dpage
    o = o + 1
   END IF
  NEXT i
 END IF
 WITH state
  state.top = INT(top / 3)
  state.pt = INT(istate.cursor / 3)
 END WITH
 draw_scrollbar state, scrollrect, , dpage
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 dowait
LOOP
menusound gen(genCancelSFX)
FOR t = 4 TO 5: carray(t) = 0: NEXT t
freepage holdscreen
EXIT FUNCTION

itcontrol:
'--keyboard checking and associated actions for the item menu
IF pick = 0 THEN '--have not picked an item yet
 IF carray(ccMenu) > 1 THEN
  '--deselect currently selected item
  IF istate.sel > -1 THEN
   istate.sel = -4
   menusound gen(genCancelSFX)
  ELSE
   quit = 1
  END IF
 END IF
 IF carray(ccUse) > 1 THEN
  '--exit
  IF istate.cursor = -3 THEN quit = 1
  '--sort
  IF istate.cursor = -2 THEN GOSUB autosort
  IF istate.cursor = -1 AND istate.sel >= 0 AND readbit(permask(), 0, 3 + istate.sel) = 0 THEN
   '--try to thow item away
   IF inventory(istate.sel).used THEN MenuSound gen(genAcceptSFX)
   inventory(istate.sel).text = SPACE$(11)
   inventory(istate.sel).used = 0
   setbit iuse(), 0, 3 + istate.sel, 0
   istate.sel = -4
   items_menu_infostr istate, permask()
   RETRACE
  END IF
  IF istate.sel >= 0 THEN
   IF istate.cursor >= 0 AND istate.cursor <> istate.sel THEN
    '--swap the selected item and the item under the cursor
    itemmenuswap inventory(), atkIDs(), iuse(), permask(), istate.cursor, istate.sel
    istate.sel = -4
    items_menu_infostr istate, permask()
    MenuSound gen(genAcceptSFX)
    RETRACE
   END IF
   IF istate.cursor >= 0 AND istate.sel = istate.cursor THEN
    '--try to use the current item
    istate.sel = -4
    '--if the usability bit is off, or you dont have any of the item, exit
    IF readbit(iuse(), 0, 3 + istate.cursor) = 0 OR inventory(istate.cursor).used = 0 THEN RETRACE
    loaditemdata itemdata(), inventory(istate.cursor).id
    IF itemdata(50) > 0 THEN '--learn a spell
     ttype = 0
     pick = 1: wptr = 0
     '--target the first non-dead hero
     WHILE hero(wptr) = 0 OR stat(wptr, 0, statHP) = 0
      wptr = loopvar(wptr, 0, 3, 1)
     WEND
     spred = 0
     MenuSound gen(genAcceptSFX)
     RETRACE
    END IF
    IF itemdata(51) > 0 THEN '--attack/oobcure
     loadattackdata atktemp(), itemdata(51) - 1
     ttype = atktemp(4)
     wptr = -1
     IF getOOBtarg(1, wptr, itemdata(51) - 1, stat()) = NO THEN
      'Failed to get a target
      MenuSound gen(genCancelSFX)
      RETRACE
     END IF
     pick = 1
     spred = 0 ' Default to focused
     IF ttype = 1 THEN ' Spread (but not optional spread) defaults to spread
      FOR i = 0 TO 3
       IF chkOOBtarg(i, wptr, stat()) THEN spred = spred + 1
      NEXT i
     END IF
     MenuSound gen(genAcceptSFX)
     RETRACE
    END IF
    IF itemdata(51) < 0 THEN '--trigger a text box
     IF itemdata(73) = 1 THEN dummy = consumeitem(istate.cursor)
     items_menu = itemdata(51) * -1
     MenuSound gen(genAcceptSFX)
     freepage holdscreen
     RETRIEVESTATE
     EXIT FUNCTION
    END IF
   END IF
  END IF
  IF istate.sel < -3 AND istate.cursor >= 0 THEN
   istate.sel = istate.cursor
   MenuSound gen(genAcceptSFX)
   RETRACE
  END IF
 END IF
 IF carray(ccUp) > 1 AND istate.cursor >= 0 THEN
  menusound gen(genCursorSFX)
  istate.cursor = istate.cursor - 3
  items_menu_infostr istate, permask()
  IF istate.cursor < top THEN top = top - 3
 END IF
 IF carray(ccDown) > 1 AND istate.cursor <= last_inv_slot() - 3 THEN
  menusound gen(genCursorSFX)
  istate.cursor = istate.cursor + 3
  items_menu_infostr istate, permask()
  IF istate.cursor > top + 62 THEN top = top + 3
 END IF
 IF carray(ccLeft) > 1 THEN
  menusound gen(genCursorSFX)
  IF (istate.cursor MOD 3) = 0 THEN
   istate.cursor = istate.cursor + 2
  ELSE
   IF istate.cursor > -3 THEN istate.cursor = istate.cursor - 1
  END IF
  items_menu_infostr istate, permask()
 END IF
 IF carray(ccRight) > 1 THEN
  menusound gen(genCursorSFX)
  IF ((istate.cursor + 3) MOD 3) = 2 THEN ' the +3 adjust for the first negative row
   istate.cursor = istate.cursor - 2
  ELSE
   IF istate.cursor < last_inv_slot() THEN istate.cursor = istate.cursor + 1
  END IF
  items_menu_infostr istate, permask()
 END IF
 IF keyval(scPageUp) > 1 THEN
  istate.cursor -= (state.size+1) * 3
  WHILE istate.cursor < 0: istate.cursor += 3: WEND
  WHILE istate.cursor < top : top = top - 3 : WEND
 END IF
 IF keyval(scPageDown) > 1 THEN
  istate.cursor += (state.size+1) * 3
  WHILE istate.cursor > last_inv_slot(): istate.cursor -= 3: WEND
  WHILE istate.cursor >= top + (state.size+1) * 3 : top = top + 3 : WEND
 END IF
ELSE '--an item is already selected
 IF carray(ccMenu) > 1 THEN
  menusound gen(genCancelSFX)
  pick = 0
  items_menu_infostr istate, permask()
  RETRACE
 END IF
 istate.info = inventory(istate.cursor).text
 IF spred = 0 THEN
  IF carray(ccUp) > 1 THEN
   getOOBtarg -1, wptr, atkIDs(istate.cursor), stat()
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccDown) > 1 THEN
   getOOBtarg 1, wptr, atkIDs(istate.cursor), stat()
   MenuSound gen(genCursorSFX)
  END IF
 END IF
 IF ttype = 2 THEN
  IF carray(ccLeft) > 1 OR carray(ccRight) > 1 THEN
   MenuSound gen(genCursorSFX)
   IF spred = 0 THEN
    FOR i = 0 TO 3
     IF chkOOBtarg(i, atkIDs(istate.cursor), stat()) THEN spred = spred + 1
    NEXT i
   ELSE
    spred = 0
   END IF
  END IF
 END IF
 IF carray(ccUse) > 1 THEN
  'DO ACTUAL EFFECT
  loaditemdata itemtemp(), inventory(istate.cursor).id
  'if can teach a spell
  didlearn = 0
  IF itemtemp(50) > 0 THEN '--teach spell
   atk = itemtemp(50)
   '--trylearn
   didlearn = trylearn(wptr, atk, 0)
   '--announce learn
   IF didlearn = 1 THEN
    menusound gen(genItemLearnSFX)
    loadattackdata learn_attack, atk - 1
    tmp$ = names(wptr) + " " + readglobalstring$(124, "learned", 10) + " " + learn_attack.name
    centerbox 160, 100, small(LEN(tmp$) * 8 + 16, 320), 24, 1, vpage
    edgeprint tmp$, large(xstring(tmp$, 160), 0), 95, uilook(uiText), vpage
    IF learn_attack.learn_sound_effect > 0 THEN playsfx learn_attack.learn_sound_effect - 1
    setvispage vpage
    dummy = getkey
   ELSE
    menusound gen(genCantLearnSFX)
   END IF
  END IF
  '--do (cure) attack outside of battle
  didcure = 0
  IF itemtemp(51) > 0 THEN
   didcure = outside_battle_cure(atkIDs(istate.cursor), wptr, -1, stat(), spred)
  END IF 'itemtemp(51) > 0
  IF itemtemp(73) = 1 AND (didcure OR didlearn = 1) THEN
   IF consumeitem(istate.cursor) THEN
    setbit iuse(), 0, 3 + istate.cursor, 0
    pick = 0
    items_menu_infostr istate, permask()
   END IF
  END IF
 END IF ' SPACE or ENTER
END IF
RETRACE

autosort:
autosort_changed = 0
FOR i = 0 TO last_inv_slot() - 1
 FOR o = i + 1 TO last_inv_slot()
  IF inventory(i).used = 0 AND inventory(o).used THEN
   itemmenuswap inventory(), atkIDs(), iuse(), permask(), i, o
   autosort_changed = -1
   EXIT FOR
  END IF
 NEXT o
NEXT i
FOR i = 0 TO last_inv_slot() - 1
 FOR o = i + 1 TO last_inv_slot()
  IF readbit(iuse(), 0, 3 + i) = 0 AND readbit(iuse(), 0, 3 + o) = 1 THEN
   itemmenuswap inventory(), atkIDs(), iuse(), permask(), i, o
   autosort_changed = -1
   EXIT FOR
  END IF
 NEXT o
NEXT i
IF autosort_changed THEN
 menusound gen(genAcceptSFX)
ELSE
 menusound gen(genCancelSFX)
END IF
RETRACE

END FUNCTION 'gosub gosub

SUB update_inventory_caption (i)
IF inventory(i).used = 0 THEN
 inventory(i).text = SPACE$(11)
ELSE
 inventory(i).text = readitemname$(inventory(i).id)
 inventory(i).text = rpad(inventory(i).text, " ", 8) + CHR$(1) + RIGHT$(XSTR$(inventory(i).num), 2)
END IF
END SUB

SUB oobcure (w, t, atk, spred, stat())

DIM st(13, 1)
dim atksize
atksize = 40 + dimbinsize(binATTACK)
dim attack(atksize)
dim h, h2
'--average stats for item-triggered spells
IF w = -1 THEN
 j = 0
 FOR o = 0 TO 3
  IF hero(o) > 0 THEN
   j = j + 1
   FOR i = 0 TO 13
    st(i, 0) = st(i, 0) + stat(o, 0, i)
    st(i, 1) = st(i, 1) + stat(o, 1, i)
   NEXT i
  END IF
 NEXT o
 FOR i = 0 TO 13
  st(i, 0) = st(i, 0) / j
  st(i, 1) = st(i, 1) / j
 NEXT i
ELSE
 FOR i = 0 TO 13
  st(i, 0) = stat(w, 0, i)
  st(i, 1) = stat(w, 1, i)
 NEXT i
END IF

loadattackdata attack(), atk

targstat = attack(18)

'attack + defense base
a = st(2, 0)
IF attack(58) <> 0 THEN
 d = st(attack(58)-1, 0) 
ELSE
 IF targstat = 6 THEN d = st(7,0) ELSE d = st(4,0)
END IF

IF attack(7) = 1 THEN a = st(6, 0) ': d = st(7, 0)
IF attack(7) = 2 THEN a = st(0, 0)
IF attack(7) = 3 THEN a = (st(0, 1) - st(0, 0))
IF attack(7) = 4 THEN a = INT(RND * 999)
IF attack(7) = 5 THEN a = 100
IF attack(7) >= 6 THEN a = st(attack(7) - 6, 0)

'calc defence
am! = 1: dm! = .5
IF attack(5) = 1 THEN am! = .8: dm! = .1
IF attack(5) = 2 THEN am! = 1.3: dm! = 1
IF attack(5) = 3 THEN am! = 1: dm! = 0

'resetting
IF readbit(attack(), 20, 57) = 1 THEN
 stat(t, 0, targstat) = stat(t, 1, targstat)
END IF

'calc harm
h2 = (a * am!) - (d * dm!)
'no elemental support

'extra damage
h2 = h2 + (h2 / 100) * attack(11)

'randomize
IF readbit(attack(), 20, 61) = 0 THEN h2 = range(h2, 20)

'spread damage
IF readbit(attack(), 20, 1) = 1 THEN h2 = h2 / (spred + 1)

'cap out
IF readbit(attack(), 20, 62) = 0 AND h2 <= 0 THEN h2 = 1

'cure bit
IF readbit(attack(), 20, 0) = 1 THEN h2 = ABS(h2) * -1

'backcompat MP-targstat
IF readbit(attack(), 20, 60) THEN
 IF targstat = 0 THEN targstat = 1
END IF

 chp& = stat(t, 0, targstat)
 mhp& = stat(t, 1, targstat)
 IF readbit(attack(), 65, 5) = 1 THEN
  SELECT CASE attack(5)
   CASE 5'% of max   
    h2 = mhp& + (attack(11) * mhp& / 100)
   CASE 6'% of cur
    h2 = chp& + (attack(11) * chp& / 100)
  END SELECT
 ELSE
  SELECT CASE attack(5) 
   CASE 5'% of max
    h2 = chp& - (mhp& + (attack(11) * mhp& / 100))
   CASE 6'% of cur
    h2 = chp& - (chp& + (attack(11) * chp& / 100))
  END SELECT
 END IF
IF h2 > 32767 THEN h2 = 32767
IF h2 < -32768 THEN h2 = -32768
h = h2

'Inflict the damage
stat(t, 0, targstat) = stat(t, 0, targstat) - h

'Sound effect
MenuSound attack(99)

'bounds
stat(t, 0, targstat) = large(stat(t, 0, targstat), 0)
IF w >= 0 THEN stat(w, 0, targstat) = large(stat(w, 0, targstat), 0)
'bitset 58 allows cure to exceed maximum
IF readbit(attack(), 20, 58) = 0 THEN
 stat(t, 0, targstat) = small(stat(t, 0, targstat), stat(t, 1, targstat))
 IF w >= 0 THEN stat(w, 0, targstat) = small(stat(w, 0, targstat), stat(w, 1, targstat))
ELSE
 'increase maximum as well if not hp or mp
 IF targstat > 1 THEN
  stat(t, 1, targstat) = large(stat(t, 0, targstat), stat(t, 1, targstat))
  IF w >= 0 THEN stat(w, 0, targstat) = large(stat(w, 0, targstat), stat(w, 1, targstat))
 END IF
END IF

'--TODO: Must add the attack-tag conditional stuff. Except, this sub doesn't use
'        the full attack data set, so I can't access the data. I also don't
'        know enough about it to make it use the whole thing...
'        [Note from James: The above is no longer true. oobcure has access to all attack data]

END SUB

SUB patcharray (array(), n$)

DIM num$(2), hexk(15)

clearpage dpage
clearpage vpage

hexk(0) = 11
FOR i = 1 TO 9
 hexk(i) = i + 1
NEXT i
hexk(10) = 30
hexk(11) = 48
hexk(12) = 46
hexk(13) = 32
hexk(14) = 18
hexk(15) = 33
pt = 0

setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN EXIT SUB
 IF keyval(scUp) > 1 THEN csr = large(0, csr - 1)
 IF keyval(scDown) > 1 THEN csr = small(2, csr + 1)
 IF csr = 0 THEN intgrabber pt, 0, UBOUND(array)
 IF csr = 1 THEN intgrabber array(pt), -32768, 32767
 IF csr = 2 THEN
  FOR i = 0 TO 15
   IF keyval(hexk(i)) > 1 THEN setbit array(), pt, i, readbit(array(), pt, i) XOR 1
  NEXT i
 END IF
 num$(0) = n$ & "(" & ABS(pt) & ")"
 num$(1) = "value = " & array(pt)
 num$(2) = ""
 FOR i = 0 TO 15
  IF readbit(array(), pt, i) THEN
   num$(2) = num$(2) + "1"
  ELSE
   num$(2) = num$(2) + "0"
  END IF
 NEXT i
 edgeprint "DEBUG MODE", 120, 50, uilook(uiText), dpage
 centerbox 160, 100, 140, 60, 1, dpage
 FOR i = 0 TO 2
  c = uilook(uiMenuItem): IF i = csr THEN c = uilook(uiSelectedItem + tog)
  edgeprint num$(i), 160 - LEN(num$(i)) * 4, 80 + i * 10, c, dpage
 NEXT i
 edgeprint "0123456789ABCDEF", 96, 110, uilook(uiSelectedDisabled), dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

END SUB

FUNCTION picksave (loading as integer) as integer

DIM full(3), herosname$(3), mapname$(3), svtime$(3), lev$(3), id(3, 3), tstat(3, 1, 16), confirm$(1), menu$(1)
DIM sprites(3, 3) AS GraphicPair

'--loading 0 is the save menu, 1 is load menu, and 2 is load with no titlescreen. it fades the screen in
'--loading 0+1 use dpage as background, loading 2 uses none. pages 2 and 3 are preserved
'--terribly sorry for the dirtyness
needf = 0
IF loading = 2 THEN
 needf = 2
END IF

'--load strings. menu$ array holds the names of the options
'--at the top of the screeen (only one appears when saving)

IF loading THEN
 cursor = 0
 menu$(0) = readglobalstring$(52, "New Game", 10)
 menu$(1) = readglobalstring$(53, "Exit", 10)
ELSE
 cursor = lastsaveslot - 1
 confirm$(0) = readglobalstring$(44, "Yes", 10)
 confirm$(1) = readglobalstring$(45, "No", 10)
 menu$(0) = readglobalstring$(59, "CANCEL", 10)
 replacedat$ = readglobalstring$(102, "Replace Old Data?", 20)
 menuwidth = 8 * large(LEN(confirm$(0)), LEN(confirm$(1)))
END IF

holdscreen = allocatepage
IF loading < 2 THEN
 '--preserve background for display beneath the save/load picker
 copypage vpage, holdscreen
END IF
'otherwise, holdscreen is black

FOR i = 0 TO 3
 sg$ = savefile
 setpicstuf buffer(), 30000, -1
 loadset sg$, i * 2, 0
 IF buffer(0) = 3 THEN 'current version number
  full(i) = 1
  '--get map number
  map = buffer(1)
  '--get stats
  '--if the save format changes, so must this
  z = 3305
  FOR i1 = 0 TO 3
   FOR i2 = 0 TO 1
    FOR i3 = 0 TO 13
     tstat(i1, i2, i3) = buffer(z)
     z = z + 1
    NEXT i3
   NEXT i2
  NEXT i1
  '--get play time
  '--if the save format changes, so must this
  z = 34 + 51
  svtime$(i) = playtime$(buffer(z), buffer(z + 1), buffer(z + 2))
  '--hero ID
  foundleader = 0
  FOR o = 0 TO 3
   '--load hero ID
   id(i, o) = buffer(2763 + o)
   '--leader name and level
   IF foundleader = 0 AND id(i, o) > 0 THEN
    foundleader = 1
    FOR j = 0 TO 15
     k = buffer(11259 + (o * 17) + j)
     IF k > 0 AND k < 255 THEN herosname$(i) = herosname$(i) + CHR$(k)
    NEXT j
    lev$(i) = readglobalstring$(43, "Level", 10) & " " & tstat(o, 0, 12)
   END IF
  NEXT o
  '--load second record
  loadset sg$, i * 2 + 1, 0
  '--get picture and palette info
  z = 6060
  picpalmagic = buffer(z): z = z + 1
  FOR i1 = 0 TO 3
   FOR i2 = 0 TO 1
    FOR i3 = 14 TO 16
     IF picpalmagic = 4444 THEN tstat(i1, i2, i3) = buffer(z)
     z = z + 1
    NEXT i3
   NEXT i2
  NEXT i1
  '--get name and load pictures n stuff
  FOR o = 0 TO 3
   IF id(i, o) >= 0 THEN
    '--hero pic and palette
    IF picpalmagic = 4444 THEN
     sprites(i, o).sprite = sprite_load(0, tstat(o, 0, 14))
     sprites(i, o).pal = palette16_load(tstat(o, 0, 15), 0, tstat(o, 0, 14))
    ELSE
     '--backcompat
     dim her as herodef
     loadherodata @her, id(i,o) - 1
     sprites(i, o).sprite = sprite_load(0, her.sprite)
     sprites(i, o).pal = palette16_load(her.sprite_pal, 0, her.sprite)
    END IF
   END IF
  NEXT o
  mapname$(i) = getmapname$(map)
 END IF
NEXT i

IF loading THEN
 'check for no slots
 nofull = 0
 FOR i = 0 TO 3
  IF full(i) = 1 THEN nofull = 1
 NEXT i
 IF nofull = 0 THEN 
  picksave = -1
  GOTO freesprites
 END IF
END IF

IF needf = 0 THEN MenuSound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 walk = walk XOR tog
 IF loading = 0 THEN playtimer
 control
 IF carray(ccMenu) > 1 THEN
  MenuSound gen(genCancelSFX)
  IF loading THEN picksave = -2 ELSE picksave = -1
  EXIT DO
 END IF
 IF cursor = -2 THEN
  IF carray(ccUp) > 1 THEN cursor = 3: MenuSound gen(genCursorSFX)
  IF carray(ccDown) > 1 THEN cursor = 0: MenuSound gen(genCursorSFX)
 ELSE
  IF carray(ccUp) > 1 THEN cursor = loopvar(cursor, -1, 3, -1): MenuSound gen(genCursorSFX)
  IF carray(ccDown) > 1 THEN cursor = loopvar(cursor, -1, 3, 1): MenuSound gen(genCursorSFX)
 END IF
 IF cursor < 0 AND loading THEN
  IF carray(ccLeft) > 1 THEN cursor = -1: MenuSound gen(genCursorSFX)
  IF carray(ccRight) > 1 THEN cursor = -2: MenuSound gen(genCursorSFX)
 END IF
 IF carray(ccUse) > 1 THEN
  IF cursor = -2 THEN
   MenuSound gen(genCancelSFX)
   picksave = cursor
   EXIT DO
  ELSEIF cursor = -1 THEN
   MenuSound gen(genAcceptSFX)
   picksave = cursor
   EXIT DO
  ELSE
   allow = 1
   IF loading THEN
    '--normal load of an existing save
    IF full(cursor) = 0 THEN allow = 0
   ELSE
    '--normal save in a slot
    IF full(cursor) = 1 THEN GOSUB confirm
   END IF
   IF allow = 1 THEN
    MenuSound gen(genAcceptSFX)
    picksave = cursor
    lastsaveslot = cursor + 1
    EXIT DO
   ELSE
    MenuSound gen(genCancelSFX)
   END IF
  END IF
 END IF
 GOSUB drawmenugosub
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 IF needf = 1 THEN   'the titlescreen might be skipped and with it the fading in
  needf = 0
  fadein
 END IF
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP

freesprites:
freepage holdscreen
FOR t = 4 TO 5: carray(t) = 0: NEXT t
FOR i = 0 TO 3
 FOR o = 0 TO 3
  sprite_unload(@sprites(i, o).sprite)
  palette16_unload(@sprites(i, o).pal)
 NEXT
NEXT
EXIT FUNCTION

confirm:
allow = 0
MenuSound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(ccMenu) > 1 THEN
  allow = 0
  MenuSound gen(genCancelSFX)
  RETRACE
 END IF
 IF carray(ccUp) > 1 OR carray(ccDown) > 1 THEN
  allow = allow XOR 1
  MenuSound gen(genCursorSFX)
 END IF
 IF carray(ccUse) > 1 THEN RETRACE
 GOSUB drawmenugosub
 centerbox 160, 14 + (44 * cursor), 40 + (LEN(replacedat$) * 8) + menuwidth, 24, 3, dpage
 edgeprint replacedat$, 200 - (LEN(replacedat$) * 8), 9 + (44 * cursor), uilook(uiText), dpage
 FOR i = 0 TO 1
 col = uilook(uiSelectedItem + tog): IF allow = i THEN col = uilook(uiMenuItem)
  edgeprint confirm$(i), 216, 5 + (i * 9) + (44 * cursor), col, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 dowait
LOOP

drawmenugosub:
centerbox 50, 11, 80, 14, 15, dpage
IF loading THEN centerbox 270, 11, 80, 14, 15, dpage
FOR i = 0 TO 3
 centerbox 160, 44 + i * 44, 310, 42, 15, dpage
NEXT i
'load and save menus enjoy different colour schemes
IF loading THEN activec = 2 ELSE activec = 1
SELECT CASE cursor
 CASE -2
  centerbox 270, 11, 82, 16, activec, dpage
 CASE -1
  centerbox 50, 11, 82, 16, activec, dpage
 CASE ELSE
  centerbox 160, 44 + cursor * 44, 312, 44, activec, dpage
END SELECT
FOR i = 0 TO 3
 IF full(i) = 1 THEN
  FOR o = 0 TO 3
   IF id(i, o) > 0 THEN
    sprite_draw sprites(i, o).sprite + iif(cursor = i, walk, 0), sprites(i, o).pal, 140 + (o * 42), 24 + i * 44, 1, -1, dpage
   END IF
  NEXT o
  col = uilook(uiMenuItem)
  IF cursor = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint herosname$(i), 14, 25 + i * 44, col, dpage
  edgeprint lev$(i), 14, 34 + i * 44, col, dpage
  edgeprint svtime$(i), 14, 43 + i * 44, col, dpage
  edgeprint mapname$(i), 14, 52 + i * 44, col, dpage
 END IF
NEXT i
col = uilook(uiMenuItem): IF cursor = -1 THEN col = uilook(uiSelectedItem + tog)
edgeprint menu$(0), xstring(menu$(0), 50), 6, col, dpage
IF loading THEN
 col = uilook(uiMenuItem): IF cursor = -2 THEN col = uilook(uiSelectedItem + tog)
 edgeprint menu$(1), xstring(menu$(1), 270), 6, col, dpage
END IF
RETRACE

END FUNCTION

SUB sellstuff (id, storebuf(), stat())
DIM b(dimbinsize(binSTF) * 50), permask(15), price(200)
recordsize = curbinsize(binSTF) / 2 ' get size in INTs

'--preserve background for display under sell menu
holdscreen = allocatepage
copypage vpage, holdscreen

cannotsell$ = readglobalstring$(75, "CANNOT SELL", 20)
worth$ = readglobalstring$(77, "Worth", 20)
tradefor$ = readglobalstring$(79, "Trade for", 20)
anda$ = readglobalstring$(81, "and a", 10)
andsome$ = readglobalstring$(153, "and", 10)
worthnothing$ = readglobalstring$(82, "Worth Nothing", 20)
sold$ = readglobalstring$(84, "Sold", 10)

loadshopstuf b(), id
GOSUB selstock

ic = 0: top = 0
alert = 0
alert$ = ""
info$ = ""

menusound gen(genAcceptSFX)
GOSUB refreshs

GOSUB sellinfostr
quit = 0
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 GOSUB keysell
 IF quit THEN EXIT DO
 centerbox 160, 92, 304, 176, 1, dpage
 FOR i = top TO top + 62
  textcolor uilook(uiMenuItem), 0
  IF readbit(permask(), 0, i) THEN textcolor uilook(uiDisabledItem), 0
  IF ic = i THEN
   textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight2)
   IF readbit(permask(), 0, i) THEN textcolor uilook(uiGold), uilook(uiHighlight2)
  END IF
  printstr inventory(i).text, 20 + 96 * (i MOD 3), 12 + 8 * ((i - top) \ 3), dpage
 NEXT i
 centerfuz 160, 180, 312, 20, 4, dpage
 edgeprint info$, xstring(info$, 160), 175, uilook(uiText), dpage
 edgeprint gold & " " & readglobalstring(32, "Money"), 310 - LEN(gold & " " & readglobalstring(32, "Money")) * 8, 1, uilook(uiGold), dpage
 IF alert THEN
  alert = alert - 1
  centerbox 160, 178, LEN(alert$) * 8 + 8, 14, 4, dpage
  edgeprint alert$, xstring(alert$, 160), 173, uilook(uiSelectedItem + tog), dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 dowait
LOOP
freepage holdscreen
menusound gen(genCancelSFX)
EXIT SUB

sellinfostr:
info$ = ""
IF inventory(ic).used = 0 THEN RETRACE
IF readbit(permask(), 0, ic) = 1 THEN info$ = cannotsell$: RETRACE
IF price(ic) > 0 THEN info$ = worth$ & " " & price(ic) & " " & readglobalstring(32, "Money")
FOR i = 0 TO storebuf(16)
 IF b(i * recordsize + 17) = 0 AND b(i * recordsize + 18) = inventory(ic).id THEN
  IF b(i * recordsize + 28) > 0 THEN
   IF info$ = "" THEN
    info$ = tradefor$ + " "
   ELSE
    IF b(i * recordsize + 29) > 0 THEN
     info$ = info$ + " " + andsome$ + " "
    ELSE
     info$ = info$ + " " + anda$ + " "
    END IF
   END IF
   IF b(i * recordsize + 29) > 0 THEN info$ = info$ + STR$(b(i * recordsize + 29) + 1) + " "
   info$ = info$ + readitemname$(b(i * recordsize + 28) - 1)
  END IF
 END IF
NEXT i
IF info$ = "" THEN info$ = worthnothing$
RETRACE

keysell:
IF carray(ccMenu) > 1 THEN quit = 1
IF carray(ccUse) > 1  AND inventory(ic).used THEN
 IF readbit(permask(), 0, ic) = 0 THEN
  menusound gen(genSellSFX)
  alert = 10
  alert$ = sold$ + " " + readitemname$(inventory(ic).id)
  'INCREMENT GOLD-----------
  gold = gold + price(ic)
  IF gold > 2000000000 THEN gold = 2000000000
  IF gold < 0 THEN gold = 0
  'CHECK FOR SPECIAL CASES---------
  FOR i = 0 TO storebuf(16)
   IF b(i * recordsize + 17) = 0 AND b(i * recordsize + 18) = inventory(ic).id THEN
    'SET SELL BIT---
    IF b(i * recordsize + 23) <> 0 THEN setbit tag(), 0, ABS(b(i * recordsize + 23)), SGN(SGN(b(i * recordsize + 23)) + 1)
    'ADD TRADED ITEM-----------
    IF b(i * recordsize + 28) > 0 THEN getitem b(i * recordsize + 28), b(i * recordsize + 29) + 1
    'INCREMENT STOCK-------
    IF b(i * recordsize + 26) > 0 THEN
     IF b(i * recordsize + 26) = 1 THEN gam.stock(id, i) = -1
     IF b(i * recordsize + 26) = 2 AND gam.stock(id, i) > 0 THEN gam.stock(id, i) = gam.stock(id, i) + 1
    END IF
   END IF
  NEXT i
  'DECREMENT ITEM-----------
  dummy = consumeitem(ic)
  'UPDATE ITEM POSESSION TAGS--------
  evalitemtag
  'REFRESH DISPLAY--------
  GOSUB refreshs
  GOSUB sellinfostr
 ELSE
  menusound gen(genCantSellSFX)
 END IF
END IF
IF carray(ccUp) > 1 AND ic >= 3 THEN
 menusound gen(genCursorSFX)
 ic = ic - 3
 GOSUB sellinfostr
 IF ic < top THEN top = top - 3
END IF
IF carray(ccDown) > 1 AND ic <= last_inv_slot() - 3 THEN
 menusound gen(genCursorSFX)
 ic = ic + 3
 GOSUB sellinfostr
 IF ic > top + 62 THEN top = top + 3
END IF
IF carray(ccLeft) > 1 THEN
 menusound gen(genCursorSFX)
 IF ic MOD 3 > 0 THEN
  ic = ic - 1
  GOSUB sellinfostr
 ELSE
  ic = ic + 2
  GOSUB sellinfostr
 END IF
END IF
IF carray(ccRight) > 1 THEN
 menusound gen(genCursorSFX)
 IF ic MOD 3 < 2 THEN
  ic = ic + 1
  GOSUB sellinfostr
 ELSE
  ic = ic - 2
  GOSUB sellinfostr
 END IF
END IF
RETRACE

refreshs:
FOR i = 0 TO last_inv_slot()
 IF inventory(i).used THEN
  loaditemdata buffer(), inventory(i).id
  IF buffer(73) = 2 THEN setbit permask(), 0, i, 1
  price(i) = INT(buffer(46) * .5)
  FOR o = 0 TO storebuf(16)
   IF b(o * recordsize + 18) = inventory(i).id THEN
    IF ABS(b(o * recordsize + 21)) > 0 THEN IF readbit(tag(), 0, ABS(b(o * recordsize + 21))) <> SGN(SGN(b(o * recordsize + 21)) + 1) THEN setbit permask(), 0, i, 1
    IF b(o * recordsize + 17) = 0 THEN
     price(i) = b(o * recordsize + 27)
     IF b(o * recordsize + 26) = 3 THEN setbit permask(), 0, i, 1
    END IF
   END IF
  NEXT o
 END IF
NEXT i
RETRACE

selstock:
FOR i = 0 TO storebuf(16)
 IF gam.stock(id, i) = 0 THEN gam.stock(id, i) = b(i * recordsize + 19): IF gam.stock(id, i) > -1 THEN gam.stock(id, i) = gam.stock(id, i) + 1
NEXT i
RETRACE

END SUB

SUB spells (pt, stat())
REMEMBERSTATE

DIM menu$(4), mi(4), mtype(5), spel$(24), speld$(24), cost$(24), spel(24), canuse(24), targt(24), spid(5), tstat(24)
dim her as herodef

cancelmenu$ = readglobalstring$(51, "(CANCEL)", 10)
hasnone$ = readglobalstring$(133, "has no spells", 20)

pick = 0
spred = 0
wptr = 0
last = 0
sptr = 0
mset = 0

GOSUB splname
'--Preserve background for display beneath the spells menu
holdscreen = allocatepage
copypage vpage, holdscreen

csr = 0
menusound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 wtogl = loopvar(wtogl, 0, 3, 1)
 playtimer
 control
 GOSUB scontrol
 centerfuz 160, 100, 312, 184, 1, dpage 'outer box
 centerbox 206, 36, 200, 17, 2, dpage   'name box
 centerbox 56, 50, 84, 60, 2, dpage     'menu box
 centerbox 160, 134, 308, 96, 2, dpage  'spell list
 rectangle 6, 168, 308, 1, uilook(uiTextBox + 3), dpage 'divider 2
 FOR i = 0 TO last
  IF mi(i) >= 0 AND csr = i THEN
   FOR o = 0 TO 23
    'Note: this will give yellow when canuse is -1 (is it ever?), orig would give blue
    textcolor uilook(uiDisabledItem - SGN(canuse(o))), 0
    IF sptr = o AND mset = 1 THEN
     IF canuse(o) > 0 THEN 
      textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight) 
     ELSE 
      textcolor uilook(uiMenuItem), uilook(uiHighlight)
     END IF
    END IF
    printstr spel$(o), 12 + (o MOD 3) * 104, 90 + (o \ 3) * 8, dpage 'spells
   NEXT o
   textcolor uilook(uiMenuItem), 0
   IF sptr = 24 AND mset = 1 THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
   printstr cancelmenu$, 16, 171, dpage 'cancel
   IF mset = 1 THEN
    IF speld$(sptr) <> "" THEN
     rectangle 6, 155, 308, 1, uilook(uiTextBox + 3), dpage  'description divider
    END IF
    textcolor uilook(uiDescription), 0
    printstr cost$(sptr), 303 - LEN(cost$(sptr)) * 8, 171, dpage 'cost
    printstr speld$(sptr), 9, 158, dpage 'description
   END IF
  END IF
  textcolor uilook(uiMenuItem), 0
  IF csr = i THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight2): IF mset = 1 THEN textcolor uilook(uiMenuItem), uilook(uiHighlight2)
  printstr menu$(i), 16, 25 + i * 10, dpage 'spell menu
 NEXT i
 IF last = 0 THEN edgeprint names(pt) + " " + hasnone$, xstring(names(pt) + " " + hasnone$, 160), 120, uilook(uiText), dpage
 edgeprint names(pt), xstring(names(pt), 206), 31, uilook(uiText), dpage
 IF pick = 1 THEN
  centerbox 196, 47, 160, 88, 2, dpage
  IF spred = 0 AND wptr >= 0 THEN
   rectangle 120, 8 + wptr * 20, 152, 20, uilook(uiHighlight2), dpage
  ELSEIF spred <> 0 THEN
   rectangle 120, 8, 152, 80, uilook(uiHighlight2 * tog), dpage
  END IF
  o = 0
  FOR i = 0 TO 3
   IF hero(i) > 0 THEN
    wt = 0: IF wptr = i THEN wt = INT(wtogl / 2)
    sprite_draw herow(o).sprite + (2 * 2) + wt, herow(o).pal, 125, 8 + i * 20, 1, -1, dpage
    IF tstat(sptr) = 0 or tstat(sptr) = 1 THEN
     temp$ = STR$(ABS(stat(i, 0, tstat(sptr)))) & "/" & STR$(ABS(stat(i, 1, tstat(sptr))))
    ELSE
     temp$ = STR$(ABS(stat(i, 0, tstat(sptr))))
    END IF
    col = uilook(uiMenuItem): IF i = wptr THEN col = uilook(uiSelectedItem + tog)
    edgeprint temp$, 155, 16 + i * 20, col, dpage
    o = o + 1
   END IF
  NEXT i
 END IF

 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 dowait
LOOP

curspellist:
IF mtype(csr) < 0 THEN RETRACE
FOR i = 0 TO 23
 spel$(i) = "": speld$(i) = "": cost$(i) = "": spel(i) = -1: canuse(i) = 0: targt(i) = 0: tstat(i) = 0
 IF spell(pt, spid(csr), i) > 0 THEN
  spel(i) = spell(pt, spid(csr), i) - 1
  loadattackdata buffer(), spel(i)
  IF readbit(buffer(), 20, 59) = 1 THEN
   canuse(i) = buffer(3) + 1
   targt(i) = buffer(4)
   tstat(i) = buffer(18)
  END IF
  cost = focuscost(buffer(8), stat(pt, 0, statFocus))
  IF mtype(csr) = 0 AND stat(pt, 0, statMP) < cost THEN canuse(i) = 0
  IF mtype(csr) = 1 AND lmp(pt, INT(i / 3)) = 0 THEN canuse(i) = 0
  IF stat(pt, 0, statHP) = 0 THEN canuse(i) = 0
  spel$(i) = readbadbinstring$(buffer(), 24, 10, 1)
  speld$(i) = readbinstring$(buffer(),73,38)
  IF mtype(csr) = 0 THEN cost$(i) = cost & " " & statnames(statMP) & " " & ABS(stat(pt, 0, statMP)) & "/" & ABS(stat(pt, 1, statMP))
  IF mtype(csr) = 1 THEN cost$(i) = readglobalstring$(160, "Level MP", 20) & " " & (INT(i / 3) + 1) & ":  " & lmp(pt, INT(i / 3))
 END IF
 WHILE LEN(spel$(i)) < 10: spel$(i) = spel$(i) + " ": WEND
NEXT i
RETRACE

splname:
loadherodata @her, hero(pt) - 1 'why the heck would we load this 12 times?!
FOR i = 0 TO 5
 '--for each btl menu slot
 '--clear menu type
 mtype(i) = -1
 '--if it is a menu...
 IF bmenu(pt, i) < 0 AND bmenu(pt, i) > -10 THEN
  '--set spell-menu-id and menu-type
  spid(i) = (bmenu(pt, i) + 1) * -1
  mtype(i) = her.list_type(spid(i))
 END IF
NEXT i
last = 0
FOR o = 0 TO 5
 IF mtype(o) >= 0 AND mtype(o) < 2 THEN
  IF readbit(her.bits(), 0, 26) <> 0 then
   IF count_available_spells(pt, o - 1) = 0 THEN CONTINUE FOR
  END IF
  menu$(last) = ""
  mtype(last) = mtype(o)
  spid(last) = spid(o)

  '--get menu index
  mi(last) = (bmenu(pt, o) + 1) * -1

  '--read menu name
  menu$(last) = her.list_name(mi(last))

  '--if non-null...
  IF menu$(last) <> "" THEN
   '--right-pad the name
   menu$(last) = rpad(menu$(last), " ", 10)
   '--increment the spell-list counter because
   '--we only (currently) care about non-null-named spell lists
   last = last + 1
  END IF
 END IF
NEXT o
menu$(last) = rpad(readglobalstring$(46, "Exit", 10), " ", 10)
mi(last) = -1
mtype(last) = -1
IF csr > last THEN csr = last
GOSUB curspellist
RETRACE

scontrol:
IF pick = 0 THEN '--picking which spell list
 IF mset = 0 THEN
  IF carray(ccMenu) > 1 THEN GOTO exitpoint
  IF carray(ccLeft) > 1 THEN
   DO
    pt = loopvar(pt, 0, 3, -1)
   LOOP UNTIL hero(pt) > 0
   menusound gen(genCursorSFX)
   GOSUB splname
  END IF
  IF carray(ccRight) > 1 THEN
   DO
    pt = loopvar(pt, 0, 3, 1)
   LOOP UNTIL hero(pt) > 0
   menusound gen(genCursorSFX)
   GOSUB splname
  END IF
  IF carray(ccUp) > 1 THEN
   csr = large(csr - 1, 0)
   menusound gen(genCursorSFX)
   GOSUB curspellist
  END IF
  IF carray(ccDown) > 1 THEN
   csr = small(csr + 1, last)
   menusound gen(genCursorSFX)
   GOSUB curspellist
  END IF
  IF carray(ccUse) > 1 THEN
   IF mi(csr) = -1 THEN GOTO exitpoint
   menusound gen(genAcceptSFX)
   mset = 1: sptr = 0
  END IF
 ELSE
  IF carray(ccMenu) > 1 THEN
   mset = 0
   menusound gen(genCancelSFX)
  END IF
  IF carray(ccUp) > 1 THEN
   sptr = sptr - 3
   menusound gen(genCursorSFX)
   IF sptr < 0 THEN sptr = 24
  END IF
  IF carray(ccDown) > 1 THEN
   IF sptr < 24 THEN
    sptr = small(sptr + 3, 24)
   ELSE
    sptr = 0
   END IF
   menusound gen(genCursorSFX)
  END IF
  IF sptr < 24 THEN
   IF carray(ccLeft) > 1 THEN
    IF sptr MOD 3 THEN
     sptr = sptr - 1
    ELSE
     sptr = sptr + 2
    END IF
    menusound gen(genCursorSFX)
   END IF
   IF carray(ccRight) > 1 THEN
    menusound gen(genCursorSFX)
    IF sptr MOD 3 = 2 THEN sptr = sptr - 2 ELSE sptr = sptr + 1
   END IF
  END IF
  IF carray(ccUse) > 1 THEN
   IF sptr = 24 THEN mset = 0
   IF canuse(sptr) > 0 THEN
    '--spell that can be used oob
    wptr = pt - 1
    IF getOOBtarg(1, wptr, spel(sptr), stat()) = 0 THEN
     '--Failed to get target
     wptr = -1
     menusound gen(genCancelSFX)
    END IF
    pick = 1
    spred = 0
    menusound gen(genAcceptSFX)
    IF targt(sptr) = 1 AND canuse(sptr) - 1 <> 2 THEN
     FOR i = 0 TO 3
      IF chkOOBtarg(i, spel(sptr), stat()) THEN spred = spred + 1
     NEXT i
    END IF
   ELSE
    menusound gen(genCancelSFX)
   END IF
  END IF
 END IF
ELSE
 IF carray(ccMenu) > 1 THEN
  menusound gen(genCancelSFX)
  pick = 0
 END IF
 IF canuse(sptr) - 1 <> 2 AND spred = 0 THEN
  IF carray(ccUp) > 1 THEN
   getOOBtarg -1, wptr, spel(sptr), stat()
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccDown) > 1 THEN
   getOOBtarg 1, wptr, spel(sptr), stat()
   MenuSound gen(genCursorSFX)
  END IF
 END IF
 IF targt(sptr) = 2 AND canuse(sptr) - 1 <> 2 THEN
  IF carray(ccLeft) > 1 OR carray(ccRight) > 1 THEN
   IF spred = 0 THEN
    FOR i = 0 TO 3
     IF chkOOBtarg(i, spel(sptr), stat()) THEN spred = spred + 1
    NEXT i
   ELSE
    spred = 0
   END IF
  END IF
 END IF
 IF carray(ccUse) > 1 THEN
  IF mtype(csr) = 0 THEN
   loadattackdata buffer(), spel(sptr)
   cost = focuscost(buffer(8), stat(pt, 0, statFocus))
   IF cost > stat(pt, 0, statMP) THEN pick = 0: RETRACE
   stat(pt, 0, statMP) = small(large(stat(pt, 0, statMP) - cost, 0), stat(pt, 1, statMP))
  END IF
  IF mtype(csr) = 1 THEN
   IF lmp(pt, INT(sptr / 3)) = 0 THEN pick = 0: RETRACE
   lmp(pt, INT(sptr / 3)) = lmp(pt, INT(sptr / 3)) - 1
  END IF
  'DO ACTUAL EFFECT
  outside_battle_cure spel(sptr), wptr, pt, stat(), spred
  GOSUB curspellist
 END IF
END IF
RETRACE

exitpoint:
menusound gen(genCancelSFX)
setkeys
FOR i = 0 TO 7: carray(i) = 0: NEXT i
freepage holdscreen
RETRIEVESTATE
EXIT SUB

END SUB

SUB status (pt, stat())
DIM mtype(5), hbits(3, 4), thishbits(4), elemtype$(2), info$(25)
DIM her AS HeroDef
DIM portrait AS GraphicPair

DIM exper_caption AS STRING = readglobalstring(33, "Experience", 10)
DIM level_caption AS STRING = readglobalstring(43, "Level", 10)
DIM level_mp_caption AS STRING = readglobalstring(160, "Level MP", 20)
elemtype$(0) = readglobalstring(127, "Weak to", 10)
elemtype$(1) = readglobalstring(128, "Strong to", 10)
elemtype$(2) = readglobalstring(129, "Absorbs", 10)

'--calculate bitsets with equipment
FOR i = 0 TO 3
 herobattlebits_raw hbits(), i
NEXT i

mode = 0
top = 0
lastinfo = 0

GOSUB nextstat
'--Preserve background for display under status menu
holdscreen = allocatepage
copypage vpage, holdscreen

menusound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(ccMenu) > 1 THEN EXIT DO
 IF carray(ccUse) > 1 THEN mode = loopvar(mode, 0, 2, 1): menusound gen(genCursorSFX)
 IF carray(ccLeft) > 1 THEN DO: pt = loopvar(pt, 0, 3, -1): LOOP UNTIL hero(pt) > 0: menusound gen(genCursorSFX): GOSUB nextstat
 IF carray(ccRight) > 1 THEN DO: pt = loopvar(pt, 0, 3, 1): LOOP UNTIL hero(pt) > 0: menusound gen(genCursorSFX): GOSUB nextstat
 IF carray(ccUp) > 1 THEN top = large(top - 1, 0): menusound gen(genCursorSFX)
 IF carray(ccDown) > 1 THEN top = small(top + 1, large(0, lastinfo - 11)): menusound gen(genCursorSFX)

 centerfuz 160, 100, 304, 184, 1, dpage
 centerbox 160, 36, 292, 40, 4, dpage
 SELECT CASE mode
  CASE 0
   centerbox 84, 120, 140, 120, 4, dpage
   centerbox 236, 120, 140, 120, 4, dpage
  CASE 1, 2
   centerbox 160, 120, 292, 120, 4, dpage
 END SELECT
 IF her.portrait >= 0 THEN
  edgeboxstyle 262, 8, 50, 50, 3, dpage
  sprite_draw portrait.sprite, portrait.pal, 262, 8,,,dpage
 END IF

 edgeprint names(pt), 142 - LEN(names(pt)) * 4, 20, uilook(uiText), dpage
 edgeprint level_caption & " " & stat(pt, 0, 12), 142 - LEN(level_caption & " " & stat(pt, 0, 12)) * 4, 30, uilook(uiText), dpage
 temp$ = (exlev(pt, 1) - exlev(pt, 0)) & " " & exper_caption & " " & readglobalstring$(47, "for next", 10) & " " & level_caption
 edgeprint temp$, 142 - LEN(temp$) * 4, 40, uilook(uiText), dpage

 SELECT CASE mode
  CASE 0
   '--show stats
   FOR i = 0 TO 9
    edgeprint statnames(i + 2), 20, 62 + i * 10, uilook(uiText), dpage
    temp$ = STR$(stat(pt, 0, i + 2))
    edgeprint temp$, 148 - LEN(temp$) * 8, 62 + i * 10, uilook(uiText), dpage
   NEXT i

   'current/max HP
   edgeprint statnames(statHP), 236 - LEN(statnames(statHP)) * 4, 65, uilook(uiText), dpage
   temp$ = STR$(ABS(stat(pt, 0, statHP))) + "/" + STR$(ABS(stat(pt, 1, statHP)))
   edgeprint temp$, 236 - LEN(temp$) * 4, 75, uilook(uiText), dpage

   '--MP and level MP
   FOR i = 0 TO 5
    IF mtype(i) = 0 THEN
     edgeprint statnames(statMP), 236 - LEN(statnames(statMP)) * 4, 95, uilook(uiText), dpage
     temp$ = STR$(ABS(stat(pt, 0, statMP))) + "/" + STR$(ABS(stat(pt, 1, statMP)))
     edgeprint temp$, 236 - LEN(temp$) * 4, 105, uilook(uiText), dpage
    END IF
    IF mtype(i) = 1 THEN
     edgeprint level_mp_caption, 236 - LEN(level_mp_caption) * 4, 125, uilook(uiText), dpage
     temp$ = ""
     FOR o = 0 TO 3
      temp$ = temp$ + STR$(ABS(lmp(pt, o))) + "/"
     NEXT o
     temp$ = LEFT$(temp$, LEN(temp$) - 1)
     edgeprint temp$, 236 - LEN(temp$) * 4, 135, uilook(uiText), dpage
     temp$ = ""
     FOR o = 4 TO 7
      temp$ = temp$ + STR$(ABS(lmp(pt, o))) + "/"
     NEXT o
     temp$ = LEFT$(temp$, LEN(temp$) - 1)
     edgeprint temp$, 236 - LEN(temp$) * 4, 145, uilook(uiText), dpage
    END IF
   NEXT i

   '--gold
   edgeprint gold & " " & readglobalstring(32, "Money"), 236 - LEN(gold & " " & readglobalstring(32, "Money")) * 4, 167, uilook(uiGold), dpage
  CASE 1

   '--show elementals
   FOR i = 0 TO 10
    IF top + i <= 25 THEN edgeprint info$(top + i), 20, 62 + i * 10, uilook(uiText), dpage
   NEXT i

  CASE 2
   '--tigger rename
   IF readbit(thishbits(), 0, 25) THEN
    '--status-screen rename is allowed
    renamehero pt
    mode = 0
   END IF

 END SELECT

 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 dowait
LOOP
menusound gen(genCancelSFX)
IF portrait.sprite THEN sprite_unload @portrait.sprite
IF portrait.pal    THEN palette16_unload @portrait.pal
freepage holdscreen
FOR t = 4 TO 5
 carray(t) = 0
NEXT t
EXIT SUB

nextstat: '--loads the hero who's ID is held in pt
'--load the hero data lump only to get the spell list types
'loadherodata buffer(), hero(pt) - 1
loadherodata @her, hero(pt) - 1

FOR i = 0 TO 5
 mtype(i) = -1
 IF bmenu(pt, i) < 0 AND bmenu(pt, i) > -10 THEN
  temp = (bmenu(pt, i) + 1) * -1
  IF her.list_name(temp) <> "" THEN mtype(i) = her.list_type(temp)
 END IF
NEXT i

'--get this heros bits
FOR i = 0 TO 4
 thishbits(i) = hbits(pt, i)
NEXT i

'--build elemental strings
lastinfo = 0
FOR o = 0 TO 2
 FOR i = 0 TO 7
  IF readbit(thishbits(), 0, i + o * 8) THEN
   info$(lastinfo) = elemtype$(o) & " " & readglobalstring(17 + i, "Type" & i+1)
   lastinfo = lastinfo + 1
  END IF
 NEXT i
NEXT o
IF lastinfo = 0 THEN info$(lastinfo) = readglobalstring$(130, "No Elemental Effects", 30): lastinfo = lastinfo + 1

FOR i = lastinfo TO 25
 info$(i) = ""
NEXT i

IF portrait.sprite THEN sprite_unload @portrait.sprite
IF portrait.pal    THEN palette16_unload @portrait.pal
IF her.portrait >= 0 THEN
 portrait.sprite = sprite_load(8, her.portrait)
 portrait.pal    = palette16_load(her.portrait_pal, 8, her.portrait)
END IF
RETRACE

END SUB

FUNCTION trylearn (who as integer, atk as integer, learntype as integer) as integer
'first arg is hero position in the party

'--returns 1 when the spell was learned, 0 when it was not learned

IF hero(who) = 0 THEN debug "trylearn fail on empty party slot " & who : RETURN 0

'--fail by default
result = 0

dim her as herodef
'--load the hero's data.
loadherodata @her, hero(who) - 1

'--for each spell list
FOR j = 0 TO 3
 '--for each spell slot
 FOR o = 0 TO 23
  '--if this slot is empty and accepts this spell
  '--and is learnable by learntype
  IF spell(who, j, o) = 0 AND her.spell_lists(j,o).attack = atk AND her.spell_lists(j,o).learned = learntype THEN
   spell(who, j, o) = atk
   result = 1
  END IF
 NEXT o
NEXT j

trylearn = result

END FUNCTION

SUB unequip (who, where, defwep, stat(), resetdw)

'--exit if nothing is equiped
IF eqstuf(who, where) = 0 THEN EXIT SUB

'--load the item data for the thing we are unequiping
loaditemdata buffer(), eqstuf(who, where) - 1

'--remove stat bonuses
FOR i = 0 TO 11
 stat(who, 1, i) = stat(who, 1, i) - buffer(54 + i)
 '--for non HP non MP stats, reset current to max
 IF i > 1 THEN stat(who, 0, i) = stat(who, 1, i)
 '--prevent negatives
 stat(who, 0, i) = small(stat(who, 0, i), stat(who, 1, i))
NEXT i

'--return item to inventory (if not the default weapon)
IF where = 0 AND eqstuf(who, where) = defwep THEN
ELSE
 getitem eqstuf(who, where), 1
END IF

'--blank out equipment
eqstuf(who, where) = 0

IF where = 0 AND resetdw THEN
 '--restore default weapon
 doequip defwep, who, where, defwep, stat()
END IF

END SUB

SUB loadshopstuf (array(), id)
ol = getbinsize(binSTF) / 2 'old size on disk
nw = curbinsize(binSTF) / 2 'new size in memory
flusharray array(), nw * 50, 0
'load shop data from STF lump
setpicstuf buffer(), ol * 2 * 50, -1
loadset game + ".stf", id, 0
'in case shop data has been resized, scale records to new size
FOR i = 0 TO ol - 1
 FOR o = 0 to 49
  array(o * nw + i) = buffer(o * ol + i)
 NEXT o
NEXT i
END SUB

'======== FIXME: move this up as code gets cleaned up ===========
OPTION EXPLICIT

FUNCTION count_available_spells(who AS INTEGER, list AS INTEGER) AS INTEGER
 DIM i AS INTEGER
 DIM n AS INTEGER = 0
 FOR i = 0 to 23
  IF spell(who, list, i) > 0 THEN n + = 1
 NEXT i
 RETURN n
END FUNCTION

FUNCTION outside_battle_cure (atk AS INTEGER, target AS INTEGER, attacker AS INTEGER, stat() AS INTEGER, spread AS INTEGER) AS INTEGER
 DIM i AS INTEGER
 DIM didcure AS INTEGER = NO
 IF spread = 0 THEN
  IF chkOOBtarg(target, atk, stat()) THEN oobcure attacker, target, atk, spread, stat() : didcure = YES
 ELSE
  FOR i = 0 TO 3
   IF chkOOBtarg(i, atk, stat()) THEN oobcure attacker, i, atk, spread, stat() : didcure = YES
  NEXT i
 END IF
 IF didcure THEN
  're-check validify of target
  getOOBtarg 1, target, atk, stat(), YES
 END IF
 RETURN didcure
END FUNCTION

SUB equip (who, stat())

'--dim stuff
DIM m$(4), menu$(6)
DIM holdscreen = allocatepage
DIM st AS EquipMenuState

DIM i AS INTEGER
DIM tog AS INTEGER = 0
DIM stat_caption AS STRING
DIM col AS INTEGER
DIM item_id AS INTEGER

'--get names
m$(0) = readglobalstring$(38, "Weapon", 10)
FOR i = 0 TO 3
 m$(i + 1) = readglobalstring(25 + i, "Armor" & i+1)
NEXT i
menu$(5) = rpad(readglobalstring$(39, "-REMOVE-", 8), " ", 8)
menu$(6) = rpad(readglobalstring$(40, "-EXIT-", 8), " ", 8)

'--initialize
WITH st
 .mode = 0
 .who = who
 .eq_cursor.size = 17
 .default_weapon = 0
 .default_weapon_name = ""
 .unequip_caption = rpad(readglobalstring$(110, "Nothing", 10), " ", 11)
END WITH
equip_menu_setup st, menu$()

'--prepare the backdrop
'preserve the background behind the equip menu
copypage vpage, holdscreen

'--main loop
MenuSound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF st.mode = 0 THEN
  '--primary menu
  IF carray(ccMenu) > 1 THEN
   carray(ccUse) = 0
   carray(ccMenu) = 0
   EXIT DO
  END IF
  IF carray(ccLeft) > 1 THEN 'Left: previous hero
   DO: st.who = loopvar(st.who, 0, 3, -1): LOOP UNTIL hero(st.who) > 0
   equip_menu_setup st, menu$()
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccRight) > 1 THEN 'Right: next hero
   DO: st.who = loopvar(st.who, 0, 3, 1): LOOP UNTIL hero(st.who) > 0
   equip_menu_setup st, menu$()
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccUp) > 1 THEN 'Up: slot cursor up
   st.slot = loopvar(st.slot, 0, 6, - 1)
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccDown) > 1 THEN 'Down slot cursor down
   st.slot = loopvar(st.slot, 0, 6, 1)
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccUse) > 1 THEN
   IF st.slot < 5 THEN
    '--change equipment
    IF st.eq(st.slot).count > 0 OR eqstuf(st.who, st.slot) > 0 THEN
     '--switch to change equipment mode
     st.mode = 1
     st.eq_cursor.pt = 0
     st.eq_cursor.top = 0
     equip_menu_stat_bonus st
     MenuSound gen(genAcceptSFX)
    END IF
    'UPDATE ITEM POSESION BITSETS
    evalitemtag
   END IF
   IF st.slot = 5 THEN
    MenuSound gen(genCancelSFX)
    '--unequip all
    FOR i AS INTEGER = 0 TO 4
     unequip st.who, i, st.default_weapon, stat(), 1
    NEXT i
    equip_menu_setup st, menu$()
    'UPDATE ITEM POSESSION BITSETS
    evalitemtag
   END IF
   IF st.slot = 6 THEN carray(ccUse) = 0: EXIT DO
  END IF
 ELSE
  '--change equip menu
  IF carray(ccMenu) > 1 THEN
   st.mode = 0
   flusharray st.stat_bonus()
   MenuSound gen(genCancelSFX)
  END IF
  IF carray(ccUp) > 1 THEN
   st.eq_cursor.pt = large(st.eq_cursor.pt - 1, 0)
   IF st.eq_cursor.pt < st.eq_cursor.top THEN st.eq_cursor.top -= 1
   equip_menu_stat_bonus st
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccDown) > 1 THEN
   st.eq_cursor.pt = small(st.eq_cursor.pt + 1, st.eq(st.slot).count)
   IF st.eq_cursor.pt > st.eq_cursor.top + st.eq_cursor.size THEN st.eq_cursor.top += 1
   equip_menu_stat_bonus st
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccUse) > 1 THEN
   IF st.eq_cursor.pt = st.eq(st.slot).count THEN
    '--unequip
    unequip st.who, st.slot, st.default_weapon, stat(), 1
    equip_menu_back_to_menu st, menu$()
    MenuSound gen(genCancelSFX)
   ELSE
    '--normal equip
    item_id = inventory(st.eq(st.slot).offset(st.eq_cursor.pt)).id
    equip_menu_do_equip item_id + 1, st, menu$()
    MenuSound gen(genAcceptSFX)
   END IF
  END IF
 END IF

 '--display
 centerfuz 160, 100, 304, 184, 1, dpage 'backdrop box
 centerbox 84, 18, 140, 16, 4, dpage    'hero name
 centerbox 84, 102, 140, 130, 4, dpage  'stats
 centerbox 236, 75, 80, 78, 4, dpage    'equipment
 edgeprint names(st.who), 84 - LEN(names(st.who)) * 4, 13, uilook(uiText), dpage
 FOR i = 0 TO 11
  stat_caption = ""
  IF st.stat_bonus(i) > 0 THEN stat_caption = stat_caption & "+" & st.stat_bonus(i)
  IF st.stat_bonus(i) < 0 THEN stat_caption = stat_caption & st.stat_bonus(i)
  edgeprint statnames(i) & stat_caption, 20, 42 + i * 10, uilook(uiMenuItem), dpage
  col = uilook(uiMenuItem)
  IF st.stat_bonus(i) < 0 THEN col = uilook(uiDisabledItem)
  IF st.stat_bonus(i) > 0 THEN col = uilook(uiSelectedItem + tog)
  IF gen(genStatCap + i) > 0 THEN
   stat_caption = STR$(small(stat(st.who, 1, i) + st.stat_bonus(i), gen(genStatCap + i)))
  ELSE
   stat_caption = STR$(stat(st.who, 1, i) + st.stat_bonus(i))
  END IF
  edgeprint stat_caption, 148 - LEN(stat_caption) * 8, 42 + i * 10, col, dpage
 NEXT i
 IF st.mode = 0 THEN
  '--main menu display
  FOR i = 0 TO 6
   textcolor uilook(uiMenuItem), uilook(uiHighlight)
   IF i < 5 THEN
    IF eqstuf(st.who, i) = 0 AND st.eq(i).count = 0 THEN textcolor uilook(uiMenuItem), uilook(uiTextBox)
   END IF
   IF st.slot = i THEN
    textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight + tog)
    IF i < 5 THEN
      IF st.eq(i).count = 0 THEN textcolor uilook(uiSelectedItem), uilook(uiHighlight2)
    END IF
   END IF
   printstr menu$(i), 204, 45 + i * 9, dpage
  NEXT i
  IF st.slot < 5 THEN
   centerbox 236, 22, (LEN(m$(st.slot)) + 2) * 8, 16, 4, dpage
   edgeprint m$(st.slot), 236 - (LEN(m$(st.slot)) * 4), 17, uilook(uiText), dpage
  END IF
 END IF
 IF st.mode = 1 THEN
  '--change equipment menu
  centerbox 236, 100, 96, 152, 4, dpage
  FOR i = st.eq_cursor.top TO st.eq_cursor.top + st.eq_cursor.size
   textcolor uilook(uiMenuItem), 0
   IF i = st.eq_cursor.pt THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight2)
   IF i >= st.eq(st.slot).count THEN
    IF i = st.eq(st.slot).count THEN
     '--unequip option
     IF st.slot = 0 THEN
      printstr st.default_weapon_name, 192, 28 + (i - st.eq_cursor.top) * 8, dpage
     ELSE
      printstr st.unequip_caption, 192, 28 + (i - st.eq_cursor.top) * 8, dpage
     END IF
    ELSE
     '--all done!
     EXIT FOR
    END IF
   ELSE
    printstr inventory(st.eq(st.slot).offset(i)).text, 192, 28 + (i - st.eq_cursor.top) * 8, dpage
   END IF
  NEXT i
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 dowait
LOOP
freepage holdscreen
MenuSound gen(genCancelSFX)
evalitemtag
END SUB

SUB equip_menu_setup (BYREF st AS EquipMenuState, menu$())
 st.default_weapon = stat(st.who, 0, 16)
 st.default_weapon_name = rpad(readitemname(st.default_weapon - 1), " ", 11)
 IF LEN(TRIM(st.default_weapon_name)) = 0 THEN
  st.default_weapon_name = st.unequip_caption
 END IF

 setpicstuf buffer(), 200, -1
 FOR i AS INTEGER = 0 TO 4
  menu$(i) = "        "
  IF eqstuf(st.who, i) > 0 THEN
   menu$(i) = rpad(readitemname(eqstuf(st.who, i) - 1), " ", 8)
  END IF
 NEXT i
 
 'erase the tables of equippables
 FOR i AS INTEGER = 0 TO 4
  FOR j AS INTEGER = 0 TO last_inv_slot()
   st.eq(i).offset(j) = -1
  NEXT j
  st.eq(i).count = 0
 NEXT i
 
 DIM itembuf(99) AS INTEGER
 DIM eq_slot AS INTEGER = 0
 FOR i AS INTEGER = 0 TO last_inv_slot()
  IF inventory(i).used THEN
   '--load item data
   loaditemdata itembuf(), inventory(i).id
   eq_slot = itembuf(49) - 1
   IF eq_slot >= 0 THEN
    '--if this item is equipable
    IF readbit(itembuf(), 66, hero(st.who) - 1) THEN
     '--if this item is equipable by this hero
     WITH st.eq(eq_slot)
      .offset(.count) = i
      .count += 1
     END WITH
    END IF
   END IF
  END IF
 NEXT i

END SUB

SUB equip_menu_do_equip(BYVAL item AS INTEGER, BYREF st AS EquipMenuState, menu$())
 unequip st.who, st.slot, st.default_weapon, stat(), 0
 doequip item, st.who, st.slot, st.default_weapon, stat()
 equip_menu_back_to_menu st, menu$()
END SUB

SUB equip_menu_back_to_menu(BYREF st AS EquipMenuState, menu$())
 st.mode = 0
 flusharray st.stat_bonus()
 equip_menu_setup st, menu$()
END SUB

SUB equip_menu_stat_bonus(BYREF st AS EquipMenuState)
 '--load stat bonuses of currently hovered weapon for display

 DIM item AS INTEGER = 0 ' Will be set to item ID + 1

 IF st.eq_cursor.pt = st.eq(st.slot).count THEN
  '--unequip
  IF st.slot = 0 THEN
   '--special handling for weapon
   '--load the default weapon info and continue as normal
   item = st.default_weapon
  ELSE
   'non-weapon unequip sets item to 0 to warn to skip that step
   item = 0
  END IF
 ELSE
  '--equip
  item = inventory(st.eq(st.slot).offset(st.eq_cursor.pt)).id + 1
 END IF

 DIM itembuf(99)
 IF item = 0 THEN
  '--nothing to load!
  flusharray st.stat_bonus()
 ELSE
  loaditemdata itembuf(), item - 1
  FOR i AS INTEGER = 0 TO 11
   st.stat_bonus(i) = itembuf(54 + i)
  NEXT i
 END IF

 IF eqstuf(st.who, st.slot) > 0 THEN
  loaditemdata itembuf(), eqstuf(st.who, st.slot) - 1
  FOR i AS INTEGER = 0 TO 11
   st.stat_bonus(i) = st.stat_bonus(i) - itembuf(54 + i)
  NEXT i
 END IF

 FOR i AS INTEGER = 0 to 11
  IF gen(genStatCap + i) > 0 THEN st.stat_bonus(i) = small(st.stat_bonus(i), gen(genStatCap + i))
 NEXT i

END SUB

SUB items_menu_infostr(istate AS ItemsMenuState, permask() AS INTEGER)
 istate.info = ""
 IF istate.sel >= 0 AND istate.cursor = -1 THEN
  IF inventory(istate.sel).used THEN
   istate.info = readglobalstring(41, "Discard", 10) & " " & inventory(istate.sel).text
   IF readbit(permask(), 0, 3 + istate.sel) THEN istate.info = readglobalstring(42, "Cannot", 10) & " " & istate.info & "!"
  END IF
 END IF
 IF istate.cursor < 0 THEN EXIT SUB
 IF inventory(istate.cursor).used = 0 THEN EXIT SUB
 DIM itemtemp(100) AS INTEGER
 loaditemdata itemtemp(), inventory(istate.cursor).id
 istate.info = readbadbinstring(itemtemp(), 9, 35, 0)
END SUB
