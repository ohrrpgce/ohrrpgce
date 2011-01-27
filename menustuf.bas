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
#include "savegame.bi"

#include "game.bi"
#include "yetmore.bi"
#include "yetmore2.bi"
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
DECLARE SUB items_menu_paint (istate AS ItemsMenuState, iuse() AS INTEGER, permask() AS INTEGER)
DECLARE SUB items_menu_infostr(state AS ItemsMenuState, permask() AS INTEGER)
DECLARE SUB items_menu_autosort(iuse() AS INTEGER, permask() AS INTEGER)
DECLARE SUB item_menu_use_item(BYVAL slot AS INTEGER, istate AS ItemsMenuState, iuse() AS INTEGER, permask() AS INTEGER)
DECLARE FUNCTION menu_attack_targ_picker(BYVAL attack_id AS INTEGER, BYVAL learn_id AS INTEGER, use_caption AS STRING, BYVAL x_offset AS INTEGER=0, BYVAL really_use_attack AS INTEGER=YES) AS INTEGER
DECLARE SUB items_menu_control (istate AS ItemsMenuState, iuse() AS INTEGER, permask() AS INTEGER)
DECLARE SUB spells_menu_refresh_list(sp AS SpellsMenuState)
DECLARE SUB spells_menu_refresh_hero(sp AS SpellsMenuState)
DECLARE SUB spells_menu_control(sp AS SpellsMenuState)
DECLARE SUB spells_menu_paint (BYREF sp AS SpellsMenuState)

REM $STATIC

SUB buystuff (id, shoptype, storebuf())
DIM b((getbinsize(binSTF) \ 2) * 50 - 1), buytype(5, 1) AS STRING, wbuf(dimbinsize(binITM)), walks(15), tradestf(3, 1)
DIM is_equipable AS INTEGER
DIM itembuf(dimbinsize(binITM)) AS INTEGER
DIM hiresprite AS Frame PTR
DIM hirepal AS Palette16 PTR
DIM herosprite(3) AS Frame PTR
DIM heropal(3) AS Palette16 PTR
DIM heroframe AS INTEGER
DIM heropos AS XYPair
DIM room_to_hire AS INTEGER = NO
DIM st AS MenuState
REDIM stuff(-1 TO -1) AS SimpleMenu   ' .dat of each menu item is item index
DIM itemno AS INTEGER   ' always equal to stuff(st.pt).dat
recordsize = curbinsize(binSTF) \ 2 ' get size in INTs

'--Preserve background for display beneath the buy menu
DIM page AS INTEGER
DIM holdscreen AS INTEGER
page = compatpage
holdscreen = allocatepage
copypage page, holdscreen

buytype(0, 0) = readglobalstring$(85, "Trade for", 20) + " "
buytype(0, 1) = readglobalstring$(87, "Joins for", 20) + " "
buytype(1, 0) = readglobalstring$(89, "Cannot Afford", 20) + " "
buytype(1, 1) = readglobalstring$(91, "Cannot Hire", 20) + " "
wepslot$ = readglobalstring$(38, "Weapon", 10)
purchased$ = readglobalstring$(93, "Purchased", 20)
joined$ = readglobalstring$(95, "Joined!", 20)
instock$ = readglobalstring$(97, "in stock", 20)
anda$ = readglobalstring$(81, "and a", 10)
andsome$ = readglobalstring$(153, "and", 10)
eqprefix$ = readglobalstring$(99, "Equip:", 10)
noroom$ = readglobalstring$(100, "No Room in Party", 20)

FOR i = 0 TO 3
 herosprite(i) = frame_load(0, gam.hero(i).battle_pic)
 IF herosprite(i) = 0 THEN debug "Couldn't load hero sprite: " & game & ".pt0#" & gam.hero(i).battle_pic
 heropal(i) = palette16_load(gam.hero(i).battle_pal, 0, gam.hero(i).battle_pic)
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

setshopstock id, recordsize, storebuf(), b()
st.pt = 0
st.size = 15
GOSUB buildmenu
IF st.last = -1 THEN GOTO cleanupquit

price$ = "": xtralines = 0: price2$ = ""
info1$ = "": info2$ = ""
eqinfo$ = ""
showhero = 0
tradingitems = 0
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
 usemenusounds
 IF usemenu(st) THEN GOSUB curinfo
 IF carray(ccMenu) > 1 THEN EXIT DO
 IF carray(ccUse) > 1 THEN '---PRESS ENTER---------------------
  IF stuff(st.pt).enabled THEN '---CHECK TO SEE IF YOU CAN AFFORD IT---
   IF gam.stock(id, itemno) > 1 THEN gam.stock(id, itemno) -= 1
   IF b(itemno * recordsize + 22) THEN setbit tag(), 0, ABS(b(itemno * recordsize + 22)), SGN(SGN(b(itemno * recordsize + 22)) + 1)
   gold = gold - b(itemno * recordsize + 24)
   IF tradingitems THEN '---TRADE IN ITEMS----------
    FOR i = 0 TO 3
     IF tradestf(i, 0) > -1 THEN
      delitem tradestf(i, 0) + 1, tradestf(i, 1)
     END IF
    NEXT
   END IF '-------END TRADE IN ITEM----------------------------
   IF b(itemno * recordsize + 17) = 0 THEN '---BUY ITEM-------------------
    menusound gen(genBuySFX)
    getitem b(itemno * recordsize + 18) + 1, 1
    acol = 4
    alert = 10
    alert$ = purchased$ + " " + stuff(st.pt).text
   END IF '-------END IF ITEM-------------------------------------
   IF b(itemno * recordsize + 17) = 1 THEN '---HIRE HERO------------------
    menusound gen(genHireSFX)
    slot = first_free_slot_in_active_party()
    IF slot >= 0 THEN
     addhero b(itemno * recordsize + 18) + 1, slot, b(itemno * recordsize + 26)
     acol = 4
     alert = 10
     alert$ = stuff(st.pt).text + " " + joined$
    END IF
   END IF '-------END IF HERO-------------------------------------
   'the last thing to do is re-eval the item and hero tags in case
   'something changed
   evalherotag
   evalitemtag
  ELSE ' WHEN CANNOT AFFORD------------------------------------
   menusound gen(genCantBuySFX)
   acol = 3
   alert = 10
   alert$ = buytype(1, shoptype) + stuff(st.pt).text
  END IF '--------END BUY THING------------
  GOSUB buildmenu
  IF st.last = -1 THEN GOTO cleanupquit
  GOSUB curinfo
 END IF '---------END TRY BUY THING--------

 centerbox 80, 94, 150, 168, 1, page
 centerbox 240, 94, 150, 168, 1, page
 '-----RIGHT PANEL------------------------------------------
 temp$ = gold & " " & readglobalstring(32, "Money")
 centerbox 240, 19, LEN(temp$) * 8 + 8, 14, 4, page
 edgeprint temp$, xstring(temp$, 240), 14, uilook(uiText), page
 o = 0
 edgeprint stuff(st.pt).text, xstring(stuff(st.pt).text, 240), 30 + o * 10, uilook(uiMenuItem), page: o = o + 1
 IF info1$ <> "" THEN edgeprint info1$, xstring(info1$, 240), 30 + o * 10, uilook(uiDisabledItem), page: o = o + 1
 IF info2$ <> "" THEN edgeprint info2$, xstring(info2$, 240), 30 + o * 10, uilook(uiDisabledItem), page: o = o + 1
 IF eqinfo$ <> "" THEN edgeprint eqinfo$, xstring(eqinfo$, 240), 30 + o * 10, uilook(uiMenuItem), page: o = o + 1
 IF gam.stock(id, itemno) > 1 THEN
  edgeprint (gam.stock(id, itemno) - 1) & " " & instock$ & " ", xstring((gam.stock(id, itemno) - 1) & " " & instock$ & " ", 240), 30 + o * 10, uilook(uiMenuItem), page: o = o + 1
 END IF
 IF showhero > -1 THEN
  'This happens only if a hireable hero is selected
  centerbox 240, 130, 36, 44, 4, page
  frame_draw(hiresprite + walks(walk), hirepal, 224, 110, 1, -1, page)
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
   edgeboxstyle heropos.x - 1, heropos.y - 2, 34, 44, col, page, , YES
   IF hero(i) > 0 THEN
    'If there is a hero in this slot
    frame_draw(herosprite(i) + heroframe, heropal(i), heropos.x, heropos.y, 1, -1, page)
   END IF
  NEXT i
 END IF
 '-----LEFT PANEL-------------------------------------------
 FOR i = st.top TO small(st.top + st.size, UBOUND(stuff))
  c = uilook(uiMenuItem): IF st.pt = i THEN c = uilook(uiSelectedItem + tog)
  IF stuff(i).enabled = 0 THEN c = uilook(uiDisabledItem): IF st.pt = i THEN c = uilook(uiMenuItem + tog)
  edgeprint stuff(i).text, 10, 15 + (i - st.top) * 10, c, page
 NEXT i
 IF price$ <> "" THEN
  centerbox 160, 187, LEN(price$) * 8 + 8, 14 + xtralines * 10, 1, page
  edgeprint price$, xstring(price$, 160), 182 - xtralines * 5, uilook(uiText), page
  IF xtralines >= 1 THEN edgeprint price2$, xstring(price2$, 160), 187, uilook(uiText), page
 END IF
 IF alert THEN
  alert = alert - 1
  centerbox 160, 178, LEN(alert$) * 8 + 8, 14, acol, page
  edgeprint alert$, xstring(alert$, 160), 173, uilook(uiSelectedItem + tog), page
 END IF
 setvispage vpage
 copypage holdscreen, page
 dowait
LOOP

cleanupquit:
'Unload the sprites used to display the heroes
FOR i = 0 TO 3
 frame_unload(@herosprite(i))
 palette16_unload(@heropal(i))
NEXT i
frame_unload(@hiresprite)
palette16_unload(@hirepal)
freepage page
freepage holdscreen
menusound gen(genCancelSFX)
vishero
EXIT SUB

buildmenu:
'--this figures out if it is okay to buy (or hire) particular stuff.
REDIM stuff(-1 TO -1)
room_to_hire = herocount(3) < 4 ANDALSO free_slots_in_party() > 0
FOR i = 0 TO storebuf(16)
 '--for each shop-thing
 IF gam.stock(id, i) = 1 THEN CONTINUE FOR
 IF b(i * recordsize + 17) = (shoptype XOR 1) THEN CONTINUE FOR
 IF NOT istag(b(i * recordsize + 20), -1) THEN CONTINUE FOR

 DIM itemname as string = readbadbinstring(b(), i * recordsize, 16)
 append_simplemenu_item stuff(), itemname, , , i
 IF b(i * recordsize + 24) > gold THEN stuff(UBOUND(stuff)).enabled = NO
 loadtrades i, tradestf(), b(), recordsize
 FOR j = 0 TO 3
  IF tradestf(j, 0) > -1 THEN
   IF countitem(tradestf(j, 0) + 1) < tradestf(j, 1) THEN stuff(UBOUND(stuff)).enabled = NO
  END IF
 NEXT
 '---PREVENT PARTY OVERFLOW
 IF b(i * recordsize + 17) = 1 THEN
  IF room_to_hire = NO THEN stuff(UBOUND(stuff)).enabled = NO
 END IF
NEXT i
init_menu_state st, stuff()
RETRACE

curinfo:
itemno = stuff(st.pt).dat
tradingitems = 0
xtralines = 0
showhero = -1
is_equipable = NO
price$ = ""
price2$ = ""
eqinfo$ = ""
info1$ = ""
info2$ = ""
IF b(itemno * recordsize + 24) > 0 THEN price$ = b(itemno * recordsize + 24) & " " & readglobalstring(32, "Money")
'--load must trade in item types+amounts
loadtrades itemno, tradestf(), b(), recordsize
FOR i = 0 TO 3
 IF tradestf(i, 0) > -1 THEN
  tradingitems = 1
  IF price$ = "" THEN
   price$ = buytype(0, shoptype)
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
IF b(itemno * recordsize + 17) = 0 THEN
 'This is an item
 loaditemdata itembuf(), b(itemno * recordsize + 18)
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
IF b(itemno * recordsize + 17) = 1 THEN
 'hire
 dim her as herodef
 loadherodata @her, b(itemno * recordsize + 18)
 loaditemdata wbuf(), her.def_weapon
 IF her.def_level < 0 THEN her.def_level = averagelev
 eqinfo$ = (atlevel(her.def_level, her.lev0.hp, her.lev99.hp) + wbuf(54 + 0)) & " " & statnames(statHP)
 showhero = her.sprite
 
 'Load the sprite for the hireable hero
 frame_unload @hiresprite
 hiresprite = frame_load(0, showhero)
 IF hiresprite = 0 THEN debug "Couldn't load hero sprite: " & game & ".pt0#" & showhero
 palette16_unload @hirepal
 hirepal = palette16_load(her.sprite_pal, 0, showhero)
 IF hirepal = 0 THEN debug "Failed to load palette for hireable hero (#" & her.sprite_pal & ")"

 IF room_to_hire = NO THEN info1$ = noroom$
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

FUNCTION chkOOBtarg (target AS INTEGER, atk AS INTEGER) AS INTEGER
'true if valid, false if not valid
'atk id can be -1 for when no attack is relevant
 IF target < 0 OR target > 40 THEN RETURN NO
 IF hero(target) = 0 THEN RETURN NO
 IF atk < -1 OR atk > gen(genMaxAttack) THEN RETURN NO

 DIM hp AS INTEGER
 hp = gam.hero(target).stat.cur.hp

 IF atk >= 0 THEN
  DIM attack AS AttackData
  loadattackdata attack, atk
  IF hp = 0 AND (attack.targ_class = 4 OR attack.targ_class = 10) THEN RETURN YES
  IF hp > 0 AND attack.targ_class = 10 THEN RETURN NO
 END IF

 IF hp = 0 THEN RETURN NO

 RETURN YES
END FUNCTION

SUB doequip (toequip, who, where, defwep)

'--load the item data for this equipment
loaditemdata buffer(), toequip -1

'--apply the stat bonuses
WITH gam.hero(who).stat
 FOR i = 0 TO 11
  'stat bonuses
  .max.sta(i) += buffer(54 + i)
  IF i > 1 THEN .cur.sta(i) = .max.sta(i)
  .cur.sta(i) = small(.cur.sta(i), .max.sta(i))
  IF gen(genStatCap + i) > 0 THEN
   .cur.sta(i) = small(.cur.sta(i), gen(genStatCap + i))
   .max.sta(i) = small(.max.sta(i), gen(genStatCap + i))
  END IF
 NEXT i
END WITH

'--special handling for weapons
IF where = 0 THEN
 gam.hero(who).wep_pic = buffer(52) 'remember weapon pic
 gam.hero(who).wep_pal = buffer(53) 'remember weapon pal
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

FUNCTION getOOBtarg (search_direction AS INTEGER, BYREF target AS INTEGER, atk AS INTEGER, recheck AS INTEGER=NO) AS INTEGER
 '--return true on success, false on failure
 '--atk id can be -1 for when no attack is relevant
 IF recheck THEN target -= 1 ' For a re-check, back the cursor up so if the current target is still valid, it won't change
 DIM safety AS INTEGER = 0
 DO
  target = loopvar(target, 0, 3, search_direction)
  IF chkOOBtarg(target, atk) THEN RETURN YES
  safety += 1
  IF safety >= 4 THEN EXIT DO
 LOOP
 'Failure
 target = -1
 RETURN NO
END FUNCTION

SUB itemmenuswap (invent() AS InventSlot, iuse(), permask(), i, o)
'this sub called from items()
SWAP invent(i), invent(o)

t1 = readbit(iuse(), 0, 3 + i)
t2 = readbit(iuse(), 0, 3 + o)
setbit iuse(), 0, 3 + i, t2
setbit iuse(), 0, 3 + o, t1
t1 = readbit(permask(), 0, 3 + i)
t2 = readbit(permask(), 0, 3 + o)
setbit permask(), 0, 3 + i, t2
setbit permask(), 0, 3 + o, t1
END SUB

SUB update_inventory_caption (i)
IF inventory(i).used = 0 THEN
 inventory(i).text = SPACE$(11)
ELSE
 inventory(i).text = readitemname$(inventory(i).id)
 inventory(i).text = rpad(inventory(i).text, " ", 8) + CHR$(1) + RIGHT$(XSTR$(inventory(i).num), 2)
END IF
END SUB

SUB oobcure (w, t, atk, spred)
'--outside-of-battle cure

DIM st(13, 1)

'--average stats for item-triggered spells
IF w = -1 THEN
 j = 0
 FOR o = 0 TO 3
  IF hero(o) > 0 THEN
   j = j + 1
   FOR i = 0 TO 11
    st(i, 0) = st(i, 0) + gam.hero(o).stat.cur.sta(i)
    st(i, 1) = st(i, 1) + gam.hero(o).stat.max.sta(i)
   NEXT i
  END IF
 NEXT o
 FOR i = 0 TO 11
  st(i, 0) = st(i, 0) / j
  st(i, 1) = st(i, 1) / j
 NEXT i
ELSE
 FOR i = 0 TO 11
  st(i, 0) = gam.hero(w).stat.cur.sta(i)
  st(i, 1) = gam.hero(w).stat.max.sta(i)
 NEXT i
END IF

DIM attack AS AttackData
loadattackdata attack, atk

'--out of battle attacks that target stats other than HP and MP
'--always affect the max stat, so force exceed_maximum on
IF attack.targ_stat > 1 THEN
 attack.allow_cure_to_exceed_maximum = YES
END IF

'--out of battle attacks aren't allowed to miss.
attack.aim_math = 3

DIM AS BattleSprite attacker, target

'--populate attacker object
IF w = -1 THEN '--use average stats
 FOR i AS INTEGER = 0 to 11
  attacker.stat.cur.sta(i) = st(i, 0)
  attacker.stat.max.sta(i) = st(i, 1)
 NEXT i
ELSE '--use actual stats
 FOR i AS INTEGER = 0 to 11
  attacker.stat.cur.sta(i) = gam.hero(w).stat.cur.sta(i)
  attacker.stat.max.sta(i) = gam.hero(w).stat.max.sta(i)
 NEXT i
END IF

'--populate the target object
FOR i AS INTEGER = 0 to 11
 target.stat.cur.sta(i) = gam.hero(t).stat.cur.sta(i)
 target.stat.max.sta(i) = gam.hero(t).stat.max.sta(i)
NEXT i
calc_hero_elementals target.elementaldmg(), t

inflict(0, 1, attacker, target, attack, spred)

'--copy back stats that need copying back
'--first copy HP and MP normally
FOR i AS INTEGER = 0 to 1
 gam.hero(t).stat.cur.sta(i) = target.stat.cur.sta(i)
 gam.hero(t).stat.max.sta(i) = target.stat.max.sta(i)
NEXT i
'--Then update just the max for the other stats
'--this kinda sucks but it is consistent with the way outside of battle cure has always worked
FOR i AS INTEGER = 2 to 11
 gam.hero(t).stat.max.sta(i) = target.stat.cur.sta(i)
 gam.hero(t).stat.cur.sta(i) = gam.hero(t).stat.max.sta(i)
NEXT i

'Sound effect
MenuSound attack.sound_effect

'--TODO: Must add the attack-tag conditional stuff.

END SUB

SUB patcharray (array(), n$)

DIM num(2) as string, hexk(15)

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
 num(0) = n$ & "(" & ABS(pt) & ")"
 num(1) = "value = " & array(pt)
 num(2) = ""
 FOR i = 0 TO 15
  IF readbit(array(), pt, i) THEN
   num(2) = num(2) + "1"
  ELSE
   num(2) = num(2) + "0"
  END IF
 NEXT i
 clearpage dpage
 edgeprint "DEBUG MODE", 120, 50, uilook(uiText), dpage
 centerbox 160, 100, 140, 60, 1, dpage
 FOR i = 0 TO 2
  IF i = csr THEN c = uilook(uiSelectedItem + tog) ELSE c = uilook(uiMenuItem)
  edgeprint num(i), 160 - LEN(num(i)) * 4, 80 + i * 10, c, dpage
 NEXT i
 edgeprint "0123456789ABCDEF", 96, 110, uilook(uiSelectedDisabled), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

END SUB

FUNCTION picksave (loading as integer) as integer

DIM mapname(3) AS STRING, lev(3) AS STRING, confirm(1) AS STRING, menu(1) AS STRING
DIM sprites(3, 3) AS GraphicPair
DIM st AS MenuState

st.first = -1
st.last = 3
st.size = 4

'--loading 0 is the save menu, 1 is load menu, and 2 is load with no titlescreen. it fades the screen in
'--loading 0+1 use vpage as background, loading 2 uses none. pages 2 and 3 are preserved
'--terribly sorry for the dirtyness
needf = 0
IF loading = 2 THEN
 needf = 2
END IF

'--load strings. menu array holds the names of the options
'--at the top of the screeen (only one appears when saving)

IF loading THEN
 st.pt = 0
 menu(0) = readglobalstring$(52, "New Game", 10)
 menu(1) = readglobalstring$(53, "Exit", 10)
ELSE
 st.pt = lastsaveslot - 1
 confirm(0) = readglobalstring$(44, "Yes", 10)
 confirm(1) = readglobalstring$(45, "No", 10)
 menu(0) = readglobalstring$(59, "CANCEL", 10)
 replacedat$ = readglobalstring$(102, "Replace Old Data?", 20)
 menuwidth = 8 * large(LEN(confirm(0)), LEN(confirm(1)))
END IF

DIM holdscreen AS INTEGER
DIM page AS INTEGER
page = compatpage
holdscreen = allocatepage
IF loading < 2 THEN
 '--preserve background for display beneath the save/load picker
 copypage page, holdscreen
END IF
'otherwise, holdscreen is black

DIM pv(3) AS SaveSlotPreview
FOR i = 0 TO 3
 get_save_slot_preview i, pv(i)
 IF pv(i).valid THEN
  mapname(i) = getmapname(pv(i).cur_map)
  '--leader level
  lev(i) = readglobalstring(43, "Level", 10) & " " & pv(i).leader_lev
  FOR o = 0 TO 3
   '--hero pic and palette
   IF pv(i).hero_id(o) > 0 THEN
    sprites(i, o).sprite = frame_load(0, pv(i).hero(o).battle_pic)
    sprites(i, o).pal = palette16_load(pv(i).hero(o).battle_pal, 0, pv(i).hero(o).battle_pic)
   END IF
  NEXT o
 END IF
NEXT i

IF loading THEN
 'check for no slots
 nofull = YES
 FOR i = 0 TO 3
  IF pv(i).valid THEN nofull = NO
 NEXT i
 IF nofull = YES THEN
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

 'Make menu position -2 appear as -1 to usemenu
 DIM temppt as integer = iif(st.pt = -2, -1, st.pt)
 usemenusounds
 IF usemenu(temppt, st.top, st.first, st.last, st.size) THEN
  st.pt = temppt
 END IF
 IF st.pt < 0 AND loading THEN
  IF carray(ccLeft) > 1 THEN st.pt = -1: MenuSound gen(genCursorSFX)
  IF carray(ccRight) > 1 THEN st.pt = -2: MenuSound gen(genCursorSFX)
 END IF

 IF carray(ccUse) > 1 THEN
  IF st.pt = -2 THEN
   MenuSound gen(genCancelSFX)
   picksave = st.pt
   EXIT DO
  ELSEIF st.pt = -1 THEN
   MenuSound gen(genAcceptSFX)
   picksave = st.pt
   EXIT DO
  ELSE
   allow = 1
   IF loading THEN
    '--normal load of an existing save
    IF pv(st.pt).valid = 0 THEN allow = 0
   ELSE
    '--normal save in a slot
    IF pv(st.pt).valid THEN GOSUB confirm
   END IF
   IF allow = 1 THEN
    MenuSound gen(genAcceptSFX)
    picksave = st.pt
    lastsaveslot = st.pt + 1
    EXIT DO
   ELSE
    MenuSound gen(genCancelSFX)
   END IF
  END IF
 END IF
 GOSUB drawmenugosub
 setvispage vpage
 copypage holdscreen, page
 IF needf = 1 THEN   'the titlescreen might be skipped and with it the fading in
  needf = 0
  fadein
 END IF
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP

freesprites:
freepage page
freepage holdscreen
FOR t = 4 TO 5: carray(t) = 0: NEXT t
FOR i = 0 TO 3
 FOR o = 0 TO 3
  frame_unload(@sprites(i, o).sprite)
  palette16_unload(@sprites(i, o).pal)
 NEXT
NEXT
EXIT FUNCTION

confirm:
allow = 0
MenuSound gen(genAcceptSFX)
confirmboxY = 14 + (44 * st.pt)
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

 copypage holdscreen, page
 IF carray(ccUse) > 1 THEN RETRACE
 GOSUB drawmenugosub
 centerbox 160, confirmboxY, 40 + (LEN(replacedat$) * 8) + menuwidth, 24, 3, page
 edgeprint replacedat$, 200 - (LEN(replacedat$) * 8), confirmboxY - 5, uilook(uiText), page
 FOR i = 0 TO 1
  col = uilook(uiSelectedItem + tog): IF allow = i THEN col = uilook(uiMenuItem)
  edgeprint confirm(i), 216, confirmboxY - 9 + (i * 9), col, page
 NEXT i
 setvispage vpage
 dowait
LOOP

drawmenugosub:
centerbox 50, 11, 80, 14, 15, page
IF loading THEN centerbox 270, 11, 80, 14, 15, page
FOR i = 0 TO 3
 centerbox 160, 44 + i * 44, 310, 42, 15, page
NEXT i
'load and save menus enjoy different colour schemes
IF loading THEN activec = 2 ELSE activec = 1
SELECT CASE st.pt
 CASE -2
  centerbox 270, 11, 82, 16, activec, page
 CASE -1
  centerbox 50, 11, 82, 16, activec, page
 CASE ELSE
  centerbox 160, 44 + st.pt * 44, 312, 44, activec, page
END SELECT
FOR i = 0 TO 3
 IF pv(i).valid THEN
  FOR o = 0 TO 3
   IF sprites(i, o).sprite THEN
    frame_draw sprites(i, o).sprite + iif(st.pt = i, walk, 0), sprites(i, o).pal, 140 + (o * 42), 24 + i * 44, 1, -1, page
   END IF
  NEXT o
  col = uilook(uiMenuItem)
  IF st.pt = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint pv(i).leader_name, 14, 25 + i * 44, col, page
  edgeprint lev(i), 14, 34 + i * 44, col, page
  edgeprint pv(i).playtime, 14, 43 + i * 44, col, page
  edgeprint mapname(i), 14, 52 + i * 44, col, page
 END IF
NEXT i
col = uilook(uiMenuItem): IF st.pt = -1 THEN col = uilook(uiSelectedItem + tog)
edgeprint menu(0), xstring(menu(0), 50), 6, col, page
IF loading THEN
 col = uilook(uiMenuItem): IF st.pt = -2 THEN col = uilook(uiSelectedItem + tog)
 edgeprint menu(1), xstring(menu(1), 270), 6, col, page
END IF
RETRACE

END FUNCTION

SUB sellstuff (id, storebuf())
DIM b((getbinsize(binSTF) \ 2) * 50 - 1), permask(15), price((inventoryMax + 1) \ 3)
recordsize = curbinsize(binSTF) \ 2 ' get size in INTs

'--preserve background for display under sell menu
DIM page AS INTEGER
DIM holdscreen AS INTEGER
page = compatpage
holdscreen = allocatepage
copypage page, holdscreen

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
 centerbox 160, 92, 304, 176, 1, page
 FOR i = top TO top + 62
  textcolor uilook(uiMenuItem), 0
  IF readbit(permask(), 0, i) THEN textcolor uilook(uiDisabledItem), 0
  IF ic = i THEN
   textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight2)
   IF readbit(permask(), 0, i) THEN textcolor uilook(uiGold), uilook(uiHighlight2)
  END IF
  printstr inventory(i).text, 20 + 96 * (i MOD 3), 12 + 8 * ((i - top) \ 3), page
 NEXT i
 centerfuz 160, 180, 312, 20, 4, page
 edgeprint info$, xstring(info$, 160), 175, uilook(uiText), page
 edgeprint gold & " " & readglobalstring(32, "Money"), 310 - LEN(gold & " " & readglobalstring(32, "Money")) * 8, 1, uilook(uiGold), page
 IF alert THEN
  alert = alert - 1
  centerbox 160, 178, LEN(alert$) * 8 + 8, 14, 4, page
  edgeprint alert$, xstring(alert$, 160), 173, uilook(uiSelectedItem + tog), page
 END IF
 setvispage vpage
 copypage holdscreen, page
 dowait
LOOP
freepage page
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

'Format one of the strings on the second Status menu screen
FUNCTION hero_elemental_resist_msg (element AS STRING, damage AS SINGLE) AS STRING
 DIM raw AS STRING
 IF ABS(damage) < 0.000005 THEN
  raw = readglobalstring(168, "Immune to $E", 30)
 ELSEIF damage < 0.0 THEN
  raw = readglobalstring(171, "Absorbs $A damage from $E", 30)
 ELSEIF damage < 1.0 THEN
  raw = readglobalstring(165, "$D damage from $E", 30)
 ELSEIF damage > 1.0 THEN
  raw = readglobalstring(162, "$D damage from $E", 30)
 END IF
 'No message for 100% damage
 replacestr raw, "$E", element
 replacestr raw, "$D", format_percent(damage, 3)
 replacestr raw, "$X", format_percent(damage - 1.0, 3)
 replacestr raw, "$R", format_percent(1.0 - damage, 3)
 replacestr raw, "$A", format_percent(-damage, 3)
 RETURN raw
END FUNCTION

SUB status (pt)
DIM mtype(5)
DIM her AS HeroDef
DIM portrait AS GraphicPair
DIM page AS INTEGER
DIM holdscreen AS INTEGER

DIM exper_caption AS STRING = readglobalstring(33, "Experience", 10)
DIM level_caption AS STRING = readglobalstring(43, "Level", 10)
DIM level_mp_caption AS STRING = readglobalstring(160, "Level MP", 20)

DIM elementalmenu() AS STRING
DIM elementalmenu_st AS MenuState
DIM elementalmenu_scrollrect AS RectType
WITH elementalmenu_scrollrect
 .x = 14
 .y = 60
 .wide = 292
 .high = 120
END WITH

DIM elementnames() AS STRING
getelementnames elementnames()

DIM elementaldmg(maxElements - 1) AS SINGLE

mode = 0

GOSUB buildmenu
'--Preserve background for display under status menu
page = compatpage
holdscreen = allocatepage
copypage page, holdscreen

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
 IF carray(ccLeft) > 1 THEN DO: pt = loopvar(pt, 0, 3, -1): LOOP UNTIL hero(pt) > 0: menusound gen(genCursorSFX): GOSUB buildmenu
 IF carray(ccRight) > 1 THEN DO: pt = loopvar(pt, 0, 3, 1): LOOP UNTIL hero(pt) > 0: menusound gen(genCursorSFX): GOSUB buildmenu

 centerfuz 160, 100, 304, 184, 1, page
 centerbox 160, 36, 292, 40, 4, page
 SELECT CASE mode
  CASE 0
   centerbox 84, 120, 140, 120, 4, page
   centerbox 236, 120, 140, 120, 4, page
  CASE 1, 2
   centerbox 160, 120, 292, 120, 4, page
 END SELECT
 IF her.portrait >= 0 THEN
  edgeboxstyle 262, 8, 50, 50, 3, page
  frame_draw portrait.sprite, portrait.pal, 262, 8,,,page
 END IF

 '--name
 edgeprint names(pt), 142 - LEN(names(pt)) * 4, 20, uilook(uiText), page
 '--level and experience
 edgeprint level_caption & " " & gam.hero(pt).lev, 142 - LEN(level_caption & " " & gam.hero(pt).lev) * 4, 30, uilook(uiText), page
 IF gam.hero(pt).lev < gen(genLevelCap) THEN
  'Can't level further, so hide experience required
  temp$ = (exlev(pt, 1) - exlev(pt, 0)) & " " & exper_caption & " " & readglobalstring$(47, "for next", 10) & " " & level_caption
  edgeprint temp$, 142 - LEN(temp$) * 4, 40, uilook(uiText), page
 END IF

 SELECT CASE mode
  CASE 0
   '--show stats
   FOR i = 0 TO 9
    edgeprint statnames(i + 2), 20, 62 + i * 10, uilook(uiText), page
    temp$ = STR(gam.hero(pt).stat.cur.sta(i + 2))
    edgeprint temp$, 148 - LEN(temp$) * 8, 62 + i * 10, uilook(uiText), page
   NEXT i

   'current/max HP
   edgeprint statnames(statHP), 236 - LEN(statnames(statHP)) * 4, 65, uilook(uiText), page
   temp$ = STR$(ABS(gam.hero(pt).stat.cur.hp)) + "/" + STR$(ABS(gam.hero(pt).stat.max.hp))
   edgeprint temp$, 236 - LEN(temp$) * 4, 75, uilook(uiText), page

   '--MP and level MP
   FOR i = 0 TO 5
    IF mtype(i) = 0 THEN
     edgeprint statnames(statMP), 236 - LEN(statnames(statMP)) * 4, 95, uilook(uiText), page
     temp$ = STR$(ABS(gam.hero(pt).stat.cur.mp)) + "/" + STR$(ABS(gam.hero(pt).stat.max.mp))
     edgeprint temp$, 236 - LEN(temp$) * 4, 105, uilook(uiText), page
    END IF
    IF mtype(i) = 1 THEN
     edgeprint level_mp_caption, 236 - LEN(level_mp_caption) * 4, 125, uilook(uiText), page
     temp$ = ""
     FOR o = 0 TO 3
      temp$ = temp$ + STR$(ABS(lmp(pt, o))) + "/"
     NEXT o
     temp$ = LEFT$(temp$, LEN(temp$) - 1)
     edgeprint temp$, 236 - LEN(temp$) * 4, 135, uilook(uiText), page
     temp$ = ""
     FOR o = 4 TO 7
      temp$ = temp$ + STR$(ABS(lmp(pt, o))) + "/"
     NEXT o
     temp$ = LEFT$(temp$, LEN(temp$) - 1)
     edgeprint temp$, 236 - LEN(temp$) * 4, 145, uilook(uiText), page
    END IF
   NEXT i

   '--gold
   edgeprint gold & " " & readglobalstring(32, "Money"), 236 - LEN(gold & " " & readglobalstring(32, "Money")) * 4, 167, uilook(uiGold), page
  CASE 1
   '--show elementals

   WITH elementalmenu_st
    usemenusounds
    scrollmenu elementalmenu_st

    draw_scrollbar elementalmenu_st, elementalmenu_scrollrect, , page
    FOR i = 0 TO .size
     IF .top + i <= .last THEN
      edgeprint elementalmenu(.top + i), 20, 64 + i * 10, uilook(uiText), page
     END IF
    NEXT i
   END WITH

  CASE 2
   '--tigger rename
   IF gam.hero(pt).rename_on_status THEN
    '--status-screen rename is allowed
    renamehero pt, YES
    IF carray(ccMenu) > 1 THEN EXIT DO
    mode = 0
   END IF

 END SELECT

 setvispage vpage
 copypage holdscreen, page
 dowait
LOOP
menusound gen(genCancelSFX)
IF portrait.sprite THEN frame_unload @portrait.sprite
IF portrait.pal    THEN palette16_unload @portrait.pal
freepage page
freepage holdscreen
FOR t = 4 TO 5
 carray(t) = 0
NEXT t
EXIT SUB

buildmenu: '--loads the hero whose slot is held in pt
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

'--get this hero's elemental resists, with worn equipment
calc_hero_elementals elementaldmg(), pt

'--build elemental strings
REDIM elementalmenu(-1 TO -1)
DIM msg AS STRING = readglobalstring$(302, "Elemental Effects:", 30)
IF LEN(msg) THEN str_array_append elementalmenu(), msg
FOR i = 0 TO gen(genNumElements) - 1
 msg = hero_elemental_resist_msg(elementnames(i), elementaldmg(i))
 IF LEN(msg) THEN str_array_append elementalmenu(), msg
NEXT

'Well, if you set some blank global text strings, you could get this message
'even if not everything is level, but that's a bonus.
IF UBOUND(elementalmenu) = 0 THEN elementalmenu(0) = readglobalstring$(130, "No Elemental Effects", 30)

WITH elementalmenu_st
 .last = UBOUND(elementalmenu)
 .size = 10
END WITH

IF portrait.sprite THEN frame_unload @portrait.sprite
IF portrait.pal    THEN palette16_unload @portrait.pal
IF her.portrait >= 0 THEN
 portrait.sprite = frame_load(8, her.portrait)
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

SUB unequip (who, where, defwep, resetdw)

'--exit if nothing is equiped
IF eqstuf(who, where) = 0 THEN EXIT SUB

'--load the item data for the thing we are unequiping
loaditemdata buffer(), eqstuf(who, where) - 1

'--remove stat bonuses
WITH gam.hero(who).stat
 FOR i = 0 TO 11
  .max.sta(i) = .max.sta(i) - buffer(54 + i)
  '--for non HP non MP stats, reset current to max
  IF i > 1 THEN .cur.sta(i) = .max.sta(i)
  '--prevent negatives
  .cur.sta(i) = small(.cur.sta(i), .max.sta(i))
 NEXT i
END WITH

'--return item to inventory (if not the default weapon)
IF where = 0 AND eqstuf(who, where) = defwep THEN
ELSE
 getitem eqstuf(who, where), 1
END IF

'--blank out equipment
eqstuf(who, where) = 0

IF where = 0 AND resetdw THEN
 '--restore default weapon
 doequip defwep, who, where, defwep
END IF

END SUB

'======== FIXME: move this up as code gets cleaned up ===========
OPTION EXPLICIT

SUB loadshopstuf (array() AS INTEGER, BYVAL id AS INTEGER)
 DIM ol AS INTEGER = getbinsize(binSTF) \ 2 'old size on disk
 DIM nw AS INTEGER = curbinsize(binSTF) \ 2 'new size in memory
 flusharray array(), nw * 50 - 1, 0
 'load shop data from STF lump
 setpicstuf buffer(), ol * 2 * 50, -1
 loadset game + ".stf", id, 0
 'in case shop data has been resized, scale records to new size
 FOR i AS INTEGER = 0 TO ol - 1
  FOR o AS INTEGER = 0 to 49
   array(o * nw + i) = buffer(o * ol + i)
  NEXT o
 NEXT i
END SUB

FUNCTION count_available_spells(who AS INTEGER, list AS INTEGER) AS INTEGER
 DIM i AS INTEGER
 DIM n AS INTEGER = 0
 FOR i = 0 to 23
  IF spell(who, list, i) > 0 THEN n + = 1
 NEXT i
 RETURN n
END FUNCTION

FUNCTION outside_battle_cure (atk AS INTEGER, target AS INTEGER, attacker AS INTEGER, spread AS INTEGER) AS INTEGER
 DIM i AS INTEGER
 DIM didcure AS INTEGER = NO
 IF spread = 0 THEN
  IF chkOOBtarg(target, atk) THEN oobcure attacker, target, atk, spread : didcure = YES
 ELSE
  FOR i = 0 TO 3
   IF chkOOBtarg(i, atk) THEN oobcure attacker, i, atk, spread : didcure = YES
  NEXT i
 END IF
 IF didcure THEN
  're-check validify of target
  getOOBtarg 1, target, atk, YES
 END IF
 RETURN didcure
END FUNCTION

SUB equip (who)

'--dim stuff
DIM m(4) AS STRING, menu(6) AS STRING
DIM page AS INTEGER = compatpage
DIM holdscreen = allocatepage
DIM st AS EquipMenuState

DIM i AS INTEGER
DIM tog AS INTEGER = 0
DIM stat_caption AS STRING
DIM col AS INTEGER
DIM item_id AS INTEGER

'--get names
m(0) = readglobalstring$(38, "Weapon", 10)
FOR i = 0 TO 3
 m(i + 1) = readglobalstring(25 + i, "Armor" & i+1)
NEXT i
menu(5) = rpad(readglobalstring$(39, "-REMOVE-", 8), " ", 8)
menu(6) = rpad(readglobalstring$(40, "-EXIT-", 8), " ", 8)

'--initialize
WITH st
 .mode = 0
 .who = who
 .eq_cursor.size = 17
 .default_weapon = 0
 .default_weapon_name = ""
 .unequip_caption = rpad(readglobalstring$(110, "Nothing", 10), " ", 11)
END WITH
equip_menu_setup st, menu()

'--prepare the backdrop
'preserve the background behind the equip menu
copypage page, holdscreen

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
   equip_menu_setup st, menu()
   MenuSound gen(genCursorSFX)
  END IF
  IF carray(ccRight) > 1 THEN 'Right: next hero
   DO: st.who = loopvar(st.who, 0, 3, 1): LOOP UNTIL hero(st.who) > 0
   equip_menu_setup st, menu()
   MenuSound gen(genCursorSFX)
  END IF
  usemenusounds
  usemenu st.slot, 0, 0, 6, 6

  IF carray(ccUse) > 1 THEN
   IF st.slot < 5 THEN
    '--change equipment
    IF st.eq(st.slot).count > 0 OR eqstuf(st.who, st.slot) > 0 THEN
     '--switch to change equipment mode
     st.mode = 1
     st.eq_cursor.pt = 0
     st.eq_cursor.top = 0
     'Number of options = num equippable things + nothing/unequip option
     st.eq_cursor.last = (st.eq(st.slot).count + 1) - 1
     equip_menu_stat_bonus st
     MenuSound gen(genAcceptSFX)
    END IF
    'UPDATE ITEM POSSESSION BITSETS
    evalitemtag
   END IF
   IF st.slot = 5 THEN
    MenuSound gen(genCancelSFX)
    '--unequip all
    FOR i AS INTEGER = 0 TO 4
     unequip st.who, i, st.default_weapon, 1
    NEXT i
    equip_menu_setup st, menu()
    'UPDATE ITEM POSSESSION BITSETS
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
  usemenusounds
  IF usemenu(st.eq_cursor) THEN
   equip_menu_stat_bonus st
  END IF
  IF carray(ccUse) > 1 THEN
   IF st.eq_cursor.pt = st.eq(st.slot).count THEN
    '--unequip
    unequip st.who, st.slot, st.default_weapon, 1
    equip_menu_back_to_menu st, menu()
    MenuSound gen(genCancelSFX)
   ELSE
    '--normal equip
    item_id = inventory(st.eq(st.slot).offset(st.eq_cursor.pt)).id
    equip_menu_do_equip item_id + 1, st, menu()
    MenuSound gen(genAcceptSFX)
   END IF
  END IF
 END IF

 '--display
 centerfuz 160, 100, 304, 184, 1, page 'backdrop box
 centerbox 84, 18, 140, 16, 4, page    'hero name
 centerbox 84, 102, 140, 130, 4, page  'stats
 centerbox 236, 75, 80, 78, 4, page    'equipment
 edgeprint names(st.who), 84 - LEN(names(st.who)) * 4, 13, uilook(uiText), page
 FOR i = 0 TO 11
  stat_caption = ""
  IF st.stat_bonus(i) > 0 THEN stat_caption = stat_caption & "+" & st.stat_bonus(i)
  IF st.stat_bonus(i) < 0 THEN stat_caption = stat_caption & st.stat_bonus(i)
  edgeprint statnames(i) & stat_caption, 20, 42 + i * 10, uilook(uiMenuItem), page
  col = uilook(uiMenuItem)
  IF st.stat_bonus(i) < 0 THEN col = uilook(uiDisabledItem)
  IF st.stat_bonus(i) > 0 THEN col = uilook(uiSelectedItem + tog)
  IF gen(genStatCap + i) > 0 THEN
   stat_caption = STR$(small(gam.hero(st.who).stat.max.sta(i) + st.stat_bonus(i), gen(genStatCap + i)))
  ELSE
   stat_caption = STR$(gam.hero(st.who).stat.max.sta(i) + st.stat_bonus(i))
  END IF
  edgeprint stat_caption, 148 - LEN(stat_caption) * 8, 42 + i * 10, col, page
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
   printstr menu(i), 204, 45 + i * 9, page
  NEXT i
  IF st.slot < 5 THEN
   centerbox 236, 22, (LEN(m(st.slot)) + 2) * 8, 16, 4, page
   edgeprint m(st.slot), 236 - (LEN(m(st.slot)) * 4), 17, uilook(uiText), page
  END IF
 END IF
 IF st.mode = 1 THEN
  '--change equipment menu
  centerbox 236, 100, 96, 152, 4, page
  FOR i = st.eq_cursor.top TO st.eq_cursor.top + st.eq_cursor.size
   textcolor uilook(uiMenuItem), 0
   IF i = st.eq_cursor.pt THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight2)
   IF i < st.eq(st.slot).count THEN
    printstr inventory(st.eq(st.slot).offset(i)).text, 192, 28 + (i - st.eq_cursor.top) * 8, page
   ELSEIF i = st.eq(st.slot).count THEN
    '--unequip option
    IF st.slot = 0 THEN
     printstr st.default_weapon_name, 192, 28 + (i - st.eq_cursor.top) * 8, page
    ELSE
     printstr st.unequip_caption, 192, 28 + (i - st.eq_cursor.top) * 8, page
    END IF
   ELSE
    '--all done!
    EXIT FOR
   END IF
  NEXT i
 END IF
 setvispage vpage
 copypage holdscreen, page
 dowait
LOOP
freepage page
freepage holdscreen
MenuSound gen(genCancelSFX)
evalitemtag
END SUB

SUB equip_menu_setup (BYREF st AS EquipMenuState, menu$())
 st.default_weapon = gam.hero(st.who).def_wep
 st.default_weapon_name = rpad(readitemname(st.default_weapon - 1), " ", 11)
 IF LEN(TRIM(st.default_weapon_name)) = 0 THEN
  st.default_weapon_name = st.unequip_caption
 END IF

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
 
 DIM itembuf(dimbinsize(binITM)) AS INTEGER
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
 unequip st.who, st.slot, st.default_weapon, 0
 doequip item, st.who, st.slot, st.default_weapon
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

 DIM itembuf(dimbinsize(binITM))
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
  'FIXME: This should take the current stats into account to decide
  'what the stat cap caps the bonus to
  IF gen(genStatCap + i) > 0 THEN st.stat_bonus(i) = small(st.stat_bonus(i), gen(genStatCap + i))
 NEXT i

END SUB

FUNCTION items_menu () as integer
 DIM istate AS ItemsMenuState
 WITH istate
  .trigger_box = -1
  .cursor = -3
  .top = -3
  .sel = -4
  .info = ""
  .re_use = NO
 END WITH

 DIM itemtemp(dimbinsize(binITM)) AS INTEGER
 DIM iuse((inventoryMax + 3) / 16) AS INTEGER 'bit 0 of iuse, permask, correspond to item -3
 DIM permask((inventoryMax + 3) / 16) AS INTEGER

 istate.special(-3) = rpad(readglobalstring(35, "DONE", 10), " ", 11)
 istate.special(-2) = rpad(readglobalstring(36, "AUTOSORT", 10), " ", 11)
 istate.special(-1) = rpad(readglobalstring(37, "TRASH", 10), " ", 11)

 '--Preserve background for display beneath the item menu
 DIM holdscreen AS INTEGER
 istate.page = compatpage
 holdscreen = allocatepage
 copypage istate.page, holdscreen

 DIM i AS INTEGER
 
 FOR i = 0 TO 2
  setbit iuse(), 0, i, 1
 NEXT i
 FOR i = 0 TO last_inv_slot()
  IF inventory(i).used THEN
   loaditemdata itemtemp(), inventory(i).id
   IF itemtemp(73) = 2 THEN setbit permask(), 0, 3 + i, 1
   IF itemtemp(50) > 0 THEN '--teach spell
    setbit iuse(), 0, 3 + i, 1
   ELSEIF itemtemp(51) > 0 THEN
    setbit iuse(), 0, 3 + i, 1
   ELSEIF itemtemp(51) < 0 THEN
    setbit iuse(), 0, 3 + i, 1
   END IF
  END IF
 NEXT i

 WITH istate.rect
  .x = 8
  .y = 5
  .wide = 304
  .high = small(180, 12 + (CINT((last_inv_slot() + 1) / 3) + 1) * 8)
 END WITH
 WITH istate.scrollrect
  .x = 20
  .y = 12
  .wide = 287
  .high = 168
 END WITH
 WITH istate.scroll
  .first = -1
  .last = INT(last_inv_slot() / 3)
  .size = 20
 END WITH

 items_menu_infostr istate, permask()
 
 DIM wtogl AS INTEGER = 0
 menusound gen(genAcceptSFX)

 setkeys
 DO
  setwait speedcontrol
  setkeys
  istate.tog = istate.tog XOR 1
  wtogl = loopvar(wtogl, 0, 3, 1)
  playtimer
 
  control
  items_menu_control istate, iuse(), permask()
  IF istate.trigger_box >= 0 THEN
   '--return the box number to trigger
   items_menu = istate.trigger_box
   EXIT DO
  END IF
  IF istate.quit THEN
   menusound gen(genCancelSFX)
   EXIT DO
  END IF
  IF istate.refresh THEN
   items_menu_infostr istate, permask()
  END IF
  
  items_menu_paint istate, iuse(), permask()
  
  setvispage vpage
  copypage holdscreen, istate.page
  IF istate.re_use = NO THEN
   dowait
  END IF
 LOOP
 carray(ccUse) = 0
 carray(ccMenu) = 0
 freepage istate.page
 freepage holdscreen
END FUNCTION

SUB items_menu_paint (istate AS ItemsMenuState, iuse() AS INTEGER, permask() AS INTEGER)
 edgeboxstyle istate.rect.x, istate.rect.y, istate.rect.wide, istate.rect.high, 0, istate.page
 DIM display AS STRING
 FOR i AS INTEGER = istate.top TO small(istate.top + 62, last_inv_slot())
  textcolor uilook(uiDisabledItem), 0
  IF readbit(iuse(), 0, 3 + i) = 1 THEN textcolor uilook(uiMenuItem), 0
  IF readbit(permask(), 0, 3 + i) THEN textcolor uilook(uiSelectedDisabled), 0
  IF istate.cursor = i THEN
   textcolor uilook(uiMenuItem), uilook(uiHighlight2)
   IF readbit(iuse(), 0, 3 + i) = 1 THEN textcolor uilook(uiText), uilook(uiHighlight2)
   IF readbit(permask(), 0, 3 + i) THEN textcolor uilook(uiGold), uilook(uiHighlight2)
  END IF
  IF istate.sel = i THEN
   textcolor uilook(uiMenuItem), uilook(uiHighlight + istate.tog)
   IF istate.cursor = i THEN textcolor uilook(uiSelectedItem + istate.tog), uilook(uiHighlight + istate.tog)
  END IF
  IF i >= 0 THEN
   display = inventory(i).text
  ELSE
   display = istate.special(i)
  END IF
  printstr display, 20 + 96 * ((i + 3) MOD 3), 12 + 8 * ((i - istate.top) \ 3), istate.page
 NEXT i
 centerfuz 160, 192, 312, 16, 4, istate.page
 edgeprint istate.info, xstring(istate.info, 160), 187, uilook(uiText), istate.page
 WITH istate.scroll
  .top = INT(istate.top / 3)
  .pt = INT(istate.cursor / 3)
 END WITH
 draw_scrollbar istate.scroll, istate.scrollrect, , istate.page
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
 istate.info = readitemdescription(inventory(istate.cursor).id)
END SUB

SUB items_menu_autosort(iuse() AS INTEGER, permask() AS INTEGER)
 DIM autosort_changed AS INTEGER = NO
 FOR i AS INTEGER = 0 TO last_inv_slot() - 1
  FOR o AS INTEGER = i + 1 TO last_inv_slot()
   IF inventory(i).used = 0 AND inventory(o).used THEN
    itemmenuswap inventory(), iuse(), permask(), i, o
    autosort_changed = YES
    EXIT FOR
   END IF
  NEXT o
 NEXT i
 FOR i AS INTEGER = 0 TO last_inv_slot() - 1
  FOR o AS INTEGER = i + 1 TO last_inv_slot()
   IF readbit(iuse(), 0, 3 + i) = 0 AND readbit(iuse(), 0, 3 + o) = 1 THEN
    itemmenuswap inventory(), iuse(), permask(), i, o
    autosort_changed = YES
    EXIT FOR
   END IF
  NEXT o
 NEXT i
 IF autosort_changed THEN
  menusound gen(genAcceptSFX)
 ELSE
  menusound gen(genCancelSFX)
 END IF
END SUB

SUB item_menu_use_item(BYVAL slot AS INTEGER, istate AS ItemsMenuState, iuse() AS INTEGER, permask() AS INTEGER)
 IF inventory(slot).used = NO THEN EXIT SUB

 DIM consumed AS INTEGER = NO

 '--repaint the item menu so it can be the background for the menu_attack_targ_picker
 items_menu_paint istate, iuse(), permask()
 
 IF use_item_in_slot(slot, istate.trigger_box, consumed) THEN
  IF consumed THEN setbit iuse(), 0, 3 + slot, 0
  IF istate.trigger_box > 0 THEN EXIT SUB
  istate.re_use = YES
  istate.refresh = YES
 END IF
END SUB

FUNCTION use_item_in_slot(BYVAL slot AS INTEGER, BYREF trigger_box AS INTEGER, BYREF consumed AS INTEGER) AS INTEGER
 '--slot is the index in your inventory
 
 '--trigger_box is used to communicate when an item has triggered a text box.

 '--consumed communicates whether the item was actually consumed
 
 '--return value is YES when an item is used, and NO when it is not used.

 consumed = NO
 IF inventory(slot).used = NO THEN RETURN NO

 DIM itemdata(dimbinsize(binITM)) AS INTEGER
 loaditemdata itemdata(), inventory(slot).id
 DIM attack_name AS STRING = readbadbinstring(itemdata(), 0, 8, 0)
 DIM should_consume AS INTEGER = (itemdata(73) = 1)
 DIM attack_id AS INTEGER = itemdata(51) - 1
 DIM is_attack_item AS INTEGER = ( itemdata(51) > 0 ANDALSO NOT itemdata(50) > 0 )

 IF use_item_by_id(inventory(slot).id, trigger_box, inventory(slot).text) THEN
  IF should_consume THEN
   IF consumeitem(slot) THEN
    IF is_attack_item ANDALSO inventory(slot).used = NO THEN
     '--used the last attack item (potion) in a consumable stack
     menu_attack_targ_picker attack_id, -1, rpad(attack_name, " ", 8) & "x 0", , NO
     menusound gen(genCancelSFX)
    END IF
    consumed = YES
   END IF
  END IF
  RETURN YES
 END IF 
 RETURN NO
END FUNCTION

FUNCTION use_item_by_id(BYVAL item_id AS INTEGER, BYREF trigger_box AS INTEGER, name_override AS STRING="") AS INTEGER
 '--item_id is the actual ID number, not offset.

 '--trigger_box communicates the box id if this item triggerd a text box.
 '  this will remain unchanged (zero) if there is no box. The caller is
 '  responsible for loading the box. (FIXME: if it makes sense to
 '  actually load the box from here later, that would be awesome!)
 
 '--name_override is used so the inventory slot text can be used to replace
 '   the item name (which we would want because inventory slot text shows
 '   how many items are in the slot you are currently using)
 '   if name_override is left blank, you will just see the item name

 '--return value is YES if the item use was confirmed by the user
 '  or NO if it was cancelled or otherwise failed.

 '--This sub does not care if you actually own the item in question,
 '   nor will it consume items from your inventory even if the item is a consuming item.
 
 DIM itemdata(dimbinsize(binITM)) AS INTEGER
 loaditemdata itemdata(), item_id

 DIM caption AS STRING
 IF name_override <> "" THEN
  caption = name_override
 ELSE
  caption = readbadbinstring(itemdata(), 0, 8, 0)
 END IF

 IF itemdata(50) > 0 THEN '--learn a spell
  '--target the first non-dead hero
  MenuSound gen(genAcceptSFX)
  IF menu_attack_targ_picker(-1, itemdata(50)-1, caption) THEN
   '--successfully learned
   RETURN YES
  END IF
  RETURN NO
 END IF
 
 IF itemdata(51) > 0 THEN '--attack/oobcure
  MenuSound gen(genAcceptSFX)
  IF menu_attack_targ_picker(itemdata(51)-1, -1, caption) THEN
   RETURN YES
  END IF
  RETURN NO
 END IF
 
 IF itemdata(51) < 0 THEN '--trigger a text box
  trigger_box = itemdata(51) * -1
  RETURN YES
 END IF
 
 RETURN NO
END FUNCTION

FUNCTION menu_attack_targ_picker(BYVAL attack_id AS INTEGER, BYVAL learn_id AS INTEGER, use_caption AS STRING, BYVAL x_offset AS INTEGER=0, BYVAL really_use_attack AS INTEGER=YES) AS INTEGER
 'Returns true if the attack/spell was actually used/learned
 
 'FIXME: x_offset should probably go away in favor of a slice template at some point in the future
 
 menu_attack_targ_picker = NO

 STATIC targ AS INTEGER
 STATIC spread AS INTEGER

 '--Preserve background for display beneath the targ picker menu
 DIM page AS INTEGER
 page = compatpage
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage
 copypage page, holdscreen
 copypage holdscreen, page

 DIM cater AS INTEGER
 DIM walk AS INTEGER
 DIM wtogl AS INTEGER
 DIM tog
 DIM col AS INTEGER
 DIM atk AS AttackData
 DIM learn_attack AS AttackData
 DIM caption AS STRING
 DIM check_consume AS INTEGER = NO
 DIM allow_spread AS INTEGER = NO
 DIM must_spread AS INTEGER = NO
 
 IF attack_id >= 0 THEN
  loadattackdata atk, attack_id
  allow_spread = (atk.targ_set = 2)
  must_spread = (atk.targ_set = 1)
 END IF
 
 IF learn_id >= 0 THEN
  loadattackdata learn_attack, learn_id
 END IF

 '--make sure the default target is okay
 IF chkOOBtarg(targ, attack_id) = NO THEN
  targ = -1
  FOR i AS INTEGER = 0 TO 3
   IF chkOOBtarg(i, attack_id) THEN
    targ = i
    EXIT FOR
   END IF
  NEXT i
 END IF

 IF allow_spread THEN
  IF spread > 0 THEN
   '--if a spread target was previously recorded, update it
   spread = 0
   FOR i AS INTEGER = 0 TO 3
    IF chkOOBtarg(i, attack_id) THEN spread += 1
   NEXT i
  END IF
 ELSE
  spread = 0
 END IF

 IF must_spread THEN
  spread = 0
  FOR i AS INTEGER = 0 TO 3
   IF chkOOBtarg(i, attack_id) THEN spread += 1
  NEXT i
 END IF

 setkeys
 DO
  setwait speedcontrol
  setkeys
  tog = tog XOR 1
  wtogl = loopvar(wtogl, 0, 3, 1)

  playtimer
  control
  '--handle keys
  IF carray(ccMenu) > 1 THEN
   menusound gen(genCancelSFX)
   EXIT DO
  END IF
  IF spread = 0 THEN
   IF carray(ccUp) > 1 THEN
    getOOBtarg -1, targ, attack_id
    MenuSound gen(genCursorSFX)
   END IF
   IF carray(ccDown) > 1 THEN
    getOOBtarg 1, targ, attack_id
    MenuSound gen(genCursorSFX)
   END IF
  END IF
  
  IF allow_spread THEN
   IF carray(ccLeft) > 1 OR carray(ccRight) > 1 THEN
    MenuSound gen(genCursorSFX)
    IF spread = 0 THEN
     FOR i AS INTEGER = 0 TO 3
      IF chkOOBtarg(i, attack_id) THEN spread += 1
     NEXT i
    ELSE
     spread = 0
    END IF
   END IF
  END IF
  
  IF carray(ccUse) > 1 THEN
   'DO ACTUAL EFFECT
   IF targ = -1 THEN
    menusound gen(genCancelSFX)
    EXIT DO
   END IF
   'if can teach a spell
   IF learn_id >= 0 THEN '--teach spell
    '--trylearn
    IF trylearn(targ, learn_id+1, 0) THEN
     '--announce learn
     menusound gen(genItemLearnSFX)
     caption = names(targ) & " " & readglobalstring(124, "learned", 10) & " " & learn_attack.name
     centerbox 160, 100, small(LEN(caption) * 8 + 16, 320), 24, 1, page
     edgeprint caption, large(xstring(caption, 160), 0), 95, uilook(uiText), page
     IF learn_attack.learn_sound_effect > 0 THEN playsfx learn_attack.learn_sound_effect - 1
     setvispage page
     waitforanykey
     menu_attack_targ_picker = YES
    ELSE
     menusound gen(genCantLearnSFX)
    END IF
   END IF
   
   '--do attack outside of battle (cure)
   IF attack_id >= 0 ANDALSO really_use_attack THEN
    IF outside_battle_cure(attack_id, targ, -1, spread) THEN
     menu_attack_targ_picker = YES
    END IF
   END IF
  
   EXIT DO
  END IF '--done using attack

  '--draw the targ picker menu
  centerbox 160 + x_offset, 47, 160, 88, 2, page
  IF spread = 0 AND targ >= 0 THEN
   rectangle 84 + x_offset, 8 + targ * 20, 152, 20, uilook(uiHighlight2), page
  ELSEIF spread <> 0 THEN
   rectangle 84 + x_offset, 8, 152, 80, uilook(uiHighlight2 * tog), page
  END IF
  cater = 0
  FOR i AS INTEGER = 0 TO 3
   IF hero(i) > 0 THEN
    walk = 0
    IF targ = i THEN walk = INT(wtogl / 2)
    frame_draw herow(cater).sprite + (2 * 2) + walk, herow(cater).pal, 89 + x_offset, 8 + i * 20, 1, -1, page
    col = uilook(uiMenuItem)
    IF i = targ THEN col = uilook(uiSelectedItem + tog)
    IF attack_id >= 0 THEN
     IF atk.targ_stat = 0 or atk.targ_stat = 1 THEN
      caption = STR(gam.hero(i).stat.cur.sta(atk.targ_stat)) & "/" & STR(gam.hero(i).stat.max.sta(atk.targ_stat)) & " " & statnames(atk.targ_stat)
     ELSE
      caption = STR(gam.hero(i).stat.cur.sta(atk.targ_stat)) & " " & statnames(atk.targ_stat)
     END IF
     edgeprint caption, 119 + x_offset, 16 + i * 20, col, page
    ELSEIF learn_id >= 0 THEN
     edgeprint names(cater), 119 + x_offset, 16 + i * 20, col, page
    END IF
    cater += 1
   END IF
  NEXT i
  centerfuz 160 + x_offset, 100, LEN(use_caption) * 8 + 32, 16, 4, page
  edgeprint use_caption, xstring(use_caption, 160 + x_offset), 95, uilook(uiText), page
 
  setvispage vpage
  copypage holdscreen, dpage
  dowait
 LOOP
 
 carray(ccUse) = 0
 carray(ccMenu) = 0
 freepage page
 freepage holdscreen
END FUNCTION

SUB items_menu_control (istate AS ItemsMenuState, iuse() AS INTEGER, permask() AS INTEGER)
 istate.refresh = NO
 IF istate.re_use THEN
  istate.re_use = NO
  item_menu_use_item istate.cursor, istate, iuse(), permask()
  EXIT SUB
 END IF
 IF carray(ccMenu) > 1 THEN
  '--deselect currently selected item
  IF istate.sel > -1 THEN
   istate.sel = -4
   menusound gen(genCancelSFX)
  ELSE
   istate.quit = YES
  END IF
 END IF
 IF carray(ccUse) > 1 THEN
  '--exit
  IF istate.cursor = -3 THEN istate.quit = YES
  '--sort
  IF istate.cursor = -2 THEN items_menu_autosort iuse(), permask()
  IF istate.cursor = -1 AND istate.sel >= 0 AND readbit(permask(), 0, 3 + istate.sel) = 0 THEN
   '--try to thow item away
   IF inventory(istate.sel).used THEN MenuSound gen(genAcceptSFX)
   WITH inventory(istate.sel)
    .used = 0
    .id = 0
    .num = 0
   END WITH
   update_inventory_caption istate.sel
   setbit iuse(), 0, 3 + istate.sel, 0
   istate.sel = -4
   istate.refresh = YES
   EXIT SUB
  END IF
  IF istate.sel >= 0 THEN
   IF istate.cursor >= 0 AND istate.cursor <> istate.sel THEN
    '--swap the selected item and the item under the cursor
    itemmenuswap inventory(), iuse(), permask(), istate.cursor, istate.sel
    istate.sel = -4
    istate.refresh = YES
    MenuSound gen(genAcceptSFX)
    EXIT SUB
   END IF
   IF istate.cursor >= 0 AND istate.sel = istate.cursor THEN
    '--try to use the current item
    istate.sel = -4
    '--if the usability bit is off, or you dont have any of the item, exit
    IF readbit(iuse(), 0, 3 + istate.cursor) = 0 OR inventory(istate.cursor).used = 0 THEN EXIT SUB
    item_menu_use_item istate.cursor, istate, iuse(), permask()
    EXIT SUB
   END IF
  END IF
  IF istate.sel < -3 AND istate.cursor >= 0 THEN
   istate.sel = istate.cursor
   MenuSound gen(genAcceptSFX)
   EXIT SUB
  END IF
 END IF
 IF carray(ccUp) > 1 AND istate.cursor >= 0 THEN
  menusound gen(genCursorSFX)
  istate.cursor -= 3
  istate.refresh = YES
  IF istate.cursor < istate.top THEN istate.top -= 3
 END IF
 IF carray(ccDown) > 1 AND istate.cursor <= last_inv_slot() - 3 THEN
  menusound gen(genCursorSFX)
  istate.cursor = istate.cursor + 3
  istate.refresh = YES
  IF istate.cursor > istate.top + 62 THEN istate.top += 3
 END IF
 IF carray(ccLeft) > 1 THEN
  menusound gen(genCursorSFX)
  IF (istate.cursor MOD 3) = 0 THEN
   istate.cursor = istate.cursor + 2
  ELSE
   IF istate.cursor > -3 THEN istate.cursor -= 1
  END IF
  istate.refresh = YES
 END IF
 IF carray(ccRight) > 1 THEN
  menusound gen(genCursorSFX)
  IF ((istate.cursor + 3) MOD 3) = 2 THEN ' the +3 adjust for the first negative row
   istate.cursor = istate.cursor - 2
  ELSE
   IF istate.cursor < last_inv_slot() THEN istate.cursor += 1
  END IF
  istate.refresh = YES
 END IF
 IF keyval(scPageUp) > 1 THEN
  menusound gen(genCursorSFX)
  istate.cursor -= (istate.scroll.size+1) * 3
  WHILE istate.cursor < -3 : istate.cursor += 3: WEND
  WHILE istate.cursor < istate.top : istate.top -= 3 : WEND
  istate.refresh = YES
 END IF
 IF keyval(scPageDown) > 1 THEN
  menusound gen(genCursorSFX)
  istate.cursor += (istate.scroll.size+1) * 3
  WHILE istate.cursor > last_inv_slot(): istate.cursor -= 3: WEND
  WHILE istate.cursor >= istate.top + (istate.scroll.size+1) * 3 : istate.top += 3 : WEND
  istate.refresh = YES
 END IF
END SUB

SUB spells_menu_refresh_list(sp AS SpellsMenuState)
 IF sp.lists(sp.listnum).magic_type < 0 THEN EXIT SUB
 
 DIM atk AS AttackData
 DIM cost AS INTEGER
 
 FOR i AS INTEGER = 0 TO 23
  WITH sp.spell(i)
   .name = ""
   .desc = ""
   .cost = ""
   .id = -1
   .can_use = 0
   .targt = 0
   .tstat = 0
   'NOTE: spell() is a global
   IF spell(sp.hero, sp.lists(sp.listnum).menu_index, i) > 0 THEN
    .id = spell(sp.hero, sp.lists(sp.listnum).menu_index, i) - 1
    loadattackdata atk, .id
    IF atk.useable_outside_battle THEN
     .can_use = atk.targ_class + 1
     .targt = atk.targ_set
     .tstat = atk.targ_stat
    END IF
    cost = focuscost(atk.mp_cost, gam.hero(sp.hero).stat.cur.foc)
    
    'FIXME: should use the same cost-checking sub that the battle spell menu uses
    IF sp.lists(sp.listnum).magic_type = 0 AND gam.hero(sp.hero).stat.cur.mp < cost THEN
     .can_use = 0
    END IF
    IF sp.lists(sp.listnum).magic_type = 1 AND lmp(sp.hero, INT(i / 3)) = 0 THEN
     .can_use = 0
    END IF
    IF gam.hero(sp.hero).stat.cur.hp = 0 THEN
     .can_use = 0
    END IF
    
    .name = atk.name
    .desc = atk.description
    IF sp.lists(sp.listnum).magic_type = 0 THEN
     .cost = cost & " " & statnames(statMP) & " " & gam.hero(sp.hero).stat.cur.mp & "/" & gam.hero(sp.hero).stat.max.mp
    END IF
    IF sp.lists(sp.listnum).magic_type = 1 THEN
     .cost = readglobalstring(160, "Level MP", 20) & " " & (INT(i / 3) + 1) & ":  " & lmp(sp.hero, INT(i / 3))
    END IF
   END IF
   .name = rpad(.name, " ", 10)
  END WITH
 NEXT i
END SUB

SUB spells_menu_refresh_hero(sp AS SpellsMenuState)
 DIM her AS HeroDef
 loadherodata @her, hero(sp.hero) - 1 'hero() is a global
 
 '--first blank out lists
 FOR i AS INTEGER = 0 TO UBOUND(sp.lists)
  WITH sp.lists(i)
   .magic_type = -1
   .menu_index = -1
   .name = ""
  END WITH
 NEXT i

 DIM bmenu_id AS INTEGER
 DIM slot AS INTEGER = 0
 '--loop through the battle menu looking for valid lists
 FOR i AS INTEGER = 0 TO UBOUND(bmenu, 2)
  bmenu_id = bmenu(sp.hero, i)
  IF bmenu_id < 0 AND bmenu_id > -10 THEN
   'Positive numbers in bmenu() are attacks (from weapon)
   '-10 in bmenu is the item menu.
   'In theory -1 thru -9 are spell lists, but in reality only -1 thru -4 ever exist
   WITH sp.lists(slot)
    .menu_index = ABS(bmenu_id + 1)
    .magic_type = her.list_type(.menu_index)
    IF .magic_type = 0 OR .magic_type = 1 THEN
     'Only display MP-based and LMP-based spell lists. Ignore Random lists.
     IF readbit(her.bits(), 0, 26) <> 0 THEN
      'If the bitset to hide empty lists is turned on...
      IF count_available_spells(sp.hero, .menu_index) = 0 THEN CONTINUE FOR
     END IF
     .name = her.list_name(.menu_index)
     IF .name <> "" THEN
      'Only show lists with non-blank names
      .name = rpad(.name, " ", 10)
      slot += 1
     END IF
    END IF
   END WITH 
  END IF
 NEXT i
 
 sp.last = slot
 
 WITH sp.lists(sp.last)
  .name = rpad(readglobalstring(46, "Exit", 10), " ", 10)
  .menu_index = -1
  .magic_type = -1
 END WITH
 IF sp.listnum > sp.last THEN sp.listnum = sp.last

 spells_menu_refresh_list sp
END SUB

SUB spells_menu_control(sp AS SpellsMenuState)
 IF sp.mset = 0 THEN '--picking which spell list
  IF carray(ccMenu) > 1 THEN sp.quit = YES : EXIT SUB
  IF carray(ccLeft) > 1 THEN
   DO
    sp.hero = loopvar(sp.hero, 0, 3, -1)
   LOOP UNTIL hero(sp.hero) > 0
   menusound gen(genCursorSFX)
   spells_menu_refresh_hero sp
  END IF
  IF carray(ccRight) > 1 THEN
   DO
    sp.hero = loopvar(sp.hero, 0, 3, 1)
   LOOP UNTIL hero(sp.hero) > 0
   menusound gen(genCursorSFX)
   spells_menu_refresh_hero sp
  END IF

  usemenusounds
  IF usemenu (sp.listnum, 0, 0, sp.last, 5) THEN
   spells_menu_refresh_list sp
  END IF
  IF carray(ccUse) > 1 THEN
   IF sp.lists(sp.listnum).menu_index = -1 THEN sp.quit = YES : EXIT SUB
   menusound gen(genAcceptSFX)
   sp.mset = 1
   sp.cursor = 0
  END IF
 ELSE '--picking a specific spell from a list
  IF sp.re_use = NO THEN
   IF carray(ccMenu) > 1 THEN
    sp.mset = 0
    menusound gen(genCancelSFX)
   END IF
   IF carray(ccUp) > 1 THEN
    sp.cursor = sp.cursor - 3
    menusound gen(genCursorSFX)
    IF sp.cursor < 0 THEN sp.cursor = 24
   END IF
   IF carray(ccDown) > 1 THEN
    IF sp.cursor < 24 THEN
     sp.cursor = small(sp.cursor + 3, 24)
    ELSE
     sp.cursor = 0
    END IF
    menusound gen(genCursorSFX)
   END IF
   IF sp.cursor < 24 THEN   'EXIT not selected
    IF carray(ccLeft) > 1 THEN
     IF sp.cursor MOD 3 THEN
      sp.cursor = sp.cursor - 1
     ELSE
      sp.cursor = sp.cursor + 2
     END IF
     menusound gen(genCursorSFX)
    END IF
    IF carray(ccRight) > 1 THEN
     IF sp.cursor MOD 3 = 2 THEN sp.cursor = sp.cursor - 2 ELSE sp.cursor += 1
     menusound gen(genCursorSFX)
    END IF
    IF keyval(scPageUp) > 1 THEN
     sp.cursor = sp.cursor MOD 3
     menusound gen(genCursorSFX)
    END IF
    IF keyval(scPageDown) > 1 THEN
     sp.cursor = (sp.cursor MOD 3) + 21
     menusound gen(genCursorSFX)
    END IF
   END IF
  END IF
  IF carray(ccUse) > 1 OR sp.re_use THEN
   sp.re_use = NO
   IF sp.cursor = 24 THEN sp.mset = 0
   IF sp.spell(sp.cursor).can_use > 0 THEN
    '--spell that can be used oob
    
    DIM atk AS AttackData
    loadattackdata atk, sp.spell(sp.cursor).id
    '--NOTE: atkallowed isn't needed here because the check is done elswehere...
    '--still, it would be nice if we could use it anyway...
    'IF atkallowed(atk, sp.hero, sp.lists(sp.listnum).magic_type, INT(sp.cursor / 3), )
    
    '--repaint the screen so it will show up under the menu attack targ picker
    spells_menu_paint sp
    IF menu_attack_targ_picker(sp.spell(sp.cursor).id, -1, TRIM(sp.spell(sp.cursor).name), 36) THEN
     '--attack was actually used
     'FIXME: outside-battle and inside-battle attack cost consumption should be unified
     '--deduct MP
     DIM cost AS INTEGER
     cost = focuscost(atk.mp_cost, gam.hero(sp.hero).stat.cur.foc)
     gam.hero(sp.hero).stat.cur.mp = small(large(gam.hero(sp.hero).stat.cur.mp - cost, 0), gam.hero(sp.hero).stat.max.mp)
     IF sp.lists(sp.listnum).magic_type = 1 THEN
      '--deduct LMP
      lmp(sp.hero, INT(sp.cursor / 3)) = lmp(sp.hero, INT(sp.cursor / 3)) - 1
     END IF
     spells_menu_refresh_hero sp
     sp.re_use = YES
    ELSE
     menusound gen(genCancelSFX)
    END IF
   ELSE
    menusound gen(genCancelSFX)
   END IF
  END IF
 END IF
END SUB

SUB spells_menu (who AS INTEGER)

 DIM sp AS SpellsMenuState
 sp.hero = who
 sp.listnum = 0

 sp.cancel_menu_caption = readglobalstring(51, "(CANCEL)", 10)
 sp.has_none_caption = readglobalstring(133, "has no spells", 20)

 spells_menu_refresh_hero sp
 '--Preserve background for display beneath the spells menu
 sp.page = compatpage
 DIM holdscreen AS INTEGER = allocatepage
 copypage sp.page, holdscreen

 menusound gen(genAcceptSFX)
 DIM wtogl AS INTEGER = 0
 setkeys
 DO
  setwait speedcontrol
  setkeys
  sp.tog = sp.tog XOR 1
  wtogl = loopvar(wtogl, 0, 3, 1)
  playtimer
  control
  spells_menu_control sp
  IF sp.quit THEN EXIT DO
  spells_menu_paint sp
  setvispage vpage
  copypage holdscreen, sp.page
  IF sp.re_use = NO THEN
   dowait
  END IF
 LOOP

 menusound gen(genCancelSFX)
 setkeys
 flusharray carray(), 7
 freepage sp.page
 freepage holdscreen

END SUB

SUB spells_menu_paint (BYREF sp AS SpellsMenuState)
 centerfuz 160, 100, 312, 184, 1, sp.page 'outer box
 centerbox 206, 36, 200, 17, 2, sp.page   'name box
 centerbox 56, 50, 84, 60, 2, sp.page     'spell lists menu box
 centerbox 160, 134, 308, 96, 2, sp.page  'spell list
 rectangle 6, 168, 308, 1, uilook(uiTextBox + 3), sp.page 'divider 2
 'top menu (spell lists)
 FOR i AS INTEGER = 0 TO sp.last
  textcolor uilook(uiMenuItem), 0
  IF sp.listnum = i THEN textcolor uilook(uiSelectedItem + sp.tog), uilook(uiHighlight2): IF sp.mset = 1 THEN textcolor uilook(uiMenuItem), uilook(uiHighlight2)
  printstr sp.lists(i).name, 16, 25 + i * 10, sp.page 'spell menu
 NEXT i

 'bottom menu (spells in spell list)
 IF sp.lists(sp.listnum).menu_index >= 0 THEN
  FOR o AS INTEGER = 0 TO 23
  'Note: this will give yellow when .can_use is -1 (is it ever?), orig would give blue
   textcolor uilook(uiDisabledItem - SGN(sp.spell(o).can_use)), 0
   IF sp.cursor = o AND sp.mset = 1 THEN
    IF sp.spell(o).can_use > 0 THEN 
     textcolor uilook(uiSelectedItem + sp.tog), uilook(uiHighlight) 
    ELSE 
     textcolor uilook(uiMenuItem), uilook(uiHighlight)
    END IF
   END IF
   printstr sp.spell(o).name, 12 + (o MOD 3) * 104, 90 + (o \ 3) * 8, sp.page 'spells
  NEXT o
  textcolor uilook(uiMenuItem), 0
  IF sp.cursor = 24 AND sp.mset = 1 THEN textcolor uilook(uiSelectedItem + sp.tog), uilook(uiHighlight)
  printstr sp.cancel_menu_caption, 16, 171, sp.page 'cancel
  IF sp.mset = 1 THEN
   IF sp.spell(sp.cursor).desc <> "" THEN
    rectangle 6, 155, 308, 1, uilook(uiTextBox + 3), sp.page  'description divider
   END IF
   textcolor uilook(uiDescription), 0
   printstr sp.spell(sp.cursor).cost, 303 - LEN(sp.spell(sp.cursor).cost) * 8, 171, sp.page 'cost
   printstr sp.spell(sp.cursor).desc, 9, 158, sp.page 'description
  END IF
 END IF

 IF sp.last = 0 THEN edgeprint names(sp.hero) & " " & sp.has_none_caption, xstring(names(sp.hero) & " " & sp.has_none_caption, 160), 120, uilook(uiText), sp.page
 edgeprint names(sp.hero), xstring(names(sp.hero), 206), 31, uilook(uiText), sp.page
END SUB
