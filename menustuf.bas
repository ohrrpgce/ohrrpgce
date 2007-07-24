'OHRRPGCE GAME - Mostly user-interface related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION focuscost% (cost%, focus%)
DECLARE SUB renamehero (who%)
DECLARE FUNCTION chkOOBtarg% (wptr%, index%, stat%(), ondead%(), onlive%())
DECLARE FUNCTION getOOBtarg% (gamma%, wptr%, index%, stat%(), ondead%(), onlive%())
DECLARE FUNCTION trylearn% (who%, atk%, learntype%)
DECLARE SUB herobattlebits (bitbuf%(), who%)
DECLARE SUB unequip (who%, where%, defwep%, stat%(), resetdw%)
DECLARE SUB loadtemppage (page%)
DECLARE SUB savetemppage (page%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB vishero (stat%())
DECLARE SUB doequip (toequip%, who%, where%, defwep%, stat%())
DECLARE FUNCTION playtime$ (d%, h%, m%)
DECLARE SUB playtimer ()
DECLARE FUNCTION averagelev% (stat%())
DECLARE FUNCTION istag% (num%, zero%)
DECLARE SUB evalherotag (stat%())
DECLARE SUB delitem (it%, num%)
DECLARE FUNCTION consumeitem% (index%)
DECLARE SUB evalitemtag ()
DECLARE SUB itstr (i%)
DECLARE SUB control ()
DECLARE FUNCTION picksave% (load%)
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB getitem (getit%, num%)
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%())
DECLARE SUB spells (pt%, stat%())
DECLARE SUB status (pt%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB resetlmp (slot%, lev%)
DECLARE SUB loadfoe (i%, formdata%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), stat%(), ebits%(), batname$())
DECLARE FUNCTION inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%())
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE SUB addhero (who%, slot%, stat%())
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION rangel& (n&, r%)
DECLARE SUB snapshot ()
DECLARE SUB checkTagCond(t,check,tag,tagand) 'in bmod.bas
DECLARE FUNCTION countitem% (it%)
DECLARE SUB loadshopstuf (array%(), id%)

DECLARE Sub MenuSound(byval s as integer)

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi" 
#include "gglobals.bi"
#include "const.bi"
#include "uiconst.bi"

REM $STATIC
SUB buystuff (id, shoptype, storebuf(), stock(), stat())
DIM b(dimbinsize(1) * 50), stuf$(50), vmask(5), emask(5), sname$(40), buytype$(5, 1), wbuf(100), walks(15), hpal(8), tradestf(3, 1)
recordsize = curbinsize(1) / 2 ' get size in INTs

getnames sname$()
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
GOSUB setstock
GOSUB stufmask
IF total = 0 THEN EXIT SUB

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

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 IF tog THEN walk = loopvar(walk, 0, 15, 1)
 playtimer
 control
 IF carray(0) > 1 THEN
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
 IF carray(1) > 1 THEN
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
 IF carray(5) > 1 THEN EXIT DO
 IF carray(4) > 1 THEN '---PRESS ENTER---------------------
  IF readbit(emask(), 0, pt) = 0 THEN '---CHECK TO SEE IF YOU CAN AFFORD IT---
   IF stock(id, pt) > 1 THEN stock(id, pt) = stock(id, pt) - 1
   IF b(pt * recordsize + 22) THEN setbit tag(), 0, ABS(b(pt * recordsize + 22)), SGN(SGN(b(pt * recordsize + 22)) + 1)
   gold& = gold& - b(pt * recordsize + 24)
   IF tradingitems THEN '---TRADE IN ITEMS----------
    FOR i = 0 TO 3
     IF tradestf(i, 0) > -1 THEN
      delitem tradestf(i, 0) + 1, tradestf(i, 1)
     END IF
    NEXT
   END IF '-------END TRADE IN ITEM----------------------------
   IF b(pt * recordsize + 17) = 0 THEN '---BUY ITEM-------------------
    getitem b(pt * recordsize + 18) + 1, 1
    acol = 4
    alert = 10
    alert$ = purchased$ + " " + stuf$(pt)
   END IF '-------END IF ITEM-------------------------------------
   IF b(pt * recordsize + 17) = 1 THEN '---HIRE HERO------------------
    'getitem b(pt * recordsize + 18) + 1, 1
    FOR i = 37 TO 0 STEP -1
     IF hero(i) = 0 THEN slot = i
    NEXT i
    addhero b(pt * recordsize + 18) + 1, slot, stat()
    acol = 4
    alert = 10
    alert$ = stuf$(pt) + " " + joined$
   END IF '-------END IF HERO-------------------------------------
   'the last thing to do is re-eval the item and hero tags in case
   'stuff changed
   evalherotag stat()
   evalitemtag
  ELSE ' WHEN CANNOT AFFORD------------------------------------
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
     IF pt > storebuf(16) THEN EXIT SUB
    LOOP
    EXIT DO
   END IF
  LOOP
  GOSUB curinfo
 END IF '---------END TRY BUY THING--------

 centerbox 80, 94, 150, 168, 1, dpage
 centerbox 240, 94, 150, 168, 1, dpage
 '-----RIGHT PANEL------------------------------------------
 centerbox 240, 20, LEN(XSTR$(gold&) + " " + sname$(32)) * 8 + 8, 12, 4, dpage
 edgeprint XSTR$(gold&) + " " + sname$(32), xstring(XSTR$(gold&) + " " + sname$(32) + " ", 240), 15, uilook(uiText), dpage
 o = 0
 edgeprint stuf$(pt), xstring(stuf$(pt), 240), 30 + o * 10, uilook(uiMenuItem), dpage: o = o + 1
 IF info1$ <> "" THEN edgeprint info1$, xstring(info1$, 240), 30 + o * 10, uilook(uiDisabledItem), dpage: o = o + 1
 IF info2$ <> "" THEN edgeprint info2$, xstring(info2$, 240), 30 + o * 10, uilook(uiDisabledItem), dpage: o = o + 1
 IF eqinfo$ <> "" THEN edgeprint eqinfo$, xstring(eqinfo$, 240), 30 + o * 10, uilook(uiMenuItem), dpage: o = o + 1
 IF stock(id, pt) > 1 THEN
  edgeprint XSTR$(stock(id, pt) - 1) + " " + instock$ + " ", xstring(XSTR$(stock(id, pt) - 1) + " in stock ", 240), 30 + o * 10, uilook(uiMenuItem), dpage: o = o + 1
 END IF
 IF showhero > -1 THEN
  centerbox 240, 130, 36, 44, 4, dpage
  loadsprite buffer(), 0, 640 * walks(walk), 0, 32, 40, 2
  drawsprite buffer(), 0, hpal(), 0, 224, 110, dpage
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
  centerbox 160, 186, LEN(price$) * 8 + 8, 12 + xtralines * 10, 1, dpage
  edgeprint price$, xstring(price$, 160), 182 - xtralines * 5, uilook(uiText), dpage
  IF xtralines >= 1 THEN edgeprint price2$, xstring(price2$, 160), 187, uilook(uiText), dpage
 END IF
 IF alert THEN
  alert = alert - 1
  centerbox 160, 178, LEN(alert$) * 8 + 8, 12, acol, dpage
  edgeprint alert$, xstring(alert$, 160), 173, uilook(uiSelectedItem + tog), dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
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
 IF stock(id, i) = 1 THEN setbit vmask(), 0, i, 1
 IF b(i * recordsize + 17) = (shoptype XOR 1) THEN setbit vmask(), 0, i, 1
 IF NOT istag(b(i * recordsize + 20), -1) THEN setbit vmask(), 0, i, 1
 IF b(i * recordsize + 24) > gold& THEN setbit emask(), 0, i, 1
 temp = i
 GOSUB loadtrades
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
price$ = ""
price2$ = ""
eqinfo$ = ""
info1$ = ""
info2$ = ""
IF b(pt * recordsize + 24) > 0 THEN price$ = STR$(b(pt * recordsize + 24)) + " " + sname$(32)
'--load must trade in item types+amounts
temp = pt
GOSUB loadtrades
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
 loaditemdata buffer(), b(pt * recordsize + 18)
 IF buffer(49) = 1 THEN eqinfo$ = eqprefix$ + " " + wepslot$
 IF buffer(49) > 1 THEN eqinfo$ = eqprefix$ + " " + sname$(23 + buffer(49))
 info1$ = readbadbinstring$(buffer(), 9, 40, 0)
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
END IF
IF b(pt * recordsize + 17) = 1 THEN
 'hire
 dim her as herodef
 loadherodata @her, b(pt * recordsize + 18)
 loaditemdata wbuf(), her.def_weapon
 IF her.def_level < 0 THEN her.def_level = averagelev(stat())
 temp$ = XSTR$(atlevel(her.def_level, her.lev0.st.hp, her.lev99.st.hp) + wbuf(54 + 0))
 eqinfo$ = RIGHT$(temp$, LEN(temp$) - 1) + " " + sname$(0)
 showhero = buffer(17)
 getpal16 hpal(), 0, her.sprite_pal, 0, showhero
 setpicstuf buffer(), 5120, 2
 loadset game$ + ".pt0", showhero, 0
 IF eslot = 0 THEN info1$ = noroom$
END IF
RETRACE

setstock:
FOR i = 0 TO storebuf(16)
 '--for each shop-stuff
 IF stock(id, i) = 0 THEN
  '--if unloaded, reload stock
  stock(id, i) = b(i * recordsize + 19)
  '--zero means unloaded, 1 means no-stock, 2+n means 1+n in stock
  IF stock(id, i) > -1 THEN stock(id, i) = stock(id, i) + 1
 END IF
NEXT i
RETRACE

loadtrades:
tradestf(0, 0) = b(temp * recordsize + 25) - 1
tradestf(0, 1) = b(temp * recordsize + 30) + 1
FOR k = 1 TO 3
 tradestf(k, 0) = b(temp * recordsize + k * 2 + 29) - 1
 tradestf(k, 1) = b(temp * recordsize + k * 2 + 30) + 1
NEXT
RETRACE

END SUB

FUNCTION chkOOBtarg (wptr, index, stat(), ondead(), onlive())
'RETRACE true if valid, false if not valid
chkOOBtarg = -1
IF hero(wptr) = 0 OR (stat(wptr, 0, 0) = 0 AND readbit(ondead(), 0, index) = 0) OR (stat(wptr, 0, 0) > 0 AND readbit(onlive(), 0, index) = 0) THEN
 chkOOBtarg = 0
END IF
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

SUB equip (pt, stat())

'--dim stuff
DIM sname$(40), sno(11), eq(199), toff(4), tlim(4), m$(4), menu$(6), stb(11)

savetemppage 3
copypage dpage, 3

'--get names
getnames sname$()
m$(0) = readglobalstring$(38, "Weapon", 10)
FOR i = 0 TO 3
 m$(i + 1) = sname$(25 + i)
NEXT i
menu$(5) = rpad$(readglobalstring$(39, "-REMOVE-", 8), " ", 8)
menu$(6) = rpad$(readglobalstring$(40, "-EXIT-", 8), " ", 8)
unequipone$ = readglobalstring$(110, "Nothing", 10)

'--stat name offsets
sno(0) = 0
sno(1) = 1
sno(2) = 2
sno(3) = 3
sno(4) = 5
sno(5) = 6
sno(6) = 29
sno(7) = 30
sno(8) = 8
sno(9) = 7
sno(10) = 31
sno(11) = 4

'--initialize
dw = 0
dw$ = ""
mset = 0
GOSUB setupeq

'--prepare the backdrop
copypage vpage, 3
centerfuz 160, 100, 304, 184, 1, 3
centerbox 84, 16, 140, 20, 4, 3
centerbox 84, 100, 140, 130, 4, 3
centerbox 236, 75, 80, 78, 4, 3

'--main loop
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF mset = 0 THEN
  '--primary menu
  IF carray(5) > 1 THEN loadtemppage 3: FOR t = 4 TO 5: carray(t) = 0: NEXT t: EXIT SUB
  IF carray(2) > 1 THEN DO: pt = loopvar(pt, 0, 3, -1): LOOP UNTIL hero(pt) > 0: GOSUB setupeq
  IF carray(3) > 1 THEN DO: pt = loopvar(pt, 0, 3, 1): LOOP UNTIL hero(pt) > 0: GOSUB setupeq
  IF carray(0) > 1 THEN csr = csr - 1: IF csr < 0 THEN csr = 6
  IF carray(1) > 1 THEN csr = csr + 1: IF csr > 6 THEN csr = 0
  IF carray(4) > 1 THEN
   IF csr < 5 THEN
    '--change equipment
    IF tlim(csr) >= 0 OR eqstuf(pt, csr) > 0 THEN
     '--switch to change equipment mode
     mset = 1
     top = toff(csr)
     csr2 = top
     GOSUB stbonus
    END IF
    'UPDATE ITEM POSESION BITSETS
    evalitemtag
   END IF
   IF csr = 5 THEN
    '--unequip all
    FOR csr = 0 TO 4
     unequip pt, csr, dw, stat(), 1
    NEXT csr
    GOSUB setupeq
    csr = 5
    'UPDATE ITEM POSESSION BITSETS
    evalitemtag
   END IF
   IF csr = 6 THEN loadtemppage 3: carray(4) = 0: EXIT SUB
  END IF
 ELSE
  '--change equip menu
  IF carray(5) > 1 THEN mset = 0: FOR i = 0 TO 11: stb(i) = 0: NEXT i
  IF carray(0) > 1 THEN
   csr2 = large(csr2 - 1, toff(csr))
   GOSUB stbonus
   IF csr2 < top THEN top = top - 1
  END IF
  IF carray(1) > 1 THEN
   csr2 = small(csr2 + 1, toff(csr) + tlim(csr) + 1)
   GOSUB stbonus
   IF csr2 > top + 17 THEN top = top + 1
  END IF
  IF carray(4) > 1 THEN
   IF csr2 = toff(csr) + tlim(csr) + 1 THEN
    '--unequip
    unequip pt, csr, dw, stat(), 1
    GOSUB EquBacktomenuSub
   ELSE
    '--normal equip
    ie = inventory(eq(csr2)).id + 1
    GOSUB newequip
   END IF
  END IF
 END IF

 '--display
 edgeprint names$(pt), 84 - LEN(names$(pt)) * 4, 12, uilook(uiText), dpage
 FOR i = 0 TO 11
  edgeprint sname$(sno(i)), 20, 42 + i * 10, uilook(uiMenuItem), dpage
  col = uilook(uiMenuItem)
  IF stb(i) < 0 THEN col = uilook(uiDisabledItem)
  IF stb(i) > 0 THEN col = uilook(uiSelectedItem + tog)
  IF gen(genStatCap + i) > 0 THEN
   temp$ = XSTR$(small(stat(pt, 1, i) + stb(i), gen(genStatCap + i)))
  ELSE
   temp$ = XSTR$(stat(pt, 1, i) + stb(i))
  END IF
  edgeprint temp$, 148 - LEN(temp$) * 8, 42 + i * 10, col, dpage
 NEXT i
 IF mset = 0 THEN
  '--main menu display
  FOR i = 0 TO 6
   textcolor uilook(uiMenuItem), uilook(uiHighlight)
   IF i < 5 THEN
    IF eqstuf(pt, i) = 0 AND tlim(i) < 0 THEN textcolor uilook(uiMenuItem), uilook(uiTextBox)
   END IF
   IF csr = i THEN
    textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight + tog)
    IF i < 5 THEN IF tlim(i) < 0 THEN textcolor uilook(uiSelectedItem), uilook(uiHighlight2)
   END IF
   printstr menu$(i), 204, 45 + i * 9, dpage
  NEXT i
  IF csr < 5 THEN
   centerbox 236, 20, (LEN(m$(csr)) + 2) * 8, 20, 4, dpage
   edgeprint m$(csr), 236 - (LEN(m$(csr)) * 4), 16, uilook(uiText), dpage
  END IF
 END IF
 IF mset = 1 THEN
  '--change equipment menu
  centerbox 236, 100, 96, 152, 4, dpage
  FOR i = top TO top + 17
   textcolor uilook(uiMenuItem), 0
   IF i = csr2 THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight2)
   IF i > toff(csr) + tlim(csr) THEN
    IF i = toff(csr) + tlim(csr) + 1 THEN
     '--unequip option
     IF csr = 0 THEN
      printstr dw$, 192, 28 + (i - top) * 8, dpage
     ELSE
      printstr unequipone$, 192, 28 + (i - top) * 8, dpage
     END IF
    ELSE
     '--all done!
     EXIT FOR
    END IF
   ELSE
    printstr inventory(eq(i)).text, 192, 28 + (i - top) * 8, dpage
   END IF
  NEXT i
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

stbonus:
'--load stat bonuses of currently hovered weapon for display

IF csr2 = toff(csr) + tlim(csr) + 1 THEN
 '--unequip
 IF csr = 0 THEN
  '--special handling for weapon
  '--load the default weapon info and continue as normal
  lb = dw
 ELSE
  'non-weapon unequip sets lb -1 to warn to skip that step
  lb = -1
 END IF
ELSE
 '--equip
 lb = inventory(eq(csr2)).id + 1
END IF

IF lb = -1 THEN
 '--nothing to load!
 FOR i = 0 TO 11
  stb(i) = 0
 NEXT i
ELSE
 loaditemdata buffer(), lb - 1
 FOR i = 0 TO 11
  stb(i) = buffer(54 + i)
 NEXT i
END IF

IF eqstuf(pt, csr) > 0 THEN
 loaditemdata buffer(), eqstuf(pt, csr) - 1
 FOR i = 0 TO 11
  stb(i) = stb(i) - buffer(54 + i)
 NEXT i
END IF

FOR i = 0 to 11
 IF gen(genStatCap + i) > 0 THEN stb(i) = small(stb(i), gen(genStatCap + i))
NEXT i

RETRACE

newequip:
unequip pt, csr, dw, stat(), 0
doequip ie, pt, csr, dw, stat()
GOSUB EquBacktomenuSub
RETRACE

EquBacktomenuSub:
mset = 0
FOR i = 0 TO 11
 stb(i) = 0
NEXT i
GOSUB setupeq
RETRACE

setupeq:
dw = stat(pt, 0, 16)
dw$ = rpad(readitemname$(dw - 1), " ", 11)

setpicstuf buffer(), 200, -1
FOR i = 0 TO 4
 menu$(i) = "        "
 IF eqstuf(pt, i) > 0 THEN
  menu$(i) = rpad$(readitemname$(eqstuf(pt, i) - 1), " ", 8)
 END IF
NEXT i
o = 0
FOR i = 0 TO inventoryMax
 IF inventory(i).used THEN
  '--load item data
  loaditemdata buffer(), inventory(i).id
  IF buffer(49) > 0 THEN
   '--if this item is equipable
   IF readbit(buffer(), 66, hero(pt) - 1) THEN
    '--if this item is equipable by this hero
    'eq low is item number
    'eq high is equip slot
    eq(o) = i + (buffer(49) * 256)
    o = o + 1
   END IF
  END IF
 END IF
NEXT i

'--sort by equip slot, right?
j = 0
FOR k = 1 TO 5
 toff(k - 1) = j
 FOR i = o TO 0 STEP -1
  WHILE INT(eq(j) / 256) = k: j = j + 1: WEND
  IF i <= j THEN EXIT FOR
  IF INT(eq(i) / 256) = k THEN SWAP eq(i), eq(j): j = j + 1
 NEXT i
 tlim(k - 1) = j - toff(k - 1) - 1
NEXT k

'clip off the high byte?
FOR i = 0 TO toff(4) + tlim(4)
 eq(i) = (eq(i) AND 255)
NEXT i

RETRACE
END SUB

SUB getitem (getit, num)

numitems = num

FOR i = 0 TO inventoryMax
 ' Loop through all inventory slots looking for a slot that already
 ' contains the item we are adding. If found increment that slot
 room = 99 - inventory(i).num
 IF inventory(i).used AND getit - 1 = inventory(i).id AND room > 0 THEN
  IF room < numitems THEN
   inventory(i).num = 99
   itstr i
   numitems -= room
  ELSE
   inventory(i).num += numitems
   itstr i
   EXIT SUB
  END IF
 END IF
NEXT
FOR i = 0 TO inventoryMax
 'loop through each inventory slot looking for an empty slot to populate 
 IF inventory(i).used = 0 THEN
  inventory(i).used = 1
  inventory(i).id = getit - 1
  inventory(i).num = small(numitems, 99)
  numitems -= inventory(i).num
  itstr i
  IF numitems = 0 THEN EXIT SUB
 END IF
NEXT
getit = 0
END SUB

FUNCTION getOOBtarg (gamma, wptr, index, stat(), ondead(), onlive())
'--return true on success, false on failure
getOOBtarg = -1
safety = 0
wptr = loopvar(wptr, 0, 3, gamma)
DO WHILE chkOOBtarg(wptr, index, stat(), ondead(), onlive()) = 0
 wptr = loopvar(wptr, 0, 3, gamma)
 safety = safety + 1
 IF safety >= 4 THEN
  getOOBtarg = 0
  EXIT FUNCTION
 END IF
LOOP
END FUNCTION

SUB itemmenuswap (invent() AS InventSlot, iuse(), permask(), i, o)
'this sub called from items()
SWAP invent(i).id, invent(o).id
SWAP invent(i).num, invent(o).num
SWAP invent(i).text, invent(o).text
temp = invent(i).used
invent(i).used = invent(o).used
invent(o).used = temp

t1 = readbit(iuse(), 0, 3 + i)
t2 = readbit(iuse(), 0, 3 + o)
setbit iuse(), 0, 3 + i, t2
setbit iuse(), 0, 3 + o, t1
t1 = readbit(permask(), 0, 3 + i)
t2 = readbit(permask(), 0, 3 + o)
setbit permask(), 0, 3 + i, t2
setbit permask(), 0, 3 + o, t1
END SUB

FUNCTION items (stat())
DIM a(100), iuse(15), ondead(15), onlive(15), permask(15), special$(-3 TO -1)
'bit 0 of iuse, permask, onlive, ondead correspond to item -3

special$(-3) = rpad$(readglobalstring$(35, "DONE", 10), " ", 11)
special$(-2) = rpad$(readglobalstring$(36, "AUTOSORT", 10), " ", 11)
special$(-1) = rpad$(readglobalstring$(37, "TRASH", 10), " ", 11)

REMEMBERSTATE

savetemppage 3
copypage dpage, 3

FOR i = 0 TO 2
 setbit iuse(), 0, i, 1
NEXT i
FOR i = 0 TO inventoryMax
 setbit ondead(), 0, 3 + i, 0
 setbit onlive(), 0, 3 + i, 1
 IF inventory(i).used THEN
  loaditemdata buffer(), inventory(i).id
  IF buffer(73) = 2 THEN setbit permask(), 0, 3 + i, 1
  IF buffer(51) > 0 OR buffer(50) > 0 THEN
   setbit iuse(), 0, 3 + i, 1
   temp = buffer(51) - 1
   loadattackdata buffer(), temp
   IF buffer(3) = 4 OR buffer(3) = 10 THEN setbit ondead(), 0, 3 + i, 1
   IF buffer(3) = 10 THEN setbit onlive(), 0, 3 + i, 0
  END IF
  IF buffer(51) < 0 THEN
   setbit iuse(), 0, 3 + i, 1
  END IF
 END IF
NEXT i
ic = -3: top = -3: sel = -4: wptr = 0: spred = 0: pick = 0
info$ = ""
centerbox 160, 92, 304, 176, 1, 3

GOSUB infostr
setkeys
quit = 0
wtogl = 0
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 wtogl = loopvar(wtogl, 0, 3, 1)
 playtimer
 control
 GOSUB itcontrol
 IF quit THEN EXIT DO
 FOR i = top TO top + 62
  textcolor uilook(uiDisabledItem), 0
  IF readbit(iuse(), 0, 3 + i) = 1 THEN textcolor uilook(uiMenuItem), 0
  IF readbit(permask(), 0, 3 + i) THEN textcolor uilook(uiSelectedDisabled), 0
  IF ic = i THEN
   textcolor uilook(uiMenuItem), uilook(uiHighlight2)
   IF readbit(iuse(), 0, 3 + i) = 1 THEN textcolor uilook(uiText), uilook(uiHighlight2)
   IF readbit(permask(), 0, 3 + i) THEN textcolor uilook(uiGold), uilook(uiHighlight2)
  END IF
  IF sel = i THEN
   textcolor uilook(uiMenuItem), uilook(uiHighlight + tog)
   IF ic = i THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight + tog)
  END IF
  IF i >= 0 THEN
   display$ = inventory(i).text
  ELSE
   display$ = special$(i)
  END IF
  printstr display$, 20 + 96 * ((i + 3) MOD 3), 12 + 8 * ((i - top) \ 3), dpage
 NEXT i
 centerfuz 160, 180, 312, 20, 4, dpage
 edgeprint info$, xstring(info$, 160), 175, uilook(uiText), dpage
 IF pick = 1 THEN
  centerbox 160, 47, 160, 88, 2, dpage
  IF spred = 0 THEN rectangle 84, 8 + wptr * 20, 152, 20, uilook(uiHighlight2), dpage ELSE rectangle 84, 8, 152, 80, uilook(uiHighlight2 * tog), dpage
  o = 0
  FOR i = 0 TO 3
   IF hero(i) > 0 THEN
    wt = 0: IF wptr = i THEN wt = INT(wtogl / 2)
    loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
    drawsprite buffer(), 0, pal16(), o * 16, 89, 8 + i * 20, dpage
    col = uilook(uiMenuItem): IF i = wptr THEN col = uilook(uiSelectedItem + tog)
    temp$ = STR$(ABS(stat(i, 0, 0))) + "/" + STR$(ABS(stat(i, 1, 0)))
    edgeprint temp$, 119, 16 + i * 20, col, dpage
    o = o + 1
   END IF
  NEXT i
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
loadtemppage 3
FOR t = 4 TO 5: carray(t) = 0: NEXT t
EXIT FUNCTION

infostr:
info$ = ""
IF sel >= 0 AND ic = -1 THEN
 IF inventory(sel).used THEN
  info$ = readglobalstring$(41, "Discard", 10) + " " + inventory(sel).text
  IF readbit(permask(), 0, 3 + sel) THEN info$ = readglobalstring$(42, "Cannot", 10) + " " + info$ + "!"
 END IF
END IF
IF ic < 0 THEN RETRACE
IF inventory(ic).used = 0 THEN RETRACE
loaditemdata buffer(), inventory(ic).id
FOR o = 10 TO 9 + buffer(9)
 info$ = info$ + CHR$(buffer(o))
NEXT o
RETRACE

itcontrol:
'--keyboard checking and associated actions for the item menu
IF pick = 0 THEN
 IF carray(5) > 1 THEN
  '--deselect currently selected item
  IF sel > -1 THEN sel = -4 ELSE quit = 1
 END IF
 IF carray(4) > 1 THEN
  '--exit
  IF ic = -3 THEN quit = 1
  '--sort
  IF ic = -2 THEN GOSUB autosort
  IF ic = -1 AND sel >= 0 AND readbit(permask(), 0, 3 + sel) = 0 THEN
   '--try to thow item away
   inventory(sel).text = SPACE$(11)
   inventory(sel).used = 0
   setbit iuse(), 0, 3 + sel, 0
   sel = -4
   GOSUB infostr
   RETRACE
  END IF
  IF sel >= 0 THEN
   IF ic >= 0 AND ic <> sel THEN
    '--swap the selected item and the item under the cursor
    itemmenuswap inventory(), iuse(), permask(), ic, sel
    sel = -4
    RETRACE
   END IF
   IF ic >= 0 AND sel = ic THEN
    '--try to use the current item
    sel = -4
    '--if the usability bit is off, or you dont have any of the item, exit
    IF readbit(iuse(), 0, 3 + ic) = 0 OR inventory(ic).used = 0 THEN RETRACE
    loaditemdata a(), inventory(ic).id
    IF a(50) > 0 THEN '--learn a spell
     tclass = 1
     ttype = 0
     pick = 1: wptr = 0
     '--target the first non-dead hero
     WHILE hero(wptr) = 0 OR stat(wptr, 0, 0) = 0
      wptr = loopvar(wptr, 0, 3, 1)
     WEND
     spred = 0
     RETRACE
    END IF
    IF a(51) > 0 THEN '--attack/oobcure
     loadattackdata buffer(), a(51) - 1
     tclass = buffer(3)
     ttype = buffer(4)
     IF tclass = 0 THEN RETRACE
     wptr = 0
     pick = 1
     spred = 0
     RETRACE
    END IF
    IF a(51) < 0 THEN '--trigger a text box
     IF buffer(73) = 1 THEN dummy = consumeitem(ic)
     items = a(51) * -1
     loadtemppage 3
     RETRIEVESTATE
     EXIT FUNCTION
    END IF
   END IF
  END IF
  IF sel < -3 AND ic >= 0 THEN sel = ic: RETRACE
 END IF
 IF carray(0) > 1 AND ic >= 0 THEN
  ic = ic - 3
  GOSUB infostr
  IF ic < top THEN top = top - 3
 END IF
 IF carray(1) > 1 AND ic <= inventoryMax - 3 THEN
  ic = ic + 3
  GOSUB infostr
  IF ic > top + 62 THEN top = top + 3
 END IF
 IF carray(2) > 1 THEN
  IF (ic MOD 3) = 0 THEN
   ic = ic + 2
  ELSE
   IF ic > -3 THEN ic = ic - 1
  END IF
  GOSUB infostr
 END IF
 IF carray(3) > 1 THEN
  IF ((ic + 3) MOD 3) = 2 THEN ' the +3 adjust for the first negative row
   ic = ic - 2
  ELSE
   IF ic < inventoryMax THEN ic = ic + 1
  END IF
  GOSUB infostr
 END IF
ELSE
 IF carray(5) > 1 THEN
  pick = 0
  GOSUB infostr
  RETRACE
 END IF
 info$ = inventory(ic).text
 IF spred = 0 THEN
  IF carray(0) > 1 THEN
   dummy = getOOBtarg(-1, wptr, 3 + ic, stat(), ondead(), onlive())
  END IF
  IF carray(1) > 1 THEN
   dummy = getOOBtarg(1, wptr, 3 + ic, stat(), ondead(), onlive())
  END IF
 END IF
 IF ttype = 2 THEN
  IF carray(2) > 1 OR carray(3) > 1 THEN
   IF spred = 0 THEN
    FOR i = 0 TO 3
     IF chkOOBtarg(i, 3 + ic, stat(), ondead(), onlive()) THEN spred = spred + 1
    NEXT i
   ELSE
    spred = 0
   END IF
  END IF
 END IF
 IF carray(4) > 1 THEN
  'DO ACTUAL EFFECT
  loaditemdata buffer(), inventory(ic).id
  'if can teach a spell
  didlearn = 0
  IF buffer(50) > 0 THEN
   atk = buffer(50)
   '--trylearn
   didlearn = trylearn(wptr, atk, 0)
   '--announce learn
   IF didlearn = 1 THEN
    tmp$ = names$(wptr) + " " + readglobalstring$(124, "learned", 10) + " " + readattackname$(atk)
    centerbox 160, 100, small(LEN(tmp$) * 8 + 16, 320), 24, 1, vpage
    edgeprint tmp$, large(xstring(tmp$, 160), 0), 95, uilook(uiText), vpage
    setvispage vpage
    dummy = getkey
   END IF
  END IF
  '(do we need to reload?)
  loaditemdata buffer(), inventory(ic).id
  '--do (cure) attack outside of battle
  didcure = 0
  IF buffer(51) > 0 THEN
   atk = buffer(51) - 1
   IF spred = 0 THEN
    IF chkOOBtarg(wptr, 3 + ic, stat(), ondead(), onlive()) THEN oobcure -1, wptr, atk, spred, stat(): didcure = -1
   ELSE
    FOR i = 0 TO 3
     ' IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR readbit(ondead(), 0, 3 + ic)) THEN oobcure -1, i, atk, spred, stat()
     IF chkOOBtarg(i, 3 + ic, stat(), ondead(), onlive()) THEN oobcure -1, i, atk, spred, stat(): didcure = -1
    NEXT i
   END IF
  END IF 'buffer(51) > 0
  IF buffer(73) = 1 AND (didcure OR didlearn = 1) THEN
   IF consumeitem(ic) THEN
    setbit iuse(), 0, 3 + ic, 0: pick = 0: GOSUB infostr
   END IF
  END IF
 END IF ' SPACE or ENTER
END IF
RETRACE

autosort:
FOR i = 0 TO inventoryMax - 1
 FOR o = i + 1 TO inventoryMax
  IF inventory(i).used = 0 AND inventory(o).used THEN
   itemmenuswap inventory(), iuse(), permask(), i, o
   EXIT FOR
  END IF
 NEXT o
NEXT i
FOR i = 0 TO inventoryMax - 1
 FOR o = i + 1 TO inventoryMax
  IF readbit(iuse(), 0, 3 + i) = 0 AND readbit(iuse(), 0, 3 + o) = 1 THEN
   itemmenuswap inventory(), iuse(), permask(), i, o
   EXIT FOR
  END IF
 NEXT o
NEXT i
RETRACE

END FUNCTION

SUB itstr (i)
IF inventory(i).used = 0 THEN
 inventory(i).text = SPACE$(11)
ELSE
 inventory(i).text = readitemname$(inventory(i).id)
 inventory(i).text = rpad$(inventory(i).text, " ", 8) + CHR$(1) + RIGHT$(XSTR$(inventory(i).num), 2)
END IF
END SUB

SUB loadtemppage (page)
'--reads a previously stored page from a temporary file
setdiskpages buffer(), 200, 0
loadpage workingdir$ + SLASH + "temppage.tmp", 0, page
END SUB

SUB oobcure (w, t, atk, spred, stat())

DIM st(13, 1)
dim atksize
atksize = 40 + dimbinsize(binATTACK)
dim attack(atksize)
dim h, h2&
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
h2& = (a * am!) - (d * dm!)
'no elemental support

'extra damage
h2& = h2& + (h2& / 100) * attack(11)

'randomize
IF readbit(attack(), 20, 61) = 0 THEN h2& = rangel(h2&, 20)

'spread damage
IF readbit(attack(), 20, 1) = 1 THEN h2& = h2& / (spred + 1)

'cap out
IF readbit(attack(), 20, 62) = 0 AND h2& <= 0 THEN h2& = 1

'cure bit
IF readbit(attack(), 20, 0) = 1 THEN h2& = ABS(h2&) * -1

'backcompat MP-targstat
IF readbit(attack(), 20, 60) THEN
 IF targstat = 0 THEN targstat = 1
END IF

 chp& = stat(t, 0, targstat)
 mhp& = stat(t, 1, targstat)
 IF readbit(attack(), 65, 5) = 1 THEN
  SELECT CASE attack(5)
   CASE 5'% of max   
    h2& = mhp& + (attack(11) * mhp& / 100)
   CASE 6'% of cur
    h2& = chp& + (attack(11) * chp& / 100)
  END SELECT
 ELSE
  SELECT CASE attack(5) 
   CASE 5'% of max
    h2& = chp& - (mhp& + (attack(11) * mhp& / 100))
   CASE 6'% of cur
    h2& = chp& - (chp& + (attack(11) * chp& / 100))
  END SELECT
 END IF
IF h2& > 32767 THEN h2& = 32767
IF h2& < -32768 THEN h2& = -32768
h = h2&

stat(t, 0, targstat) = stat(t, 0, targstat) - h

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

END SUB

SUB patcharray (array(), n$, max)

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
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT SUB
 IF keyval(72) > 1 THEN csr = large(0, csr - 1)
 IF keyval(80) > 1 THEN csr = small(2, csr + 1)
 IF csr = 0 THEN intgrabber pt, 0, max, 75, 77
 IF csr = 1 THEN intgrabber array(pt), -32768, 32767, 75, 77
 IF csr = 2 THEN
  FOR i = 0 TO 15
   IF keyval(hexk(i)) > 1 THEN setbit array(), pt, i, readbit(array(), pt, i) XOR 1
  NEXT i
 END IF
 num$(0) = n$ + "(" + STR$(ABS(pt)) + ")"
 num$(1) = "value =" + XSTR$(array(pt))
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

FUNCTION picksave (loading)

DIM full(3), herosname$(3), mapname$(3), svtime$(3), lev$(3), id(3, 3), tstat(3, 1, 16), pic(3, 3), confirm$(1), menu$(1)

'--if loading is 2, that means fade the screen in, and loadmenu
'--loading 2 uses page 3 as background and overwrites it. loading 0+1 use dpage and preserve page 3. All wipe page 2
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

'--draw buttons, save current display
IF loading < 2 THEN
 savetemppage 3
 copypage dpage, 3
END IF
centerbox 50, 10, 80, 12, 15, 3
IF loading THEN centerbox 270, 10, 80, 12, 15, 3

FOR i = 0 TO 3
 centerbox 160, 40 + i * 44, 310, 42, 15, 3
 sg$ = savefile$
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
    lev$(i) = readglobalstring$(43, "Level", 10) + XSTR$(tstat(o, 0, 12))
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
     pic(i, o) = tstat(o, 0, 14)
     getpal16 pal16(), 40 + (i * 4) + o, tstat(o, 0, 15), 0, pic(i, o)
    ELSE
     '--backcompat
     dim her as herodef
     loadherodata @her, id(i,o) - 1
     pic(i, o) = her.sprite
     getpal16 pal16(), 40 + (i * 4) + o, her.sprite_pal, 0, pic(i, o)
    END IF
    setpicstuf buffer(), 5120, 2
    loadset game$ + ".pt0", pic(i, o), 0
    loadsprite buffer(), 0, 0, 0, 32, 40, 2
    stosprite buffer(), 0, 0, 16 + (i * 16) + (o * 4), 2
    loadsprite buffer(), 0, 0, 2, 32, 40, 2
    stosprite buffer(), 0, 0, 16 + (i * 16) + (o * 4) + 2, 2
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
  IF loading = 2 THEN clearpage 2 ELSE loadtemppage 3
  picksave = -1
  EXIT FUNCTION
 END IF
END IF


setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 walk = walk XOR tog
 IF loading = 0 THEN playtimer
 control
 IF carray(5) > 1 THEN
  IF loading THEN picksave = -2 ELSE picksave = -1
  EXIT DO
 END IF
 IF cursor = -2 THEN
  IF carray(0) > 1 THEN cursor = 3
  IF carray(1) > 1 THEN cursor = 0
 ELSE
  IF carray(0) > 1 THEN cursor = loopvar(cursor, -1, 3, -1)
  IF carray(1) > 1 THEN cursor = loopvar(cursor, -1, 3, 1)
 END IF
 IF cursor < 0 AND loading THEN
  IF carray(2) > 1 THEN cursor = -1
  IF carray(3) > 1 THEN cursor = -2
 END IF
 IF carray(4) > 1 THEN
  IF cursor < 0 THEN
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
     picksave = cursor
     lastsaveslot = cursor + 1
     EXIT DO
   END IF
  END IF
 END IF
 GOSUB drawmenu
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 IF needf = 1 THEN   'the titlescreen might be skipped and with it the fading in
  needf = 0
  fademusic fmvol
  fadein
 END IF
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
IF loading = 2 THEN
 clearpage 2
ELSE
 loadtemppage 3
END IF
FOR t = 4 TO 5: carray(t) = 0: NEXT t
EXIT FUNCTION

confirm:
allow = 0
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(5) > 1 THEN allow = 0: RETRACE
 IF carray(0) > 1 OR carray(1) > 1 THEN allow = allow XOR 1
 IF carray(4) > 1 THEN RETRACE
 GOSUB drawmenu
 centerbox 160, 14 + (44 * cursor), 40 + (LEN(replacedat$) * 8) + menuwidth, 22, 3, dpage
 edgeprint replacedat$, 200 - (LEN(replacedat$) * 8), 9 + (44 * cursor), uilook(uiText), dpage
 FOR i = 0 TO 1
 col = uilook(uiSelectedItem + tog): IF allow = i THEN col = uilook(uiMenuItem)
  edgeprint confirm$(i), 216, 5 + (i * 9) + (44 * cursor), col, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

drawmenu:
'load and save menus enjoy different colour schemes
IF loading THEN activec = 2 ELSE activec = 1
SELECT CASE cursor
 CASE -2
  centerbox 270, 10, 82, 14, activec, dpage
 CASE -1
  centerbox 50, 10, 82, 14, activec, dpage
 CASE ELSE
  centerbox 160, 40 + cursor * 44, 312, 44, activec, dpage
END SELECT
FOR i = 0 TO 3
 IF full(i) = 1 THEN
  FOR o = 0 TO 3
   IF id(i, o) > 0 THEN
    temp = 16 + (i * 16) + (o * 4)
    IF cursor = i THEN temp = temp + (2 * walk)
    loadsprite buffer(), 0, 0, temp, 32, 40, 2
    drawsprite buffer(), 0, pal16(), (40 + (i * 4) + o) * 16, 140 + (o * 42), 20 + i * 44, dpage
   END IF
  NEXT o
  col = uilook(uiMenuItem)
  IF cursor = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint herosname$(i), 14, 21 + i * 44, col, dpage
  edgeprint lev$(i), 14, 30 + i * 44, col, dpage
  edgeprint svtime$(i), 14, 39 + i * 44, col, dpage
  edgeprint mapname$(i), 14, 48 + i * 44, col, dpage
 END IF
NEXT i
col = uilook(uiMenuItem): IF cursor = -1 THEN col = uilook(uiSelectedItem + tog)
edgeprint menu$(0), xstring(menu$(0), 50), 5, col, dpage
IF loading THEN
 col = uilook(uiMenuItem): IF cursor = -2 THEN col = uilook(uiSelectedItem + tog)
 edgeprint menu$(1), xstring(menu$(1), 270), 5, col, dpage
END IF
RETRACE

END FUNCTION

FUNCTION readglobalstring$ (index, default$, maxlen)
fh = FREEFILE
OPEN game$ + ".stt" FOR BINARY AS #fh

a$ = CHR$(0)
GET #fh, 1 + index * 11, a$
namelen = 0: IF a$ <> "" THEN namelen = ASC(a$)

IF index * 11 + namelen > LOF(fh) THEN
 result$ = default$
ELSE
 result$ = STRING$(small(namelen, maxlen), CHR$(0))
 GET #fh, 2 + index * 11, result$
END IF

CLOSE #fh

readglobalstring = result$
END FUNCTION

FUNCTION rpad$ (s$, pad$, size)
result$ = LEFT$(s$, size)
WHILE LEN(result$) < size: result$ = result$ + pad$: WEND
rpad$ = result$
END FUNCTION

SUB savetemppage (page)
'--writes a page into a temp file for situations where we need more pages than we have
setdiskpages buffer(), 200, 0
storepage workingdir$ + SLASH + "temppage.tmp", 0, page
END SUB

SUB sellstuff (id, storebuf(), stock(), stat())
DIM b(dimbinsize(1) * 50), sname$(40), permask(15), price(200)
recordsize = curbinsize(1) / 2 ' get size in INTs

getnames sname$()

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

GOSUB refreshs

GOSUB sellinfostr
quit = 0
setkeys
DO
 setwait timing(), speedcontrol
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
 edgeprint XSTR$(gold&) + " " + sname$(32), 310 - LEN(XSTR$(gold&) + " " + sname$(32)) * 8, 1, uilook(uiGold), dpage
 IF alert THEN
  alert = alert - 1
  centerbox 160, 178, LEN(alert$) * 8 + 8, 12, 4, dpage
  edgeprint alert$, xstring(alert$, 160), 173, uilook(uiSelectedItem + tog), dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
vishero stat()
EXIT SUB

sellinfostr:
info$ = ""
IF inventory(ic).used = 0 THEN RETRACE
IF readbit(permask(), 0, ic) = 1 THEN info$ = cannotsell$: RETRACE
IF price(ic) > 0 THEN info$ = worth$ + XSTR$(price(ic)) + " " + sname$(32)
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
IF carray(5) > 1 THEN quit = 1
IF carray(4) > 1 AND readbit(permask(), 0, ic) = 0 AND inventory(ic).used THEN
 alert = 10
 alert$ = sold$ + " " + LEFT$(inventory(ic).text, 8)
 'inventory(ic).text = RTRIM$(inventory(ic).text)   '??? There's an itstr(ic) right down there
 'INCREMENT GOLD-----------
 gold& = gold& + price(ic)
 IF gold& > 2000000000 THEN gold& = 2000000000
 IF gold& < 0 THEN gold& = 0
 'CHECK FOR SPECIAL CASES---------
 FOR i = 0 TO storebuf(16)
  IF b(i * recordsize + 17) = 0 AND b(i * recordsize + 18) = inventory(ic).id THEN
   'SET SELL BIT---
   IF b(i * recordsize + 23) <> 0 THEN setbit tag(), 0, ABS(b(i * recordsize + 23)), SGN(SGN(b(i * recordsize + 23)) + 1)
   'ADD TRADED ITEM-----------
   IF b(i * recordsize + 28) > 0 THEN getitem b(i * recordsize + 28), b(i * recordsize + 29) + 1
   'INCREMENT STOCK-------
   IF b(i * recordsize + 26) > 0 THEN
    IF b(i * recordsize + 26) = 1 THEN stock(id, i) = -1
    IF b(i * recordsize + 26) = 2 AND stock(id, i) > 0 THEN stock(id, i) = stock(id, i) + 1
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
END IF
IF carray(0) > 1 AND ic >= 3 THEN
 ic = ic - 3
 GOSUB sellinfostr
 IF ic < top THEN top = top - 3
END IF
IF carray(1) > 1 AND ic <= inventoryMax - 3 THEN
 ic = ic + 3
 GOSUB sellinfostr
 IF ic > top + 62 THEN top = top + 3
END IF
IF carray(2) > 1 THEN
 IF ic MOD 3 > 0 THEN ic = ic - 1: GOSUB sellinfostr ELSE ic = ic + 2: GOSUB sellinfostr
END IF
IF carray(3) > 1 THEN
 IF ic MOD 3 < 2 THEN ic = ic + 1: GOSUB sellinfostr ELSE ic = ic - 2: GOSUB sellinfostr
END IF
RETRACE

refreshs:
FOR i = 0 TO inventoryMax
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
 IF stock(id, i) = 0 THEN stock(id, i) = b(i * recordsize + 19): IF stock(id, i) > -1 THEN stock(id, i) = stock(id, i) + 1
NEXT i
RETRACE

END SUB

SUB spells (pt, stat())
REMEMBERSTATE

DIM sname$(40), menu$(4), mi(4), mtype(5), spel$(24), speld$(24), cost$(24), spel(24), canuse(24), targt(24), spid(5), ondead(2), onlive(2)
dim her as herodef

getnames sname$()

savetemppage 3
copypage dpage, 3

cancelmenu$ = readglobalstring$(51, "(CANCEL)", 10)
hasnone$ = readglobalstring$(133, "has no spells", 20)

pick = 0
spred = 0
wptr = 0
last = 0
sptr = 0
mset = 0

GOSUB splname
copypage vpage, 3
centerfuz 160, 100, 312, 184, 1, 3	'outer box
centerbox 206, 36, 200, 20, 2, 3 	'name box
centerbox 60, 50, 82, 60, 2, 3		'menu box
centerbox 160, 133, 308, 94, 2, 3	'spell list
rectangle 6, 168, 308, 1, uilook(uiTextBox + 3), 3	'divider 2
csr = 0
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 wtogl = loopvar(wtogl, 0, 3, 1)
 playtimer
 control
 GOSUB scontrol
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
  printstr menu$(i), 21, 25 + i * 10, dpage 'spell menu
 NEXT i
 IF last = 0 THEN edgeprint names$(pt) + " " + hasnone$, xstring(names$(pt) + " " + hasnone$, 160), 120, uilook(uiText), dpage
 edgeprint names$(pt), xstring(names$(pt), 206), 31, uilook(uiText), dpage
 IF pick = 1 THEN
  centerbox 196, 47, 160, 88, 2, dpage
  IF spred = 0 THEN rectangle 120, 8 + wptr * 20, 152, 20, uilook(uiHighlight2), dpage ELSE rectangle 120, 8, 152, 80, uilook(uiHighlight2 * tog), dpage
  o = 0
  FOR i = 0 TO 3
   IF hero(i) > 0 THEN
    wt = 0: IF wptr = i THEN wt = INT(wtogl / 2)
    loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
    drawsprite buffer(), 0, pal16(), o * 16, 125, 8 + i * 20, dpage
    temp$ = ABS(stat(i, 0, 0)) & "/" & ABS(stat(i, 1, 0))
    col = uilook(uiMenuItem): IF i = wptr THEN col = uilook(uiSelectedItem + tog)
    edgeprint temp$, 155, 16 + i * 20, col, dpage
    o = o + 1
   END IF
  NEXT i
 END IF

 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

curspellist:
IF mtype(csr) < 0 THEN RETRACE
FOR i = 0 TO 23
 spel$(i) = "": speld$(i) = "": cost$(i) = "": spel(i) = -1: canuse(i) = 0: targt(i) = 0
 IF spell(pt, spid(csr), i) > 0 THEN
  spel(i) = spell(pt, spid(csr), i) - 1
  loadattackdata buffer(), spel(i)
  IF readbit(buffer(), 20, 59) = 1 AND buffer(3) > 0 THEN
   canuse(i) = buffer(3)
   targt(i) = buffer(4)
   setbit ondead(), 0, i, 0
   setbit onlive(), 0, i, 1
   IF buffer(3) = 4 OR buffer(3) = 10 THEN setbit ondead(), 0, i, 1
   IF buffer(3) = 10 THEN setbit onlive(), 0, i, 0
  END IF
  cost = focuscost(buffer(8), stat(pt, 0, 10))
  IF mtype(csr) = 0 AND stat(pt, 0, 1) < cost THEN canuse(i) = 0
  IF mtype(csr) = 1 AND lmp(pt, INT(i / 3)) = 0 THEN canuse(i) = 0
  IF stat(pt, 0, 0) = 0 THEN canuse(i) = 0
  spel$(i) = readbadbinstring$(buffer(), 24, 10, 1)
  speld$(i) = readbinstring$(buffer(),73,38)
  'debug "i = " + XSTR$(i) + ", spel(sptr) = " + XSTR$(spel(sptr))
  IF mtype(csr) = 0 THEN cost$(i) = XSTR$(cost) + " " + sname$(1) + " " + STR$(ABS(stat(pt, 0, 1))) + "/" + STR$(ABS(stat(pt, 1, 1)))
  IF mtype(csr) = 1 THEN cost$(i) = readglobalstring$(43, "Level", 10) + XSTR$(INT(i / 3) + 1) + ":  " + XSTR$(lmp(pt, INT(i / 3)))
 END IF
 WHILE LEN(spel$(i)) < 10: spel$(i) = spel$(i) + " ": WEND
NEXT i
RETRACE

splname:
FOR i = 0 TO 5
 '--for each btl menu slot
 '--clear menu type
 mtype(i) = -1
 '--load hero data
 loadherodata @her, hero(pt) - 1
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
  menu$(last) = ""
  mtype(last) = mtype(o)
  spid(last) = spid(o)

 
  loadherodata @her, hero(pt) - 1

  '--get menu index
  mi(last) = (bmenu(pt, o) + 1) * -1

  '--read menu name
  menu$(last) = her.list_name(mi(last))

  '--old crappy code for reading menu name (very obsolete)
  'FOR j = 244 + temp * 11 TO 243 + temp * 11 + buffer(243 + temp * 11)
  ' menu$(last) = menu$(last) + CHR$(buffer(j))
  'NEXT j

  '--if non-null...
  IF menu$(last) <> "" THEN
   '--right-pad the name
   menu$(last) = rpad$(menu$(last), " ", 10)
   '--increment the spell-list counter because
   '--we only (currently) care about non-null-named spell lists
   last = last + 1
  END IF
 END IF
NEXT o
menu$(last) = rpad$(readglobalstring$(46, "Exit", 10), " ", 10)
mi(last) = -1
mtype(last) = -1
IF csr > last THEN csr = last
GOSUB curspellist
RETRACE

scontrol:
IF pick = 0 THEN
 IF mset = 0 THEN
  IF carray(5) > 1 THEN
   menusound gen(genCancelSFX)
   setkeys
   FOR i = 0 TO 7: carray(i) = 0: NEXT i
   loadtemppage 3
   RETRIEVESTATE
   EXIT SUB
  END IF
  IF carray(2) > 1 THEN DO: pt = loopvar(pt, 0, 3, -1): LOOP UNTIL hero(pt) > 0: menusound gen(genCursorSFX): GOSUB splname
  IF carray(3) > 1 THEN DO: pt = loopvar(pt, 0, 3, 1): LOOP UNTIL hero(pt) > 0: menusound gen(genCursorSFX): GOSUB splname
  IF carray(0) > 1 THEN csr = large(csr - 1, 0): menusound gen(genCursorSFX): GOSUB curspellist
  IF carray(1) > 1 THEN csr = small(csr + 1, last): menusound gen(genCursorSFX): GOSUB curspellist
  IF carray(4) > 1 THEN
   IF mi(csr) = -1 THEN
    menusound gen(genCancelSFX)
    setkeys
    FOR i = 0 TO 7: carray(i) = 0: NEXT i
    loadtemppage 3
    RETRIEVESTATE
    EXIT SUB
   END IF
   menusound gen(genAcceptSFX)
   mset = 1: sptr = 0
  END IF
 ELSE
  IF carray(5) > 1 THEN
   mset = 0
   menusound gen(genCancelSFX)
  END IF
  IF carray(0) > 1 THEN sptr = sptr - 3: menusound gen(genCursorSFX): IF sptr < 0 THEN sptr = 24
  IF carray(1) > 1 THEN
   IF sptr < 24 THEN
    sptr = small(sptr + 3, 24)
   ELSE
    sptr = 0
   END IF
   menusound gen(genCursorSFX)
  END IF
  IF sptr < 24 THEN
   IF carray(2) > 1 THEN
    IF sptr MOD 3 THEN
     sptr = sptr - 1
    ELSE
     sptr = sptr + 2
    END IF
    menusound gen(genCursorSFX)
   END IF
   IF carray(3) > 1 THEN menusound gen(genCursorSFX): IF sptr MOD 3 = 2 THEN sptr = sptr - 2 ELSE sptr = sptr + 1
  END IF
  IF carray(4) > 1 THEN
   IF sptr = 24 THEN mset = 0
   IF canuse(sptr) > 0 THEN '{
    '--spell that can be used oob
    wptr = pt
    pick = 1
    spred = 0
    menusound gen(genAcceptSFX)
    IF targt(sptr) = 1 AND canuse(sptr) <> 2 THEN
     FOR i = 0 TO 3
      'IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR readbit(ondead(), 0, sptr)) THEN spred = spred + 1
      IF chkOOBtarg(i, sptr, stat(), ondead(), onlive()) THEN spred = spred + 1
     NEXT i
    END IF
   ELSE
    menusound gen(genCancelSFX)
   END IF '}
  END IF
 END IF
ELSE
 IF carray(5) > 1 THEN pick = 0
 IF canuse(sptr) <> 2 AND spred = 0 THEN
  IF carray(0) > 1 THEN
   'DO: wptr = loopvar(wptr, 0, 3, -1): LOOP UNTIL hero(wptr) > 0 AND (stat(wptr, 0, 0) > 0 OR readbit(ondead(), 0, sptr))
   dummy = getOOBtarg(-1, wptr, sptr, stat(), ondead(), onlive())
  END IF
  IF carray(1) > 1 THEN
   'DO: wptr = loopvar(wptr, 0, 3, 1): LOOP UNTIL hero(wptr) > 0 AND (stat(wptr, 0, 0) > 0 OR readbit(ondead(), 0, sptr))
   dummy = getOOBtarg(1, wptr, sptr, stat(), ondead(), onlive())
  END IF
 END IF
 IF targt(sptr) = 2 AND canuse(sptr) <> 2 THEN
  IF carray(2) > 1 OR carray(3) > 1 THEN
   IF spred = 0 THEN
    FOR i = 0 TO 3
     'IF hero(i) > 0 AND (stat(wptr, 0, 0) > 0 OR readbit(ondead(), 0, sptr)) THEN spred = spred + 1
     IF chkOOBtarg(i, sptr, stat(), ondead(), onlive()) THEN spred = spred + 1
    NEXT i
   ELSE
    spred = 0
   END IF
  END IF
 END IF
 IF carray(4) > 1 THEN
  IF mtype(csr) = 0 THEN
   loadattackdata buffer(), spel(sptr)
   cost = focuscost(buffer(8), stat(pt, 0, 10))
   IF cost > stat(pt, 0, 1) THEN pick = 0: RETRACE
   stat(pt, 0, 1) = small(large(stat(pt, 0, 1) - cost, 0), stat(pt, 1, 1))
  END IF
  IF mtype(csr) = 1 THEN
   IF lmp(pt, INT(sptr / 3)) = 0 THEN pick = 0: RETRACE
   lmp(pt, INT(sptr / 3)) = lmp(pt, INT(sptr / 3)) - 1
  END IF
  'DO ACTUAL EFFECT
  IF spred = 0 THEN
   IF chkOOBtarg(wptr, sptr, stat(), ondead(), onlive()) THEN oobcure pt, wptr, spel(sptr), spred, stat()
  ELSE
   FOR i = 0 TO 3
    'IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR readbit(ondead(), 0, sptr)) THEN oobcure pt, i, spel(sptr), spred, stat()
    IF chkOOBtarg(i, sptr, stat(), ondead(), onlive()) THEN oobcure pt, i, spel(sptr), spred, stat()
   NEXT i
  END IF
  GOSUB curspellist
 END IF
END IF
RETRACE

END SUB

SUB status (pt, stat())
DIM sname$(40), sno(9), mtype(5), hbits(3, 4), thishbits(4), elemtype$(2), info$(25)
dim her as herodef

savetemppage 3
copypage dpage, 3

getnames sname$()
sname$(33) = readglobalstring$(33, "Experience", 10)
sname$(34) = readglobalstring$(43, "Level", 10)
elemtype$(0) = readglobalstring(127, "Weak to", 10)
elemtype$(1) = readglobalstring(128, "Strong to", 10)
elemtype$(2) = readglobalstring(129, "Absorbs", 10)

sno(0) = 2
sno(1) = 3
sno(2) = 5
sno(3) = 6
sno(4) = 29
sno(5) = 30
sno(6) = 8
sno(7) = 7
sno(8) = 31
sno(9) = 4

'--calculate bitsets with equipment
FOR i = 0 TO 3
 herobattlebits hbits(), i
NEXT i

mode = 0
top = 0
lastinfo = 0

GOSUB nextstat
copypage vpage, 3
centerfuz 160, 100, 304, 184, 1, 3
centerbox 160, 36, 260, 40, 4, 3

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(5) > 1 THEN
 	menusound gen(genCancelSFX)
 	loadtemppage 3
 	FOR t = 4 TO 5
 		carray(t) = 0
 	NEXT t
 	EXIT SUB
 END IF
 IF carray(4) > 1 THEN mode = loopvar(mode, 0, 2, 1): menusound gen(genCursorSFX)
 IF carray(2) > 1 THEN DO: pt = loopvar(pt, 0, 3, -1): LOOP UNTIL hero(pt) > 0: menusound gen(genCursorSFX): GOSUB nextstat
 IF carray(3) > 1 THEN DO: pt = loopvar(pt, 0, 3, 1): LOOP UNTIL hero(pt) > 0: menusound gen(genCursorSFX): GOSUB nextstat
 IF carray(0) > 1 THEN top = large(top - 1, 0): menusound gen(genCursorSFX)
 IF carray(1) > 1 THEN top = small(top + 1, large(0, lastinfo - 11)): menusound gen(genCursorSFX)

 SELECT CASE mode
  CASE 0
   centerbox 84, 120, 140, 120, 4, dpage
   centerbox 236, 120, 140, 120, 4, dpage
  CASE 1, 2
   centerbox 160, 120, 292, 120, 4, dpage
 END SELECT

 edgeprint names$(pt), 160 - LEN(names$(pt)) * 4, 20, uilook(uiText), dpage
 edgeprint sname$(34) + XSTR$(stat(pt, 0, 12)), 160 - LEN(sname$(34) + STR$(stat(pt, 0, 12))) * 4, 30, uilook(uiText), dpage
 temp$ = STR$(exlev&(pt, 1) - exlev&(pt, 0)) + " " + sname$(33) + " " + readglobalstring$(47, "for next", 10) + " " + sname$(34)
 edgeprint temp$, 160 - LEN(temp$) * 4, 40, uilook(uiText), dpage

 SELECT CASE mode
  CASE 0
   '--show stats
   FOR i = 0 TO 9
    edgeprint sname$(sno(i)), 20, 62 + i * 10, uilook(uiText), dpage
    temp$ = XSTR$(stat(pt, 0, i + 2))
    edgeprint temp$, 148 - LEN(temp$) * 8, 62 + i * 10, uilook(uiText), dpage
   NEXT i

   'current/max HP
   edgeprint sname$(0), 236 - LEN(sname$(0)) * 4, 65, uilook(uiText), dpage
   temp$ = STR$(ABS(stat(pt, 0, 0))) + "/" + STR$(ABS(stat(pt, 1, 0)))
   edgeprint temp$, 236 - LEN(temp$) * 4, 75, uilook(uiText), dpage

   '--MP and level MP
   FOR i = 0 TO 5
    IF mtype(i) = 0 THEN
     edgeprint sname$(1), 236 - LEN(sname$(1)) * 4, 95, uilook(uiText), dpage
     temp$ = STR$(ABS(stat(pt, 0, 1))) + "/" + STR$(ABS(stat(pt, 1, 1)))
     edgeprint temp$, 236 - LEN(temp$) * 4, 105, uilook(uiText), dpage
    END IF
    IF mtype(i) = 1 THEN
     edgeprint sname$(34) + " " + sname$(1), 236 - LEN(sname$(34) + " " + sname$(1)) * 4, 125, uilook(uiText), dpage
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
   edgeprint STR$(gold&) + " " + sname$(32), 236 - LEN(STR$(gold&) + " " + sname$(32)) * 4, 167, uilook(uiGold), dpage
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
 copypage 3, dpage
 dowait
LOOP

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
   info$(lastinfo) = elemtype$(o) + " " + sname$(17 + i)
   lastinfo = lastinfo + 1
  END IF
 NEXT i
NEXT o
IF lastinfo = 0 THEN info$(lastinfo) = readglobalstring$(130, "No Elemental Effects", 30): lastinfo = lastinfo + 1

FOR i = lastinfo TO 25
 info$(i) = ""
NEXT i
RETRACE

END SUB

FUNCTION trylearn (who, atk, learntype)

'CLOBBERS BUFFER!

'--returns 1 when the spell was learned, 0 when it was not learned

'--fail by default
result = 0

dim her as herodef
'--pre-populate buffer() with the hero's data.
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
ol = getbinsize(1) / 2 'old size on disk
nw = curbinsize(1) / 2 'new size in memory
flusharray array(), nw * 50, 0
'load shop data from STF lump
setpicstuf buffer(), ol * 2 * 50, -1
loadset game$ + ".stf", id, 0
'in case shop data has been resized, scale records to new size
FOR i = 0 TO ol - 1
 FOR o = 0 to 49
  array(o * nw + i) = buffer(o * ol + i)
 NEXT o
NEXT i
END SUB
