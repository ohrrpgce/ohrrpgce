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
DECLARE FUNCTION readatkname$ (id%)
DECLARE SUB herobattlebits (bitbuf%(), who%)
DECLARE SUB unequip (who%, where%, defwep%, stat%(), resetdw%)
DECLARE SUB loadtemppage (page%)
DECLARE SUB savetemppage (page%)
DECLARE FUNCTION readitemname$ (itemnum%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE FUNCTION readbadbinstring$ (array%(), offset%, maxlen%, skipword%)
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB getLongName (filename$, outfile$)
DECLARE SUB vishero (stat%())
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE SUB doequip (toequip%, who%, where%, defwep%, stat%())
DECLARE FUNCTION playtime$ (d%, h%, m%)
DECLARE SUB playtimer ()
DECLARE FUNCTION averagelev% (stat%())
DECLARE FUNCTION istag% (num%, zero%)
DECLARE SUB evalherotag (stat%())
DECLARE SUB delitem (it%)
DECLARE SUB getmapname (mapname$, m%)
DECLARE FUNCTION consumeitem% (index%)
DECLARE SUB evalitemtag ()
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION usemenu% (ptr%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
DECLARE SUB intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB itstr (i%)
DECLARE SUB control ()
DECLARE FUNCTION picksave% (load%)
DECLARE SUB equip (ptr%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB getitem (getit%)
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%())
DECLARE SUB spells (ptr%, stat%())
DECLARE SUB status (ptr%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
DECLARE SUB resetlmp (slot%, lev%)
DECLARE SUB loadfoe (i%, formdata%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), stat%(), ebits%(), batname$())
DECLARE FUNCTION inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%())
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE SUB addhero (who%, slot%, stat%())
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION xstring% (s$, x%)
DECLARE SUB snapshot ()

'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'gglobals.bi'
'$INCLUDE: 'sglobals.bi'

'$INCLUDE: 'const.bi'

REM $STATIC
FUNCTION bound (n, lowest, highest)
bound = n
IF n < lowest THEN bound = lowest
IF n > highest THEN bound = highest
END FUNCTION

SUB buystuff (id, shoptype, storebuf(), stock(), stat())
DIM b(1600), stuf$(50), vmask(5), emask(5), sname$(40), buytype$(5, 1), wbuf(100), walks(15), hpal(8)

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
eqprefix$ = readglobalstring$(99, "Equip:", 10)
noroom$ = readglobalstring$(100, "No Room in Party", 20)

FOR i = 0 TO 10 STEP 2
 walks(i) = 1
NEXT i
walks(11) = 2
walks(12) = 2
walks(13) = 3
walks(14) = 3

setpicstuf b(), 3200, -1
loadset game$ + ".stf" + CHR$(0), id, 0
FOR o = 0 TO storebuf(16)
 stuf$(o) = ""
 FOR i = 1 TO small(b(o * 32 + 0), 16)
  IF b(o * 32 + i) >= 0 AND b(o * 32 + i) < 256 THEN stuf$(o) = stuf$(o) + CHR$(b(o * 32 + i))
 NEXT i
NEXT o

GOSUB setstock
GOSUB stufmask
IF total = 0 THEN EXIT SUB

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
   IF b(pt * 32 + 22) THEN setbit tag(), 0, ABS(b(pt * 32 + 22)), SGN(SGN(b(pt * 32 + 22)) + 1)
   gold& = gold& - b(pt * 32 + 24)
   IF b(pt * 32 + 25) > 0 THEN '---TRADE IN ITEM----------
    delitem b(pt * 32 + 25)
   END IF '-------END TRADE IN ITEM----------------------------
   IF b(pt * 32 + 17) = 0 THEN '---BUY ITEM-------------------
    getitem b(pt * 32 + 18) + 1
    acol = 4
    alert = 10
    alert$ = purchased$ + " " + stuf$(pt)
   END IF '-------END IF ITEM-------------------------------------
   IF b(pt * 32 + 17) = 1 THEN '---HIRE HERO------------------
    'getitem b(pt * 32 + 18) + 1
    FOR i = 37 TO 0 STEP -1
     IF hero(i) = 0 THEN slot = i
    NEXT i
    addhero b(pt * 32 + 18) + 1, slot, stat()
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
 centerbox 240, 20, LEN(STR$(gold&) + " " + sname$(32)) * 8 + 8, 12, 4, dpage
 edgeprint STR$(gold&) + " " + sname$(32), xstring(STR$(gold&) + " " + sname$(32) + " ", 240), 15, 15, dpage
 o = 0
 edgeprint stuf$(pt), xstring(stuf$(pt), 240), 30 + o * 10, 7, dpage: o = o + 1
 IF info1$ <> "" THEN edgeprint info1$, xstring(info1$, 240), 30 + o * 10, 8, dpage: o = o + 1
 IF info2$ <> "" THEN edgeprint info2$, xstring(info2$, 240), 30 + o * 10, 8, dpage: o = o + 1
 IF eqinfo$ <> "" THEN edgeprint eqinfo$, xstring(eqinfo$, 240), 30 + o * 10, 7, dpage: o = o + 1
 IF stock(id, pt) > 1 THEN
  edgeprint STR$(stock(id, pt) - 1) + " " + instock$ + " ", xstring(STR$(stock(id, pt) - 1) + " in stock ", 240), 30 + o * 10, 7, dpage: o = o + 1
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
   c = 7: IF pt = i THEN c = 14 + tog
   IF readbit(emask(), 0, i) THEN c = 8: IF pt = i THEN c = 7 + tog
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
  centerbox 160, 190, LEN(price$) * 8 + 8, 12, 1, dpage
  edgeprint price$, xstring(price$, 160), 185, 15, dpage
 END IF
 IF alert THEN
  alert = alert - 1
  centerbox 160, 178, LEN(alert$) * 8 + 8, 12, acol, dpage
  edgeprint alert$, xstring(alert$, 160), 173, 14 + tog, dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 IF needf = 1 THEN needf = 0: fadetopal master(), buffer(): setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
vishero stat()
EXIT SUB

curinfo:
showhero = -1
price$ = ""
eqinfo$ = ""
info1$ = ""
info2$ = ""
IF b(pt * 32 + 24) > 0 THEN price$ = RIGHT$(STR$(b(pt * 32 + 24)), LEN(STR$(b(pt * 32 + 24))) - 1) + " " + sname$(32)
IF b(pt * 32 + 25) > 0 THEN
 IF price$ = "" THEN
  price$ = buytype$(0, shoptype)
 ELSE
  price$ = price$ + " " + anda$
 END IF
 price$ = price$ + readitemname$(b(pt * 32 + 25) - 1)
 'setpicstuf buffer(), 200, -1
 'loadset game$ + ".itm" + CHR$(0), b(pt * 32 + 25) - 1, 0
 'FOR o = 1 TO buffer(0)
 ' price$ = price$ + CHR$(small(large(buffer(o), 0), 255))
 'NEXT o
END IF
IF b(pt * 32 + 17) = 0 THEN
 setpicstuf buffer(), 200, -1
 loadset game$ + ".itm" + CHR$(0), b(pt * 32 + 18), 0
 IF buffer(49) = 1 THEN eqinfo$ = eqprefix$ + " " + wepslot$
 IF buffer(49) > 1 THEN eqinfo$ = eqprefix$ + " " + sname$(23 + buffer(49))
 info1$ = readbadbinstring$(buffer(), 9, 40, 0)
 'FOR i = 1 TO buffer(9)
 ' info1$ = info1$ + CHR$(small(large(buffer(i + 9), 0), 255))
 'NEXT i
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
IF b(pt * 32 + 17) = 1 THEN
 'hire
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), b(pt * 32 + 18), 0
 setpicstuf wbuf(), 200, -1
 loadset game$ + ".itm" + CHR$(0), buffer(22), 0
 IF buffer(21) < 0 THEN buffer(21) = averagelev(stat())
 temp$ = STR$(atlevel(buffer(21), buffer(23 + 0 * 2), buffer(24 + 0 * 2)) + wbuf(54 + 0))
 eqinfo$ = RIGHT$(temp$, LEN(temp$) - 1) + " " + sname$(0)
 showhero = buffer(17)
 getpal16 hpal(), 0, buffer(18)
 setpicstuf buffer(), 5120, 2
 loadset game$ + ".pt0" + CHR$(0), showhero, 0
 IF eslot = 0 THEN info1$ = noroom$
END IF
RETURN

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
 IF b(i * 32 + 17) = (shoptype XOR 1) THEN setbit vmask(), 0, i, 1
 IF NOT istag(b(i * 32 + 20), -1) THEN setbit vmask(), 0, i, 1
 IF b(i * 32 + 24) > gold& THEN setbit emask(), 0, i, 1
 IF b(i * 32 + 25) > 0 THEN
  setbit emask(), 0, i, 1
  FOR o = 0 TO 199
   lb = (item(o) AND 255)
   hb = INT(item(o) / 256)
   IF b(i * 32 + 25) = lb AND hb > 0 AND b(i * 32 + 24) <= gold& THEN setbit emask(), 0, i, 0
  NEXT o
 END IF
 '---PREVENT PARTY OVERFLOW
 IF b(i * 32 + 17) = 1 AND eslot = 0 THEN setbit emask(), 0, i, 1
 IF readbit(vmask(), 0, i) = 0 THEN total = total + 1
NEXT i
RETURN

setstock:
FOR i = 0 TO storebuf(16)
 '--for each shop-stuff
 IF stock(id, i) = 0 THEN
  '--if unloaded, reload stock
  stock(id, i) = b(i * 32 + 19)
  '--zero means unloaded, 1 means no-stock, 2+n means 1+n in stock
  IF stock(id, i) > -1 THEN stock(id, i) = stock(id, i) + 1
 END IF
NEXT i
RETURN

END SUB

SUB centerbox (x, y, w, h, c, p)
rectangle x - INT(w * .5), y - INT(h * .5), w, h, c * 16 + 2, p
rectangle x - INT(w * .5), y - INT(h * .5), w, 1, c * 16 + 12, p
rectangle x - INT(w * .5), y + (h - INT(h * .5)), w, 1, c * 16 + 12, p
rectangle x - INT(w * .5), y - INT(h * .5), 1, h, c * 16 + 12, p
rectangle x + (w - INT(w * .5)), y - INT(h * .5), 1, h + 1, c * 16 + 12, p
END SUB

SUB centerfuz (x, y, w, h, c, p)
fuzzyrect x - INT(w * .5), y - INT(h * .5), w, h, c * 16 + 2, p
rectangle x - INT(w * .5), y - INT(h * .5), w, 1, c * 16 + 12, p
rectangle x - INT(w * .5), y + (h - INT(h * .5)), w, 1, c * 16 + 12, p
rectangle x - INT(w * .5), y - INT(h * .5), 1, h, c * 16 + 12, p
rectangle x + (w - INT(w * .5)), y - INT(h * .5), 1, h + 1, c * 16 + 12, p
END SUB

FUNCTION chkOOBtarg (wptr, index, stat(), ondead(), onlive())
'return true if valid, false if not valid
chkOOBtarg = -1
IF hero(wptr) = 0 OR (stat(wptr, 0, 0) = 0 AND readbit(ondead(), 0, index) = 0) OR (stat(wptr, 0, 0) > 0 AND readbit(onlive(), 0, index) = 0) THEN
 chkOOBtarg = 0
END IF
END FUNCTION

SUB doequip (toequip, who, where, defwep, stat())

'--load the item data for this equipment
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), toequip - 1, 0

'--apply the stat bonuses
FOR i = 0 TO 11
 'stat bonuses
 stat(who, 1, i) = stat(who, 1, i) + buffer(54 + i)
 IF i > 1 THEN stat(who, 0, i) = stat(who, 1, i)
 stat(who, 0, i) = small(stat(who, 0, i), stat(who, 1, i))
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
 delitem toequip
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
  IF carray(0) > 1 THEN csr2 = large(csr2 - 1, toff(csr)): GOSUB stbonus: IF csr2 < top THEN top = top - 1
  IF carray(1) > 1 THEN csr2 = small(csr2 + 1, toff(csr) + tlim(csr) + 1): GOSUB stbonus: IF csr2 > top + 17 THEN top = top + 1
  IF carray(4) > 1 THEN
   IF csr2 = toff(csr) + tlim(csr) + 1 THEN
    '--unequip
    unequip pt, csr, dw, stat(), 1
    GOSUB EquBacktomenuSub
   ELSE
    '--normal equip
    ie = (item(eq(csr2)) AND 255)
    GOSUB newequip
   END IF
  END IF
 END IF
 
 '--display
 edgeprint names$(pt), 84 - LEN(names$(pt)) * 4, 12, 15, dpage
 FOR i = 0 TO 11
  edgeprint sname$(sno(i)), 20, 42 + i * 10, 7, dpage
  col = 7
  IF stb(i) < 0 THEN col = 8
  IF stb(i) > 0 THEN col = 14 + tog
  temp$ = STR$(stat(pt, 1, i) + stb(i))
  edgeprint temp$, 148 - LEN(temp$) * 8, 42 + i * 10, col, dpage
 NEXT i
 IF mset = 0 THEN
  '--main menu display
  FOR i = 0 TO 6
   textcolor 7, 1
   IF i < 5 THEN
    IF eqstuf(pt, i) = 0 AND tlim(i) < 0 THEN textcolor 7, 18
   END IF
   IF csr = i THEN
    textcolor 14 + tog, 1 + tog
    IF i < 5 THEN IF tlim(i) < 0 THEN textcolor 14 + tog, 2
   END IF
   printstr menu$(i), 204, 45 + i * 9, dpage
  NEXT i
  IF csr < 5 THEN
   centerbox 236, 20, (LEN(m$(csr)) + 2) * 8, 20, 4, dpage
   edgeprint m$(csr), 236 - (LEN(m$(csr)) * 4), 16, 15, dpage
  END IF
 END IF
 IF mset = 1 THEN
  '--change equipment menu
  centerbox 236, 100, 96, 152, 4, dpage
  FOR i = top TO top + 17
   textcolor 7, 0
   IF i = csr2 THEN textcolor 14 + tog, 2
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
    printstr item$(eq(i)), 192, 28 + (i - top) * 8, dpage
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

setpicstuf buffer(), 200, -1

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
 lb = (item(eq(csr2)) AND 255)
END IF

IF lb = -1 THEN
 '--nothing to load!
 FOR i = 0 TO 11
  stb(i) = 0
 NEXT i
ELSE
 loadset game$ + ".itm" + CHR$(0), lb - 1, 0
 FOR i = 0 TO 11
  stb(i) = buffer(54 + i)
 NEXT i
END IF

IF eqstuf(pt, csr) > 0 THEN
 loadset game$ + ".itm" + CHR$(0), eqstuf(pt, csr) - 1, 0
 FOR i = 0 TO 11
  stb(i) = stb(i) - buffer(54 + i)
 NEXT i
END IF

RETURN

newequip:
unequip pt, csr, dw, stat(), 0
doequip ie, pt, csr, dw, stat()
GOSUB EquBacktomenuSub
RETURN

EquBacktomenuSub:
mset = 0
FOR i = 0 TO 11
 stb(i) = 0
NEXT i
GOSUB setupeq
RETURN

setupeq:
'setpicstuf buffer(), 636, -1
'loadset game$ + ".dt0" + CHR$(0), hero(pt) - 1, 0
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
setpicstuf buffer(), 200, -1
FOR i = 0 TO 197
 lb = (item(i) AND 255)
 IF lb > 0 THEN
  '--load item data
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
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

RETURN
END SUB

SUB getitem (getit)

i = 0
DO
 lb = (item(i) AND 255)
 hb = INT(item(i) / 256)
 IF getit = lb AND hb < 99 THEN
  item(i) = lb + ((hb + 1) * 256)
  itstr i
  EXIT SUB
 END IF
 i = i + 1
LOOP UNTIL i > 197
i = 0
DO
 lb = (item(i) AND 255)
 IF lb = 0 THEN item(i) = getit + 256: itstr i: EXIT SUB
 i = i + 1
LOOP UNTIL i > 199
getit = 0
EXIT SUB

RETURN

END SUB

SUB getLongName (filename$, outfile$)
'--given a filename, returns its longname.
'  it will always return the filename only, without the path
'  even though it can accept a fully qualified filename as input

'--has a bug that prevents it from returning files that are longer
'  than 260 chars including pathname

failed = 0
result$ = ""
length = LongNameLength(filename$ + CHR$(0))
IF length = -1 THEN
 '--failed to get any name at all
 failed = -1
ELSE
 a$ = STRING$(length, 0)
 getstring a$
 FOR i = LEN(a$) TO 1 STEP -1
  IF MID$(a$, i, 1) = "\" OR MID$(a$, i, 1) = ":" THEN EXIT FOR
  IF MID$(a$, i, 1) <> CHR$(0) THEN
   result$ = MID$(a$, i, 1) + result$
  END IF
 NEXT i
 IF result$ = "" THEN
  '--never return a null result!
  failed = -1
 END IF
END IF
IF failed THEN
 '--failed, return input (minus path)
 FOR i = LEN(filename$) TO 1 STEP -1
  IF MID$(filename$, i, 1) = "\" OR MID$(filename$, i, 1) = ":" THEN EXIT FOR
  result$ = MID$(filename$, i, 1) + result$
 NEXT i
END IF
outfile$ = result$
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

FUNCTION items (stat())
DIM a(100), iuse(15), ondead(15), onlive(15), permask(15)

savetemppage 3
copypage dpage, 3

FOR i = 0 TO 2
 setbit iuse(), 0, i, 1
NEXT i
FOR i = 0 TO 199
 setbit ondead(), 0, 3 + i, 0
 setbit onlive(), 0, 3 + i, 1
 setpicstuf buffer(), 200, -1
 lb = (item(i) AND 255)
 IF lb > 0 THEN
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
  IF buffer(73) = 2 THEN setbit permask(), 0, 3 + i, 1
  IF buffer(51) > 0 OR buffer(50) > 0 THEN
   setbit iuse(), 0, 3 + i, 1
   temp = buffer(51) - 1
   setpicstuf buffer(), 80, -1
   loadset game$ + ".dt6" + CHR$(0), temp, 0
   IF buffer(3) = 4 OR buffer(3) = 10 THEN setbit ondead(), 0, 3 + i, 1
   IF buffer(3) = 10 THEN setbit onlive(), 0, 3 + i, 0
  END IF
  IF buffer(51) < 0 THEN
   setbit iuse(), 0, 3 + i, 1
  END IF
 END IF
NEXT i
ic = -3: top = -3: sel = -4
centerbox 160, 92, 304, 176, 1, 3

GOSUB infostr
setkeys
quit = 0
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 GOSUB itcontrol
 IF quit THEN EXIT DO
 FOR i = top TO top + 62
  textcolor 8, 0
  IF readbit(iuse(), 0, 3 + i) = 1 THEN textcolor 7, 0
  IF readbit(permask(), 0, 3 + i) THEN textcolor 6, 0
  IF ic = i THEN
   textcolor 7, 2
   IF readbit(iuse(), 0, 3 + i) = 1 THEN textcolor 15, 2
   IF readbit(permask(), 0, 3 + i) THEN textcolor 14, 2
  END IF
  IF sel = i THEN
   textcolor 7, 1 + tog
   IF ic = i THEN textcolor 14 + tog, 1 + tog
  END IF
  printstr item$(i), 20 + 96 * (((i / 3) - INT(i / 3)) * 3), 12 + 8 * INT((i - top) / 3), dpage
 NEXT i
 centerfuz 160, 180, 312, 20, 4, dpage
 edgeprint info$, xstring(info$, 160), 175, 15, dpage
 IF pick = 1 THEN
  centerbox 160, 47, 160, 88, 2, dpage
  IF spred = 0 THEN rectangle 84, 8 + wptr * 20, 152, 20, 2, dpage ELSE rectangle 84, 8, 152, 80, 2 * tog, dpage
  o = 0
  FOR i = 0 TO 3
   IF hero(i) > 0 THEN
    wt = 0: IF wptr = i THEN wt = INT(wtogl / 2)
    loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
    drawsprite buffer(), 0, pal16(), o * 16, 89, 8 + i * 20, dpage
    col = 7: IF i = wptr THEN col = 14 + tog
    temp$ = RIGHT$(STR$(stat(i, 0, 0)), LEN(STR$(stat(i, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(i, 1, 0)), LEN(STR$(stat(i, 1, 0))) - 1)
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
 IF item(sel) > 0 THEN
  info$ = readglobalstring$(41, "Discard", 10) + " " + item$(sel)
  IF readbit(permask(), 0, 3 + sel) THEN info$ = readglobalstring$(42, "Cannot", 10) + " " + info$ + "!"
 END IF
END IF
IF item(ic) = 0 OR ic < 0 THEN RETURN
lb = (item(ic) AND 255)
hb = INT(item(ic) / 256)
setpicstuf buffer(), 200, -1
IF lb > 0 THEN
 loadset game$ + ".itm" + CHR$(0), lb - 1, 0
 FOR o = 10 TO 9 + buffer(9)
  info$ = info$ + CHR$(buffer(o))
 NEXT o
END IF
RETURN

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
  '--try to thow item away
  IF ic = -1 AND sel > -4 AND readbit(permask(), 0, 3 + sel) = 0 THEN item$(sel) = "           ": item(sel) = 0: setbit iuse(), 0, 3 + sel, 0: sel = -4: GOSUB infostr: RETURN
  IF sel >= 0 THEN
   IF ic >= 0 AND ic <> sel THEN
    '--swap the selected item and the item under the cursor
    SWAP item(ic), item(sel)
    SWAP item$(ic), item$(sel)
    t1 = readbit(iuse(), 0, 3 + ic)
    t2 = readbit(iuse(), 0, 3 + sel)
    SWAP t1, t2
    setbit iuse(), 0, 3 + ic, t1
    setbit iuse(), 0, 3 + sel, t2
    t1 = readbit(permask(), 0, 3 + ic)
    t2 = readbit(permask(), 0, 3 + sel)
    SWAP t1, t2
    setbit permask(), 0, 3 + ic, t1
    setbit permask(), 0, 3 + sel, t2
    sel = -4
    RETURN
   END IF
   IF ic >= 0 AND sel = ic THEN
    '--try to use the current item
    sel = -4
    lb = (item(ic) AND 255)
    hb = INT(item(ic) / 256)
    '--if the usability bit is off, or you dont have any of the item, exit
    IF readbit(iuse(), 0, 3 + ic) = 0 OR lb = 0 THEN RETURN
    setpicstuf a(), 200, -1
    loadset game$ + ".itm" + CHR$(0), lb - 1, 0
    IF a(50) > 0 THEN '--learn a spell
     tclass = 1
     ttype = 0
     pick = 1: wptr = 0
     '--target the first non-dead hero
     WHILE hero(wptr) = 0 OR stat(wptr, 0, 0) = 0
      wptr = loopvar(wptr, 0, 3, 1)
     WEND
     spred = 0
     RETURN
    END IF
    IF a(51) > 0 THEN '--attack/oobcure
     setpicstuf buffer(), 80, -1
     loadset game$ + ".dt6" + CHR$(0), a(51) - 1, 0
     tclass = buffer(3)
     ttype = buffer(4)
     IF tclass = 0 THEN RETURN
     wptr = 0
     pick = 1
     spred = 0
     RETURN
    END IF
    IF a(51) < 0 THEN '--trigger a text box
     IF buffer(73) = 1 THEN dummy = consumeitem(ic)
     items = a(51) * -1
     loadtemppage 3
     EXIT FUNCTION
    END IF
   END IF
  END IF
  IF sel < -3 AND ic >= 0 THEN sel = ic: RETURN
 END IF
 IF carray(0) > 1 AND ic >= 0 THEN ic = ic - 3: GOSUB infostr: IF ic < top THEN top = top - 3
 IF carray(1) > 1 AND ic < 195 THEN ic = ic + 3: GOSUB infostr: IF ic > top + 62 THEN top = top + 3
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
   IF ic < 197 THEN ic = ic + 1
  END IF
  GOSUB infostr
 END IF
ELSE
 info$ = item$(ic)
 IF carray(5) > 1 THEN pick = 0: GOSUB infostr: RETURN
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
  lb = (item(ic) AND 255)
  hb = INT(item(ic) / 256)
  'DO ACTUAL EFFECT
  setpicstuf buffer(), 200, -1
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
  'if can teach a spell
  didlearn = 0
  IF buffer(50) > 0 THEN
   atk = buffer(50)
   '--trylearn
   didlearn = trylearn(wptr, atk, 0)
   '--announce learn
   IF didlearn = 1 THEN
    tmp$ = names$(wptr) + " " + readglobalstring$(124, "learned", 10) + " " + readatkname$(atk)
    centerbox 160, 100, small(LEN(tmp$) * 8 + 16, 320), 24, 1, vpage
    edgeprint tmp$, large(xstring(tmp$, 160), 0), 95, 15, vpage
    dummy = igetkey
   END IF
  END IF
  setpicstuf buffer(), 200, -1
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
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
    setbit iuse(), 0, 3 + ic, 0: info$ = "": pick = 0: GOSUB infostr
   END IF
  END IF
 END IF ' SPACE or ENTER
END IF
RETURN

autosort:
FOR i = 0 TO 196
 FOR o = i + 1 TO 197
  IF item(i) = 0 AND item(o) <> 0 THEN
   GOSUB swapitem
   EXIT FOR
  END IF
 NEXT o
NEXT i
FOR i = 0 TO 196
 FOR o = i + 1 TO 197
  IF readbit(iuse(), 0, 3 + i) = 0 AND readbit(iuse(), 0, 3 + o) = 1 THEN
   GOSUB swapitem
   EXIT FOR
  END IF
 NEXT o
NEXT i
RETURN

swapitem:
SWAP item(i), item(o)
SWAP item$(i), item$(o)
t1 = readbit(iuse(), 0, 3 + i)
t2 = readbit(iuse(), 0, 3 + o)
SWAP t1, t2
setbit iuse(), 0, 3 + i, t1
setbit iuse(), 0, 3 + o, t2
t1 = readbit(permask(), 0, 3 + i)
t2 = readbit(permask(), 0, 3 + o)
SWAP t1, t2
setbit permask(), 0, 3 + i, t1
setbit permask(), 0, 3 + o, t2
RETURN

END FUNCTION

SUB itstr (i)

item$(i) = "           "
IF item(i) = 0 THEN EXIT SUB
item$(i) = ""
lb = (item(i) AND 255)
hb = INT(item(i) / 256)
item$(i) = readitemname$(lb - 1)
item$(i) = rpad$(item$(i), " ", 8) + CHR$(1) + RIGHT$(STR$(hb), 2)

END SUB

SUB loadtemppage (page)
'--reads a previously stored page from a temporary file
setdiskpages buffer(), 200, 0
loadpage workingdir$ + "\temppage.tmp" + CHR$(0), 0, page
END SUB

SUB oobcure (w, t, atk, spred, stat())
DIM st(13, 1)

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

'load attack data
setpicstuf buffer(), 80, -1
loadset game$ + ".dt6" + CHR$(0), atk, 0
targstat = buffer(18)

'defence base
a = st(2, 0): d = st(4, 0)
IF buffer(7) = 1 THEN a = st(6, 0): d = st(7, 0)
IF buffer(7) = 2 THEN a = st(0, 0)
IF buffer(7) = 3 THEN a = (st(0, 1) - st(0, 0))
IF buffer(7) = 4 THEN a = INT(RND * 999)
IF buffer(7) = 5 THEN a = 100
IF buffer(7) >= 6 THEN a = st(buffer(7) - 6, 0)

'calc defence
am! = 1: dm! = .5
IF buffer(5) = 1 THEN am! = .8: dm! = .1
IF buffer(5) = 2 THEN am! = 1.3: dm! = 1
IF buffer(5) = 3 THEN am! = 1: dm! = 0

'resetting
IF readbit(buffer(), 20, 57) = 1 THEN
 stat(t, 0, targstat) = stat(t, 1, targstat)
END IF

'calc harm
h = (a * am!) - (d * dm!)

'no elemental support

'extra damage
h = h + (h / 100) * buffer(11)

'randomize
IF readbit(buffer(), 20, 61) = 0 THEN h = range(h, 20)

'spread damage
IF readbit(buffer(), 20, 1) = 1 THEN h = h / (spred + 1)

'cap out
h = large(h, 1 - readbit(buffer(), 20, 62))

'cure bit
IF readbit(buffer(), 20, 0) = 1 THEN h = ABS(h) * -1

'backcompat MP-targstat
IF readbit(buffer(), 20, 60) THEN
 IF targstat = 0 THEN targstat = 1
END IF

SELECT CASE buffer(5)
 CASE 5'% of max
  stat(t, 0, targstat) = stat(t, 1, targstat) + (stat(t, 1, targstat) / 100 * buffer(11))
 CASE 6'% of cur
  stat(t, 0, targstat) = stat(t, 0, targstat) + (stat(t, 0, targstat) / 100 * buffer(11))
 CASE ELSE'normal
  stat(t, 0, targstat) = stat(t, 0, targstat) - h
  IF readbit(buffer(), 20, 2) AND w >= 0 THEN
   'drain
   stat(w, 0, targstat) = stat(w, 0, targstat) + h
  END IF
END SELECT

'bounds
stat(t, 0, targstat) = large(stat(t, 0, targstat), 0)
IF w >= 0 THEN stat(w, 0, targstat) = large(stat(w, 0, targstat), 0)
'bitset 58 allows cure to exceed maximum
IF readbit(buffer(), 20, 58) = 0 THEN
 stat(t, 0, targstat) = small(stat(t, 0, targstat), stat(t, 1, targstat))
 IF w >= 0 THEN stat(w, 0, targstat) = small(stat(w, 0, targstat), stat(w, 1, targstat))
ELSE
 'increase maximum as well if not hp or mp
 IF targstat > 1 THEN
  stat(t, 1, targstat) = large(stat(t, 0, targstat), stat(t, 1, targstat))
  IF w >= 0 THEN stat(w, 0, targstat) = large(stat(w, 0, targstat), stat(w, 1, targstat))
 END IF
END IF

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
 num$(0) = n$ + "(" + RIGHT$(STR$(pt), LEN(STR$(pt)) - 1) + ")"
 num$(1) = "value =" + STR$(array(pt))
 num$(2) = ""
 FOR i = 0 TO 15
  IF readbit(array(), pt, i) THEN
   num$(2) = num$(2) + "1"
  ELSE
   num$(2) = num$(2) + "0"
  END IF
 NEXT i
 edgeprint "DEBUG MODE", 120, 50, 15, dpage
 centerbox 160, 100, 140, 60, 1, dpage
 FOR i = 0 TO 2
  c = 7: IF i = csr THEN c = 14 + tog
  edgeprint num$(i), 160 - LEN(num$(i)) * 4, 80 + i * 10, c, dpage
 NEXT i
 edgeprint "0123456789ABCDEF", 96, 110, 6, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

END SUB

FUNCTION picksave (loading)

DIM full(3), herosname$(3), mapname$(3), svtime$(3), lev$(3), id(3, 3), tstat(3, 1, 16), pic(3, 3), confirm$(1), menu$(1)

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
IF loading THEN
 centerbox 50, 10, 80, 12, 15, 3
 centerbox 270, 10, 80, 12, 15, 3
ELSE
 savetemppage 3
 copypage dpage, 3
 centerbox 50, 10, 80, 12, 15, 3
END IF

FOR i = 0 TO 3
 centerbox 160, 40 + i * 44, 310, 42, 15, 3
 sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
 setpicstuf buffer(), 30000, -1
 loadset sg$ + CHR$(0), i * 2, 0
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
    lev$(i) = readglobalstring$(43, "Level", 10) + STR$(tstat(o, 0, 12))
   END IF
  NEXT o
  '--load second record
  loadset sg$ + CHR$(0), i * 2 + 1, 0
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
     getpal16 pal16(), 40 + (i * 4) + o, tstat(o, 0, 15)
    ELSE
     '--backcompat
     setpicstuf buffer(), 636, -1
     loadset game$ + ".dt0" + CHR$(0), id(i, o) - 1, 0
     pic(i, o) = buffer(17)
     getpal16 pal16(), 40 + (i * 4) + o, buffer(18)
    END IF
    setpicstuf buffer(), 5120, 2
    loadset game$ + ".pt0" + CHR$(0), pic(i, o), 0
    loadsprite buffer(), 0, 0, 0, 32, 40, 2
    stosprite buffer(), 0, 0, 16 + (i * 16) + (o * 4), 2
    loadsprite buffer(), 0, 0, 2, 32, 40, 2
    stosprite buffer(), 0, 0, 16 + (i * 16) + (o * 4) + 2, 2
   END IF
  NEXT o
  getmapname mapname$(i), map
 END IF
NEXT i

IF loading THEN
 'check for no slots
 nofull = 0
 FOR i = 0 TO 3
  IF full(i) = 1 THEN nofull = 1
 NEXT i
 IF nofull = 0 THEN picksave = -1: clearpage 2: EXIT FUNCTION
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
 dowait
LOOP
IF loading THEN
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
 IF carray(5) > 1 THEN allow = 0: RETURN
 IF carray(0) > 1 OR carray(1) > 1 THEN allow = allow XOR 1
 IF carray(4) > 1 THEN RETURN
 GOSUB drawmenu
 centerbox 160, 14 + (44 * cursor), 40 + (LEN(replacedat$) * 8) + menuwidth, 22, 3, dpage
 edgeprint replacedat$, 200 - (LEN(replacedat$) * 8), 9 + (44 * cursor), 15, dpage
 FOR i = 0 TO 1
 col = 14 + tog: IF allow = i THEN col = 7
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
  col = 7
  IF cursor = i THEN col = 14 + tog
  edgeprint herosname$(i), 14, 21 + i * 44, col, dpage
  edgeprint lev$(i), 14, 30 + i * 44, col, dpage
  edgeprint svtime$(i), 14, 39 + i * 44, col, dpage
  edgeprint mapname$(i), 14, 48 + i * 44, col, dpage
 END IF
NEXT i
col = 7: IF cursor = -1 THEN col = 14 + tog
edgeprint menu$(0), xstring(menu$(0), 50), 5, col, dpage
IF loading THEN
 col = 7: IF cursor = -2 THEN col = 14 + tog
 edgeprint menu$(1), xstring(menu$(1), 270), 5, col, dpage
END IF
RETURN

END FUNCTION

FUNCTION readatkname$ (id)
setpicstuf buffer(), 80, -1
loadset game$ + ".dt6" + CHR$(0), id - 1, 0
readatkname$ = readbadbinstring$(buffer(), 24, 10, 1)
END FUNCTION

FUNCTION readglobalstring$ (index, default$, maxlen)
fh = FREEFILE
OPEN game$ + ".stt" FOR BINARY AS #fh

a$ = CHR$(0)
GET #fh, 1 + index * 11, a$
namelen = 0: IF a$ <> "" THEN namelen = ASC(a$)

IF index * 11 + i > LOF(fh) THEN
 result$ = default$
ELSE
 result$ = STRING$(small(namelen, maxlen), CHR$(0))
 GET #fh, 2 + index * 11, result$
END IF

CLOSE #fh

readglobalstring = result$
END FUNCTION

FUNCTION readitemname$ (itemnum)
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), itemnum, 0
readitemname$ = readbadbinstring$(buffer(), 0, 8, 0)
END FUNCTION

FUNCTION rpad$ (s$, pad$, size)
result$ = LEFT$(s$, size)
WHILE LEN(result$) < size: result$ = result$ + pad$: WEND
rpad$ = result$
END FUNCTION

SUB savetemppage (page)
'--writes a page into a temp file for situations where we need more pages than we have
setdiskpages buffer(), 200, 0
storepage workingdir$ + "\temppage.tmp" + CHR$(0), 0, page
END SUB

SUB sellstuff (id, storebuf(), stock(), stat())
DIM b(1600), sname$(40), permask(15), price(200)

getnames sname$()

cannotsell$ = readglobalstring$(75, "CANNOT SELL", 20)
worth$ = readglobalstring$(77, "Worth", 20)
tradefor$ = readglobalstring$(79, "Trade for", 20)
anda$ = readglobalstring$(81, "and a", 10)
worthnothing$ = readglobalstring$(82, "Worth Nothing", 20)
sold$ = readglobalstring$(84, "Sold", 10)

setpicstuf b(), 3200, -1
loadset game$ + ".stf" + CHR$(0), id, 0
GOSUB selstock

ic = 0: top = 0

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
  textcolor 7, 0
  IF readbit(permask(), 0, i) THEN textcolor 8, 0
  IF ic = i THEN
   textcolor 14 + tog, 2
   IF readbit(permask(), 0, i) THEN textcolor 14, 2
  END IF
  printstr item$(i), 20 + 96 * (((i / 3) - INT(i / 3)) * 3), 12 + 8 * INT((i - top) / 3), dpage
 NEXT i
 centerfuz 160, 180, 312, 20, 4, dpage
 edgeprint info$, xstring(info$, 160), 175, 15, dpage
 edgeprint STR$(gold&) + " " + sname$(32), 310 - LEN(STR$(gold&) + " " + sname$(32)) * 8, 1, 14, dpage
 IF alert THEN
  alert = alert - 1
  centerbox 160, 178, LEN(alert$) * 8 + 8, 12, 4, dpage
  edgeprint alert$, xstring(alert$, 160), 173, 14 + tog, dpage
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
IF item(ic) = 0 THEN RETURN
IF readbit(permask(), 0, ic) = 1 THEN info$ = cannotsell$: RETURN
lb = (item(ic) AND 255)
hb = INT(item(ic) / 256)
IF lb > 0 THEN
 IF price(ic) > 0 THEN info$ = worth$ + STR$(price(ic)) + " " + sname$(32)
 FOR i = 0 TO storebuf(16)
  IF b(i * 32 + 17) = 0 AND b(i * 32 + 18) = lb - 1 THEN
   IF b(i * 32 + 28) > 0 THEN
    IF info$ = "" THEN info$ = tradefor$ + " " ELSE info$ = info$ + " " + anda$ + " "
    info$ = info$ + readitemname$(b(i * 32 + 28) - 1)
    'setpicstuf buffer(), 200, -1
    'loadset game$ + ".itm" + CHR$(0), b(i * 32 + 28) - 1, 0
    'FOR o = 1 TO buffer(0)
    ' info$ = info$ + CHR$(small(large(buffer(o), 0), 255))
    'NEXT o
   END IF
  END IF
 NEXT i
 IF info$ = "" THEN info$ = worthnothing$
END IF
RETURN

keysell:
IF carray(5) > 1 THEN quit = 1
IF carray(4) > 1 AND readbit(permask(), 0, ic) = 0 AND item(ic) > 0 THEN
 alert = 10: alert$ = sold$ + " " + LEFT$(item$(ic), 8): WHILE RIGHT$(item$(ic), 1) = " ": item$(ic) = LEFT$(item$(ic), LEN(item$(ic)) - 1): WEND
 'INCREMENT GOLD-----------
 gold& = gold& + price(ic)
 IF gold& > 2000000000 THEN gold& = 2000000000
 IF gold& < 0 THEN gold& = 0
 'CHECK FOR SPECIAL CASES---------
 FOR i = 0 TO storebuf(16)
  IF b(i * 32 + 17) = 0 AND b(i * 32 + 18) = lb - 1 THEN
   'SET SELL BIT---
   IF b(i * 32 + 23) <> 0 THEN setbit tag(), 0, ABS(b(i * 32 + 23)), SGN(SGN(b(i * 32 + 23)) + 1)
   'ADD TRADED ITEM-----------
   IF b(i * 32 + 28) > 0 THEN getitem b(i * 32 + 28)
   'INCREMENT STOCK-------
   IF b(i * 32 + 26) > 0 THEN
    IF b(i * 32 + 26) = 1 THEN stock(id, i) = -1
    IF b(i * 32 + 26) = 2 AND stock(id, i) > 0 THEN stock(id, i) = stock(id, i) + 1
   END IF
  END IF
 NEXT i
 'DECREMENT ITEM-----------
 lb = (item(ic) AND 255)
 hb = INT(item(ic) / 256)
 hb = hb - 1: IF hb = 0 THEN lb = 0
 item(ic) = lb + (hb * 256)
 itstr ic
 'UPDATE ITEM POSESSION TAGS--------
 evalitemtag
 'REFRESH DISPLAY--------
 GOSUB refreshs
 GOSUB sellinfostr
END IF
IF carray(0) > 1 AND ic >= 3 THEN ic = ic - 3: GOSUB sellinfostr: IF ic < top THEN top = top - 3
IF carray(1) > 1 AND ic < 195 THEN ic = ic + 3: GOSUB sellinfostr: IF ic > top + 62 THEN top = top + 3
IF carray(2) > 1 THEN
 IF ((ic / 3) - INT(ic / 3)) * 3 > 0 THEN ic = ic - 1: GOSUB sellinfostr ELSE ic = ic + 2: GOSUB sellinfostr
END IF
IF carray(3) > 1 THEN
 IF ((ic / 3) - INT(ic / 3)) * 3 < 2 THEN ic = ic + 1: GOSUB sellinfostr ELSE ic = ic - 2: GOSUB sellinfostr
END IF
RETURN

refreshs:
FOR i = 0 TO 199
 setpicstuf buffer(), 200, -1
 lb = (item(i) AND 255)
 IF lb > 0 THEN
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
  IF buffer(73) = 2 THEN setbit permask(), 0, i, 1
  price(i) = INT(buffer(46) * .5)
  FOR o = 0 TO storebuf(16)
   IF b(o * 32 + 18) = lb - 1 THEN
    IF ABS(b(o * 32 + 21)) > 0 THEN IF readbit(tag(), 0, ABS(b(o * 32 + 21))) <> SGN(SGN(b(o * 32 + 21)) + 1) THEN setbit permask(), 0, i, 1
    IF b(o * 32 + 17) = 0 THEN
     price(i) = b(o * 32 + 27)
     IF b(o * 32 + 26) = 3 THEN setbit permask(), 0, i, 1
    END IF
   END IF
  NEXT o
 END IF
NEXT i
RETURN

selstock:
FOR i = 0 TO storebuf(16)
 IF stock(id, i) = 0 THEN stock(id, i) = b(i * 32 + 19): IF stock(id, i) > -1 THEN stock(id, i) = stock(id, i) + 1
NEXT i
RETURN

END SUB

SUB spells (pt, stat())
DIM sname$(40), menu$(4), mi(4), mtype(5), spel$(24), cost$(24), spel(24), canuse(24), targt(24), spid(5), ondead(2), onlive(2)
getnames sname$()

savetemppage 3
copypage dpage, 3

cancelmenu$ = readglobalstring$(51, "(CANCEL)", 10)
hasnone$ = readglobalstring$(133, "has no spells", 20)

GOSUB splname
copypage vpage, 3
centerfuz 160, 100, 304, 184, 1, 3
centerbox 206, 36, 200, 20, 2, 3
centerbox 60, 50, 82, 60, 2, 3
centerbox 160, 135, 280, 80, 2, 3
rectangle 21, 164, 280, 1, 40, 3
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
    textcolor 8 - SGN(canuse(o)), 0
    IF sptr = o AND mset = 1 THEN
     IF canuse(o) > 0 THEN textcolor 14 + tog, 1 ELSE textcolor 7, 1
    END IF
    printstr spel$(o), 24 + (((o / 3) - INT(o / 3)) * 3) * 88, 98 + INT(o / 3) * 8, dpage
   NEXT o
   textcolor 7, 0
   IF sptr = 24 AND mset = 1 THEN textcolor 14 + tog, 1
   printstr cancelmenu$, 24, 166, dpage
   IF mset = 1 THEN
    textcolor 10, 0
    printstr cost$(sptr), 288 - LEN(cost$(sptr)) * 8, 166, dpage
   END IF
  END IF
  textcolor 7, 0
  IF csr = i THEN textcolor 14 + tog, 2: IF mset = 1 THEN textcolor 7, 2
  printstr menu$(i), 21, 25 + i * 10, dpage
 NEXT i
 IF last = 0 THEN edgeprint names$(pt) + " " + hasnone$, xstring(names$(pt) + " " + hasnone$, 160), 120, 15, dpage
 edgeprint names$(pt), xstring(names$(pt), 206), 31, 15, dpage
 IF pick = 1 THEN
  centerbox 196, 47, 160, 88, 2, dpage
  IF spred = 0 THEN rectangle 120, 8 + wptr * 20, 152, 20, 2, dpage ELSE rectangle 120, 8, 152, 80, 2 * tog, dpage
  o = 0
  FOR i = 0 TO 3
   IF hero(i) > 0 THEN
    wt = 0: IF wptr = i THEN wt = INT(wtogl / 2)
    loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
    drawsprite buffer(), 0, pal16(), o * 16, 125, 8 + i * 20, dpage
    temp$ = RIGHT$(STR$(stat(i, 0, 0)), LEN(STR$(stat(i, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(i, 1, 0)), LEN(STR$(stat(i, 1, 0))) - 1)
    col = 7: IF i = wptr THEN col = 14 + tog
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
IF mtype(csr) < 0 THEN RETURN
FOR i = 0 TO 23
 spel$(i) = "": cost$(i) = "": spel(i) = -1: canuse(i) = 0: targt(i) = 0
 IF spell(pt, spid(csr), i) > 0 THEN
  spel(i) = spell(pt, spid(csr), i) - 1
  setpicstuf buffer(), 80, -1
  loadset game$ + ".dt6" + CHR$(0), spel(i), 0
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
  IF mtype(csr) = 0 THEN cost$(i) = STR$(cost) + " " + sname$(1) + " " + RIGHT$(STR$(stat(pt, 0, 1)), LEN(STR$(stat(pt, 0, 1))) - 1) + "/" + RIGHT$(STR$(stat(pt, 1, 1)), LEN(STR$(stat(pt, 1, 1))) - 1)
  IF mtype(csr) = 1 THEN cost$(i) = readglobalstring$(43, "Level", 10) + STR$(INT(i / 3) + 1) + ":  " + STR$(lmp(pt, INT(i / 3)))
 END IF
 WHILE LEN(spel$(i)) < 10: spel$(i) = spel$(i) + " ": WEND
NEXT i
RETURN

splname:
FOR i = 0 TO 5
 '--for each btl menu slot
 '--clear menu type
 mtype(i) = -1
 '--load hero data
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), hero(pt) - 1, 0
 '--if it is a menu...
 IF bmenu(pt, i) < 0 AND bmenu(pt, i) > -10 THEN
  '--set spell-menu-id and menu-type
  spid(i) = (bmenu(pt, i) + 1) * -1
  mtype(i) = buffer(288 + spid(i))
 END IF
NEXT i
last = 0
FOR o = 0 TO 5
 IF mtype(o) >= 0 AND mtype(o) < 2 THEN
  menu$(last) = ""
  mtype(last) = mtype(o)
  spid(last) = spid(o)
  
  '--load herodata
  setpicstuf buffer(), 636, -1
  loadset game$ + ".dt0" + CHR$(0), hero(pt) - 1, 0
  
  '--get menu index
  mi(last) = (bmenu(pt, o) + 1) * -1
  
  '--read menu name
  menu$(last) = readbadbinstring$(buffer(), 243 + mi(last) * 11, 10, 0)
  
  '--old crappy code for reading menu name
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
RETURN

scontrol:
IF pick = 0 THEN
 IF mset = 0 THEN
  IF carray(5) > 1 THEN
   setkeys
   FOR i = 0 TO 7: carray(i) = 0: NEXT i
   loadtemppage 3
   EXIT SUB
  END IF
  IF carray(2) > 1 THEN DO: pt = loopvar(pt, 0, 3, -1): LOOP UNTIL hero(pt) > 0: GOSUB splname
  IF carray(3) > 1 THEN DO: pt = loopvar(pt, 0, 3, 1): LOOP UNTIL hero(pt) > 0: GOSUB splname
  IF carray(0) > 1 THEN csr = large(csr - 1, 0): GOSUB curspellist
  IF carray(1) > 1 THEN csr = small(csr + 1, last): GOSUB curspellist
  IF carray(4) > 1 THEN
   IF mi(csr) = -1 THEN
    setkeys
    FOR i = 0 TO 7: carray(i) = 0: NEXT i
    loadtemppage 3
    EXIT SUB
   END IF
   mset = 1: sptr = 0
  END IF
 ELSE
  IF carray(5) > 1 THEN mset = 0
  IF carray(0) > 1 THEN sptr = sptr - 3: IF sptr < 0 THEN sptr = 24
  IF carray(1) > 1 THEN
   IF sptr < 24 THEN sptr = small(sptr + 3, 24) ELSE sptr = 0
  END IF
  IF carray(2) > 1 THEN IF (((sptr / 3) - INT(sptr / 3)) * 3) > 0 THEN sptr = sptr - 1 ELSE sptr = small(sptr + 2, 24)
  IF sptr < 24 THEN
   IF carray(3) > 1 THEN IF (((sptr / 3) - INT(sptr / 3)) * 3) < 2 THEN sptr = sptr + 1 ELSE sptr = sptr - 2
  END IF
  IF carray(4) > 1 THEN
   IF sptr = 24 THEN mset = 0
   IF canuse(sptr) > 0 THEN
    '--spell that can be used oob
    wptr = pt
    pick = 1
    spred = 0
    IF targt(sptr) = 1 AND canuse(sptr) <> 2 THEN
     FOR i = 0 TO 3
      'IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR readbit(ondead(), 0, sptr)) THEN spred = spred + 1
      IF chkOOBtarg(i, sptr, stat(), ondead(), onlive()) THEN spred = spred + 1
     NEXT i
    END IF
   END IF
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
   setpicstuf buffer(), 80, -1
   loadset game$ + ".dt6" + CHR$(0), spel(sptr), 0
   cost = focuscost(buffer(8), stat(pt, 0, 10))
   IF cost > stat(pt, 0, 1) THEN pick = 0: RETURN
   stat(pt, 0, 1) = small(large(stat(pt, 0, 1) - cost, 0), stat(pt, 1, 1))
  END IF
  IF mtype(csr) = 1 THEN
   IF lmp(pt, INT(sptr / 3)) = 0 THEN pick = 0: RETURN
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
RETURN

END SUB

SUB status (pt, stat())
DIM sname$(40), sno(9), mtype(5), hbits(3, 4), thishbits(4), elemtype$(2), info$(25)

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
 IF carray(5) > 1 THEN loadtemppage 3: FOR t = 4 TO 5: carray(t) = 0: NEXT t: EXIT SUB
 IF carray(4) > 1 THEN mode = loopvar(mode, 0, 2, 1)
 IF carray(2) > 1 THEN DO: pt = loopvar(pt, 0, 3, -1): LOOP UNTIL hero(pt) > 0: GOSUB nextstat
 IF carray(3) > 1 THEN DO: pt = loopvar(pt, 0, 3, 1): LOOP UNTIL hero(pt) > 0: GOSUB nextstat
 IF carray(0) > 1 THEN top = large(top - 1, 0)
 IF carray(1) > 1 THEN top = small(top + 1, large(0, lastinfo - 11))
 
 SELECT CASE mode
  CASE 0
   centerbox 84, 120, 140, 120, 4, dpage
   centerbox 236, 120, 140, 120, 4, dpage
  CASE 1, 2
   centerbox 160, 120, 292, 120, 4, dpage
 END SELECT
 
 edgeprint names$(pt), 160 - LEN(names$(pt)) * 4, 20, 15, dpage
 edgeprint sname$(34) + STR$(stat(pt, 0, 12)), 160 - LEN(sname$(34) + STR$(stat(pt, 0, 12))) * 4, 30, 15, dpage
 temp$ = LTRIM$(STR$(exlev&(pt, 1) - exlev&(pt, 0))) + " " + sname$(33) + " " + readglobalstring$(47, "for next", 10) + " " + sname$(34)
 edgeprint temp$, 160 - LEN(temp$) * 4, 40, 15, dpage
 
 SELECT CASE mode
  CASE 0
   '--show stats
   FOR i = 0 TO 9
    edgeprint sname$(sno(i)), 20, 62 + i * 10, 15, dpage
    edgeprint STR$(stat(pt, 0, i + 2)), 148 - LEN(STR$(stat(pt, 0, i + 2))) * 8, 62 + i * 10, 15, dpage
   NEXT i
   
   'current/max HP
   edgeprint sname$(0), 236 - LEN(sname$(0)) * 4, 65, 15, dpage
   edgeprint RIGHT$(STR$(stat(pt, 0, 0)), LEN(STR$(stat(pt, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(pt, 1, 0)), LEN(STR$(stat(pt, 1, 0))) - 1), 236 - LEN(RIGHT$(STR$(stat(pt, 0, 0)), LEN(STR$(stat(pt, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(pt, 0 _
, 0)), LEN(STR$(stat(pt, 0, 0))) - 1)) * 4, 75, 15, dpage
   
   '--MP and level MP
   FOR i = 0 TO 5
    IF mtype(i) = 0 THEN
     edgeprint sname$(1), 236 - LEN(sname$(1)) * 4, 95, 15, dpage
     edgeprint RIGHT$(STR$(stat(pt, 0, 1)), LEN(STR$(stat(pt, 0, 1))) - 1) + "/" + RIGHT$(STR$(stat(pt, 1, 1)), LEN(STR$(stat(pt, 1, 1))) - 1), 236 - LEN(RIGHT$(STR$(stat(pt, 0, 1)), LEN(STR$(stat(pt, 0, 1))) - 1) + "/" + RIGHT$(STR$(stat(pt _
, 0, 1)), LEN(STR$(stat(pt, 0, 1))) - 1)) * 4, 105, 15, dpage
    END IF
    IF mtype(i) = 1 THEN
     edgeprint sname$(34) + " " + sname$(1), 236 - LEN(sname$(34) + " " + sname$(1)) * 4, 125, 15, dpage
     temp$ = ""
     FOR o = 0 TO 3
      temp$ = temp$ + RIGHT$(STR$(lmp(pt, o)), LEN(STR$(lmp(pt, o))) - 1) + "/"
     NEXT o
     temp$ = LEFT$(temp$, LEN(temp$) - 1)
     edgeprint temp$, 236 - LEN(temp$) * 4, 135, 15, dpage
     temp$ = ""
     FOR o = 4 TO 7
      temp$ = temp$ + RIGHT$(STR$(lmp(pt, o)), LEN(STR$(lmp(pt, o))) - 1) + "/"
     NEXT o
     temp$ = LEFT$(temp$, LEN(temp$) - 1)
     edgeprint temp$, 236 - LEN(temp$) * 4, 145, 15, dpage
    END IF
   NEXT i
   
   '--gold
   edgeprint LTRIM$(STR$(gold&)) + " " + sname$(32), 236 - LEN(LTRIM$(STR$(gold&)) + " " + sname$(32)) * 4, 167, 14, dpage
  CASE 1
   
   '--show elementals
   FOR i = 0 TO 10
    IF top + i <= 25 THEN edgeprint info$(top + i), 20, 62 + i * 10, 15, dpage
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
setpicstuf buffer(), 636, -1
loadset game$ + ".dt0" + CHR$(0), hero(pt) - 1, 0

FOR i = 0 TO 5
 mtype(i) = -1
 IF bmenu(pt, i) < 0 AND bmenu(pt, i) > -10 THEN
  temp = (bmenu(pt, i) + 1) * -1
  IF buffer(243 + temp * 11) > 0 THEN mtype(i) = buffer(288 + temp)
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
RETURN

END SUB

FUNCTION trylearn (who, atk, learntype)

'CLOBBERS BUFFER!

'--returns 1 when the spell was learned, 0 when it was not learned

'--fail by default
result = 0

'--pre-populate buffer() with the hero's data.
setpicstuf buffer(), 636, -1
loadset game$ + ".dt0" + CHR$(0), hero(who) - 1, 0

'--for each spell list
FOR j = 0 TO 3
 '--for each spell slot
 FOR o = 0 TO 23
  '--if this slot is empty and accepts this spell
  '--and is learnable by learntype
  k = (j * 48) + (o * 2)
  IF spell(who, j, o) = 0 AND buffer(47 + k) = atk AND buffer(48 + k) = learntype THEN
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
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), eqstuf(who, where) - 1, 0

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
 getitem eqstuf(who, where)
END IF

'--blank out equipment
eqstuf(who, where) = 0

IF where = 0 AND resetdw THEN
 '--restore default weapon
 doequip defwep, who, where, defwep, stat()
END IF

END SUB

FUNCTION usemenu (pt, top, first, last, size)

oldptr = pt
oldtop = top

IF keyval(72) > 1 THEN pt = large(pt - 1, first)
IF keyval(80) > 1 THEN pt = small(pt + 1, last)
IF keyval(73) > 1 THEN pt = large(pt - size, first)
IF keyval(81) > 1 THEN pt = small(pt + size, last)
top = bound(top, pt - size, pt)

IF olptr = pt AND oldtop = top THEN
 usemenu = 0
ELSE
 usemenu = 1
END IF

END FUNCTION

