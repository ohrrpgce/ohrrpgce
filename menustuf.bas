DECLARE FUNCTION playtime$ (d%, h%, m%)
DECLARE SUB playtimer ()
DECLARE FUNCTION averagelev% (stat%(), hero%())
DECLARE FUNCTION istag% (tag%(), num%, zero%)
DECLARE SUB evalherotag (tag%(), herobits%(), hero%(), stat%(), leader%)
DECLARE SUB delitem (it%, item%(), item$())
DECLARE SUB getmapname (mapname$, m%)
DECLARE FUNCTION consumeitem% (index%, item%(), item$())
DECLARE SUB evalitemtag (tag%(), itembits%(), hero%(), eqstuf%(), item%())
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION usemenu% (ptr%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
DECLARE SUB intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB itstr (i%, item%(), item$())
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB control (carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), herox%, heroy%, mmode%, timing%())
DECLARE FUNCTION pickload% (svcsr%, pal%(), timing%(), dpage%, vpage%, carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), sourcerpg$)
DECLARE FUNCTION picksave% (svcsr%, pal%(), timing%(), dpage%, vpage%, carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), sourcerpg$)
DECLARE SUB equip (ptr%, hero%(), stat%(), name$(), timing%(), vpage%, dpage%, item%(), item$(), eqstuf%(), bmenu%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), tag%(), itembits%())
DECLARE FUNCTION items% (item%(), item$(), hero%(), stat%(), name$(), timing%(), vpage%, dpage%, bmenu%(), spell%(), pal%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB getitem (getit%, item%(), item$())
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%(), hero%())
DECLARE SUB spells (ptr%, hero%(), stat%(), name$(), timing%(), vpage%, dpage%, bmenu%(), lmp%(), spell%(), pal%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB status (ptr%, hero%(), stat%(), name$(), exlev&(), gold&, timing%(), vpage%, dpage%, bmenu%(), lmp%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB getnames (stat$())
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
DECLARE SUB resetlmp (lmp%(), slot%, lev%)
DECLARE SUB loadfoe (i%, a%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), name$(), stat%(), ebits%())
DECLARE SUB inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%())
DECLARE SUB battle (form%, fatal%, hero%(), pal%(), timing%(), exstat%(), bmenu%(), spell%(), lmp%(), gold&, exlev&(), item%(), item$(), eqstuf%(), fmvol%, carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB addhero (who%, slot%, hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(), eqstuf%())
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION xstring% (s$, x%)
DECLARE SUB snapshot (vpage%)
'assembly subs and functions
DECLARE SUB setmodex ()
DECLARE SUB restoremode ()
DECLARE SUB setpicstuf (buf(), BYVAL b, BYVAL p)
DECLARE SUB loadset (fil$, BYVAL i, BYVAL l)
DECLARE SUB storeset (fil$, BYVAL i, BYVAL l)
DECLARE SUB copypage (BYVAL page1, BYVAL page2)
DECLARE SUB setvispage (BYVAL page)
DECLARE SUB drawsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB wardsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB getsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB loadsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB stosprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB setdiskpages (buf(), BYVAL h, BYVAL l)
DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
DECLARE SUB fadeto (palbuff(), BYVAL red, BYVAL green, BYVAL blue)
DECLARE SUB fadetopal (pal(), palbuff())
DECLARE SUB setpal (pal())
DECLARE SUB clearpage (BYVAL page)
DECLARE SUB setkeys ()
DECLARE SUB setfont (f())
DECLARE SUB printstr (s$, BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB textcolor (BYVAL f, BYVAL b)
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
'DECLARE SUB setitup (fil$, buff(), tbuff(), BYVAL p)
'DECLARE FUNCTION resetdsp
'DECLARE SUB playsnd (BYVAL n, BYVAL f)
'DECLARE SUB closefile
DECLARE SUB rectangle (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB fuzzyrect (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()
DECLARE SUB setmapdata (array(), pas(), BYVAL t, BYVAL b)
DECLARE SUB setmapblock (BYVAL x, BYVAL y, BYVAL v)
DECLARE FUNCTION readmapblock (BYVAL x, BYVAL y)
DECLARE SUB drawmap (BYVAL x, BYVAL y, BYVAL t, BYVAL p)
DECLARE SUB putpixel (BYVAL x, BYVAL y, BYVAL c, BYVAL p)
DECLARE FUNCTION readpixel (BYVAL x, BYVAL y, BYVAL p)
DECLARE FUNCTION Keyseg ()
DECLARE FUNCTION keyoff ()
DECLARE FUNCTION keyval (BYVAL a)
DECLARE FUNCTION getkey ()
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
'DECLARE SUB lumpfiles (listf$, lump$, path$, buffer())
DECLARE SUB unlump (lump$, ulpath$, buffer())
DECLARE SUB unlumpfile (lump$, fmask$, path$, buf())
DECLARE FUNCTION isfile (n$)
DECLARE SUB setupmusic (mbuf())
DECLARE SUB closemusic ()
DECLARE SUB stopsong ()
DECLARE SUB resumesong ()
DECLARE SUB resetfm ()
DECLARE SUB loadsong (f$)
DECLARE SUB fademusic (BYVAL vol)
DECLARE FUNCTION getfmvol ()
DECLARE SUB setfmvol (BYVAL vol)
DECLARE SUB screenshot (f$, BYVAL p, maspal(), buf())
DECLARE FUNCTION readjoy (joybuf(), BYVAL jnum)
'DECLARE FUNCTION setmouse (mbuf())
'DECLARE SUB readmouse (mbuf())
'DECLARE SUB movemouse (BYVAL x, BYVAL y)

COMMON SHARED /trueglobals/ game$, buffer(), master(), gen()

REM $STATIC
FUNCTION bound (n, lowest, highest)
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

SUB buystuff (id, shoptype, a(), vpage, dpage, timing(), stock(), gold&, item(), item$(), tag(), carray(), csetup(), gotm, gotj(), mouse(), joy(), pal(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf(), herobits(), itembits())
DIM b(1600), stuf$(50), vmask(5), emask(5), sname$(40), buytype$(5, 1), wbuf(100), walk(15)

getnames sname$()
buytype$(0, 0) = "Trade for "
buytype$(0, 1) = "Joins for "
buytype$(1, 0) = "Cannot Afford "
buytype$(1, 1) = "Cannot Hire "
FOR i = 0 TO 10 STEP 2
 walk(i) = 1
NEXT i
walk(11) = 2
walk(12) = 2
walk(13) = 3
walk(14) = 3

setpicstuf b(), 3200, -1
loadset game$ + ".stf" + CHR$(0), id, 0
FOR o = 0 TO a(16)
 stuf$(o) = ""
 FOR i = 1 TO small(b(o * 32 + 0), 16)
  IF b(o * 32 + i) >= 0 AND b(o * 32 + i) < 256 THEN stuf$(o) = stuf$(o) + CHR$(b(o * 32 + i))
 NEXT i
NEXT o

GOSUB setstock
GOSUB stufmask
IF total = 0 THEN GOTO exitbuy
ptr = 0: top = 0
DO UNTIL readbit(vmask(), 0, ptr) = 0
 ptr = ptr + 1
LOOP
GOSUB curinfo

setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
IF tog THEN walk = loopvar(walk, 0, 15, 1)
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
IF carray(0) > 1 THEN
 DO
  ptr = ptr - 1
  IF ptr < 0 THEN
   DO
    ptr = ptr + 1
   LOOP UNTIL readbit(vmask(), 0, ptr) = 0 OR ptr > a(16)
   EXIT DO
  END IF
 LOOP UNTIL readbit(vmask(), 0, ptr) = 0
 top = small(ptr, top)
 GOSUB curinfo
END IF
IF carray(1) > 1 THEN
 DO
  ptr = ptr + 1
  IF ptr > a(16) THEN
   DO
    ptr = ptr - 1
   LOOP UNTIL readbit(vmask(), 0, ptr) = 0 OR ptr < 0
   EXIT DO
  END IF
 LOOP UNTIL readbit(vmask(), 0, ptr) = 0
 GOSUB curinfo
END IF
IF carray(5) > 1 GOTO exitbuy
IF carray(4) > 1 THEN '---PRESS ENTER---------------------
 IF readbit(emask(), 0, ptr) = 0 THEN '---CHECK TO SEE IF YOU CAN AFFORD IT---
  IF stock(id, ptr) > 1 THEN stock(id, ptr) = stock(id, ptr) - 1
  IF b(ptr * 32 + 22) THEN setbit tag(), 0, ABS(b(ptr * 32 + 22)), SGN(SGN(b(ptr * 32 + 22)) + 1)
  gold& = gold& - b(ptr * 32 + 24)
  IF b(ptr * 32 + 25) > 0 THEN '---TRADE IN ITEM----------
   delitem b(ptr * 32 + 25), item(), item$()
  END IF '-------END TRADE IN ITEM----------------------------
  IF b(ptr * 32 + 17) = 0 THEN '---BUY ITEM-------------------
   getitem b(ptr * 32 + 18) + 1, item(), item$()
   acol = 4
   alert = 10
   alert$ = "Purchased " + stuf$(ptr)
  END IF '-------END IF ITEM-------------------------------------
  IF b(ptr * 32 + 17) = 1 THEN '---HIRE HERO------------------
   'getitem b(ptr * 32 + 18) + 1,  item(), item$()
   FOR i = 37 TO 0 STEP -1
    IF hero(i) = 0 THEN slot = i
   NEXT i
   addhero b(ptr * 32 + 18) + 1, slot, hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
   acol = 4
   alert = 10
   alert$ = stuf$(ptr) + " Joined!"
  END IF '-------END IF HERO-------------------------------------
  'the last thing to do is re-eval the item and hero tags in case
  'stuff changed
  evalherotag tag(), herobits(), hero(), stat(), leader
  evalitemtag tag(), itembits(), hero(), eqstuf(), item()
 ELSE ' WHEN CANNOT AFFORD------------------------------------
  acol = 3
  alert = 10
  alert$ = buytype$(1, shoptype) + stuf$(ptr)
 END IF '--------END BUY THING------------
 GOSUB stufmask
 DO WHILE readbit(vmask(), 0, ptr) = 1
  ptr = ptr - 1
  IF ptr < 0 THEN
   ptr = 0
   DO WHILE readbit(vmask(), 0, ptr) = 1
    ptr = ptr + 1
    IF ptr > a(16) THEN EXIT SUB
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
edgeprint stuf$(ptr), xstring(stuf$(ptr), 240), 30 + o * 10, 7, dpage: o = o + 1
IF info1$ <> "" THEN edgeprint info1$, xstring(info1$, 240), 30 + o * 10, 8, dpage: o = o + 1
IF info2$ <> "" THEN edgeprint info2$, xstring(info2$, 240), 30 + o * 10, 8, dpage: o = o + 1
IF eqinfo$ <> "" THEN edgeprint eqinfo$, xstring(eqinfo$, 240), 30 + o * 10, 7, dpage: o = o + 1
IF stock(id, ptr) > 1 THEN edgeprint STR$(stock(id, ptr) - 1) + " in stock ", xstring(STR$(stock(id, ptr) - 1) + " in stock ", 240), 30 + o * 10, 7, dpage: o = o + 1
IF showhero > -1 THEN
 centerbox 240, 130, 36, 44, 4, dpage
 loadsprite buffer(), 0, 640 * walk(walk), 0, 32, 40, 2
 drawsprite buffer(), 0, pal(), 16 * hpal, 224, 110, dpage
END IF
'-----LEFT PANEL-------------------------------------------
o = 0
FOR i = top TO a(16)
 IF readbit(vmask(), 0, i) = 0 THEN
  c = 7: IF ptr = i THEN c = 14 + tog
  IF readbit(emask(), 0, i) THEN c = 8: IF ptr = i THEN c = 7 + tog
  edgeprint stuf$(i), 10, 15 + o * 10, c, dpage
  o = o + 1
  IF o > 14 THEN
   IF ptr > i THEN
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
dowait
LOOP

curinfo:
 showhero = -1
 price$ = ""
 eqinfo$ = ""
 info1$ = ""
 info2$ = ""
 IF b(ptr * 32 + 24) > 0 THEN price$ = RIGHT$(STR$(b(ptr * 32 + 24)), LEN(STR$(b(ptr * 32 + 24))) - 1) + " " + sname$(32)
 IF b(ptr * 32 + 25) > 0 THEN
  IF price$ = "" THEN
   price$ = buytype$(0, shoptype)
  ELSE
   price$ = price$ + " and a "
  END IF
  setpicstuf buffer(), 200, -1
  loadset game$ + ".itm" + CHR$(0), b(ptr * 32 + 25) - 1, 0
  FOR o = 1 TO buffer(0)
   price$ = price$ + CHR$(small(large(buffer(o), 0), 255))
  NEXT o
 END IF
 IF b(ptr * 32 + 17) = 0 THEN
  setpicstuf buffer(), 200, -1
  loadset game$ + ".itm" + CHR$(0), b(ptr * 32 + 18), 0
  IF buffer(49) = 1 THEN eqinfo$ = "Equip: Weapon"
  IF buffer(49) > 1 THEN eqinfo$ = "Equip: " + sname$(23 + buffer(49))
  FOR i = 1 TO buffer(9)
   info1$ = info1$ + CHR$(small(large(buffer(i + 9), 0), 255))
  NEXT i
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
 IF b(ptr * 32 + 17) = 1 THEN
  setpicstuf buffer(), 636, -1
  loadset game$ + ".dt0" + CHR$(0), b(ptr * 32 + 18), 0
  setpicstuf wbuf(), 200, -1
  loadset game$ + ".itm" + CHR$(0), buffer(22), 0
  IF buffer(21) < 0 THEN buffer(21) = averagelev(stat(), hero())
  temp$ = STR$(atlevel(buffer(21), buffer(23 + 0 * 2), buffer(24 + 0 * 2)) + wbuf(54 + 0))
  eqinfo$ = RIGHT$(temp$, LEN(temp$) - 1) + " " + sname$(0)
  showhero = buffer(17)
  hpal = buffer(18)
  setpicstuf buffer(), 5120, 2
  loadset game$ + ".pt0" + CHR$(0), showhero, 0
  IF eslot = 0 THEN info1$ = "No Room in Party"
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
FOR i = 0 TO a(16)
 'edgeprint STR$(stock(id, i)), 0, i * 10, 15, vpage
 IF stock(id, i) = 1 THEN setbit vmask(), 0, i, 1
 IF b(i * 32 + 17) = (shoptype XOR 1) THEN setbit vmask(), 0, i, 1
 IF ABS(b(i * 32 + 20)) > 0 THEN IF istag(tag(), b(i * 32 + 20), 0) THEN setbit vmask(), 0, i, 1
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
'edgeprint "Total" + STR$(total), 160, 190, 15, vpage
'w = getkey
RETURN

setstock:
FOR i = 0 TO a(16)
 IF stock(id, i) = 0 THEN stock(id, i) = b(i * 32 + 19): IF stock(id, i) > -1 THEN stock(id, i) = stock(id, i) + 1
NEXT i
RETURN

exitbuy:
END SUB

SUB centerbox (x, y, w, h, c, p)
rectangle x - INT(w * .5), y - INT(h * .5), w, h, c * 16 + 2, p
 rectangle x - INT(w * .5), y - INT(h * .5), w, 1, c * 16 + 12, p
 rectangle x - INT(w * .5), y + INT(h * .5), w, 1, c * 16 + 12, p
 rectangle x - INT(w * .5), y - INT(h * .5), 1, h, c * 16 + 12, p
 rectangle x + INT(w * .5), y - INT(h * .5), 1, h, c * 16 + 12, p
END SUB

SUB centerfuz (x, y, w, h, c, p)
fuzzyrect x - INT(w * .5), y - INT(h * .5), w, h, c * 16 + 2, p
 rectangle x - INT(w * .5), y - INT(h * .5), w, 1, c * 16 + 12, p
 rectangle x - INT(w * .5), y + INT(h * .5), w, 1, c * 16 + 12, p
 rectangle x - INT(w * .5), y - INT(h * .5), 1, h, c * 16 + 12, p
 rectangle x + INT(w * .5), y - INT(h * .5), 1, h, c * 16 + 12, p
END SUB

SUB equip (ptr, hero(), stat(), name$(), timing(), vpage, dpage, item(), item$(), eqstuf(), bmenu(), carray(), csetup(), gotm, gotj(), mouse(), joy(), tag(), itembits())
DIM sname$(40), sno(11), eq(199), toff(4), tlim(4), m$(4), menu$(6), stb(11)
getnames sname$()
m$(0) = "Weapon"
FOR i = 0 TO 3
 m$(i + 1) = sname$(25 + i)
NEXT i
menu$(5) = "-REMOVE-"
menu$(6) = " -EXIT- "
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
GOSUB setupeq

copypage vpage, 3
centerfuz 160, 100, 304, 184, 1, 3
centerbox 84, 16, 140, 20, 4, 3
centerbox 84, 100, 140, 130, 4, 3
centerbox 236, 75, 80, 78, 4, 3

setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
IF mset = 0 THEN
 IF carray(5) > 1 THEN EXIT SUB
 IF carray(2) > 1 THEN DO: ptr = loopvar(ptr, 0, 3, -1): LOOP UNTIL hero(ptr) > 0: GOSUB setupeq
 IF carray(3) > 1 THEN DO: ptr = loopvar(ptr, 0, 3, 1): LOOP UNTIL hero(ptr) > 0: GOSUB setupeq
 IF carray(0) > 1 THEN csr = csr - 1: IF csr < 0 THEN csr = 6
 IF carray(1) > 1 THEN csr = csr + 1: IF csr > 6 THEN csr = 0
 IF carray(4) > 1 THEN
  IF csr < 5 THEN
   IF tlim(csr) >= 0 THEN mset = 1: top = toff(csr): csr2 = top: GOSUB stbonus
   'UPDATE ITEM POSESION BITSETS
   evalitemtag tag(), itembits(), hero(), eqstuf(), item()
  END IF
  IF csr = 5 THEN
   csr = 0
   IF eqstuf(ptr, csr) <> dw THEN
    GOSUB unequip
    ie = dw: GOSUB newequip
   END IF
   FOR csr = 1 TO 4
    IF eqstuf(ptr, csr) > 0 THEN
     GOSUB unequip
    END IF
   NEXT csr
   GOSUB setupeq
   csr = 5
   'UPDATE ITEM POSESSION BITSETS
   evalitemtag tag(), itembits(), hero(), eqstuf(), item()
  END IF
  IF csr = 6 THEN EXIT SUB
 END IF
ELSE
 IF carray(5) > 1 THEN mset = 0: FOR i = 0 TO 11: stb(i) = 0: NEXT i
 IF carray(0) > 1 THEN csr2 = large(csr2 - 1, toff(csr)): GOSUB stbonus: IF csr2 < top THEN top = top - 1
 IF carray(1) > 1 THEN csr2 = small(csr2 + 1, toff(csr) + tlim(csr)): GOSUB stbonus: IF csr2 > top + 17 THEN top = top + 1
 IF carray(4) > 1 THEN ie = (item(eq(csr2)) AND 255): GOSUB unequip: GOSUB newequip
END IF
edgeprint name$(ptr), 84 - LEN(name$(ptr)) * 4, 12, 15, dpage
FOR i = 0 TO 11
 edgeprint sname$(sno(i)), 20, 42 + i * 10, 7, dpage
 col = 7
 IF stb(i) < 0 THEN col = 8
 IF stb(i) > 0 THEN col = 14 + tog
 edgeprint STR$(stat(ptr, 1, i) + stb(i)), 148 - LEN(STR$(stat(ptr, 1, i) + stb(i))) * 8, 42 + i * 10, col, dpage
NEXT i
IF mset = 0 THEN
 FOR i = 0 TO 6
  textcolor 7, 1
  IF i < 5 THEN IF tlim(i) < 0 THEN textcolor 7, 18
  IF csr = i THEN
   textcolor 14 + tog, 1 + tog
   IF i < 5 THEN IF tlim(i) < 0 THEN textcolor 14 + tog, 2
  END IF
  printstr menu$(i), 204, 45 + i * 9, dpage
 NEXT i
 IF csr < 5 THEN
  centerbox 236, 20, (LEN(m$(csr)) + 2) * 8, 20, 4, dpage: edgeprint m$(csr), 236 - (LEN(m$(csr)) * 4), 16, 15, dpage
 END IF
END IF
IF mset = 1 THEN
 centerbox 236, 100, 96, 152, 4, dpage
 FOR i = top TO top + 17
  IF i > toff(csr) + tlim(csr) THEN EXIT FOR
  textcolor 7, 0
  IF i = csr2 THEN textcolor 14 + tog, 2
  printstr item$(eq(i)), 192, 28 + (i - top) * 8, dpage
 NEXT i
END IF
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
dowait
LOOP

stbonus:
setpicstuf buffer(), 200, -1
lb = (item(eq(csr2)) AND 255)
loadset game$ + ".itm" + CHR$(0), lb - 1, 0
FOR i = 0 TO 11
 stb(i) = buffer(54 + i)
NEXT i
IF eqstuf(ptr, csr) > 0 THEN
 loadset game$ + ".itm" + CHR$(0), eqstuf(ptr, csr) - 1, 0
 FOR i = 0 TO 11
  stb(i) = stb(i) - buffer(54 + i)
 NEXT i
END IF
RETURN

unequip:
IF eqstuf(ptr, csr) = 0 THEN RETURN
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), eqstuf(ptr, csr) - 1, 0
FOR i = 0 TO 11
 stat(ptr, 1, i) = stat(ptr, 1, i) - buffer(54 + i)
 IF i > 1 THEN stat(ptr, 0, i) = stat(ptr, 1, i)
 stat(ptr, 0, i) = small(stat(ptr, 0, i), stat(ptr, 1, i))
NEXT i
IF csr = 0 AND eqstuf(ptr, csr) = dw THEN eqstuf(ptr, csr) = 0: RETURN
getitem eqstuf(ptr, csr), item(), item$()
eqstuf(ptr, csr) = 0
RETURN

newequip:
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), ie - 1, 0
FOR i = 0 TO 11
 stat(ptr, 1, i) = stat(ptr, 1, i) + buffer(54 + i)
 IF i > 1 THEN stat(ptr, 0, i) = stat(ptr, 1, i)
 stat(ptr, 0, i) = small(stat(ptr, 0, i), stat(ptr, 1, i))
NEXT i
IF csr = 0 THEN
 stat(ptr, 0, 13) = buffer(52)
 stat(ptr, 1, 13) = buffer(53)
 bmenu(ptr, 0) = large(buffer(48), 1)
END IF
eqstuf(ptr, csr) = ie
mset = 0
FOR i = 0 TO 11
 stb(i) = 0
NEXT i
IF ie = dw AND csr = 0 THEN GOSUB setupeq: RETURN
lb = (item(eq(csr2)) AND 255)
hb = INT(item(eq(csr2)) / 256)
hb = hb - 1
item(eq(csr2)) = lb + (hb * 256)
item$(eq(csr2)) = LEFT$(item$(eq(csr2)), 9) + RIGHT$(STR$(hb), 2)
IF hb < 1 THEN item(eq(csr2)) = 0: item$(eq(csr2)) = "           "
GOSUB setupeq
RETURN

setupeq:
setpicstuf buffer(), 636, -1
loadset game$ + ".dt0" + CHR$(0), hero(ptr) - 1, 0
dw = buffer(22) + 1

setpicstuf buffer(), 200, -1
FOR i = 0 TO 4
 menu$(i) = "        "
 IF eqstuf(ptr, i) > 0 THEN
  loadset game$ + ".itm" + CHR$(0), eqstuf(ptr, i) - 1, 0
  menu$(i) = ""
  FOR o = 1 TO buffer(0)
   menu$(i) = menu$(i) + CHR$(buffer(o))
  NEXT o
  WHILE LEN(menu$(i)) < 8: menu$(i) = menu$(i) + " ": WEND
 END IF
NEXT i
o = 0
setpicstuf buffer(), 200, -1
FOR i = 0 TO 197
 lb = (item(i) AND 255)
 IF lb > 0 THEN
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
  IF buffer(49) > 0 THEN
   IF readbit(buffer(), 66, hero(ptr) - 1) THEN
    eq(o) = i + (buffer(49) * 256)
    o = o + 1
   END IF
  END IF
 END IF
NEXT i
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
FOR i = 0 TO toff(4) + tlim(4)
 eq(i) = (eq(i) AND 255)
NEXT i
RETURN

END SUB

SUB getitem (getit, item(), item$())

i = 0
DO
 lb = (item(i) AND 255)
 hb = INT(item(i) / 256)
 IF getit = lb AND hb < 99 THEN item(i) = lb + ((hb + 1) * 256): itstr i, item(), item$(): EXIT SUB
 i = i + 1
LOOP UNTIL i > 197
i = 0
DO
 lb = (item(i) AND 255)
 IF lb = 0 THEN item(i) = getit + 256: itstr i, item(), item$(): EXIT SUB
 i = i + 1
LOOP UNTIL i > 199
getit = 0
EXIT SUB

RETURN

END SUB

FUNCTION items (item(), item$(), hero(), stat(), name$(), timing(), vpage, dpage, bmenu(), spell(), pal(), carray(), csetup(), gotm, gotj(), mouse(), joy())
DIM a(100), iuse(15), ondead(15), permask(15), owpal(4)

FOR i = 0 TO 2
 setbit iuse(), 0, i, 1
NEXT i
FOR i = 0 TO 199
 setbit ondead(), 0, 3 + i, 0
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
   IF buffer(3) = 4 THEN setbit ondead(), 0, 3 + i, 1
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
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
GOSUB itcontrol
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
   wt = 0: IF wptr = i THEN wt = INT(wtog / 2)
   loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
   drawsprite buffer(), 0, pal(), owpal(i) * 16, 89, 8 + i * 20, dpage
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

infostr:
info$ = ""
IF sel >= 0 AND ic = -1 THEN
 IF item(sel) > 0 THEN
  info$ = "Discard " + item$(sel)
  IF readbit(permask(), 0, 3 + sel) THEN info$ = "Cannot " + info$ + "!"
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
  IF sel > -1 THEN sel = -4 ELSE GOTO doneitem
 END IF
 IF carray(4) > 1 THEN
   '--exit
   IF ic = -3 THEN GOTO doneitem
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
     IF a(50) > 0 THEN
      tclass = 1
      ttype = 0
      pick = 1: wptr = 0
      WHILE hero(wptr) = 0 OR (stat(wptr, 0, 0) = 0 AND readbit(ondead(), 0, 3 + ic) = 0)
       wptr = loopvar(wptr, 0, 3, 1)
      WEND
      spred = 0
      FOR i = 0 TO 3
       IF hero(i) > 0 THEN
        setpicstuf buffer(), 636, -1
        loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
        owpal(i) = buffer(20)
       END IF
      NEXT i
      RETURN
     END IF
     IF a(51) > 0 THEN
      setpicstuf buffer(), 80, -1
      loadset game$ + ".dt6" + CHR$(0), a(51) - 1, 0
      tclass = buffer(3)
      ttype = buffer(4)
      IF tclass = 0 THEN RETURN
      IF ttype = 1 THEN
       FOR i = 0 TO 3
        IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR readbit(ondead(), 0, 3 + ic)) THEN spred = spred + 1
       NEXT i
      END IF
      pick = 1: wptr = 0
      WHILE hero(wptr) = 0 OR (stat(wptr, 0, 0) = 0 AND readbit(ondead(), 0, 3 + ic) = 0)
       wptr = loopvar(wptr, 0, 3, 1)
      WEND
      spred = 0
      FOR i = 0 TO 3
       IF hero(i) > 0 THEN
        setpicstuf buffer(), 636, -1
        loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
        owpal(i) = buffer(20)
       END IF
      NEXT i
      RETURN
     END IF
     IF a(51) < 0 THEN
      IF buffer(73) = 1 THEN dummy = consumeitem(ic, item(), item$())
      items = a(51) * -1: EXIT FUNCTION
     END IF
    END IF
   END IF
   IF sel < -3 AND ic >= 0 THEN sel = ic: RETURN
 END IF
 IF carray(0) > 1 AND ic >= 0 THEN ic = ic - 3: GOSUB infostr: IF ic < top THEN top = top - 3
 IF carray(1) > 1 AND ic < 195 THEN ic = ic + 3: GOSUB infostr: IF ic > top + 62 THEN top = top + 3
 IF carray(2) > 1 THEN
  IF ((ic / 3) - INT(ic / 3)) * 3 > 0 THEN ic = ic - 1: GOSUB infostr ELSE ic = ic + 2: GOSUB infostr
 END IF
 IF carray(3) > 1 THEN
  IF ((ic / 3) - INT(ic / 3)) * 3 < 2 THEN ic = ic + 1: GOSUB infostr ELSE ic = ic - 2: GOSUB infostr
 END IF
ELSE
 info$ = item$(ic)
 IF carray(5) > 1 THEN pick = 0: GOSUB infostr: RETURN
 IF spred = 0 THEN
  IF carray(0) > 1 THEN DO: wptr = loopvar(wptr, 0, 3, -1): LOOP UNTIL hero(wptr) > 0 AND (stat(wptr, 0, 0) > 0 OR readbit(ondead(), 0, 3 + ic))
  IF carray(1) > 1 THEN DO: wptr = loopvar(wptr, 0, 3, 1): LOOP UNTIL hero(wptr) > 0 AND (stat(wptr, 0, 0) > 0 OR readbit(ondead(), 0, 3 + ic))
 END IF
 IF ttype = 2 THEN
  IF carray(2) > 1 OR carray(3) > 1 THEN
   IF spred = 0 THEN
    FOR i = 0 TO 3
     IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR readbit(ondead(), 0, 3 + ic)) THEN spred = spred + 1
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
  IF buffer(50) > 0 THEN
   atk = buffer(50)
   setpicstuf buffer(), 636, -1
   loadset game$ + ".dt0" + CHR$(0), hero(wptr) - 1, 0
   temp = 0
   FOR j = 0 TO 3
    FOR o = 0 TO 23
     IF buffer(47 + (j * 48) + (o * 2)) = atk AND buffer(48 + (j * 48) + (o * 2)) = 0 THEN spell(wptr, j, o) = buffer(47 + (j * 48) + (o * 2)): temp = 1
    NEXT o
   NEXT j
   IF temp = 0 THEN RETURN
  END IF
  setpicstuf buffer(), 200, -1
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
  IF buffer(51) > 0 THEN
   atk = buffer(51) - 1
   IF spred = 0 THEN
    oobcure -1, wptr, atk, spred, stat(), hero()
   ELSE
    FOR i = 0 TO 3
     IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR readbit(ondead(), 0, 3 + ic)) THEN oobcure -1, i, atk, spred, stat(), hero()
    NEXT i
   END IF
  END IF 'buffer(51) > 0
  IF buffer(73) = 1 THEN
   IF consumeitem(ic, item(), item$()) THEN
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

doneitem:
END FUNCTION

SUB itstr (i, item(), item$())

item$(i) = "           "
IF item(i) = 0 THEN EXIT SUB
item$(i) = ""
lb = (item(i) AND 255)
hb = INT(item(i) / 256)
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), lb - 1, 0
FOR o = 1 TO buffer(0)
 item$(i) = item$(i) + CHR$(buffer(o))
NEXT o
WHILE LEN(item$(i)) < 8: item$(i) = item$(i) + " ": WEND
item$(i) = item$(i) + CHR$(1) + RIGHT$(STR$(hb), 2)

END SUB

SUB oobcure (w, t, atk, spred, stat(), hero())
DIM st(13)
IF w = -1 THEN
 j = 0
 FOR o = 0 TO 3
  IF hero(o) > 0 THEN
   j = j + 1
   FOR i = 0 TO 13
    st(i) = st(i) + stat(o, 0, i)
   NEXT i
  END IF
 NEXT o
 FOR i = 0 TO 13
  st(i) = st(i) / j
 NEXT i
ELSE
 FOR i = 0 TO 13
  st(i) = stat(w, 0, i)
 NEXT i
END IF

setpicstuf buffer(), 80, -1
loadset game$ + ".dt6" + CHR$(0), atk, 0
emp = 0

a = st(2): d = stat(t, 0, 4)
IF buffer(7) = 1 THEN a = st(6): d = stat(t, 0, 7)
IF buffer(7) = 4 THEN a = INT(RND * 999)
IF buffer(7) = 5 THEN a = 100
am! = 1: dm! = .5
IF buffer(5) = 1 THEN am! = .8: dm = .1
IF buffer(5) = 2 THEN am! = 1.3: dm = 1
IF buffer(5) = 3 THEN am! = 1: dm = 0
h = range(a * am!, 20) - range(d * dm!, 20)
h = h + (h / 100) * buffer(11)
IF readbit(buffer(), 20, 1) = 1 THEN h = h / (spred + 1)
h = large(h, 1)
IF readbit(buffer(), 20, 0) = 1 THEN h = h * -1
emp = readbit(buffer(), 20, 60)
stat(t, 0, 0 + emp) = stat(t, 0, 0 + emp) - h
stat(t, 0, 0 + emp) = large(stat(t, 0, 0 + emp), 0): stat(t, 0, 0 + emp) = small(stat(t, 0, 0 + emp), stat(t, 1, 0 + emp))

END SUB

SUB patcharray (array(), n$, max, timing(), vpage, dpage)

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
setwait timing(), 80
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN EXIT SUB
IF keyval(72) > 1 THEN csr = large(0, csr - 1)
IF keyval(80) > 1 THEN csr = small(2, csr + 1)
IF csr = 0 THEN intgrabber ptr, 0, max, 75, 77
IF csr = 1 THEN intgrabber array(ptr), -32768, 32767, 75, 77
IF csr = 2 THEN
 FOR i = 0 TO 15
  IF keyval(hexk(i)) > 1 THEN setbit array(), ptr, i, readbit(array(), ptr, i) XOR 1
 NEXT i
END IF
num$(0) = n$ + "(" + RIGHT$(STR$(ptr), LEN(STR$(ptr)) - 1) + ")"
num$(1) = "value =" + STR$(array(ptr))
num$(2) = ""
FOR i = 0 TO 15
 IF readbit(array(), ptr, i) THEN
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

FUNCTION pickload (svcsr, pal(), timing(), dpage, vpage, carray(), csetup(), gotm, gotj(), mouse(), joy(), sourcerpg$)

DIM full(3), name$(3), svtime$(3), mapname$(3), lev$(3), id(3, 3), leader, tstat(3, 1, 13), pic(3, 3), po(3, 3)

centerbox 50, 10, 80, 12, 15, 3
FOR i = 0 TO 3
 centerbox 160, 40 + i * 44, 310, 42, 15, 3
 setpicstuf buffer(), 30000, -1
 sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
 loadset sg$ + CHR$(0), i * 2, 0
 IF buffer(0) = 3 THEN 'save version number
  full(i) = 1
  map = buffer(1)
  leader = buffer(6)
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
  '--if the save format changes, so must this
  z = 34 + 51
  svtime$(i) = playtime$(buffer(z), buffer(z + 1), buffer(z + 2))
  lev$(i) = "Level" + STR$(tstat(leader, 0, 12))
  FOR o = 0 TO 3
   id(i, o) = buffer(2763 + o)
  NEXT o
  FOR o = 0 TO 3
   IF id(i, o) >= 0 THEN
    setpicstuf buffer(), 636, -1
    loadset game$ + ".dt0" + CHR$(0), id(i, o) - 1, 0
    IF leader = o THEN
     FOR j = 1 TO buffer(0)
     name$(i) = name$(i) + CHR$(buffer(j))
     NEXT j
    END IF
    pic(i, o) = buffer(17)
    po(i, o) = buffer(18)
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

nofull = 0
FOR i = 0 TO 3
 IF full(i) = 1 THEN nofull = 1
NEXT i
IF nofull = 0 THEN pickload = -1: clearpage 2: EXIT FUNCTION

setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
walk = walk XOR tog
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
IF carray(5) > 1 THEN pickload = -2: clearpage 2: EXIT FUNCTION
IF carray(0) > 1 THEN svcsr = loopvar(svcsr, -1, 3, -1)
IF carray(1) > 1 THEN svcsr = loopvar(svcsr, -1, 3, 1)
IF carray(4) > 1 THEN
 IF svcsr = -1 THEN pickload = -1: clearpage 2: EXIT FUNCTION
 IF svcsr >= 0 AND full(svcsr) = 1 THEN
  pickload = svcsr: clearpage 2: EXIT FUNCTION
 END IF
END IF
GOSUB drawld
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
dowait
LOOP

drawld:
IF svcsr >= 0 THEN
 centerbox 160, 40 + svcsr * 44, 312, 44, 2, dpage
ELSE
 centerbox 50, 10, 82, 14, 2, dpage
END IF
FOR i = 0 TO 3
 IF full(i) = 1 THEN
  FOR o = 0 TO 3
   IF id(i, o) > 0 THEN
    temp = 16 + (i * 16) + (o * 4)
    IF svcsr = i THEN temp = temp + (2 * walk)
    loadsprite buffer(), 0, 0, temp, 32, 40, 2
    drawsprite buffer(), 0, pal(), po(i, o) * 16, 140 + (o * 42), 20 + i * 44, dpage
   END IF
  NEXT o
  col = 7
  IF svcsr = i THEN col = 14 + tog
  edgeprint name$(i), 14, 21 + i * 44, col, dpage
  edgeprint lev$(i), 14, 30 + i * 44, col, dpage
  edgeprint svtime$(i), 14, 39 + i * 44, col, dpage
  edgeprint mapname$(i), 14, 48 + i * 44, col, dpage
 END IF
NEXT i
col = 7
IF svcsr = -1 THEN col = 14 + tog
edgeprint "NEW GAME", 18, 5, col, dpage
RETURN

END FUNCTION

FUNCTION picksave (svcsr, pal(), timing(), dpage, vpage, carray(), csetup(), gotm, gotj(), mouse(), joy(), sourcerpg$)

DIM full(3), name$(3), mapname$(3), svtime$(3), lev$(3), id(3, 3), leader, tstat(3, 1, 13), pic(3, 3), po(3, 3), menu$(1)
menu$(0) = "Yes"
menu$(1) = "No"

copypage vpage, 3
centerbox 50, 10, 60, 12, 15, 3
FOR i = 0 TO 3
 centerbox 160, 40 + i * 44, 310, 42, 15, 3
 setpicstuf buffer(), 30000, -1
 sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
 loadset sg$ + CHR$(0), i * 2, 0
 IF buffer(0) = 3 THEN 'save version id
  full(i) = 1
  map = buffer(1)
  leader = buffer(6)
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
  '--if the save format changes, so must this
  z = 34 + 51
  svtime$(i) = playtime$(buffer(z), buffer(z + 1), buffer(z + 2))
  lev$(i) = "Level" + STR$(tstat(leader, 0, 12))
  FOR o = 0 TO 3
   id(i, o) = buffer(2763 + o)
  NEXT o
  FOR o = 0 TO 3
   IF id(i, o) >= 0 THEN
    setpicstuf buffer(), 636, -1
    loadset game$ + ".dt0" + CHR$(0), id(i, o) - 1, 0
    IF leader = o THEN
     FOR j = 1 TO buffer(0)
     name$(i) = name$(i) + CHR$(buffer(j))
     NEXT j
    END IF
    pic(i, o) = buffer(17)
    po(i, o) = buffer(18)
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

setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
walk = walk XOR tog
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
IF carray(5) > 1 THEN picksave = -1: clearpage 2: EXIT FUNCTION
IF carray(0) > 1 THEN svcsr = loopvar(svcsr, -1, 3, -1)
IF carray(1) > 1 THEN svcsr = loopvar(svcsr, -1, 3, 1)
IF carray(4) > 1 THEN
 IF svcsr = -1 THEN picksave = -1: clearpage 2: EXIT FUNCTION
 IF svcsr >= 0 THEN
  deny = 0
  IF full(svcsr) = 1 THEN GOSUB confirm
  IF deny = 0 THEN picksave = svcsr: clearpage 2: EXIT FUNCTION
 END IF
END IF
GOSUB drawsv
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
dowait
LOOP

savedraw:

RETURN

confirm:
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
IF carray(5) > 1 THEN deny = 1: RETURN
IF carray(0) > 1 OR carray(1) > 1 THEN deny = deny XOR 1
IF carray(4) > 1 THEN RETURN
GOSUB drawsv
centerbox 160, 14 + (44 * svcsr), 200, 22, 3, dpage
edgeprint "Replace Old Data?", 70, 9 + (44 * svcsr), 15, dpage
FOR i = 0 TO 1
 col = 7: IF deny = i THEN col = 14 + tog
 edgeprint menu$(i), 216, 5 + (i * 9) + (44 * svcsr), col, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
dowait
LOOP

drawsv:
IF svcsr >= 0 THEN
 centerbox 160, 40 + svcsr * 44, 312, 44, 1, dpage
ELSE
 centerbox 50, 10, 62, 14, 1, dpage
END IF
FOR i = 0 TO 3
 IF full(i) = 1 THEN
  FOR o = 0 TO 3
   IF id(i, o) > 0 THEN
    temp = 16 + (i * 16) + (o * 4)
    IF svcsr = i THEN temp = temp + (2 * walk)
    loadsprite buffer(), 0, 0, temp, 32, 40, 2
    drawsprite buffer(), 0, pal(), po(i, o) * 16, 140 + (o * 42), 20 + i * 44, dpage
   END IF
  NEXT o
  col = 7
  IF svcsr = i THEN col = 14 + tog
  edgeprint name$(i), 14, 21 + i * 44, col, dpage
  edgeprint lev$(i), 14, 30 + i * 44, col, dpage
  edgeprint svtime$(i), 14, 39 + i * 44, col, dpage
  edgeprint mapname$(i), 14, 48 + i * 44, col, dpage
 END IF
NEXT i
col = 7
IF svcsr = -1 THEN col = 14 + tog
edgeprint "CANCEL", 27, 5, col, dpage
RETURN

END FUNCTION

SUB sellstuff (id, a(), vpage, dpage, timing(), stock(), gold&, item(), item$(), tag(), carray(), csetup(), gotm, gotj(), mouse(), joy(), pal(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf(), itembits())
DIM b(1600), sname$(40), permask(15), price(200)

getnames sname$()

setpicstuf b(), 3200, -1
loadset game$ + ".stf" + CHR$(0), id, 0
GOSUB selstock

ic = 0: top = 0

GOSUB refreshs

GOSUB sellinfostr
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
GOSUB keysell
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

sellinfostr:
info$ = ""
IF item(ic) = 0 THEN RETURN
IF readbit(permask(), 0, ic) = 1 THEN info$ = "CANNOT SELL": RETURN
lb = (item(ic) AND 255)
hb = INT(item(ic) / 256)
setpicstuf buffer(), 200, -1
IF lb > 0 THEN
 IF price(ic) > 0 THEN info$ = "Worth" + STR$(price(ic)) + " " + sname$(32)
 FOR i = 0 TO a(16)
  IF b(i * 32 + 17) = 0 AND b(i * 32 + 18) = lb - 1 THEN
   IF b(i * 32 + 28) > 0 THEN
    IF info$ = "" THEN info$ = "Trade for " ELSE info$ = info$ + " and a "
    loadset game$ + ".itm" + CHR$(0), b(i * 32 + 28) - 1, 0
    FOR o = 1 TO buffer(0)
     info$ = info$ + CHR$(small(large(buffer(o), 0), 255))
    NEXT o
   END IF
  END IF
 NEXT i
 IF info$ = "" THEN info$ = "Worth Nothing"
END IF
RETURN

keysell:
 IF carray(5) > 1 THEN GOTO donesell
 IF carray(4) > 1 AND readbit(permask(), 0, ic) = 0 AND item(ic) > 0 THEN
  alert = 10: alert$ = "Sold " + LEFT$(item$(ic), 8): WHILE RIGHT$(item$(ic), 1) = " ": item$(ic) = LEFT$(item$(ic), LEN(item$(ic)) - 1): WEND
  'INCREMENT GOLD-----------
  gold& = gold& + price(ic)
  IF gold& > 2000000000 THEN gold& = 2000000000
  IF gold& < 0 THEN gold& = 0
  'CHECK FOR SPECIAL CASES---------
  FOR i = 0 TO a(16)
   IF b(i * 32 + 17) = 0 AND b(i * 32 + 18) = lb - 1 THEN
    'SET SELL BIT---
    IF b(i * 32 + 23) <> 0 THEN setbit tag(), 0, ABS(b(i * 32 + 23)), SGN(SGN(b(i * 32 + 23)) + 1)
    'ADD TRADED ITEM-----------
    IF b(i * 32 + 28) > 0 THEN getitem b(i * 32 + 28), item(), item$()
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
  itstr ic, item(), item$()
  'UPDATE ITEM POSESSION TAGS--------
  evalitemtag tag(), itembits(), hero(), eqstuf(), item()
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
  FOR o = 0 TO a(16)
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
FOR i = 0 TO a(16)
 IF stock(id, i) = 0 THEN stock(id, i) = b(i * 32 + 19): IF stock(id, i) > -1 THEN stock(id, i) = stock(id, i) + 1
NEXT i
RETURN

donesell:

END SUB

SUB spells (ptr, hero(), stat(), name$(), timing(), vpage, dpage, bmenu(), lmp(), spell(), pal(), carray(), csetup(), gotm, gotj(), mouse(), joy())
DIM sname$(40), menu$(4), mi(4), mtype(5), spel$(24), cost$(24), spel(24), canuse(24), targt(24), alpha$(10), spid(5), owpal(3)
getnames sname$()
alpha$(0) = "Zero"
alpha$(1) = "One"
alpha$(2) = "Two"
alpha$(3) = "Three"
alpha$(4) = "Four"
alpha$(5) = "Five"
alpha$(6) = "Six"
alpha$(7) = "Seven"
alpha$(8) = "Eight"
alpha$(9) = "Nine"
alpha$(10) = "Ten"

GOSUB splname
copypage vpage, 3
centerfuz 160, 100, 304, 184, 1, 3
centerbox 206, 36, 200, 20, 2, 3
centerbox 60, 50, 82, 60, 2, 3
centerbox 160, 135, 280, 80, 2, 3
rectangle 21, 164, 280, 1, 40, 3
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
wtog = loopvar(wtog, 0, 3, 1)
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
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
  printstr " (CANCEL) ", 24, 166, dpage
  IF mset = 1 THEN
   textcolor 10, 0
   printstr cost$(sptr), 288 - LEN(cost$(sptr)) * 8, 166, dpage
  END IF
 END IF
 textcolor 7, 0
 IF csr = i THEN textcolor 14 + tog, 2: IF mset = 1 THEN textcolor 7, 2
 printstr menu$(i), 21, 25 + i * 10, dpage
NEXT i
IF last = 0 THEN edgeprint name$(ptr) + " has no spells", xstring(name$(ptr) + " has no spells", 160), 120, 15, dpage
edgeprint name$(ptr), xstring(name$(ptr), 206), 31, 15, dpage
IF pick = 1 THEN
 centerbox 196, 47, 160, 88, 2, dpage
 IF spred = 0 THEN rectangle 120, 8 + wptr * 20, 152, 20, 2, dpage ELSE rectangle 120, 8, 152, 80, 2 * tog, dpage
 o = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   wt = 0: IF wptr = i THEN wt = INT(wtog / 2)
   loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
   drawsprite buffer(), 0, pal(), owpal(i) * 16, 125, 8 + i * 20, dpage
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
  IF spell(ptr, spid(csr), i) > 0 THEN
   spel(i) = spell(ptr, spid(csr), i) - 1
   setpicstuf buffer(), 80, -1
   loadset game$ + ".dt6" + CHR$(0), spel(i), 0
   IF readbit(buffer(), 20, 59) = 1 AND buffer(3) > 0 THEN canuse(i) = buffer(3): targt(i) = buffer(4)
   IF mtype(csr) = 0 AND stat(ptr, 0, 1) < buffer(8) THEN canuse(i) = 0
   IF mtype(csr) = 1 AND lmp(ptr, INT(i / 3)) = 0 THEN canuse(i) = 0
   IF stat(ptr, 0, 0) = 0 THEN canuse(i) = 0
   FOR j = 26 TO 25 + buffer(24)
    spel$(i) = spel$(i) + CHR$(buffer(j))
   NEXT j
   IF mtype(csr) = 0 THEN cost$(i) = STR$(buffer(8)) + "MP  " + RIGHT$(STR$(stat(ptr, 0, 1)), LEN(STR$(stat(ptr, 0, 1))) - 1) + "/" + RIGHT$(STR$(stat(ptr, 1, 1)), LEN(STR$(stat(ptr, 1, 1))) - 1)
   IF mtype(csr) = 1 THEN cost$(i) = "Level " + alpha$(INT(i / 3) + 1) + "  " + STR$(lmp(ptr, INT(i / 3)))
  END IF
  WHILE LEN(spel$(i)) < 10: spel$(i) = spel$(i) + " ": WEND
 NEXT i
RETURN

splname:
FOR i = 0 TO 5
 mtype(i) = -1
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), hero(ptr) - 1, 0
 IF bmenu(ptr, i) < 0 AND bmenu(ptr, i) > -10 THEN
  spid(i) = (bmenu(ptr, i) + 1) * -1
  mtype(i) = buffer(288 + spid(i))
 END IF
NEXT i
last = 0
FOR o = 0 TO 5
 IF mtype(o) >= 0 AND mtype(o) < 2 THEN
  menu$(last) = ""
  mtype(last) = mtype(o)
  spid(last) = spid(o)
  setpicstuf buffer(), 636, -1
  loadset game$ + ".dt0" + CHR$(0), hero(ptr) - 1, 0
  temp = (bmenu(ptr, o) + 1) * -1
  mi(last) = temp
  FOR j = 244 + temp * 11 TO 243 + temp * 11 + buffer(243 + temp * 11)
   menu$(last) = menu$(last) + CHR$(buffer(j))
  NEXT j
  IF menu$(last) <> "" THEN
   WHILE LEN(menu$(last)) < 10: menu$(last) = menu$(last) + " ": WEND
   last = last + 1
  END IF
 END IF
NEXT o
menu$(last) = "Exit      "
mi(last) = -1
mtype(last) = -1
IF csr > last THEN csr = last
GOSUB curspellist
RETURN

scontrol:
IF pick = 0 THEN
 IF mset = 0 THEN
  IF carray(5) > 1 THEN setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: EXIT SUB
  IF carray(2) > 1 THEN DO: ptr = loopvar(ptr, 0, 3, -1): LOOP UNTIL hero(ptr) > 0: GOSUB splname
  IF carray(3) > 1 THEN DO: ptr = loopvar(ptr, 0, 3, 1): LOOP UNTIL hero(ptr) > 0: GOSUB splname
  IF carray(0) > 1 THEN csr = large(csr - 1, 0): GOSUB curspellist
  IF carray(1) > 1 THEN csr = small(csr + 1, last): GOSUB curspellist
  IF carray(4) > 1 THEN
   IF mi(csr) = -1 THEN setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: EXIT SUB
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
    pick = 1: wptr = ptr
    FOR i = 0 TO 3
     IF hero(i) > 0 THEN
      setpicstuf buffer(), 636, -1
      loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
      owpal(i) = buffer(20)
     END IF
    NEXT i
    spred = 0
    IF targt(sptr) = 1 AND canuse(sptr) <> 2 THEN
     FOR i = 0 TO 3
      IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR targt(sptr) = 4) THEN spred = spred + 1
     NEXT i
    END IF
   END IF
  END IF
 END IF
ELSE
 IF carray(5) > 1 THEN pick = 0
 IF canuse(sptr) <> 2 AND spred = 0 THEN
  IF carray(0) > 1 THEN DO: wptr = loopvar(wptr, 0, 3, -1): LOOP UNTIL hero(wptr) > 0 AND (stat(wptr, 0, 0) > 0 OR targt(sptr) = 4)
  IF carray(1) > 1 THEN DO: wptr = loopvar(wptr, 0, 3, 1): LOOP UNTIL hero(wptr) > 0 AND (stat(wptr, 0, 0) > 0 OR targt(sptr) = 4)
 END IF
 IF targt(sptr) = 2 AND canuse(sptr) <> 2 THEN
  IF carray(2) > 1 OR carray(3) > 1 THEN
   IF spred = 0 THEN
    FOR i = 0 TO 3
     IF hero(i) > 0 AND (stat(wptr, 0, 0) > 0 OR targt(sptr) = 4) THEN spred = spred + 1
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
   IF buffer(8) > stat(ptr, 0, 1) THEN pick = 0: RETURN
   stat(ptr, 0, 1) = small(large(stat(ptr, 0, 1) - buffer(8), 0), stat(ptr, 1, 1))
  END IF
  IF mtype(csr) = 1 THEN
   IF lmp(ptr, INT(sptr / 3)) = 0 THEN pick = 0: RETURN
   lmp(ptr, INT(sptr / 3)) = lmp(ptr, INT(sptr / 3)) - 1
  END IF
  'DO ACTUAL EFFECT
  IF spred = 0 THEN
   oobcure ptr, wptr, spel(sptr), spred, stat(), hero()
  ELSE
   FOR i = 0 TO 3
    IF hero(i) > 0 AND (stat(i, 0, 0) > 0 OR targt(sptr) = 4) THEN oobcure ptr, i, spel(sptr), spred, stat(), hero()
   NEXT i
  END IF
  GOSUB curspellist
 END IF
END IF
RETURN

END SUB

SUB status (ptr, hero(), stat(), name$(), exlev&(), gold&, timing(), vpage, dpage, bmenu(), lmp(), carray(), csetup(), gotm, gotj(), mouse(), joy())
DIM sname$(40), sno(9), mtype(5)
getnames sname$()
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

GOSUB nextstat
copypage vpage, 3
centerfuz 160, 100, 304, 184, 1, 3
centerbox 160, 36, 260, 40, 4, 3
centerbox 84, 120, 140, 120, 4, 3
centerbox 236, 120, 140, 120, 4, 3
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
IF carray(5) > 1 OR carray(4) > 1 THEN EXIT SUB
IF carray(2) > 1 OR carray(0) > 1 THEN DO: ptr = loopvar(ptr, 0, 3, -1): LOOP UNTIL hero(ptr) > 0: GOSUB nextstat
IF carray(3) > 1 OR carray(1) > 1 THEN DO: ptr = loopvar(ptr, 0, 3, 1): LOOP UNTIL hero(ptr) > 0: GOSUB nextstat
edgeprint sname$(0), 236 - LEN(sname$(0)) * 4, 65, 15, dpage
edgeprint RIGHT$(STR$(stat(ptr, 0, 0)), LEN(STR$(stat(ptr, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(ptr, 1, 0)), LEN(STR$(stat(ptr, 1, 0))) - 1), 236 - LEN(RIGHT$(STR$(stat(ptr, 0, 0)), LEN(STR$(stat(ptr, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(ptr, 0, 0 _
)), LEN(STR$(stat(ptr, 0, 0))) - 1)) * 4, 75, 15, dpage
FOR i = 0 TO 5
 IF mtype(i) = 0 THEN
  edgeprint sname$(1), 236 - LEN(sname$(1)) * 4, 95, 15, dpage
  edgeprint RIGHT$(STR$(stat(ptr, 0, 1)), LEN(STR$(stat(ptr, 0, 1))) - 1) + "/" + RIGHT$(STR$(stat(ptr, 1, 1)), LEN(STR$(stat(ptr, 1, 1))) - 1), 236 - LEN(RIGHT$(STR$(stat(ptr, 0, 1)), LEN(STR$(stat(ptr, 0, 1))) - 1) + "/" + RIGHT$(STR$(stat(ptr, 0 _
, 1)), LEN(STR$(stat(ptr, 0, 1))) - 1)) * 4, 105, 15, dpage
 END IF
 IF mtype(i) = 1 THEN
  edgeprint "Level " + sname$(1), 236 - LEN("Level " + sname$(1)) * 4, 125, 15, dpage
  temp$ = ""
  FOR o = 0 TO 3
   temp$ = temp$ + RIGHT$(STR$(lmp(ptr, o)), LEN(STR$(lmp(ptr, o))) - 1) + "/"
  NEXT o
  temp$ = LEFT$(temp$, LEN(temp$) - 1)
  edgeprint temp$, 236 - LEN(temp$) * 4, 135, 15, dpage
  temp$ = ""
  FOR o = 4 TO 7
   temp$ = temp$ + RIGHT$(STR$(lmp(ptr, o)), LEN(STR$(lmp(ptr, o))) - 1) + "/"
  NEXT o
  temp$ = LEFT$(temp$, LEN(temp$) - 1)
  edgeprint temp$, 236 - LEN(temp$) * 4, 145, 15, dpage
 END IF
NEXT i
edgeprint name$(ptr), 160 - LEN(name$(ptr)) * 4, 20, 15, dpage
edgeprint "Level" + STR$(stat(ptr, 0, 12)), 160 - LEN("Level" + STR$(stat(ptr, 0, 12))) * 4, 30, 15, dpage
edgeprint "Need" + STR$(exlev&(ptr, 1) - exlev&(ptr, 0)) + " exp. for levelup", 160 - LEN("Need" + STR$(exlev&(ptr, 1) - exlev&(ptr, 0)) + " exp. for levelup") * 4, 40, 15, dpage
edgeprint RIGHT$(STR$(gold&), LEN(STR$(gold&)) - 1) + " " + sname$(32), 148 - LEN(RIGHT$(STR$(gold&), LEN(STR$(gold&)) - 1) + " " + sname$(32)) * 8, 167, 14, dpage
FOR i = 0 TO 9
 edgeprint sname$(sno(i)), 20, 62 + i * 10, 15, dpage
 edgeprint STR$(stat(ptr, 0, i + 2)), 148 - LEN(STR$(stat(ptr, 0, i + 2))) * 8, 62 + i * 10, 15, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
dowait
LOOP

nextstat:
FOR i = 0 TO 5
 mtype(i) = -1
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), hero(ptr) - 1, 0
 IF bmenu(ptr, i) < 0 AND bmenu(ptr, i) > -10 THEN
  temp = (bmenu(ptr, i) + 1) * -1
  IF buffer(243 + temp * 11) > 0 THEN mtype(i) = buffer(288 + temp)
 END IF
NEXT i

RETURN
END SUB

FUNCTION usemenu (ptr, top, first, last, size)

oldptr = ptr
oldtop = top

 IF keyval(72) > 1 THEN ptr = large(ptr - 1, first)
 IF keyval(80) > 1 THEN ptr = small(ptr + 1, last)
 IF keyval(73) > 1 THEN ptr = large(ptr - size, first)
 IF keyval(81) > 1 THEN ptr = small(ptr + size, last)
 top = bound(top, ptr - size, ptr)

IF olptr = ptr AND oldtop = top THEN
 usemenu = 0
ELSE
 usemenu = 1
END IF

END FUNCTION

