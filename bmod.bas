DECLARE SUB playtimer ()
DECLARE SUB debug (s$)
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB advance (s%, who%, atk%(), script%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB heroanim (s%, who%, script%(), atk%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB retreat (s%, who%, script%(), atk%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB etwitch (s%, who%, script%(), atk%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB eretreat (s%, who%, script%(), atk%(), x%(), y%(), w%(), h%(), t%())
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
SUB battle (form, fatal, hero(), pal(), timing(), exstat(), bmenu(), spell(), lmp(), gold&, exlev&(), item(), item$(), eqstuf(), fmvol, carray(), csetup(), gotm, gotj(), mouse(), joy())
DIM sname$(40), a(40), atk(40), st(3, 318), es(7, 160), x(24), y(24), z(24), d(24), zbuf(24), xm(24), ym(24), zm(24), mvx(24), mvy(24), mvz(24), v(24), p(24), w(24), h(24), of(24), ext$(7), ctr(11), stat(11, 1, 11), ready(11), name$(11), menu$(3, 5) _
, mend(3), spel$(23), spel(23), cost$(24), alpha$(10), godo(11), targ(11), t(11, 12), tmask(11), delay(11), script(0 TO 10000), cycle(24), walk(3), sjump(20), aframe(11, 11), fctr(24), harm$(11), hc(11), hx(11), hy(11), die(24), conlmp(11), bits(11 _
, 4), atktype(8), iuse(15), icons(11), ebits(40), eflee(11), firstt(11), ltarg(11), found(16, 1)
getnames sname$()
fademusic 0
fadeto buffer(), 60, 60, 60
stopsong
vpage = 0: dpage = 1: needf = 1: anim = -1: you = -1: them = -1: fiptr = 0
FOR i = 0 TO 11
 icons(i) = -1
NEXT i
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
sjump(0) = 1: sjump(1) = 1: sjump(2) = 6: sjump(3) = 5: sjump(4) = 1
sjump(5) = 2: sjump(6) = 2: sjump(7) = 3: sjump(8) = 6: sjump(9) = 1
sjump(10) = 2: sjump(11) = 3: sjump(12) = 6: sjump(13) = 2: sjump(14) = 2
sjump(15) = 4
FOR i = 0 TO 199
 setpicstuf buffer(), 200, -1
 lb = (item(i) AND 255)
 IF lb > 0 THEN
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
  IF buffer(47) > 0 THEN setbit iuse(), 0, i, 1
 END IF
NEXT i
clearpage 0
clearpage 1
clearpage 2
clearpage 3
GOSUB loadall
copypage 2, dpage

'--main battle loop
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
flash = loopvar(flash, 0, 14, 1)
IF keyval(29) > 0 AND keyval(69) > 0 THEN GOSUB pgame
IF carray(5) > 1 THEN
 flee = flee + 1
END IF
GOSUB tryrun
IF away > 0 THEN
 FOR i = 0 TO 3: xm(i) = 10 * v(i): mvx(i) = 6: walk(i) = 1: d(i) = v(i): NEXT i
 away = away + 1: IF away > 10 THEN GOTO donebattle
END IF
IF keyval(87) > 1 THEN vis = vis XOR 1
IF keyval(88) > 1 THEN snapshot vpage
IF anim >= 0 AND aset = 0 AND vdance = 0 THEN GOSUB atkscript
IF anim >= 0 AND aset = 1 AND vdance = 0 AND away = 0 THEN GOSUB action
GOSUB animate
FOR i = 0 TO 11
 IF you <> i THEN delay(i) = large(delay(i) - 1, 0)
NEXT i
na = loopvar(na, 0, 11, 1)
IF anim = -1 AND vdance = 0 THEN
 GOSUB meters
 IF godo(na) > 0 AND delay(na) = 0 THEN anim = godo(na) - 1: who = na: aset = 0: godo(na) = 0
END IF
yn = loopvar(yn, 0, 3, 1)
IF you = -1 THEN
 IF ready(yn) = 1 AND stat(yn, 0, 0) > 0 AND dead = 0 THEN you = yn: ptr = 0: mset = 0
END IF
en = loopvar(en, 4, 11, 1)
IF them = -1 THEN
 IF ready(en) = 1 AND stat(en, 0, 0) > 0 AND dead = 0 THEN them = en
END IF
IF vdance = 0 THEN
 IF them >= 0 THEN GOSUB enemyai
 IF you >= 0 AND ptarg = 0 THEN
  IF mset = 2 THEN GOSUB itemmenu
  IF mset = 1 THEN GOSUB spellmenu
  IF mset = 0 THEN GOSUB heromenu
 END IF
 IF you >= 0 AND ptarg > 0 THEN GOSUB picktarg
END IF
GOSUB sprite
GOSUB display
IF vdance = -1 THEN vdance = -2
IF vdance > 0 THEN GOSUB vicdance
IF vis = 1 THEN GOSUB seestuff
IF dead = 1 AND vdance = 0 THEN
 o = 0
 FOR i = 4 TO 11
  IF die(i) > 0 THEN o = 1
 NEXT i
 IF o = 0 THEN GOSUB victory
END IF
IF vdance = -2 GOTO donebattle
IF dead = 2 THEN
 fatal = 1
 GOTO donebattle
END IF
IF alert > 0 THEN
 alert = alert - 1
 centerfuz 160, 190, 100, 16, 3, dpage
 edgeprint alert$, 160 - LEN(alert$) * 4, 185, 14 + tog, dpage
END IF
SWAP vpage, dpage
setvispage vpage
copypage 2, dpage
IF needf = 1 THEN needf = 0: fademusic fmvol: fadetopal master(), buffer(): setkeys
dowait
LOOP

pgame:
fuzzyrect 0, 0, 320, 200, 19, vpage
edgeprint "PAUSE", xstring("PAUSE", 160), 95, 15, vpage
w = getkey
RETURN

enemyai:
ai = 0
IF stat(them, 0, 0) < stat(them, 1, 0) / 5 THEN ai = 1
o = 0
FOR i = 4 TO 11
 IF v(i) = 1 AND stat(i, 0, 0) > 0 THEN o = o + 1
NEXT i
IF o = 1 THEN ai = 2
IF ai = 2 AND es(them - 4, 81) THEN
 FOR j = 1 TO es(them - 4, 91)
  slot = -1
  FOR k = 7 TO 0 STEP -1
   IF a(k * 4) = 0 THEN slot = k
  NEXT k
  IF slot > -1 THEN
   a(slot * 4) = es(them - 4, 81)
   loadfoe slot, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), name$(), stat(), ebits()
  END IF
 NEXT j
END IF
o = 0
FOR i = 0 TO 4
 IF es(them - 4, 92 + (ai * 5) + i) > 0 THEN o = o + 1
NEXT i
IF o = 0 THEN
 ai = 0
 IF stat(them, 0, 0) < stat(them, 1, 0) / 5 THEN ai = 1
END IF
o = 0
FOR i = 0 TO 4
 IF es(them - 4, 92 + (ai * 5) + i) > 0 THEN o = o + 1
NEXT i
IF o = 0 THEN ai = 0
o = 0
FOR i = 0 TO 4
 IF es(them - 4, 92 + (ai * 5) + i) > 0 THEN o = o + 1
NEXT i
IF o > 0 THEN
 lim = 0
 DO
  godo(them) = es(them - 4, 92 + (ai * 5) + INT(RND * 5))
 LOOP UNTIL godo(them) > 0
 setpicstuf buffer(), 80, -1
 loadset game$ + ".dt6" + CHR$(0), godo(them) - 1, 0
 delay(them) = buffer(16)
 FOR i = 0 TO 11
  t(them, i) = -1
 NEXT i
 j = them
 IF buffer(4) = 0 OR buffer(4) >= 2 OR buffer(4) <= 4 THEN GOSUB eaifocus
 IF buffer(4) = 1 OR (buffer(4) = 2 AND INT(RND * 100) < 33) THEN GOSUB eaispread
END IF
IF stat(them, 0, 1) - buffer(8) < 0 THEN godo(them) = 0
IF godo(them) = 0 THEN
 IF readbit(ebits(), (them - 4) * 5, 55) = 0 THEN them = -1: RETURN
END IF
ready(them) = 0: ctr(them) = 0: them = -1
RETURN

eaifocus:
FOR ii = 0 TO 11
 t(j, ii) = -1
NEXT ii
ii = 0
FOR o = 0 TO 3
 IF stat(o, 0, 0) > 0 THEN ii = 1
NEXT o
IF ii = 0 THEN RETURN
ii = 0
FOR o = 4 TO 11
 IF j < 4 THEN
  IF stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN ii = 1
 END IF
 IF j >= 4 THEN
  IF stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN ii = 1
 END IF
NEXT o
IF ii = 0 THEN RETURN
ii = 0
 IF buffer(3) = 0 THEN
  ol = 0
  DO
   IF j >= 4 THEN o = INT(RND * 4) ELSE o = 4 + INT(RND * 8)
   t(j, ii) = o
   ol = ol + 1
   IF (v(o) = 1 AND stat(o, 0, 0) > 0) THEN
    IF j >= 4 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN EXIT DO
    IF j < 4 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN EXIT DO
   END IF
  LOOP UNTIL ol > 999
 END IF
 IF buffer(3) = 1 OR buffer(3) = 4 OR buffer(3) = 5 THEN
  ol = 0
  DO
   IF j >= 4 THEN o = 4 + INT(RND * 8) ELSE o = INT(RND * 4)
   IF buffer(3) = 5 THEN
    WHILE j = o
     IF j >= 4 THEN o = 4 + INT(RND * 8) ELSE o = INT(RND * 4)
    WEND
   END IF
   t(j, ii) = o
   ol = ol + 1
   IF (v(o) = 1 AND stat(o, 0, 0) > 0) THEN
    IF j >= 4 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN EXIT DO
    IF j < 4 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN EXIT DO
   END IF
  LOOP UNTIL ol > 999
 END IF
 IF buffer(3) = 2 THEN t(j, 0) = j
 IF buffer(3) = 3 THEN
  ol = 0
  DO
   o = INT(RND * 12)
   t(j, ii) = o
   ol = ol + 1
   IF (v(o) = 1 AND stat(o, 0, 0) > 0) THEN
    IF j >= 4 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN EXIT DO
    IF j < 4 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN EXIT DO
   END IF
  LOOP UNTIL ol > 999
 END IF
RETURN

eaispread:
FOR ii = 0 TO 11
 t(j, ii) = -1
NEXT ii
'ii = 0
'IF j < 4 THEN
' FOR o = 4 TO 11
'  IF v(o) = 1 THEN ii = 1
' NEXT o
' IF ii = 0 THEN RETURN
'END IF
'ii = 0
'IF j >= 4 THEN
' FOR o = 0 TO 3
'  IF v(o) = 1 THEN ii = 1
' NEXT o
' IF ii = 0 THEN RETURN
'END IF
ii = 0
FOR o = 0 TO 3
 IF stat(o, 0, 0) > 0 THEN ii = 1
NEXT o
IF ii = 0 THEN RETURN
ii = 0
FOR o = 4 TO 11
 IF j < 4 THEN
  IF stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN ii = 1
 END IF
 IF j >= 4 THEN
  IF stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN ii = 1
 END IF
NEXT o
IF ii = 0 THEN RETURN
ii = 0
 IF buffer(3) = 0 AND j >= 4 THEN
  FOR o = 0 TO 3
   IF v(o) = 1 AND stat(o, 0, 0) > 0 THEN t(j, ii) = o: ii = ii + 1
  NEXT o
 END IF
 IF buffer(3) = 0 AND j < 4 THEN
  FOR o = 4 TO 11
   IF j < 4 THEN
    IF v(o) = 1 AND stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN t(j, ii) = o: ii = ii + 1
   END IF
   IF j >= 4 THEN
    IF v(o) = 1 AND stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN t(j, ii) = o: ii = ii + 1
   END IF
  NEXT o
 END IF
 IF buffer(3) = 1 AND j >= 4 THEN
  FOR o = 4 TO 11
   IF v(o) = 1 AND stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN t(j, ii) = o: ii = ii + 1
  NEXT o
 END IF
 IF buffer(3) = 1 AND j < 4 THEN
  FOR o = 0 TO 3
   IF v(o) = 1 AND stat(o, 0, 0) > 0 THEN t(j, ii) = o: ii = ii + 1
  NEXT o
 END IF
 IF buffer(3) = 2 THEN t(j, 0) = j
 IF buffer(3) = 3 THEN
  FOR o = 0 TO 11
   IF j < 4 THEN
    IF v(o) = 1 AND stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN t(j, ii) = o: ii = ii + 1
   END IF
   IF j >= 4 THEN
    IF v(o) = 1 AND stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN t(j, ii) = o: ii = ii + 1
   END IF
  NEXT o
 END IF
RETURN

heromenu:
IF carray(5) > 1 THEN : yn = you: you = -1: RETURN
IF carray(0) > 1 THEN ptr = ptr - 1: IF ptr < 0 THEN ptr = mend(you)
IF carray(1) > 1 THEN ptr = ptr + 1: IF ptr > mend(you) THEN ptr = 0
IF carray(4) > 1 THEN
 IF bmenu(you, ptr) > 0 THEN
  godo(you) = bmenu(you, ptr)
  setpicstuf buffer(), 80, -1
  loadset game$ + ".dt6" + CHR$(0), godo(you) - 1, 0
  delay(you) = large(buffer(16), 1)
  ptarg = 1: setkeys
  FOR i = 0 TO 7: carray(i) = 0: NEXT i
  RETURN
 END IF
 IF bmenu(you, ptr) < 0 AND bmenu(you, ptr) > -4 AND st(you, 288 + (bmenu(you, ptr) + 1) * -1) < 2 THEN
  mset = 1: sptr = 0
  sptype = (bmenu(you, ptr) + 1) * -1
  FOR i = 0 TO 23
   spel$(i) = "": cost$(i) = "": spel(i) = -1
   IF spell(you, sptype, i) > 0 THEN
    spel(i) = spell(you, sptype, i) - 1
    setpicstuf buffer(), 80, -1
    loadset game$ + ".dt6" + CHR$(0), spel(i), 0
    FOR j = 26 TO 25 + buffer(24)
     spel$(i) = spel$(i) + CHR$(buffer(j))
    NEXT j
    IF st(you, 288 + sptype) = 0 THEN cost$(i) = STR$(buffer(8)) + "MP  " + RIGHT$(STR$(stat(you, 0, 1)), LEN(STR$(stat(you, 0, 1))) - 1) + "/" + RIGHT$(STR$(stat(you, 1, 1)), LEN(STR$(stat(you, 1, 1))) - 1)
    IF st(you, 288 + sptype) = 1 THEN cost$(i) = "Level " + alpha$(INT(i / 3) + 1) + "  " + STR$(lmp(you, INT(i / 3)))
   END IF
   WHILE LEN(spel$(i)) < 10: spel$(i) = spel$(i) + " ": WEND
  NEXT i
 END IF
 IF bmenu(you, ptr) < 0 AND bmenu(you, ptr) > -4 AND st(you, 288 + (bmenu(you, ptr) + 1) * -1) = 2 THEN
  last = -1
  sptype = (bmenu(you, ptr) + 1) * -1
  FOR i = 0 TO 23
   spel(i) = -1
   IF spell(you, sptype, i) > 0 THEN
    spel(i) = spell(you, sptype, i) - 1: last = i
   END IF
  NEXT i
  IF last = -1 THEN RETURN
  rptr = INT(RND * 24)
  FOR i = 0 TO INT(RND * last + 1)
   ol = 0
   DO
    rptr = loopvar(rptr, 0, 23, 1)
    ol = ol + 1
   LOOP UNTIL spel(rptr) > -1 OR ol > 999
  NEXT i
  godo(you) = spel(rptr) + 1
  setpicstuf buffer(), 80, -1
  loadset game$ + ".dt6" + CHR$(0), godo(you) - 1, 0
  delay(you) = large(buffer(16), 1)
  ptarg = 1: setkeys
  FOR i = 0 TO 7: carray(i) = 0: NEXT i
  RETURN
 END IF
 IF bmenu(you, ptr) = -10 THEN mset = 2: iptr = 0: itop = 0
END IF
RETURN

atkscript:
setpicstuf atk(), 80, -1
loadset game$ + ".dt6" + CHR$(0), anim, 0
setpicstuf buffer(), 3750, 3
loadset game$ + ".pt6" + CHR$(0), atk(0), 144
FOR i = 12 TO 23
 p(i) = atk(1)
 of(i) = 0
 cycle(i) = -1
 z(i) = 0
NEXT i
s = 0: tcount = -1: pdir = 0: conmp = 1
IF who >= 4 THEN pdir = 1
ltarg(who) = 0
'CANNOT HIT INVISABLE FOES
FOR i = 0 TO 11
 IF t(who, i) > -1 THEN
  IF v(t(who, i)) = 0 AND atk(3) <> 4 THEN
   t(who, i) = -1
  END IF
 END IF
NEXT i
'MOVE EMPTY TARGET SLOTS TO THE BACK
FOR o = 0 TO 10
 FOR i = 0 TO 10
  IF t(who, i) = -1 THEN SWAP t(who, i), t(who, i + 1)
 NEXT i
NEXT o
'COUNT TARGETS
FOR i = 0 TO 11
 IF t(who, i) > -1 THEN tcount = tcount + 1
 cycle(i) = -1
NEXT i
atktype(0) = 1
FOR i = 0 TO 7
 atktype(i + 1) = 0
 IF readbit(atk(), 20, 5 + i) = 1 THEN atktype(i + 1) = 1: atktype(0) = 0
NEXT i
'ABORT IF TARGETLESS
IF tcount = -1 THEN anim = -1: RETURN
script(s) = 0
' BIG CRAZY SCRIPT CONSTRUCTION
IF who < 4 THEN
 setpicstuf buffer(), 576, 3
 loadset game$ + ".pt5" + CHR$(0), exstat(who, 0, 13), 156
 p(24) = exstat(who, 1, 13)
END IF
'----------------------------NULL ANIMATION
IF atk(15) = 10 THEN
 IF who < 4 THEN advance s, who, atk(), script(), x(), y(), w(), h(), t()
 FOR j = 1 TO atk(17) + INT(RND * (stat(who, 0, 11) + 1))
  IF who < 4 THEN heroanim s, who, script(), atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch s, who, script(), atk(), x(), y(), w(), h(), t()
  FOR i = 0 TO tcount
   script(s) = 10: script(s + 1) = t(who, i): s = s + 2
  NEXT i
  script(s) = 6: script(s + 1) = 24: s = s + 2
 NEXT j
 IF who < 4 THEN retreat s, who, script(), atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat s, who, script(), atk(), x(), y(), w(), h(), t()
 script(s) = 0: s = s + 1
END IF
'----------------------------NORMAL, DROP, SPREAD-RING, and SCATTER
IF atk(15) = 0 OR atk(15) = 3 OR atk(15) = 6 OR (atk(15) = 4 AND tcount > 0) THEN
 FOR i = 0 TO tcount
  yt = (h(t(who, i)) - 50) + 2
  xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
  script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = pdir: s = s + 5
  IF atk(15) = 3 THEN script(s) = 11: script(s + 1) = 12 + i: script(s + 2) = 180: s = s + 3
  IF atk(15) = 4 THEN script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt: script(s + 3) = y(t(who, i)) + yt - w(t(who, i)): script(s + 4) = pdir: s = s + 5
 NEXT i
 IF who < 4 THEN advance s, who, atk(), script(), x(), y(), w(), h(), t()
 FOR j = 1 TO atk(17) + INT(RND * (stat(who, 0, 11) + 1))
  IF who < 4 THEN heroanim s, who, script(), atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch s, who, script(), atk(), x(), y(), w(), h(), t()
  FOR i = 0 TO tcount
   script(s) = 5: script(s + 1) = 12 + i: s = s + 2
   IF atk(15) = 4 THEN script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt - w(t(who, i)): script(s + 3) = y(t(who, i)) + yt: script(s + 4) = 3: script(s + 5) = 3: s = s + 6
   IF atk(15) = 3 THEN script(s) = 15: script(s + 1) = 12 + i: script(s + 2) = -10: script(s + 3) = 20: s = s + 4
   IF atk(15) = 6 THEN script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = INT(RND * 270): script(s + 3) = INT(RND * 150): script(s + 4) = 6: script(s + 5) = 6: s = s + 6
  NEXT i
  script(s) = 13: script(s + 1) = 2: s = s + 2
  IF atk(15) = 3 THEN script(s) = 13: script(s + 1) = 3: s = s + 2
  script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
  script(s) = 6: script(s + 1) = 24: s = s + 2
  IF atk(15) = 4 THEN
   FOR i = 0 TO tcount
    script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt: script(s + 3) = y(t(who, i)) + yt + w(t(who, i)): script(s + 4) = 3: script(s + 5) = 3: s = s + 6
   NEXT i
   script(s) = 9: s = s + 1
   FOR i = 0 TO tcount
    script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt + w(t(who, i)): script(s + 3) = y(t(who, i)) + yt: script(s + 4) = 3: script(s + 5) = 3: s = s + 6
   NEXT i
   script(s) = 9: s = s + 1
   FOR i = 0 TO tcount
    script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt: script(s + 3) = y(t(who, i)) + yt - w(t(who, i)): script(s + 4) = 3: script(s + 5) = 3: s = s + 6
   NEXT i
   script(s) = 9: s = s + 1
  END IF
  FOR i = 0 TO tcount
   script(s) = 10: script(s + 1) = t(who, i): s = s + 2
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   IF readbit(atk(), 20, 0) = 0 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 5: s = s + 3
   IF readbit(atk(), 20, 0) = 1 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 2: s = s + 3
  NEXT i
  IF atk(15) <> 4 THEN script(s) = 13: script(s + 1) = 3: s = s + 2
  FOR i = 0 TO tcount
   script(s) = 6: script(s + 1) = 12 + i: s = s + 2
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
  NEXT i
  script(s) = 13: script(s + 1) = 2: s = s + 2
 NEXT j
 IF who < 4 THEN retreat s, who, script(), atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat s, who, script(), atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
 NEXT i
 script(s) = 0: s = s + 1
END IF
'----------------------------SEQUENTIAL PROJECTILE
IF atk(15) = 7 THEN
 IF who < 4 THEN advance s, who, atk(), script(), x(), y(), w(), h(), t()
 FOR j = 1 TO atk(17) + INT(RND * (stat(who, 0, 11) + 1))
  IF who < 4 THEN heroanim s, who, script(), atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch s, who, script(), atk(), x(), y(), w(), h(), t()
  temp = 50: IF who < 4 THEN temp = -50
  dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
  script(s) = 3: script(s + 1) = 12: script(s + 2) = x(who) + temp: script(s + 3) = y(who): script(s + 4) = dtemp: s = s + 5
  script(s) = 5: script(s + 1) = 12: s = s + 2
  FOR i = 0 TO tcount
   yt = (h(t(who, i)) - 50) + 2
   xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
   script(s) = 8: script(s + 1) = 12: script(s + 2) = x(t(who, i)) + xt: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = 5: script(s + 5) = 5: s = s + 6
   script(s) = 9: s = s + 1
   script(s) = 10: script(s + 1) = t(who, i): s = s + 2
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   IF readbit(atk(), 20, 0) = 0 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 5: s = s + 3
   IF readbit(atk(), 20, 0) = 1 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 2: s = s + 3
   script(s) = 13: script(s + 1) = 3: s = s + 2
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
   IF i = 0 THEN script(s) = 6: script(s + 1) = 24: s = s + 2
  NEXT i
  IF who < 4 THEN script(s) = 8: script(s + 1) = 12: script(s + 2) = -50: script(s + 3) = 100: script(s + 4) = 5: script(s + 5) = 5: s = s + 6
  IF who >= 4 THEN script(s) = 8: script(s + 1) = 12: script(s + 2) = 320: script(s + 3) = 100: script(s + 4) = 5: script(s + 5) = 5: s = s + 6
  script(s) = 9: s = s + 1
  script(s) = 6: script(s + 1) = 12: s = s + 2
 NEXT j
 IF who < 4 THEN retreat s, who, script(), atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat s, who, script(), atk(), x(), y(), w(), h(), t()
 script(s) = 0: s = s + 1
END IF
'-----------------PROJECTILE, REVERSE PROJECTILE and METEOR
IF (atk(15) >= 1 AND atk(15) <= 2) OR atk(15) = 8 THEN
 IF who < 4 THEN advance s, who, atk(), script(), x(), y(), w(), h(), t()
 FOR j = 1 TO atk(17) + INT(RND * (stat(who, 0, 11) + 1))
  FOR i = 0 TO tcount
   temp = 50: IF who < 4 THEN temp = -50
   dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
   yt = (h(t(who, i)) - 50) + 2
   xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 THEN script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = x(who) + temp: script(s + 3) = y(who): script(s + 4) = dtemp: s = s + 5
   IF atk(15) = 2 THEN script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = dtemp: s = s + 5
   IF atk(15) = 8 THEN
    IF who < 4 THEN script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = 320: script(s + 3) = 100: script(s + 4) = dtemp: s = s + 5
    IF who >= 4 THEN script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = -50: script(s + 3) = 100: script(s + 4) = dtemp: s = s + 5
    script(s) = 11: script(s + 1) = 12 + i: script(s + 2) = 180: s = s + 3
   END IF
  NEXT i
  IF who < 4 THEN heroanim s, who, script(), atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch s, who, script(), atk(), x(), y(), w(), h(), t()
  FOR i = 0 TO tcount
   script(s) = 5: script(s + 1) = 12 + i: s = s + 2
   temp = 50: IF who < 4 THEN temp = -50
   yt = (h(t(who, i)) - 50) + 2
   xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 OR atk(15) = 8 THEN script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = 6: script(s + 5) = 6: s = s + 6
   IF atk(15) = 2 THEN script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = x(who) + temp: script(s + 3) = y(who): script(s + 4) = 6: script(s + 5) = 6: s = s + 6
   IF atk(15) = 8 THEN script(s) = 15: script(s + 1) = 12 + i: script(s + 2) = -6: script(s + 3) = 30: s = s + 4
  NEXT i
  script(s) = 13: script(s + 1) = 8: s = s + 2
  script(s) = 6: script(s + 1) = 24: s = s + 2
  script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
  FOR i = 0 TO tcount
   script(s) = 10: script(s + 1) = t(who, i): s = s + 2
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   IF readbit(atk(), 20, 0) = 0 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 5: s = s + 3
   IF readbit(atk(), 20, 0) = 1 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 2: s = s + 3
  NEXT i
  script(s) = 13: script(s + 1) = 3: s = s + 2
  FOR i = 0 TO tcount
   script(s) = 6: script(s + 1) = 12 + i: s = s + 2
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
  NEXT i
  script(s) = 13: script(s + 1) = 3: s = s + 2
 NEXT j
 IF who < 4 THEN retreat s, who, script(), atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat s, who, script(), atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
 NEXT i
 script(s) = 0: s = s + 1
END IF
'--------------------------------------DRIVEBY
IF atk(15) = 9 THEN
 IF who < 4 THEN advance s, who, atk(), script(), x(), y(), w(), h(), t()
 FOR j = 1 TO atk(17) + INT(RND * (stat(who, 0, 11) + 1))
  dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
  FOR i = 0 TO tcount
   yt = (h(t(who, i)) - 50) + 2
   IF who < 4 THEN script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = 320: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = dtemp: s = s + 5
   IF who >= 4 THEN script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = -50: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = dtemp: s = s + 5
  NEXT i
  IF who < 4 THEN heroanim s, who, script(), atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch s, who, script(), atk(), x(), y(), w(), h(), t()
  FOR i = 0 TO tcount
   script(s) = 5: script(s + 1) = 12 + i: s = s + 2
   temp = 50: IF who < 4 THEN temp = -50
   yt = (h(t(who, i)) - 50) + 2
   script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = x(t(who, i)) + xt: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = 8: script(s + 5) = 8: s = s + 6
  NEXT i
  script(s) = 13: script(s + 1) = 4: s = s + 2
  script(s) = 6: script(s + 1) = 24: s = s + 2
  script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
  script(s) = 9: s = s + 1
  FOR i = 0 TO tcount
   script(s) = 10: script(s + 1) = t(who, i): s = s + 2
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   IF readbit(atk(), 20, 0) = 0 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 5: s = s + 3
   IF readbit(atk(), 20, 0) = 1 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 2: s = s + 3
   yt = (h(t(who, i)) - 50) + 2
   IF who < 4 THEN script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = -50: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = 5: script(s + 5) = 7: s = s + 6
   IF who >= 4 THEN script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = 320: script(s + 3) = y(t(who, i)) + yt: script(s + 4) = 5: script(s + 5) = 7: s = s + 6
  NEXT i
  script(s) = 9: s = s + 1
  FOR i = 0 TO tcount
   script(s) = 6: script(s + 1) = 12 + i: s = s + 2
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
  NEXT i
  script(s) = 13: script(s + 1) = 3: s = s + 2
 NEXT j
 IF who < 4 THEN retreat s, who, script(), atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat s, who, script(), atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
 NEXT i
 script(s) = 0: s = s + 1
END IF
'--------------------------------FOCUSED RING
IF atk(15) = 4 AND tcount = 0 THEN
 dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
 IF who < 4 THEN advance s, who, atk(), script(), x(), y(), w(), h(), t()
 FOR j = 1 TO atk(17) + INT(RND * (stat(who, 0, 11) + 1))
  i = 0
  yt = (h(t(who, i)) - 50) + 2
  xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
  script(s) = 3: script(s + 1) = 12 + 0: script(s + 2) = x(t(who, i)) + xt + 0: script(s + 3) = y(t(who, i)) + yt - 50: script(s + 4) = dtemp: s = s + 5
  script(s) = 3: script(s + 1) = 12 + 1: script(s + 2) = x(t(who, i)) + xt + 30: script(s + 3) = y(t(who, i)) + yt - 30: script(s + 4) = dtemp: s = s + 5
  script(s) = 3: script(s + 1) = 12 + 2: script(s + 2) = x(t(who, i)) + xt + 50: script(s + 3) = y(t(who, i)) + yt + 0: script(s + 4) = dtemp: s = s + 5
  script(s) = 3: script(s + 1) = 12 + 3: script(s + 2) = x(t(who, i)) + xt + 30: script(s + 3) = y(t(who, i)) + yt + 30: script(s + 4) = dtemp: s = s + 5
  script(s) = 3: script(s + 1) = 12 + 4: script(s + 2) = x(t(who, i)) + xt + 0: script(s + 3) = y(t(who, i)) + yt + 50: script(s + 4) = dtemp: s = s + 5
  script(s) = 3: script(s + 1) = 12 + 5: script(s + 2) = x(t(who, i)) + xt - 30: script(s + 3) = y(t(who, i)) + yt + 30: script(s + 4) = dtemp: s = s + 5
  script(s) = 3: script(s + 1) = 12 + 6: script(s + 2) = x(t(who, i)) + xt - 50: script(s + 3) = y(t(who, i)) + yt + 0: script(s + 4) = dtemp: s = s + 5
  script(s) = 3: script(s + 1) = 12 + 7: script(s + 2) = x(t(who, i)) + xt - 30: script(s + 3) = y(t(who, i)) + yt - 30: script(s + 4) = dtemp: s = s + 5
  IF who < 4 THEN heroanim s, who, script(), atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch s, who, script(), atk(), x(), y(), w(), h(), t()
  yt = (h(t(who, 0)) - 50) + 2
  xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
  FOR i = 0 TO 7
   script(s) = 5: script(s + 1) = 12 + i: s = s + 2
   script(s) = 8: script(s + 1) = 12 + i: script(s + 2) = x(t(who, 0)) + xt: script(s + 3) = y(t(who, 0)) + yt: script(s + 4) = 4: script(s + 5) = 4: s = s + 6
  NEXT i
  script(s) = 13: script(s + 1) = 8: s = s + 2
  script(s) = 6: script(s + 1) = 24: s = s + 2
  script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
  FOR i = 0 TO tcount
   script(s) = 10: script(s + 1) = t(who, i): s = s + 2
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   IF readbit(atk(), 20, 0) = 0 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 5: s = s + 3
   IF readbit(atk(), 20, 0) = 1 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 2: s = s + 3
  NEXT i
  script(s) = 13: script(s + 1) = 3: s = s + 2
  FOR i = 0 TO 7
   script(s) = 6: script(s + 1) = 12 + i: s = s + 2
  NEXT i
  FOR i = 0 TO tcount
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
  NEXT i
  script(s) = 13: script(s + 1) = 3: s = s + 2
 NEXT j
 IF who < 4 THEN retreat s, who, script(), atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat s, who, script(), atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
 NEXT i
 script(s) = 0: s = s + 1
END IF
'--------------------------------WAVE
IF atk(15) = 5 THEN
 yt = y(t(who, 0)) + (h(t(who, 0)) - 50) + 2
 IF who < 4 THEN advance s, who, atk(), script(), x(), y(), w(), h(), t()
 FOR j = 1 TO atk(17) + INT(RND * (stat(who, 0, 11) + 1))
  FOR i = 0 TO 11
   temp = -50: IF who < 4 THEN temp = 320
   IF tcount > 0 OR atk(4) = 1 THEN
    script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = temp: script(s + 3) = i * 15: script(s + 4) = pdir: s = s + 5
   ELSE
    script(s) = 3: script(s + 1) = 12 + i: script(s + 2) = temp: script(s + 3) = yt: script(s + 4) = pdir: s = s + 5
   END IF
  NEXT i
  IF who < 4 THEN heroanim s, who, script(), atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch s, who, script(), atk(), x(), y(), w(), h(), t()
  temp = 24: IF who < 4 THEN temp = -24
  FOR i = 0 TO 11
   script(s) = 5: script(s + 1) = 12 + i: s = s + 2
   script(s) = 2: script(s + 1) = 12 + i: script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 16: script(s + 5) = 0: s = s + 6
   script(s) = 13: script(s + 1) = 1: s = s + 2
  NEXT i
  script(s) = 13: script(s + 1) = 15: s = s + 2
  script(s) = 6: script(s + 1) = 24: s = s + 2
  script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
  FOR i = 0 TO tcount
   script(s) = 10: script(s + 1) = t(who, i): s = s + 2
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   IF readbit(atk(), 20, 0) = 0 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 5: s = s + 3
   IF readbit(atk(), 20, 0) = 1 THEN script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 2: s = s + 3
  NEXT i
  script(s) = 9: s = s + 1
  FOR i = 0 TO 11
   script(s) = 6: script(s + 1) = 12 + i: s = s + 2
  NEXT i
  FOR i = 0 TO tcount
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   script(s) = 2: script(s + 1) = t(who, i): script(s + 2) = temp: script(s + 3) = 0: script(s + 4) = 2: script(s + 5) = 0: s = s + 6
   script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
  NEXT i
  script(s) = 13: script(s + 1) = 2: s = s + 2
 NEXT j
 IF who < 4 THEN retreat s, who, script(), atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat s, who, script(), atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  script(s) = 7: script(s + 1) = t(who, i): script(s + 2) = 0: s = s + 3
 NEXT i
 script(s) = 0: s = s + 1
END IF
FOR i = 0 TO 11
 fctr(i) = 0
 IF atk(2) = 0 THEN aframe(i, 0) = 0: aframe(i, 1) = 0: aframe(i, 2) = 1: aframe(i, 3) = 1: aframe(i, 4) = 2: aframe(i, 5) = 2: aframe(i, 6) = -1
 IF atk(2) = 1 THEN aframe(i, 0) = 2: aframe(i, 1) = 2: aframe(i, 2) = 1: aframe(i, 3) = 1: aframe(i, 4) = 0: aframe(i, 5) = 0: aframe(i, 6) = -1
 IF atk(2) = 2 THEN aframe(i, 0) = 0: aframe(i, 1) = 0: aframe(i, 2) = 1: aframe(i, 3) = 1: aframe(i, 4) = 2: aframe(i, 5) = 2: aframe(i, 6) = 1: aframe(i, 7) = 1: aframe(i, 8) = -1
 IF atk(2) = 3 THEN aframe(i, 0) = -1: aframe(i, 1) = -1
NEXT i
s = 0: aset = 1
RETURN


action:
IF wf > 0 THEN wf = wf - 1: IF wf > 0 THEN RETURN
IF wf = -1 THEN
 wf = 0
 FOR i = 0 TO 23
  IF xm(i) <> 0 OR ym(i) <> 0 OR zm(i) <> 0 THEN wf = -1
 NEXT i
 IF wf = -1 THEN RETURN
END IF
wf = 0
DO: 'INTERPRET THE ANIMATION SCRIPT
act = script(s)
IF act = 0 THEN
 FOR i = 0 TO 3
  x(i) = (240 + i * 8)
  y(i) = (82 + i * 20)
 NEXT i
 FOR i = 0 TO 7
  IF eflee(4 + i) = 0 THEN
   x(4 + i) = a(i * 4 + 1)
   y(4 + i) = a(i * 4 + 2)
  END IF
 NEXT i
 anim = -1
END IF
IF act = 1 THEN
 FOR i = 0 TO 3
  a(i * 4 + 1) = x(4 + i)
  a(i * 4 + 2) = y(4 + i)
 NEXT i
 anim = -1
END IF
IF act = 2 THEN
 w = script(s + 1)
 xm(w) = script(s + 2)
 ym(w) = script(s + 3)
 mvx(w) = script(s + 4)
 mvy(w) = script(s + 5)
END IF
IF act = 3 THEN
 w = script(s + 1)
 x(w) = script(s + 2)
 y(w) = script(s + 3)
 d(w) = script(s + 4)
END IF
IF act = 4 THEN
 'SOMETHIN GOES HERE
END IF
IF act = 5 THEN
 v(script(s + 1)) = 1
END IF
IF act = 6 THEN
 v(script(s + 1)) = 0
END IF
IF act = 7 THEN
 w = script(s + 1)
 IF w < 4 THEN walk(w) = 0: of(w) = script(s + 2)
 IF w > 23 THEN of(w) = script(s + 2)
END IF
IF act = 8 THEN
 w = script(s + 1)
 mvx(w) = (script(s + 2) - x(w)) / script(s + 4)
 mvy(w) = (script(s + 3) - y(w)) / script(s + 5)
 xm(w) = script(s + 4)
 ym(w) = script(s + 5)
END IF
IF act = 9 THEN
 wf = -1
END IF
IF act = 10 THEN
 'IF stat(script(s + 1), 0, 0) THEN
  inflict who, script(s + 1), stat(), x(), y(), w(), h(), harm$(), hc(), hx(), hy(), atk(), tcount, die(), bits()
  IF stat(script(s + 1), 0, 0) <= 0 THEN
   die(script(s + 1)) = w(script(s + 1)) * .5
   IF script(s + 1) >= 4 THEN
    IF readbit(ebits(), (script(s + 1) - 4) * 5, 59) = 1 THEN eflee(script(s + 1)) = 1: die(script(s + 1)) = (w(script(s + 1)) + x(script(s + 1))) / 10
   END IF
  ELSE
   '---REVIVE---
   v(script(s + 1)) = 1: die(script(s + 1)) = 0
  END IF
 'END IF
 IF script(s + 1) >= 4 THEN GOSUB sponhit
 IF atk(8) > 0 AND conmp = 1 THEN stat(who, 0, 1) = large(stat(who, 0, 1) - atk(8), 0): conmp = 0
 IF conlmp(who) > 0 THEN lmp(who, conlmp(who) - 1) = lmp(who, conlmp(who) - 1) - 1: conlmp(who) = 0
 IF icons(who) >= 0 THEN GOSUB iconsume
 IF atk(9) <> 0 THEN
  hc(who) = 7
  hx(who) = x(who) + (w(who) * .5)
  hy(who) = y(who) + (h(who) * .5)
  temp = large(range(atk(9), 50), 1)
  harm$(who) = RIGHT$(STR$(temp), LEN(STR$(temp)) - 1)
  stat(who, 0, 0) = large(stat(who, 0, 0) - temp, 0)
  IF stat(who, 0, 0) <= 0 THEN
   die(who) = w(who) * .5
   IF who >= 4 THEN
    IF readbit(ebits(), (who - 4) * 5, 59) = 1 THEN eflee(who) = 1: die(who) = (w(who) + x(who)) / 10
   END IF
  END IF
 END IF
 IF atk(10) <> 0 THEN
  hc(who) = 7
  hx(who) = x(who) + (w(who) * .5)
  hy(who) = y(who) + (h(who) * .5)
  temp = large(range(ABS(atk(10)), 50), 1)
  temp = temp * SGN(atk(10))
  harm$(who) = RIGHT$(STR$(temp), LEN(STR$(temp)) - 1) + "$"
  IF atk(10) < 0 THEN harm$(who) = "+" + harm$(who)
  gold& = gold& - temp
  IF gold& > 1000000000 THEN gold& = 1000000000
  IF gold& < 0 THEN gold& = 0
 END IF
 o = 0
 FOR i = 0 TO 3
  IF stat(i, 0, 0) = 0 THEN o = o + 1
 NEXT i
 IF o = 4 THEN anim = -1
 o = 0
 FOR i = 4 TO 11
  IF stat(i, 0, 0) = 0 THEN o = o + 1
 NEXT i
 IF stat(script(s + 1), 0, 0) = 0 AND o < 8 AND anim > -1 THEN
  j = who
  IF atk(4) = 0 OR atk(4) >= 2 OR atk(4) <= 4 THEN GOSUB eaifocus
  IF atk(4) = 1 OR (atk(4) = 2 AND INT(RND * 100) < 33) THEN GOSUB eaispread
 END IF
END IF
IF act = 11 THEN
 w = script(s + 1)
 z(w) = script(s + 2)
END IF
IF act = 12 THEN
 'SOMETHIN GOES HERE
END IF
IF act = 13 THEN
 wf = script(s + 1)
END IF
IF act = 14 THEN
 w = script(s + 1)
 of(w) = 0
 IF w < 4 THEN walk(w) = walk(w) XOR 1
END IF
IF act = 15 THEN
 w = script(s + 1)
 zm(w) = script(s + 2)
 mvz(w) = script(s + 3)
END IF
s = s + sjump(act)
LOOP UNTIL wf <> 0 OR anim = -1
IF anim = -1 THEN GOSUB afterdone
'-------Spawn a Chained Attack--------
IF anim = -1 AND atk(12) > 0 AND INT(RND * 100) < atk(13) AND stat(who, 0, 0) > 0 THEN
 wf = 0: aset = 0
 setpicstuf buffer(), 80, -1
 loadset game$ + ".dt6" + CHR$(0), atk(12) - 1, 0
 IF buffer(16) > 0 THEN
  godo(who) = atk(12)
  delay(who) = buffer(16)
 ELSE
  anim = atk(12) - 1: aset = 0: godo(who) = 0
 END IF
 o = 0
 FOR i = 4 TO 11
  IF stat(i, 0, 0) = 0 THEN o = o + 1
 NEXT i
 IF o < 8 THEN
  j = who
  IF buffer(4) <> atk(4) OR buffer(3) <> atk(3) THEN
   IF buffer(4) = 0 OR buffer(4) >= 2 OR buffer(4) <= 4 THEN GOSUB eaifocus
   IF buffer(4) = 1 OR (buffer(4) = 2 AND INT(RND * 100) < 33) THEN GOSUB eaispread
  END IF
 END IF
END IF
RETURN

afterdone:
FOR i = 4 TO 11
 IF stat(i, 0, 0) <= 0 THEN
  IF readbit(ebits(), (i - 4) * 5, 56) = 1 THEN
   FOR o = 4 TO 11
    IF readbit(ebits(), (o - 4) * 5, 58) = 1 THEN
     stat(o, 0, 0) = 0: die(o) = w(o) * .5
     IF o >= 4 THEN
      IF readbit(ebits(), (o - 4) * 5, 59) = 1 THEN eflee(o) = 1: die(o) = (w(o) + x(o)) / 10
     END IF
    END IF
   NEXT o
  END IF
 END IF
NEXT i
oo = 0
FOR i = 4 TO 11
 GOSUB ifdead
NEXT i
IF oo = 8 THEN dead = 1
oo = 0
FOR i = 0 TO 3
 GOSUB ifdead
NEXT i
IF oo = 4 THEN dead = 2
bos = 0: dbos = 0
FOR i = 4 TO 11
 IF readbit(ebits(), (i - 4) * 5, 56) = 1 THEN
  bos = bos + 1
  IF stat(i, 0, 0) <= 0 THEN dbos = dbos + 1
 END IF
NEXT i
 IF bos > 0 AND dbos = bos THEN dead = 1
RETURN

ifdead:
IF i >= 4 THEN
 IF stat(i, 0, 0) > 0 AND readbit(ebits(), (i - 4) * 5, 61) = 1 THEN oo = oo + 1
END IF
IF stat(i, 0, 0) = 0 THEN
 oo = oo + 1
 v(i) = 0
 ready(i) = 0
 godo(i) = 0
 IF you = i THEN you = -1
 IF them = i THEN them = -1
 GOSUB spawnally
 IF i >= 4 THEN '------PLUNDER AND EXPERIENCE AND ITEMS------
  IF a((i - 4) * 4) > 0 THEN
   plunder& = plunder& + es(i - 4, 56)
   IF plunder& > 1000000000 THEN plunder& = 1000000000
   exper& = exper& + es(i - 4, 57)
   IF exper& > 1000000 THEN exper& = 1000000
   IF INT(RND * 100) < es(i - 4, 59) THEN '---GET ITEMS FROM FOES-----
    FOR j = 0 TO 16
     IF found(j, 1) = 0 THEN found(j, 0) = es(i - 4, 58): found(j, 1) = 1: EXIT FOR
     IF found(j, 0) = es(i - 4, 58) THEN found(j, 1) = found(j, 1) + 1: EXIT FOR
    NEXT j
   ELSE '------END NORMAL ITEM---------------
    IF INT(RND * 100) < es(i - 4, 59) THEN
     FOR j = 0 TO 16
      IF found(j, 1) = 0 THEN found(j, 0) = es(i - 4, 58): found(j, 1) = 1: EXIT FOR
      IF found(j, 0) = es(i - 4, 58) THEN found(j, 1) = found(j, 1) + 1: EXIT FOR
     NEXT j
    END IF '---END RARE ITEM-------------
   END IF '----END GET ITEMS----------------
  END IF
  a((i - 4) * 4) = 0
 END IF'------------END PLUNDER-------------------
 IF noifdead = 0 THEN '---THIS IS NOT DONE FOR ALLY+DEAD------
  tcount = tcount - 1
  FOR j = 0 TO 11
   t(j, 12) = -1
   FOR k = 0 TO 11
    IF t(j, k) = i AND readbit(ltarg(), j, i) = 0 THEN SWAP t(j, k), t(j, k + 1)
   NEXT k
   IF t(j, 0) = -1 AND who <> j AND godo(j) > 0 THEN
    'godo(j) = 0: ready(j) = 1: delay(j) = 0
    setpicstuf buffer(), 80, -1
    loadset game$ + ".dt6" + CHR$(0), godo(j) - 1, 0
    IF buffer(4) = 1 OR (buffer(4) = 2 AND INT(RND * 100) < 33) THEN GOSUB eaispread ELSE GOSUB eaifocus
   END IF
   IF tmask(i) = 1 THEN tmask(i) = 0
   IF targ(i) = 1 THEN targ(i) = 0
  NEXT j
  IF tptr = i THEN
   WHILE tmask(tptr) = 0
    tptr = tptr + 1: IF tptr > 11 THEN ptarg = 0: RETURN
   WEND
  END IF
 END IF  '----END ONLY WHEN NOIFDEAD = 0
END IF  '----END (i) IS DEAD
RETURN

sponhit:
FOR i = 0 TO 8
 IF es(script(s + 1) - 4, 82 + i) > 0 AND atktype(i) = 1 THEN
  FOR j = 1 TO es(script(s + 1) - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF a(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    a(slot * 4) = es(script(s + 1) - 4, 82 + i)
    loadfoe slot, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), name$(), stat(), ebits()
   END IF
  NEXT j
  EXIT FOR
 END IF
NEXT i
RETURN

spawnally:
IF i >= 4 THEN
 IF es(i - 4, 80) > 0 AND atktype(0) = 1 THEN
  die(i) = 1
  FOR j = 1 TO es(i - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF a(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    a(slot * 4) = es(i - 4, 80)
    oo = oo - 1
    loadfoe slot, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), name$(), stat(), ebits()
   END IF
  NEXT j
  es(i - 4, 80) = 0
 END IF
 IF es(i - 4, 79) > 0 THEN
  die(i) = 1
  FOR j = 1 TO es(i - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF a(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    a(slot * 4) = es(i - 4, 79)
    oo = oo - 1
    loadfoe slot, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), name$(), stat(), ebits()
   END IF
  NEXT j
  es(i - 4, 79) = 0
 END IF
END IF
RETURN

itemmenu:
IF carray(5) > 1 THEN mset = 0: setkeys: FOR i = 0 TO 7: carray(i) = 0: icons(you) = -1: NEXT i
IF carray(0) > 1 AND iptr > 2 THEN iptr = iptr - 3: IF iptr < itop THEN itop = itop - 3
IF carray(1) > 1 AND iptr < 195 THEN iptr = iptr + 3: IF iptr > itop + 26 THEN itop = itop + 3
IF keyval(73) > 1 THEN
 iptr = iptr - 27: itop = itop - 27
 IF itop < 0 THEN itop = 0
 WHILE iptr < 0: iptr = iptr + 3: WEND
END IF
IF keyval(81) > 1 THEN
 iptr = iptr + 27: itop = itop + 27
 IF itop > 171 THEN itop = 171
 WHILE iptr > 197: iptr = iptr - 3: WEND
END IF
IF carray(2) > 1 THEN
 IF ((iptr / 3) - INT(iptr / 3)) * 3 > 0 THEN iptr = iptr - 1 ELSE iptr = iptr + 2
END IF
IF carray(3) > 1 THEN
 IF ((iptr / 3) - INT(iptr / 3)) * 3 < 2 THEN iptr = iptr + 1 ELSE iptr = iptr - 2
END IF
IF carray(4) > 1 THEN
 IF readbit(iuse(), 0, iptr) = 1 THEN
  lb = (item(iptr) AND 255)
  setpicstuf buffer(), 200, -1
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
  icons(you) = -1: IF buffer(73) = 1 THEN icons(you) = iptr
  temp = buffer(47)
  setpicstuf buffer(), 80, -1
  loadset game$ + ".dt6" + CHR$(0), temp - 1, 0
  godo(you) = temp
  delay(you) = large(buffer(16), 1)
  ptarg = 1: mset = 0: setkeys
  FOR i = 0 TO 7: carray(i) = 0: NEXT i
 END IF
END IF
RETURN

spellmenu:
IF carray(5) > 1 THEN mset = 0: setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i
IF carray(0) > 1 THEN
 IF sptr > 2 THEN sptr = sptr - 3 ELSE sptr = 24
END IF
IF carray(1) > 1 THEN
 IF sptr < 24 THEN sptr = small(sptr + 3, 24) ELSE sptr = 0
END IF
IF carray(2) > 1 AND sptr < 24 THEN
 IF ((sptr / 3) - INT(sptr / 3)) * 3 > 0 THEN sptr = sptr - 1 ELSE sptr = sptr + 2
END IF
IF carray(3) > 1 AND sptr < 24 THEN
 IF ((sptr / 3) - INT(sptr / 3)) * 3 < 2 THEN sptr = sptr + 1 ELSE sptr = sptr - 2
END IF
IF carray(4) > 1 THEN
 IF sptr = 24 THEN mset = 0: setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: RETURN
 temp = 0
 IF spel(sptr) > -1 THEN
  setpicstuf buffer(), 80, -1
  loadset game$ + ".dt6" + CHR$(0), spel(sptr), 0
  IF st(you, 288 + sptype) = 0 AND stat(you, 0, 1) - buffer(8) >= 0 THEN temp = 1
  IF st(you, 288 + sptype) = 1 AND lmp(you, INT(sptr / 3)) - 1 >= 0 THEN temp = 1: conlmp(you) = INT(sptr / 3) + 1
 END IF
 IF temp = 1 AND spel(sptr) > -1 THEN
  godo(you) = spel(sptr) + 1
  delay(you) = large(buffer(16), 1)
  ptarg = 1: mset = 0: setkeys
  FOR i = 0 TO 7: carray(i) = 0: NEXT i
  RETURN
 END IF
END IF
RETURN

picktarg:
IF carray(5) > 1 THEN godo(you) = 0: ptarg = 0: setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: RETURN
IF ptarg = 1 THEN GOSUB setuptarg
IF ran = 1 THEN
 FOR i = 0 TO INT(RND * 5)
  tptr = loopvar(tptr, 0, 11, 1)
  WHILE tmask(tptr) = 0
   tptr = loopvar(tptr, 0, 11, 1)
  WEND
 NEXT i
END IF
IF ran = 2 THEN
 IF firstt(you) = 0 THEN
  WHILE tmask(tptr) = 0
   tptr = loopvar(tptr, 0, 11, 1)
  WEND
 ELSE
  tptr = firstt(you) - 1
 END IF
 GOSUB gottarg
 RETURN
END IF
IF spred = 2 AND (carray(2) > 1 OR carray(3) > 1) THEN FOR i = 0 TO 11: targ(i) = 0: NEXT i: spred = 1: setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i
IF aim = 1 AND spred < 2 THEN
 IF carray(0) > 1 THEN
  temp = 0: o = tptr
  FOR i = 0 TO 11
   IF y(i) < y(tptr) AND y(i) > temp AND tmask(i) = 1 THEN temp = y(i): o = i
  NEXT i
  tptr = o
 END IF
 IF carray(1) > 1 THEN
  temp = 200: o = tptr
  FOR i = 0 TO 11
   IF y(i) > y(tptr) AND y(i) < temp AND tmask(i) = 1 THEN temp = y(i): o = i
  NEXT i
  tptr = o
 END IF
 IF carray(2) > 1 THEN
  temp = 0: o = tptr
  FOR i = 0 TO 11
   IF x(i) < x(tptr) AND x(i) > temp AND tmask(i) = 1 THEN temp = x(i): o = i
  NEXT i
  IF o = tptr AND spred = 1 THEN FOR i = 0 TO 11: targ(i) = tmask(i): NEXT i: spred = 2
  tptr = o
 END IF
 IF carray(3) > 1 THEN
  temp = 320: o = tptr
  FOR i = 0 TO 11
   IF x(i) > x(tptr) AND x(i) < temp AND tmask(i) = 1 THEN temp = x(i): o = i
  NEXT i
  IF o = tptr AND spred = 1 THEN FOR i = 0 TO 11: targ(i) = tmask(i): NEXT i: spred = 2
  tptr = o
 END IF
END IF
IF carray(4) > 1 THEN GOSUB gottarg
RETURN

gottarg:
 targ(tptr) = 1
 o = 0
 FOR i = 0 TO 11
  IF targ(i) = 1 THEN
   t(you, o) = i: o = o + 1
   IF noifdead THEN setbit ltarg(), you, i, 1
  END IF
 NEXT i
 ctr(you) = 0
 ready(you) = 0
 firstt(you) = tptr + 1
 you = -1
 ptarg = 0
 noifdead = 0
RETURN

setuptarg:
spred = 0: aim = 0: ran = 0: firstt(you) = 0
FOR i = 0 TO 11
 targ(i) = 0
 tmask(i) = 0
 t(you, i) = -1
NEXT i
setpicstuf buffer(), 80, -1
loadset game$ + ".dt6" + CHR$(0), godo(you) - 1, 0
tptr = 0
noifdead = 0
ltarg(you) = 0
IF buffer(3) = 0 THEN FOR i = 4 TO 11: tmask(i) = v(i): NEXT i
IF buffer(3) = 1 THEN FOR i = 0 TO 3: tmask(i) = v(i): NEXT i
IF buffer(3) = 2 THEN tmask(you) = 1
IF buffer(3) = 3 THEN FOR i = 0 TO 11: tmask(i) = v(i): NEXT i: tptr = 4
IF buffer(3) = 4 THEN
 noifdead = 1
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN tmask(i) = 1
 NEXT i
END IF
IF buffer(3) = 5 THEN FOR i = 0 TO 3: tmask(i) = v(i): NEXT i: tmask(you) = 0
FOR i = 4 TO 11
 IF readbit(ebits(), (i - 4) * 5, 61) = 1 THEN tmask(i) = 0
NEXT i
WHILE tmask(tptr) = 0
 tptr = tptr + 1: IF tptr > 11 THEN ptarg = 0: RETURN
WEND
IF buffer(4) = 0 THEN aim = 1
IF buffer(4) = 1 THEN FOR i = 0 TO 11: targ(i) = tmask(i): NEXT i
IF buffer(4) = 2 THEN aim = 1: spred = 1
IF buffer(4) = 3 THEN ran = 1
IF buffer(4) = 4 THEN ran = 2
ptarg = 2
RETURN

iconsume:
lb = (item(icons(who)) AND 255)
hb = INT(item(icons(who)) / 256)
hb = hb - 1
item(icons(who)) = lb + (hb * 256)
item$(icons(who)) = LEFT$(item$(icons(who)), 9) + RIGHT$(STR$(hb), 2)
IF hb = 0 THEN item(icons(who)) = 0: item$(icons(who)) = "           ": setbit iuse(), 0, icons(who), 0
icons(who) = -1
RETURN

display:
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  col = 7: IF i = you THEN col = 14 + tog
  edgeprint name$(i), 128 - LEN(name$(i)) * 8, 5 + i * 10, col, dpage
  edgeprint RIGHT$(STR$(stat(i, 0, 0)), LEN(STR$(stat(i, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(i, 1, 0)), LEN(STR$(stat(i, 1, 0))) - 1), 136, 5 + i * 10, col, dpage
 END IF
NEXT i
IF you >= 0 THEN
 centerbox 264, 5 + (4 * (mend(you) + 2)), 88, 8 * (mend(you) + 2), 1, dpage
 FOR i = 0 TO mend(you)
  textcolor 7, 0
  IF ptr = i THEN textcolor 14 + tog, 1
  printstr menu$(you, i), 224, 9 + i * 8, dpage
 NEXT i
 IF mset = 1 THEN
  centerbox 148, 45, 280, 80, 1, dpage
  rectangle 7, 74, 282, 1, 25, dpage
  FOR i = 0 TO 23
   textcolor 7, 0
   IF sptr = i THEN textcolor 14 + tog, 1
   printstr spel$(i), 16 + (((i / 3) - INT(i / 3)) * 3) * 88, 8 + INT(i / 3) * 8, dpage
  NEXT i
  textcolor 7, 0
  IF sptr = 24 THEN textcolor 14 + tog, 1
  printstr " (CANCEL) ", 16, 76, dpage
  textcolor 10, 0
  printstr cost$(sptr), 280 - LEN(cost$(sptr)) * 8, 76, dpage
 END IF
 IF mset = 2 THEN
  centerbox 160, 45, 304, 80, 1, dpage
  FOR i = itop TO itop + 26
   textcolor 8 - readbit(iuse(), 0, i), 0
   IF iptr = i THEN textcolor 14 + tog, 1 + (readbit(iuse(), 0, i) * tog)
   printstr item$(i), 20 + 96 * (((i / 3) - INT(i / 3)) * 3), 8 + 8 * INT((i - itop) / 3), dpage
  NEXT i
 END IF
 IF ptarg > 0 THEN
  FOR i = 0 TO 11
   IF targ(i) = 1 OR tptr = i THEN
    edgeprint CHR$(24), x(i) + (w(i) / 2) - 4, y(i) - 6, 160 + flash, dpage
    edgeprint name$(i), xstring(name$(i), x(i) + (w(i) / 2)), y(i) - 16, 14 + tog, dpage
   END IF
  NEXT i
 END IF
END IF
IF you >= 0 AND ptarg = 0 THEN edgeprint CHR$(24), x(you) + (w(you) / 2) - 4, y(you) - 5 + (tog * 2), 14 + tog, dpage
RETURN

meters:
IF away = 1 THEN RETURN
IF mset > 0 AND readbit(gen(), 101, 0) THEN RETURN
FOR i = 0 TO 11
 IF godo(i) = 0 AND die(i) = 0 AND ready(i) = 0 THEN
  ctr(i) = small(1000, ctr(i) + stat(i, 0, 8))
  IF ctr(i) = 1000 AND wf = 0 THEN ready(i) = 1
 END IF
NEXT i
RETURN

animate:
FOR i = 0 TO 3
 IF walk(i) = 1 THEN of(i) = of(i) XOR tog
 IF who <> i AND stat(i, 0, 0) < stat(i, 1, 0) / 5 AND vdance = 0 THEN of(i) = 6
 IF vdance > 0 AND stat(i, 0, 0) > 0 AND tog = 0 THEN
  IF of(i) = 0 THEN of(i) = 2 ELSE of(i) = 0
 END IF
NEXT i
FOR i = 0 TO 23
 IF xm(i) <> 0 THEN x(i) = x(i) + (mvx(i) * SGN(xm(i))): xm(i) = xm(i) - SGN(xm(i))
 IF ym(i) <> 0 THEN y(i) = y(i) + (mvy(i) * SGN(ym(i))): ym(i) = ym(i) - SGN(ym(i))
 IF zm(i) <> 0 THEN z(i) = z(i) + (mvz(i) * SGN(zm(i))): zm(i) = zm(i) - SGN(zm(i))
NEXT i
FOR i = 0 TO 11
 IF v(i + 12) = 1 THEN
  fctr(i) = fctr(i) + 1: IF aframe(i, fctr(i)) = -1 THEN fctr(i) = 0
  of(i + 12) = aframe(i, fctr(i))
  IF atk(2) = 3 THEN of(i + 12) = INT(RND * 3)
 END IF
 IF die(i) > 0 THEN
  'ENEMIES DEATH THROES
  IF i >= 4 THEN
   IF eflee(i) = 0 THEN
    'rectangle 2 * die(i) * (h(i) * .5), 64 + 10 * (i - 4), h(i) * .5, 1, 0, 3
    FOR ii = 0 TO w(i) * .5
     putpixel INT(RND * (h(i) * w(i) * .5)), 64 + 10 * (i - 4), 0, 3
    NEXT ii
   ELSE
    x(i) = x(i) - 10: d(i) = 1
   END IF
   die(i) = die(i) - 1
  END IF
  IF i < 4 THEN of(i) = 7
 END IF
NEXT i
RETURN

sprite:
FOR i = 0 TO 24
 zbuf(i) = i
NEXT i
FOR i = 0 TO 23
 temp = 200
 FOR o = 24 TO i STEP -1
  IF y(zbuf(o)) + h(zbuf(o)) < temp THEN temp = y(zbuf(o)) + h(zbuf(o)): j = o
 NEXT o
 SWAP zbuf(j), zbuf(i)
NEXT i
FOR i = 0 TO 24
 IF (v(zbuf(i)) = 1 OR die(zbuf(i)) > 0) THEN
  temp = 64 + (zbuf(i) - 4) * 10
  IF zbuf(i) < 4 THEN temp = zbuf(i) * 16
  IF zbuf(i) > 11 THEN temp = 144
  IF zbuf(i) > 23 THEN temp = 156
  loadsprite buffer(), 0, of(zbuf(i)) * (w(zbuf(i)) * h(zbuf(i)) * .5), temp, w(zbuf(i)), h(zbuf(i)), 3
  IF d(zbuf(i)) = 0 THEN drawsprite buffer(), 0, pal(), p(zbuf(i)) * 16, x(zbuf(i)), y(zbuf(i)) - z(zbuf(i)), dpage ELSE wardsprite buffer(), 0, pal(), p(zbuf(i)) * 16, x(zbuf(i)), y(zbuf(i)) - z(zbuf(i)), dpage
 END IF
NEXT i
FOR i = 0 TO 11
 IF hc(i) > 0 THEN edgeprint harm$(i), hx(i) - LEN(harm$(i)) * 4, hy(i), 15, dpage: hc(i) = hc(i) - 1: hy(i) = hy(i) - 2
NEXT i
RETURN

seestuff:
FOR i = 0 TO 11
 c = 12: IF i < 4 THEN c = 10
 rectangle 0, 80 + (i * 10), ctr(i) / 10, 4, c, dpage
 IF i >= 4 THEN edgeprint STR$(es(i - 4, 82)), 0, 80 + i * 10, c, dpage
 edgeprint STR$(v(i)) + ":v" + STR$(delay(i)) + ":dly" + STR$(tmask(i)) + ":tm", 20, 80 + i * 10, c, dpage
NEXT i
RETURN

tryrun:
IF flee > 0 AND flee < 4 THEN
 IF carray(6) = 0 THEN flee = 0: FOR i = 0 TO 3: d(i) = 0: walk(i) = 0: NEXT i
END IF
IF flee = 4 THEN
 FOR i = 4 TO 11
  IF stat(i, 0, 0) > 0 AND v(i) = 1 AND readbit(ebits(), (i - 4) * 5, 57) = 1 THEN flee = 0: alert$ = "CANNOT RUN!": alert = 10
 NEXT i
END IF
IF flee > 4 THEN
 FOR i = 0 TO 3
  IF v(i) THEN d(i) = 1
  walk(i) = 1
  godo(i) = 0
  ctr(i) = large(0, ctr(i) - stat(i, 0, 8) * 2)
 NEXT i
 IF carray(6) = 0 THEN flee = 0: FOR i = 0 TO 3: d(i) = 0: walk(i) = 0: NEXT i
 temp = 400
 FOR i = 4 TO 11
  temp = temp + stat(i, 0, 8)
 NEXT i
 IF RND * temp < flee THEN away = 1: flee = 2: FOR i = 0 TO 3: ctr(i) = 0: ready(i) = 0: NEXT i
END IF
RETURN

loadall:
setpicstuf a(), 80, -1
loadset game$ + ".for" + CHR$(0), form, 0
IF a(33) > 0 THEN loadsong game$ + "." + RIGHT$(STR$(a(33) - 1), LEN(STR$(a(33) - 1)) - 1) + CHR$(0)
setpicstuf buffer(), 636, -1
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
  FOR o = 0 TO 317
   st(i, o) = buffer(o)
  NEXT o
  x(i) = (240 + i * 8)
  y(i) = (82 + i * 20)
  w(i) = 32
  h(i) = 40
  p(i) = st(i, 18)
  v(i) = 1
 END IF
NEXT i
FOR i = 0 TO 3
 setpicstuf buffer(), 5120, 3
 IF hero(i) > 0 THEN
  loadset game$ + ".pt0" + CHR$(0), st(i, 17), i * 16
  FOR o = 0 TO 11
   stat(i, 0, o) = exstat(i, 0, o)
   stat(i, 1, o) = exstat(i, 1, o)
  NEXT o
  FOR o = 0 TO 4
   bits(i, o) = st(i, 240 + o)
  NEXT o
  name$(i) = ""
  FOR o = 1 TO st(i, 0)
   name$(i) = name$(i) + CHR$(st(i, o))
  NEXT o
  FOR o = 0 TO 5
   menu$(i, o) = ""
   IF bmenu(i, o) > 0 THEN
    setpicstuf atk(), 80, -1
    loadset game$ + ".dt6" + CHR$(0), bmenu(i, o) - 1, 0
    FOR j = 26 TO 25 + atk(24)
     menu$(i, o) = menu$(i, o) + CHR$(atk(j))
    NEXT j
   END IF
   IF bmenu(i, o) < 0 AND bmenu(i, o) > -5 THEN
    temp = (bmenu(i, o) + 1) * -1
    FOR j = 244 + temp * 11 TO 243 + temp * 11 + st(i, 243 + temp * 11)
     menu$(i, o) = menu$(i, o) + CHR$(st(i, j))
    NEXT j
   END IF
   IF bmenu(i, o) = -10 THEN menu$(i, o) = "Item": mend(i) = o
   WHILE LEN(menu$(i, o)) < 10: menu$(i, o) = menu$(i, o) + " ": WEND
  NEXT o
 END IF
NEXT i
FOR i = 0 TO 7
 loadfoe i, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), name$(), stat(), ebits()
NEXT i
FOR i = 0 TO 11
 ctr(i) = INT(RND * 500)
 t(i, 12) = -1
NEXT i
FOR i = 12 TO 23
 w(i) = 50
 h(i) = 50
NEXT i
setdiskpages buffer(), 200, 0
loadpage game$ + ".mxs" + CHR$(0), a(32), 2
FOR i = 0 TO 3
 IF hero(i) > 0 AND stat(i, 0, 0) = 0 THEN die(i) = 1
NEXT i
w(24) = 24
h(24) = 24
bos = 0
FOR i = 4 TO 11
 IF readbit(ebits(), (i - 4) * 5, 56) = 1 THEN bos = 1
NEXT i
IF bos = 0 THEN
 FOR i = 4 TO 11
  IF readbit(ebits(), (i - 4) * 5, 58) = 1 THEN
   stat(i, 0, 0) = 0: die(i) = w(i) * 4
   IF i >= 4 THEN
    IF readbit(ebits(), (i - 4) * 5, 59) = 1 THEN eflee(i) = 1: die(i) = (w(i) + x(i)) / 10
   END IF
  END IF
 NEXT i
 oo = 0
 FOR i = 4 TO 11
  GOSUB ifdead
 NEXT i
 IF oo = 8 THEN dead = 1
END IF
RETURN

victory:
IF gen(3) > 0 THEN loadsong game$ + "." + RIGHT$(STR$(gen(3) - 1), LEN(STR$(gen(3) - 1)) - 1) + CHR$(0)
gold& = gold& + plunder&
IF gold& > 1000000000 THEN gold& = 1000000000
o = 0
FOR i = 0 TO 3
 IF hero(i) > 0 AND stat(i, 0, 0) > 0 THEN o = o + 1
NEXT i
IF o > 0 THEN exper& = exper& / o
FOR i = 0 TO 3
 IF hero(i) > 0 AND stat(i, 0, 0) > 0 AND exstat(i, 0, 12) < 99 THEN exlev&(i, 0) = exlev&(i, 0) + exper&
 IF hero(i) > 0 AND exlev&(i, 0) >= exlev&(i, 1) AND exstat(i, 0, 12) < 99 THEN
  exlev&(i, 0) = exlev&(i, 0) - exlev&(i, 1)
  exlev&(i, 1) = 30
  exstat(i, 0, 12) = exstat(i, 0, 12) + 1
  exstat(i, 1, 12) = 1
  FOR o = 1 TO exstat(i, 0, 12)
   exlev&(i, 1) = exlev&(i, 1) * 1.2 + 5
   IF exlev&(i, 1) > 1000000 THEN exlev&(i, 1) = 1000000
  NEXT o
 END IF
NEXT i
FOR i = 0 TO 3
 'THIS PART UPDATES STATS FOR A LEVEL UP
 IF exstat(i, 1, 12) = 1 THEN
  setpicstuf buffer(), 636, -1
  loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
  FOR o = 0 TO 11
   exstat(i, 1, o) = exstat(i, 1, o) + (atlevel(exstat(i, 0, 12), buffer(23 + o * 2), buffer(24 + o * 2)) - atlevel(exstat(i, 0, 12) - 1, buffer(23 + o * 2), buffer(24 + o * 2)))
  NEXT o
  IF NOT readbit(gen(), 101, 2) THEN exstat(i, 0, 0) = exstat(i, 1, 0)
  IF NOT readbit(gen(), 101, 3) THEN exstat(i, 0, 1) = exstat(i, 1, 1)
  FOR o = 2 TO 11
    exstat(i, 0, o) = exstat(i, 1, o)
  NEXT o
  FOR j = 0 TO 3
   FOR o = 0 TO 23
    IF buffer(47 + (j * 48) + (o * 2)) > 0 AND buffer(48 + (j * 48) + (o * 2)) - 1 <= exstat(i, 0, 12) AND buffer(48 + (j * 48) + (o * 2)) > 0 THEN spell(i, j, o) = buffer(47 + (j * 48) + (o * 2))
   NEXT o
  NEXT j
  setpicstuf buffer(), 200, -1
  resetlmp lmp(), i, exstat(i, 0, 12)
  stat(i, 0, 0) = exstat(i, 0, 0)
  stat(i, 0, 1) = exstat(i, 0, 1)
 END IF
NEXT i
vdance = 1
RETURN

vicdance:
centerfuz 160, 30, 280, 50, 1, dpage
IF vdance = 3 THEN
 GOSUB vicfind
END IF
IF vdance = 2 THEN
 IF carray(4) > 1 OR carray(5) > 1 THEN found$ = "": vdance = 3
 o = 0
 FOR i = 0 TO 3
  IF exstat(i, 1, 12) = 1 THEN
   edgeprint "Level up for " + name$(i) + "!", xstring("Level up for " + name$(i) + "!", 160), 14 + i * 10, 15, dpage
   o = 1
  END IF
 NEXT i
 IF o = 0 THEN vdance = 3
END IF
IF vdance = 1 THEN
 IF carray(4) > 1 OR carray(5) > 1 THEN vdance = 2
 temp$ = "Found" + STR$(plunder&) + " " + sname$(32) + "!"
 edgeprint temp$, xstring(temp$, 160), 16, 15, dpage
 edgeprint "Gained" + STR$(exper&) + " Experience!", xstring("Gained" + STR$(exper&) + " Experience!", 160), 28, 15, dpage
END IF
RETURN

vicfind:
 IF carray(4) > 1 OR carray(5) > 1 THEN
  IF found(fptr, 1) = 0 THEN
   vdance = -1
  ELSE
   fptr = fptr + 1: found$ = ""
  END IF
 END IF
 IF found$ = "" THEN
  IF found(fptr, 1) = 0 THEN vdance = -1: RETURN
  setpicstuf buffer(), 200, -1
  loadset game$ + ".itm" + CHR$(0), found(fptr, 0), 0
  FOR o = 1 TO buffer(0)
   found$ = found$ + CHR$(small(large(buffer(o), 0), 255))
  NEXT o
  FOR i = 1 TO found(fptr, 1)
   getitem found(fptr, 0) + 1, item(), item$()
  NEXT i
 END IF
 IF found(fptr, 1) = 1 THEN
  edgeprint "Found a " + found$, xstring("Found a " + found$, 160), 22, 15, dpage
 ELSE
  edgeprint "Found" + STR$(found(fptr, 1)) + " " + found$, xstring("Found" + STR$(found(fptr, 1)) + " " + found$, 160), 22, 15, dpage
 END IF
RETURN

writestats:
setpicstuf buffer(), 636, -1
FOR i = 0 TO 3
 exstat(i, 1, 12) = 0
 IF hero(i) > 0 THEN
  FOR o = 0 TO 1
    exstat(i, 0, o) = stat(i, 0, o)
  NEXT o
 END IF
NEXT i
RETURN

donebattle:
GOSUB writestats
fademusic 0
fadeto buffer(), 0, 0, 0
clearpage 0
clearpage 1
clearpage 2
clearpage 3
END SUB

