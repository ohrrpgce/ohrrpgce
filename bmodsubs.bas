DECLARE SUB calibrate (gotj%(), joy%(), dpage%, vpage%, timing%())
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
SUB advance (s, who, atk(), script(), x(), y(), w(), h(), t())

IF atk(14) < 2 OR (atk(14) > 2 AND atk(14) < 5) THEN
 script(s) = 14: script(s + 1) = who: s = s + 2
 script(s) = 2: script(s + 1) = who: script(s + 2) = -5: script(s + 3) = 0: script(s + 4) = 4: script(s + 5) = 0: s = s + 6
 script(s) = 9: s = s + 1
END IF
IF atk(14) = 2 THEN
 yt = (h(t(who, 0)) - h(who)) + 2
 script(s) = 8: script(s + 1) = who: script(s + 2) = x(t(who, 0)) + w(t(who, 0)): script(s + 3) = y(t(who, 0)) + yt: script(s + 4) = 6: script(s + 5) = 6: s = s + 6
 script(s) = 9: s = s + 1
END IF
IF atk(14) = 8 THEN
 script(s) = 3: script(s + 1) = who: script(s + 2) = x(t(who, 0)) + w(t(who, 0)): script(s + 3) = y(t(who, 0)) + (h(t(who, 0)) - (h(who))): script(s + 4) = 0: s = s + 5
END IF

END SUB

SUB control (carray(), csetup(), gotm, gotj(), mouse(), joy(), herox, heroy, mmode, timing())

'  CARRAY()
'  0=up 1=down 2=left 3=right
'  4=use
'  5=menu
'  6=run
'  7=
'  8=

'  CSETUP()
'  0=up 1=down 2=left 3=right                13=oldmouse X
'  4=useA                                    14=oldmouse Y
'  5=useB                                    15=showmouse
'  6=useC
'  7=menuA                                      MOUSEMODE
'  8=menuB                                      0=map
'  9=runA                                       1=click only
'  10=runB
'  11=calibrate
'  12=

STATIC joyuse, joymenu

FOR i = 0 TO 7: carray(i) = 0: NEXT i

GOSUB keyboard
'GOSUB mouse
GOSUB joystick
EXIT SUB

keyboard:
FOR i = 0 TO 3
 carray(i) = keyval(csetup(i)): carray(15) = carray(15) - 1
NEXT i
carray(4) = large(large(keyval(csetup(4)), keyval(csetup(5))), keyval(csetup(6))): carray(15) = carray(15) - 1
carray(5) = large(keyval(csetup(7)), keyval(csetup(8))): carray(15) = carray(15) - 1
carray(6) = large(keyval(csetup(9)), keyval(csetup(10))): carray(15) = carray(15) - 1
IF keyval(csetup(11)) > 1 THEN
 calibrate gotj(), joy(), 0, 1, timing()
 FOR i = 0 TO 1
  gotj(i) = readjoy(joy(), i)
 NEXT i
END IF
carray(8) = keyval(csetup(12)): carray(15) = carray(15) - 1
RETURN

mouse:
'IF gotm THEN
' carray(13) = mouse(0)
' carray(14) = mouse(1)
' readmouse mouse()
' IF ABS(mouse(0) - carray(13)) > 2 OR ABS(mouse(1) - carray(14)) > 2 THEN carray(15) = 10
'END IF
'
'IF mmode = 0 THEN
' IF mouse(2) = 1 THEN
'  carray(15) = 10
'  IF mouse(1) < heroy AND ABS(mouse(0) - herox) < (mouse(1) - heroy) THEN carray(0) = 2
'  IF mouse(1) > heroy + 20 AND ABS(mouse(0) - herox) < (mouse(1) - heroy) THEN carray(1) = 2
'  IF mouse(0) < herox AND ABS(mouse(1) - heroy) < (mouse(o) - herox) THEN carray(2) = 2
'  IF mouse(0) > herox + 20 AND ABS(mouse(1) - heroy) < (mouse(o) - herox) THEN carray(3) = 2
'  IF mouse(0) > herox AND mouse(0) < herox + 20 AND mouse(1) > heroy AND mouse(1) < heroy + 20 THEN carray(5) = 1
' END IF
' IF mouse(2) = 2 THEN
'  carray(15) = 10
'  carray(4) = 1
' END IF
'END IF
'
'RETURN

joystick:
FOR i = 0 TO 1
 IF gotj(i) THEN gotj(i) = readjoy(joy(), i): EXIT FOR
NEXT i
IF gotj(i) = 0 THEN RETURN

'edgeprint STR$(i) + STR$(gotj(i)) + STR$(joy(0)) + STR$(joy(1)) + STR$(joy(2)) + STR$(joy(3)) + STR$(carray(4)) + STR$(carray(5)), 0, 170, 14, 0
'edgeprint STR$(i) + STR$(gotj(i)) + STR$(joy(0)) + STR$(joy(1)) + STR$(joy(2)) + STR$(joy(3)) + STR$(carray(4)) + STR$(carray(5)), 0, 170, 14, 1

IF joy(1) < joy(9) THEN
 carray(0) = 3
 IF carray(10) = 3 THEN carray(0) = 2
 IF carray(10) = 2 THEN carray(0) = 1
 IF carray(10) = 1 THEN carray(0) = 2
END IF
IF joy(1) > joy(10) THEN
 carray(1) = 3
 IF carray(11) = 3 THEN carray(1) = 1
 IF carray(11) = 2 THEN carray(1) = 1
 IF carray(11) = 1 THEN carray(1) = 2
END IF
IF joy(0) < joy(11) THEN
 carray(2) = 3
 IF carray(12) = 3 THEN carray(2) = 1
 IF carray(12) = 2 THEN carray(2) = 1
 IF carray(12) = 1 THEN carray(2) = 2
END IF
IF joy(0) > joy(12) THEN
 carray(3) = 3
 IF carray(13) = 3 THEN carray(3) = 1
 IF carray(13) = 2 THEN carray(3) = 1
 IF carray(13) = 1 THEN carray(3) = 2
END IF
'--Joystick buttons!
'edgeprint STR$(joyuse) + STR$(joymenu) + STR$(joy(13)) + STR$(joy(14)), 0, 190, 15, 0
'edgeprint STR$(joyuse) + STR$(joymenu) + STR$(joy(13)) + STR$(joy(14)), 0, 190, 15, 1
SELECT CASE joyuse
CASE 0
 IF joy(joy(13)) = 0 THEN joyuse = 1
CASE 1
 IF joy(joy(13)) <> 0 THEN joyuse = 2
CASE 2
 carray(4) = 2
 joyuse = 3
CASE 3
 carray(4) = 1
 joyuse = 0
END SELECT
SELECT CASE joymenu
CASE 0
 IF joy(joy(14)) = 0 THEN joymenu = 1
CASE 1
 carray(6) = 2
 IF joy(joy(14)) <> 0 THEN joymenu = 2
CASE 2
 carray(5) = 2
 joymenu = 3
CASE 3
 carray(5) = 1
 joymenu = 0
END SELECT

FOR i = 0 TO 3
 carray(10 + i) = carray(i)
NEXT i
RETURN

END SUB

SUB eretreat (s, who, script(), atk(), x(), y(), w(), h(), t())

IF atk(14) = 2 OR atk(14) = 5 THEN
 script(s) = 11: script(s + 1) = who: script(s + 2) = 0: s = s + 3
 script(s) = 8: script(s + 1) = who: script(s + 2) = x(who): script(s + 3) = y(who): script(s + 4) = 6: script(s + 5) = 6: s = s + 6
 script(s) = 9: s = s + 1
END IF

END SUB

SUB etwitch (s, who, script(), atk(), x(), y(), w(), h(), t())

IF atk(14) < 2 THEN
 script(s) = 11: script(s + 1) = who: script(s + 2) = 2: s = s + 3
 script(s) = 13: script(s + 1) = 1: s = s + 2
 script(s) = 11: script(s + 1) = who: script(s + 2) = 0: s = s + 3
END IF
IF atk(14) = 2 THEN
 yt = (h(t(who, 0)) - h(who)) + 2
 script(s) = 8: script(s + 1) = who: script(s + 2) = x(t(who, 0)) - w(who): script(s + 3) = y(t(who, 0)) + yt: script(s + 4) = 6: script(s + 5) = 6: s = s + 6
 script(s) = 9: s = s + 1
END IF
IF atk(14) = 3 THEN
 FOR ii = 0 TO 2
  script(s) = 3: script(s + 1) = who: script(s + 2) = x(who): script(s + 3) = y(who): script(s + 4) = 1: s = s + 5
  script(s) = 13: script(s + 1) = 1: s = s + 2
  script(s) = 3: script(s + 1) = who: script(s + 2) = x(who): script(s + 3) = y(who): script(s + 4) = 0: s = s + 5
  script(s) = 13: script(s + 1) = 1: s = s + 2
 NEXT ii
END IF
IF atk(14) = 4 THEN
 script(s) = 8: script(s + 1) = who: script(s + 2) = x(who) + 50: script(s + 3) = y(who): script(s + 4) = 7: script(s + 5) = 7: s = s + 6
 script(s) = 15: script(s + 1) = who: script(s + 2) = 10: script(s + 3) = 20: s = s + 4
 script(s) = 9: s = s + 1
 script(s) = 6: script(s + 1) = who: s = s + 2
END IF
IF atk(14) = 5 THEN
 script(s) = 11: script(s + 1) = who: script(s + 2) = 200: s = s + 3
 script(s) = 5: script(s + 1) = who: s = s + 2
 script(s) = 3: script(s + 1) = who: script(s + 2) = x(t(who, 0)): script(s + 3) = y(t(who, 0)): script(s + 4) = 0: s = s + 5
 script(s) = 15: script(s + 1) = who: script(s + 2) = -10: script(s + 3) = 20: s = s + 4
 script(s) = 9: s = s + 1
END IF

END SUB

SUB heroanim (s, who, script(), atk(), x(), y(), w(), h(), t())

IF atk(14) < 3 OR (atk(14) > 6 AND atk(14) < 9) THEN
 script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
 script(s) = 13: script(s + 1) = 3: s = s + 2
 IF atk(14) <> 1 AND atk(14) <> 7 THEN
  script(s) = 7: script(s + 1) = who: script(s + 2) = 2: s = s + 3
  IF atk(14) <> 2 THEN script(s) = 3: script(s + 1) = 24: script(s + 2) = x(who) + 4: script(s + 3) = y(who): script(s + 4) = 0: s = s + 5
  yt = (h(t(who, 0)) - h(who)) + 2
  IF atk(14) = 2 THEN script(s) = 3: script(s + 1) = 24: script(s + 2) = x(t(who, 0)) + w(t(who, 0)) + 24: script(s + 3) = y(t(who, 0)) + yt: script(s + 4) = 0: s = s + 5
  script(s) = 7: script(s + 1) = 24: script(s + 2) = 0: s = s + 3
  script(s) = 5: script(s + 1) = 24: s = s + 2
 END IF
 IF atk(14) = 1 OR atk(14) = 7 THEN script(s) = 7: script(s + 1) = who: script(s + 2) = 4: s = s + 3
 script(s) = 13: script(s + 1) = 3: s = s + 2
 IF atk(14) <> 1 AND atk(14) <> 7 THEN
  script(s) = 7: script(s + 1) = who: script(s + 2) = 3: s = s + 3
  IF atk(14) <> 2 THEN script(s) = 3: script(s + 1) = 24: script(s + 2) = x(who) - 40: script(s + 3) = y(who): script(s + 4) = 0: s = s + 5
  yt = (h(t(who, 0)) - h(who)) + 2
  IF atk(14) = 2 THEN script(s) = 3: script(s + 1) = 24: script(s + 2) = x(t(who, 0)) + w(t(who, 0)) - 20: script(s + 3) = y(t(who, 0)) + yt: script(s + 4) = 0: s = s + 5
  script(s) = 7: script(s + 1) = 24: script(s + 2) = 1: s = s + 3
 END IF
END IF
IF atk(14) = 3 THEN
 FOR ii = 0 TO 2
  script(s) = 3: script(s + 1) = who: script(s + 2) = x(who): script(s + 3) = y(who): script(s + 4) = 1: s = s + 5
  script(s) = 3: script(s + 1) = 24: script(s + 2) = x(who) + 40: script(s + 3) = y(who): script(s + 4) = 0: s = s + 5
  script(s) = 7: script(s + 1) = 24: script(s + 2) = 0: s = s + 3
  script(s) = 13: script(s + 1) = 1: s = s + 2
  script(s) = 3: script(s + 1) = who: script(s + 2) = x(who): script(s + 3) = y(who): script(s + 4) = 0: s = s + 5
  script(s) = 3: script(s + 1) = 24: script(s + 2) = x(who) - 40: script(s + 3) = y(who): script(s + 4) = 0: s = s + 5
  script(s) = 7: script(s + 1) = 24: script(s + 2) = 1: s = s + 3
  script(s) = 13: script(s + 1) = 1: s = s + 2
 NEXT ii
END IF
IF atk(14) = 4 THEN
 script(s) = 7: script(s + 1) = who: script(s + 2) = 4: s = s + 3
 script(s) = 8: script(s + 1) = who: script(s + 2) = x(who) - 40: script(s + 3) = y(who): script(s + 4) = 7: script(s + 5) = 7: s = s + 6
 script(s) = 15: script(s + 1) = who: script(s + 2) = 10: script(s + 3) = 20: s = s + 4
 script(s) = 9: s = s + 1
 script(s) = 6: script(s + 1) = who: s = s + 2
 script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
END IF
IF atk(14) = 5 THEN
 script(s) = 11: script(s + 1) = who: script(s + 2) = 200: s = s + 3
 script(s) = 7: script(s + 1) = who: script(s + 2) = 2: s = s + 3
 script(s) = 5: script(s + 1) = who: s = s + 2
 script(s) = 3: script(s + 1) = who: script(s + 2) = x(t(who, 0)): script(s + 3) = y(t(who, 0)): script(s + 4) = 0: s = s + 5
 script(s) = 15: script(s + 1) = who: script(s + 2) = -10: script(s + 3) = 20: s = s + 4
 script(s) = 9: s = s + 1
 script(s) = 7: script(s + 1) = who: script(s + 2) = 5: s = s + 3
END IF

END SUB

SUB inflict (w, t, stat(), x(), y(), w(), h(), harm$(), hc(), hx(), hy(), atk(), tcount, die(), bits())
 DIM tbits(4)
 cure = 0
 emp = 0
 FOR i = 0 TO 4
  tbits(i) = bits(t, i)
 NEXT i
 IF atk(5) = 4 THEN EXIT SUB
 hc(t) = 7
 hx(t) = x(t) + (w(t) * .5)
 hy(t) = y(t) + (h(t) * .5)
 a = stat(w, 0, 3): d = stat(t, 0, 5): dm! = .25
 IF atk(6) = 1 THEN dm! = .5
 IF atk(6) = 2 THEN dm! = 1
 IF atk(6) = 3 THEN dm! = 0
 IF atk(6) = 4 THEN a = stat(w, 0, 6): d = stat(t, 0, 7): dm! = 1.25
 IF range(a, 75) < range(d * dm!, 75) THEN harm$(t) = "miss": EXIT SUB
 a = stat(w, 0, 2): d = stat(t, 0, 4)
 IF atk(7) = 1 THEN a = stat(w, 0, 6): d = stat(t, 0, 7)
 IF atk(7) = 2 THEN a = stat(w, 0, 0)
 IF atk(7) = 3 THEN a = (stat(w, 1, 0) - stat(w, 0, 0))
 IF atk(7) = 4 THEN a = INT(RND * 999)
 IF atk(7) = 5 THEN a = 100
 am! = 1: dm! = .5
 IF atk(5) = 1 THEN am! = .8: dm = .1
 IF atk(5) = 2 THEN am! = 1.3: dm = 1
 IF atk(5) = 3 THEN am! = 1: dm = 0
 h = range(a * am!, 20) - range(d * dm!, 20)
 FOR i = 0 TO 7
  IF readbit(atk(), 20, 5 + i) = 1 THEN
   IF readbit(tbits(), 0, 0 + i) = 1 THEN h = h * 2
   IF readbit(tbits(), 0, 8 + i) = 1 THEN h = h * .1
   IF readbit(tbits(), 0, 16 + i) = 1 THEN cure = 1
  END IF
  IF readbit(atk(), 20, 13 + i) = 1 THEN
   IF t >= 4 AND readbit(tbits(), 0, 24 + i) = 1 THEN h = h * 1.8
  END IF
 NEXT i
 h = h + (h / 100) * atk(11)
 IF readbit(atk(), 20, 1) = 1 THEN h = h / (tcount + 1)
 h = large(h, 1)
 harm$(t) = RIGHT$(STR$(h), LEN(STR$(h)) - 1)
 IF readbit(atk(), 20, 0) = 1 OR cure = 1 THEN h = h * -1: harm$(t) = "+" + harm$(t)
 IF readbit(tbits(), 0, 54) THEN h = ABS(h)
 emp = readbit(atk(), 20, 60): IF emp = 1 THEN harm$(t) = "(" + harm$(t) + ")"
 stat(t, 0, 0 + emp) = stat(t, 0, 0 + emp) - h
 stat(t, 0, 0 + emp) = large(stat(t, 0, 0 + emp), 0): stat(t, 0, 0 + emp) = small(stat(t, 0, 0 + emp), stat(t, 1, 0 + emp))
 stat(w, 0, 0 + emp) = large(stat(w, 0, 0 + emp), 0): stat(w, 0, 0 + emp) = small(stat(w, 0, 0 + emp), stat(w, 1, 0 + emp))
END SUB

SUB loadfoe (i, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), name$(), stat(), ebits())
setpicstuf buffer(), 320, -1
IF a(i * 4) > 0 THEN
 loadset game$ + ".dt1" + CHR$(0), a(i * 4) - 1, 0
 FOR o = 0 TO 160
  es(i, o) = buffer(o)
 NEXT o
 FOR o = 0 TO 4
  ebits(i * 5 + o) = buffer(74 + o)
 NEXT o
 x(4 + i) = a(i * 4 + 1)
 y(4 + i) = a(i * 4 + 2)
 p(4 + i) = es(i, 54)
 v(4 + i) = 1
 IF es(i, 55) = 0 THEN
  ext$(i) = ".pt1"
  w(4 + i) = 34
  h(4 + i) = 34
 END IF
 IF es(i, 55) = 1 THEN
  ext$(i) = ".pt2"
  w(4 + i) = 50
  h(4 + i) = 50
 END IF
 IF es(i, 55) = 2 THEN
  ext$(i) = ".pt3"
  w(4 + i) = 80
  h(4 + i) = 80
 END IF
END IF
IF v(4 + i) = 1 THEN
 setpicstuf buffer(), (w(4 + i) * h(4 + i)) * .5, 3
 loadset game$ + ext$(i) + CHR$(0), es(i, 53), 64 + i * 10
 FOR o = 0 TO 11
  stat(4 + i, 0, o) = es(i, 62 + o)
  stat(4 + i, 1, o) = es(i, 62 + o)
 NEXT o
 FOR o = 0 TO 4
  bits(4 + i, o) = es(i, 74 + o)
 NEXT o
 name$(4 + i) = ""
 FOR o = 1 TO es(i, 0)
  name$(4 + i) = name$(4 + i) + CHR$(es(i, o))
 NEXT o
END IF
END SUB

SUB retreat (s, who, script(), atk(), x(), y(), w(), h(), t())

IF atk(14) < 2 THEN
 script(s) = 14: script(s + 1) = who: s = s + 2
 script(s) = 2: script(s + 1) = who: script(s + 2) = 5: script(s + 3) = 0: script(s + 4) = 4: script(s + 5) = 0: s = s + 6
 script(s) = 9: s = s + 1
 script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
END IF
IF atk(14) = 2 OR atk(14) = 5 THEN
 script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
 script(s) = 14: script(s + 1) = who: s = s + 2
 script(s) = 11: script(s + 1) = who: script(s + 2) = 0: s = s + 3
 script(s) = 8: script(s + 1) = who: script(s + 2) = x(who): script(s + 3) = y(who): script(s + 4) = 6: script(s + 5) = 6: s = s + 6
 script(s) = 9: s = s + 1
 script(s) = 7: script(s + 1) = who: script(s + 2) = 0: s = s + 3
END IF

END SUB

