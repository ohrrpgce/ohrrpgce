'OHRRPGCE GAME - Additional mostly battle-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB exitprogram (needfade%)
DECLARE SUB quitcleanup ()
DECLARE FUNCTION focuscost% (cost%, focus%)
DECLARE FUNCTION safesubtract% (number%, minus%)
DECLARE FUNCTION safemultiply% (number%, by!)
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION atkallowed% (atkid%, attacker%, spclass%, lmplev%, stat%())
DECLARE FUNCTION trytheft% (who%, targ%, atk%(), es%())
DECLARE FUNCTION readitemname$ (itemnum%)
DECLARE SUB setbatcap (cap$, captime%, capdelay%)
DECLARE FUNCTION checktheftchance% (item%, itemP%, rareitem%, rareitemP%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE FUNCTION readbadbinstring$ (array%(), offset%, maxlen%, skipword%)
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE FUNCTION getbinsize% (id%)
DECLARE SUB readattackdata (array%(), index%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE SUB smartarrowmask (inrange%(), pt%, d%, axis%(), tmask%())
DECLARE SUB debug (s$)
DECLARE FUNCTION targetmaskcount% (tmask%())
DECLARE FUNCTION randomally% (who%)
DECLARE FUNCTION randomfoe% (who%)
DECLARE FUNCTION targetable% (attacker%, target%, ebits%())
DECLARE FUNCTION visibleandalive% (o%, stat%(), v%())
DECLARE FUNCTION liveherocount% (stat%())
DECLARE FUNCTION countai% (ai%, them%, es%())
DECLARE FUNCTION enemycount% (v%(), stat%())
DECLARE SUB calibrate ()
DECLARE SUB control ()
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB getitem (getit%)
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%())
DECLARE SUB spells (pt%, stat%())
DECLARE SUB status (pt%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
DECLARE SUB resetlmp (slot%, lev%)
DECLARE SUB loadfoe (i%, formdata%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), stat%(), ebits%(), batname$())
DECLARE FUNCTION inflict (w%, t%, stat%(), x%(), y%(), wid%(), hei%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%(), revenge%(), revengemask%(), targmem%(), revengeharm%(), repeatharm%())
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
DECLARE FUNCTION bound% (n%, lowest%, highest%)

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'gglobals.bi'
'$INCLUDE: 'const.bi'

REM $STATIC
SUB advance (who, atk(), x(), y(), w(), h(), t())

IF atk(14) < 2 OR (atk(14) > 2 AND atk(14) < 5) THEN
 pushw 14: pushw who
 pushw 2: pushw who: pushw -5: pushw 0: pushw 4: pushw 0
 pushw 9
END IF
IF atk(14) = 2 THEN
 yt = (h(t(who, 0)) - h(who)) + 2
 pushw 8: pushw who: pushw x(t(who, 0)) + w(t(who, 0)): pushw y(t(who, 0)) + yt: pushw 6: pushw 6
 pushw 9
END IF
IF atk(14) = 8 THEN
 pushw 3: pushw who: pushw x(t(who, 0)) + w(t(who, 0)): pushw y(t(who, 0)) + (h(t(who, 0)) - (h(who))): pushw 0
END IF

END SUB

FUNCTION atkallowed (atkid, attacker, spclass, lmplev, stat())
'--atkid    = attack ID number
'--attacker = hero or enemy who is attacking
'--spclass  = 0 for normal attacks, 1 for level-MP spells
'--lmplev   = which level-MP level to use

IF atkid < 0 THEN
 '--fail if not a valid attack id
 atkallowed = 0
 EXIT FUNCTION
END IF

'--load attack data
setpicstuf buffer(), 80, -1
loadset game$ + ".dt6" + CHR$(0), atkid, 0

'--check for sufficient mp
IF stat(attacker, 0, 1) - focuscost(buffer(8), stat(attacker, 0, 10)) < 0 THEN
 atkallowed = 0
 EXIT FUNCTION
END IF

'--check for level-MP
IF spclass = 1 THEN
 IF lmp(attacker, lmplev) - 1 < 0 THEN
  atkallowed = 0
  EXIT FUNCTION
 END IF
END IF

'--succeed
atkallowed = -1

END FUNCTION

FUNCTION checktheftchance (item, itemP, rareitem, rareitemP)
IF RND * 100 < itemP THEN
 '--success!
 getitem item + 1
 checktheftchance = item + 1
ELSE
 IF RND * 100 < rareitemP THEN
  '--rare success!
  getitem rareitem + 1
  checktheftchance = rareitem + 1
 ELSE
  checktheftchance = 0
 END IF
END IF
END FUNCTION

SUB control

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

'Quick abort
IF keyval(73) > 0 AND keyval(81) > 0 THEN
 IF keyval(1) > 1 THEN exitprogram 0
END IF

FOR i = 0 TO 7: carray(i) = 0: NEXT i

IF keyval(88) > 1 THEN snapshot

IF keyval(69) = 0 THEN ' no controls while PAUSE is pressed, because of its scancode wierdness
 GOSUB keyboard
 GOSUB joystick
END IF
EXIT SUB

keyboard:
FOR i = 0 TO 3
 carray(i) = keyval(csetup(i)): carray(15) = carray(15) - 1
NEXT i
carray(4) = large(large(keyval(csetup(4)), keyval(csetup(5))), keyval(csetup(6))): carray(15) = carray(15) - 1
carray(5) = large(keyval(csetup(7)), keyval(csetup(8))): carray(15) = carray(15) - 1
carray(6) = large(keyval(csetup(9)), keyval(csetup(10))): carray(15) = carray(15) - 1
'--gen(60) is the calibration disabler flag
IF gen(60) = 0 AND keyval(29) > 0 AND keyval(csetup(11)) > 1 THEN
 calibrate
 FOR i = 0 TO 1
  gotj(i) = readjoy(joy(), i)
 NEXT i
END IF
carray(8) = keyval(csetup(12)): carray(15) = carray(15) - 1
RETURN

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
  'IF joy(joy(13)) = 0 THEN joyuse = 1
  IF joy(joy(13)) = 0 THEN joyuse = 2
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

FUNCTION countai (ai, them, es())
o = 0
FOR i = 0 TO 4
 IF es(them - 4, 92 + (ai * 5) + i) > 0 THEN o = o + 1
NEXT i
countai = o
END FUNCTION

SUB eaifocus (j, atkdat(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem())

'flush the targeting space for this attacker
FOR ii = 0 TO 11
 t(j, ii) = -1
NEXT ii

'cancel if no heros are alive
IF liveherocount(stat()) = 0 THEN EXIT SUB

'fail if no targetable enemies are present
'ii = 0
'FOR o = 4 TO 11
' IF j < 4 THEN
'  IF stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN ii = 1
' END IF
' IF j >= 4 THEN
'  IF stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN ii = 1
' END IF
'NEXT o
'IF ii = 0 THEN exit sub

targetptr = 0

'target random foe
IF atkdat(3) = 0 THEN
 ol = 0
 DO
  'random targ in opposite class
  o = randomfoe(j)
  t(j, targetptr) = o
  ol = ol + 1
  'if its alive and visable and targetable, target it!
  IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN EXIT DO
 LOOP UNTIL ol > 99 'safety cutoff
END IF

'target ally, ally-including dead, ally-not-self
IF atkdat(3) = 1 OR atkdat(3) = 4 OR atkdat(3) = 5 THEN
 ol = 0
 DO
  'pick random targ from own class
  o = randomally(j)
  'if ally-not-self
  IF atkdat(3) = 5 THEN
   WHILE j = o
    o = randomally(j)
   WEND
  END IF
  t(j, targetptr) = o
  ol = ol + 1
  'if alive and targetable, target it!
  IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN EXIT DO
 LOOP UNTIL ol > 99 'safety cutoff
END IF

'self
IF atkdat(3) = 2 AND targetable(j, j, ebits()) THEN t(j, targetptr) = j

'all
IF atkdat(3) = 3 THEN
 ol = 0
 DO
  'random anybody
  o = INT(RND * 12)
  t(j, targetptr) = o
  ol = ol + 1
  IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN EXIT DO
 LOOP UNTIL ol > 99 'safety cutoff
END IF

'revenge one
IF atkdat(3) = 6 AND revenge(j) >= 0 THEN
 IF visibleandalive(revenge(j), stat(), v()) AND targetable(j, revenge(j), ebits()) THEN
  t(j, targetptr) = revenge(j)
 END IF
END IF

'revenge all
IF atkdat(3) = 7 AND revengemask(j) THEN
 ol = 0
 DO
  'random anybody
  o = INT(RND * 12)
  IF readbit(revengemask(), j, o) THEN
   t(j, targetptr) = o
   IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN EXIT DO
  END IF
  ol = ol + 1
 LOOP UNTIL ol > 99 'safety cutoff
END IF

'previous targs
IF atkdat(3) = 8 AND targmem(j) THEN
 ol = 0
 DO
  'random anybody
  o = INT(RND * 12)
  IF readbit(targmem(), j, o) THEN
   t(j, targetptr) = o
   IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN EXIT DO
  END IF
  ol = ol + 1
 LOOP UNTIL ol > 99 'safety cutoff
END IF

'stored targs
IF atkdat(3) = 9 AND targmem(j + 12) THEN
 ol = 0
 DO
  'random targ in opposite class
  o = randomfoe(j)
  IF readbit(targmem(), j + 12, o) THEN
   t(j, targetptr) = o
   IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN EXIT DO
  END IF
  ol = ol + 1
 LOOP UNTIL ol > 99 'safety cutoff
END IF

END SUB

SUB eaispread (j, atkdat(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem())

'clear attacker's temporary target area
FOR ii = 0 TO 11
 t(j, ii) = -1
NEXT ii

'fail if no heros are present
IF liveherocount(stat()) = 0 THEN EXIT SUB

'fail if no targetable foes are present
'ii = 0
'FOR o = 4 TO 11
' IF j < 4 THEN
'  IF stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 61) = 0 THEN ii = 1
' END IF
' IF j >= 4 THEN
'  IF stat(o, 0, 0) > 0 AND readbit(ebits(), (o - 4) * 5, 60) = 0 THEN ii = 1
' END IF
'NEXT o
'IF ii = 0 THEN exit sub

targetptr = 0

'enemy targets hero
IF atkdat(3) = 0 AND j >= 4 THEN
 FOR o = 0 TO 3
  IF visibleandalive(o, stat(), v()) THEN
   t(j, targetptr) = o
   targetptr = targetptr + 1
  END IF
 NEXT o
END IF

'hero targets enemy
IF atkdat(3) = 0 AND j < 4 THEN
 FOR o = 4 TO 11
  IF j < 4 THEN
   IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN
    t(j, targetptr) = o
    targetptr = targetptr + 1
   END IF
  END IF
 NEXT o
END IF

'enemy targets enemy
IF (atkdat(3) = 1 OR atkdat(3) = 4 OR atkdat(3) = 5) AND j >= 4 THEN
 FOR o = 4 TO 11
  IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN
   IF atkdat(3) <> 5 OR j <> o THEN
    t(j, targetptr) = o
    targetptr = targetptr + 1
   END IF
  END IF
 NEXT o
END IF

'hero targets hero
IF (atkdat(3) = 1 OR atkdat(3) = 4 OR atkdat(3) = 5 OR atkdat(3) = 10) AND j < 4 THEN
 FOR o = 0 TO 3
  IF visibleandalive(o, stat(), v()) OR (v(o) = 1 AND (atkdat(3) = 4 OR atkdat(3) = 10)) THEN
   IF atkdat(3) <> 5 OR j <> o THEN
    t(j, targetptr) = o
    targetptr = targetptr + 1
   END IF
  END IF
 NEXT o
END IF

'self
IF atkdat(3) = 2 AND targetable(j, j, ebits()) THEN t(j, 0) = j

'all
IF atkdat(3) = 3 THEN
 FOR o = 0 TO 11
  IF visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN
   t(j, targetptr) = o
   targetptr = targetptr + 1
  END IF
 NEXT o
END IF

'revenge one
IF atkdat(3) = 6 AND revenge(j) >= 0 THEN
 IF visibleandalive(revenge(j), stat(), v()) AND targetable(j, revenge(j), ebits()) THEN
  t(j, targetptr) = revenge(j)
 END IF
END IF

'revengeall
IF atkdat(3) = 7 THEN
 FOR o = 0 TO 11
  IF readbit(revengemask(), j, o) AND visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN
   t(j, targetptr) = o
   targetptr = targetptr + 1
  END IF
 NEXT o
END IF

'lasttargs
IF atkdat(3) = 8 THEN
 FOR o = 0 TO 11
  IF readbit(targmem(), j, o) AND visibleandalive(o, stat(), v()) AND targetable(j, o, ebits()) THEN
   t(j, targetptr) = o
   targetptr = targetptr + 1
  END IF
 NEXT o
END IF

'stored targs
IF atkdat(3) = 9 THEN
 FOR o = 0 TO 11
  IF readbit(targmem(), j + 12, o) AND visibleandalive(o, stat(), v()) THEN
   t(j, targetptr) = o
   targetptr = targetptr + 1
  END IF
 NEXT o
END IF

END SUB

FUNCTION enemycount (v(), stat())
o = 0
FOR i = 4 TO 11
 IF v(i) = 1 AND stat(i, 0, 0) > 0 THEN o = o + 1
NEXT i
enemycount = o
END FUNCTION

SUB eretreat (who, atk(), x(), y(), w(), h(), t())

IF atk(14) = 2 OR atk(14) = 5 THEN
 pushw 11: pushw who: pushw 0
 pushw 8: pushw who: pushw x(who): pushw y(who): pushw 6: pushw 6
 pushw 9
END IF

END SUB

SUB etwitch (who, atk(), x(), y(), w(), h(), t())

IF atk(14) < 2 THEN
 pushw 11: pushw who: pushw 2
 pushw 13: pushw 1
 pushw 11: pushw who: pushw 0
END IF
IF atk(14) = 2 THEN
 yt = (h(t(who, 0)) - h(who)) + 2
 pushw 8: pushw who: pushw x(t(who, 0)) - w(who): pushw y(t(who, 0)) + yt: pushw 6: pushw 6
 pushw 9
END IF
IF atk(14) = 3 THEN
 FOR ii = 0 TO 2
  pushw 3: pushw who: pushw x(who): pushw y(who): pushw 1
  pushw 13: pushw 1
  pushw 3: pushw who: pushw x(who): pushw y(who): pushw 0
  pushw 13: pushw 1
 NEXT ii
END IF
IF atk(14) = 4 THEN
 pushw 8: pushw who: pushw x(who) + 50: pushw y(who): pushw 7: pushw 7
 pushw 15: pushw who: pushw 10: pushw 20
 pushw 9
 pushw 6: pushw who
END IF
IF atk(14) = 5 THEN
 pushw 11: pushw who: pushw 200
 pushw 5: pushw who
 pushw 3: pushw who: pushw x(t(who, 0)): pushw y(t(who, 0)): pushw 0
 pushw 15: pushw who: pushw -10: pushw 20
 pushw 9
END IF

END SUB

FUNCTION getbinsize (id)
fbdim recordsize

IF isfile(workingdir$ + "\binsize.bin" + CHR$(0)) THEN
 fh = FREEFILE
 OPEN workingdir$ + "\binsize.bin" FOR BINARY AS #fh
 IF LOF(fh) < id * 2 + 2 THEN
  getbinsize = defbinsize(id)
 ELSE
  GET #fh, 1 + (id * 2), recordsize
  getbinsize = recordsize
 END IF
 CLOSE #fh
ELSE
 getbinsize = defbinsize(id)
END IF

'0  ATTACK.BIN
'1 .STF

END FUNCTION

SUB heroanim (who, atk(), x(), y(), w(), h(), t())

IF atk(14) < 3 OR (atk(14) > 6 AND atk(14) < 9) THEN
 pushw 7: pushw who: pushw 0
 pushw 13: pushw 3
 IF atk(14) <> 1 AND atk(14) <> 7 THEN
  pushw 7: pushw who: pushw 2
  IF atk(14) <> 2 THEN
   pushw 3: pushw 24: pushw x(who) + 4: pushw y(who): pushw 0
  END IF
  yt = (h(t(who, 0)) - h(who)) + 2
  IF atk(14) = 2 THEN
   pushw 3: pushw 24: pushw x(t(who, 0)) + w(t(who, 0)) + 24: pushw y(t(who, 0)) + yt: pushw 0
  END IF
  pushw 7: pushw 24: pushw 0
  pushw 5: pushw 24
 END IF
 IF atk(14) = 1 OR atk(14) = 7 THEN
  pushw 7: pushw who: pushw 4
 END IF
 pushw 13: pushw 3
 IF atk(14) <> 1 AND atk(14) <> 7 THEN
  pushw 7: pushw who: pushw 3
  IF atk(14) <> 2 THEN
   pushw 3: pushw 24: pushw x(who) - 40: pushw y(who): pushw 0
  END IF
  yt = (h(t(who, 0)) - h(who)) + 2
  IF atk(14) = 2 THEN
   pushw 3: pushw 24: pushw x(t(who, 0)) + w(t(who, 0)) - 20: pushw y(t(who, 0)) + yt: pushw 0
  END IF
  pushw 7: pushw 24: pushw 1
 END IF
END IF
IF atk(14) = 3 THEN
 FOR ii = 0 TO 2
  pushw 3: pushw who: pushw x(who): pushw y(who): pushw 1
  pushw 3: pushw 24: pushw x(who) + 40: pushw y(who): pushw 0
  pushw 7: pushw 24: pushw 0
  pushw 13: pushw 1
  pushw 3: pushw who: pushw x(who): pushw y(who): pushw 0
  pushw 3: pushw 24: pushw x(who) - 40: pushw y(who): pushw 0
  pushw 7: pushw 24: pushw 1
  pushw 13: pushw 1
 NEXT ii
END IF
IF atk(14) = 4 THEN
 pushw 7: pushw who: pushw 4
 pushw 8: pushw who: pushw x(who) - 40: pushw y(who): pushw 7: pushw 7
 pushw 15: pushw who: pushw 10: pushw 20
 pushw 9
 pushw 6: pushw who
 pushw 7: pushw who: pushw 0
END IF
IF atk(14) = 5 THEN
 pushw 11: pushw who: pushw 200
 pushw 7: pushw who: pushw 2
 pushw 5: pushw who
 pushw 3: pushw who: pushw x(t(who, 0)): pushw y(t(who, 0)): pushw 0
 pushw 15: pushw who: pushw -10: pushw 20
 pushw 9
 pushw 7: pushw who: pushw 5
END IF

END SUB

FUNCTION inflict (w, t, stat(), x(), y(), wid(), hei(), harm$(), hc(), hx(), hy(), atk(), tcount, die(), bits(), revenge(), revengemask(), targmem(), revengeharm(), repeatharm())

DIM tbits(4)

dim h&

'failure by default
inflict = 0

'remember this target
setbit targmem(), w, t, 1

'stored targs
IF readbit(atk(), 20, 52) THEN setbit targmem(), w + 12, t, 1
IF readbit(atk(), 20, 53) THEN
 FOR i = 0 TO 11
  setbit targmem(), w + 12, i, 0
 NEXT i
END IF

FOR i = 0 TO 4
 tbits(i) = bits(t, i)
NEXT i

'no damage
IF atk(5) <> 4 THEN
 
 'init
 cure = 0
 harm$(t) = ""
 'harm$(w) = "" ' this is probably bad! What if they already have a harm$ and we wipe it out?
 hc(t) = 7
 hx(t) = x(t) + (wid(t) * .5)
 hy(t) = y(t) + (hei(t) * .5)
 targstat = atk(18)
 
 'accuracy
 a = stat(w, 0, 3): d = stat(t, 0, 5): dm! = .25
 IF atk(6) = 1 THEN dm! = .5
 IF atk(6) = 2 THEN dm! = 1
 IF atk(6) = 3 THEN dm! = 0
 IF atk(6) = 4 THEN a = stat(w, 0, 6): d = stat(t, 0, 7): dm! = 1.25
 IF range(a, 75) < range(d * dm!, 75) THEN
  harm$(t) = readglobalstring$(120, "miss", 20)
  EXIT FUNCTION
 END IF
 
 'attack and defence base
 a = stat(w, 0, 2): d = stat(t, 0, 4)
 SELECT CASE atk(7)
  CASE 1
   a = stat(w, 0, 6): d = stat(t, 0, 7)
  CASE 2
   a = stat(w, 0, 0)
  CASE 3
   a = (stat(w, 1, 0) - stat(w, 0, 0))
  CASE 4
   a = INT(RND * 999)
  CASE 5
   a = 100
  CASE 6 TO 17
   a = stat(w, 0, atk(7) - 6)
  CASE 18
   a = repeatharm(w)
  CASE 19
   a = revengeharm(w)
  CASE 20
   a = revengeharm(t)
 END SELECT
 
 '--defense base
 IF atk(58) > 0 THEN d = stat(t, 0, atk(58) - 1)
 
 'calc defense
 am! = 1: dm! = .5                    'atk-def*.5
 IF atk(5) = 1 THEN am! = .8: dm! = .1 'atk*.8-def*.5
 IF atk(5) = 2 THEN am! = 1.3: dm! = 1 'atk-1.3-def
 IF atk(5) = 3 THEN am! = 1: dm! = 0   'atk
 
 'resetting
 IF readbit(atk(), 20, 57) = 1 THEN
  stat(t, 0, targstat) = stat(t, 1, targstat)
 END IF
 
 'calc harm
 h& = (a * am!) - (d * dm!)
 
 'elementals
 FOR i = 0 TO 7
  IF readbit(atk(), 20, 5 + i) = 1 THEN
   IF readbit(tbits(), 0, 0 + i) = 1 THEN h& = h& * 2   'weakness
   IF readbit(tbits(), 0, 8 + i) = 1 THEN h& = h& * .12 'resistance
   IF readbit(tbits(), 0, 16 + i) = 1 THEN cure = 1   'absorb
  END IF
  IF readbit(atk(), 20, 13 + i) = 1 THEN
   IF t >= 4 AND readbit(tbits(), 0, 24 + i) = 1 THEN h& = h& * 1.8
  END IF
  IF readbit(atk(), 20, 21 + i) = 1 THEN
   IF readbit(tbits(), 0, 8 + i) = 1 THEN
    harm$(t) = readglobalstring$(122, "fail", 20)
    EXIT FUNCTION
   END IF
  END IF
  IF readbit(atk(), 20, 29 + i) = 1 THEN
   IF t >= 4 AND readbit(tbits(), 0, 24 + i) = 1 THEN
    harm$(t) = readglobalstring$(122, "fail", 20)
    EXIT FUNCTION
   END IF
  END IF
 NEXT i
 
 'extra damage
 h& = h& + (h& / 100) * atk(11)
 
 'randomize
 r = 0
 IF readbit(atk(), 20, 61) = 0 THEN r = INT(RND * 40) - 20
 h& = h& + r
 
 'spread damage
 IF readbit(atk(), 20, 1) = 1 THEN h& = h& / (tcount + 1)
 
 'cap out
 IF readbit(atk(), 20, 62) = 0 AND h& <= 0 THEN h& = 1
 
 IF readbit(atk(), 20, 0) = 1 THEN h& = ABS(h&) * -1 'cure bit
 IF readbit(tbits(), 0, 54) THEN h& = ABS(h&)        'zombie
 IF cure = 1 THEN h& = ABS(h&) * -1                  'absorb
 
 'backcompat MP-targstat
 IF readbit(atk(), 20, 60) THEN
  IF targstat = 0 THEN targstat = 1
 END IF
 
 'remember target stat
 remtargstat = stat(t, 0, targstat)
 rematkrstat = stat(w, 0, targstat)
 
 'pre-calculate percentage damage for display
 SELECT CASE atk(5)
  CASE 5'% of max
   chp& = stat(t, 0, targstat)
   mhp& = stat(t, 1, targstat)
   h& = chp& - (mhp& + (atk(11) * mhp& / 100))
  CASE 6'% of cur
   h& = stat(t, 0, targstat) - (stat(t, 0, targstat) + (atk(11) * stat(t, 0, targstat) / 100))
 END SELECT

 'inflict
 IF readbit(atk(), 20, 51) = 0 THEN
  IF gen(genDamageCap) > 0 THEN
   IF h& > gen(genDamageCap) THEN h& = gen(genDamageCap)
   IF h& < -gen(genDamageCap) THEN h& = -gen(genDamageCap)
  END IF
  h = h&
  stat(t, 0, targstat) = safesubtract(stat(t, 0, targstat), h)
  IF readbit(atk(), 20, 2) THEN
   '--drain
   IF readbit(atk(), 20, 56) = 0 THEN
    harm$(w) = RIGHT$(STR$(h), LEN(STR$(h)) - 1)
    IF h > 0 THEN harm$(w) = "+" + harm$(w)
   END IF
   hc(w) = 7
   hc(w + 12) = 12 'pink
   hx(w) = x(w) + (wid(w) * .5)
   hy(w) = y(w) + (hei(w) * .5)
   stat(w, 0, targstat) = stat(w, 0, targstat) + h
  END IF
 END IF
 
 'enforce bounds
 stat(t, 0, targstat) = large(stat(t, 0, targstat), 0)
 stat(w, 0, targstat) = large(stat(w, 0, targstat), 0)
 IF readbit(atk(), 20, 58) = 0 THEN
  stat(t, 0, targstat) = small(stat(t, 0, targstat), large(stat(t, 1, targstat), remtargstat))
  stat(w, 0, targstat) = small(stat(w, 0, targstat), large(stat(w, 1, targstat), rematkrstat))
 END IF
 
 'set damage display
 IF readbit(atk(), 20, 56) = 0 THEN
  harm$(t) = RIGHT$(STR$(h), LEN(STR$(h)) - 1)
  '--if cure, show + sign
  IF h < 0 THEN harm$(t) = "+" + harm$(t)
 END IF
 
 'remember revenge data
 IF remtargstat > stat(t, 0, targstat) THEN
  setbit revengemask(), t, w, 1
  revenge(t) = w
  revengeharm(t) = remtargstat - stat(t, 0, targstat)
  repeatharm(w) = remtargstat - stat(t, 0, targstat)
 END IF
 
END IF 'skips to here if no damage

'name
IF readbit(atk(), 20, 55) = 1 THEN
 IF LEN(harm$(t)) > 0 THEN harm$(t) = harm$(t) + " "
 harm$(t) = harm$(t) + readbadbinstring$(atk(), 24, 10, 1)
END IF

'--success!
inflict = 1

END FUNCTION

FUNCTION liveherocount (stat())
i = 0
FOR o = 0 TO 3
 IF hero(o) > 0 AND stat(o, 0, 0) > 0 THEN i = i + 1
NEXT o
liveherocount = i
END FUNCTION

SUB loadfoe (i, formdata(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), stat(), ebits(), batname$())
setpicstuf buffer(), 320, -1
IF formdata(i * 4) > 0 THEN
 loadset game$ + ".dt1" + CHR$(0), formdata(i * 4) - 1, 0
 FOR o = 0 TO 160
  es(i, o) = buffer(o)
 NEXT o
 FOR o = 0 TO 4
  ebits(i * 5 + o) = buffer(74 + o)
 NEXT o
 x(4 + i) = formdata(i * 4 + 1)
 y(4 + i) = formdata(i * 4 + 2)
 p(4 + i) = 44 + i
 getpal16 pal16(), 44 + i, es(i, 54)
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
 batname$(4 + i) = ""
 FOR o = 1 TO es(i, 0)
  batname$(4 + i) = batname$(4 + i) + CHR$(es(i, o))
 NEXT o
END IF
END SUB

FUNCTION randomally (who)
IF who < 4 THEN
 randomally = INT(RND * 4)
ELSE
 randomally = 4 + INT(RND * 8)
END IF
END FUNCTION

FUNCTION randomfoe (who)
IF who >= 4 THEN
 randomfoe = INT(RND * 4)
ELSE
 randomfoe = 4 + INT(RND * 8)
END IF
END FUNCTION

SUB readattackdata (array(), index)

flusharray array(), 39 + curbinsize(0) / 2, 0

setpicstuf array(), 80, -1
loadset game$ + ".dt6" + CHR$(0), index, 0

size = getbinsize(0)

IF size THEN
 IF isfile(workingdir$ + "\attack.bin" + CHR$(0)) THEN
  setpicstuf buffer(), size, -1
  loadset workingdir$ + "\attack.bin" + CHR$(0), index, 0
  FOR i = 0 TO size / 2 - 1
   array(40 + i) = buffer(i)
  NEXT i
 END IF
END IF

END SUB

SUB retreat (who, atk(), x(), y(), w(), h(), t())

IF atk(14) < 2 THEN
 pushw 14: pushw who
 pushw 2: pushw who: pushw 5: pushw 0: pushw 4: pushw 0
 pushw 9
 pushw 7: pushw who: pushw 0
END IF
IF atk(14) = 2 OR atk(14) = 5 THEN
 pushw 7: pushw who: pushw 0
 pushw 14: pushw who
 pushw 11: pushw who: pushw 0
 pushw 8: pushw who: pushw x(who): pushw y(who): pushw 6: pushw 6
 pushw 9
 pushw 7: pushw who: pushw 0
END IF

END SUB

FUNCTION safesubtract (number, minus)
longnumber& = number
longminus& = minus
longresult& = longnumber& - longminus&
IF longresult& > 32767 THEN longresult& = 32767
IF longresult& < -32768 THEN longresult& = -32768
result = longresult&
safesubtract = result
END FUNCTION

FUNCTION safemultiply (number, by!)
longnumber& = number
longby! = by!
longresult& = longnumber& * longby!
IF longresult& > 32767 THEN longresult& = 32767
IF longresult& < -32768 THEN longresult& = -32768
result = longresult&
safemultiply = result
END FUNCTION

SUB setbatcap (cap$, captime, capdelay)
battlecaption$ = cap$
battlecaptime = captime
battlecapdelay = capdelay
END SUB

SUB smartarrowmask (inrange(), pt, d, axis(), tmask())
FOR i = 0 TO 11
 IF tmask(i) THEN
  distance = (axis(i) - axis(pt)) * d
  IF distance > 0 THEN
   setbit inrange(), 0, i, 1
  END IF
 END IF
NEXT i
END SUB

SUB smartarrows (pt, d, axis(), targ(), tmask(), spred)
DIM inrange(0)
inrange(0) = 0
smartarrowmask inrange(), pt, d, axis(), tmask()
IF inrange(0) THEN
 best = 999
 newptr = pt
 FOR i = 0 TO 11
  IF readbit(inrange(), 0, i) THEN
   distance = (axis(i) - axis(pt)) * d
   IF distance < best THEN
    best = distance
    newptr = i
   END IF
  END IF
 NEXT i
 pt = newptr
ELSE
 IF spred = 1 THEN
  FOR i = 0 TO 11
   targ(i) = tmask(i)
  NEXT i
  spred = 2
 END IF
END IF
END SUB

FUNCTION targetable (attacker, target, ebits())
targetable = 0
IF target < 4 THEN
 'target is hero
 targetable = 1
ELSE
 'target is enemy
 IF readbit(ebits(), (target - 4) * 5, 60 + ABS(SGN(attacker < 4))) = 0 THEN targetable = 1
END IF
END FUNCTION

FUNCTION targetmaskcount (tmask())
n = 0
FOR i = 0 TO 11
 IF tmask(i) THEN n = n + 1
NEXT i
targetmaskcount = n
END FUNCTION

SUB traceshow (s$)
textcolor 15, 240
s$ = s$ + STRING$(40 - LEN(s$), " ")
printstr s$, 0, 191, 0
printstr s$, 0, 191, 1
END SUB

FUNCTION trytheft (who, targ, atk(), es())
trytheft = 0'--return false by default
IF who <= 3 AND targ >= 4 THEN
 '--a hero is attacking an enemy
 IF readbit(atk(), 20, 4) THEN
  '--steal bitset is on for this attack
  IF es(targ - 4, 17) >= 0 THEN
   '--enemy is theftable
   stole = checktheftchance(es(targ - 4, 18), es(targ - 4, 19), es(targ - 4, 20), es(targ - 4, 21))
   IF stole THEN
    '--success!
    IF es(targ - 4, 17) = 0 THEN
     '--only one theft permitted
     es(targ - 4, 17) = -1
    END IF
    setbatcap readglobalstring$(117, "Stole", 40) + " " + readitemname$(stole - 1), 40, 0
    trytheft = -1'--return success
   ELSE
    '--steal failed
    setbatcap readglobalstring$(114, "Cannot Steal", 40) + " ", 40, 0
   END IF
  ELSE
   '--has nothing to steal / steal disabled
   setbatcap readglobalstring$(111, "Has Nothing", 30), 40, 0
  END IF
 END IF
END IF
END FUNCTION

FUNCTION exptolevel& (level)
' cp needed to level: calling with level 0 returns xp to lvl 1
' HINT: Customisation goes here :)

 exper& = 30
 FOR o = 1 TO level
  exper& = exper& * 1.2 + 5
  IF exper& > 1000000 THEN exper& = 1000000
 NEXT o
 exptolevel = exper&
END FUNCTION

SUB updatestatslevelup (i, exstat(), stat(), allowforget)
' i = who
' exstat = external stats
' stat = in-battle stats
' allowforget = forget spells if level dropped below requirement

'wipe learnmask for this hero
'for heroes not in the active party, 5th hero is used for spell bitsets (oob only)
FOR o = small(i, 4) * 6 TO small(i, 4) * 6 + 5
 learnmask(o) = 0
NEXT

'THIS PART UPDATES STATS FOR A LEVEL UP
IF exstat(i, 1, 12) THEN

 'load hero's data
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
 
 'update stats
 FOR o = 0 TO 11
  exstat(i, 1, o) = exstat(i, 1, o) + (atlevel(exstat(i, 0, 12), buffer(23 + o * 2), buffer(24 + o * 2)) - atlevel(exstat(i, 0, 12) - exstat(i, 1, 12), buffer(23 + o * 2), buffer(24 + o * 2)))
  'simulate levelup bug
  IF readbit(gen(), 101, 9) = 1 THEN
   setpicstuf buffer(), 200, -1
   FOR j = 0 TO 4
    IF eqstuf(i, j) > 0 THEN
     loadset game$ + ".itm" + CHR$(0), eqstuf(i, j) - 1, 0
     exstat(i, 1, o) = exstat(i, 1, o) + buffer(54 + o)
    END IF
   NEXT j
   setpicstuf buffer(), 636, -1
   loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
  END IF
 NEXT o

 'stat restoration
 IF readbit(gen(), 101, 2) = 0 THEN
  '--HP restoration ON
  exstat(i, 0, 0) = exstat(i, 1, 0) 'set external cur to external max
  FOR o = 0 TO 1
   stat(i, o, 0) = exstat(i, 1, 0) 'set in-battle min and max to external max
  NEXT o
 END IF
 IF readbit(gen(), 101, 3) = 0 THEN
  '--MP restoration ON
  exstat(i, 0, 1) = exstat(i, 1, 1) 'set external cur to external max
  FOR o = 0 TO 1
   stat(i, o, 1) = exstat(i, 1, 1) 'set in-battle min and max to external max
  NEXT o
  resetlmp i, exstat(i, 0, 12)
 END IF
 
 'make current stats match max stats
 FOR o = 2 TO 11
  exstat(i, 0, o) = exstat(i, 1, o)
 NEXT o
 
 'learn spells
 FOR j = 0 TO 3
  FOR o = 0 TO 23
   '--if slot is empty and slot accepts this spell and learn-by-level condition is true
   IF spell(i, j, o) = 0 AND buffer(47 + (j * 48) + (o * 2)) > 0 AND buffer(48 + (j * 48) + (o * 2)) - 1 <= exstat(i, 0, 12) AND buffer(48 + (j * 48) + (o * 2)) > 0 THEN
    spell(i, j, o) = buffer(47 + (j * 48) + (o * 2))
    setbit learnmask(), 0, small(i, 4) * 96 + j * 24 + o, 1
   END IF
   IF allowforget THEN
    '--plotscripts may lower level, forget spells if drop below requirement and know the spell specified
    IF spell(i, j, o) = buffer(47 + (j * 48) + (o * 2)) AND buffer(48 + (j * 48) + (o * 2)) - 1 > exstat(i, 0, 12) THEN
     spell(i, j, o) = 0
    END IF
   END IF
  NEXT o
 NEXT j
 
END IF

END SUB

SUB giveheroexperience (i, exstat(), exper&)
 'experience
 IF hero(i) > 0 AND exstat(i, 0, 12) < 99 THEN
  exlev&(i, 0) = exlev&(i, 0) + exper&
  'levelups
  exstat(i, 1, 12) = 0
  WHILE exlev&(i, 0) >= exlev&(i, 1) AND exstat(i, 0, 12) < 99
   exlev&(i, 0) = exlev&(i, 0) - exlev&(i, 1)
   exstat(i, 0, 12) = exstat(i, 0, 12) + 1 'current level
   exstat(i, 1, 12) = exstat(i, 1, 12) + 1 'levelup flag
   exlev&(i, 1) = exptolevel(exstat(i, 0, 12))
  WEND
 END IF
END SUB

FUNCTION visibleandalive (o, stat(), v())
visibleandalive = (v(o) = 1 AND stat(o, 0, 0) > 0)
END FUNCTION

SUB writestats (exstat(), stat())
setpicstuf buffer(), 636, -1
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  '--set out-of-battle HP and MP equal to in-battle HP and MP
  FOR o = 0 TO 1
   exstat(i, 0, o) = stat(i, 0, o)
  NEXT o
 END IF
NEXT i
END SUB

