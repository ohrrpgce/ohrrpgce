'OHRRPGCE GAME - Main battle-related routines
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
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE SUB fadein (force%)
DECLARE SUB fadeout (red%, green%, blue%, force%)
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION atkallowed% (atkid%, attacker%, spclass%, lmplev%, stat%())
DECLARE SUB herobattlebits (bitbuf%(), who%)
DECLARE SUB quickinflict (harm%, targ%, hc%(), hx%(), hy%(), x%(), y%(), w%(), h%(), harm$(), stat%())
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE SUB advance (who%, atk%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB heroanim (who%, atk%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB retreat (who%, atk%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB etwitch (who%, atk%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB eretreat (who%, atk%(), x%(), y%(), w%(), h%(), t%())
DECLARE SUB invertstack ()
DECLARE SUB fatalerror (e$)
DECLARE SUB updatestatslevelup (i%, exstat%(), stat%(), allowforget)
DECLARE FUNCTION readitemname$ (itemnum%)
DECLARE FUNCTION trytheft (who%, targ%, atk%(), es%())
DECLARE SUB setbatcap (cap$, captime%, capdelay%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE FUNCTION readbadbinstring$ (array%(), offset%, maxlen%, skipword%)
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB readattackdata (array%(), index%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE SUB traceshow (s$)
DECLARE SUB smartarrows (pt%, d%, axis%(), targ%(), tmask%(), spred%)
DECLARE FUNCTION targetmaskcount% (tmask%())
DECLARE SUB eaispread (j%, atkdat%(), t%(), stat%(), v%(), ebits%(), revenge%(), revengemask%(), targmem%())
DECLARE SUB eaifocus (j%, atkdat%(), t%(), stat%(), v%(), ebits%(), revenge%(), revengemask%(), targmem%())
DECLARE FUNCTION randomoposite% (who%)
DECLARE FUNCTION randomally% (who%)
DECLARE FUNCTION targetable% (attacker%, target%, ebits%())
DECLARE FUNCTION visibleandalive% (o%, stat%(), v%())
DECLARE FUNCTION liveherocount% (stat%())
DECLARE FUNCTION countai% (ai%, them%, es%())
DECLARE FUNCTION enemycount% (v%(), stat%())
DECLARE SUB writestats (exstat%(), stat%())
DECLARE SUB playtimer ()
DECLARE SUB debug (s$)
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
DECLARE FUNCTION inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%(), revenge%(), revengemask%(), targmem%(), revengeharm%(), repeatharm%())
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
DECLARE FUNCTION checkNoRunBit (stat%(), ebits%(), v%())
DECLARE SUB checkTagCond (t, check, tag, tagand)
DECLARE FUNCTION exptolevel& (level%)
DECLARE SUB giveheroexperience (i%, exstat%(), exper&)

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'gglobals.bi'
'$INCLUDE: 'const.bi'

REM $STATIC
FUNCTION battle (form, fatal, exstat())

'--prepare stack
bstackstart = stackpos

battle = 1
DIM a(40), atktemp(40), atk(100), st(3, 318), es(7, 160), x(24), y(24), z(24), d(24), zbuf(24), xm(24), ym(24), zm(24), mvx(24), mvy(24), mvz(24), v(24), p(24), w(24), h(24), of(24), ext$(7), ctr(11), stat(11, 1, 17), ready(11), batname$(11), menu$( _
3, 5), mend(3), spel$(23), spel(23), cost$(24), godo(11), targs(11), t(11, 12), tmask(11), delay(11), cycle(24), walk(3), aframe(11, 11), fctr(24), harm$(11), hc(23), hx(11), hy(11), die(24), conlmp(11), bits(11, 4), atktype(8), iuse(15), icons(11) _
, ebits(40), eflee(11), firstt(11), ltarg(11), found(16, 1), lifemeter(3), revenge(11), revengemask(11), revengeharm(11), repeatharm(11), targmem(23), prtimer(11, 1), spelmask(1)

mpname$ = readglobalstring(1, "MP", 10)
goldname$ = readglobalstring(32, "Gold", 10)
expname$ = readglobalstring(33, "Exp", 10)
cancelspell$ = readglobalstring$(51, "(CANCEL)", 10)
pause$ = readglobalstring$(54, "PAUSE", 10)
learned$ = " " + readglobalstring$(124, "learned", 10) + " "
goldcap$ = readglobalstring$(125, "Found", 10)
expcap$ = readglobalstring$(126, "Gained", 10)
foundcap$ = readglobalstring$(139, "Found a", 20)
foundpcap$ = readglobalstring$(141, "Found", 20)
cannotrun$ = readglobalstring$(147, "CANNOT RUN!", 20)
level1up$ = readglobalstring$(149, "Level up for", 20)
levelXup$ = readglobalstring$(151, "levels for", 20)

battlecaptime = 0
battlecapdelay = 0
battlecaption$ = ""

fademusic 0
fadeout 60, 60, 60, 0
stopsong
vpage = 0: dpage = 1: needf = 1: anim = -1: you = -1: them = -1: fiptr = 0
FOR i = 0 TO 11
 icons(i) = -1
 revenge(i) = -1
NEXT i
GOSUB checkitemusability
'hc(0-11) is harm count... hc(12-23) is harm color... I know, tacky :(
FOR i = 0 TO 11
 hc(i + 12) = 15
NEXT i

'--init affliction registers
'--it is important to understand that stat() is not the same array
'--inside battle as it is outside battle. indexes above stat(11) are
'--used differently. In battle they are affliction registers. Out of battle
'--they are picture and palette sets. Tacky hacks suck.
FOR i = 0 TO 11
 FOR o = 12 TO 17
  stat(i, 0, o) = 1000 ' cur (perhaps preserve some for heros?)
  stat(i, 1, o) = 1000 ' max
 NEXT o
 prtimer(i, 0) = INT(RND * 2000)
 prtimer(i, 1) = INT(RND * 2000)
NEXT i
laststun! = TIMER
IF gen(61) <= 0 THEN gen(61) = 161
IF gen(62) <= 0 THEN gen(62) = 159

clearpage 0
clearpage 1
clearpage 2
clearpage 3
GOSUB loadall
copypage 2, dpage

'--main battle loop----------------------------------------------------------
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 flash = loopvar(flash, 0, 14, 1)
 
 '--background animation hack
 IF a(34) > 0 THEN
  bgspeed = loopvar(bgspeed, 0, a(35), 1)
  IF bgspeed = 0 THEN
   curbg = loopvar(curbg, a(32), a(32) + a(34), 1)
   loadpage game$ + ".mxs" + CHR$(0), curbg, 2
  END IF
 END IF

 IF readbit(gen(), 101, 8) = 0 THEN
  '--debug keys
  IF keyval(62) > 1 THEN away = 11 ' Instant-cheater-running
  IF keyval(63) > 1 THEN exper& = 1000000  'Million experience!
 END IF
 IF keyval(69) > 0 THEN GOSUB pgame '--PAUSE
 '--running away
 IF carray(5) > 1 THEN
  flee = flee + 1
 END IF
 GOSUB tryrun
 IF away > 0 THEN
  FOR i = 0 TO 3
   '--if alive, animate running away
   IF stat(i, 0, 0) THEN
    xm(i) = 10 * v(i): mvx(i) = 6: walk(i) = 1: d(i) = v(i)
   END IF
  NEXT i
  away = away + 1
  IF away > 10 THEN
   battle = 0
   EXIT DO
  END IF
 END IF
 IF keyval(87) > 1 AND readbit(gen(), 101, 8) = 0 THEN vis = vis XOR 1
 IF anim >= 0 AND aset = 0 AND vdance = 0 THEN GOSUB atkscript
 IF anim >= 0 AND aset = 1 AND vdance = 0 AND away = 0 THEN GOSUB action
 GOSUB animate
 na = loopvar(na, 0, 11, 1)
 IF anim = -1 AND vdance = 0 THEN
  GOSUB meters
  IF godo(na) > 0 AND delay(na) = 0 THEN
   '--next attacker has an attack selected and the delay is over
   anim = godo(na) - 1
   who = na
   aset = 0
   godo(na) = 0
  END IF
 END IF
 yn = loopvar(yn, 0, 3, 1)
 IF you = -1 THEN
  '--if it is no heros turn, check to see if anyone is alive and ready
  IF ready(yn) = 1 AND stat(yn, 0, 0) > 0 AND dead = 0 THEN
   you = yn
   pt = 0
   mset = 0
  END IF
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
 IF vdance = -2 THEN EXIT DO 'normal victory exit
 IF dead = 2 THEN
  fatal = 1
  EXIT DO
 END IF
 IF alert > 0 THEN
  alert = alert - 1
  centerfuz 160, 190, 100, 16, 3, dpage
  edgeprint alert$, 160 - LEN(alert$) * 4, 185, 14 + tog, dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 IF needf = 1 THEN
  needf = 0
  fademusic fmvol
  fadein -1
  setkeys
 END IF
 dowait
LOOP

donebattle:
writestats exstat(), stat()
IF fatal THEN battle = 0

'--overflow checking for the battle stack
IF (stackpos - bstackstart) \ 2 > 0 THEN
 '--an overflow is not unusual. This happens if the battle terminates
 '--while an attack is still going on
 WHILE stackpos > bstackstart: dummy = popw: WEND
END IF

'--underflow checking
IF (stackpos - bstackstart) \ 2 < 0 THEN
 '--and underflow is bad. it means that whatever script was on
 '--the top of the stack has been corrupted.
 fatalerror "bstack underflow" + STR$(stackpos) + STR$(bstackstart)
END IF

fademusic 0

fadeout 0, 0, 0, -1

clearpage 0
clearpage 1
clearpage 2
clearpage 3

EXIT FUNCTION '---------------------------------------------------------------

pgame:
fuzzyrect 0, 0, 320, 200, 19, vpage
edgeprint pause$, xstring(pause$, 160), 95, 15, vpage
'--wait for a key
wk = getkey
RETURN

enemyai: '-------------------------------------------------------------------
ai = 0

'if HP is less than 20% go into desperation mode
IF stat(them, 0, 0) < stat(them, 1, 0) / 5 THEN ai = 1

'if enemy count is 1, go into alone mode
IF enemycount(v(), stat()) = 1 THEN ai = 2

'spawn allys when alone
IF ai = 2 AND es(them - 4, 81) THEN
 FOR j = 1 TO es(them - 4, 91)
  slot = -1
  FOR k = 7 TO 0 STEP -1
   IF a(k * 4) = 0 THEN slot = k
  NEXT k
  IF slot > -1 THEN
   a(slot * 4) = es(them - 4, 81)
   loadfoe slot, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), stat(), ebits(), batname$()
  END IF
 NEXT j
END IF

'make sure that the current ai set is valid
'otherwise fall back on another
IF countai(ai, them, es()) = 0 THEN
 ai = 0
 IF stat(them, 0, 0) < stat(them, 1, 0) / 5 THEN
  ai = 1
  IF countai(ai, them, es()) = 0 THEN ai = 0
 END IF
END IF

'if a valid ai set is available, use it
IF countai(ai, them, es()) > 0 THEN
 
 'pick a random attack
 lim = 0
 DO
  godo(them) = es(them - 4, 92 + (ai * 5) + INT(RND * 5))
 LOOP UNTIL godo(them) > 0
 
 'load the data for this attack
 setpicstuf atktemp(), 80, -1
 loadset game$ + ".dt6" + CHR$(0), godo(them) - 1, 0
 
 'get the delay to wait for this attack
 delay(them) = atktemp(16)
 
 IF atktemp(4) = 1 OR (atktemp(4) = 2 AND INT(RND * 100) < 33) THEN
  'spread attack
  eaispread them, atktemp(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
 ELSE
  'focused attack
  eaifocus them, atktemp(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
 END IF
END IF

'fail if MP is inadequate
IF stat(them, 0, 1) - atktemp(8) < 0 THEN godo(them) = 0

'MP-idiot loses its turn
IF godo(them) = 0 THEN
 IF readbit(ebits(), (them - 4) * 5, 55) = 0 THEN them = -1: RETURN
END IF

'ready for next
ready(them) = 0: ctr(them) = 0: them = -1

RETURN


heromenu: '-----------------------------------------------------------------
IF carray(5) > 1 THEN yn = you: you = -1: RETURN
IF carray(0) > 1 THEN pt = pt - 1: IF pt < 0 THEN pt = mend(you)
IF carray(1) > 1 THEN pt = pt + 1: IF pt > mend(you) THEN pt = 0
IF carray(4) > 1 THEN
 IF bmenu(you, pt) > 0 THEN 'simple attack
  godo(you) = bmenu(you, pt)
  setpicstuf buffer(), 80, -1
  loadset game$ + ".dt6" + CHR$(0), godo(you) - 1, 0
  delay(you) = large(buffer(16), 1)
  ptarg = 1
  flusharray carray(), 7, 0
  RETURN
 END IF
 IF bmenu(you, pt) < 0 AND bmenu(you, pt) >= -4 AND st(you, 288 + (bmenu(you, pt) + 1) * -1) < 2 THEN
  '--init spell menu
  mset = 1: sptr = 0
  sptype = (bmenu(you, pt) + 1) * -1 '-tells which menu
  FOR i = 0 TO 23
   spel$(i) = ""
   cost$(i) = ""
   spel(i) = -1
   setbit spelmask(), 0, i, 0
   IF spell(you, sptype, i) > 0 THEN
    spel(i) = spell(you, sptype, i) - 1
    setpicstuf atktemp(), 80, -1
    loadset game$ + ".dt6" + CHR$(0), spel(i), 0
    spel$(i) = readbadbinstring$(atktemp(), 24, 10, 1)
    IF st(you, 288 + sptype) = 0 THEN
     '--regular MP
     cost$(i) = STR$(focuscost(atktemp(8), stat(you, 0, 10))) + " " + mpname$ + " " + LTRIM$(STR$(stat(you, 0, 1))) + "/" + LTRIM$(STR$(stat(you, 1, 1)))
    END IF
    IF st(you, 288 + sptype) = 1 THEN
     '--level MP
     cost$(i) = "Level" + STR$(INT(i / 3) + 1) + ":  " + STR$(lmp(you, INT(i / 3)))
    END IF
    IF atkallowed(spel(i), you, st(you, 288 + sptype), INT(i / 3), stat()) THEN
     setbit spelmask(), 0, i, 1
    END IF
   END IF
   spel$(i) = rpad$(spel$(i), " ", 10)
  NEXT i
 END IF
 IF bmenu(you, pt) < 0 AND bmenu(you, pt) >= -4 AND st(you, 288 + (bmenu(you, pt) + 1) * -1) = 2 THEN
  last = -1
  sptype = (bmenu(you, pt) + 1) * -1
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
  ptarg = 1
  flusharray carray(), 7, 0
  RETURN
 END IF
 IF bmenu(you, pt) = -10 THEN mset = 2: iptr = 0: itop = 0
END IF
RETURN

atkscript: '---------------------------------------------------------------
'--check for item consumption
IF icons(who) >= 0 THEN
 IF INT(item(icons(who)) / 256) = 0 THEN
  '--abort if item is gone
  anim = -1: RETURN
 END IF
END IF
'--load attack
readattackdata atk(), anim
'--load picture
setpicstuf buffer(), 3750, 3
loadset game$ + ".pt6" + CHR$(0), atk(0), 144
'--load palette
getpal16 pal16(), 53, atk(1)
FOR i = 12 TO 23
 p(i) = 53
 of(i) = 0
 cycle(i) = -1
 z(i) = 0
NEXT i
tcount = -1: pdir = 0: conmp = 1
IF who >= 4 THEN pdir = 1
ltarg(who) = 0
'CANNOT HIT INVISIBLE FOES
FOR i = 0 TO 11
 IF t(who, i) > -1 THEN
  IF v(t(who, i)) = 0 AND (atk(3) <> 4 AND atk(3) <> 10) THEN
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
'Kill Target history
targmem(who) = 0
' BIG CRAZY SCRIPT CONSTRUCTION
'DEBUG debug "begin script construction"
IF who < 4 THEN
 setpicstuf buffer(), 576, 3
 loadset game$ + ".pt5" + CHR$(0), exstat(who, 0, 13), 156
 p(24) = 52
 getpal16 pal16(), 52, exstat(who, 1, 13)
END IF
numhits = atk(17) + INT(RND * (stat(who, 0, 11) + 1))
IF readbit(atk(), 20, 49) THEN numhits = atk(17)
'----------------------------NULL ANIMATION
IF atk(15) = 10 THEN
 IF who < 4 THEN advance who, atk(), x(), y(), w(), h(), t()
 FOR j = 1 TO numhits
  IF who < 4 THEN heroanim who, atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch who, atk(), x(), y(), w(), h(), t()
  FOR i = 0 TO tcount
   pushw 10: pushw t(who, i)
  NEXT i
  pushw 6: pushw 24
 NEXT j
 IF who < 4 THEN retreat who, atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat who, atk(), x(), y(), w(), h(), t()
 pushw 0
END IF
'----------------------------NORMAL, DROP, SPREAD-RING, and SCATTER
IF atk(15) = 0 OR atk(15) = 3 OR atk(15) = 6 OR (atk(15) = 4 AND tcount > 0) THEN
 FOR i = 0 TO tcount
  yt = (h(t(who, i)) - 50) + 2
  xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
  pushw 3: pushw 12 + i: pushw x(t(who, i)) + xt: pushw y(t(who, i)) + yt: pushw pdir
  IF atk(15) = 3 THEN
   pushw 11: pushw 12 + i: pushw 180
  END IF
  IF atk(15) = 4 THEN
   pushw 3: pushw 12 + i: pushw x(t(who, i)) + xt: pushw y(t(who, i)) + yt - w(t(who, i)): pushw pdir
  END IF
 NEXT i
 IF who < 4 THEN advance who, atk(), x(), y(), w(), h(), t()
 FOR j = 1 TO numhits
  IF who < 4 THEN heroanim who, atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch who, atk(), x(), y(), w(), h(), t()
  FOR i = 0 TO tcount
   pushw 5: pushw 12 + i
   IF atk(15) = 4 THEN
    pushw 8: pushw 12 + i: pushw x(t(who, i)) + xt - w(t(who, i)): pushw y(t(who, i)) + yt: pushw 3: pushw 3
   END IF
   IF atk(15) = 3 THEN
    pushw 15: pushw 12 + i: pushw -10: pushw 20
   END IF
   IF atk(15) = 6 THEN
    pushw 8: pushw 12 + i: pushw INT(RND * 270): pushw INT(RND * 150): pushw 6: pushw 6
   END IF
  NEXT i
  pushw 13: pushw 2
  IF atk(15) = 3 THEN
   pushw 13: pushw 3
  END IF
  pushw 7: pushw who: pushw 0
  pushw 6: pushw 24
  IF atk(15) = 4 THEN
   FOR i = 0 TO tcount
    pushw 8: pushw 12 + i: pushw x(t(who, i)) + xt: pushw y(t(who, i)) + yt + w(t(who, i)): pushw 3: pushw 3
   NEXT i
   pushw 9
   FOR i = 0 TO tcount
    pushw 8: pushw 12 + i: pushw x(t(who, i)) + xt + w(t(who, i)): pushw y(t(who, i)) + yt: pushw 3: pushw 3
   NEXT i
   pushw 9
   FOR i = 0 TO tcount
    pushw 8: pushw 12 + i: pushw x(t(who, i)) + xt: pushw y(t(who, i)) + yt - w(t(who, i)): pushw 3: pushw 3
   NEXT i
   pushw 9
  END IF
  FOR i = 0 TO tcount
   pushw 10: pushw t(who, i)
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   IF readbit(atk(), 20, 0) = 0 THEN
    pushw 7: pushw t(who, i): pushw 5
   END IF
   IF readbit(atk(), 20, 0) = 1 THEN
    pushw 7: pushw t(who, i): pushw 2
   END IF
  NEXT i
  IF atk(15) <> 4 THEN
   pushw 13: pushw 3
  END IF
  FOR i = 0 TO tcount
   pushw 6: pushw 12 + i
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   pushw 7: pushw t(who, i): pushw 0
  NEXT i
  pushw 13: pushw 2
 NEXT j
 IF who < 4 THEN retreat who, atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat who, atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  pushw 7: pushw t(who, i): pushw 0
 NEXT i
 pushw 0
END IF
'----------------------------SEQUENTIAL PROJECTILE
IF atk(15) = 7 THEN
 IF who < 4 THEN advance who, atk(), x(), y(), w(), h(), t()
 FOR j = 1 TO numhits
  IF who < 4 THEN heroanim who, atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch who, atk(), x(), y(), w(), h(), t()
  temp = 50: IF who < 4 THEN temp = -50
  dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
  pushw 3: pushw 12: pushw x(who) + temp: pushw y(who): pushw dtemp
  pushw 5: pushw 12
  FOR i = 0 TO tcount
   yt = (h(t(who, i)) - 50) + 2
   xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
   pushw 8: pushw 12: pushw x(t(who, i)) + xt: pushw y(t(who, i)) + yt: pushw 5: pushw 5
   pushw 9
   pushw 10: pushw t(who, i)
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   IF readbit(atk(), 20, 0) = 0 THEN
    pushw 7: pushw t(who, i): pushw 5
   END IF
   IF readbit(atk(), 20, 0) = 1 THEN
    pushw 7: pushw t(who, i): pushw 2
   END IF
   pushw 13: pushw 3
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   pushw 7: pushw t(who, i): pushw 0
   IF i = 0 THEN
    pushw 6: pushw 24
   END IF
  NEXT i
  IF who < 4 THEN
   pushw 8: pushw 12: pushw -50: pushw 100: pushw 5: pushw 5
  END IF
  IF who >= 4 THEN
   pushw 8: pushw 12: pushw 320: pushw 100: pushw 5: pushw 5
  END IF
  pushw 9
  pushw 6: pushw 12
 NEXT j
 IF who < 4 THEN retreat who, atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat who, atk(), x(), y(), w(), h(), t()
 pushw 0
END IF
'-----------------PROJECTILE, REVERSE PROJECTILE and METEOR
IF (atk(15) >= 1 AND atk(15) <= 2) OR atk(15) = 8 THEN
 IF who < 4 THEN advance who, atk(), x(), y(), w(), h(), t()
 FOR j = 1 TO numhits
  FOR i = 0 TO tcount
   temp = 50: IF who < 4 THEN temp = -50
   dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
   yt = (h(t(who, i)) - 50) + 2
   xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 THEN
    pushw 3: pushw 12 + i: pushw x(who) + temp: pushw y(who): pushw dtemp
   END IF
   IF atk(15) = 2 THEN
    pushw 3: pushw 12 + i: pushw x(t(who, i)) + xt: pushw y(t(who, i)) + yt: pushw dtemp
   END IF
   IF atk(15) = 8 THEN
    IF who < 4 THEN
     pushw 3: pushw 12 + i: pushw 320: pushw 100: pushw dtemp
    END IF
    IF who >= 4 THEN
     pushw 3: pushw 12 + i: pushw -50: pushw 100: pushw dtemp
    END IF
    pushw 11: pushw 12 + i: pushw 180
   END IF
  NEXT i
  IF who < 4 THEN heroanim who, atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch who, atk(), x(), y(), w(), h(), t()
  FOR i = 0 TO tcount
   pushw 5: pushw 12 + i
   temp = 50: IF who < 4 THEN temp = -50
   yt = (h(t(who, i)) - 50) + 2
   xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 OR atk(15) = 8 THEN
    pushw 8: pushw 12 + i: pushw x(t(who, i)) + xt: pushw y(t(who, i)) + yt: pushw 6: pushw 6
   END IF
   IF atk(15) = 2 THEN
    pushw 8: pushw 12 + i: pushw x(who) + temp: pushw y(who): pushw 6: pushw 6
   END IF
   IF atk(15) = 8 THEN
    pushw 15: pushw 12 + i: pushw -6: pushw 30
   END IF
  NEXT i
  pushw 13: pushw 8
  pushw 6: pushw 24
  pushw 7: pushw who: pushw 0
  FOR i = 0 TO tcount
   pushw 10: pushw t(who, i)
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   IF readbit(atk(), 20, 0) = 0 THEN
    pushw 7: pushw t(who, i): pushw 5
   ELSE
    pushw 7: pushw t(who, i): pushw 2
   END IF
  NEXT i
  pushw 13: pushw 3
  FOR i = 0 TO tcount
   pushw 6: pushw 12 + i
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   pushw 7: pushw t(who, i): pushw 0
  NEXT i
  pushw 13: pushw 3
 NEXT j
 IF who < 4 THEN retreat who, atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat who, atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  pushw 7: pushw t(who, i): pushw 0
 NEXT i
 pushw 0
END IF
'--------------------------------------DRIVEBY
IF atk(15) = 9 THEN
 IF who < 4 THEN advance who, atk(), x(), y(), w(), h(), t()
 FOR j = 1 TO numhits
  dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
  FOR i = 0 TO tcount
   yt = (h(t(who, i)) - 50) + 2
   IF who < 4 THEN
    pushw 3: pushw 12 + i: pushw 320: pushw y(t(who, i)) + yt: pushw dtemp
   END IF
   IF who >= 4 THEN
    pushw 3: pushw 12 + i: pushw -50: pushw y(t(who, i)) + yt: pushw dtemp
   END IF
  NEXT i
  IF who < 4 THEN heroanim who, atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch who, atk(), x(), y(), w(), h(), t()
  FOR i = 0 TO tcount
   pushw 5: pushw 12 + i
   temp = 50: IF who < 4 THEN temp = -50
   yt = (h(t(who, i)) - 50) + 2
   pushw 8: pushw 12 + i: pushw x(t(who, i)) + xt: pushw y(t(who, i)) + yt: pushw 8: pushw 8
  NEXT i
  pushw 13: pushw 4
  pushw 6: pushw 24
  pushw 7: pushw who: pushw 0
  pushw 9
  FOR i = 0 TO tcount
   pushw 10: pushw t(who, i)
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   IF readbit(atk(), 20, 0) = 0 THEN
    pushw 7: pushw t(who, i): pushw 5
   ELSE
    pushw 7: pushw t(who, i): pushw 2
   END IF
   yt = (h(t(who, i)) - 50) + 2
   IF who < 4 THEN
    pushw 8: pushw 12 + i: pushw -50: pushw y(t(who, i)) + yt: pushw 5: pushw 7
   END IF
   IF who >= 4 THEN
    pushw 8: pushw 12 + i: pushw 320: pushw y(t(who, i)) + yt: pushw 5: pushw 7
   END IF
  NEXT i
  pushw 9
  FOR i = 0 TO tcount
   pushw 6: pushw 12 + i
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   pushw 7: pushw t(who, i): pushw 0
  NEXT i
  pushw 13: pushw 3
 NEXT j
 IF who < 4 THEN retreat who, atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat who, atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  pushw 7: pushw t(who, i): pushw 0
 NEXT i
 pushw 0
END IF
'--------------------------------FOCUSED RING
IF atk(15) = 4 AND tcount = 0 THEN
 dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
 IF who < 4 THEN advance who, atk(), x(), y(), w(), h(), t()
 FOR j = 1 TO numhits
  i = 0
  yt = (h(t(who, i)) - 50) + 2
  xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
  pushw 3: pushw 12 + 0: pushw x(t(who, i)) + xt + 0: pushw y(t(who, i)) + yt - 50: pushw dtemp
  pushw 3: pushw 12 + 1: pushw x(t(who, i)) + xt + 30: pushw y(t(who, i)) + yt - 30: pushw dtemp
  pushw 3: pushw 12 + 2: pushw x(t(who, i)) + xt + 50: pushw y(t(who, i)) + yt + 0: pushw dtemp
  pushw 3: pushw 12 + 3: pushw x(t(who, i)) + xt + 30: pushw y(t(who, i)) + yt + 30: pushw dtemp
  pushw 3: pushw 12 + 4: pushw x(t(who, i)) + xt - 0: pushw y(t(who, i)) + yt + 50: pushw dtemp
  pushw 3: pushw 12 + 5: pushw x(t(who, i)) + xt - 30: pushw y(t(who, i)) + yt + 30: pushw dtemp
  pushw 3: pushw 12 + 6: pushw x(t(who, i)) + xt - 50: pushw y(t(who, i)) + yt - 0: pushw dtemp
  pushw 3: pushw 12 + 7: pushw x(t(who, i)) + xt - 30: pushw y(t(who, i)) + yt - 30: pushw dtemp
  IF who < 4 THEN heroanim who, atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch who, atk(), x(), y(), w(), h(), t()
  yt = (h(t(who, 0)) - 50) + 2
  xt = 0: IF t(who, i) = who AND who < 4 AND atk(14) <> 7 THEN xt = -20
  FOR i = 0 TO 7
   pushw 5: pushw 12 + i
   pushw 8: pushw 12 + i: pushw x(t(who, 0)) + xt: pushw y(t(who, 0)) + yt: pushw 4: pushw 4
  NEXT i
  pushw 13: pushw 8
  pushw 6: pushw 24
  pushw 7: pushw who: pushw 0
  FOR i = 0 TO tcount
   pushw 10: pushw t(who, i)
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   IF readbit(atk(), 20, 0) = 0 THEN
    pushw 7: pushw t(who, i): pushw 5
   ELSE
    pushw 7: pushw t(who, i): pushw 2
   END IF
  NEXT i
  pushw 13: pushw 3
  FOR i = 0 TO 7
   pushw 6: pushw 12 + i
  NEXT i
  FOR i = 0 TO tcount
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   pushw 7: pushw t(who, i): pushw 0
  NEXT i
  pushw 13: pushw 3
 NEXT j
 IF who < 4 THEN retreat who, atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat who, atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  pushw 7: pushw t(who, i): pushw 0
 NEXT i
 pushw 0
END IF
'--------------------------------WAVE
IF atk(15) = 5 THEN
 yt = y(t(who, 0)) + (h(t(who, 0)) - 50) + 2
 IF who < 4 THEN advance who, atk(), x(), y(), w(), h(), t()
 FOR j = 1 TO numhits
  FOR i = 0 TO 11
   temp = -50: IF who < 4 THEN temp = 320
   IF tcount > 0 OR atk(4) = 1 THEN
    pushw 3: pushw 12 + i: pushw temp: pushw i * 15: pushw pdir
   ELSE
    pushw 3: pushw 12 + i: pushw temp: pushw yt: pushw pdir
   END IF
  NEXT i
  IF who < 4 THEN heroanim who, atk(), x(), y(), w(), h(), t()
  IF who >= 4 THEN etwitch who, atk(), x(), y(), w(), h(), t()
  temp = 24: IF who < 4 THEN temp = -24
  FOR i = 0 TO 11
   pushw 5: pushw 12 + i
   pushw 2: pushw 12 + i: pushw temp: pushw 0: pushw 16: pushw 0
   pushw 13: pushw 1
  NEXT i
  pushw 13: pushw 15
  pushw 6: pushw 24
  pushw 7: pushw who: pushw 0
  FOR i = 0 TO tcount
   pushw 10: pushw t(who, i)
   temp = 3: IF t(who, i) >= 4 THEN temp = -3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   IF readbit(atk(), 20, 0) = 0 THEN
    pushw 7: pushw t(who, i): pushw 5
   ELSE
    pushw 7: pushw t(who, i): pushw 2
   END IF
  NEXT i
  pushw 9
  FOR i = 0 TO 11
   pushw 6: pushw 12 + i
  NEXT i
  FOR i = 0 TO tcount
   temp = -3: IF t(who, i) >= 4 THEN temp = 3
   pushw 2: pushw t(who, i): pushw temp: pushw 0: pushw 2: pushw 0
   pushw 7: pushw t(who, i): pushw 0
  NEXT i
  pushw 13: pushw 2
 NEXT j
 IF who < 4 THEN retreat who, atk(), x(), y(), w(), h(), t()
 IF who >= 4 THEN eretreat who, atk(), x(), y(), w(), h(), t()
 FOR i = 0 TO tcount
  pushw 7: pushw t(who, i): pushw 0
 NEXT i
 pushw 0
END IF
'--setup animation pattern
FOR i = 0 TO 11
 fctr(i) = 0
 IF atk(2) = 0 THEN aframe(i, 0) = 0: aframe(i, 1) = 0: aframe(i, 2) = 1: aframe(i, 3) = 1: aframe(i, 4) = 2: aframe(i, 5) = 2: aframe(i, 6) = -1
 IF atk(2) = 1 THEN aframe(i, 0) = 2: aframe(i, 1) = 2: aframe(i, 2) = 1: aframe(i, 3) = 1: aframe(i, 4) = 0: aframe(i, 5) = 0: aframe(i, 6) = -1
 IF atk(2) = 2 THEN aframe(i, 0) = 0: aframe(i, 1) = 0: aframe(i, 2) = 1: aframe(i, 3) = 1: aframe(i, 4) = 2: aframe(i, 5) = 2: aframe(i, 6) = 1: aframe(i, 7) = 1: aframe(i, 8) = -1
 IF atk(2) = 3 THEN aframe(i, 0) = -1: aframe(i, 1) = -1
NEXT i
'--if caption has length and is set to display
IF atk(37) AND atk(36) >= 0 THEN
 '--load caption
 setbatcap readbinstring$(atk(), 37, 38), 0, battlecapdelay = atk(57)
 SELECT CASE atk(36)
  CASE 0
   '--full duration
   battlecaptime = 16383 + atk(57)
  CASE IS > 0
   '--timed
   battlecaptime = atk(36) + atk(57)
 END SELECT
END IF
'DEBUG debug "stackpos =" + STR$((stackpos - bstackstart) \ 2)
invertstack
'--aset indicates that animation is set and that we should proceed to "action"
aset = 1
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
 act = popw
 SELECT CASE act
  CASE 0 '--end()
   FOR i = 0 TO 3
    '--enforce weak picture
    IF stat(i, 0, 0) < stat(i, 1, 0) / 5 AND vdance = 0 THEN of(i) = 6
    '--re-enforce party's X/Y positions...
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
   'DEBUG debug "~ end"
  CASE 1 '???()
   FOR i = 0 TO 3
    a(i * 4 + 1) = x(4 + i)
    a(i * 4 + 2) = y(4 + i)
   NEXT i
   anim = -1
   'DEBUG debug "~ 1???"
  CASE 2 'setmovement(who,xm,ym,xstep,ystep)
   ww = popw
   xm(ww) = popw
   ym(ww) = popw
   mvx(ww) = popw
   mvy(ww) = popw
   'DEBUG debug "~ setmovement" + STR$(w) + STR$(xm(w)) + STR$(ym(w)) + STR$(mvx(w)) + STR$(mvy(w))
  CASE 3 'setposition(who,x,y,d)
   ww = popw
   x(ww) = popw
   y(ww) = popw
   d(ww) = popw
   'DEBUG debug "~ setposition" + STR$(w) + STR$(x(w)) + STR$(y(w)) + STR$(d(w))
  CASE 4 '???()
   '--undefined
   'DEBUG debug "~ undefined4"
  CASE 5 'appear(who)
   ww = popw
   v(ww) = 1
   'DEBUG debug "~ appear" + STR$(w)
  CASE 6 'disappear(who)
   ww = popw
   v(ww) = 0
   'DEBUG debug "~ disappear" + STR$(w)
  CASE 7 'setframe(who,frame)
   ww = popw
   fr = popw
   IF ww < 4 THEN walk(ww) = 0: of(ww) = fr
   IF ww > 23 THEN of(ww) = fr '--is this right?
   'DEBUG debug "~ setframe" + STR$(w) + STR$(fr)
  CASE 8 'relmovement(who,n,n,n,n)
   ww = popw
   tmp1 = popw
   tmp2 = popw
   tmp3 = popw
   tmp4 = popw
   mvx(ww) = (tmp1 - x(ww)) / tmp3
   mvy(ww) = (tmp2 - y(ww)) / tmp4
   xm(ww) = tmp3
   ym(ww) = tmp4
   'DEBUG debug "~ relmovement" + STR$(w) + STR$(tmp1) + STR$(tmp2) + STR$(tmp3) + STR$(tmp4)
  CASE 9 'waitforall()
   wf = -1
   'DEBUG debug "~ waitforall"
  CASE 10 'inflict(targ)
   targ = popw
   'set tag, if there is one
   checkTagCond atk(60), 1, atk(59), atk(61)
   checkTagCond atk(63), 1, atk(62), atk(64)
   'DEBUG debug "~ inflict on " + STR$(targ) + " by " + str$(who)
   IF inflict(who, targ, stat(), x(), y(), w(), h(), harm$(), hc(), hx(), hy(), atk(), tcount, die(), bits(), revenge(), revengemask(), targmem(), revengeharm(), repeatharm()) THEN
    '--attack succeeded
	IF readbit(atk(), 20, 50) = 1 THEN
	 es(targ - 4, 56) = 0
	 es(targ - 4, 57) = 0
	 es(targ - 4, 59) = 0
	 es(targ - 4, 61) = 0
	END IF
	IF readbit(atk(), 20, 63) = 1 THEN
	 'force heroes to run away
	 IF checkNoRunBit(stat(), ebits(), v()) THEN
	  alert$ = cannotrun$
	  alert = 10
	 ELSE
	  away = 1
	 END IF
	END IF
	checkTagCond atk(60), 2, atk(59), atk(61)
	checkTagCond atk(63), 2, atk(62), atk(64)
	IF stat(targ, 0, 0) = 0 THEN
	 checkTagCond atk(60), 4, atk(59), atk(61)
	 checkTagCond atk(63), 4, atk(62), atk(64)
	END IF
	
    IF trytheft(who, targ, atk(), es()) THEN
     GOSUB checkitemusability
    END IF
   ELSE
	checkTagCond atk(60), 3, atk(59), atk(61)
	checkTagCond atk(63), 3, atk(62), atk(64)
   END IF
   tdwho = targ
   GOSUB triggerfade
   IF stat(targ, 0, 0) > 0 THEN
    '---REVIVE---
    v(targ) = 1: die(targ) = 0
   END IF
   IF targ >= 4 THEN GOSUB sponhit
   IF atk(8) > 0 AND conmp = 1 THEN
    '--if the attack costs MP, and we actually want to consume MP
    stat(who, 0, 1) = large(stat(who, 0, 1) - focuscost(atk(8), stat(who, 0, 10)), 0)
    '--set the flag to prevent re-consuming MP
    conmp = 0
   END IF
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
     tdwho = who
     stat(tdwho, 0, 0) = 0
     GOSUB triggerfade
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
   IF stat(targ, 0, 0) = 0 AND o < 8 AND anim > -1 THEN
    IF atk(4) = 1 OR (atk(4) = 2 AND INT(RND * 100) < 33) THEN
     eaispread who, buffer(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
    ELSE
     eaifocus who, buffer(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
    END IF
   END IF
  CASE 11 'setz(who,z)
   ww = popw
   z(ww) = popw
   'DEBUG debug "~ setz" + STR$(w) + STR$(z(w))
  CASE 12 '???(n,n,n,n,n)
   'unimplemented
   'DEBUG debug "~ unimplemented12"
  CASE 13 'waitfor(ticks)
   wf = popw
   'DEBUG debug "~ waitfor" + STR$(wf)
  CASE 14 'walktoggle(who)
   ww = popw
   of(ww) = 0
   IF ww < 4 THEN walk(ww) = walk(ww) XOR 1
   'DEBUG debug "~ walktoggle" + STR$(ww)
  CASE 15 'zmovement(who,zm,zstep)
   ww = popw
   zm(ww) = popw
   mvz(ww) = popw
   'DEBUG debug "~ zmovement" + STR$(w) + STR$(zm(w)) + STR$(mvz(w))
 END SELECT
LOOP UNTIL wf <> 0 OR anim = -1

IF anim = -1 THEN
 GOSUB afterdone
 '--clean up stack
 'DEBUG debug "discarding" + STR$((stackpos - bstackstart) \ 2) + " from stack"
 WHILE stackpos > bstackstart: dummy = popw: WEND
 '-------Spawn a Chained Attack--------
 IF atk(12) > 0 AND INT(RND * 100) < atk(13) AND stat(who, 0, 0) > 0 THEN
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
   IF buffer(4) <> atk(4) OR buffer(3) <> atk(3) THEN
    IF buffer(4) = 1 OR (buffer(4) = 2 AND INT(RND * 100) < 33) THEN
     eaispread who, buffer(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
    ELSE
     eaifocus who, buffer(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
    END IF
   END IF
  END IF
 END IF
END IF
RETURN

afterdone:
'--hide the caption when the animation is done
IF atk(36) = 0 THEN
 '--clear duration-timed caption
 battlecaptime = 0
 battlecapdelay = 0
END IF
GOSUB dieWOboss
GOSUB fulldeathcheck
RETURN

dieWOboss:
bosses = 0
'--count bosses
FOR j = 4 TO 11
 '--is it a boss?
 IF readbit(ebits(), (j - 4) * 5, 56) = 1 THEN
  '-- is it alive?
  IF stat(j, 0, 0) > 0 THEN
   bosses = bosses + 1
  END IF
 END IF
NEXT j
'--if there are no bossess...
IF bosses = 0 THEN
 '--for each foe...
 FOR j = 4 TO 11
  '--should it die without a boss?
  IF readbit(ebits(), (j - 4) * 5, 58) = 1 THEN
   '-- is it still alive?
   IF stat(j, 0, 0) > 0 THEN
    '--trigger death fade
    tdwho = j
    stat(tdwho, 0, 0) = 0
    GOSUB triggerfade
   END IF
  END IF
 NEXT j
END IF
RETURN

triggerfade:
IF stat(tdwho, 0, 0) = 0 THEN
 die(tdwho) = w(tdwho) * .5
 IF tdwho >= 4 THEN
  '--flee as alternative to death
  IF readbit(ebits(), (tdwho - 4) * 5, 59) = 1 THEN
   eflee(tdwho) = 1
   die(tdwho) = (w(tdwho) + x(tdwho)) / 10
  END IF
 END IF
END IF
RETURN

fulldeathcheck:
deadguycount = 0
FOR deadguy = 4 TO 11
 GOSUB ifdead
NEXT deadguy
IF deadguycount = 8 THEN dead = 1
deadguycount = 0
FOR deadguy = 0 TO 3
 GOSUB ifdead
NEXT deadguy
IF deadguycount = 4 THEN dead = 2
RETURN

ifdead:
IF deadguy >= 4 THEN
 IF stat(deadguy, 0, 0) > 0 AND readbit(ebits(), (deadguy - 4) * 5, 61) = 1 THEN deadguycount = deadguycount + 1
END IF
IF stat(deadguy, 0, 0) = 0 THEN
 '--deadguy is really dead
 deadguycount = deadguycount + 1
 v(deadguy) = 0
 ready(deadguy) = 0
 godo(deadguy) = 0
 d(deadguy) = 0
 '--reset poison/regen/stun
 FOR j = 12 TO 17
  '--am I certain that these all should be reset?
  stat(deadguy, 0, j) = stat(deadguy, 1, j)
 NEXT j
 IF you = deadguy THEN you = -1: mset = 0
 IF them = deadguy THEN them = -1
 GOSUB spawnally
 IF deadguy >= 4 THEN '------PLUNDER AND EXPERIENCE AND ITEMS------
  IF a((deadguy - 4) * 4) > 0 THEN
   plunder& = plunder& + es(deadguy - 4, 56)
   IF plunder& > 1000000000 THEN plunder& = 1000000000
   exper& = exper& + es(deadguy - 4, 57)
   IF exper& > 1000000 THEN exper& = 1000000
   IF INT(RND * 100) < es(deadguy - 4, 59) THEN '---GET ITEMS FROM FOES-----
    FOR j = 0 TO 16
     IF found(j, 1) = 0 THEN found(j, 0) = es(deadguy - 4, 58): found(j, 1) = 1: EXIT FOR
     IF found(j, 0) = es(deadguy - 4, 58) THEN found(j, 1) = found(j, 1) + 1: EXIT FOR
    NEXT j
   ELSE '------END NORMAL ITEM---------------
    IF INT(RND * 100) < es(deadguy - 4, 61) THEN
     FOR j = 0 TO 16
      IF found(j, 1) = 0 THEN found(j, 0) = es(deadguy - 4, 60): found(j, 1) = 1: EXIT FOR
      IF found(j, 0) = es(deadguy - 4, 60) THEN found(j, 1) = found(j, 1) + 1: EXIT FOR
     NEXT j
    END IF '---END RARE ITEM-------------
   END IF '----END GET ITEMS----------------
  END IF
  'a((deadguy - 4) * 4) = 0 ' fixing spawning bug
 END IF'------------END PLUNDER-------------------
 IF noifdead = 0 THEN '---THIS IS NOT DONE FOR ALLY+DEAD------
  tcount = tcount - 1
  FOR j = 0 TO 11
   t(j, 12) = -1
   FOR k = 0 TO 11
    IF t(j, k) = deadguy AND readbit(ltarg(), j, deadguy) = 0 THEN SWAP t(j, k), t(j, k + 1)
   NEXT k
   IF t(j, 0) = -1 AND who <> j AND godo(j) > 0 THEN
    'godo(j) = 0: ready(j) = 1: delay(j) = 0
    setpicstuf buffer(), 80, -1
    loadset game$ + ".dt6" + CHR$(0), godo(j) - 1, 0
    IF buffer(4) = 1 OR (buffer(4) = 2 AND INT(RND * 100) < 33) THEN
     eaispread j, buffer(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
    ELSE
     eaifocus j, buffer(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
    END IF
   END IF
   IF tmask(deadguy) = 1 THEN tmask(deadguy) = 0
   IF targs(deadguy) = 1 THEN targs(deadguy) = 0
  NEXT j
  IF tptr = deadguy THEN
   WHILE tmask(tptr) = 0
    tptr = tptr + 1: IF tptr > 11 THEN ptarg = 0: RETURN
   WEND
  END IF
 END IF  '----END ONLY WHEN NOIFDEAD = 0
END IF  '----END (deadguy) IS DEAD
RETURN

sponhit:
FOR i = 0 TO 8
 IF es(targ - 4, 82 + i) > 0 AND atktype(i) = 1 THEN
  FOR j = 1 TO es(targ - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF a(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    a(slot * 4) = es(targ - 4, 82 + i)
    loadfoe slot, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), stat(), ebits(), batname$()
   END IF
  NEXT j
  EXIT FOR
 END IF
NEXT i
RETURN

spawnally:
IF deadguy >= 4 THEN
 IF es(deadguy - 4, 80) > 0 AND atktype(0) = 1 THEN
  die(deadguy) = 1
  FOR j = 1 TO es(deadguy - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF a(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    a(slot * 4) = es(deadguy - 4, 80)
    deadguycount = deadguycount - 1
    loadfoe slot, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), stat(), ebits(), batname$()
   END IF
  NEXT j
  es(deadguy - 4, 80) = 0
 END IF
 IF es(deadguy - 4, 79) > 0 THEN
  die(deadguy) = 1
  FOR j = 1 TO es(deadguy - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF a(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    a(slot * 4) = es(deadguy - 4, 79)
    deadguycount = deadguycount - 1
    loadfoe slot, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), stat(), ebits(), batname$()
   END IF
  NEXT j
  es(deadguy - 4, 79) = 0
 END IF
END IF
RETURN

itemmenu:
IF carray(5) > 1 THEN
 mset = 0
 flusharray carray(), 7, 0
 icons(you) = -1 '--is this right?
END IF
IF carray(0) > 1 AND iptr > 2 THEN iptr = iptr - 3
IF carray(1) > 1 AND iptr < 195 THEN iptr = iptr + 3
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
IF carray(2) > 1 AND iptr > 0 THEN
 iptr = iptr - 1
END IF
IF carray(3) > 1 AND iptr < 197 THEN
 iptr = iptr + 1
END IF
'--scroll when past top or bottom
IF iptr < itop THEN itop = itop - 3
IF iptr > itop + 26 THEN itop = itop + 3

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
  ptarg = 1
  mset = 0
  flusharray carray(), 7, 0
 END IF
END IF
RETURN

spellmenu:
IF carray(5) > 1 THEN '--cancel
 mset = 0
 flusharray carray(), 7, 0
END IF
IF carray(0) > 1 THEN
 IF sptr > 2 THEN sptr = sptr - 3 ELSE sptr = 24
END IF
IF carray(1) > 1 THEN
 IF sptr < 24 THEN sptr = small(sptr + 3, 24) ELSE sptr = 0
END IF
IF carray(2) > 1 AND sptr < 24 AND sptr > 0 THEN
 sptr = sptr - 1
END IF
IF carray(3) > 1 AND sptr < 24 THEN
 sptr = sptr + 1
END IF
IF carray(4) > 1 THEN
 '--use selected spell
 IF sptr = 24 THEN
  '--used cancel
  mset = 0
  flusharray carray(), 7, 0
  RETURN
 END IF
 
 '--can-I-use-it? checking
 IF spel(sptr) > -1 THEN
  '--list-entry is non-empty
  IF atkallowed(spel(sptr), you, st(you, 288 + sptype), INT(sptr / 3), stat()) THEN
   '--attack is allowed
   '--if lmp then set lmp consume flag
   IF st(you, 288 + sptype) = 1 THEN conlmp(you) = INT(sptr / 3) + 1
   '--load atk data (for delay)
   setpicstuf atktemp(), 80, -1
   loadset game$ + ".dt6" + CHR$(0), spel(sptr), 0
   '--queue attack
   godo(you) = spel(sptr) + 1
   delay(you) = large(atktemp(16), 1)
   '--exit spell menu
   ptarg = 1: mset = 0
   flusharray carray(), 7, 0
  END IF
 END IF
END IF
RETURN

picktarg: '-----------------------------------------------------------

'cancel
IF carray(5) > 1 THEN
 godo(you) = 0
 ptarg = 0
 flusharray carray(), 7, 0
 RETURN
END IF

IF ptarg = 1 THEN GOSUB setuptarg

'autotarget
IF ptarg = 3 THEN
 IF buffer(4) = 1 OR (buffer(4) = 2 AND INT(RND * 100) < 33) THEN
  eaispread you, buffer(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
 ELSE
  eaifocus you, buffer(), t(), stat(), v(), ebits(), revenge(), revengemask(), targmem()
 END IF
 ctr(you) = 0
 ready(you) = 0
 firstt(you) = tptr + 1
 you = -1
 ptarg = 0
 RETURN
END IF

IF targetmaskcount(tmask()) = 0 THEN
 RETURN
END IF

'random target
IF ran = 1 THEN
 FOR i = 0 TO INT(RND * 2)
  tptr = loopvar(tptr, 0, 11, 1)
  WHILE tmask(tptr) = 0
   tptr = loopvar(tptr, 0, 11, 1)
  WEND
 NEXT i
END IF

'first target
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
IF spred = 2 AND (carray(2) > 1 OR carray(3) > 1) THEN
 FOR i = 0 TO 11
  targs(i) = 0
 NEXT i
 spred = 1
 flusharray carray(), 7, 0
END IF
IF aim = 1 AND spred < 2 THEN
 IF carray(0) > 1 THEN
  smartarrows tptr, -1, y(), targs(), tmask(), 0
 END IF
 IF carray(1) > 1 THEN
  smartarrows tptr, 1, y(), targs(), tmask(), 0
 END IF
 IF carray(2) > 1 THEN
  smartarrows tptr, -1, x(), targs(), tmask(), spred
 END IF
 IF carray(3) > 1 THEN
  smartarrows tptr, 1, x(), targs(), tmask(), spred
 END IF
END IF
IF carray(4) > 1 THEN GOSUB gottarg
RETURN

gottarg: '-----------------------------------------------------------------
targs(tptr) = 1
o = 0
FOR i = 0 TO 11
 IF targs(i) = 1 THEN
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

setuptarg: '--identify valid targets (heroes only)

'init
spred = 0: aim = 0: ran = 0: firstt(you) = 0: tptr = 0
FOR i = 0 TO 11
 targs(i) = 0
 tmask(i) = 0
 t(you, i) = -1
NEXT i

'load attack
setpicstuf buffer(), 80, -1
loadset game$ + ".dt6" + CHR$(0), godo(you) - 1, 0

noifdead = 0
ltarg(you) = 0

SELECT CASE buffer(3)
 
 CASE 0 'enemy
  FOR i = 4 TO 11: tmask(i) = v(i): NEXT i
  
 CASE 1 'ally
  FOR i = 0 TO 3: tmask(i) = v(i): NEXT i
  
 CASE 2 'self
  tmask(you) = 1
  
 CASE 3 'all
  FOR i = 0 TO 11: tmask(i) = v(i): NEXT i: tptr = 4
  
 CASE 4 'ally-including-dead
  noifdead = 1
  FOR i = 0 TO 3
   IF hero(i) > 0 THEN tmask(i) = 1
  NEXT i
  
 CASE 5 'ally-not-self
  FOR i = 0 TO 3
   tmask(i) = v(i)
  NEXT i
  tmask(you) = 0
  
 CASE 6 'revenge-one
  IF revenge(you) >= 0 THEN
   tmask(revenge(you)) = v(revenge(you))
  END IF
  
 CASE 7 'revenge-all
  FOR i = 0 TO 11
   tmask(i) = (readbit(revengemask(), you, i) AND v(i))
  NEXT i
  
 CASE 8 'previous
  FOR i = 0 TO 11
   tmask(i) = (readbit(targmem(), you, i) AND v(i))
  NEXT i
  
 CASE 9 'stored
  FOR i = 0 TO 11
   tmask(i) = (readbit(targmem(), you + 12, i) AND v(i))
  NEXT i
  
 CASE 10 'dead-ally (hero only)
  noifdead = 1
  FOR i = 0 TO 3
   IF hero(i) > 0 AND stat(i, 0, 0) = 0 THEN tmask(i) = 1
  NEXT i
  
END SELECT

'enforce attack's disabled target slots
FOR i = 0 TO 7
 IF readbit(buffer(), 20, 37 + i) THEN tmask(4 + i) = 0
NEXT i
FOR i = 0 TO 3
 IF readbit(buffer(), 20, 45 + i) THEN tmask(i) = 0
NEXT i

'enforce untargetability by heros
FOR i = 4 TO 11
 IF readbit(ebits(), (i - 4) * 5, 61) = 1 THEN tmask(i) = 0
NEXT i

'fail if there are no targets
WHILE tmask(tptr) = 0
 tptr = tptr + 1: IF tptr > 11 THEN ptarg = 0: RETURN
WEND

'autoattack
IF readbit(buffer(), 20, 54) THEN
 ptarg = 3
 RETURN
END IF

IF buffer(4) = 0 THEN aim = 1
IF buffer(4) = 1 THEN FOR i = 0 TO 11: targs(i) = tmask(i): NEXT i
IF buffer(4) = 2 THEN aim = 1: spred = 1
IF buffer(4) = 3 THEN ran = 1
IF buffer(4) = 4 THEN ran = 2
'ready to choose targ from targset
ptarg = 2
RETURN

iconsume: '-----------------------------------------------------------------
lb = (item(icons(who)) AND 255)
hb = INT(item(icons(who)) / 256)
hb = large(hb - 1, 0)
item(icons(who)) = lb + (hb * 256)
item$(icons(who)) = LEFT$(item$(icons(who)), 9) + RIGHT$(STR$(hb), 2)
IF hb = 0 THEN item(icons(who)) = 0: item$(icons(who)) = "           ": setbit iuse(), 0, icons(who), 0
icons(who) = -1
RETURN

display:
IF vdance = 0 THEN 'only display interface till you win
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   IF readbit(gen(), 101, 6) = 0 THEN
    '--speed meter--
    col = 18: IF ready(i) = 1 THEN col = 21
    centerfuz 66, 9 + i * 10, 131, 10, 1, dpage
    IF stat(i, 0, 0) > 0 THEN
     j = ctr(i) / 7.7
     IF delay(i) > 0 OR godo(i) > 0 OR (anim >= 0 AND who = i) THEN
      col = 18
      j = 130
     END IF
     rectangle 2, 5 + i * 10, j, 9, col, dpage
    END IF
   END IF
   IF readbit(gen(), 101, 7) = 0 THEN
    '--hp-meter--
    col = 35
    IF lifemeter(i) < INT((87 / large(stat(i, 1, 0), 1)) * stat(i, 0, 0)) THEN lifemeter(i) = lifemeter(i) + 1
    IF lifemeter(i) > INT((87 / large(stat(i, 1, 0), 1)) * stat(i, 0, 0)) THEN lifemeter(i) = lifemeter(i) - 1
    IF lifemeter(i) > 87 THEN
     lifemeter(i) = 87
     col = 35 + tog * 2
    END IF
    centerfuz 180, 9 + i * 10, 88, 10, 1, dpage
    rectangle 137, 5 + i * 10, lifemeter(i), 9, col, dpage
   END IF
   '--name--
   col = 7: IF i = you THEN col = 14 + tog
   edgeprint batname$(i), 128 - LEN(batname$(i)) * 8, 5 + i * 10, col, dpage
   '--hp--
   edgeprint LTRIM$(STR$(stat(i, 0, 0))) + "/" + LTRIM$(STR$(stat(i, 1, 0))), 136, 5 + i * 10, col, dpage
   'poison indicator
   IF (stat(i, 1, 12) - stat(i, 0, 12)) > 0 THEN
   edgeprint CHR$(gen(61)), 209, 5 + i * 10, col, dpage
   END IF
   'stun indicator
   IF (stat(i, 1, 14) - stat(i, 0, 14)) > 0 THEN
   edgeprint CHR$(gen(62)), 217, 5 + i * 10, col, dpage
   END IF
  END IF
 NEXT i
 IF battlecaptime > 0 THEN
  battlecaptime = battlecaptime - 1
  IF battlecapdelay > 0 THEN
   battlecapdelay = battlecapdelay - 1
  ELSE
   centerbox 160, 186, 310, 14, 1, dpage
   edgeprint battlecaption$, xstring(battlecaption$, 160), 182, 15, dpage
  END IF
 END IF
 IF you >= 0 THEN
  centerbox 268, 5 + (4 * (mend(you) + 2)), 88, 8 * (mend(you) + 2), 1, dpage
  FOR i = 0 TO mend(you)
   textcolor 7, 0
   IF pt = i THEN textcolor 14 + tog, 1
   printstr menu$(you, i), 228, 9 + i * 8, dpage
  NEXT i
  IF mset = 1 THEN '--draw spell menu
   centerbox 148, 45, 280, 80, 1, dpage
   rectangle 7, 74, 282, 1, 25, dpage
   FOR i = 0 TO 23
    textcolor 8 - readbit(spelmask(), 0, i), 0
    IF sptr = i THEN textcolor 7 + (7 * readbit(spelmask(), 0, i)) + tog, 1
    printstr spel$(i), 16 + (((i / 3) - INT(i / 3)) * 3) * 88, 8 + INT(i / 3) * 8, dpage
   NEXT i
   textcolor 7, 0
   IF sptr = 24 THEN textcolor 14 + tog, 1
   printstr cancelspell$, 16, 76, dpage
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
    IF targs(i) = 1 OR tptr = i THEN
     edgeprint CHR$(24), x(i) + (w(i) / 2) - 4, y(i) - 6, 160 + flash, dpage
     edgeprint batname$(i), xstring(batname$(i), x(i) + (w(i) / 2)), y(i) - 16, 14 + tog, dpage
    END IF
   NEXT i
  END IF
 END IF
 IF you >= 0 AND ptarg = 0 AND readbit(gen(), genBits, 14) = 0 THEN edgeprint CHR$(24), x(you) + (w(you) / 2) - 4, y(you) - 5 + (tog * 2), 14 + tog, dpage
END IF'--end if vdance=0
RETURN

meters:
IF away = 1 THEN RETURN
'--if a menu is up, and pause-on-menus is ON then no time passes
IF (mset > 0 AND readbit(gen(), genBits, 0)) OR (mset >= 0 AND you >= 0 AND readbit(gen(), genBits, 13)) THEN RETURN

FOR i = 0 TO 11
 
 'delays for attacks already selected
 IF you <> i THEN delay(i) = large(delay(i) - 1, 0)
 
 '--poison/regen
 FOR poisreg = 0 TO 1
  dif = stat(i, 1, 12 + poisreg) - stat(i, 0, 12 + poisreg)
  IF dif > 0 THEN
   prtimer(i, poisreg) = prtimer(i, poisreg) + large(stat(i, 0, 8), 7)
   IF prtimer(i, poisreg) >= 1500 THEN
    prtimer(i, poisreg) = 0
    harm = dif
    IF poisreg = 1 THEN '--regen
     harm = harm * -1
    END IF
    harm = range(harm, 20)
    quickinflict harm, i, hc(), hx(), hy(), x(), y(), w(), h(), harm$(), stat()
    tdwho = i
    GOSUB triggerfade
    GOSUB dieWOboss
    GOSUB fulldeathcheck
   END IF
  END IF
 NEXT poisreg
 
 '--if not doing anything, not dying, not ready, and not stunned
 IF godo(i) = 0 AND die(i) = 0 AND ready(i) = 0 AND stat(i, 0, 14) = stat(i, 1, 14) THEN
  '--increment ctr by speed
  ctr(i) = small(1000, ctr(i) + stat(i, 0, 8))
  IF ctr(i) = 1000 AND wf = 0 THEN ready(i) = 1
 END IF
 
NEXT i

'--decrement stun

IF TIMER > laststun! + 1 THEN
 FOR i = 0 TO 11
  stat(i, 0, 14) = small(stat(i, 0, 14) + 1, stat(i, 1, 14))
  IF stat(i, 0, 14) < stat(i, 1, 14) THEN
   ready(i) = 0
   godo(i) = 0
   IF you = i THEN you = -1
   IF them = i THEN them = -1
  END IF
 NEXT i
 laststun! = TIMER
END IF

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
   IF die(i) = 0 THEN a(i * 4) = 0 'moved from way above
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
  IF d(zbuf(i)) = 0 THEN
   drawsprite buffer(), 0, pal16(), p(zbuf(i)) * 16, x(zbuf(i)), y(zbuf(i)) - z(zbuf(i)), dpage
  ELSE
   wardsprite buffer(), 0, pal16(), p(zbuf(i)) * 16, x(zbuf(i)), y(zbuf(i)) - z(zbuf(i)), dpage
  END IF
 END IF
NEXT i
FOR i = 0 TO 11
 IF hc(i) > 0 THEN
  edgeprint harm$(i), hx(i) - LEN(harm$(i)) * 4, hy(i), hc(i + 12), dpage
  hc(i) = hc(i) - 1
  hy(i) = hy(i) - 2
  IF hc(i) = 0 THEN hc(i + 12) = 15
 END IF
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
 IF carray(6) = 0 THEN
  flee = 0
  FOR i = 0 TO 3
   d(i) = 0
   walk(i) = 0
  NEXT i
 END IF
END IF
IF flee = 4 THEN
 IF checkNoRunBit(stat(), ebits(), v()) THEN
  flee = 0
  alert$ = cannotrun$
  alert = 10
 END IF
END IF
IF flee > 4 THEN
 FOR i = 0 TO 3
  '--if alive and visible, turn around
  'IF v(i) AND stat(i, 0, 0) THEN d(i) = 1
  IF stat(i, 0, 0) THEN d(i) = 1
  walk(i) = 1
  godo(i) = 0
  ready(i) = 0
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
IF a(33) > 0 THEN wrappedsong a(33) - 1
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
  p(i) = 40 + i
  v(i) = 1
 END IF
NEXT i
FOR i = 0 TO 3
 setpicstuf buffer(), 5120, 3
 IF hero(i) > 0 THEN
  loadset game$ + ".pt0" + CHR$(0), exstat(i, 0, 14), i * 16
  getpal16 pal16(), 40 + i, exstat(i, 0, 15)
  FOR o = 0 TO 11
   stat(i, 0, o) = exstat(i, 0, o)
   stat(i, 1, o) = exstat(i, 1, o)
  NEXT o
  herobattlebits bits(), i
  batname$(i) = names$(i)
  FOR o = 0 TO 5
   menu$(i, o) = ""
   IF bmenu(i, o) > 0 THEN
    setpicstuf atk(), 80, -1
    loadset game$ + ".dt6" + CHR$(0), bmenu(i, o) - 1, 0
    menu$(i, o) = readbadbinstring$(atk(), 24, 10, 1)
   END IF
   IF bmenu(i, o) < 0 AND bmenu(i, o) > -5 THEN
    temp = (bmenu(i, o) + 1) * -1
    FOR j = 244 + temp * 11 TO 243 + temp * 11 + st(i, 243 + temp * 11)
     menu$(i, o) = menu$(i, o) + CHR$(st(i, j))
    NEXT j
   END IF
   IF bmenu(i, o) = -10 THEN menu$(i, o) = readglobalstring$(34, "Item", 10): mend(i) = o
   WHILE LEN(menu$(i, o)) < 10: menu$(i, o) = menu$(i, o) + " ": WEND
  NEXT o
 END IF
NEXT i
FOR i = 0 TO 7
 loadfoe i, a(), es(), x(), y(), p(), v(), w(), h(), ext$(), bits(), stat(), ebits(), batname$()
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
 IF stat(i, 0, 0) < stat(i, 1, 0) / 5 AND vdance = 0 THEN of(i) = 6
 IF hero(i) > 0 AND stat(i, 0, 0) = 0 THEN die(i) = 1
 lifemeter(i) = (88 / large(stat(i, 1, 0), 1)) * stat(i, 0, 0)
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
   tdwho = i
   stat(tdwho, 0, 0) = 0
   GOSUB triggerfade
  END IF
 NEXT i
 deadguycount = 0
 FOR deadguy = 4 TO 11
  GOSUB ifdead
 NEXT deadguy
 IF deadguycount = 8 THEN dead = 1
END IF
RETURN

victory: '------------------------------------------------------------------
IF gen(3) > 0 THEN wrappedsong gen(3) - 1
gold& = gold& + plunder&
IF gold& > 1000000000 THEN gold& = 1000000000
IF liveherocount(stat()) > 0 THEN exper& = exper& / liveherocount(stat())
FOR i = 0 TO 3
 IF stat(i, 0, 0) > 0 THEN giveheroexperience i, exstat(), exper&
 updatestatslevelup i, exstat(), stat(), 0
NEXT i
vdance = 1
RETURN

vicdance:
IF drawbox THEN centerfuz 160, 30, 280, 50, 1, dpage
IF vdance = 4 THEN
 '--print found items, one at a time
 GOSUB vicfind
END IF
IF vdance = 3 THEN
 '--print learned spells, one at a time
 IF showlearn = 0 THEN
  DO WHILE readbit(learnmask(), 0, learna * 96 + learnb * 24 + learnc) = 0 OR nextbit
   nextbit = 0 '-- to skip a set bit
   learnc = learnc + 1
   IF learnc > 23 THEN learnc = 0: learnb = learnb + 1
   IF learnb > 3 THEN learnb = 0: learna = learna + 1
   IF learna > 3 THEN
    vdance = 4
    found$ = ""
    drawbox = 0
    EXIT DO
   END IF
  LOOP
  IF vdance = 3 THEN
   found$ = batname$(learna) + learned$
   setpicstuf buffer(), 80, -1
   loadset game$ + ".dt6" + CHR$(0), spell(learna, learnb, learnc) - 1, 0
   found$ = found$ + readbadbinstring$(buffer(), 24, 10, 1)
   showlearn = 1
   drawbox = 1
  END IF
 ELSE
  IF carray(4) > 1 OR carray(5) > 1 THEN
   showlearn = 0
   'setbit learnmask(), 0, learna * 96 + learnb * 24 + learnc, 0
   nextbit = 1
  END IF
  edgeprint found$, xstring(found$, 160), 22, 15, dpage
 END IF
END IF
IF vdance = 2 THEN
 '--print levelups
 IF carray(4) > 1 OR carray(5) > 1 THEN found$ = "": vdance = 3
 o = 0
 FOR i = 0 TO 3
  IF o = 0 AND exstat(i, 1, 12) THEN centerfuz 160, 30, 280, 50, 1, dpage
  SELECT CASE exstat(i, 1, 12)
   CASE 1
    temp$ = level1up$ + " " + batname$(i)
    edgeprint temp$, xstring(temp$, 160), 12 + i * 10, 15, dpage
    o = 1
   CASE IS > 1
    temp$ = LTRIM$(STR$(exstat(i, 1, 12))) + " " + levelXup$ + " " + batname$(i)
    edgeprint temp$, xstring(temp$, 160), 12 + i * 10, 15, dpage
    o = 1
  END SELECT
 NEXT i
 IF o = 0 THEN vdance = 3 ELSE drawbox = 1
END IF
IF vdance = 1 THEN
 '--print acquired gold and experience
 IF carray(4) > 1 OR carray(5) > 1 OR (plunder& = 0 AND exper& = 0) THEN
  vdance = 2
 END IF
 IF plunder& > 0 OR exper& > 0 THEN drawbox = 1: centerfuz 160, 30, 280, 50, 1, dpage
 IF plunder& > 0 THEN
  temp$ = goldcap$ + STR$(plunder&) + " " + goldname$ + "!"
  edgeprint temp$, xstring(temp$, 160), 16, 15, dpage
 END IF
 IF exper& > 0 THEN
  temp$ = expcap$ + STR$(exper&) + " " + expname$ + "!"
  edgeprint temp$, xstring(temp$, 160), 28, 15, dpage
 END IF
END IF
RETURN

vicfind:
'--check to see if we are currently displaying a gotten item
IF found$ = "" THEN
 '--if not, check to see if there are any more gotten items to display
 IF found(fptr, 1) = 0 THEN vdance = -1: RETURN
 '--get the item name
 found$ = readitemname$(found(fptr, 0))
 '--actually aquire the item
 FOR i = 1 TO found(fptr, 1)
  getitem found(fptr, 0) + 1
 NEXT i
END IF
'--if the present item is gotten, show the caption
IF found(fptr, 1) = 1 THEN
 temp$ = foundcap$ + " " + found$
ELSE
 temp$ = foundpcap$ + STR$(found(fptr, 1)) + " " + found$
END IF
IF LEN(temp$) THEN centerfuz 160, 30, 280, 50, 1, dpage
edgeprint temp$, xstring(temp$, 160), 22, 15, dpage
'--check for a keypress
IF carray(4) > 1 OR carray(5) > 1 THEN
 IF found(fptr, 1) = 0 THEN
  '--if there are no further items, exit
  vdance = -1
 ELSE
  '--otherwize, increment the findpointer and reset the caption
  fptr = fptr + 1: found$ = ""
 END IF
END IF
RETURN

checkitemusability:
FOR i = 0 TO 199
 setpicstuf buffer(), 200, -1
 lb = (item(i) AND 255)
 IF lb > 0 THEN
  loadset game$ + ".itm" + CHR$(0), lb - 1, 0
  IF buffer(47) > 0 THEN setbit iuse(), 0, i, 1
 END IF
NEXT i
RETURN

'afflictions
'12=poison
'13=regen
'14=stun
'15=limit
'16=unused
'17=unused

END FUNCTION

FUNCTION checkNoRunBit (stat(), ebits(), v())
 checkNoRunBit = 0
 FOR i = 4 TO 11
  IF stat(i, 0, 0) > 0 AND v(i) = 1 AND readbit(ebits(), (i - 4) * 5, 57) = 1 THEN checkNoRunBit = 1
 NEXT i
END FUNCTION

SUB checkTagCond (t, check, tg, tagand)
 't - type, check = curtype, tg - the tag to be set, tagand - the tag to check
 IF t = check THEN
  IF tagand <> 0 AND readbit(tag(), 0, ABS(tagand)) <> SGN(SGN(tagand) + 1) THEN EXIT SUB
  setbit tag(), 0, ABS(tg), SGN(SGN(tg) + 1) 'Set the original damned tag!
 END IF
END SUB

FUNCTION focuscost (cost, focus)
IF focus > 0 THEN
 focuscost = cost - INT(cost / (100 / focus))
ELSE
 focuscost = cost
END IF
END FUNCTION

SUB herobattlebits (bitbuf(), who)

'--native bits
FOR i = 0 TO 4
 bitbuf(who, i) = nativehbits(who, i)
NEXT i

setpicstuf buffer(), 200, -1

'--equipment bits
FOR j = 0 TO 4
 IF eqstuf(who, j) > 0 THEN
  loadset game$ + ".itm" + CHR$(0), eqstuf(who, j) - 1, 0
  FOR i = 0 TO 4
   bitbuf(who, i) = (bitbuf(who, i) OR buffer(70 + i))
  NEXT i
 END IF
NEXT j

END SUB

SUB invertstack
'--this is a hack so I can use the stack like a fifo

stackdepth = (stackpos - bstackstart) \ 2

FOR i = 0 TO stackdepth - 1
 buffer(i) = popw
NEXT i

FOR i = 0 TO stackdepth - 1
 pushw buffer(i)
NEXT i

END SUB

SUB quickinflict (harm, targ, hc(), hx(), hy(), x(), y(), w(), h(), harm$(), stat())
'--quick damage infliction to hp. no bells and whistles
hc(targ) = 7
hx(targ) = x(targ) + (w(targ) * .5)
hy(targ) = y(targ) + (h(targ) * .5)
IF harm < 0 THEN
 harm$(targ) = "+" + RIGHT$(STR$(harm), LEN(STR$(harm)) - 1)
ELSE
 harm$(targ) = LTRIM$(STR$(harm))
END IF
IF readbit(gen(),genBits,15) = 1 THEN
 harm = small(harm,9999)
ELSE
 harm = small(harm,32767)
END IF
stat(targ, 0, 0) = bound(stat(targ, 0, 0) - harm, 0, stat(targ, 1, 0))
END SUB
