'OHRRPGCE GAME - Additional mostly battle-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB exitprogram (needfade%)
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB calibrate ()
DECLARE SUB getitem (getit%, num%)
DECLARE SUB getnames (stat$())
DECLARE SUB resetlmp (slot%, lev%)
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION rangel% (n&, r%)
DECLARE SUB snapshot ()
DECLARE SUB delitem (it%, num%)
DECLARE FUNCTION countitem% (it%)


#include "bmod.bi"
#include "bmodsubs.bi"

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "uiconst.bi"
#include "udts.bi"


DECLARE SUB confirm_auto_spread (who, confirmtarg(), tmask())
DECLARE SUB confirm_auto_focus (who, confirmtarg(), tmask(), atkbuf(), bslot() AS BattleSprite, bstat() AS BattleStats)
DECLARE SUB confirm_auto_first (who, confirmtarg(), tmask())

DECLARE FUNCTION quick_battle_distance(who1, who2, bslot() AS BattleSprite)
DECLARE FUNCTION battle_distance(who1, who2, bslot() AS BattleSprite)

REM $STATIC
FUNCTION is_hero(who)
 IF who >= 0 AND who <= 3 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_enemy(who)
 IF who >= 4 AND who <= 11 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_attack(who)
 IF who >= 12 AND who <= 23 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_weapon(who)
 IF who = 24 THEN RETURN -1
 RETURN 0
END FUNCTION

SUB advance (who, atk(), bslot() AS BattleSprite, t())
d = 1 ' Hero
IF is_enemy(who) THEN d = -1 ' Enemy

IF is_hero(who) THEN
 IF atk(14) < 2 OR (atk(14) > 2 AND atk(14) < 5) THEN ' strike, cast, spin, jump
  anim_walktoggle who
  anim_setmove who, -5, 0, 4, 0
  anim_waitforall
 END IF
END IF
IF atk(14) = 2 THEN ' Dash in
 yt = (bslot(t(who, 0)).h - bslot(who).h) + 2
 anim_walktoggle who
 anim_absmove who, bslot(t(who, 0)).x + bslot(t(who, 0)).w * d, bslot(t(who, 0)).y + yt, 6, 6
 anim_waitforall
END IF
IF atk(14) = 8 THEN ' Teleport
 anim_setpos who, bslot(t(who, 0)).x + bslot(t(who, 0)).w * d, bslot(t(who, 0)).y + (bslot(t(who, 0)).h - (bslot(who).h)), 0
END IF

END SUB

FUNCTION atkallowed (atkbuf(), attacker, spclass, lmplev, bstat() AS BattleStats)
'--atkbuf   = attack data
'--attacker = hero or enemy who is attacking
'--spclass  = 0 for normal attacks, 1 for level-MP spells
'--lmplev   = which level-MP level to use

'--check for mutedness
IF readbit(atkbuf(),65,0) = 1 AND bstat(attacker).cur.mute < bstat(attacker).max.mute THEN
 atkallowed = 0
 EXIT FUNCTION
END IF

'--check for sufficient mp
IF bstat(attacker).cur.mp - focuscost(atkbuf(8), bstat(attacker).cur.foc) < 0 THEN
 atkallowed = 0
 EXIT FUNCTION
END IF

'--check for level-MP (heroes only)
IF attacker <= 3 AND spclass = 1 THEN
 IF lmp(attacker, lmplev) - 1 < 0 THEN
  atkallowed = 0
  EXIT FUNCTION
 END IF
END IF

'--check for sufficient items
DIM itemid, itemcount AS INTEGER
FOR i = 0 to 2
  itemid = atkbuf(93 + i * 2)
  itemcount = atkbuf(94 + i * 2)
  IF itemid > 0 THEN 'this slot is used
    IF attacker <= 3 THEN ' Only hero items are checked right now
      IF countitem(itemid) < itemcount THEN
        'yes, this still works for adding items.
        atkallowed = 0
        EXIT FUNCTION
      END IF
    END IF
  END IF
NEXT i

'--succeed
atkallowed = -1

END FUNCTION

FUNCTION checktheftchance (item, itemP, rareitem, rareitemP)
IF RND * 100 < itemP THEN
 '--success!
 getitem item + 1, 1
 checktheftchance = item + 1
ELSE
 IF RND * 100 < rareitemP THEN
  '--rare success!
  getitem rareitem + 1, 1
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
'  7=<nothing> (not used anywhere)
'  8=??? (not used anywhere)
'  9=<nothing> (not used anywhere)
'  10-13=last CARRAY(0-3) (not used outside of this SUB)

'  CSETUP()
'  0=up 1=down 2=left 3=right                13=oldmouse X (???)
'  4=useA                                    14=oldmouse Y (???)
'  5=useB                                    15=showmouse (???)
'  6=useC
'  7=menuA                                      MOUSEMODE
'  8=menuB                                      0=map
'  9=runA                                       1=click only
'  10=runB
'  11=calibrate
'  12=??? (comma)

STATIC joyuse, joymenu

'Quick abort
IF keyval(-1) OR (keyval(73) > 0 AND keyval(81) > 0 AND keyval(1) > 1) THEN
 exitprogram 0
END IF

'alt-enter toggle windowed
if keyval(56) > 0 and keyval(28) > 0 then
	togglewindowed
end if

FOR i = 0 TO 7: carray(i) = 0: NEXT i

IF keyval(88) > 0 THEN snapshot

IF keyval(69) = 0 THEN ' no controls while PAUSE is pressed, because of its scancode wierdness
 GOSUB keyboard
 GOSUB joystick
END IF
EXIT SUB

keyboard:
FOR i = 0 TO 3
 carray(i) = keyval(csetup(i))
NEXT i
carray(4) = keyval(csetup(4)) OR keyval(csetup(5)) OR keyval(csetup(6))
carray(5) = keyval(csetup(7)) OR keyval(csetup(8))
carray(6) = keyval(csetup(9)) OR keyval(csetup(10))
'--gen(60) is the calibration disabler flag
IF gen(60) = 0 AND keyval(29) > 0 AND keyval(csetup(11)) > 1 THEN
 calibrate
 FOR i = 0 TO 1
  gotj(i) = readjoy(joy(), i)
 NEXT i
END IF
carray(8) = keyval(csetup(12))
RETRACE

joystick:

FOR i = 0 TO 1
 IF gotj(i) THEN gotj(i) = readjoy(joy(), i): EXIT FOR
NEXT i
if i = 2 THEN RETRACE

if gen(60) = 0 then retrace

'edgeprint XSTR$(i) + XSTR$(gotj(i)) + XSTR$(joy(0)) + XSTR$(joy(1)) + XSTR$(joy(2)) + XSTR$(joy(3)) + XSTR$(carray(4)) + XSTR$(carray(5)), 0, 170, uilook(uiSelectedItem), 0
'edgeprint XSTR$(i) + XSTR$(gotj(i)) + XSTR$(joy(0)) + XSTR$(joy(1)) + XSTR$(joy(2)) + XSTR$(joy(3)) + XSTR$(carray(4)) + XSTR$(carray(5)), 0, 170, uilook(uiSelectedItem), 1

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
'edgeprint XSTR$(joyuse) + XSTR$(joymenu) + XSTR$(joy(13)) + XSTR$(joy(14)), 0, 190, uilook(uiText), 0
'edgeprint XSTR$(joyuse) + XSTR$(joymenu) + XSTR$(joy(13)) + XSTR$(joy(14)), 0, 190, uilook(uiText), 1
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
RETRACE

END SUB

FUNCTION countai (ai, them, es())
o = 0
FOR i = 0 TO 4
 IF es(them - 4, 92 + (ai * 5) + i) > 0 THEN o = o + 1
NEXT i
countai = o
END FUNCTION

FUNCTION enemycount (bslot() AS BattleSprite, bstat() AS BattleStats)
o = 0
FOR i = 4 TO 11
 IF bstat(i).cur.hp > 0 THEN o = o + 1
NEXT i
RETURN o
END FUNCTION

FUNCTION targenemycount (bslot() AS BattleSprite, bstat() AS BattleStats)
o = 0
FOR i = 4 TO 11
 IF bstat(i).cur.hp > 0 AND bslot(i).vis = 1 AND bslot(i).hero_untargetable = 0 THEN o = o + 1
NEXT i
RETURN o
END FUNCTION

SUB etwitch (who, atk(), bslot() AS BattleSprite, t())

IF atk(14) < 2 THEN' twitch
 anim_setz who, 2
 anim_wait 1
 anim_setz who, 0
END IF
IF atk(14) = 3 THEN' spin
 FOR ii = 0 TO 2
  anim_setdir who, 1
  anim_wait 1
  anim_setdir who, 0
  anim_wait 1
 NEXT ii
END IF
IF atk(14) = 4 THEN' jump
 anim_absmove who, bslot(who).x + 50, bslot(who).y, 7, 7
 anim_zmove who, 10, 20
 anim_waitforall
 anim_disappear who
END IF
IF atk(14) = 5 THEN' drop
 anim_setz who, 200
 anim_appear who
 anim_setpos who, bslot(t(who, 0)).x, bslot(t(who, 0)).y, 0
 anim_zmove who, -10, 20
 anim_waitforall
END IF

END SUB

Function GetWeaponPos(w,f,isY)'or x?
 dim fh
 IF w >= 0 THEN
  fh = FREEFILE
  OPEN game + ".itm" FOR BINARY AS #fh
  'debug "weapon" + XSTR$(w) + " offset: " + XSTR$(w * 200 + 157 + f * 4 + isY * 2)
  GetWeaponPos = ReadShort(fh,w * 200 + 157 + f * 4 + iif(isY,1,0) * 2)
  CLOSE #FH
 END IF
End Function

Function GetHeroPos(h,f,isY)'or x?
 dim fh
 fh = FREEFILE
 OPEN game + ".dt0" FOR BINARY AS #fh
 'debug "hero offset: " + XSTR$(h * 636 + 595 + f * 4 + isY * 2)
 GetHeroPos = ReadShort(fh,h * 636 + 595 + f * 4 + iif(isY,1,0) * 2)
 CLOSE #FH
End Function

SUB heroanim (who, atk(), bslot() AS BattleSprite, t())
hx = 0:hy = 0:wx = 0: wy = 0: xt = 0: yt = 0
IF atk(14) < 3 OR (atk(14) > 6 AND atk(14) < 9) THEN ' strike, cast, dash, standing cast, teleport
 anim_setframe who, 0
 anim_wait 3 'wait 3 ticks
 IF atk(14) <> 1 AND atk(14) <> 7 THEN 'if it's not cast or standing cast
 
  anim_setframe who, 2
  
  hx = GetHeroPos(hero(who)-1,0,0)
  hy = GetHeroPos(hero(who)-1,0,1)
  wx = GetWeaponPos(eqstuf(who,0)-1,1,0)
  wy = GetWeaponPos(eqstuf(who,0)-1,1,1)
  dx = hx - wx
  dy = hy - wy
  
  anim_align2 24, who, 0, 0, dx, dy
  
  anim_setframe 24, 0
  anim_appear 24
  
 END IF
 
 IF atk(14) = 1 OR atk(14) = 7 THEN 'if it's cast or standing cast
  anim_setframe who, 4
 END IF
 
 anim_wait 3
 
 IF atk(14) <> 1 AND atk(14) <> 7 THEN 'if it's not cast or standing cast
  anim_setframe who, 3
  
  hx = GetHeroPos(hero(who)-1,1,0)
  hy = GetHeroPos(hero(who)-1,1,1)
  wx = GetWeaponPos(eqstuf(who,0)-1,0,0)
  wy = GetWeaponPos(eqstuf(who,0)-1,0,1)
  dx = hx - wx
  dy = hy - wy
  
  anim_align2 24, who, 0, 0, dx, dy
  
  anim_setframe 24, 1
 END IF
 
END IF
IF atk(14) = 3 THEN ' spin
 FOR ii = 0 TO 2
  anim_setdir who, 1
  anim_wait 1
  anim_setdir who, 0
  anim_wait 1
 NEXT ii
END IF
IF atk(14) = 4 THEN ' Jump
 anim_setframe who, 4
 anim_relmove who, -40, 0, 7, 0
 anim_zmove who, 20, 10
 anim_waitforall
 anim_disappear who
 anim_setframe who, 0
END IF
IF atk(14) = 5 THEN ' Land
 anim_setz who, 200
 anim_setframe who, 2
 anim_appear who
 anim_setcenter who, t(who, 0), 0, 0
 anim_align who, t(who, 0), dirDown, 0
 anim_zmove who, -10, 20
 anim_waitforall
 anim_setframe who, 5
END IF

END SUB

FUNCTION inflict (w, t, bstat() AS BattleStats, bslot() AS BattleSprite, harm$(), hc(), hx(), hy(), atk(), tcount, bits(), revenge(), revengemask(), targmem(), revengeharm(), repeatharm())

DIM tbits(4)
DIM h = 0

'failure by default
inflict = 0
bslot(w).attack_succeeded = 0

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
 hx(t) = bslot(t).x + (bslot(t).w * .5)
 hy(t) = bslot(t).y + (bslot(t).h * .5)
 targstat = bound(atk(18), 0, UBOUND(bstat(t).cur.sta))

 'accuracy
 a = bstat(w).cur.acc
 d = bstat(t).cur.dog
 dm! = .25
 IF atk(6) = 1 THEN dm! = .5
 IF atk(6) = 2 THEN dm! = 1
 IF atk(6) = 4 THEN dm! = 1.25
 IF atk(6) = 4 OR atk(6) = 7 OR atk(6) = 8 THEN a = bstat(w).cur.mag : d = bstat(t).cur.wil

 attackhit = range(a, 75) >= range(d * dm!, 75)
 IF atk(6) = 3 THEN attackhit = 1
 IF atk(6) = 5 OR atk(6) = 7 THEN attackhit = RND * 100 < (a * (100 - d)) / 100 
 IF atk(6) = 6 OR atk(6) = 8 THEN attackhit = RND * 100 < a
 IF attackhit = 0 THEN
  harm$(t) = readglobalstring$(120, "miss", 20)
  EXIT FUNCTION
 END IF

 IF readbit(atk(),65,1) = 1 AND bstat(t).cur.poison < bstat(t).max.poison THEN
  harm$(t) = readglobalstring$(122, "fail", 20)
  EXIT FUNCTION
 END IF
 IF readbit(atk(),65,2) = 1 AND bstat(t).cur.regen < bstat(t).max.regen THEN
  harm$(t) = readglobalstring$(122, "fail", 20)
  EXIT FUNCTION
 END IF
 IF readbit(atk(),65,3) = 1 AND bstat(t).cur.stun <> bstat(t).max.stun THEN
  harm$(t) = readglobalstring$(122, "fail", 20)
  EXIT FUNCTION
 END IF
 IF readbit(atk(),65,4) = 1 AND bstat(t).cur.mute <> bstat(t).max.mute THEN
  harm$(t) = readglobalstring$(122, "fail", 20)
  EXIT FUNCTION
 END IF

 'attack and defense base
 a = bstat(w).cur.str
 d = bstat(t).cur.def
 SELECT CASE atk(7)
  CASE 1
   a = bstat(w).cur.mag
   d = bstat(t).cur.wil
  CASE 2
   a = bstat(w).cur.hp
  CASE 3
   a = bstat(w).max.hp - bstat(w).cur.hp
  CASE 4
   a = INT(RND * 999)
  CASE 5
   a = 100
  CASE 6 TO 17
   a = bstat(w).cur.sta(atk(7) - 6)
  CASE 18
   a = repeatharm(w)
  CASE 19
   a = revengeharm(w)
  CASE 20
   a = revengeharm(t)
 END SELECT

 '--defense base
 IF atk(58) > 0 AND atk(58) <= UBOUND(bstat(t).cur.sta) + 1 THEN d = bstat(t).cur.sta(atk(58) - 1)

 'calc defense
 am! = 1: dm! = .5                    'atk-def*.5
 IF atk(5) = 1 THEN am! = .8: dm! = .1 'atk*.8-def*.5
 IF atk(5) = 2 THEN am! = 1.3: dm! = 1 'atk-1.3-def
 IF atk(5) = 3 THEN am! = 1: dm! = 0   'atk

 'resetting
 IF readbit(atk(), 20, 57) = 1 THEN
  bstat(t).cur.sta(targstat) = bstat(t).max.sta(targstat)
 END IF

 'calc harm
 h = (a * am!) - (d * dm!)

 'elementals
 FOR i = 0 TO 7
  IF readbit(atk(), 20, 5 + i) = 1 THEN
   IF readbit(tbits(), 0, 0 + i) = 1 THEN h = h * 2   'weakness
   IF readbit(tbits(), 0, 8 + i) = 1 THEN h = h * .12 'resistance
   IF readbit(tbits(), 0, 16 + i) = 1 THEN cure = 1   'absorb
  END IF
  IF readbit(atk(), 20, 13 + i) = 1 THEN
   IF is_enemy(t) AND readbit(tbits(), 0, 24 + i) = 1 THEN h = h * 1.8
  END IF
  IF readbit(atk(), 20, 21 + i) = 1 THEN
   IF readbit(tbits(), 0, 8 + i) = 1 THEN
    harm$(t) = readglobalstring$(122, "fail", 20)
    EXIT FUNCTION
   END IF
  END IF
  IF readbit(atk(), 20, 29 + i) = 1 THEN
   IF is_enemy(t) AND readbit(tbits(), 0, 24 + i) = 1 THEN
    harm$(t) = readglobalstring$(122, "fail", 20)
    EXIT FUNCTION
   END IF
  END IF
 NEXT i

 'extra damage
 h = h + (h / 100) * atk(11)

 'randomize
 IF readbit(atk(), 20, 61) = 0 THEN h = rangel(h,20)

 'spread damage
 IF readbit(atk(), 20, 1) = 1 THEN h = h / (tcount + 1)

 'cap out
 IF h <= 0 THEN
  IF readbit(atk(), 20, 62) = 0 THEN h = 1 ELSE h = 0
 END IF

 'backcompat MP-targstat
 IF readbit(atk(), 20, 60) THEN
  IF targstat = 0 THEN targstat = 1
 END IF

 'remember target stat
 remtargstat = bstat(t).cur.sta(targstat)
 rematkrstat = bstat(w).cur.sta(targstat)

 'pre-calculate percentage damage for display
 chp = bstat(t).cur.sta(targstat)
 mhp = bstat(t).max.sta(targstat)
 IF readbit(atk(), 65, 5) = 1 THEN
  'percentage attacks do damage
  'FIXME: see bug 134 about moving this block up the function. This should be base damage?
  SELECT CASE atk(5)
   CASE 5'% of max
    h = mhp + (atk(11) * mhp / 100)
    cure = 0
   CASE 6'% of cur
    h = chp + (atk(11) * chp / 100)
    cure = 0
  END SELECT
 END IF

 IF readbit(atk(), 20, 0) = 1 THEN h = ABS(h) * -1 'cure bit
 IF readbit(tbits(), 0, 54) THEN h = ABS(h)        'zombie
 IF cure = 1 THEN h = ABS(h) * -1                  'elemental absorb

 IF readbit(atk(), 65, 5) = 0 THEN
  'percentage attacks set stat
  'and by set, we really mean set, ignore nearly all attack settings,
  'that's my interpretation of intent anyway - TMC
  SELECT CASE atk(5)
   CASE 5'% of max
    h = chp - (mhp + (atk(11) * mhp / 100))
   CASE 6'% of cur
    h = chp - (chp + (atk(11) * chp / 100))
  END SELECT
 END IF

 'inflict
 IF readbit(atk(), 20, 51) = 0 THEN
  IF gen(genDamageCap) > 0 THEN
   IF h > gen(genDamageCap) THEN h = gen(genDamageCap)
   IF h < -gen(genDamageCap) THEN h = -gen(genDamageCap)
  END IF

  bstat(t).cur.sta(targstat) = safesubtract(bstat(t).cur.sta(targstat), h)
  IF readbit(atk(), 20, 2) THEN
   '--drain
   IF readbit(atk(), 20, 56) = 0 THEN
    harm$(w) = STR$(ABS(h))
    IF h > 0 THEN harm$(w) = "+" + harm$(w)
   END IF
   hc(w) = 7
   hc(w + 12) = 12 'pink
   hx(w) = bslot(w).x + (bslot(w).w * .5)
   hy(w) = bslot(w).y + (bslot(w).h * .5)
   bstat(w).cur.sta(targstat) = bstat(w).cur.sta(targstat) + h
  END IF
 END IF

 'enforce bounds
 bstat(t).cur.sta(targstat) = large(bstat(t).cur.sta(targstat), 0)
 bstat(w).cur.sta(targstat) = large(bstat(w).cur.sta(targstat), 0)
 IF readbit(atk(), 20, 58) = 0 THEN
  bstat(t).cur.sta(targstat) = small(bstat(t).cur.sta(targstat), large(bstat(t).max.sta(targstat), remtargstat))
  bstat(w).cur.sta(targstat) = small(bstat(w).cur.sta(targstat), large(bstat(w).max.sta(targstat), rematkrstat))
 END IF

 'set damage display
 IF readbit(atk(), 20, 56) = 0 THEN
  harm$(t) = STR$(ABS(h))
  '--if cure, show + sign
  IF h < 0 THEN harm$(t) = "+" + harm$(t)
 END IF

 'remember revenge data
 IF remtargstat > bstat(t).cur.sta(targstat) THEN
  setbit revengemask(), t, w, 1
  revenge(t) = w
  revengeharm(t) = remtargstat - bstat(t).cur.sta(targstat)
  repeatharm(w) = remtargstat - bstat(t).cur.sta(targstat)
 END IF

END IF 'skips to here if no damage
'debug(readbadbinstring$(atk(), 24, 10, 1) + " - " + XSTR$(targstat))
'name
IF readbit(atk(), 20, 55) = 1 THEN
 IF LEN(harm$(t)) > 0 THEN harm$(t) = harm$(t) + " "
 harm$(t) = harm$(t) + readbadbinstring$(atk(), 24, 10, 1)
END IF

'reset registers as per convenience bits
FOR i = 0 to 3
 IF readbit(atk(), 65, 8 + i) = 1 THEN
  bstat(t).cur.sta(12 + i) = bstat(t).max.sta(12 + i) 'this should work better, methinks
 END IF
NEXT

'--success!
inflict = 1
bslot(w).attack_succeeded = 1

END FUNCTION

FUNCTION liveherocount (bstat() AS BattleStats)
i = 0
FOR o = 0 TO 3
 IF hero(o) > 0 AND bstat(o).cur.hp > 0 THEN i = i + 1
NEXT o
liveherocount = i
END FUNCTION

SUB loadfoe (i, formdata(), es(), bslot() AS BattleSprite, p(), bits(), bstat() AS BattleStats, ebits(), batname$(), BYREF rew AS RewardsState, allow_dead = NO)
IF formdata(i * 4) > 0 THEN
 loadenemydata buffer(), formdata(i * 4) - 1, -1
 FOR o = 0 TO 160
  es(i, o) = buffer(o)
 NEXT o
 FOR o = 0 TO 4
  ebits(i * 5 + o) = buffer(74 + o)
 NEXT o
 IF allow_dead = NO THEN
  'enemies which spawn already-dead should be killed off immediately
  'die without boss or 0 hp?
  IF dieWOboss(4 + i, bstat(), ebits()) OR es(i, 62) <= 0 THEN
   'rewards and spawn enemies on death
   'enemy is only partially constructed, but already have everything needed.
   DIM atktype(8) 'regular "spawn on death"
   dead_enemy 4 + i, rew, bstat(), bslot(), es(), atktype(), formdata(), p(), bits(), ebits(), batname$()
   EXIT SUB
  END IF
 END IF
 WITH bslot(4 + i)
  .basex = formdata(i * 4 + 1)
  .basey = formdata(i * 4 + 2)
  .x = bslot(4 + i).basex
  .y = bslot(4 + i).basey
  p(4 + i) = 44 + i
  getpal16 pal16(), 44 + i, es(i, 54), 1 + es(i, 55), es(i, 53)
  .vis = 1
  .d = 0
  .dissolve = 0
  .flee = 0
  .deathtype = es(i, 22)
  .deathtime = es(i, 23)
  IF es(i, 55) = 0 THEN
   .w = 34
   .h = 34
  END IF
  IF es(i, 55) = 1 THEN
   .w = 50
   .h = 50
  END IF
  IF es(i, 55) = 2 THEN
   .w = 80
   .h = 80
  END IF
  .death_unneeded = readbit(ebits(), i * 5, 62)
  .hero_untargetable = readbit(ebits(), i * 5, 61)
  .enemy_untargetable = readbit(ebits(), i * 5, 60)
  .death_sfx = es(i, 24)
 END WITH
END IF
IF bslot(4 + i).vis = 1 THEN
 setpicstuf buffer(), (bslot(4 + i).w * bslot(4 + i).h) * .5, 3
 IF es(i, 55) = 0 THEN ext$ = ".pt1"
 IF es(i, 55) = 1 THEN ext$ = ".pt2"
 IF es(i, 55) = 2 THEN ext$ = ".pt3"
 loadset game + ext$, es(i, 53), 64 + i * 10
 with bslot(4 + i)
  .sprite_num = 1
  .sprites = sprite_load(game + ext$, es(i, 53), 1, .w, .h)
  if not sprite_is_valid(.sprites) then debug "Failed to load enemy sprite (#" & i & ")"
  .pal = palette16_load(game + ".pal", es(i, 54), 1 + es(i, 55), es(i, 53))
  if .pal = 0 then debug "Failed to load palette (#" & (4+i) & ")"
  
 end with
 FOR o = 0 TO 11
  bstat(4 + i).cur.sta(o) = es(i, 62 + o)
  bstat(4 + i).max.sta(o) = es(i, 62 + o)
 NEXT o
 FOR o = 0 TO 4
  bits(4 + i, o) = es(i, 74 + o)
 NEXT o
 batname$(4 + i) = ""
 FOR o = 1 TO es(i, 0)
  batname$(4 + i) = batname$(4 + i) + CHR$(es(i, o))
 NEXT o
ELSE
 bslot(4 + i).sprites = 0
END IF
END SUB

FUNCTION randomally (who)
IF is_hero(who) THEN
 randomally = INT(RND * 4)
ELSE
 randomally = 4 + INT(RND * 8)
END IF
END FUNCTION

FUNCTION randomfoe (who)
IF is_enemy(who) THEN
 randomfoe = INT(RND * 4)
ELSE
 randomfoe = 4 + INT(RND * 8)
END IF
END FUNCTION

SUB retreat (who, atk(), bslot() AS BattleSprite, t())

IF is_enemy(who) THEN
 IF atk(14) = 2 OR atk(14) = 5 THEN
  anim_setz who, 0
  anim_absmove who, bslot(who).x, bslot(who).y, 6, 6
  anim_waitforall
 END IF
END IF

IF is_hero(who) THEN
 IF atk(14) < 2 THEN ' strike, cast
  anim_walktoggle who
  anim_setmove who, 5, 0, 4, 0
  anim_waitforall
  anim_setframe who, 0
 END IF
 IF atk(14) = 2 OR atk(14) = 5 THEN ' dash, land
  anim_setframe who, 0
  anim_walktoggle who
  anim_setz who, 0
  anim_absmove who, bslot(who).x, bslot(who).y, 6, 6
  anim_waitforall
  anim_setframe who, 0
 END IF
 IF atk(14) = 7 THEN
  anim_setframe who, 0
 END IF
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
battlecaption = cap$
battlecaptime = captime
battlecapdelay = capdelay
END SUB

SUB smartarrowmask (inrange(), pt, d, axis, bslot() AS BattleSprite, tmask())
FOR i = 0 TO 11
 IF tmask(i) THEN
  IF axis THEN
   distance = (bslot(i).y - bslot(pt).y) * d
  ELSE
   distance = (bslot(i).x - bslot(pt).x) * d
  END IF
  IF distance > 0 THEN
   setbit inrange(), 0, i, 1
  END IF
 END IF
NEXT i
END SUB

SUB smartarrows (pt, d, axis, bslot() AS BattleSprite, targ(), tmask(), spred)
DIM inrange(0)
inrange(0) = 0
smartarrowmask inrange(), pt, d, axis, bslot(), tmask()
IF inrange(0) THEN
 best = 999
 newptr = pt
 FOR i = 0 TO 11
  IF readbit(inrange(), 0, i) THEN
   IF axis THEN
    distance = (bslot(i).y - bslot(pt).y) * d
   ELSE
    distance = (bslot(i).x - bslot(pt).y) * d
   END IF
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

FUNCTION targetable (attacker, target, ebits(), bslot() AS BattleSprite)
'this function is orphaned and probably too inaccurate to be useful
targetable = 0
IF is_hero(target) THEN
 'target is hero
 targetable = 1
ELSE
 'target is enemy
 IF is_hero(attacker) THEN
  IF bslot(target).hero_untargetable = 0 THEN targetable = 1
 END IF
 IF is_enemy(attacker) THEN
  IF bslot(target).enemy_untargetable = 0 THEN targetable = 1
 END IF
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
textcolor uilook(uiText), uilook(uiOutline)
s$ = s$ + STRING$(40 - LEN(s$), " ")
printstr s$, 0, 191, 0
printstr s$, 0, 191, 1
END SUB

FUNCTION trytheft (who, targ, atk(), es())
trytheft = 0'--return false by default
IF is_hero(who) AND is_enemy(targ) THEN
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

SUB updatestatslevelup (i, exstat(), bstat() AS BattleStats, allowforget)
' i = who
' exstat = external stats
' stat = in-battle stats
' allowforget = forget spells if level dropped below requirement

'wipe learnmask for this hero
FOR o = i * 6 TO i * 6 + 5
 learnmask(o) = 0
NEXT

'THIS PART UPDATES STATS FOR A LEVEL UP
IF exstat(i, 1, 12) THEN

 dim her as herodef
 loadherodata @her, hero(i) - 1

 'update stats
 FOR o = 0 TO 11
  n0 = her.Lev0.sta(o)
  n99 = her.Lev99.sta(o)
  exstat(i, 1, o) = exstat(i, 1, o) + (atlevel(exstat(i, 0, 12), n0, n99) - atlevel(exstat(i, 0, 12) - exstat(i, 1, 12), n0, n99))

  'simulate levelup bug
  IF readbit(gen(), 101, 9) = 1 THEN
   FOR j = 0 TO 4
    IF eqstuf(i, j) > 0 THEN
     loaditemdata buffer(), eqstuf(i, j) - 1
     exstat(i, 1, o) = exstat(i, 1, o) + buffer(54 + o) * exstat(i, 1, 12)
    END IF
   NEXT j
  END IF

  'do stat caps
  IF gen(genStatCap + o) > 0 THEN exstat(i, 1, o) = small(exstat(i, 1, o),gen(genStatCap + o))
 NEXT o

 'stat restoration
 IF readbit(gen(), 101, 2) = 0 THEN
  '--HP restoration ON
  exstat(i, 0, 0) = exstat(i, 1, 0) 'set external cur to external max
  bstat(i).cur.hp = exstat(i, 1, 0) 'set in-battle cur to external max
  bstat(i).max.hp = exstat(i, 1, 0) 'set in-battle max to external max
 END IF
 IF readbit(gen(), 101, 3) = 0 THEN
  '--MP restoration ON
  exstat(i, 0, 1) = exstat(i, 1, 1) 'set external cur to external max
  bstat(i).cur.mp = exstat(i, 1, 1) 'set in-battle cur to external max
  bstat(i).max.mp = exstat(i, 1, 1) 'set in-battle max to external max
  resetlmp i, exstat(i, 0, 12)
 END IF

 'make current stats match max stats
 FOR o = 2 TO 11
  exstat(i, 0, o) = exstat(i, 1, o)
 NEXT o

 'learn spells
 FOR j = 0 TO 3
  FOR o = 0 TO 23
   WITH her.spell_lists(j,o)
    '--if slot is empty and slot accepts a spell and learn-by-level condition is true
    IF spell(i, j, o) = 0 AND .attack > 0 AND .learned - 1 <= exstat(i, 0, 12) AND .learned > 0 THEN
     spell(i, j, o) = .attack
     setbit learnmask(), 0, i * 96 + j * 24 + o, 1
    END IF
    IF allowforget THEN
     '--plotscripts may lower level, forget spells if drop below requirement and know the spell specified
     IF spell(i, j, o) = .attack AND .learned - 1 > exstat(i, 0, 12) THEN
      spell(i, j, o) = 0
     END IF
    END IF
   END WITH
  NEXT o
 NEXT j

END IF

END SUB

SUB giveheroexperience (i, exstat(), exper&)
 'reset levels gained
 exstat(i, 1, 12) = 0
 IF hero(i) > 0 AND exstat(i, 0, 12) < 99 THEN
  exlev(i, 0) = exlev(i, 0) + exper&
  'levelups
  WHILE exlev(i, 0) >= exlev(i, 1) AND exstat(i, 0, 12) < 99
   exlev(i, 0) = exlev(i, 0) - exlev(i, 1)
   exstat(i, 0, 12) = exstat(i, 0, 12) + 1 'current level
   exstat(i, 1, 12) = exstat(i, 1, 12) + 1 'levelup flag
   exlev(i, 1) = exptolevel(exstat(i, 0, 12))
  WEND
 END IF
END SUB

SUB setheroexperience (BYVAL who, BYVAL amount, BYVAL allowforget, exstat(), exlev() AS LONG)
 'unlike giveheroexperience, this can cause delevelling
 DIM dummystats(40) AS BattleStats

 temp = exstat(who, 0, 12)
 total = 0
 FOR i = 0 TO exstat(who, 0, 12) - 1
  total += exptolevel(i)
 NEXT
 IF total > amount THEN
  'losing levels; lvl up from level 0
  exstat(who, 0, 12) = 0
  exlev(who, 1) = exptolevel(0)
  lostlevels = -1
 ELSE
  'set spell learnt bits correctly
  amount -= total
  temp = 0
  lostlevels = 0
 END IF
 exlev(who, 0) = 0
 giveheroexperience who, exstat(), amount
 updatestatslevelup who, exstat(), dummystats(), allowforget
 exstat(who, 1, 12) -= temp
 IF lostlevels THEN
  'didn't learn spells, wipe mask
  FOR i = who * 6 TO who * 6 + 5
   learnmask(i) = 0
  NEXT
 END IF
END SUB

FUNCTION visibleandalive (o, bstat() AS BattleStats, bslot() AS BattleSprite)
visibleandalive = (bslot(o).vis = 1 AND bstat(o).cur.hp > 0)
END FUNCTION

SUB writestats (exstat(), bstat() AS BattleStats)
setpicstuf buffer(), 636, -1
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  '--set out-of-battle HP and MP equal to in-battle HP and MP
  exstat(i, 0, 0) = bstat(i).cur.hp
  exstat(i, 0, 1) = bstat(i).cur.mp
 END IF
NEXT i
END SUB

SUB get_valid_targs(tmask(), who, atkbuf(), bslot() AS BattleSprite, bstat() AS BattleStats, revenge(), revengemask(), targmem())

DIM i AS INTEGER

FOR i = 0 TO 11
 tmask(i) = 0 ' clear list of available targets
NEXT i

SELECT CASE atkbuf(3)

 CASE 0 'foe
  IF is_hero(who) THEN
   FOR i = 4 TO 11: tmask(i) = bslot(i).vis: NEXT i
  ELSEIF is_enemy(who) THEN
   FOR i = 0 TO 3: tmask(i) = bslot(i).vis: NEXT i
  END IF

 CASE 1 'ally
  IF is_hero(who) THEN
   FOR i = 0 TO 3: tmask(i) = bslot(i).vis: NEXT i
  ELSEIF is_enemy(who) THEN
   FOR i = 4 TO 11: tmask(i) = bslot(i).vis: NEXT i
  END IF

 CASE 2 'self
  tmask(who) = 1

 CASE 3 'all
  FOR i = 0 TO 11: tmask(i) = bslot(i).vis: NEXT i

 CASE 4 'ally-including-dead
  IF is_hero(who) THEN
   FOR i = 0 TO 3
    IF hero(i) > 0 THEN tmask(i) = 1
   NEXT i
  ELSEIF is_enemy(who) THEN
   'enemies don't actually support targetting of dead allies
   FOR i = 4 TO 11: tmask(i) = bslot(i).vis: NEXT i
  END IF

 CASE 5 'ally-not-self
  IF is_hero(who) THEN
   FOR i = 0 TO 3: tmask(i) = bslot(i).vis: NEXT i
  ELSEIF is_enemy(who) THEN
   FOR i = 4 TO 11: tmask(i) = bslot(i).vis: NEXT i
  END IF
  tmask(who) = 0

 CASE 6 'revenge-one
  IF revenge(who) >= 0 THEN
   tmask(revenge(who)) = bslot(revenge(who)).vis
  END IF

 CASE 7 'revenge-all
  FOR i = 0 TO 11
   tmask(i) = (readbit(revengemask(), who, i) AND bslot(i).vis)
  NEXT i

 CASE 8 'previous
  FOR i = 0 TO 11
   tmask(i) = (readbit(targmem(), who, i) AND bslot(i).vis)
  NEXT i

 CASE 9 'stored
  FOR i = 0 TO 11
   tmask(i) = (readbit(targmem(), who + 12, i) AND bslot(i).vis)
  NEXT i

 CASE 10 'dead-ally (hero only)
  IF is_hero(who) THEN
   FOR i = 0 TO 3
    IF hero(i) > 0 AND bstat(i).cur.hp = 0 THEN tmask(i) = 1
   NEXT i
  END IF

END SELECT

'enforce attack's disabled enemy target slots
FOR i = 0 TO 7
 IF readbit(atkbuf(), 20, 37 + i) THEN tmask(4 + i) = 0
NEXT i

'enforce attack's disabled hero target slots
FOR i = 0 TO 3
 IF readbit(atkbuf(), 20, 45 + i) THEN tmask(i) = 0
NEXT i

'target:self overrides enemy untargetable bit
IF atkbuf(3) <> 2 THEN
 'enforce untargetability
 FOR i = 0 TO 11
  IF is_hero(who) THEN
   IF bslot(i).hero_untargetable <> 0 THEN tmask(i) = 0
  ELSEIF is_enemy(who) THEN
   IF bslot(i).enemy_untargetable <> 0 THEN tmask(i) = 0
  END IF
 NEXT i
END IF

END SUB

FUNCTION attack_can_hit_dead(who, atkbuf())

SELECT CASE atkbuf(3)
 CASE 4 'ally-including-dead (hero only)
  IF is_hero(who) THEN RETURN YES
 CASE 10 'dead-ally (hero only)
  IF is_hero(who) THEN RETURN YES
END SELECT

RETURN NO
END FUNCTION

SUB autotarget (confirmtarg(), tmask(), who, atkbuf(), bslot() AS BattleSprite, bstat() AS BattleStats)

DIM i AS INTEGER

'flush the targeting space for this attacker
FOR i = 0 TO 11
 confirmtarg(who, i) = -1
NEXT i

targetptr = 0

SELECT CASE atkbuf(4)

 CASE 0, 3: '--focus and random focus
  confirm_auto_focus who, confirmtarg(), tmask(), atkbuf(), bslot(), bstat()

 CASE 1: '--spread attack
  confirm_auto_spread who, confirmtarg(), tmask()

 CASE 2: '-- optional spread
  IF INT(RND * 100) < 33 THEN
   confirm_auto_spread who, confirmtarg(), tmask()
  ELSE
   confirm_auto_focus who, confirmtarg(), tmask(), atkbuf(), bslot(), bstat()
  END IF

 CASE 4: '--first target
  confirm_auto_first who, confirmtarg(), tmask()

END SELECT

END SUB

SUB confirm_auto_spread (who, confirmtarg(), tmask())
 DIM i AS INTEGER
 DIM targetptr AS INTEGER = 0
 FOR i = 0 TO 11
  IF tmask(i) <> 0 THEN
   confirmtarg(who, targetptr) = i
   targetptr = targetptr + 1
  END IF
 NEXT i
END SUB

SUB confirm_auto_focus (who, confirmtarg(), tmask(), atkbuf(), bslot() AS BattleSprite, bstat() AS BattleStats)
 confirmtarg(who, 0) = find_preferred_target(tmask(), who, atkbuf(), bslot(), bstat())
END SUB

SUB confirm_auto_first (who, confirmtarg(), tmask())
 DIM i AS INTEGER
 FOR i = 0 TO 11
  IF tmask(i) <> 0 THEN
   confirmtarg(who, 0) = i
   EXIT SUB
  END IF
 NEXT i
END SUB

FUNCTION find_preferred_target(tmask(), who, atkbuf(), bslot() AS BattleSprite, bstat() AS BattleStats)

DIM i AS INTEGER
DIM best AS INTEGER
DIM search AS INTEGER
DIM found AS INTEGER
DIM prefstat AS INTEGER
 
IF atkbuf(100) = 0 THEN
 'Weak/Strong pref stat defaults to target stat
 prefstat = atkbuf(18)
ELSE
 prefstat = atkbuf(100) - 1
END IF

SELECT CASE atkbuf(19) ' Preferred target type

 CASE 0 '--Default
  IF is_hero(who) THEN
   atkbuf(19) = 1 ' heroes default to first target
  ELSEIF is_enemy(who) THEN
   atkbuf(19) = 4 ' enemies default to a random target
  END IF
  found = find_preferred_target(tmask(), who, atkbuf(), bslot(), bstat())
  atkbuf(19) = 0
  RETURN found

 CASE 1 '--First
  'special handling for heroes using attacks that target all
  IF is_hero(who) AND atkbuf(3) = 3 THEN
   FOR i = 4 to 11
    IF tmask(i) <> 0 THEN RETURN i
   NEXT i
  ELSE ' normal first-target handling
   FOR i = 0 to 11
    IF tmask(i) <> 0 THEN RETURN i
   NEXT i
  END IF

 CASE 2 '--Closest
  best = -1
  found = 200000
  FOR i = 0 TO 11
   IF tmask(i) <> 0 THEN
    search = quick_battle_distance(who, i, bslot())
    IF search < found THEN
     best = i
     found = search
    END IF
   END IF
  NEXT i
  IF best >= 0 THEN RETURN best

 CASE 3 '--Farthest
  best = -1
  found = -1
  FOR i = 0 TO 11
   IF tmask(i) <> 0 THEN
    search = quick_battle_distance(who, i, bslot())
    IF search > found THEN
     best = i
     found = search
    END IF
   END IF
  NEXT i
  IF best >= 0 THEN RETURN best

 CASE 4 '--Random
  search = 0
  DO
   found = INT(RND * 12)
   IF tmask(found) <> 0 THEN RETURN found
   search = search + 1
  LOOP UNTIL search > 999 ' safety

 CASE 5 'Weakest (absolute)
  best = -1
  found = 32767
  FOR i = 0 TO 11
   IF tmask(i) <> 0 THEN
    search = bstat(i).cur.sta(prefstat)
    IF search < found THEN
     best = i
     found = search
    END IF
   END IF
  NEXT i
  IF best >= 0 THEN RETURN best

 CASE 6 'Strongest (absolute)
  best = -1
  found = -1
  FOR i = 0 TO 11
   IF tmask(i) <> 0 THEN
    search = bstat(i).cur.sta(prefstat)
    IF search > found THEN
     best = i
     found = search
    END IF
   END IF
  NEXT i
  IF best >= 0 THEN RETURN best

 CASE 7 'Weakest (percent)
  best = -1
  found = 10001 'use ten-thousands rather than hundreds to simulate two fixed-precision decmal places
  FOR i = 0 TO 11
   IF tmask(i) <> 0 THEN
    search = INT(10000 / bstat(i).max.sta(prefstat) * bstat(i).cur.sta(prefstat))
    IF search < found THEN
     best = i
     found = search
    END IF
   END IF
  NEXT i
  IF best >= 0 THEN RETURN best

 CASE 8 'Strongest (percent)
  best = -1
  found = -1
  FOR i = 0 TO 11
   IF tmask(i) <> 0 THEN
    search = INT(10000 / bstat(i).max.sta(prefstat) * bstat(i).cur.sta(prefstat))
    IF search > found THEN
     best = i
     found = search
    END IF
   END IF
  NEXT i
  IF best >= 0 THEN RETURN best

END SELECT

'-- If all else fails, make sure the default target is valid
FOR i = 0 TO 11
 IF tmask(i) <> 0 THEN RETURN i
NEXT i

' If no valid targets were found, fail with -1
RETURN -1

END FUNCTION

FUNCTION quick_battle_distance(who1, who2, bslot() AS BattleSprite)
 ' For speed, this function only implements exponent part of the pythagorean theorum
 ' and not the square root part of it, making the results only directly useful for
 ' quick comparisons of distance
 DIM AS INTEGER distx, disty
 distx = ABS((bslot(who1).x + bslot(who1).w \ 2) - (bslot(who2).x + bslot(who2).w \ 2))
 disty = ABS((bslot(who1).y + bslot(who1).h) - (bslot(who2).y + bslot(who2).h))
 RETURN distx ^ 2 + disty ^ 2
END FUNCTION

FUNCTION battle_distance(who1, who2, bslot() AS BattleSprite)
 'Returns exact distance between two battlesprites
 ' Square root is a bit slow, so don't over-use this function
 RETURN SQR(quick_battle_distance(who1, who2, bslot()))
END FUNCTION
