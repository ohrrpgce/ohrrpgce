'OHRRPGCE GAME - Additional mostly battle-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'!$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB exitprogram (needfade%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB calibrate ()
DECLARE SUB getitem (getit%, num%)
DECLARE SUB resetlmp (slot%, lev%)
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE SUB snapshot ()
DECLARE SUB delitem (it%, num%)
DECLARE FUNCTION countitem% (it%)


#include "bmod.bi"
#include "bmodsubs.bi"
#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "gglobals.bi"
#include "const.bi"
#include "uiconst.bi"
#include "udts.bi"
#INCLUDE "battle_udts.bi"

DECLARE SUB confirm_auto_spread (who as integer, tmask() as integer, bslot() AS BattleSprite)
DECLARE SUB confirm_auto_focus (who as integer, tmask() as integer, atk as AttackData, bslot() AS BattleSprite)
DECLARE SUB confirm_auto_first (who as integer, tmask() as integer, bslot() AS BattleSprite)

DECLARE FUNCTION quick_battle_distance(who1 as integer, who2 as integer, bslot() AS BattleSprite)
DECLARE FUNCTION battle_distance(who1 as integer, who2 as integer, bslot() AS BattleSprite)

REM $STATIC
FUNCTION is_hero(who as integer) as integer
 IF who >= 0 AND who <= 3 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_enemy(who as integer) as integer
 IF who >= 4 AND who <= 11 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_attack(who as integer) as integer
 IF who >= 12 AND who <= 23 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_weapon(who as integer) as integer
 IF who = 24 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION atkallowed (atk as AttackData, attacker as integer, spclass as integer, lmplev as integer, bslot() AS BattleSprite) as integer
'--atk   = attack data
'--attacker = hero or enemy who is attacking
'--spclass  = 0 for normal attacks, 1 for level-MP spells
'--lmplev   = which level-MP level to use

'--check for mutedness
IF atk.mutable AND bslot(attacker).stat.cur.mute < bslot(attacker).stat.max.mute THEN
 RETURN NO
END IF

'--check for sufficient mp
IF bslot(attacker).stat.cur.mp - focuscost(atk.mp_cost, bslot(attacker).stat.cur.foc) < 0 THEN
 RETURN NO
END IF

'--check for level-MP (heroes only)
IF attacker <= 3 AND spclass = 1 THEN
 IF lmp(attacker, lmplev) - 1 < 0 THEN
  RETURN NO
 END IF
END IF

'--check for sufficient items
DIM itemid, itemcount AS INTEGER
FOR i = 0 to 2
  itemid = atk.item(i).id
  itemcount = atk.item(i).number
  IF itemid > 0 THEN 'this slot is used
    IF attacker <= 3 THEN ' Only hero items are checked right now
      IF countitem(itemid) < itemcount THEN
        'yes, this still works for adding items.
        RETURN NO
      END IF
    END IF
  END IF
NEXT i

'--succeed
RETURN YES

END FUNCTION 'stat

FUNCTION checktheftchance (item as integer, itemP as integer, rareitem as integer, rareitemP as integer) as integer
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

'  CARRAY() - seealso cc* constants in const.bi
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
IF keyval(-1) THEN
 DestroyGameSlices YES
 exitprogram 0
END IF

'alt-enter toggle windowed
if keyval(scAlt) > 0 and keyval(scEnter) > 0 then
	togglewindowed
end if

FOR i = 0 TO 7: carray(i) = 0: NEXT i

IF keyval(scF12) > 0 THEN snapshot

IF keyval(scNumlock) = 0 THEN ' no controls while PAUSE is pressed, because of its scancode wierdness
 GOSUB keyboard
 GOSUB joystick
END IF
EXIT SUB

keyboard:
FOR i = 0 TO 3
 carray(i) = keyval(csetup(i))
NEXT i
carray(ccUse) = keyval(csetup(4)) OR keyval(csetup(5)) OR keyval(csetup(6))
carray(ccMenu) = keyval(csetup(7)) OR keyval(csetup(8))
carray(ccRun) = keyval(csetup(9)) OR keyval(csetup(10))
'--gen(genJoy) is the calibration disabler flag
IF gen(genJoy) = 0 AND keyval(scCtrl) > 0 AND keyval(csetup(11)) > 1 THEN
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

if gen(genJoy) = 0 then retrace

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
  carray(ccUse) = 2
  joyuse = 3
 CASE 3
  carray(ccUse) = 1
  joyuse = 0
END SELECT
SELECT CASE joymenu
 CASE 0
  IF joy(joy(14)) = 0 THEN joymenu = 1
 CASE 1
  carray(ccRun) = 2
  IF joy(joy(14)) <> 0 THEN joymenu = 2
 CASE 2
  carray(ccMenu) = 2
  joymenu = 3
 CASE 3
  carray(ccMenu) = 1
  joymenu = 0
END SELECT

FOR i = 0 TO 3
 carray( + i) = carray(i)
NEXT i
RETRACE

END SUB

FUNCTION countai (ai as integer, them as integer, es() as integer) as integer
o = 0
FOR i = 0 TO 4
 IF es(them - 4, 92 + (ai * 5) + i) > 0 THEN o = o + 1
NEXT i
countai = o
END FUNCTION

FUNCTION enemycount (bslot() AS BattleSprite) as integer
o = 0
FOR i = 4 TO 11
 IF bslot(i).stat.cur.hp > 0 THEN o = o + 1
NEXT i
RETURN o
END FUNCTION

Function GetWeaponPos(w as integer,f as integer,isY as integer) as integer'or x?
 'FIXME: Ack! Lets just make handle position a member of bslot()
 dim fh
 IF w >= 0 THEN
  fh = FREEFILE
  OPEN game + ".itm" FOR BINARY AS #fh
  'debug "weapon" + XSTR$(w) + " offset: " + XSTR$(w * 200 + 157 + f * 4 + isY * 2)
  GetWeaponPos = ReadShort(fh,w * 200 + 157 + f * 4 + iif(isY,1,0) * 2)
  CLOSE #FH
 END IF
End Function

Function GetHeroPos(h as integer,f as integer,isY as integer) as integer'or x?
 'FIXME: Ack! Lets just make hand position a member of bslot()
 dim fh
 fh = FREEFILE
 OPEN game + ".dt0" FOR BINARY AS #fh
 'debug "hero offset: " + XSTR$(h * 636 + 595 + f * 4 + isY * 2)
 GetHeroPos = ReadShort(fh,h * 636 + 595 + f * 4 + iif(isY,1,0) * 2)
 CLOSE #FH
End Function

FUNCTION inflict (w as integer, t as integer, bslot() AS BattleSprite, attack as AttackData, tcount as integer) as integer

DIM h = 0

'failure by default
inflict = 0
bslot(w).attack_succeeded = 0

'remember this target
bslot(w).last_targs(t) = YES

'stored targs
IF attack.store_targ THEN bslot(w).stored_targs(t) = YES
IF attack.delete_stored_targ THEN
 FOR i = 0 TO 11
  bslot(w).stored_targs(i) = NO
 NEXT i
END IF

'no damage
IF attack.damage_math <> 4 THEN

 'init
 cure = 0
 WITH bslot(t)
  .harm.text = ""
  .harm.ticks = gen(genDamageDisplayTicks)
  .harm.pos.x = .x + (.w * .5)
  .harm.pos.y = .y + (.h * .5)
  
  targstat = bound(attack.targ_stat, 0, UBOUND(.stat.cur.sta))
 END WITH

 'accuracy
 a = bslot(w).stat.cur.acc
 d = bslot(t).stat.cur.dog
 dm! = .25
 IF attack.aim_math = 1 THEN dm! = .5
 IF attack.aim_math = 2 THEN dm! = 1
 IF attack.aim_math = 4 THEN dm! = 1.25
 IF attack.aim_math = 4 OR attack.aim_math = 7 OR attack.aim_math = 8 THEN
  a = bslot(w).stat.cur.mag
  d = bslot(t).stat.cur.wil
 END IF

 attackhit = range(a, 75) >= range(d * dm!, 75)
 IF attack.aim_math = 3 THEN attackhit = 1
 IF attack.aim_math = 5 OR attack.aim_math = 7 THEN attackhit = RND * 100 < (a * (100 - d)) / 100 
 IF attack.aim_math = 6 OR attack.aim_math = 8 THEN attackhit = RND * 100 < a
 IF attackhit = 0 THEN
  bslot(t).harm.text = readglobalstring$(120, "miss", 20)
  EXIT FUNCTION
 END IF

 WITH bslot(t)
  IF attack.fail_if_targ_poison = YES AND .stat.cur.poison < .stat.max.poison THEN
   .harm.text = readglobalstring$(122, "fail", 20)
   EXIT FUNCTION
  END IF
  IF attack.fail_if_targ_regen = YES AND .stat.cur.regen < .stat.max.regen THEN
   .harm.text = readglobalstring$(122, "fail", 20)
   EXIT FUNCTION
  END IF
  IF attack.fail_if_targ_stun = YES AND .stat.cur.stun <> .stat.max.stun THEN
   .harm.text = readglobalstring$(122, "fail", 20)
   EXIT FUNCTION
  END IF
  IF attack.fail_if_targ_mute = YES AND .stat.cur.mute <> .stat.max.mute THEN
   .harm.text = readglobalstring$(122, "fail", 20)
   EXIT FUNCTION
  END IF
 END WITH

 'attack and defense base
 a = bslot(w).stat.cur.str
 d = bslot(t).stat.cur.def
 SELECT CASE attack.base_atk_stat
  CASE 1
   a = bslot(w).stat.cur.mag
   d = bslot(t).stat.cur.wil
  CASE 2
   a = bslot(w).stat.cur.hp
  CASE 3
   a = bslot(w).stat.max.hp - bslot(w).stat.cur.hp
  CASE 4
   a = INT(RND * 999)
  CASE 5
   a = 100
  CASE 6 TO 17
   a = bslot(w).stat.cur.sta(attack.base_atk_stat - 6)
  CASE 18
   a = bslot(w).repeatharm
  CASE 19
   a = bslot(w).revengeharm
  CASE 20
   a = bslot(t).revengeharm
  CASE 21
   a = bslot(w).thankvengecure
  CASE 22
   a = bslot(t).thankvengecure
 END SELECT

 '--defense base
 IF attack.base_def_stat > 0 AND attack.base_def_stat <= UBOUND(bslot(t).stat.cur.sta) + 1 THEN d = bslot(t).stat.cur.sta(attack.base_def_stat - 1)

 'calc defense
 am! = 1: dm! = .5                    'atk-def*.5
 IF attack.damage_math = 1 THEN am! = .8: dm! = .1 'atk*.8-def*.5
 IF attack.damage_math = 2 THEN am! = 1.3: dm! = 1 'atk-1.3-def
 IF attack.damage_math = 3 THEN am! = 1: dm! = 0   'atk

 'resetting
 IF attack.reset_targ_stat_before_hit = YES THEN
  bslot(t).stat.cur.sta(targstat) = bslot(t).stat.max.sta(targstat)
 END IF

 'calc harm
 h = (a * am!) - (d * dm!)

 'elementals
 FOR i = 0 TO 7
  IF attack.elemental_damage(i) = YES THEN
   IF bslot(t).weak(i) = YES THEN h = h * 2   'weakness
   IF bslot(t).strong(i) = YES THEN h = h * .12 'resistance
   IF bslot(t).absorb(i) = YES THEN cure = 1    'absorb
  END IF
  IF attack.monster_type_bonus(i) = YES THEN
   IF is_enemy(t) AND bslot(t).enemytype(i) = YES THEN h = h * 1.8
  END IF
  IF attack.fail_vs_elemental(i) = YES THEN
   IF bslot(t).strong(i) = YES THEN
    bslot(t).harm.text = readglobalstring$(122, "fail", 20)
    EXIT FUNCTION
   END IF
  END IF
  IF attack.fail_vs_monster_type(i) = YES THEN
   IF is_enemy(t) AND bslot(t).enemytype(i) = YES THEN
    bslot(t).harm.text = readglobalstring$(122, "fail", 20)
    EXIT FUNCTION
   END IF
  END IF
 NEXT i

 'extra damage
 h = h + (h / 100) * attack.extra_damage

 'randomize
 IF attack.do_not_randomize = NO THEN h = range(h,20)

 'spread damage
 IF attack.divide_spread_damage = YES THEN h = h / (tcount + 1)

 'cap out
 IF h <= 0 THEN
  IF attack.damage_can_be_zero = NO THEN h = 1 ELSE h = 0
 END IF

 'backcompat MP-targstat
 IF attack.obsolete_damage_mp THEN
  IF targstat = 0 THEN targstat = 1
 END IF

 'remember target stat
 remtargstat = bslot(t).stat.cur.sta(targstat)
 rematkrstat = bslot(w).stat.cur.sta(targstat)

 'pre-calculate percentage damage for display
 chp = bslot(t).stat.cur.sta(targstat)
 mhp = bslot(t).stat.max.sta(targstat)
 IF attack.percent_damage_not_set = YES THEN
  'percentage attacks do damage
  'FIXME: see bug 134 about moving this block up the function. This should be base damage?
  SELECT CASE attack.damage_math
   CASE 5'% of max
    h = mhp + (attack.extra_damage * mhp / 100)
    cure = 0
   CASE 6'% of cur
    h = chp + (attack.extra_damage * chp / 100)
    cure = 0
  END SELECT
 END IF

 IF attack.cure_instead_of_harm = YES THEN h = ABS(h) * -1 'cure bit
 IF bslot(t).harmed_by_cure = YES THEN h = ABS(h)  'zombie
 IF cure = 1 THEN h = ABS(h) * -1                  'elemental absorb

 IF attack.percent_damage_not_set = NO THEN
  'percentage attacks set stat
  'and by set, we really mean set, ignore nearly all attack settings,
  'that's my interpretation of intent anyway - TMC
  '...And mine to. - James
  SELECT CASE attack.damage_math
   CASE 5'% of max
    h = chp - (mhp + (attack.extra_damage * mhp / 100))
   CASE 6'% of cur
    h = chp - (chp + (attack.extra_damage * chp / 100))
  END SELECT
 END IF

 'inflict
 IF attack.show_damage_without_inflicting = NO THEN
  IF gen(genDamageCap) > 0 THEN
   IF h > gen(genDamageCap) THEN h = gen(genDamageCap)
   IF h < -gen(genDamageCap) THEN h = -gen(genDamageCap)
  END IF

  bslot(t).stat.cur.sta(targstat) = safesubtract(bslot(t).stat.cur.sta(targstat), h)
  IF attack.absorb_damage THEN
   WITH bslot(w)
    '--drain
    IF attack.do_not_display_damage = NO THEN
     .harm.text = STR(ABS(h))
     IF h > 0 THEN .harm.text = "+" + .harm.text
    END IF
    .harm.ticks = gen(genDamageDisplayTicks)
    .harm.col = 12 'FIXME: pink
    .harm.pos.x = .x + (.w * .5)
    .harm.pos.y = .y + (.h * .5)
    .stat.cur.sta(targstat) += h
   END WITH
  END IF
 END IF

 'enforce bounds
 bslot(t).stat.cur.sta(targstat) = large(bslot(t).stat.cur.sta(targstat), 0)
 bslot(w).stat.cur.sta(targstat) = large(bslot(w).stat.cur.sta(targstat), 0)
 IF attack.allow_cure_to_exceed_maximum = NO THEN
  bslot(t).stat.cur.sta(targstat) = small(bslot(t).stat.cur.sta(targstat), large(bslot(t).stat.max.sta(targstat), remtargstat))
  bslot(w).stat.cur.sta(targstat) = small(bslot(w).stat.cur.sta(targstat), large(bslot(w).stat.max.sta(targstat), rematkrstat))
 END IF

 'set damage display
 IF attack.do_not_display_damage = NO THEN
  bslot(t).harm.text = STR(ABS(h))
  '--if cure, show + sign
  IF h < 0 THEN bslot(t).harm.text = "+" + bslot(t).harm.text
 END IF

 'remember revenge data
 IF remtargstat > bslot(t).stat.cur.sta(targstat) THEN
  bslot(t).revengemask(w) = YES
  bslot(t).revenge = w
  bslot(t).revengeharm = remtargstat - bslot(t).stat.cur.sta(targstat)
  bslot(w).repeatharm = remtargstat - bslot(t).stat.cur.sta(targstat)
 END IF

 'remember thankvenge data
 IF remtargstat < bslot(t).stat.cur.sta(targstat) THEN
  bslot(t).thankvengemask(w) = YES
  bslot(t).thankvenge = w
  bslot(t).thankvengecure = ABS(remtargstat - bslot(t).stat.cur.sta(targstat))
 END IF

END IF 'skips to here if no damage

IF attack.show_name = YES THEN
 IF LEN(bslot(t).harm.text) > 0 THEN bslot(t).harm.text += " "
 bslot(t).harm.text += attack.name
END IF

'reset registers as per convenience bits
IF attack.reset_poison = YES THEN bslot(t).stat.cur.poison = bslot(t).stat.max.poison
IF attack.reset_regen = YES  THEN bslot(t).stat.cur.regen  = bslot(t).stat.max.regen
IF attack.reset_stun = YES   THEN bslot(t).stat.cur.stun   = bslot(t).stat.max.stun
IF attack.reset_mute = YES   THEN bslot(t).stat.cur.mute   = bslot(t).stat.max.mute

'--success!
inflict = 1
bslot(w).attack_succeeded = 1

END FUNCTION

FUNCTION liveherocount (bslot() AS BattleSprite) as integer
i = 0
FOR o = 0 TO 3
 IF hero(o) > 0 AND bslot(o).stat.cur.hp > 0 THEN i = i + 1
NEXT o
liveherocount = i
END FUNCTION

FUNCTION liveherocount (oobstat() AS integer) as integer
i = 0
FOR o = 0 TO 3
 IF hero(o) > 0 AND oobstat(o, 0, statHP) > 0 THEN i = i + 1
NEXT o
liveherocount = i
END FUNCTION

SUB loadfoe (i as integer, formdata() as integer, es() as integer, BYREF bat AS BattleState, bslot() AS BattleSprite, BYREF rew AS RewardsState, allow_dead as integer = NO)
DIM tempbits(4) AS INTEGER ' This is a hack because readbit doesn't work on double-index arrays
IF formdata(i * 4) > 0 THEN
 loadenemydata buffer(), formdata(i * 4) - 1, -1
 FOR o = 0 TO 160
  es(i, o) = buffer(o)
 NEXT o
 
 'Copy elemental bits and other bits from es() to bslot()
 FOR o = 0 TO 4
  tempbits(o) = es(i, 74 + o)
 NEXT o
 WITH bslot(4 + i)
  .harmed_by_cure = xreadbit(tempbits(), 54)
  .mp_idiot = xreadbit(tempbits(), 55)
  .is_boss = xreadbit(tempbits(), 56)
  .unescapable = xreadbit(tempbits(), 57)
  .die_without_boss = xreadbit(tempbits(), 58)
  .flee_instead_of_die = xreadbit(tempbits(), 59)
  .enemy_untargetable = xreadbit(tempbits(), 60)
  .hero_untargetable = xreadbit(tempbits(), 61)
  .death_unneeded = xreadbit(tempbits(), 62)
  .never_flinch = xreadbit(tempbits(), 63)
  .ignore_for_alone = xreadbit(tempbits(), 64)
  FOR o = 0 TO 7
   .weak(o) = xreadbit(tempbits(), o)
   .strong(o) = xreadbit(tempbits(), 8 + o)
   .absorb(o) = xreadbit(tempbits(), 16 + o)
   .enemytype(o) = xreadbit(tempbits(), 24 + o)
  NEXT o
 END WITH
 
 IF allow_dead = NO THEN
  'enemies which spawn already-dead should be killed off immediately
  'die without boss or 0 hp?
  IF dieWOboss(4 + i, bslot()) OR es(i, 62) <= 0 THEN
   'rewards and spawn enemies on death
   'enemy is only partially constructed, but already have everything needed.
   DIM atktype(8) 'regular "spawn on death"
   dead_enemy 4 + i, -1, bat, rew, bslot(), es(), formdata()
   EXIT SUB
  END IF
 END IF
 WITH bslot(4 + i)
  .basex = formdata(i * 4 + 1)
  .basey = formdata(i * 4 + 2)
  .x = bslot(4 + i).basex
  .y = bslot(4 + i).basey
  .vis = 1
  .d = 0
  .dissolve = 0
  .flee = 0
  .deathtype = es(i, 22)
  .deathtime = es(i, 23)
  .w = sprite_sizes(1 + es(i, 55)).size.x
  .h = sprite_sizes(1 + es(i, 55)).size.y
  .cursorpos.x = .w / 2 - es(i, 25) '--X offset is subtracted instead of added because enemies are always h-flipped
  .cursorpos.y = es(i, 26)
  .death_sfx = es(i, 24)
  .revenge = -1
  .thankvenge = -1
  FOR o = 0 TO 11
   .revengemask(o) = NO
   .last_targs(o) = NO
   .stored_targs(o) = NO
   .thankvengemask(o) = NO
  NEXT o
 END WITH
END IF
IF bslot(4 + i).vis = 1 THEN
 with bslot(4 + i)
  .sprite_num = 1
  .sprites = sprite_load(1 + es(i, 55), es(i, 53))
  .pal = palette16_load(es(i, 54), 1 + es(i, 55), es(i, 53))
 end with
 FOR o = 0 TO 11
  bslot(4 + i).stat.cur.sta(o) = es(i, 62 + o)
  bslot(4 + i).stat.max.sta(o) = es(i, 62 + o)
 NEXT o
 'can't use readbadbinstring because es is 2D
 bslot(4 + i).name = ""
 FOR o = 1 TO es(i, 0)
  bslot(4 + i).name = bslot(4 + i).name + CHR$(es(i, o))
 NEXT o
ELSE
 bslot(4 + i).sprites = 0
END IF
END SUB

FUNCTION randomally (who as integer) as integer
IF is_hero(who) THEN
 randomally = INT(RND * 4)
ELSE
 randomally = 4 + INT(RND * 8)
END IF
END FUNCTION

FUNCTION randomfoe (who as integer) as integer
IF is_enemy(who) THEN
 randomfoe = INT(RND * 4)
ELSE
 randomfoe = 4 + INT(RND * 8)
END IF
END FUNCTION

FUNCTION safesubtract (number as integer, minus as integer) as integer
longnumber& = number
longminus& = minus
longresult& = longnumber& - longminus&
IF longresult& > 32767 THEN longresult& = 32767
IF longresult& < -32768 THEN longresult& = -32768
result = longresult&
safesubtract = result
END FUNCTION

FUNCTION safemultiply (number as integer, by as single) as integer
 dim as integer longnumber = number
 dim as single longby = by
 dim as integer longresult = longnumber * longby
 IF longresult > 32767 THEN longresult = 32767
 IF longresult < -32768 THEN longresult = -32768
 result = longresult
 return result
END FUNCTION

SUB setbatcap (BYREF bat AS BattleState, cap as string, captime as integer, capdelay as integer)
 bat.caption = cap
 bat.caption_time = captime
 bat.caption_delay = capdelay
END SUB

SUB smartarrowmask (inrange() as integer, d as integer, axis as integer, bslot() AS BattleSprite, targ AS TargettingState)
FOR i = 0 TO 11
 IF targ.mask(i) THEN
  IF axis THEN
   distance = (bslot(i).y - bslot(targ.pointer).y) * d
  ELSE
   distance = (bslot(i).x - bslot(targ.pointer).x) * d
  END IF
  IF distance > 0 THEN
   setbit inrange(), 0, i, 1
  END IF
 END IF
NEXT i
END SUB

SUB smartarrows (d as integer, axis as integer, bslot() AS BattleSprite, BYREF targ AS TargettingState, allow_spread as integer=0)
DIM inrange(0)
inrange(0) = 0
smartarrowmask inrange(), d, axis, bslot(), targ
IF inrange(0) THEN
 best = 999
 newptr = targ.pointer
 FOR i = 0 TO 11
  IF readbit(inrange(), 0, i) THEN
   IF axis THEN
    distance = (bslot(i).y - bslot(targ.pointer).y) * d
   ELSE
    distance = (bslot(i).x - bslot(targ.pointer).y) * d
   END IF
   IF distance < best THEN
    best = distance
    newptr = i
   END IF
  END IF
 NEXT i
 targ.pointer = newptr
ELSE
 IF allow_spread = YES AND targ.opt_spread = 1 THEN
  FOR i = 0 TO 11
   targ.selected(i) = targ.mask(i)
  NEXT i
  targ.opt_spread = 2
 END IF
END IF
END SUB

FUNCTION targetable (attacker as integer, target as integer, bslot() AS BattleSprite) as integer
'this function is orphaned and probably too inaccurate to be useful
targetable = 0
IF is_hero(target) THEN
 'target is hero
 targetable = 1
ELSE
 'target is enemy
 IF is_hero(attacker) THEN
  IF bslot(target).hero_untargetable = NO THEN targetable = 1
 END IF
 IF is_enemy(attacker) THEN
  IF bslot(target).enemy_untargetable = NO THEN targetable = 1
 END IF
END IF
END FUNCTION

FUNCTION targetmaskcount (tmask() as integer) as integer
n = 0
FOR i = 0 TO 11
 IF tmask(i) THEN n = n + 1
NEXT i
targetmaskcount = n
END FUNCTION

SUB traceshow (s as string)
textcolor uilook(uiText), uilook(uiOutline)
s$ = s$ + STRING$(40 - LEN(s$), " ")
printstr s$, 0, 191, 0
printstr s$, 0, 191, 1
END SUB

FUNCTION trytheft (BYREF bat AS BattleState, who as integer, targ as integer, attack as AttackData, es() as integer) as integer
trytheft = 0'--return false by default
IF is_hero(who) AND is_enemy(targ) THEN
 '--a hero is attacking an enemy
 IF attack.can_steal_item THEN
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
    setbatcap bat, readglobalstring$(117, "Stole", 40) + " " + readitemname$(stole - 1), 40, 0
    trytheft = -1'--return success
   ELSE
    '--steal failed
    setbatcap bat, readglobalstring$(114, "Cannot Steal", 40) + " ", 40, 0
   END IF
  ELSE
   '--has nothing to steal / steal disabled
   setbatcap bat, readglobalstring$(111, "Has Nothing", 30), 40, 0
  END IF
 END IF
END IF
END FUNCTION

FUNCTION exptolevel (level as integer) as integer
' cp needed to level: calling with level 0 returns xp to lvl 1
' HINT: Customisation goes here :)

 dim exper as integer = 30
 FOR o as integer = 1 TO level
  exper = exper * 1.2 + 5
  IF exper > 1000000 THEN exper = 1000000
 NEXT
 return exper
END FUNCTION

SUB updatestatslevelup (i as integer, exstat() as integer, stats AS BattleStats, allowforget as integer)
' i = who
' exstat = external stats
' stats = in-battle stats
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
  exstat(i, 0, statHP) = exstat(i, 1, statHP) 'set external cur to external max
  stats.cur.hp = exstat(i, 1, statHP) 'set in-battle cur to external max
  stats.max.hp = exstat(i, 1, statHP) 'set in-battle max to external max
 END IF
 IF readbit(gen(), 101, 3) = 0 THEN
  '--MP restoration ON
  exstat(i, 0, statMP) = exstat(i, 1, statMP) 'set external cur to external max
  stats.cur.mp = exstat(i, 1, statMP) 'set in-battle cur to external max
  stats.max.mp = exstat(i, 1, statMP) 'set in-battle max to external max
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

SUB giveheroexperience (i as integer, exstat() as integer, exper as integer)
 'reset levels gained
 exstat(i, 1, 12) = 0
 IF hero(i) > 0 AND exstat(i, 0, 12) < 99 THEN
  exlev(i, 0) = exlev(i, 0) + exper
  'levelups
  WHILE exlev(i, 0) >= exlev(i, 1) AND exstat(i, 0, 12) < 99
   exlev(i, 0) = exlev(i, 0) - exlev(i, 1)
   exstat(i, 0, 12) = exstat(i, 0, 12) + 1 'current level
   exstat(i, 1, 12) = exstat(i, 1, 12) + 1 'levelup flag
   exlev(i, 1) = exptolevel(exstat(i, 0, 12))
  WEND
 END IF
END SUB

SUB setheroexperience (BYVAL who as integer, BYVAL amount as integer, BYVAL allowforget as integer, exstat() as integer, exlev() as integer)
 'unlike giveheroexperience, this can cause delevelling
 DIM dummystats AS BattleStats

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
 updatestatslevelup who, exstat(), dummystats, allowforget
 exstat(who, 1, 12) -= temp
 IF lostlevels THEN
  'didn't learn spells, wipe mask
  FOR i = who * 6 TO who * 6 + 5
   learnmask(i) = 0
  NEXT
 END IF
END SUB

FUNCTION visibleandalive (o as integer, bslot() AS BattleSprite) as integer
visibleandalive = (bslot(o).vis = 1 AND bslot(o).stat.cur.hp > 0)
END FUNCTION

SUB writestats (exstat() as integer, bslot() AS BattleSprite)
setpicstuf buffer(), 636, -1
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  '--set out-of-battle HP and MP equal to in-battle HP and MP
  exstat(i, 0, statHP) = bslot(i).stat.cur.hp
  exstat(i, 0, statMP) = bslot(i).stat.cur.mp
 END IF
NEXT i
END SUB

SUB get_valid_targs(tmask(), who, BYREF atk AS AttackData, bslot() AS BattleSprite)

 DIM i AS INTEGER

 FOR i = 0 TO 11
  tmask(i) = 0 ' clear list of available targets
 NEXT i

 SELECT CASE atk.targ_class

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
  IF bslot(who).revenge >= 0 THEN
   tmask(bslot(who).revenge) = bslot(bslot(who).revenge).vis
  END IF

 CASE 7 'revenge-all
  FOR i = 0 TO 11
   IF bslot(who).revengemask(i) = YES AND bslot(i).vis <> 0 THEN
    tmask(i) = 1
   END IF
  NEXT i

 CASE 8 'previous
  FOR i = 0 TO 11
   IF bslot(who).last_targs(i) = YES AND bslot(i).vis <> 0 THEN
    tmask(i) = 1
   END IF
  NEXT i

 CASE 9 'stored
  FOR i = 0 TO 11
   IF bslot(who).stored_targs(i) = YES AND bslot(i).vis <> 0 THEN
    tmask(i) = 1
   END IF
  NEXT i

 CASE 10 'dead-ally (hero only)
  IF is_hero(who) THEN
   FOR i = 0 TO 3
    IF hero(i) > 0 AND bslot(i).stat.cur.hp = 0 THEN tmask(i) = 1
   NEXT i
  END IF

 CASE 11 'thankvenge-one
  IF bslot(who).thankvenge >= 0 THEN
   tmask(bslot(who).thankvenge) = bslot(bslot(who).thankvenge).vis
  END IF

 CASE 12 'thankvenge-all
  FOR i = 0 TO 11
   IF bslot(who).thankvengemask(i) = YES AND bslot(i).vis <> 0 THEN
    tmask(i) = 1
   END IF
  NEXT i

 END SELECT

 'enforce attack's disabled enemy target slots
 FOR i = 0 TO 7
  IF atk.cannot_target_enemy_slot(i) THEN tmask(4 + i) = 0
 NEXT i

 'enforce attack's disabled hero target slots
 FOR i = 0 TO 3
  IF atk.cannot_target_hero_slot(i) THEN tmask(i) = 0
 NEXT i

 'target:self overrides enemy untargetable bit
 IF atk.targ_class <> 2 THEN
  'enforce untargetability
  FOR i = 0 TO 11
   IF is_hero(who) THEN
    IF bslot(i).hero_untargetable = YES THEN tmask(i) = 0
   ELSEIF is_enemy(who) THEN
    IF bslot(i).enemy_untargetable = YES THEN tmask(i) = 0
   END IF
  NEXT i
 END IF

END SUB

OPTION EXPLICIT 'FIXME: move this up as code gets cleaned up

SUB anim_advance (who as integer, attack as AttackData, bslot() AS BattleSprite)

 DIM d AS INTEGER
 d = 1 ' Hero faces left
 IF is_enemy(who) THEN d = -1 ' Enemy faces right

 IF is_hero(who) THEN
  IF attack.attacker_anim < 2 OR (attack.attacker_anim > 2 AND attack.attacker_anim < 5) THEN ' strike, cast, spin, jump
   anim_walktoggle who
   anim_setmove who, -5, 0, 4, 0
   anim_waitforall
  END IF
 END IF
 
 IF attack.attacker_anim = 2 THEN ' Dash in
  DIM yt AS INTEGER
  yt = (bslot(bslot(who).t(0)).h - bslot(who).h) + 2
  anim_walktoggle who
  anim_absmove who, bslot(bslot(who).t(0)).x + bslot(bslot(who).t(0)).w * d, bslot(bslot(who).t(0)).y + yt, 6, 6
  anim_waitforall
 END IF
 
 IF attack.attacker_anim = 8 THEN ' Teleport
  anim_setpos who, bslot(bslot(who).t(0)).x + bslot(bslot(who).t(0)).w * d, bslot(bslot(who).t(0)).y + (bslot(bslot(who).t(0)).h - (bslot(who).h)), 0
 END IF

END SUB

SUB anim_hero (who as integer, attack as AttackData, bslot() AS BattleSprite)
 DIM hx AS INTEGER = 0
 DIM hy AS INTEGER = 0
 DIM wx AS INTEGER = 0
 DIM wy AS INTEGER = 0
 DIM xt AS INTEGER = 0
 DIM yt AS INTEGER = 0
 DIM dx AS INTEGER = 0
 DIM dy AS INTEGER = 0
 
 IF attack.attacker_anim < 3 OR (attack.attacker_anim > 6 AND attack.attacker_anim < 9) THEN ' strike, cast, dash, standing cast, teleport
  anim_setframe who, 0
  anim_wait 3 'wait 3 ticks
  
  IF attack.attacker_anim <> 1 AND attack.attacker_anim <> 7 THEN 'if it's not cast or standing cast
   anim_setframe who, 2
  
   hx = GetHeroPos(hero(who)-1,0,0)
   hy = GetHeroPos(hero(who)-1,0,1)
   wx = GetWeaponPos(eqstuf(who,0)-1,1,0)
   wy = GetWeaponPos(eqstuf(who,0)-1,1,1)
   dx = hx - wx
   dy = hy - wy
  
   anim_align2 24, who, 0, 0, dx, 16
   anim_setz 24, 16 - dy
  
   anim_setframe 24, 0
   anim_appear 24
  END IF
 
  IF attack.attacker_anim = 1 OR attack.attacker_anim = 7 THEN 'if it's cast or standing cast
   anim_setframe who, 4
  END IF
 
  anim_wait 3
 
  IF attack.attacker_anim <> 1 AND attack.attacker_anim <> 7 THEN 'if it's not cast or standing cast
   anim_setframe who, 3
  
   hx = GetHeroPos(hero(who)-1,1,0)
   hy = GetHeroPos(hero(who)-1,1,1)
   wx = GetWeaponPos(eqstuf(who,0)-1,0,0)
   wy = GetWeaponPos(eqstuf(who,0)-1,0,1)
   dx = hx - wx
   dy = hy - wy
  
   anim_align2 24, who, 0, 0, dx, 16
   anim_setz 24, 16 - dy
  
   anim_setframe 24, 1
  END IF
 
 END IF
 
 IF attack.attacker_anim = 3 THEN ' spin
  FOR ii AS INTEGER = 0 TO 2
   anim_setdir who, 1
   anim_wait 1
   anim_setdir who, 0
   anim_wait 1
  NEXT ii
 END IF
 
 IF attack.attacker_anim = 4 THEN ' Jump
  anim_setframe who, 4
  anim_relmove who, -40, 0, 7, 0
  anim_zmove who, 20, 10
  anim_waitforall
  anim_disappear who
  anim_setframe who, 0
 END IF
 
 IF attack.attacker_anim = 5 THEN ' Land
  anim_setz who, 200
  anim_setframe who, 2
  anim_appear who
  anim_setcenter who, bslot(who).t(0), 0, 0
  anim_align who, bslot(who).t(0), dirDown, 0
  anim_zmove who, -10, 20
  anim_waitforall
  anim_setframe who, 5
 END IF

END SUB

SUB anim_enemy (who as integer, attack as AttackData, bslot() AS BattleSprite)

 IF attack.attacker_anim < 2 THEN' twitch
  anim_setz who, 2
  anim_wait 1
  anim_setz who, 0
 END IF
 IF attack.attacker_anim = 3 THEN' spin
  FOR ii AS INTEGER = 0 TO 2
   anim_setdir who, 1
   anim_wait 1
   anim_setdir who, 0
   anim_wait 1
  NEXT ii
 END IF
 IF attack.attacker_anim = 4 THEN' jump
  anim_absmove who, bslot(who).x + 50, bslot(who).y, 7, 7
  anim_zmove who, 10, 20
  anim_waitforall
  anim_disappear who
 END IF
 IF attack.attacker_anim = 5 THEN' drop
  anim_setz who, 200
  anim_appear who
  anim_setpos who, bslot(bslot(who).t(0)).x, bslot(bslot(who).t(0)).y, 0
  anim_zmove who, -10, 20
  anim_waitforall
 END IF
END SUB

SUB anim_retreat (who as integer, attack as AttackData, bslot() AS BattleSprite)

 IF is_enemy(who) THEN
  IF attack.attacker_anim = 2 OR attack.attacker_anim = 5 THEN
   anim_setz who, 0
   anim_absmove who, bslot(who).x, bslot(who).y, 6, 6
   anim_waitforall
  END IF
 END IF

 IF is_hero(who) THEN
  IF attack.attacker_anim < 2 THEN ' strike, cast
   anim_walktoggle who
   anim_setmove who, 5, 0, 4, 0
   anim_waitforall
   anim_setframe who, 0
  END IF
  IF attack.attacker_anim = 2 OR attack.attacker_anim = 5 THEN ' dash, land
   anim_setframe who, 0
   anim_walktoggle who
   anim_setz who, 0
   anim_absmove who, bslot(who).x, bslot(who).y, 6, 6
   anim_waitforall
   anim_setframe who, 0
  END IF
  IF attack.attacker_anim = 7 THEN
   anim_setframe who, 0
  END IF
 END IF
END SUB

FUNCTION attack_can_hit_dead(who as integer, attack as AttackData) as integer

 SELECT CASE attack.targ_class
  CASE 4 'ally-including-dead (hero only)
   IF is_hero(who) THEN RETURN YES
  CASE 10 'dead-ally (hero only)
   IF is_hero(who) THEN RETURN YES
 END SELECT

 RETURN NO
END FUNCTION

SUB autotarget (who, atk AS AttackData, bslot() AS BattleSprite)

 DIM tmask(11) ' A list of true/false values indicating
               ' which targets are valid for the currently targetting attack

 DIM i AS INTEGER

 get_valid_targs tmask(), who, atk, bslot()

 'flush the targeting space for this attacker
 FOR i = 0 TO 11
  bslot(who).t(i) = -1
 NEXT i

 DIM targetptr AS INTEGER = 0

 SELECT CASE atk.targ_set

  CASE 0, 3: '--focus and random focus
   confirm_auto_focus who, tmask(), atk, bslot()

  CASE 1: '--spread attack
   confirm_auto_spread who, tmask(), bslot()

  CASE 2: '-- optional spread
   IF INT(RND * 100) < 33 THEN
    confirm_auto_spread who, tmask(), bslot()
   ELSE
    confirm_auto_focus who, tmask(), atk, bslot()
   END IF

  CASE 4: '--first target
   confirm_auto_first who, tmask(), bslot()

 END SELECT

END SUB

SUB confirm_auto_spread (who, tmask(), bslot() AS BattleSprite)
 DIM i AS INTEGER
 DIM targetptr AS INTEGER = 0
 FOR i = 0 TO 11
  IF tmask(i) <> 0 THEN
   bslot(who).t(targetptr) = i
   targetptr = targetptr + 1
  END IF
 NEXT i
END SUB

SUB confirm_auto_focus (who, tmask(), atk AS AttackData, bslot() AS BattleSprite)
 bslot(who).t(0) = find_preferred_target(tmask(), who, atk, bslot())
END SUB

SUB confirm_auto_first (who, tmask(), bslot() AS BattleSprite)
 DIM i AS INTEGER
 FOR i = 0 TO 11
  IF tmask(i) <> 0 THEN
   bslot(who).t(0) = i
   EXIT SUB
  END IF
 NEXT i
END SUB

FUNCTION find_preferred_target(tmask() as integer, who as integer, atk as AttackData, bslot() AS BattleSprite) as integer

 DIM i AS INTEGER
 DIM best AS INTEGER
 DIM search AS INTEGER
 DIM found AS INTEGER
 DIM prefstat AS INTEGER
 
 IF atk.prefer_targ_stat = 0 THEN
  'Weak/Strong pref stat defaults to target stat
  prefstat = atk.targ_stat
 ELSE
  prefstat = atk.prefer_targ_stat - 1
 END IF

 SELECT CASE atk.prefer_targ ' Preferred target type

 CASE 0 '--Default
  IF is_hero(who) THEN
   atk.prefer_targ = 1 ' heroes default to first target
  ELSEIF is_enemy(who) THEN
   atk.prefer_targ = 4 ' enemies default to a random target
  END IF
  found = find_preferred_target(tmask(), who, atk, bslot())
  atk.prefer_targ = 0
  RETURN found

 CASE 1 '--First
  'special handling for heroes using attacks that target all
  IF is_hero(who) AND atk.targ_class = 3 THEN
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
    search = bslot(i).stat.cur.sta(prefstat)
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
    search = bslot(i).stat.cur.sta(prefstat)
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
    search = INT(10000 / bslot(i).stat.max.sta(prefstat) * bslot(i).stat.cur.sta(prefstat))
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
    search = INT(10000 / bslot(i).stat.max.sta(prefstat) * bslot(i).stat.cur.sta(prefstat))
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

FUNCTION targenemycount (bslot() AS BattleSprite, for_alone_ai as integer=0) as integer
 DIM count AS INTEGER = 0
 DIM ignore AS INTEGER = NO
 FOR i AS INTEGER = 4 TO 11
  IF for_alone_ai THEN
   ignore = bslot(i).ignore_for_alone
  END IF
  IF bslot(i).stat.cur.hp > 0 AND bslot(i).vis = 1 AND ignore = NO THEN
   count = count + 1
  END IF
 NEXT i
 RETURN count
END FUNCTION
