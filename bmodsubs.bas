'OHRRPGCE GAME - Additional mostly battle-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "bmod.bi"
#include "bmodsubs.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "gglobals.bi"
#include "const.bi"
#include "uiconst.bi"
#include "udts.bi"
#include "battle_udts.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "yetmore2.bi"

DECLARE SUB confirm_auto_spread (byval who as integer, tmask() as integer, bslot() as BattleSprite, t() as integer)
DECLARE SUB confirm_auto_focus (byval who as integer, tmask() as integer, byref atk as AttackData, bslot() as BattleSprite, t() as integer)
DECLARE SUB confirm_auto_first (byval who as integer, tmask() as integer, bslot() as BattleSprite, t() as integer)

DECLARE FUNCTION quick_battle_distance(byval who1 as integer, byval who2 as integer, bslot() as BattleSprite) as integer
DECLARE FUNCTION battle_distance(byval who1 as integer, byval who2 as integer, bslot() as BattleSprite) as integer

DECLARE SUB transfer_enemy_bits(byval slot as integer, bslot() as BattleSprite)
DECLARE SUB transfer_enemy_counterattacks (byval slot as integer, bslot() as BattleSprite)
DECLARE SUB setup_non_volitile_enemy_state(byval slot as integer, bslot() as BattleSprite)
DECLARE SUB setup_enemy_sprite_and_name(byval slot as integer, bslot() as BattleSprite)
DECLARE SUB change_foe_stat(bspr as BattleSprite, byval stat_num as integer, byval new_max as integer, byval stat_rule as integer)

FUNCTION is_hero(byval who as integer) as integer
 IF who >= 0 AND who <= 3 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_enemy(byval who as integer) as integer
 IF who >= 4 AND who <= 11 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_attack(byval who as integer) as integer
 IF who >= 12 AND who <= 23 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION is_weapon(byval who as integer) as integer
 IF who = 24 THEN RETURN -1
 RETURN 0
END FUNCTION

FUNCTION atkallowed (atk as AttackData, byval attacker as integer, byval spclass as integer, byval lmplev as integer, bslot() as BattleSprite) as integer
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
DIM itemid as integer
DIM itemcount as integer
FOR i as integer = 0 to 2
  itemid = atk.item(i).id
  itemcount = atk.item(i).number
  IF itemid > 0 THEN 'this slot is used
    IF attacker <= 3 THEN ' Only hero items are checked right now
      IF countitem(itemid - 1) < itemcount THEN
        'yes, this still works for adding items.
        RETURN NO
      END IF
    END IF
  END IF
NEXT i

'--succeed
RETURN YES

END FUNCTION 'stat

FUNCTION checktheftchance (byval item as integer, byval itemP as integer, byval rareitem as integer, byval rareitemP as integer) as integer
IF randint(100) < itemP THEN
 '--success!
 getitem item
 checktheftchance = item + 1
ELSE
 IF randint(100) < rareitemP THEN
  '--rare success!
  getitem rareitem
  checktheftchance = rareitem + 1
 ELSE
  checktheftchance = 0
 END IF
END IF
'Update tags, this may matter for attack chaining
evalitemtags
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

 STATIC joyuse as integer
 STATIC joymenu as integer

 'Since this sub is called from lots of places, sticking this here is a stop-gap so
 'that messages from Custom are usually processed promptly
 IF running_as_slave THEN try_reload_lumps_anywhere

 FOR i as integer = 0 TO 7
  carray(i) = 0
 NEXT i

 'commented due to bug 619: gfx_sdl weirdness. Anyway, this check is from the DOS days
 'IF keyval(scNumlock) <> 0 THEN EXIT SUB ' no controls while PAUSE is pressed, because of its scancode wierdness

 'Keyboard

 FOR i as integer = 0 TO 3
  carray(i) = keyval(csetup(i))
 NEXT i
 carray(ccUse) = keyval(csetup(4)) OR keyval(csetup(5)) OR keyval(csetup(6))
 carray(ccMenu) = keyval(csetup(7)) OR keyval(csetup(8))
 carray(ccRun) = keyval(csetup(9)) OR keyval(csetup(10))
 IF keyval(scCtrl) > 0 AND keyval(csetup(11)) > 1 THEN  'ctrl + J
  calibrate
  FOR i as integer = 0 TO 1
   gotj(i) = readjoy(joy(), i)
  NEXT i
 END IF
 carray(8) = keyval(csetup(12))

 'Joystick

 DIM proceed as integer = NO
 FOR i as integer = 0 TO 1
  IF gotj(i) THEN
   gotj(i) = readjoy(joy(), i)
   proceed = YES
   EXIT FOR
  END IF
 NEXT i
 IF proceed ANDALSO gen(genJoy) THEN

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

  FOR i as integer = 0 TO 3
   carray(10 + i) = carray(i)
  NEXT i

 END IF  'joystick input
END SUB

FUNCTION countai (byval ai as integer, byval them as integer, bslot() as BattleSprite) as integer
 DIM atk as AttackData
 DIM atk_id as integer
 DIM count as integer = 0
 WITH bslot(them).enemy
  FOR i as integer = 0 TO 4
   atk_id = -1
   SELECT CASE ai
    CASE 0: atk_id = .regular_ai(i) - 1
    CASE 1: atk_id = .desperation_ai(i) - 1
    CASE 2: atk_id = .alone_ai(i) - 1
   END SELECT
   IF atk_id >= 0 THEN
    loadattackdata atk, atk_id
    IF atkallowed(atk, them, 0, 0, bslot()) THEN
     'this attack is allowed right now
     count += 1
    END IF
   END IF
  NEXT i
 END WITH
 RETURN count
END FUNCTION

FUNCTION enemycount (bslot() as BattleSprite) as integer
 DIM result as integer = 0
 FOR i as integer = 4 TO 11
  IF bslot(i).stat.cur.hp > 0 THEN result += 1
 NEXT i
 RETURN result
END FUNCTION

Function GetWeaponPos(byval w as integer, byval f as integer, byval isY as integer) as integer'or x?
 'FIXME: Ack! Lets just make handle position a member of bslot()
 'FIXME: Ack! Already did the above... using this sub!
 DIM fh as integer
 IF w >= 0 THEN
  fh = FREEFILE
  OPEN game + ".itm" FOR BINARY as #fh
  DIM recoff as integer = w * getbinsize(binITM) + 1
  'debug "weapon " & w & " offset: " & (recoff + 156 + f * 4 + isY * 2)
  GetWeaponPos = ReadShort(fh, recoff + 156 + f * 4 + iif(isY,1,0) * 2)
  CLOSE #FH
 END IF
End Function

FUNCTION inflict (byval attackerslot as integer, byval targetslot as integer, byref attacker as BattleSprite, byref target as BattleSprite, attack as AttackData, byval tcount as integer, byval hit_dead as integer=NO) as integer
 'This overload is for when you want the luxury of not caring which stat was damaged, or by how much.
 DIM h as integer = 0
 DIM targstat as integer = 0
 RETURN inflict(h, targstat, attackerslot, targetslot, attacker, target, attack, tcount, hit_dead)
END FUNCTION

FUNCTION inflict (byref h as integer, byref targstat as integer, byval attackerslot as integer, byval targetslot as integer, byref attacker as BattleSprite, byref target as BattleSprite, attack as AttackData, byval tcount as integer, byval hit_dead as integer=NO) as integer
 
 attacker.attack_succeeded = 0
 
 'remember this target
 attacker.last_targs(targetslot) = YES
 
 'stored targs
 IF attack.store_targ THEN
  attacker.stored_targs(targetslot) = YES
  attacker.stored_targs_can_be_dead = hit_dead
 END IF
 IF attack.delete_stored_targ THEN
  FOR i as integer = 0 TO UBOUND(attacker.stored_targs)
   attacker.stored_targs(i) = NO
  NEXT i
  attacker.stored_targs_can_be_dead = NO
 END IF
 
 'no damage
 IF attack.damage_math <> 4 THEN
 
  'init
  WITH target
   .harm.text = ""
   .harm.ticks = gen(genDamageDisplayTicks)
   .harm.pos.x = .x + (.w * .5)
   .harm.pos.y = .y + (.h * .5)
  
   targstat = bound(attack.targ_stat, 0, UBOUND(.stat.cur.sta))
   'backcompat MP-targstat
   IF attack.obsolete_damage_mp THEN
    IF targstat = statHP THEN targstat = statMP
   END IF
  END WITH
 
  'accuracy
  DIM acc as integer = attacker.stat.cur.acc
  DIM dog as integer = target.stat.cur.dog
  DIM dogm as SINGLE = .25 'dodge modifier
  IF attack.aim_math = 1 THEN dogm = 0.5
  IF attack.aim_math = 2 THEN dogm = 1.0
  IF attack.aim_math = 4 THEN dogm = 1.25
  IF attack.aim_math = 4 OR attack.aim_math = 7 OR attack.aim_math = 8 THEN
   acc = attacker.stat.cur.mag
   dog = target.stat.cur.wil
  END IF
 
  DIM attackhit as integer
  attackhit = range(acc, 75) >= range(dog * dogm, 75)
  IF attack.aim_math = 3 THEN attackhit = YES
  IF attack.aim_math = 5 OR attack.aim_math = 7 THEN attackhit = randint(100) < (acc * (100 - dog)) / 100 
  IF attack.aim_math = 6 OR attack.aim_math = 8 THEN attackhit = randint(100) < acc
  IF attackhit = NO THEN
   target.harm.text = readglobalstring$(120, "miss", 20)
   RETURN NO
  END IF
 
  WITH target
   IF attack.fail_if_targ_poison = YES AND .stat.cur.poison < .stat.max.poison THEN
    .harm.text = readglobalstring$(122, "fail", 20)
    RETURN NO
   END IF
   IF attack.fail_if_targ_regen = YES AND .stat.cur.regen < .stat.max.regen THEN
    .harm.text = readglobalstring$(122, "fail", 20)
    RETURN NO
   END IF
   IF attack.fail_if_targ_stun = YES AND .stat.cur.stun <> .stat.max.stun THEN
    .harm.text = readglobalstring$(122, "fail", 20)
    RETURN NO
   END IF
   IF attack.fail_if_targ_mute = YES AND .stat.cur.mute <> .stat.max.mute THEN
    .harm.text = readglobalstring$(122, "fail", 20)
    RETURN NO
   END IF
  END WITH
 
  'attack power and defense power
  DIM ap as integer = attacker.stat.cur.str
  DIM dp as integer = target.stat.cur.def
  SELECT CASE attack.base_atk_stat
   CASE 1
    ap = attacker.stat.cur.mag
    dp = target.stat.cur.wil
   CASE 2
    ap = attacker.stat.cur.hp
   CASE 3
    ap = attacker.stat.max.hp - attacker.stat.cur.hp
   CASE 4
    ap = randint(1000)  '0 to 999
   CASE 5
    ap = 100
   CASE 6 TO 17
    ap = attacker.stat.cur.sta(attack.base_atk_stat - 6)
   CASE 18
    ap = attacker.repeatharm
   CASE 19
    ap = attacker.revengeharm
   CASE 20
    ap = target.revengeharm
   CASE 21
    ap = attacker.thankvengecure
   CASE 22
    ap = target.thankvengecure
   CASE 23 TO 34
    ap = target.stat.cur.sta(attack.base_atk_stat - 23)
   CASE 35 TO 46
    ap = attacker.stat.max.sta(attack.base_atk_stat - 36)
   CASE 47 TO 58
    ap = target.stat.max.sta(attack.base_atk_stat - 47)
   CASE IS >= 59
    debug "Unknown base stat " & attack.base_atk_stat & " for attack " & attack.id
  END SELECT
 
  '--defense base
  IF attack.base_def_stat > 0 AND attack.base_def_stat <= UBOUND(target.stat.cur.sta) + 1 THEN dp = target.stat.cur.sta(attack.base_def_stat - 1)
 
  'calc defense
  DIM am as SINGLE = 1.0
  DIM dm as SINGLE = 0.5                    'atk-def*.5
  IF attack.damage_math = 1 THEN am = 0.8  : dm = 0.1 'atk*.8-def*.5
  IF attack.damage_math = 2 THEN am = 1.3 : dm = 1.0  'atk-1.3-def
  IF attack.damage_math = 3 THEN am = 1.0 : dm = 0.0  'atk
 
  'calc harm
  h = (ap * am) - (dp * dm)
  'Temporarily use floating point
  DIM harmf as SINGLE = h
  DIM cure as integer = NO
  DIM immune as integer = NO
 
  'elementals
  FOR i as integer = 0 TO gen(genNumElements) - 1
   IF attack.elemental_damage(i) = YES THEN
    harmf *= ABS(target.elementaldmg(i))
    'What's a good cut off for immunity? When we switch to 32bit HP values, maybe
    'you'll want to be able to do 1/1000,000 normal damage without triggering immunity?
    IF ABS(target.elementaldmg(i)) < 0.000005 THEN
     immune = YES
    ELSEIF target.elementaldmg(i) < 0.0 THEN
     cure = YES  'absorb
    END IF
   END IF
   WITH attack.elemental_fail_conds(i)
    DIM fail as integer = NO
    IF .type = compLt THEN
     DIM effectiveval as single = target.elementaldmg(i)
     'Simulate old fail vs element resist bit:
     'The old bit checked only the target's Strong bits, ignoring their Absorb bits
     IF readbit(gen(), genBits2, 9) = 1 THEN effectiveval = ABS(effectiveval)
     fail = (effectiveval < .value - 0.000005)
    END IF
    IF .type = compGt THEN fail = (target.elementaldmg(i) > .value + 0.000005)
    IF fail THEN
     target.harm.text = readglobalstring$(122, "fail", 20)
     RETURN NO
    END IF
   END WITH
  NEXT
 
  'extra damage
  harmf *= (1.0 + attack.extra_damage / 100)
 
  'convert to integer -- do this now as using an accurate floating point
  'version of range causes up to 1 more average damage to be done
  IF harmf > 2147483647 THEN
   h = 2147483647
  ELSEIF harmf < 0 THEN
   'This check just prevents overflow; actual 0 damage cap is below.
   h = 0
  ELSE
   h = harmf

   'randomize +/- 20%
   IF attack.do_not_randomize = NO THEN h = range(h, 20)
  END IF
 
  'spread damage
  IF attack.divide_spread_damage = YES THEN h = h / tcount
 
  'cap under
  IF immune ANDALSO readbit(gen(), genBits2, 10) THEN
   'zero damage from elemental immunity, even without attack bit
   h = 0
  ELSEIF h <= 0 THEN
   IF attack.damage_can_be_zero = NO THEN h = 1 ELSE h = 0
  END IF
 
  IF attack.show_damage_without_inflicting = NO THEN
   'resetting
   IF attack.reset_targ_stat_before_hit = YES THEN
    target.stat.cur.sta(targstat) = target.stat.max.sta(targstat)
   END IF
  END IF

  DIM chp as integer = target.stat.cur.sta(targstat)  'for convenience, not for remembering value
  DIM mhp as integer = target.stat.max.sta(targstat)
  IF attack.percent_damage_not_set = YES THEN
   'percentage attacks do damage
   'FIXME: see bug 134 about moving this block up the function. This should be base damage?
   SELECT CASE attack.damage_math
    CASE 5'% of max
     h = mhp + (attack.extra_damage * mhp / 100)
     cure = NO
    CASE 6'% of cur
     h = chp + (attack.extra_damage * chp / 100)
     cure = NO
   END SELECT
  END IF

  'h should always be nonnegative at this point
  IF h < 0 THEN debug "inflict: negative h!"

  IF cure THEN
   h *= -1      'elemental absorb
  ELSEIF attack.cure_instead_of_harm = YES AND target.harmed_by_cure = NO THEN
   h *= -1      'cure bit
  END IF
 
  DIM capdamage as integer = YES
 
  IF attack.percent_damage_not_set = NO THEN
   'percentage attacks set stat
   'and by set, we really mean set, ignore nearly all attack settings,
   'that's my interpretation of intent anyway - TMC
   '...And mine to. - James
   SELECT CASE attack.damage_math
    CASE 5'% of max
     h = chp - (mhp + (attack.extra_damage * mhp / 100))
     capdamage = NO
    CASE 6'% of cur
     h = chp - (chp + (attack.extra_damage * chp / 100))
     capdamage = NO
   END SELECT
  END IF

  'Maybe cap damage to max possible damage/cure
  'Note that unlike the damage cap, this still happens if not inflicting
  IF attack.do_not_exceed_targ_stat AND capdamage THEN
   IF h > 0 THEN 'damage
    h = small(h, target.stat.cur.sta(targstat))
   ELSEIF h < 0 THEN ' cure
    'Note: this cure can no exceed max regardless of "allow cure to exceed maximum" bit
    DIM diff as integer = target.stat.max.sta(targstat) - target.stat.cur.sta(targstat)
    IF diff >= 0 THEN
     h = large(h, diff * -1)
    END IF
   END IF
  END IF

  'remember target stat
  DIM remtargstat as integer = target.stat.cur.sta(targstat)
  DIM rematkrstat as integer = attacker.stat.cur.sta(targstat)

  'inflict
  IF attack.show_damage_without_inflicting = NO THEN
   'I think that not applying the damage cap when only showing damage is pretty dodgy...
   IF gen(genDamageCap) > 0 AND capdamage THEN
    IF h > gen(genDamageCap) THEN h = gen(genDamageCap)
    IF h < -gen(genDamageCap) THEN h = -gen(genDamageCap)
   END IF
 
   target.stat.cur.sta(targstat) = safesubtract(target.stat.cur.sta(targstat), h)
   IF attack.absorb_damage THEN
    WITH attacker
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

   'enforce stat bounds
   target.stat.cur.sta(targstat) = large(target.stat.cur.sta(targstat), 0)
   attacker.stat.cur.sta(targstat) = large(attacker.stat.cur.sta(targstat), 0)
   IF attack.allow_cure_to_exceed_maximum = NO THEN
    target.stat.cur.sta(targstat) = small(target.stat.cur.sta(targstat), large(target.stat.max.sta(targstat), remtargstat))
    attacker.stat.cur.sta(targstat) = small(attacker.stat.cur.sta(targstat), large(attacker.stat.max.sta(targstat), rematkrstat))
   END IF

   'FIXME: stat caps aren't observed (bug 980)
   'HOWEVER, when bug 980 is fixed, need to be careful not to break out-of-battle permanent stat
   'boosting items, which rely on the caps being exceeded, then update hero base stats as a result.
   'See oobcure.

   'remember revenge data
   'NOTE: revenge & thankvenge record difference AFTER "reset target stat to max" takes effect
   IF remtargstat > target.stat.cur.sta(targstat) THEN
    target.revengemask(attackerslot) = YES
    target.revenge = attackerslot
    target.revengeharm = remtargstat - target.stat.cur.sta(targstat)
    attacker.repeatharm = remtargstat - target.stat.cur.sta(targstat)
   END IF

   'remember thankvenge data
   IF remtargstat < target.stat.cur.sta(targstat) THEN
    target.thankvengemask(attackerslot) = YES
    target.thankvenge = attackerslot
    target.thankvengecure = ABS(remtargstat - target.stat.cur.sta(targstat))
   END IF
  END IF
 
  'set damage display
  IF attack.do_not_display_damage = NO THEN
   target.harm.text = STR(ABS(h))
   '--if cure, show + sign
   IF h < 0 THEN target.harm.text = "+" + target.harm.text
  END IF
 
 END IF 'skips to here if no damage
 
 IF attack.show_name = YES THEN
  IF LEN(target.harm.text) > 0 THEN target.harm.text += " "
  target.harm.text += attack.name
 END IF
 
 'reset registers as per convenience bits
 IF attack.reset_poison = YES THEN target.stat.cur.poison = target.stat.max.poison
 IF attack.reset_regen = YES  THEN target.stat.cur.regen  = target.stat.max.regen
 IF attack.reset_stun = YES   THEN target.stat.cur.stun   = target.stat.max.stun
 IF attack.reset_mute = YES   THEN target.stat.cur.mute   = target.stat.max.mute
 
 '--success!
 attacker.attack_succeeded = 1
 RETURN YES

END FUNCTION

FUNCTION liveherocount (bslot() as BattleSprite) as integer
 '--with bslot() counts heros in-battle HP state
 DIM i as integer = 0
 FOR o as integer = 0 TO 3
  IF hero(o) > 0 AND bslot(o).stat.cur.hp > 0 THEN i = i + 1
 NEXT o
 RETURN i
END FUNCTION

FUNCTION liveherocount () as integer
 '--with no argument, counts live heroes in outside-of-battle active party
 DIM liveheroes as integer = 0
 FOR o as integer = 0 TO 3
  IF hero(o) > 0 THEN
   IF gam.hero(o).stat.cur.hp > 0 OR gam.hero(o).stat.max.hp <= 0 THEN liveheroes += 1
  END IF
 NEXT o
 RETURN liveheroes
END FUNCTION

FUNCTION safesubtract (byval number as integer, byval minus as integer) as integer
 DIM longnumber as DOUBLE = number
 DIM longminus as DOUBLE = minus
 DIM longresult as DOUBLE = longnumber - longminus
 IF longresult > 32767 THEN longresult = 32767
 IF longresult < -32768 THEN longresult = -32768
 DIM result as integer = longresult
 RETURN result
END FUNCTION

FUNCTION safemultiply (byval number as integer, byval by as single) as integer
 dim as integer longnumber = number
 dim as single longby = by
 dim as integer longresult = longnumber * longby
 IF longresult > 32767 THEN longresult = 32767
 IF longresult < -32768 THEN longresult = -32768
 dim result as integer = longresult
 return result
END FUNCTION

SUB setbatcap (bat as BattleState, cap as string, byval captime as integer, byval capdelay as integer)
 bat.caption = cap
 bat.caption_time = captime
 bat.caption_delay = capdelay
END SUB

'This picks out the targets which are within a 90 degree wide sector
SUB battle_target_arrows_sector_mask (inrange() as integer, byval d as integer, byval axis as integer, bslot() as BattleSprite, targ as TargettingState, foredistance() as integer, sidedistance() as integer)
 DIM as integer xdistance, ydistance
 FOR i as integer = 0 TO 11
  IF targ.mask(i) THEN
   ydistance = (bslot(i).y - bslot(targ.pointer).y) * d
   xdistance = (bslot(i).x - bslot(targ.pointer).x) * d
   IF axis THEN
    foredistance(i) = ydistance
    sidedistance(i) = xdistance
   ELSE
    foredistance(i) = xdistance
    sidedistance(i) = ydistance
   END IF
   IF foredistance(i) > 0 AND ABS(sidedistance(i)) <= foredistance(i) THEN
    setbit inrange(), 0, i, 1
   END IF
  END IF
 NEXT i
END SUB

SUB battle_target_arrows (byval d as integer, byval axis as integer, bslot() as BattleSprite, targ as TargettingState, byval allow_spread as integer=0)
 DIM newptr as integer = targ.pointer

 'first, special case for target at same position at current
 DIM idx as integer = targ.pointer
 FOR i as integer = 1 TO 11
  idx += d  'search through slots according to direction, but don't loop
  IF idx < 0 OR idx > UBOUND(bslot) THEN EXIT FOR
  IF bslot(idx).x = bslot(targ.pointer).x AND bslot(idx).y = bslot(targ.pointer).y THEN
   targ.pointer = idx
   EXIT SUB
  END IF
 NEXT

 '--look for a nearby target within a 90 degree wide "sector" in the right direction
 DIM foredistance(11) as integer
 DIM sidedistance(11) as integer
 DIM inrange(0) as integer
 inrange(0) = 0
 battle_target_arrows_sector_mask inrange(), d, axis, bslot(), targ, foredistance(), sidedistance()
 IF inrange(0) THEN
  DIM best as integer = 99999
  FOR i as integer = 0 TO 11
   IF readbit(inrange(), 0, i) THEN
    IF foredistance(i) < best THEN
     best = foredistance(i)
     newptr = i
    END IF
   END IF
  NEXT i
 END IF
 IF newptr = targ.pointer THEN
  DIM bestangle as DOUBLE = 999.
  DIM angle as DOUBLE
  FOR i as integer = 0 TO 11
   IF targ.mask(i) AND foredistance(i) > 0 THEN
    angle = ABS(ATAN2(sidedistance(i), foredistance(i)))
    IF angle < bestangle THEN
     bestangle = angle
     newptr = i
    END IF
   END IF
  NEXT i
 END IF 
 IF newptr = targ.pointer THEN
  IF allow_spread = YES AND targ.opt_spread = 1 THEN
   FOR i as integer = 0 TO 11
    targ.selected(i) = targ.mask(i)
   NEXT i
   targ.opt_spread = 2
  END IF
 END IF
 targ.pointer = newptr
END SUB

FUNCTION targetmaskcount (tmask() as integer) as integer
 DIM n as integer = 0
 FOR i as integer = 0 TO 11
  IF tmask(i) THEN n += 1
 NEXT i
 RETURN n
END FUNCTION

SUB traceshow (s as string)
 textcolor uilook(uiText), uilook(uiOutline)
 s += STRING(40 - LEN(s), " ")
 printstr s, 0, 191, 0
 printstr s, 0, 191, 1
END SUB

FUNCTION trytheft (bat as BattleState, byval who as integer, byval targ as integer, attack as AttackData, bslot() as BattleSprite) as integer
 IF is_hero(who) AND is_enemy(targ) THEN
  '--a hero is attacking an enemy
  IF attack.can_steal_item THEN
   '--steal bitset is on for this attack
   WITH bslot(targ).enemy.steal
    IF .thievability >= 0 THEN
     '--enemy is theftable
     DIM stole as integer = checktheftchance(.item, .item_rate, .rare_item, .rare_item_rate)
     IF stole THEN
      '--success!
      IF .thievability = 0 THEN
       '--only one theft permitted
       .thievability = -1
      END IF
      setbatcap bat, readglobalstring$(117, "Stole", 40) + " " + readitemname$(stole - 1), 40, 0
      RETURN YES '--success
     ELSE
      '--steal failed
      setbatcap bat, readglobalstring$(114, "Cannot Steal", 40), 40, 0
     END IF
    ELSE
     '--has nothing to steal / steal disabled
     setbatcap bat, readglobalstring$(111, "Has Nothing", 30), 40, 0
    END IF
   END WITH
  END IF
 END IF
 RETURN NO '--return false by default
END FUNCTION

FUNCTION hero_total_exp (byval hero_slot as integer) as integer
 RETURN total_exp_to_level(gam.hero(hero_slot).lev) + gam.hero(hero_slot).exp_cur
END FUNCTION

SUB updatestatslevelup (byval hero_slot as integer, byval allowforget as integer)
 ' hero_slot = who
 ' allowforget = forget spells if level dropped below requirement

 'wipe learnmask for this hero
 'note that this gets wiped again later, but that is okay.
 'this makes sure that learnmask gets cleared for this battle
 'even if the hero *doesn't* get a level-up.
 FOR o as integer = hero_slot * 6 TO hero_slot * 6 + 5
  learnmask(o) = 0
 NEXT

 WITH gam.hero(hero_slot)

  'THIS PART UPDATES STATS FOR A LEVEL UP
  IF .lev_gain THEN

   DIM her as herodef
   loadherodata her, hero(hero_slot) - 1

   'stat increase/decrease
   FOR statnum as integer = 0 TO statLast
    DIM n0 as integer = her.Lev0.sta(statnum)
    DIM nMax as integer = her.LevMax.sta(statnum)
    DIM statgain as integer
    statgain = atlevel(.lev, n0, nMax) - atlevel(.lev - .lev_gain, n0, nMax)
    .stat.base.sta(statnum) += statgain
   NEXT

   'simulate ancient levelup bug
   IF readbit(gen(), genBits, 9) = 1 THEN
    DIM bonuses(statLast) as integer
    hero_total_equipment_bonuses hero_slot, bonuses()
    FOR statnum as integer = 0 TO statLast
     .stat.base.sta(statnum) += bonuses(statnum) * .lev_gain
    NEXT
   END IF 

   recompute_hero_max_stats hero_slot

   'stat restoration
   IF readbit(gen(), genBits, 2) = 0 THEN
    '--HP restoration ON
    .stat.cur.hp = .stat.max.hp 'set external cur to external max
   END IF
   IF readbit(gen(), genBits, 3) = 0 THEN
    '--MP restoration ON
    .stat.cur.mp = .stat.max.mp 'set external cur to external max
    resetlmp hero_slot, .lev
   END IF

   'make current stats match max stats
   FOR statnum as integer = 2 TO 11
    .stat.cur.sta(statnum) = .stat.max.sta(statnum)
   NEXT statnum

   learn_spells_for_current_level(hero_slot, allowforget)

  END IF
 END WITH
END SUB

'Load sum of bonuses for all of a hero's equip in bonuses() (which should be sized to statLast)
SUB hero_total_equipment_bonuses(byval hero_slot as integer, bonuses() as integer)
 flusharray bonuses()
 DIM itembuf(dimbinsize(binITM)) as integer
 FOR slot as integer = 0 TO 4
  IF eqstuf(hero_slot, slot) > 0 THEN    
   loaditemdata itembuf(), eqstuf(hero_slot, slot) - 1
   FOR statnum as integer = 0 TO statLast
    bonuses(statnum) += itembuf(54 + statnum)
   NEXT statnum
  END IF
 NEXT slot
END SUB

'Recompute max stats from base
'(See update_hero_max_and_cur_stats for wrapper that updates cur values too)
SUB recompute_hero_max_stats(byval hero_slot as integer)
 WITH gam.hero(hero_slot).stat
  DIM bonuses(statLast) as integer
  hero_total_equipment_bonuses hero_slot, bonuses()
  FOR statnum as integer = 0 TO statLast
   .max.sta(statnum) = .base.sta(statnum) + bonuses(statnum)
   DIM cap as integer = gen(genStatCap + statnum)
   IF cap > 0 THEN .max.sta(statnum) = small(.max.sta(statnum), cap)
  NEXT
 END WITH
END SUB

'This is used for backcompat
SUB compute_hero_base_stats_from_max(byval hero_slot as integer)
 WITH gam.hero(hero_slot).stat
  DIM bonuses(statLast) as integer
  hero_total_equipment_bonuses hero_slot, bonuses()
  FOR statnum as integer = 0 TO statLast
   .base.sta(statnum) = .max.sta(statnum) - bonuses(statnum)
  NEXT
 END WITH
END SUB

SUB learn_spells_for_current_level(byval who as integer, byval allowforget as integer)

 'Teaches all spells that can be learned from level
 ' up to the hero's current level.
 '(but should not overwrite any spells learned with "write spell")

 'wipe learnmask for this hero
 FOR o as integer = who * 6 TO who * 6 + 5
  learnmask(o) = 0
 NEXT

 dim her as herodef
 loadherodata her, hero(who) - 1

 'learn spells
 FOR j as integer = 0 TO 3
  FOR o as integer = 0 TO 23
   WITH her.spell_lists(j,o)
    '--if slot is empty and slot accepts a spell and learn-by-level condition is true
    IF spell(who, j, o) = 0 AND .attack > 0 AND .learned - 1 <= gam.hero(who).lev AND .learned > 0 THEN
     spell(who, j, o) = .attack
     setbit learnmask(), 0, who * 96 + j * 24 + o, 1
    END IF
    IF allowforget THEN
     '--plotscripts may lower level, forget spells if drop below requirement and know the spell specified
     IF spell(who, j, o) = .attack AND .learned - 1 > gam.hero(who).lev THEN
      spell(who, j, o) = 0
     END IF
    END IF
   END WITH
  NEXT o
 NEXT j
 
END SUB

FUNCTION allowed_to_gain_levels(byval heroslot as integer) as integer
 IF heroslot < 0 THEN RETURN NO 'out of range
 IF heroslot > UBOUND(gam.hero) THEN RETURN NO ' out of range
 IF hero(heroslot) <= 0 THEN RETURN NO ' no hero in this slot
 IF gam.hero(heroslot).lev >= current_max_level THEN RETURN NO
 RETURN YES
END FUNCTION

SUB giveheroexperience (byval who as integer, byval exper as integer)
 'reset levels gained
 gam.hero(who).lev_gain = 0
 IF allowed_to_gain_levels(who) THEN
  gam.hero(who).exp_cur += exper
  'levelups
  WHILE gam.hero(who).exp_cur >= gam.hero(who).exp_next
   gam.hero(who).exp_cur -= gam.hero(who).exp_next
   gam.hero(who).lev += 1 'current level
   gam.hero(who).lev_gain += 1 'levelup counter
   gam.hero(who).exp_next = exptolevel(gam.hero(who).lev + 1)
   IF gam.hero(who).lev >= current_max_level THEN
    'You can't gain experience once you've hit the level cap
    gam.hero(who).exp_cur = 0
    EXIT WHILE
   END IF
  WEND
 END IF
END SUB

SUB setheroexperience (byval who as integer, byval amount as integer, byval allowforget as integer)
 'unlike giveheroexperience, this can cause delevelling
 DIM orig_lev as integer = gam.hero(who).lev
 DIM total as integer = 0
 DIM lostlevels as integer = NO
 
 FOR i as integer = 0 TO gam.hero(who).lev - 1
  total += exptolevel(i + 1)
 NEXT
 IF total > amount THEN
  'losing levels; lvl up from level 0
  gam.hero(who).lev = 0
  gam.hero(who).exp_next = exptolevel(1)
  lostlevels = YES
 ELSE
  'set spell learnt bits correctly
  amount -= total
  orig_lev = 0
  lostlevels = NO
 END IF
 gam.hero(who).exp_cur = 0
 giveheroexperience who, amount
 updatestatslevelup who, allowforget
 gam.hero(who).lev_gain -= orig_lev
 IF lostlevels THEN
  'didn't learn spells, wipe mask
  FOR i as integer = who * 6 TO who * 6 + 5
   learnmask(i) = 0
  NEXT
 END IF
END SUB

FUNCTION visibleandalive (byval who as integer, bslot() as BattleSprite) as integer
 RETURN (bslot(who).vis = 1 AND bslot(who).stat.cur.hp > 0)
END FUNCTION

SUB writestats (bslot() as BattleSprite)
 FOR i as integer = 0 TO 3
  IF hero(i) > 0 THEN
   '--set out-of-battle HP and MP equal to in-battle HP and MP
   gam.hero(i).stat.cur.hp = bslot(i).stat.cur.hp
   gam.hero(i).stat.cur.mp = bslot(i).stat.cur.mp
  END IF
 NEXT i
END SUB

SUB get_valid_targs(tmask() as integer, byval who as integer, byref atk as AttackData, bslot() as BattleSprite)

 DIM i as integer

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
   IF bslot(who).stored_targs(i) = YES AND (bslot(i).vis <> 0 OR bslot(who).stored_targs_can_be_dead) THEN
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

 FOR i = 0 TO 11
  'If a target is doing an on-death bequest attack,
  ' it cannot be targeted by anyone but itself
  IF bslot(i).bequesting AND i <> who THEN
   tmask(i) = 0
  END IF
 NEXT i

 'Some restrictions are only applied when the target class is not "self"
 IF atk.targ_class <> 2 THEN
  FOR i = 0 TO 11
   'enforce untargetability
   IF is_hero(who) THEN
    IF bslot(i).hero_untargetable = YES THEN tmask(i) = 0
   ELSEIF is_enemy(who) THEN
    IF bslot(i).enemy_untargetable = YES THEN tmask(i) = 0
   END IF
  NEXT i
 END IF

END SUB

SUB anim_advance (byval who as integer, attack as AttackData, bslot() as BattleSprite, t() as integer)

 DIM d as integer
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
  DIM yt as integer
  yt = (bslot(t(0)).h - bslot(who).h) + 2
  anim_walktoggle who
  anim_absmove who, bslot(t(0)).x + bslot(t(0)).w * d, bslot(t(0)).y + yt, 6, 6
  anim_waitforall
 END IF
 
 IF attack.attacker_anim = 8 THEN ' Teleport
  anim_setpos who, bslot(t(0)).x + bslot(t(0)).w * d, bslot(t(0)).y + (bslot(t(0)).h - (bslot(who).h)), 0
 END IF

END SUB

SUB anim_hero (byval who as integer, attack as AttackData, bslot() as BattleSprite, t() as integer)
 DIM hx as integer = 0
 DIM hy as integer = 0
 DIM wx as integer = 0
 DIM wy as integer = 0
 DIM dx as integer = 0
 DIM dy as integer = 0
 
 IF attack.attacker_anim < 3 OR (attack.attacker_anim > 6 AND attack.attacker_anim < 9) THEN ' strike, cast, dash, standing cast, teleport
  anim_setframe who, 0
  anim_wait 3 'wait 3 ticks
  
  IF attack.attacker_anim <> 1 AND attack.attacker_anim <> 7 THEN 'if it's not cast or standing cast
   anim_setframe who, 2
  
   hx = gam.hero(who).hand_pos(0).x
   hy = gam.hero(who).hand_pos(0).y
   wx = bslot(24).hand(0).x
   wy = bslot(24).hand(0).y
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
  
   hx = gam.hero(who).hand_pos(1).x
   hy = gam.hero(who).hand_pos(1).y
   wx = bslot(24).hand(1).x
   wy = bslot(24).hand(1).y
   dx = hx - wx
   dy = hy - wy
  
   anim_align2 24, who, 0, 0, dx, 16
   anim_setz 24, 16 - dy
  
   anim_setframe 24, 1
  END IF
 
 END IF
 
 IF attack.attacker_anim = 3 THEN ' spin
  FOR ii as integer = 0 TO 2
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
  anim_setcenter who, t(0), 0, 0
  anim_align who, t(0), dirDown, 0
  anim_zmove who, -10, 20
  anim_waitforall
  anim_setframe who, 5
 END IF

END SUB

SUB anim_enemy (byval who as integer, attack as AttackData, bslot() as BattleSprite, t() as integer)

 IF attack.attacker_anim < 2 THEN' twitch
  anim_setz who, 2
  anim_wait 1
  anim_setz who, 0
 END IF
 IF attack.attacker_anim = 3 THEN' spin
  FOR ii as integer = 0 TO 2
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
  anim_setpos who, bslot(t(0)).x, bslot(t(0)).y, 0
  anim_zmove who, -10, 20
  anim_waitforall
 END IF
END SUB

SUB anim_retreat (byval who as integer, attack as AttackData, bslot() as BattleSprite)

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

FUNCTION attack_can_hit_dead(byval who as integer, byval atk_id as integer, byval stored_targs_can_be_dead as integer=NO) as integer
 DIM attack as AttackData
 loadattackdata attack, atk_id
 RETURN attack_can_hit_dead(who, attack, stored_targs_can_be_dead)
END FUNCTION

FUNCTION attack_can_hit_dead(byval who as integer, attack as AttackData, byval stored_targs_can_be_dead as integer=NO) as integer

 SELECT CASE attack.targ_class
  CASE 4 'ally-including-dead (hero only)
   IF is_hero(who) THEN RETURN YES
  CASE 9 'stored target
   IF is_hero(who) THEN
    IF stored_targs_can_be_dead THEN RETURN YES
   END IF
  CASE 10 'dead-ally (hero only)
   IF is_hero(who) THEN RETURN YES
 END SELECT

 RETURN NO
END FUNCTION

SUB autotarget (byval who as integer, byval atk_id as integer, bslot() as BattleSprite, byval queue as integer=YES, byval override_blocking as integer=-2, byval dont_retarget as integer=NO)
 DIM t(11) as integer
 autotarget who, atk_id, bslot(), t(), queue, override_blocking, dont_retarget
END SUB

SUB autotarget (byval who as integer, byval atk_id as integer, bslot() as BattleSprite, t() as integer, byval queue as integer=YES, byval override_blocking as integer=-2, byval dont_retarget as integer=NO)
 DIM attack as AttackData
 loadattackdata attack, atk_id
 autotarget who, attack, bslot(), t(), queue, override_blocking, dont_retarget
END SUB

SUB autotarget (byval who as integer, byref atk as AttackData, bslot() as BattleSprite, byval queue as integer=YES, byval override_blocking as integer=-2, byval dont_retarget as integer=NO)
 DIM t(11) as integer
 autotarget who, atk, bslot(), t(), queue, override_blocking, dont_retarget
END SUB

SUB autotarget (byval who as integer, byref atk as AttackData, bslot() as BattleSprite, t() as integer, byval queue as integer=YES, byval override_blocking as integer=-2, byval dont_retarget as integer=NO)

 DIM tmask(11) as integer ' A list of true/false values indicating
                          ' which targets are valid for the currently targetting attack
 DIM i as integer

 get_valid_targs tmask(), who, atk, bslot()

 flusharray t(), 11, -1

 SELECT CASE atk.targ_set

  CASE 0, 3: '--focus and random roulette
   confirm_auto_focus who, tmask(), atk, bslot(), t()

  CASE 1: '--spread attack
   confirm_auto_spread who, tmask(), bslot(), t()

  CASE 2: '-- optional spread
   IF randint(100) < 33 THEN
    confirm_auto_spread who, tmask(), bslot(), t()
   ELSE
    confirm_auto_focus who, tmask(), atk, bslot(), t()
   END IF

  CASE 4: '--first target
   confirm_auto_first who, tmask(), bslot(), t()

 END SELECT

 '--Now copy the target into the queue
 IF queue THEN
  queue_attack atk.id, who, t(), override_blocking, dont_retarget
 END IF

END SUB

SUB confirm_auto_spread (byval who as integer, tmask() as integer, bslot() as BattleSprite, t() as integer)
 DIM i as integer
 DIM targetptr as integer = 0
 FOR i = 0 TO 11
  IF tmask(i) <> 0 THEN
   t(targetptr) = i
   targetptr = targetptr + 1
  END IF
 NEXT i
END SUB

SUB confirm_auto_focus (byval who as integer, tmask() as integer, byref atk as AttackData, bslot() as BattleSprite, t() as integer)
 t(0) = find_preferred_target(tmask(), who, atk, bslot())
END SUB

SUB confirm_auto_first (byval who as integer, tmask() as integer, bslot() as BattleSprite, t() as integer)
 DIM i as integer
 FOR i = 0 TO 11
  IF tmask(i) <> 0 THEN
   t(0) = i
   EXIT SUB
  END IF
 NEXT i
END SUB

FUNCTION find_preferred_target(tmask() as integer, byval who as integer, atk as AttackData, bslot() as BattleSprite) as integer

 DIM i as integer
 DIM best as integer
 DIM search as integer
 DIM found as integer
 DIM prefstat as integer
 
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
   IF atk.targ_set = 3 THEN
    'unless the target setting is random roulette, in which case random
    atk.prefer_targ = 4
   END IF
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
   found = randint(12)
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

FUNCTION quick_battle_distance(byval who1 as integer, byval who2 as integer, bslot() as BattleSprite) as integer
 ' For speed, this function only implements the exponent part of Pythagoras' formula
 ' and not the square root part of it, making the results only directly useful for
 ' quick comparisons of distance
 DIM as integer distx, disty
 distx = ABS((bslot(who1).x + bslot(who1).w \ 2) - (bslot(who2).x + bslot(who2).w \ 2))
 disty = ABS((bslot(who1).y + bslot(who1).h) - (bslot(who2).y + bslot(who2).h))
 RETURN distx ^ 2 + disty ^ 2
END FUNCTION

FUNCTION battle_distance(byval who1 as integer, byval who2 as integer, bslot() as BattleSprite) as integer
 'Returns exact distance between two battlesprites
 ' Square root is a bit slow, so don't over-use this function
 RETURN SQR(quick_battle_distance(who1, who2, bslot()))
END FUNCTION

FUNCTION targenemycount (bslot() as BattleSprite, byval for_alone_ai as integer=0) as integer
 DIM count as integer = 0
 DIM ignore as integer = NO
 FOR i as integer = 4 TO 11
  IF for_alone_ai THEN
   ignore = bslot(i).ignore_for_alone
  END IF
  IF bslot(i).stat.cur.hp > 0 AND bslot(i).vis = 1 AND ignore = NO THEN
   count = count + 1
  END IF
 NEXT i
 RETURN count
END FUNCTION

SUB loadfoe (byval slot as integer, formdata as Formation, byref bat as BattleState, bslot() as BattleSprite, byval allow_dead as integer = NO)
 '--slot is the enemy formation slot

 DIM i as integer
 
 IF formdata.slots(slot).id >= 0 THEN '-- if this slot is occupied
 
  '--load enemy data
  loadenemydata bslot(4 + slot).enemy, formdata.slots(slot).id, -1

  transfer_enemy_bits slot, bslot()
  transfer_enemy_counterattacks slot, bslot()

  '--Special handling for spawning already-dead enemies
 
  IF allow_dead = NO THEN
   'enemies which spawn already-dead should be killed off immediately
   'die without boss or 0 hp?
   IF dieWOboss(4 + slot, bslot()) OR bslot(4 + slot).enemy.stat.hp <= 0 THEN
    'rewards and spawn enemies on death
    'enemy is only partially constructed, but already have everything needed.
    DIM atktype(8) as integer 'regular "spawn on death"
    dead_enemy 4 + slot, -1, bat, bslot(), formdata
    EXIT SUB
   END IF
  END IF

  '--set up battle state
  WITH bslot(4 + slot)
   '--Size and position
   .w = sprite_sizes(1 + .enemy.size).size.x
   .h = sprite_sizes(1 + .enemy.size).size.y
   .basex = formdata.slots(slot).pos.x
   .basey = formdata.slots(slot).pos.y
   .x = .basex
   .y = .basey
   '--targetting state
   .revenge = -1
   .thankvenge = -1
   FOR i = 0 TO 11
    .revengemask(i) = NO
    .last_targs(i) = NO
    .stored_targs(i) = NO
    .thankvengemask(i) = NO
   NEXT i
   .bequesting = NO
   .active_turn_num = 0
  END WITH
  setup_non_volitile_enemy_state slot, bslot()
  
 END IF 'this slot is occupied
 
 '--if the enemy in this slot is visible
 IF bslot(4 + slot).vis = 1 THEN

  setup_enemy_sprite_and_name slot, bslot()

  '--update stats
  WITH bslot(4 + slot)
   FOR i = 0 TO 11
    .stat.cur.sta(i) = .enemy.stat.sta(i)
    .stat.max.sta(i) = .enemy.stat.sta(i)
   NEXT i
  END WITH
    
 ELSE
  '--if the enemy in this slot is not visible, mark its sprite count as 0
  bslot(4 + slot).sprites = 0
 END IF

END SUB

SUB transfer_enemy_bits(byval slot as integer, bslot() as BattleSprite)
  '--Copy elemental bits and other bits from bslot().enemy to bslot()
  
  WITH bslot(4 + slot)
   .harmed_by_cure = .enemy.harmed_by_cure
   .mp_idiot = .enemy.mp_idiot
   .is_boss = .enemy.is_boss
   .unescapable = .enemy.unescapable
   .die_without_boss = .enemy.die_without_boss
   .flee_instead_of_die = .enemy.flee_instead_of_die
   .enemy_untargetable = .enemy.enemy_untargetable
   .hero_untargetable = .enemy.hero_untargetable
   .death_unneeded = .enemy.death_unneeded
   .never_flinch = .enemy.never_flinch
   .ignore_for_alone = .enemy.ignore_for_alone
   FOR i as integer = 0 TO gen(genNumElements) - 1
    .elementaldmg(i) = .enemy.elementals(i)
   NEXT
  END WITH

END SUB

SUB transfer_enemy_counterattacks (byval slot as integer, bslot() as BattleSprite)
 '--transfer counterattacks
 WITH bslot(4 + slot)
  FOR j as integer = 0 TO gen(genNumElements) - 1
   .elem_counter_attack(j) = .enemy.elem_counter_attack(j)
  NEXT j
  FOR j as integer = 0 TO 11
   .stat_counter_attack(j) = .enemy.stat_counter_attack(j)
  NEXT j
 END WITH
END SUB

SUB setup_non_volitile_enemy_state(byval slot as integer, bslot() as BattleSprite)
 WITH bslot(slot + 4)
  .vis = 1
  .d = 0
  .dissolve = 0
  .flee = 0
  .deathtype = .enemy.dissolve - 1
  IF .deathtype = -1 THEN .deathtype = gen(genEnemyDissolve)
  .deathtime = .enemy.dissolve_length
  IF .deathtime = 0 THEN .deathtime = default_dissolve_time(.deathtype, .w, .h)
  .cursorpos.x = .w / 2 - .enemy.cursor_offset.x '--X offset is subtracted instead of added because enemies are always h-flipped
  .cursorpos.y = .enemy.cursor_offset.y
  .death_sfx = .enemy.death_sound
  .bequesting = NO
 END WITH
END SUB

SUB setup_enemy_sprite_and_name(byval slot as integer, bslot() as BattleSprite)
 '--Update sprite
 with bslot(4 + slot)
  .sprite_num = 1
  .sprites = frame_load(1 + .enemy.size, .enemy.pic)
  .pal = palette16_load(.enemy.pal, 1 + .enemy.size, .enemy.pic)
  .name = .enemy.name
 end with
END SUB

SUB changefoe(byval slot as integer, byval new_id as integer, formdata as Formation, bslot() as BattleSprite, byval hp_rule as integer, byval other_stats_rule as integer)
 IF formdata.slots(slot).id = -1 THEN
  debug "changefoe doesn't work on empty slot " & slot & " " & new_id
  EXIT SUB
 END IF
 
 formdata.slots(slot).id = new_id - 1

 '--load enemy data
 loadenemydata bslot(4 + slot).enemy, formdata.slots(slot).id, -1

 transfer_enemy_bits slot, bslot()
 transfer_enemy_counterattacks slot, bslot()

 '--update battle state
 WITH bslot(4 + slot)
  DIM old_w as integer = .w
  DIM old_h as integer = .h
  .w = sprite_sizes(1 + .enemy.size).size.x
  .h = sprite_sizes(1 + .enemy.size).size.y
  .basex = .basex + old_w / 2 - .w / 2
  .basey = .basey + old_h - .h
  .x = .x + old_w / 2 - .w / 2
  .y = .y + old_h - .h
 END WITH
 setup_non_volitile_enemy_state slot, bslot()

 setup_enemy_sprite_and_name slot, bslot()

 '--update stats
 WITH bslot(4 + slot)
  change_foe_stat bslot(4 + slot), 0, .enemy.stat.hp, hp_rule
  FOR i as integer = 1 TO 11
   change_foe_stat bslot(4 + slot), i, .enemy.stat.sta(i), other_stats_rule
  NEXT i
 END WITH
 
END SUB

SUB change_foe_stat(bspr as BattleSprite, byval stat_num as integer, byval new_max as integer, byval stat_rule as integer)
 WITH bspr.stat
  '--selectively alter current stat
  SELECT CASE stat_rule
   CASE 0 '--keep old current
   CASE 1 '--use new max
    .cur.sta(stat_num) = new_max
   CASE 2 '--preserve % of max
    IF .max.sta(stat_num) > 0 THEN
     .cur.sta(stat_num) = CINT(new_max / .max.sta(stat_num) * .cur.sta(stat_num))
    ELSE
     .cur.sta(stat_num) = new_max
    END IF
   CASE 3 '--keep old current, crop to new max
    .cur.sta(stat_num) = small(.cur.sta(stat_num), new_max)
  END SELECT
  '--always use new max stat
  .max.sta(stat_num) = new_max
 END WITH
END SUB

'There's no need to handle any lumps here which don't have effect on battles such as map data
SUB try_to_reload_files_inbattle ()
 'calls receive_file_updates
 try_reload_lumps_anywhere

 DIM i as integer = 0
 WHILE i < v_len(modified_lumps)
  DIM handled as integer = NO
  DIM basename as string = trimextension(modified_lumps[i])
  DIM extn as string = justextension(modified_lumps[i])

  'Nothing here yet!!

  IF handled THEN
   v_delete_slice modified_lumps, i, i + 1
  ELSE
   i += 1
  END IF
 WEND
END SUB
