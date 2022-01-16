'OHRRPGCE GAME - Additional mostly battle-related routines
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "bmod.bi"
#include "bmodsubs.bi"
#include "game.bi"
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
#include "scriptcommands.bi"
#include "yetmore2.bi"
#include "bcommon.bi"

DECLARE SUB confirm_auto_spread (byval who as integer, tmask() as bool, bslot() as BattleSprite, t() as integer)
DECLARE SUB confirm_auto_focus (byval who as integer, tmask() as bool, byref atk as AttackData, bslot() as BattleSprite, t() as integer)
DECLARE SUB confirm_auto_first (byval who as integer, tmask() as bool, bslot() as BattleSprite, t() as integer)

DECLARE FUNCTION quick_battle_distance(byval who1 as integer, byval who2 as integer, bslot() as BattleSprite) as integer
DECLARE FUNCTION battle_distance(byval who1 as integer, byval who2 as integer, bslot() as BattleSprite) as integer

DECLARE SUB transfer_enemy_bits(byref bspr as BattleSprite)
DECLARE SUB transfer_enemy_counterattacks (byref bspr as BattleSprite)
DECLARE SUB setup_non_volatile_enemy_state(byref bspr as BattleSprite)
DECLARE SUB reset_enemy_state(byref bspr as BattleSprite)
DECLARE SUB change_foe_stat(bspr as BattleSprite, byval stat_num as integer, byval new_max as integer, byval stat_rule as TransmogStatsRule)

DIM SHARED XY00 as XYpair

PROPERTY BattleSprite.pos() byref as XYPair
 IF sl THEN RETURN sl->Pos ELSE RETURN XY00
END PROPERTY

PROPERTY BattleSprite.x() as integer
 IF sl THEN RETURN sl->X
END PROPERTY

PROPERTY BattleSprite.x(val as integer)
 IF sl THEN sl->X = val
END PROPERTY

PROPERTY BattleSprite.y() as integer
 IF sl THEN RETURN sl->Y
END PROPERTY

PROPERTY BattleSprite.y(val as integer)
 IF sl THEN sl->Y = val
END PROPERTY

PROPERTY BattleSprite.z() as integer
 IF sprite THEN RETURN -sprite->Y
END PROPERTY

PROPERTY BattleSprite.z(val as integer)
 IF sprite THEN sprite->Y = -val
END PROPERTY

PROPERTY BattleSprite.size() byref as XYPair
 IF sl THEN RETURN sl->Size ELSE RETURN XY00
END PROPERTY

PROPERTY BattleSprite.w() as integer
 IF sl THEN RETURN sl->Width
END PROPERTY

PROPERTY BattleSprite.w(val as integer)
 IF sl THEN sl->Width = val
END PROPERTY

PROPERTY BattleSprite.h() as integer
 IF sl THEN RETURN sl->Height
END PROPERTY

PROPERTY BattleSprite.h(val as integer)
 IF sl THEN sl->Height = val
END PROPERTY

PROPERTY BattleSprite.frame() as integer
 IF sprite ANDALSO sprite->SliceType = slSprite THEN
  RETURN sprite->SpriteData->frame
 END IF
END PROPERTY

PROPERTY BattleSprite.frame(fr as integer)
 IF sprite THEN ChangeSpriteSlice sprite, , , , fr
END PROPERTY

FUNCTION BattleSprite.deathtime() as integer
 IF _deathtime <= 0 THEN RETURN default_dissolve_time(deathtype, w, h)
 RETURN _deathtime
END FUNCTION

FUNCTION BattleSprite.appeartime() as integer
 IF _appeartime <= 0 THEN RETURN default_dissolve_time(appeartype, w, h)
 RETURN _appeartime
END FUNCTION

FUNCTION is_hero(byval who as integer) as bool
 IF who >= 0 AND who <= 3 THEN RETURN YES
 RETURN NO
END FUNCTION

FUNCTION is_enemy(byval who as integer) as bool
 IF who >= 4 AND who <= 11 THEN RETURN YES
 RETURN NO
END FUNCTION

FUNCTION is_attack(byval who as integer) as bool
 IF who >= 12 AND who <= 23 THEN RETURN YES
 RETURN NO
END FUNCTION

FUNCTION is_weapon(byval who as integer) as bool
 IF who = 24 THEN RETURN YES
 RETURN NO
END FUNCTION

'==============================================================================
'                                Attack costs
'
'See also attack_cost_info in bcommon.bas

'In-battle function
'--attackerslot = bslot() index of hero or enemy who is attacking
'--spclass  = 0 for normal attacks, 1 for level-MP spells
'--lmplev   = which level-MP level to use
FUNCTION atkallowed (attack as AttackData, attackerslot as integer, spclass as integer, lmplev as integer, bslot() as BattleSprite) as bool

 DIM byref attacker as BattleSprite = bslot(attackerslot)

 '--check for mutedness
 IF attack.mutable AND attacker.stat.cur.mute < attacker.stat.max.mute THEN
  RETURN NO
 END IF

 '--Check for sufficient MP (You can cast with negative MP as long as the attack costs no MP)
 IF large(attacker.stat.cur.mp, 0) - focuscost(attack.mp_cost, attacker.stat.cur.focus) < 0 THEN
  RETURN NO
 END IF

 'NOTE: hp_cost is not checked!
 'NOTE: money_cost is not checked!

 '--check for level-MP (heroes only)
 IF attackerslot <= 3 AND spclass = 1 THEN
  IF gam.hero(attackerslot).levelmp(lmplev) <= 0 THEN
   RETURN NO
  END IF
 END IF

 '--check for sufficient items
 DIM itemid as integer
 DIM itemcount as integer
 FOR i as integer = 0 TO UBOUND(attack.item)
  itemid = attack.item(i).id
  itemcount = attack.item(i).number
  IF itemid > 0 THEN 'this slot is used
   ' Only hero items are checked
   ' However if an enemy uses this attack, it will add/subtract items from the player!
   IF attackerslot <= 3 THEN
    IF countitem(itemid - 1) < itemcount THEN
     'yes, this still works for adding items.
     RETURN NO
    END IF
   END IF
  END IF
 NEXT i

 '--succeed
 RETURN YES
END FUNCTION

'Out-of-battle function
'Note: this function is used only for the OOB Spells menu, not for "map cure", etc.
'See also chkOOBtarg, which is used for "map cure".
'attackerslot = party slot of hero who wants to cast the spell
'spclass  = 0 for normal attacks, 1 for level-MP spells
FUNCTION atkallowed(attack as AttackData, attackerslot as integer, spclass as integer, lmplev as integer) as bool

 DIM byref attacker as HeroState = gam.hero(attackerslot)

 DIM cost as integer = focuscost(attack.mp_cost, attacker.stat.cur.focus)
 IF spclass = 0 AND attacker.stat.cur.mp < cost THEN
  RETURN NO
 END IF
 IF spclass = 1 AND attacker.levelmp(lmplev) = 0 THEN
  RETURN NO
 END IF
 IF attacker.stat.cur.hp <= 0 THEN
  RETURN NO
 END IF

 RETURN YES
END FUNCTION

'In-battle routine
SUB subtract_attack_costs(attack as AttackData, attackerslot as integer, byref bat as BattleState, bslot() as BattleSprite)
 DIM byref attacker as BattleSprite = bslot(attackerslot)

 '--if the attack costs MP, we want to actually consume MP
 IF attack.mp_cost > 0 THEN
  WITH attacker
   .stat.cur.mp = large(.stat.cur.mp - focuscost(attack.mp_cost, .stat.cur.focus), 0)
  END WITH
 END IF

 '--Level-MP
 WITH attacker
  IF attackerslot <= 3 AND .consume_lmp >= 0 THEN
   gam.hero(attackerslot).levelmp(.consume_lmp) -= 1
   .consume_lmp = -1
  END IF
 END WITH

 '--ditto for HP
 IF attack.hp_cost > 0 THEN
   WITH attacker
     .stat.cur.hp = large(.stat.cur.hp - attack.hp_cost, 0)
     .harm.ticks = gen(genDamageDisplayTicks)
     .harm.pos.x = .x + (.w * .5)
     .harm.pos.y = .y + (.h * .5)
     .harm.text = STR(attack.hp_cost)
   END WITH
   'This triggerfade is needed because the hp cost might have killed the attacker
   triggerfade attackerslot, bslot()
 END IF

 '--ditto for money
 IF attack.money_cost <> 0 THEN
   gold = large(gold - attack.money_cost, 0)
   WITH attacker
     .harm.ticks = gen(genDamageDisplayTicks)
     .harm.pos.x = .x + (.w * .5)
     .harm.pos.y = .y + (.h * .5)
     .harm.text = price_string(ABS(attack.money_cost))
     IF attack.money_cost < 0 THEN .harm.text = "+" & .harm.text
   END WITH
   IF gold > 2000000000 THEN gold = 2000000000
   IF gold < 0 THEN gold = 0
 END IF

 '--if the attack consumes items, we want to consume those too
 FOR i as integer = 0 TO UBOUND(attack.item)
  WITH attack.item(i)
   IF .id > 0 THEN 'this slot is used
    IF .number > 0 THEN 'remove items
     delitem(.id - 1, .number)
    ELSEIF .number < 0 THEN 'add items
     getitem(.id - 1, abs(.number))
    END IF
    'Update tags when items have changed because it could affect chain conditionals
    evalitemtags
   END IF
  END WITH
 NEXT i
END SUB

'Out-of-battle routine
SUB subtract_attack_costs(attack as AttackData, attackerslot as integer, spclass as integer, lmplev as integer)
 DIM byref attacker as HeroState = gam.hero(attackerslot)

 '--deduct/add MP
 DIM cost as integer
 WITH attacker.stat
  cost = focuscost(attack.mp_cost, .cur.focus)
  'Don't increase MP past max, but if it is already past max don't clamp it.
  .cur.mp = bound(.cur.mp - cost, 0, large(.max.mp, .cur.mp))
 END WITH
 IF spclass = 1 THEN
  '--deduct LMP
  attacker.levelmp(lmplev) -= 1
 END IF
END SUB


'==============================================================================

'Returns 0 for fail, id+1 on success. (item and rareitem are non-offset ids)
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

'Count number of attacks in one of an enemy's attack lists
'ai: which attack list
FUNCTION count_attacks_in_ai_list (byval ai as EnemyAIEnum, byval slot as integer, bslot() as BattleSprite) as integer
 DIM atk as AttackData
 DIM atk_id as integer
 DIM count as integer = 0
 WITH bslot(slot).enemy
  FOR i as integer = 0 TO 4
   atk_id = -1
   SELECT CASE ai
    CASE aiNormal: atk_id = .regular_ai(i) - 1
    CASE aiWeak:   atk_id = .desperation_ai(i) - 1
    CASE aiAlone:  atk_id = .alone_ai(i) - 1
   END SELECT
   IF atk_id >= 0 THEN
    loadattackdata atk, atk_id
    IF atkallowed(atk, slot, 0, 0, bslot()) THEN
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

'Do an attack (in-battle only).
'Handled here:
'- hit or miss or fail
'- stat reset bits
'- damage calculation and inflict
'- stat bounds (0 to (optionally) max), but not stat caps
'- harm text
'- records stored, last, revenge & thankvenge targets & damage
'Not handled here:
'- 'instead' chains
'- costs
'- counterattacks and spawning
'- other non-damaging effects like tags, force-run, erase rewards
'- death checks
'- stat caps (bug 980)
FUNCTION inflict (byref h as integer = 0, byref targstat as integer = 0, attackerslot as integer, targetslot as integer, byref attacker as BattleSprite, byref target as BattleSprite, attack as AttackData, tcount as integer) as AttackResult

 attacker.attack_succeeded = NO
 
 'remember this target
 attacker.last_targs(targetslot) = YES
 
 'stored targs
 IF attack.store_targ THEN
  attacker.stored_targs(targetslot) = YES
  attacker.stored_targs_can_be_dead = attack_can_hit_dead(attackerslot, attack)
 END IF
 IF attack.delete_stored_targs THEN
  FOR i as integer = 0 TO UBOUND(attacker.stored_targs)
   attacker.stored_targs(i) = NO
  NEXT i
  attacker.stored_targs_can_be_dead = NO
 END IF
 
 'If not 'no damage' (which skips hit/miss/fail checking)
 IF attack.damage_math <> 4 THEN
  DIM target_is_register as bool = NO
 
  'init
  WITH target
   .harm.text = ""
   .harm.ticks = gen(genDamageDisplayTicks)
   .harm.col = uilook(uiBattleDamage)
   .harm.pos.x = .x + (.w * .5)
   .harm.pos.y = .y + (.h * .5)
  
   targstat = bound(attack.targ_stat, 0, UBOUND(.stat.cur.sta))
   'backcompat MP-targstat
   IF attack.obsolete_damage_mp THEN
    IF targstat = statHP THEN targstat = statMP
   END IF
   IF targstat > statLast AND targstat <= statLastRegister THEN target_is_register = YES
  END WITH
 
  'accuracy
  DIM acc as integer = attacker.stat.cur.acc
  DIM dog as integer = target.stat.cur.dog
  DIM accmult as single = 1
  DIM dogmult as single = .25 'dodge modifier
  IF attack.aim_math = 1 THEN dogmult = 0.5
  IF attack.aim_math = 2 THEN dogmult = 1.0
  IF attack.aim_math = 4 THEN dogmult = 1.25
  IF attack.aim_math = 4 OR attack.aim_math = 7 OR attack.aim_math = 8 THEN
   acc = attacker.stat.cur.mag
   dog = target.stat.cur.wil
  ELSEIF attack.aim_math > 8 THEN
   accmult = attack.acc_mult
   dogmult = attack.dog_mult
   SELECT CASE attack.base_acc_stat
    CASE -1
     acc = 1
    CASE 0 TO statLast
     acc = attacker.stat.cur.sta(attack.base_acc_stat)
    CASE 256 TO 256 + statLast
     acc = target.stat.cur.sta(attack.base_acc_stat - 256)
    CASE 512 TO 512 + statLast
     acc = attacker.stat.cur.sta(attack.base_acc_stat - 512)
     accmult = accmult / tcount
    CASE ELSE
     debug "Invalid Base ACC stat"
   END SELECT
   SELECT CASE attack.base_dog_stat
    CASE -1
     dog = 1
    CASE 0 TO statLast
     dog = attacker.stat.cur.sta(attack.base_acc_stat)
    CASE 256 TO 256 + statLast
     dog = target.stat.cur.sta(attack.base_acc_stat - 256)
    CASE ELSE
     debug "Invalid Base DOG stat"
   END SELECT
  END IF
 
  DIM attackhit as integer
  attackhit = range(acc, 75) >= range(dog * dogmult, 75)
  IF attack.aim_math = 3 THEN attackhit = YES
  IF attack.aim_math = 5 OR attack.aim_math = 7 THEN attackhit = randint(100) < (acc * (100 - dog)) / 100
  IF attack.aim_math = 6 OR attack.aim_math = 8 THEN attackhit = randint(100) < acc
  IF attack.aim_math = 9 THEN attackhit = range(acc * accmult + attack.aim_extra, 75) >= range(dog * dogmult, 75)
  IF attack.aim_math = 10 THEN attackhit = randint(100) < attack.aim_extra + (acc * accmult * (100 - dog * dogmult)) / 100
  IF attack.aim_math = 11 THEN attackhit = rando() < (1 + erf(attack.aim_extra + acc * accmult - dog * dogmult)) / 2
  IF attack.aim_math = 12 THEN attackhit = randint(100) < ((attack.aim_extra + acc * accmult) * (100 - dog * dogmult)) / 100
  IF attackhit = NO THEN
   RETURN atkMiss
  END IF

  WITH target
   IF attack.fail_if_targ_poison = YES AND .stat.cur.poison < .stat.max.poison THEN
    RETURN atkFail
   END IF
   IF attack.fail_if_targ_regen = YES AND .stat.cur.regen < .stat.max.regen THEN
    RETURN atkFail
   END IF
   IF attack.fail_if_targ_stun = YES AND .stat.cur.stun <> .stat.max.stun THEN
    RETURN atkFail
   END IF
   IF attack.fail_if_targ_mute = YES AND .stat.cur.mute <> .stat.max.mute THEN
    RETURN atkFail
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
    ap = attacker.stat.max.sta(attack.base_atk_stat - 35)
   CASE 47 TO 58
    ap = target.stat.max.sta(attack.base_atk_stat - 47)
   CASE 59
    ap = tcount * 100
   CASE 60
    ap = attacker.stat.max.mp - attacker.stat.cur.mp
   CASE 61
    ap = target.stat.max.mp - target.stat.cur.mp
   CASE 62
    ap = target.stat.max.hp - target.stat.cur.hp
   CASE IS >= 63
    debug "Unknown base stat " & attack.base_atk_stat & " for attack " & attack.id
  END SELECT
 
  '--defense base
  IF attack.base_def_stat > 0 AND attack.base_def_stat <= UBOUND(target.stat.cur.sta) + 1 THEN dp = target.stat.cur.sta(attack.base_def_stat - 1)
 
  'Attack and Defense multipliers
  DIM as double atkmult, defmult
  IF attack.damage_math = 0 THEN atkmult = 1.0 : defmult = 0.5  'atk*.8-def*.1
  IF attack.damage_math = 1 THEN atkmult = 0.8 : defmult = 0.1  'atk*.8-def*.1
  IF attack.damage_math = 2 THEN atkmult = 1.3 : defmult = 1.0  'atk-1.3-def
  IF attack.damage_math = 3 THEN atkmult = 1.0 : defmult = 0.0  'atk
  ' damage_math = 4 is No Damage
  ' damage_math = 5 is set/damage % of max (special handling)
  ' damage_math = 6 is set/damage % of current (special handling)
  IF attack.damage_math = 7 THEN atkmult = attack.atk_mult : defmult = attack.def_mult
 
  'Temporarily use floating point for damage
  DIM harmf as double = ap * atkmult - dp * defmult
  DIM elemental_absorb as bool = NO
  DIM immune as bool = NO
 
  'elemental damage, and also failure conditions
  FOR i as integer = 0 TO gen(genNumElements) - 1
   IF attack.elemental_damage(i) = YES THEN
    harmf *= ABS(target.elementaldmg(i))
    'What's a good cut off for immunity? When we switch to 32bit HP values, maybe
    'you'll want to be able to do 1/1000,000 normal damage without triggering immunity?
    IF ABS(target.elementaldmg(i)) < 0.000005 THEN
     immune = YES
    ELSEIF target.elementaldmg(i) < 0.0 THEN
     elemental_absorb = YES  'absorb
    END IF
   END IF
   WITH attack.elemental_fail_conds(i)
    DIM fail as bool = NO
    DIM effectiveval as single = target.elementaldmg(i)
    DIM t as CompType = .comp AND 15
    IF .comp AND 16 THEN effectiveval = ABS(effectiveval)
    IF .comp AND 64 THEN effectiveval = rando() * effectiveval
    IF t = compLt OR t = compLe THEN
     '"Simulate old fail vs element resist bit":
     'The old bit checked only the target's Strong bits, ignoring their Absorb bits
     '(For consistency, this bit also affects compLe, although it didn't exist at the time.
     'But we can't 100% safely apply it to compGt, because we didn't used to do that in the past.)
     IF prefbit(25) THEN effectiveval = ABS(effectiveval)
    END IF
    IF t = compLt THEN fail = (effectiveval < .value - 0.000005)
    IF t = compLe THEN fail = (effectiveval <= .value + 0.000005)
    IF t = compGt THEN fail = (effectiveval > .value + 0.000005)
    IF t = compGe THEN fail = (effectiveval >= .value - 0.000005)
    IF fail THEN
     IF .comp AND 32 THEN
      RETURN atkMiss
     ELSE
      RETURN atkFail
     END IF
    END IF
   END WITH
  NEXT

  'extra damage
  harmf *= (1.0 + attack.extra_damage / 100)

  'Now we convert damage back to integer (for backcompat)
  h = bound(harmf, 1. * LONG_MIN, 1. * LONG_MAX)

  ' + or - some amount. (Note: due to how range is programmed, the average
  ' result is 0.5 less than h)
  h = range(h, attack.randomization)

  'spread damage
  IF attack.divide_spread_damage = YES THEN h = h / tcount

  'cap under
  IF immune ANDALSO prefbit(26) THEN
   '"0 damage when immune to attack elements", even without attack.damage_can_be_zero
   h = 0
  ELSEIF h <= 0 THEN
   IF attack.damage_can_be_zero = NO THEN h = 1 ELSE h = 0
  END IF

  DIM byref tstat_cur as integer = target.stat.cur.sta(targstat)
  DIM       tstat_max as integer = target.stat.max.sta(targstat)
  DIM byref astat_cur as integer = attacker.stat.cur.sta(targstat)
  DIM       astat_max as integer = attacker.stat.max.sta(targstat)

  IF attack.show_damage_without_inflicting = NO THEN
   'resetting
   IF attack.reset_targ_stat_before_hit = YES THEN
    tstat_cur = tstat_max
   END IF
  END IF

  'remember target stat
  DIM tstat_original as integer = tstat_cur
  DIM astat_original as integer = astat_cur

  IF attack.percent_damage_not_set = YES THEN
   'percentage attacks do damage
   'Note that even with this bitset, these damage types ignore lots of stuff like randomization.
   'It was once suggested (see bug 134) to move this block up the function, but
   'we decided to just leave it as it is; you should use a different damage type
   'with the target's stat as Base ATK Stat instead.
   SELECT CASE attack.damage_math
    CASE 5'% of max
     h = tstat_max + (attack.extra_damage * tstat_max / 100)
     elemental_absorb = NO
    CASE 6'% of cur
     h = tstat_cur + (attack.extra_damage * tstat_cur / 100)
     elemental_absorb = NO
   END SELECT
  END IF

  'h should always be nonnegative at this point
  IF h < 0 THEN debug "inflict: negative h!"

  IF elemental_absorb THEN
   h *= -1      'elemental absorb
  ELSEIF attack.cure_instead_of_harm = YES AND target.harmed_by_cure = NO THEN
   h *= -1      'cure bit
  END IF

  DIM capdamage as bool = YES

  IF attack.percent_damage_not_set = NO THEN
   'percentage attacks set stat
   'and by set, we really mean set, ignore nearly all attack settings,
   'that's my interpretation of intent anyway - TMC
   '...And mine to. - James
   SELECT CASE attack.damage_math
    CASE 5'% of max
     h = tstat_cur - (tstat_max + (attack.extra_damage * tstat_max / 100))
     capdamage = NO
    CASE 6'% of cur
     h = tstat_cur - (tstat_cur + (attack.extra_damage * tstat_cur / 100))
     capdamage = NO
   END SELECT
  END IF

  'Maybe cap damage to max possible damage/cure
  'Note that unlike the damage cap, this still happens if not inflicting
  IF attack.do_not_exceed_targ_stat AND capdamage THEN
   IF h > 0 THEN 'damage
    h = small(h, tstat_cur)
   ELSEIF h < 0 THEN ' cure
    'Note: this cure can no exceed max regardless of "allow cure to exceed maximum" bit
    DIM diff as integer = tstat_max - tstat_cur
    IF diff >= 0 THEN
     h = large(h, diff * -1)
    END IF
   END IF
  END IF

  'inflict
  IF attack.show_damage_without_inflicting = NO THEN
   'I think that not applying the damage cap when only showing damage is pretty dodgy...
   IF gen(genDamageCap) > 0 AND capdamage THEN
    IF h > gen(genDamageCap) THEN h = gen(genDamageCap)
    IF h < -gen(genDamageCap) THEN h = -gen(genDamageCap)
   END IF

   tstat_cur = safesubtract(tstat_cur, h)
   IF attack.absorb_damage THEN
    WITH attacker
     '--drain
     DIM absorb_amount as integer = h * attack.absorb_rate
     astat_cur += absorb_amount
     IF attack.dont_display_damage = NO THEN
      .harm.text = STR(ABS(absorb_amount))
      IF h > 0 THEN .harm.text = "+" + .harm.text
     END IF
     .harm.ticks = gen(genDamageDisplayTicks)
     .harm.col = uilook(uiBattleAbsorb)
     .harm.pos.x = .x + (.w * .5)
     .harm.pos.y = .y + (.h * .5)
    END WITH
   END IF

   IF attack.poison_is_negative_regen AND (targstat = statPoison OR targstat = statRegen) THEN
    'Healing poison causes regen, and vice versa
    DIM negatedstat as integer = IIF(targstat = statPoison, statRegen, statPoison)
    ' For both of target.stat and attacker.stat:
    FOR itr as integer = 0 TO 1
     WITH *IIF(itr, @target.stat, @attacker.stat)
      DIM abovemax as integer
      ' Healing poison register above max becomes regen and vice versa
      abovemax = .cur.sta(targstat) - .max.sta(targstat)
      IF abovemax > 0 THEN
       .cur.sta(negatedstat) -= abovemax
       .cur.sta(targstat) = .max.sta(targstat)
      END IF
     END WITH
    NEXT
   END IF

   'enforce stat bounds
   tstat_cur = large(tstat_cur, 0)
   astat_cur = large(astat_cur, 0)
   IF target_is_register OR attack.allow_cure_to_exceed_maximum = NO THEN
    'Cap to max. But if the stat was already above max then instead don't allow
    'it to go higher.
    tstat_cur = small(tstat_cur, large(tstat_max, tstat_original))
    astat_cur = small(astat_cur, large(astat_max, astat_original))
   END IF

   'FIXME: stat caps aren't observed (bug 980)
   'HOWEVER, when bug 980 is fixed, need to be careful not to break out-of-battle permanent stat
   'boosting items, which rely on the caps being exceeded, then update hero base stats as a result.
   'See oobcure.

   'remember revenge data
   'NOTE: revenge & thankvenge record difference AFTER "reset target stat to max" takes effect
   IF tstat_original > tstat_cur THEN
    target.revengemask(attackerslot) = YES
    target.revenge = attackerslot
    target.revengeharm = tstat_original - tstat_cur
    attacker.repeatharm = tstat_original - tstat_cur
   END IF

   'remember thankvenge data
   IF tstat_original < tstat_cur THEN
    target.thankvengemask(attackerslot) = YES
    target.thankvenge = attackerslot
    target.thankvengecure = ABS(tstat_original - tstat_cur)
   END IF
  END IF

  'set damage display
  IF attack.dont_display_damage = NO THEN
   target.harm.text = STR(ABS(h))
   IF h < 0 THEN
    '--if cure, show + sign and change color
    target.harm.text = "+" + target.harm.text
    target.harm.col = uilook(uiBattleHeal)
   END IF
   IF attack.damage_color > 0 THEN
    target.harm.col = attack.damage_color - 1
   END IF

  END IF 'attack.show_damage_without_inflicting = NO

 END IF 'skips to here for No Damage attacks

 'remember "Counter" target-class data
 target.counter_target = attackerslot

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
 attacker.attack_succeeded = YES
 RETURN atkHit

END FUNCTION

FUNCTION liveherocount (bslot() as BattleSprite) as integer
 '--with bslot() counts heros in-battle HP state
 DIM i as integer = 0
 FOR o as integer = 0 TO 3
  IF gam.hero(o).id >= 0 AND bslot(o).stat.cur.hp > 0 THEN i = i + 1
 NEXT o
 RETURN i
END FUNCTION

FUNCTION liveherocount () as integer
 '--with no argument, counts live heroes in outside-of-battle active party
 DIM liveheroes as integer = 0
 FOR o as integer = 0 TO 3
  IF gam.hero(o).id >= 0 THEN
   IF gam.hero(o).stat.cur.hp > 0 OR gam.hero(o).stat.max.hp <= 0 THEN liveheroes += 1
  END IF
 NEXT o
 RETURN liveheroes
END FUNCTION

'TODO: no need to limit to 16 bit arithmetic. Also
'at the moment there's no need for this function at all.
'But if we change to 32 bit stats then this must be changed
'to use long longs instead of doubles.
FUNCTION safesubtract (byval number as integer, byval minus as integer) as integer
 DIM longnumber as double = number
 DIM longminus as double = minus
 DIM longresult as double = longnumber - longminus
 IF longresult > 32767 THEN longresult = 32767
 IF longresult < -32768 THEN longresult = -32768
 DIM result as integer = longresult
 RETURN result
END FUNCTION

'TODO: See safesubtract
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
 embedtext(bat.caption)
 bat.caption_time = captime
 bat.caption_delay = capdelay
END SUB

'This picks out the targets which are within a 90 degree wide sector in the
'specified direction. Returns number of in-range targets.
FUNCTION battle_target_arrows_sector_mask (inrange() as integer, byval d as integer, byval axis as integer, bslot() as BattleSprite, targ as TargettingState, foredistance() as integer, sidedistance() as integer) as integer
 DIM as integer xdistance, ydistance, count
 FOR i as integer = 0 TO 11
  IF targ.mask(i) THEN
   ydistance = bslot(i).y - bslot(targ.pointer).y
   xdistance = bslot(i).x - bslot(targ.pointer).x
   IF axis THEN
    foredistance(i) = ydistance * d
    sidedistance(i) = ABS(xdistance)
   ELSE
    foredistance(i) = xdistance * d
    sidedistance(i) = ABS(ydistance)
   END IF
   IF foredistance(i) > 0 THEN
    'Angle in degrees from the forward direction. Always nonnegative
    DIM angle as double = ATAN2(sidedistance(i), foredistance(i)) * 180 / M_PI
    '? "angle from " & bslot(targ.pointer).pos & " to " & i & " " & bslot(i).pos & " is " & angle & " foredist " & foredistance(i) & " sidedist " & sidedistance(i) & IIF(angle <= 45, " IN SECTOR", "")
    'WARNING: allowing angles >= 45 can result in unselectable targets
    IF angle < 45 THEN
     'To also allow targets within a 40x40 box:  ORELSE sidedistance(i) < 20
     setbit inrange(), 0, i, 1
     count += 1
    END IF
   END IF
  END IF
 NEXT i
 RETURN count
END FUNCTION

'Move targ.pointer, the target currently selected by the player, or turn on/off optional spread.
'axis: 0 for x (left/right), 1 for y (up/down) movement
'd: -1 (left/up) or 1 (right/down)
SUB battle_target_arrows (byval d as integer, byval axis as integer, bslot() as BattleSprite, targ as TargettingState, byval allow_spread as bool = NO)
 DIM newptr as integer = targ.pointer

 'First, special case for target at same position as current:
 'Right and Down keys loop over them from lowest to highest index, and Left and Up keys
 'from highest to lowest. Only after looping past the last one do we do normal target selection.
 DIM idx as integer = targ.pointer
 FOR i as integer = 1 TO 11
  idx += d  'search through slots according to direction, but don't loop
  IF idx < 0 OR idx > UBOUND(targ.mask) THEN EXIT FOR
  IF targ.mask(idx) = NO THEN CONTINUE FOR
  IF bslot(idx).pos = bslot(targ.pointer).pos THEN
   targ.pointer = idx
   EXIT SUB
  END IF
 NEXT

 'Look for a nearby target within a 90 degree wide sector in the right direction
 DIM foredistance(11) as integer
 DIM sidedistance(11) as integer
 DIM inrange(0) as integer
 DIM best as integer = 99999
 IF battle_target_arrows_sector_mask(inrange(), d, axis, bslot(), targ, foredistance(), sidedistance()) THEN
  'At least one target is in the sector, pick the closest
  FOR i as integer = 0 TO 11
   IF readbit(inrange(), 0, i) THEN
    IF foredistance(i) < best THEN
     best = foredistance(i)
     newptr = i
    END IF
   END IF
  NEXT i
 ELSE
  'If there's none, allow targets which are at any angle, and pick the closest one
  FOR i as integer = 0 TO 11
   IF targ.mask(i) THEN
    IF foredistance(i) > 0 THEN
     DIM dist as integer = foredistance(i) + sidedistance(i)  'Both are non-negative
     IF dist < best THEN
      best = dist
      newptr = i
     END IF
    END IF
   END IF
  NEXT i
 END IF 

 IF newptr = targ.pointer THEN
  'Spread attack
  IF allow_spread = YES AND targ.opt_spread = 1 THEN
   FOR i as integer = 0 TO 11
    targ.selected(i) = targ.mask(i)
   NEXT i
   targ.opt_spread = 2
  END IF
 END IF
 targ.pointer = newptr
END SUB

FUNCTION targetmaskcount (tmask() as bool) as integer
 DIM n as integer = 0
 FOR i as integer = 0 TO UBOUND(tmask)
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

FUNCTION trytheft (bat as BattleState, byval who as integer, byval targ as integer, attack as AttackData, bslot() as BattleSprite) as bool
 IF is_hero(who) ANDALSO is_enemy(targ) THEN
  '--a hero is attacking an enemy
  IF attack.can_steal_item THEN
   '--steal bitset is on for this attack
   WITH bslot(targ).enemy.steal
    IF .thievability >= 0 ANDALSO (.item_rate > 0 ORELSE .rare_item_rate > 0) THEN
     '--enemy is theftable
     DIM stole as integer = checktheftchance(.item, .item_rate, .rare_item, .rare_item_rate)
     IF stole THEN
      '--success!
      IF .thievability = 0 THEN
       '--only one theft permitted
       .thievability = -1
      END IF
      setbatcap bat, readglobalstring(117, "Stole", 30) + " " + readitemname(stole - 1), 40, 0
      menusound gen(genStealSuccessSFX)
      RETURN YES '--success
     ELSE
      '--steal failed
      setbatcap bat, readglobalstring(114, "Cannot Steal", 30), 40, 0
      menusound gen(genStealFailSFX)
     END IF
    ELSE
     '--has nothing to steal / steal disabled
     setbatcap bat, readglobalstring(111, "Has Nothing", 30), 40, 0
     menusound gen(genStealNoItemSFX)
    END IF
   END WITH
  END IF
 END IF
 RETURN NO '--return false by default
END FUNCTION

FUNCTION hero_total_exp (byval hero_slot as integer) as integer
 RETURN total_exp_to_level(gam.hero(hero_slot).lev, gam.hero(hero_slot).exp_mult) + gam.hero(hero_slot).exp_cur
END FUNCTION

SUB updatestatslevelup (byval hero_slot as integer, byval allowforget as bool)
 ' hero_slot = who
 ' allowforget = forget spells if level dropped below requirement

 WITH gam.hero(hero_slot)

  'wipe learnmask for this hero
  'note that this gets wiped again later, but that is okay.
  'this makes sure that learnmask gets cleared for this battle
  'even if the hero *doesn't* get a level-up.
  flusharray .learnmask()

  'THIS PART UPDATES STATS FOR A LEVEL UP
  IF .lev_gain THEN

   DIM her as HeroDef
   loadherodata her, gam.hero(hero_slot).id

   'stat increase/decrease
   FOR statnum as integer = 0 TO statLast
    DIM n0 as integer = her.Lev0.sta(statnum)
    DIM nMax as integer = her.LevMax.sta(statnum)
    DIM statgain as integer
    statgain = atlevel(.lev, n0, nMax) - atlevel(.lev - .lev_gain, n0, nMax)
    .stat.base.sta(statnum) += statgain
   NEXT

   IF prefbit(9) THEN  '"Simulate Old Levelup bonus-accretion bug"
    DIM bonuses(statLast) as integer
    hero_total_equipment_bonuses hero_slot, bonuses()
    FOR statnum as integer = 0 TO statLast
     .stat.base.sta(statnum) += bonuses(statnum) * .lev_gain
    NEXT
   END IF 

   recompute_hero_max_stats hero_slot

   'stat restoration
   IF prefbit(2) = NO THEN '"Don't restore HP on level-up" OFF
    '--HP restoration
    .stat.cur.hp = .stat.max.hp 'set external cur to external max
   END IF
   IF prefbit(3) = NO THEN '"Don't restore MP on level-up" OFF
    '--MP restoration
    .stat.cur.mp = .stat.max.mp 'set external cur to external max
    reset_levelmp gam.hero(hero_slot)
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
 WITH gam.hero(hero_slot)
  FOR slot as integer = 0 TO UBOUND(.equip)
   IF .equip(slot).id >= 0 THEN
    loaditemdata itembuf(), .equip(slot).id
    FOR statnum as integer = 0 TO statLast
     bonuses(statnum) += itembuf(54 + statnum)
    NEXT statnum
   END IF
  NEXT slot
 END WITH
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
   IF prefbit(43) THEN
    'Cap minimum stats at zero
    .max.sta(statnum) = large(.max.sta(statnum), 0)
    .cur.sta(statnum) = large(.cur.sta(statnum), 0)
   END IF
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

SUB learn_spells_for_current_level(byval who as integer, byval allowforget as bool)

 'Teaches all spells that can be learned from level
 ' up to the hero's current level.
 '(but should not overwrite any spells learned with "write spell")

 'wipe learnmask for this hero
 flusharray gam.hero(who).learnmask()

 DIM her as HeroDef
 loadherodata her, gam.hero(who).id

 'learn spells
 FOR j as integer = 0 TO 3
  FOR o as integer = 0 TO 23
   WITH her.spell_lists(j,o)
    '--if slot is empty and slot accepts a spell and learn-by-level condition is true
    IF gam.hero(who).spells(j, o) = 0 AND .attack > 0 AND .learned - 1 <= gam.hero(who).lev AND .learned > 0 THEN
     gam.hero(who).spells(j, o) = .attack
     setbit gam.hero(who).learnmask(), 0, j * 24 + o, 1
    END IF
    IF allowforget THEN
     '--plotscripts may lower level, forget spells if drop below requirement and know the spell specified
     IF gam.hero(who).spells(j, o) = .attack AND .learned - 1 > gam.hero(who).lev THEN
      gam.hero(who).spells(j, o) = 0
     END IF
    END IF
   END WITH
  NEXT o
 NEXT j
 
END SUB

FUNCTION allowed_to_gain_levels(byval heroslot as integer) as bool
 IF heroslot < 0 THEN RETURN NO 'out of range
 IF heroslot > UBOUND(gam.hero) THEN RETURN NO ' out of range
 IF gam.hero(heroslot).id = -1 THEN RETURN NO ' no hero in this slot
 IF gam.hero(heroslot).lev >= current_max_level THEN RETURN NO
 RETURN YES
END FUNCTION

'Returns true if hero gained XP
FUNCTION giveheroexperience (byval who as integer, byval exper as integer) as bool
 WITH gam.hero(who)
  'reset levels gained
  .lev_gain = 0
  IF allowed_to_gain_levels(who) THEN
   .exp_cur += exper
   'levelups
   WHILE .exp_cur >= .exp_next
    .exp_cur -= .exp_next
    .lev += 1 'current level
    .lev_gain += 1 'levelup counter
    .exp_next = exptolevel(.lev + 1, .exp_mult)
    IF .lev >= current_max_level THEN
     'You can't gain experience once you've hit the level cap
     .exp_cur = 0
     EXIT WHILE
    END IF
   WEND
   RETURN YES
  END IF
 END WITH
END FUNCTION

SUB setheroexperience (byval who as integer, byval amount as integer, byval allowforget as bool)
 'unlike giveheroexperience, this can cause delevelling
 DIM orig_lev as integer = gam.hero(who).lev
 DIM total as integer = 0
 DIM lostlevels as bool = NO
 
 FOR i as integer = 0 TO gam.hero(who).lev - 1
  total += exptolevel(i + 1, gam.hero(who).exp_mult)
 NEXT
 IF total > amount THEN
  'losing levels; lvl up from level 0
  gam.hero(who).lev = 0
  gam.hero(who).exp_next = exptolevel(1, gam.hero(who).exp_mult)
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
  flusharray gam.hero(who).learnmask()
 END IF
 'Update tags
 evalherotags
 tag_updates
END SUB

FUNCTION visibleandalive (byval who as integer, bslot() as BattleSprite) as bool
 RETURN (bslot(who).vis ANDALSO bslot(who).stat.cur.hp > 0)
END FUNCTION

SUB export_battle_hero_stats (bslot() as BattleSprite)
 'Export a few specific hero battle stats to the out-of-battle party
 'This may be used frequently in battle
 FOR i as integer = 0 TO 3
  IF gam.hero(i).id >= 0 THEN
   '--set out-of-battle HP and MP equal to in-battle HP and MP
   gam.hero(i).stat.cur.hp = bslot(i).stat.cur.hp
   gam.hero(i).stat.cur.mp = bslot(i).stat.cur.mp
  END IF
 NEXT i
END SUB

SUB import_battle_hero_stats (bslot() as BattleSprite)
 'Import a few specific stats to the temporary hero battle stats
 'This would normally only be used at victory, in case out-of-battle
 'stats have changed due to a level-up, and would never be used without
 'following an accompanying call to export_battle_hero_stats
 FOR i as integer = 0 TO 3
  IF gam.hero(i).id >= 0 THEN
   '--set in-battle HP and MP equal to out-of-battle HP and MP
   bslot(i).stat.cur.hp = gam.hero(i).stat.cur.hp
   bslot(i).stat.cur.mp = gam.hero(i).stat.cur.mp
  END IF
 NEXT i
END SUB

'who: attacker
SUB get_valid_targs(tmask() as bool, byval who as integer, byref atk as AttackData, bslot() as BattleSprite)
 DIM i as integer

 FOR i = 0 TO 11
  tmask(i) = NO ' clear list of available targets
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
  tmask(who) = YES

 CASE 3 'all (not dead)
  FOR i = 0 TO 11: tmask(i) = bslot(i).vis: NEXT i

 CASE 4 'ally-including-dead
  IF is_hero(who) THEN
   FOR i = 0 TO 3
    IF gam.hero(i).id >= 0 THEN tmask(i) = YES
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
  tmask(who) = NO

 CASE 6 'revenge-one
  IF bslot(who).revenge >= 0 THEN
   tmask(bslot(who).revenge) = bslot(bslot(who).revenge).vis
  END IF

 CASE 7 'revenge-all
  FOR i = 0 TO 11
   IF bslot(who).revengemask(i) = YES AND bslot(i).vis THEN
    tmask(i) = YES
   END IF
  NEXT i

 CASE 8 'previous
  FOR i = 0 TO 11
   IF bslot(who).last_targs(i) = YES AND bslot(i).vis THEN
    tmask(i) = YES
   END IF
  NEXT i

 CASE 9 'stored
  FOR i = 0 TO 11
   IF bslot(who).stored_targs(i) = YES AND (bslot(i).vis OR bslot(who).stored_targs_can_be_dead) THEN
    tmask(i) = YES
   END IF
  NEXT i

 CASE 10 'dead-ally (hero only)
  IF is_hero(who) THEN
   FOR i = 0 TO 3
    IF gam.hero(i).id >= 0 AND bslot(i).stat.cur.hp <= 0 THEN tmask(i) = YES
   NEXT i
  END IF

 CASE 11 'thankvenge-one
  IF bslot(who).thankvenge >= 0 THEN
   tmask(bslot(who).thankvenge) = bslot(bslot(who).thankvenge).vis
  END IF

 CASE 12 'thankvenge-all
  FOR i = 0 TO 11
   IF bslot(who).thankvengemask(i) = YES AND bslot(i).vis THEN
    tmask(i) = YES
   END IF
  NEXT i

 CASE 13 'counter
  DIM counter as integer = bslot(who).counter_target
  IF counter >= 0 THEN
   tmask(counter) = bslot(counter).vis
  END IF

 CASE 14 'all-including-dead
  FOR i = 0 TO 3
   IF gam.hero(i).id >= 0 THEN tmask(i) = YES
  NEXT i
  FOR i = 4 TO 11: tmask(i) = bslot(i).vis: NEXT i

 CASE 15 'dead foe (enemy only)
  IF is_enemy(who) THEN
   FOR i = 0 TO 3
    IF gam.hero(i).id >= 0 ANDALSO bslot(i).stat.cur.hp <= 0 THEN tmask(i) = YES
   NEXT i
  END IF

 CASE 16 'foe-including-dead
  IF is_hero(who) THEN
   FOR i = 4 TO 11: tmask(i) = bslot(i).vis: NEXT i
  ELSEIF is_enemy(who) THEN
   FOR i = 0 TO 3
    IF gam.hero(i).id >= 0 THEN tmask(i) = YES
   NEXT i
  END IF

 'Consider updating chkOOBtarg when adding new target classes concerning dead allies...
 'but OOB nearly all are treated as 'All'.

 END SELECT

 'enforce "hidden" status
 FOR i = 0 TO 11
  'Self targetted attacks always ignore hidden status
  IF atk.targ_class = 2 THEN CONTINUE FOR
  'Otherwise, hidden ones can't be targetted
  IF bslot(i).hidden THEN tmask(i) = NO
 NEXT i

 'enforce attack's disabled enemy target slots
 FOR i = 0 TO 7
  IF atk.cannot_target_enemy_slot(i) THEN tmask(4 + i) = NO
 NEXT i

 'enforce attack's disabled hero target slots
 FOR i = 0 TO 3
  IF atk.cannot_target_hero_slot(i) THEN tmask(i) = NO
 NEXT i

 FOR i = 0 TO 11
  'If a target is doing an on-death bequest attack,
  ' it cannot be targeted by anyone but itself
  IF bslot(i).bequesting AND i <> who THEN
   tmask(i) = NO
  END IF
 NEXT i

 'Some restrictions are only applied when the target class is not "self"
 IF atk.targ_class <> 2 THEN
  FOR i = 0 TO 11
   'enforce untargetability
   IF is_hero(who) THEN
    IF bslot(i).hero_untargetable = YES THEN tmask(i) = NO
   ELSEIF is_enemy(who) THEN
    IF bslot(i).enemy_untargetable = YES THEN tmask(i) = NO
   END IF
  NEXT i
 END IF

END SUB

' Note: attack_placement_over_target has a special case for the walk-forward-20-pixels behaviour
SUB anim_advance (byval who as integer, attack as AttackData, bslot() as BattleSprite, t() as integer)
 DIM d as integer
 ' Enemy faces right, hero faces left

 DIM target as BattleSprite ptr = @bslot(t(0))

 IF attack.always_unhide_attacker THEN
  anim_appear who
  anim_unhide who
 END IF

 SELECT CASE attack.attacker_anim
 CASE atkrAnimStrike, atkrAnimCast, atkrAnimSpinStrike, atkrAnimJump
  IF is_hero(who) THEN
   ' Walk forward 20 pixels
   anim_walktoggle who
   anim_setmove who, -5, 0, 4, 0
   anim_waitforall
  END IF

 CASE atkrAnimDashIn
  ' Don't backstab yourself.
  IF t(0) = who THEN EXIT SUB
  anim_walktoggle who
  IF is_enemy(who) THEN
   anim_absmove who, target->x - bslot(who).w, target->y + target->h - bslot(who).h + 2, 6, 6
  ELSE
   anim_absmove who, target->x + target->w, target->y + target->h - bslot(who).h + 2, 6, 6
  END IF
  anim_waitforall
 
 CASE atkrAnimTeleport
  ' Don't backstab yourself.
  IF t(0) = who THEN EXIT SUB
  IF is_enemy(who) THEN
   anim_setpos who, target->x - bslot(who).w, target->y + target->h - bslot(who).h, 0
  ELSE
   anim_setpos who, target->x + target->w, target->y + target->h - bslot(who).h, 0
  END IF

 CASE atkrAnimLand, atkrAnimNull, atkrAnimStandingCast, atkrAnimStandingStrike
  ' Do nothing

 END SELECT
END SUB

'Generate attacker animation when hero attacks
SUB anim_hero (byval who as integer, attack as AttackData, bslot() as BattleSprite, t() as integer)

 SELECT CASE attack.attacker_anim
  ' Animations using the cast animation
  CASE atkrAnimCast, atkrAnimStandingCast
   anim_setframe who, frameSTAND
   anim_wait 3
   anim_setframe who, frameCAST
   anim_wait 3

  CASE atkrAnimStrike, atkrAnimDashIn, atkrAnimTeleport, atkrAnimStandingStrike
   ' Animations which show the weapon graphic
   anim_setframe who, frameSTAND
   anim_wait 3 'wait 3 ticks
   anim_setframe who, frameATTACKA

   DIM as XYPair hand = gam.hero(who).hand_pos(0)
   DIM as XYPair wep = bslot(24).hand(0)
   DIM as XYPair wepoff = hand - wep

   anim_align2 24, who, 0, 0, wepoff.x, 16
   anim_setz 24, 16 - wepoff.y

   anim_setframe 24, 0
   anim_appear 24
   anim_wait 3
   anim_setframe who, frameATTACKB

   hand = gam.hero(who).hand_pos(1)
   wep = bslot(24).hand(1)
   wepoff = hand - wep

   anim_align2 24, who, 0, 0, wepoff.x, 16
   anim_setz 24, 16 - wepoff.y
   anim_setframe 24, 1

  CASE atkrAnimSpinStrike
   FOR ii as integer = 0 TO 2
    anim_setdir who, 1
    anim_wait 1
    anim_setdir who, 0
    anim_wait 1
   NEXT ii

  CASE atkrAnimJump
   anim_setframe who, frameJUMP
   anim_relmove who, -26, 0, 13, 0
   anim_zmove who, 13, 18
   anim_waitforall
   anim_disappear who
   anim_hide who
   anim_setframe who, frameSTAND

  CASE atkrAnimLand
   anim_setz who, 200
   anim_setframe who, frameLAND
   anim_appear who
   anim_unhide who
   anim_setcenter who, t(0), 0, 0
   anim_align who, t(0), dirDown, 0
   anim_zmove who, -10, 20
   anim_waitforall
   anim_setframe who, frameHURT

  CASE atkrAnimRunAndHide
   anim_setframe who, frameJUMP
   anim_setdir who, 1
   anim_absmove who, 320 + bslot(t(0)).w / 2, bslot(t(0)).y, 10, 1
   anim_waitforall
   anim_disappear who
   anim_hide who
   anim_setframe who, frameSTAND
   anim_setdir who, 0

  CASE atkrAnimRunInUnhide
   anim_setz who, 0
   anim_setpos who, 320 + bslot(t(0)).w / 2, bslot(t(0)).y, 0
   anim_setframe who, frameSTAND
   anim_appear who
   anim_unhide who
   anim_absmove who, bslot(t(0)).x, bslot(t(0)).y, 10, 1
   anim_waitforall

  CASE atkrAnimNull
   'Nothing

 END SELECT

END SUB

'Generate attacker animation when an enemy attacks
SUB anim_enemy (byval who as integer, attack as AttackData, bslot() as BattleSprite, t() as integer)

 SELECT CASE attack.attacker_anim
 CASE atkrAnimStrike, atkrAnimCast  ' twitch
  anim_setz who, 2
  anim_wait 1
  anim_setz who, 0
 CASE atkrAnimSpinStrike
  FOR ii as integer = 0 TO 2
   anim_setdir who, 1
   anim_wait 1
   anim_setdir who, 0
   anim_wait 1
  NEXT ii
 CASE atkrAnimJump
  anim_absmove who, bslot(who).x + 50, bslot(who).y, 7, 7
  anim_zmove who, 10, 20
  anim_waitforall
  anim_disappear who
  anim_hide who
 CASE atkrAnimLand
  anim_setz who, 200
  anim_appear who
  anim_unhide who
  anim_setpos who, bslot(t(0)).x, bslot(t(0)).y, 0
  anim_zmove who, -10, 20
  anim_waitforall
 CASE atkrAnimRunAndHide
  anim_setdir who, 1
  anim_absmove who, 0 - bslot(t(0)).w / 2, bslot(t(0)).y, 10, 1
  anim_waitforall
  anim_disappear who
  anim_hide who
  anim_setdir who, 0
 CASE atkrAnimRunInUnhide
  anim_setz who, 0
  anim_setpos who, 0 - bslot(t(0)).w / 2, bslot(t(0)).y, 0
  anim_appear who
  anim_unhide who
  anim_absmove who, bslot(t(0)).x, bslot(t(0)).y, 10, 1
  anim_waitforall
 CASE atkrAnimDashIn, atkrAnimNull, atkrAnimStandingCast, atkrAnimTeleport, atkrAnimStandingStrike
  ' nothing
 END SELECT
END SUB

' Undoes anim_advance
SUB anim_retreat (byval who as integer, attack as AttackData, bslot() as BattleSprite)

 IF is_enemy(who) THEN
  IF attack.attacker_anim = atkrAnimDashIn OR attack.attacker_anim = atkrAnimLand THEN
   anim_setz who, 0
   anim_absmove who, bslot(who).x, bslot(who).y, 6, 6
   anim_waitforall
  END IF
 END IF

 IF is_hero(who) THEN
  SELECT CASE attack.attacker_anim
  CASE atkrAnimStrike, atkrAnimCast
   ' Walk back 20 pixels
   anim_walktoggle who
   anim_setmove who, 5, 0, 4, 0
   anim_waitforall
   anim_setframe who, frameSTAND
  CASE atkrAnimDashIn, atkrAnimLand
   anim_setframe who, frameSTAND
   anim_walktoggle who
   anim_setz who, 0
   anim_absmove who, bslot(who).x, bslot(who).y, 6, 6
   anim_waitforall
   anim_setframe who, frameSTAND
  CASE atkrAnimStandingCast, atkrAnimStandingStrike
   anim_setframe who, frameSTAND
  CASE atkrAnimNull, atkrAnimSpinStrike, atkrAnimJump, atkrAnimTeleport
  ' Do nothing
  END SELECT
 END IF

 IF attack.always_hide_attacker THEN
  anim_disappear who
  anim_unhide who
 END IF

END SUB

FUNCTION attack_can_hit_dead(attacker as integer, atk_id as integer, stored_targs_can_be_dead as bool=NO) as bool
 DIM attack as AttackData
 loadattackdata attack, atk_id
 RETURN attack_can_hit_dead(attacker, attack, stored_targs_can_be_dead)
END FUNCTION

FUNCTION attack_can_hit_dead(attacker as integer, attack as AttackData, stored_targs_can_be_dead as bool=NO) as bool
 'AFAICT, the reason for the is_hero/is_enemy checks here is to ensure that
 'check_for_unhittable_invisible_foe only cancels attacks against dead enemies,
 'not dead heroes. Which is obtuse.

 SELECT CASE attack.targ_class
  CASE 4 'ally-including-dead (hero only)
   IF is_hero(attacker) THEN RETURN YES
  CASE 9 'stored targets
   IF is_hero(attacker) THEN
    IF stored_targs_can_be_dead THEN RETURN YES
   END IF
  CASE 10 'dead-ally (hero only)
   'Is this really necessary? Can't we just RETURN YES?
   IF is_hero(attacker) THEN RETURN YES
  CASE 14 'all-including-dead
   RETURN YES
  CASE 15, 16 'dead foe (enemy only), foe-incluidng-dead
   'As above, just RETURN YES?
   IF is_enemy(attacker) THEN RETURN YES
 END SELECT

 RETURN NO
END FUNCTION

FUNCTION autotarget (who as integer, atk_id as integer, bslot() as BattleSprite, queue as bool=YES, override_blocking as integer=-2, dont_retarget as bool=NO, is_counterattack as bool=NO) as bool
 DIM t(11) as integer
 RETURN autotarget(who, atk_id, bslot(), t(), queue, override_blocking, dont_retarget, is_counterattack)
END FUNCTION

FUNCTION autotarget (who as integer, atk_id as integer, bslot() as BattleSprite, t() as integer, queue as bool=YES, override_blocking as integer=-2, dont_retarget as bool=NO, is_counterattack as bool=NO) as bool
 DIM attack as AttackData
 loadattackdata attack, atk_id
 RETURN autotarget(who, attack, bslot(), t(), queue, override_blocking, dont_retarget, is_counterattack)
END FUNCTION

FUNCTION autotarget (who as integer, byref atk as AttackData, bslot() as BattleSprite, queue as bool=YES, override_blocking as integer=-2, dont_retarget as bool=NO, is_counterattack as bool=NO) as bool
 DIM t(11) as integer
 RETURN autotarget(who, atk, bslot(), t(), queue, override_blocking, dont_retarget, is_counterattack)
END FUNCTION

FUNCTION autotarget (who as integer, byref atk as AttackData, bslot() as BattleSprite, t() as integer, queue as bool=YES, override_blocking as integer=-2, dont_retarget as bool=NO, is_counterattack as bool=NO) as bool
 '--Returns true if the targetting was successful, or false if it failed for some reason
 ' such as no valid targets being available.

 IF is_counterattack THEN
  IF atk.blocking_counterattack THEN override_blocking = YES
 END IF

 DIM tmask(11) as bool ' Which targets are valid for the currently targetting attack

 get_valid_targs tmask(), who, atk, bslot()

 flusharray t(), 11, YES

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
 
 '--check to see if any targets were actually chosen
 DIM result as bool = NO
 FOR i as integer = 0 TO UBOUND(t)
  IF t(i) >= 0 THEN result = YES
 NEXT i

 '--Now copy the target into the queue
 '(Note! this is done even if the attack has no targets, which will result in it being
 'cancelled when it gets dequeued, wasting the attacker's turn)
 IF queue THEN
  queue_attack atk.id, who, t(), override_blocking, dont_retarget
 END IF

 RETURN result
END FUNCTION

SUB confirm_auto_spread (byval who as integer, tmask() as bool, bslot() as BattleSprite, t() as integer)
 DIM i as integer
 DIM targetptr as integer = 0
 FOR i = 0 TO 11
  IF tmask(i) THEN
   t(targetptr) = i
   targetptr = targetptr + 1
  END IF
 NEXT i
END SUB

SUB confirm_auto_focus (byval who as integer, tmask() as bool, byref atk as AttackData, bslot() as BattleSprite, t() as integer)
 t(0) = find_preferred_target(tmask(), who, atk, bslot())
END SUB

SUB confirm_auto_first (byval who as integer, tmask() as bool, bslot() as BattleSprite, t() as integer)
 DIM i as integer
 FOR i = 0 TO 11
  IF tmask(i) THEN
   t(0) = i
   EXIT SUB
  END IF
 NEXT i
END SUB

FUNCTION find_preferred_target(tmask() as bool, byval who as integer, atk as AttackData, bslot() as BattleSprite) as integer

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
  'special handling for heroes using attacks that target all/all-including-dead
  IF is_hero(who) AND (atk.targ_class = 3 OR atk.targ_class = 14) THEN
   FOR i = 4 to 11
    IF tmask(i) THEN RETURN i
   NEXT i
  ELSE ' normal first-target handling
   FOR i = 0 to 11
    IF tmask(i) THEN RETURN i
   NEXT i
  END IF

 CASE 2 '--Closest
  best = -1
  found = 200000
  FOR i = 0 TO 11
   IF tmask(i) THEN
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
   IF tmask(i) THEN
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
   IF tmask(found) THEN RETURN found
   search = search + 1
  LOOP UNTIL search > 999 ' safety

 CASE 5 'Weakest (absolute)
  best = -1
  found = 32767
  FOR i = 0 TO 11
   IF tmask(i) THEN
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
   IF tmask(i) THEN
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
   IF tmask(i) THEN
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
   IF tmask(i) THEN
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
  IF tmask(i) THEN RETURN i
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

'If for_alone_ai=YES: number of enemies left, for purpose of Alone AI
'Otherwise: number of enemies left
FUNCTION targenemycount (bslot() as BattleSprite, byval for_alone_ai as bool = NO) as integer
 DIM count as integer = 0
 DIM ignore as bool = NO
 FOR i as integer = 4 TO 11
  IF for_alone_ai THEN
   ignore = bslot(i).ignore_for_alone
  END IF
  IF bslot(i).stat.cur.hp > 0 AND bslot(i).vis AND ignore = NO THEN
   count = count + 1
  END IF
 NEXT i
 RETURN count
END FUNCTION

'Called to load an enemy from a Formation slot. Can be called even if the
'slot is empty, but can not be called to cleanup a loaded enemy.
SUB loadfoe (byval slot as integer, formdata as Formation, byref bat as BattleState, bslot() as BattleSprite, byval allow_dead as bool = NO)
 '--slot is the enemy formation slot
 DIM byref bspr as BattleSprite = bslot(4 + slot)

 IF formdata.slots(slot).id >= 0 THEN '-- if this slot is occupied

  WITH bspr
   loadenemydata .enemy, formdata.slots(slot).id, YES

   setup_non_volatile_enemy_state bspr
   reset_enemy_state bspr

   '--Special handling for spawning already-dead enemies
   IF allow_dead = NO THEN
    'enemies which spawn already-dead should be killed off immediately
    'die without boss or 0 hp?
    IF dieWOboss(4 + slot, bslot()) OR .enemy.stat.hp <= 0 THEN
     'rewards and spawn enemies on death
     'enemy is only partially constructed, but already have everything needed.
     dead_enemy 4 + slot, -1, bat, bslot(), formdata
     EXIT SUB
    END IF
   END IF

   setup_enemy_slice bspr, bat

   '--Position
   .basepos = formdata.slots(slot).pos
   (.pos) = .basepos
   '--targetting state
   .revenge = -1
   .thankvenge = -1
   .counter_target = -1
   FOR i as integer = 0 TO 11
    .revengemask(i) = NO
    .last_targs(i) = NO
    .stored_targs(i) = NO
    .thankvengemask(i) = NO
   NEXT i
   .active_turn_num = 0

   '--update stats
   FOR i as integer = 0 TO 11
    .stat.cur.sta(i) = .enemy.stat.sta(i)
    .stat.max.sta(i) = .enemy.stat.sta(i)
   NEXT i
  END WITH
 END IF
END SUB

SUB transfer_enemy_bits(byref bspr as BattleSprite)
  '--Copy elemental bits and other bits from bslot().enemy to bslot()

  WITH bspr
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
   .give_rewards_even_if_alive = .enemy.give_rewards_even_if_alive

   FOR i as integer = 0 TO gen(genNumElements) - 1
    .elementaldmg(i) = .enemy.elementals(i)
   NEXT
  END WITH
END SUB

SUB transfer_enemy_counterattacks (byref bspr as BattleSprite)
 '--transfer counterattacks
 WITH bspr
  FOR j as integer = 0 TO gen(genNumElements) - 1
   .elem_counter_attack(j) = .enemy.elem_counter_attack(j)
  NEXT j
  .non_elem_counter_attack = .enemy.non_elem_counter_attack
  FOR j as integer = 0 TO statLast
   .stat_counter_attack(j) = .enemy.stat_counter_attack(j)
  NEXT j
 END WITH
END SUB

' Called when loading or reloading (transmogrification) an enemy,
' sets up anything that should be reset when transmogrifying
SUB reset_enemy_state(byref bspr as BattleSprite)
 WITH bspr
  .vis = YES
  .d = 0
  .dissolve = 0
  .dissolve_appear = -1  'One tick delay before starting to appear, to happen after battle fade-in
  .fleeing = NO
  .bequesting = NO
  .self_bequesting = NO
 END WITH
END SUB

' Called when loading or reloading (transmogrification) an enemy,
' sets everything that doesn't change
SUB setup_non_volatile_enemy_state(byref bspr as BattleSprite)
 WITH bspr
  .name = .enemy.name
  .deathtype = .enemy.dissolve - 1
  IF .deathtype = -1 THEN .deathtype = gen(genEnemyDissolve)
  ._deathtime = .enemy.dissolve_length
  .appeartype = .enemy.dissolve_in - 1
  ._appeartime = .enemy.dissolve_in_length
  .cursorpos.x = -.enemy.cursor_offset.x '--negated because enemy sprite was originally h-flipped in the editor!
  .cursorpos.y = .enemy.cursor_offset.y
  .death_sfx = .enemy.death_sound

  transfer_enemy_bits bspr
  transfer_enemy_counterattacks bspr
 END WITH
END SUB

SUB changefoe(bat as BattleState, byval slot as integer, transmog as TransmogData, formdata as Formation, bslot() as BattleSprite)
 IF formdata.slots(slot).id = -1 THEN
  showbug "changefoe doesn't work on empty slot " & slot & " (enemy " & transmog.enemy & ")"
  EXIT SUB
 END IF

 DIM byref bspr as BattleSprite = bslot(4 + slot)

 IF transmog.rewards_rule = 1 THEN  'Give rewards before transmogrification
  enemy_death_rewards bat, bspr
 END IF

 formdata.slots(slot).id = transmog.enemy

 WITH bspr
  BUG_IF(.sl = NULL, "foe not loaded")

  loadenemydata .enemy, formdata.slots(slot).id, YES

  setup_non_volatile_enemy_state bspr
  reset_enemy_state bspr

  DIM old_w as integer = .w
  DIM old_h as integer = .h

  setup_enemy_slice bspr, bat, YES

  '--adjust position
  DIM shift as XYPair = XY(old_w / 2 - .w / 2, old_h - .h)
  .basepos += shift
  .pos += shift

  '--update stats
  change_foe_stat bspr, 0, .enemy.stat.hp, transmog.hp_rule
  FOR i as integer = 1 TO statLast
   change_foe_stat bspr, i, .enemy.stat.sta(i), transmog.other_stats_rule
  NEXT i
 END WITH
END SUB

SUB change_foe_stat(bspr as BattleSprite, byval stat_num as integer, byval new_max as integer, byval stat_rule as TransmogStatsRule)
 WITH bspr.stat
  '--selectively alter current stat
  SELECT CASE stat_rule
   CASE transmogKeepCurrent '--keep old current
   CASE transmogUseNewMax   '--use new max
    .cur.sta(stat_num) = new_max
   CASE transmogKeepCurrentPercent '--preserve % of max
    IF .max.sta(stat_num) > 0 THEN
     .cur.sta(stat_num) = CINT(new_max / .max.sta(stat_num) * .cur.sta(stat_num))
    ELSE
     .cur.sta(stat_num) = new_max
    END IF
   CASE transmogKeepCurrentCropMax '--keep old current, crop to new max
    .cur.sta(stat_num) = small(.cur.sta(stat_num), new_max)
  END SELECT
  '--always use new max stat
  .max.sta(stat_num) = new_max
 END WITH
END SUB

'There's no need to handle any lumps here which don't have effect on battles such as map data.
'Any lumps that aren't handled here stay in the modified_lumps vector and are handled
'after leaving battle.
SUB try_to_reload_files_inbattle ()
 'calls receive_file_updates
 try_reload_lumps_anywhere

 DIM i as integer = 0
 WHILE i < v_len(modified_lumps)
  DIM handled as bool = NO
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
