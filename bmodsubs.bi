'OHRRPGCE - bmodsubs.bi
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF BMODSUBS_BI
#DEFINE BMODSUBS_BI

#include "udts.bi"
#include "battle_udts.bi"
#include "bmod.bi"

Enum AttackResult
 atkFail
 atkMiss
 atkHit
End Enum

declare function is_hero(byval who as integer) as bool
declare function is_enemy(byval who as integer) as bool
declare function is_attack(byval who as integer) as bool
declare function is_weapon(byval who as integer) as bool

declare sub anim_advance (byval who as integer, attack as AttackData, bslot() as battlesprite, t() as integer)

declare function atkallowed overload (attack as AttackData, attackerslot as integer, spclass as integer, lmplev as integer, bspr as BattleSprite) as bool
declare function atkallowed overload (attack as AttackData, attackerslot as integer, spclass as integer, lmplev as integer) as bool
declare sub subtract_attack_costs overload (attack as AttackData, attackerslot as integer, byref bat as BattleState, bslot() as BattleSprite)
declare sub subtract_attack_costs overload (attack as AttackData, attackerslot as integer, spclass as integer, lmplev as integer)

declare function checktheftchance (byval item as integer, byval itemp as integer, byval rareitem as integer, byval rareitemp as integer) as integer
declare function trytheft (bat as BattleState, byval who as integer, byval targ as integer, attack as AttackData, bslot() as BattleSprite) as bool

declare function count_available_attacks_in_ai_list (byval ai as EnemyAIEnum, byval slot as integer, bslot() as BattleSprite) as integer
declare function is_foe_of(target as integer, attacker as integer, bslot() as BattleSprite) as bool
declare function count_allies(of_whom as integer, bslot() as BattleSprite) as integer
declare function count_foes(of_whom as integer, bslot() as BattleSprite) as integer
declare sub anim_enemy (byval who as integer, attack as AttackData, bslot() as BattleSprite, t() as integer)
declare sub anim_hero (byval who as integer, attack as AttackData, bslot() as BattleSprite, t() as integer)
declare function inflict (byref h as integer = 0, byref targstat as integer = 0, byval attackerslot as integer, targetslot as integer, byref attacker as BattleSprite, byref target as BattleSprite, attack as AttackData, tcount as integer) as AttackResult
declare function liveherocount overload (bslot() as BattleSprite) as integer
declare function liveherocount () as integer
declare sub loadfoe (byval slot as integer, formdata as Formation, byref bat as BattleState, bslot() as BattleSprite, byval allow_dead as bool = NO)
declare sub changefoe(bat as BattleState, byval slot as integer, transmog as TransmogData, formdata as Formation, bslot() as BattleSprite)
declare sub anim_retreat (byval who as integer, attack as AttackData, bslot() as BattleSprite)
declare function safesubtract (byval number as integer, byval minus as integer) as integer
declare function safemultiply (byval number as integer, byval by as single) as integer
declare sub setbatcap (bat as BattleState, cap as string, byval captime as integer, byval capdelay as integer)
declare sub battle_target_arrows_mask (inrange() as integer, byval d as integer, byval axis as integer, bslot() as battlesprite, targ as TargettingState)
declare sub battle_target_arrows (byval d as integer, byval axis as integer, bslot() as battlesprite, targ as TargettingState, byval allow_spread as bool = NO)
declare function targetmaskcount (tmask() as integer) as integer
declare sub traceshow (s as string)

declare function hero_total_exp (byval hero_slot as integer) as integer
declare sub updatestatslevelup (byval hero_slot as integer, byval allowforget as bool)
declare sub hero_total_equipment_bonuses (byval hero_slot as integer, bonuses() as integer)
declare sub recompute_hero_max_stats (byval hero_slot as integer)
declare sub compute_hero_base_stats_from_max (byval hero_slot as integer)
declare sub learn_spells_for_current_level(byval who as integer, byval allowforget as bool)
declare function giveheroexperience (byval i as integer, byval exper as integer) as bool
declare sub setheroexperience (byval who as integer, byval amount as integer, byval allowforget as bool)
declare function allowed_to_gain_levels(byval heroslot as integer) as bool

declare function visibleandalive (byval who as integer, bslot() as battlesprite) as bool
declare sub export_battle_hero_stats (bslot() as BattleSprite)
declare sub import_battle_hero_stats (bslot() as BattleSprite)

declare function has_valid_targs OVERLOAD (byval who as integer, byval atk_id as integer, bslot() as BattleSprite) as bool
declare function has_valid_targs OVERLOAD (byval who as integer, byref atk as AttackData, bslot() as BattleSprite) as bool

declare sub get_valid_targs (tmask() as integer, byval who as integer, byref atk as AttackData, bslot() as BattleSprite)
declare function attack_can_hit_dead OVERLOAD (attacker as integer, atk_id as integer, stored_targs_can_be_dead as bool=NO) as bool
declare function attack_can_hit_dead OVERLOAD (attacker as integer, attack as AttackData, stored_targs_can_be_dead as bool=NO) as bool
declare function autotarget OVERLOAD (who as integer, atk_id as integer, bslot() as BattleSprite, t() as integer, queue as bool=YES, override_blocking as integer=-2, dont_retarget as bool=NO, is_counterattack as bool=NO) as bool
declare function autotarget OVERLOAD (who as integer, byref atk as AttackData, bslot() as BattleSprite, t() as integer, queue as bool=YES, override_blocking as integer=-2, dont_retarget as bool=NO, is_counterattack as bool=NO) as bool
declare function autotarget OVERLOAD (who as integer, atk_id as integer, bslot() as BattleSprite, queue as bool=YES, override_blocking as integer=-2, dont_retarget as bool=NO, is_counterattack as bool=NO) as bool
declare function autotarget OVERLOAD (who as integer, byref atk as AttackData, bslot() as BattleSprite, queue as bool=YES, override_blocking as integer=-2, dont_retarget as bool=NO, is_counterattack as bool=NO) as bool

declare function find_preferred_target (tmask() as integer, byval who as integer, atk as AttackData, bslot() as BattleSprite) as integer

declare sub try_to_reload_files_inbattle ()

#ENDIF
