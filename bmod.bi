'OHRRPGCE - bmod.bi
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF BMOD_BI
#DEFINE BMOD_BI

#INCLUDE "udts.bi"
#INCLUDE "battle_udts.bi"

'Which enemy AI attack list
ENUM EnemyAIEnum
 aiNone = -1      'No attack list available
 aiNormal = 0
 aiWeak = 1
 aiAlone = 2
END ENUM

declare function battle (byval form as integer) as bool
declare function checkNoRunBit (bslot() as BattleSprite) as bool
DECLARE SUB checkTagCond (byref t as AttackDataTag, byval check as AttackTagConditionEnum)
DECLARE SUB calc_hero_elementals (elemental_resists() as single, byval who as integer)
declare sub invertstack
declare sub quickinflict (byval harm as integer, byval targ as integer, bslot() as BattleSprite, byval col as integer=-1)

DECLARE SUB anim_end()
DECLARE SUB anim_wait(byval ticks as integer)
DECLARE SUB anim_waitforall()
DECLARE SUB anim_inflict(byval who as integer, byval target_count as integer)
DECLARE SUB anim_disappear(byval who as integer)
DECLARE SUB anim_appear(byval who as integer)
DECLARE SUB anim_hide(byval who as integer)
DECLARE SUB anim_unhide(byval who as integer)
DECLARE SUB anim_setframe(byval who as integer, byval frame as integer)
DECLARE SUB anim_setpos(byval who as integer, byval x as integer, byval y as integer, byval d as integer)
DECLARE SUB anim_setz(byval who as integer, byval z as integer)
DECLARE SUB anim_setmove(who as integer, xmove_ticks as integer, ymove_ticks as integer, xstep as integer, ystep as integer)
DECLARE SUB anim_absmove(byval who as integer, byval tox as integer, byval toy as integer, byval xspeed as integer, byval yspeed as integer)
DECLARE SUB anim_abszmove(byval who as integer, byval toz as integer, byval zticks as integer)
DECLARE SUB anim_zmove(byval who as integer, byval zm as integer, byval zstep as integer)
DECLARE SUB anim_walktoggle(byval who as integer)
DECLARE SUB anim_sound(byval which as integer)
DECLARE SUB anim_align(byval who as integer, byval target as integer, byval dire as integer, byval offset as integer)
DECLARE SUB anim_setcenter(byval who as integer, byval target as integer, byval offx as integer, byval offy as integer)
DECLARE SUB anim_align2(byval who as integer, byval target as integer, byval edgex as integer, byval edgey as integer, byval offx as integer, byval offy as integer)
DECLARE SUB anim_relmove(byval who as integer, byval tox as integer, byval toy as integer, byval xspeed as integer, byval yspeed as integer)
DECLARE SUB anim_setdir(byval who as integer, byval d as integer)

DECLARE SUB setup_enemy_slice(byref bspr as BattleSprite, bat as BattleState, keep_existing as bool = NO)

DECLARE FUNCTION dieWOboss(byval who as integer, bslot() as BattleSprite) as integer
DECLARE SUB dead_enemy(byval deadguy as integer, byval killing_attack as integer, byref bat as BattleState, bslot() as BattleSprite, formdata as Formation)
DECLARE SUB triggerfade(byval who as integer, bslot() as BattleSprite)
DECLARE SUB enemy_death_rewards(byref bat as BattleState, battler as BattleSprite)
declare function pick_enemy_attack_list(slot as integer, bslot() as BattleSprite) as EnemyAIEnum
DECLARE SUB enemy_ai (byref bat as BattleState, bslot() as BattleSprite, formdata as Formation)
DECLARE SUB hero_ai (byref bat as BattleState, bspr as BattleSprite, byval attacker_id as integer, bslot() as BattleSprite)
DECLARE SUB heromenu (byref bat as BattleState, bslot() as BattleSprite, st() as HeroDef)
DECLARE SUB spellmenu (byref bat as BattleState, st() as HeroDef, bslot() as BattleSprite)
DECLARE SUB generate_atkscript(byref attack as AttackData, byref bat as BattleState, bslot() as BattleSprite, t() as integer)
DECLARE SUB enforce_weak_picture(byval who as integer, bslot() as BattleSprite, byref bat as BattleState)
DECLARE SUB battle_loadall(byval form as integer, byref bat as BattleState, bslot() as BattleSprite, st() as HeroDef, formdata as Formation)
DECLARE SUB setup_targetting (byref bat as BattleState, bslot() as BattleSprite)
DECLARE SUB itemmenu (byref bat as BattleState, bslot() as BattleSprite)
DECLARE FUNCTION spawn_chained_attack(byref ch as AttackDataChain, instead_chain as bool, byref attack as AttackData, byref bat as BattleState, bslot() as BattleSprite) as bool
DECLARE FUNCTION check_attack_chain(byref ch as AttackDataChain, byref bat as BattleState, bslot() as BattleSprite) as bool
DECLARE FUNCTION valid_statnum(byval statnum as integer, context as string) as bool
DECLARE FUNCTION knows_attack(byval who as integer, byval atk as integer, bslot() as BattleSprite) as bool
DECLARE FUNCTION distribute_party_experience (byval exper as integer) as integer

DECLARE SUB queue_attack OVERLOAD (attack as integer, who as integer, targs() as integer, override_blocking as integer=-2, dont_retarget as bool = NO)
DECLARE SUB queue_attack OVERLOAD (attack as integer, who as integer, delay as integer, turn_delay as integer, targs() as integer, blocking as bool=YES, dont_retarget as bool = NO)
DECLARE SUB set_attack_queue_slot(slot as integer, attack as integer, who as integer, delay as integer, turn_delay as integer, targs() as integer, blocking as bool=YES, dont_retarget as bool = NO)
DECLARE SUB clear_attack_queue()
DECLARE SUB clear_attack_queue_slot(byval slot as integer)
DECLARE SUB display_attack_queue (bslot() as BattleSprite)
DECLARE FUNCTION blocked_by_attack (bat as BattleState, byval who as integer) as bool
DECLARE FUNCTION ready_meter_may_grow (bat as BattleState, bslot() as BattleSprite, byval who as integer) as bool

DECLARE FUNCTION hero_attack_cost_info(byref atk as AttackData, byval hero_slot as integer, byval magic_list_type as integer=0, byval lmp_level as integer=-1) as string
DECLARE FUNCTION bslot_attack_cost_info(bslot() as BattleSprite, byref atk as AttackData, byval slot as integer, byval magic_list_type as integer=0, byval lmp_level as integer=-1) as string

DECLARE FUNCTION attack_placement_over_target(attack as AttackData, targslot as integer, bat as BattleState, bslot() as BattleSprite, byval reverse as integer=0) as XYZTriple

DECLARE SUB populate_battle_menu_menudef (byval hero_id as integer, menu as MenuDef, hero as HeroDef)
DECLARE SUB populate_battle_menu_menudef_for_enemy (byref bat as BattleState, bslot() as BattleSprite, bspr as BattleSprite)
DECLARE FUNCTION does_battle_menu_have_targets(byref bspr as BattleSprite, bslot() as BattleSprite) as bool

#ENDIF
