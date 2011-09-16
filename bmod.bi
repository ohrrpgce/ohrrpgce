'OHRRPGCE - bmod.bi
'(C) Copyright 1997-2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Auto-generated by MAKEBI from bmod.bas

#IFNDEF BMOD_BI
#DEFINE BMOD_BI

#INCLUDE "udts.bi"
#INCLUDE "battle_udts.bi"

declare function battle (form as integer) as integer
declare function checknorunbit (bslot() as battlesprite) as integer
DECLARE SUB checkTagCond (t AS AttackDataTag, check AS INTEGER)
declare function focuscost (cost as integer, focus as integer) as integer
DECLARE SUB calc_hero_elementals (elemental_resists() as single, byval who as integer)
declare sub invertstack
declare sub quickinflict (harm as integer, targ as integer, bslot() as battlesprite)
DECLARE SUB anim_end()
DECLARE SUB anim_wait(ticks as integer)
DECLARE SUB anim_waitforall()
DECLARE SUB anim_inflict(who AS INTEGER, target_count AS INTEGER)
DECLARE SUB anim_disappear(who as integer)
DECLARE SUB anim_appear(who as integer)
DECLARE SUB anim_setframe(who as integer, frame as integer)
DECLARE SUB anim_setpos(who as integer, x as integer, y as integer, d as integer)
DECLARE SUB anim_setz(who as integer, z as integer)
DECLARE SUB anim_setmove(who as integer, xm as integer, ym as integer, xstep as integer, ystep as integer)
DECLARE SUB anim_absmove(who as integer, tox as integer, toy as integer, xspeed as integer, yspeed as integer)
DECLARE SUB anim_zmove(who as integer, zm as integer, zstep as integer)
DECLARE SUB anim_walktoggle(who as integer)
DECLARE SUB anim_sound(which as integer)
DECLARE SUB anim_align(who as integer, target as integer, dire as integer, offset as integer)
DECLARE SUB anim_setcenter(who as integer, target as integer, offx as integer, offy as integer)
DECLARE SUB anim_align2(who as integer, target as integer, edgex as integer, edgey as integer, offx as integer, offy as integer)
DECLARE SUB anim_relmove(who as integer, tox as integer, toy as integer, xspeed as integer, yspeed as integer)
DECLARE SUB anim_setdir(who as integer, d as integer)
DECLARE FUNCTION dieWOboss(BYVAL who as integer, bslot() AS BattleSprite) as integer
DECLARE SUB dead_enemy(deadguy AS INTEGER, killing_attack AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() as integer)
DECLARE SUB enemy_ai (BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
DECLARE SUB heromenu (BYREF bat AS BattleState, bslot() AS BattleSprite, menubits() AS INTEGER, st() as herodef)
DECLARE SUB spellmenu (BYREF bat AS BattleState, st() as HeroDef, bslot() AS BattleSprite)
DECLARE SUB generate_atkscript(BYREF attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite, t() AS INTEGER)
DECLARE SUB enforce_weak_picture(who AS INTEGER, bslot() AS BattleSprite, bat AS BattleState)
DECLARE SUB battle_loadall(BYVAL form AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, st() AS HeroDef, formdata() as integer)
DECLARE SUB setup_targetting (BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB itemmenu (BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE FUNCTION spawn_chained_attack(ch AS AttackDataChain, attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite) AS INTEGER
DECLARE FUNCTION check_attack_chain(ch AS AttackDataChain, bat AS BattleState, bslot() AS BattleSprite) AS INTEGER
DECLARE FUNCTION valid_statnum(statnum AS INTEGER, context AS STRING) AS INTEGER
DECLARE FUNCTION knows_attack(BYVAL who AS INTEGER, BYVAL atk AS INTEGER, bslot() AS BattleSprite) AS INTEGER
DECLARE FUNCTION distribute_party_experience (BYVAL exper AS INTEGER) AS INTEGER

DECLARE SUB queue_attack OVERLOAD (attack AS INTEGER, who AS INTEGER, targs() AS INTEGER, override_blocking AS INTEGER=-2, dont_retarget AS INTEGER = NO)
DECLARE SUB queue_attack OVERLOAD (attack AS INTEGER, who AS INTEGER, delay AS INTEGER, targs() AS INTEGER, blocking AS INTEGER=YES, dont_retarget AS INTEGER = NO)
DECLARE SUB set_attack_queue_slot(slot AS INTEGER, attack AS INTEGER, who AS INTEGER, delay AS INTEGER, targs() AS INTEGER, blocking AS INTEGER=YES, dont_retarget AS INTEGER = NO)
DECLARE SUB clear_attack_queue()
DECLARE SUB clear_attack_queue_slot(slot AS INTEGER)
DECLARE SUB display_attack_queue (bslot() AS BattleSprite)
DECLARE FUNCTION blocked_by_attack (who AS INTEGER) AS INTEGER
DECLARE FUNCTION ready_meter_may_grow (bslot() AS BattleSprite, who AS INTEGER) AS INTEGER

#ENDIF
