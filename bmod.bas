'OHRRPGCE GAME - Main battle-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

'misc
#include "compat.bi"
#include "common.bi"
#include "loading.bi"
#INCLUDE "gglobals.bi"
#INCLUDE "const.bi"
#INCLUDE "uiconst.bi"
#INCLUDE "udts.bi"
#INCLUDE "battle_udts.bi"

'modules
#include "bmod.bi"
#include "bmodsubs.bi"
#include "game.bi"
#include "moresubs.bi"
#INCLUDE "allmodex.bi"

DECLARE SUB wrappedsong (songnumber)
DECLARE SUB playtimer
DECLARE SUB getitem (getit, num)
DECLARE FUNCTION count_available_spells(who AS INTEGER, list AS INTEGER) AS INTEGER

'--local subs and functions
DECLARE FUNCTION count_dissolving_enemies(bslot() AS BattleSprite) AS INTEGER
DECLARE FUNCTION find_empty_enemy_slot(formdata() AS INTEGER) AS INTEGER
DECLARE SUB spawn_on_death(deadguy AS INTEGER, killing_attack AS INTEGER, BYREF bat AS BattleState, formdata(), bslot() AS BattleSprite)
DECLARE SUB triggerfade(BYVAL who, bslot() AS BattleSprite)
DECLARE SUB check_death(deadguy AS INTEGER, BYVAL killing_attack AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata())
DECLARE SUB checkitemusability(iuse() AS INTEGER, bslot() AS BattleSprite, who AS INTEGER)
DECLARE SUB reset_battle_state (BYREF bat AS BattleState)
DECLARE SUB reset_targetting (BYREF bat AS BattleState)
DECLARE SUB reset_attack (BYREF bat AS BattleState)
DECLARE SUB reset_victory_state (BYREF vic AS VictoryState)
DECLARE SUB reset_rewards_state (BYREF rew AS RewardsState)
DECLARE SUB show_victory (BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB trigger_victory(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB fulldeathcheck (killing_attack AS INTEGER, bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
DECLARE SUB anim_flinchstart(who AS INTEGER, bslot() AS BattleSprite, attack AS AttackData)
DECLARE SUB anim_flinchdone(who AS INTEGER, bslot() AS BattleSprite, attack AS AttackData)
DECLARE SUB draw_battle_sprites(bslot() AS BattleSprite)
DECLARE FUNCTION battle_time_can_pass(bat AS BattleState) AS INTEGER
DECLARE SUB battle_crappy_run_handler(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB show_enemy_meters(bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
DECLARE SUB battle_animate(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB battle_meters (BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
DECLARE SUB battle_display (BYREF bat AS BattleState, bslot() AS BattleSprite, menubits() AS INTEGER, st() AS HeroDef)
DECLARE SUB battle_confirm_target(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB battle_targetting(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB battle_spawn_on_hit(targ as INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
DECLARE SUB battle_attack_anim_cleanup (BYREF attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
DECLARE SUB battle_attack_anim_playback (BYREF attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
DECLARE SUB battle_attack_do_inflict(targ AS INTEGER, tcount AS INTEGER, BYREF attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata())
DECLARE SUB battle_pause ()
DECLARE SUB battle_cleanup(bslot() AS BattleSprite)
DECLARE SUB battle_init(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB battle_background_anim(BYREF bat AS BattleState, formdata() AS INTEGER)
DECLARE FUNCTION battle_run_away(BYREF bat AS BattleState, bslot() AS BattleSprite) AS INTEGER
DECLARE SUB battle_animate_running_away (bslot() AS BattleSprite)
DECLARE SUB battle_check_delays(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE FUNCTION battle_check_a_delay(BYREF bat AS BattleState, bslot() AS BattleSprite, index AS INTEGER) AS INTEGER
DECLARE SUB battle_check_for_hero_turns(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE FUNCTION battle_check_a_hero_turn(BYREF bat AS BattleState, bslot() AS BattleSprite, index AS INTEGER)
DECLARE SUB battle_check_for_enemy_turns(BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE FUNCTION battle_check_an_enemy_turn(BYREF bat AS BattleState, bslot() AS BattleSprite, index AS INTEGER)
DECLARE SUB battle_attack_cancel_target_attack(targ as INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, BYREF attack AS AttackData)
DECLARE SUB battle_reevaluate_dead_targets (deadguy AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite)
DECLARE SUB battle_sort_away_dead_t_target(deadguy AS INTEGER, t() AS INTEGER)

'these are the battle global variables
DIM bstackstart AS INTEGER
DIM learnmask(245) AS INTEGER '6 shorts of bits per hero

REM $STATIC

OPTION EXPLICIT

FUNCTION battle (form, fatal) as integer
 battle = 1 'default return value

 DIM formdata(40)
 DIM attack AS AttackData
 DIM st(3) as HeroDef
 DIM menubits(2)
 DIM bat AS BattleState
 REDIM atkq(15) AS AttackQueue
 clear_attack_queue()
 DIM bslot(24) AS BattleSprite
 DIM show_info_mode AS INTEGER = 0

 '--lastformation is a global
 lastformation = form

 battle_init bat, bslot()

 'fade to near white
 fadeout 240, 240, 240
 DIM needf AS INTEGER = YES
 
 clearpage 0
 clearpage 1
 clearpage 2
 clearpage 3

 battle_loadall form, bat, bslot(), st(), formdata()

 copypage 2, dpage

 '--main battle loop
 setkeys
 DO
  setwait speedcontrol
  setkeys
  bat.tog XOR= 1
  playtimer
  control

  '--background animation hack
  battle_background_anim bat, formdata()

  IF readbit(gen(), 101, 8) = 0 THEN
   '--debug keys
   IF keyval(scF4) > 1 THEN bat.away = 11 ' Instant-cheater-running
   IF keyval(scF5) > 1 THEN bat.rew.exper = 1000000  'Million experience!
   IF keyval(scF11) > 1 THEN show_info_mode = loopvar(show_info_mode, 0, 2, 1)  'Draw debug info
  END IF
  IF keyval(scNumlock) > 1 THEN battle_pause
  IF battle_run_away(bat, bslot()) THEN
   battle = 0
   EXIT DO
  END IF
  IF bat.atk.id >= 0 AND bat.anim_ready = NO AND bat.vic.state = 0 THEN
   generate_atkscript attack, bat, bslot(), bat.anim_t()
  END IF
  IF bat.atk.id >= 0 AND bat.anim_ready = YES AND bat.vic.state = 0 AND bat.away = 0 THEN
   battle_attack_anim_playback attack, bat, bslot(), formdata()
  END IF
  battle_animate bat, bslot()
  
  bat.next_attacker = loopvar(bat.next_attacker, 0, 11, 1)
  IF battle_time_can_pass(bat) THEN
   battle_meters bat, bslot(), formdata()
   battle_check_delays bat, bslot()
  END IF
  battle_check_for_hero_turns bat, bslot()
  battle_check_for_enemy_turns bat, bslot()
  IF bat.vic.state = 0 THEN
   IF bat.enemy_turn >= 0 THEN enemy_ai bat, bslot(), formdata()
   IF bat.hero_turn >= 0 AND bat.targ.mode = targNONE THEN
    IF bat.menu_mode = batMENUITEM  THEN itemmenu bat, bslot()
    IF bat.menu_mode = batMENUSPELL THEN spellmenu bat, st(), bslot()
    IF bat.menu_mode = batMENUHERO  THEN heromenu bat, bslot(), menubits(), st()
   END IF
   IF bat.hero_turn >= 0 AND bat.targ.mode > targNONE THEN battle_targetting bat, bslot()
  END IF
 
  '--Begin display 
  copypage 2, dpage
  draw_battle_sprites bslot()
  battle_display bat, bslot(), menubits(), st()
  IF bat.vic.state = vicEXITDELAY THEN bat.vic.state = vicEXIT
  IF bat.vic.state > 0 THEN show_victory bat, bslot()
  IF show_info_mode = 1 THEN
   show_enemy_meters bat, bslot(), formdata()
  ELSEIF show_info_mode = 2 THEN
   display_attack_queue bslot()
  END IF
  IF bat.death_mode = deathENEMIES AND bat.vic.state = 0 THEN
   IF count_dissolving_enemies(bslot()) = 0 THEN trigger_victory bat, bslot()
  END IF
  IF bat.vic.state = vicEXIT THEN EXIT DO 'normal victory exit
  IF bat.death_mode = deathHEROES THEN
   fatal = 1
   EXIT DO
  END IF
  IF bat.alert_ticks > 0 THEN
   bat.alert_ticks -= 1
   centerfuz 160, 190, 100, 16, 3, dpage
   edgeprint bat.alert, 160 - LEN(bat.alert) * 4, 185, uilook(uiSelectedItem + bat.tog), dpage
  END IF
 
  if dotimerbattle then
   fatal = 0
   exit do
  end if
 
  '--show the timer
  FOR i AS INTEGER = 0 to UBOUND(timers)
   IF timers(i).speed > 0 AND timers(i).st > -1 AND timers(i).flags AND 2 = 2 THEN
    edgeprint plotstr(timers(i).st-1).s, 320 - LEN(plotstr(timers(i).st-1).s) * 10, 185, uilook(uiText), dpage
    EXIT FOR
   END IF
  NEXT

  SWAP vpage, dpage
  setvispage vpage
  IF needf THEN
   needf = NO
   fadein
   setkeys
  END IF
  dowait
 LOOP
 IF fatal THEN battle = 0
 battle_cleanup bslot()
END FUNCTION

SUB battle_init(BYREF bat AS BattleState, bslot() AS BattleSprite)

 '--prepare stack
 bstackstart = stackpos

 'Remember the music that was playing on the map so that the prepare_map() sub can restart it later
 gam.remembermusic = presentsong

 reset_battle_state bat

 '--Init BattleState
 FOR i AS INTEGER = 0 TO 11
  bslot(i).consume_item = -1
  bslot(i).revenge = -1
  bslot(i).thankvenge = -1
  bslot(i).harm.col = uilook(uiText)
  '--init affliction registers
  '--it should be clear by the fact that BattleStats is a separate type that
  '--that bslot().stat inside battle is not the same as gam.hero().stat outside battle
  WITH bslot(i).stat.cur
   .poison = 1000
   .regen  = 1000
   .stun   = 1000
   .mute   = 1000
  END WITH
  WITH bslot(i).stat.max
   .poison = 1000
   .regen  = 1000
   .stun   = 1000
   .mute   = 1000
  END WITH
  bslot(i).poison_repeat = INT(RND * 2000)
  bslot(i).regen_repeat = INT(RND * 2000)
 NEXT i

 '--sanity-check afliction indicators and set defaults for old games that don't have them yet. 
 IF gen(genPoison) <= 0 THEN gen(genPoison) = 161
 IF gen(genStun) <= 0 THEN gen(genStun) = 159
 IF gen(genMute) <= 0 THEN gen(genMute) = 163
END SUB

SUB battle_cleanup(bslot() AS BattleSprite)
 writestats bslot()

 '--overflow checking for the battle stack
 IF (stackpos - bstackstart) \ 2 > 0 THEN
  '--an overflow is not unusual. This happens if the battle terminates
  '--while an attack is still going on
  DIM dummy AS INTEGER
  WHILE stackpos > bstackstart: dummy = popw: WEND
 END IF

 '--underflow checking
 IF (stackpos - bstackstart) \ 2 < 0 THEN
  '--an underflow is bad. It used to mean that whatever script was on
  '--the top of the stack has been corrupted, but now scripts don't use this stack
  '--but and underflow is still bad in principal.
  fatalerror "bstack underflow " & stackpos & " " & bstackstart
 END IF

 fadeout 0, 0, 0
 clearpage 0
 clearpage 1
 clearpage 2
 clearpage 3

 FOR i AS INTEGER = LBOUND(bslot) TO UBOUND(bslot)
  frame_unload(@bslot(i).sprites)
  palette16_unload(@bslot(i).pal)
 NEXT i

END SUB

SUB battle_pause ()
 DIM pause AS STRING = readglobalstring(54, "PAUSE", 10)
 fuzzyrect 0, 0, 320, 200, uilook(uiTextBox), vpage
 edgeprint pause, xstring(pause, 160), 95, uilook(uiText), vpage
 setvispage vpage
 '--wait for a key
 DIM wk AS INTEGER = getkey
END SUB

SUB battle_attack_anim_playback (BYREF attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
 '--this plays back the animation sequence built when the attack starts.

 DIM i AS INTEGER

 '--decrement the animation wait ticks, and don't proceed until they are zero
 IF bat.wait_frames > 0 THEN bat.wait_frames -= 1: IF bat.wait_frames > 0 THEN EXIT SUB
 
 '--special handling when we are waiting for all motion to stop
 IF bat.wait_frames = -1 THEN
  bat.wait_frames = 0
  FOR i = 0 TO 23
   IF bslot(i).xmov <> 0 OR bslot(i).ymov <> 0 OR bslot(i).zmov <> 0 THEN bat.wait_frames = -1
  NEXT i
  IF bat.wait_frames = -1 THEN EXIT SUB
 END IF
 bat.wait_frames = 0

 DIM act AS INTEGER
 
 '--these are used to temporarily store "who" arguments
 DIM ww AS INTEGER
 DIM w1 AS INTEGER
 DIM w2 AS INTEGER
 
 DO: 'INTERPRET THE ANIMATION SCRIPT
  act = popw
  SELECT CASE act
   CASE 0 '--end()
    FOR i = 0 TO 3
     enforce_weak_picture i, bslot(), bat
     '--re-enforce party's X/Y positions...
     bslot(i).x = bslot(i).basex
     bslot(i).y = bslot(i).basey
    NEXT i
    FOR i = 0 TO 7
     IF bslot(4 + i).flee = 0 THEN
      bslot(4 + i).x = bslot(4 + i).basex
      bslot(4 + i).y = bslot(4 + i).basey
     END IF
    NEXT i
    bat.atk.id = -1
   CASE 1 '???()
    FOR i = 0 TO 3
     formdata(i * 4 + 1) = bslot(4 + i).x
     formdata(i * 4 + 2) = bslot(4 + i).y
    NEXT i
    bat.atk.id = -1
   CASE 2 'setmove(who,xm,ym,xstep,ystep)
    ww = popw
    bslot(ww).xmov = popw
    bslot(ww).ymov = popw
    bslot(ww).xspeed = popw
    bslot(ww).yspeed = popw
   CASE 3 'setpos(who,x,y,d)
    ww = popw
    bslot(ww).x = popw
    bslot(ww).y = popw
    bslot(ww).d = popw
   CASE 4 '???()
    '--undefined
   CASE 5 'appear(who)
    ww = popw
    bslot(ww).vis = 1
   CASE 6 'disappear(who)
    ww = popw
    bslot(ww).vis = 0
   CASE 7 'setframe(who,frame)
    ww = popw
    DIM fr AS INTEGER = popw
    bslot(ww).frame = fr
    IF is_hero(ww) THEN bslot(ww).walk = 0
   CASE 8 'absmove(who,n,n,n,n)
    ww = popw
    DIM destpos AS XYPair
    destpos.x = popw
    destpos.y = popw
    DIM movestep AS XYPair
    movestep.x = popw
    movestep.y = popw
    bslot(ww).xspeed = (destpos.x - bslot(ww).x) / movestep.x
    bslot(ww).yspeed = (destpos.y - bslot(ww).y) / movestep.y
    bslot(ww).xmov = movestep.x
    bslot(ww).ymov = movestep.y
   CASE 9 'waitforall()
    bat.wait_frames = -1
   CASE 10 'inflict(targ, target_count)
    DIM targ AS INTEGER = popw
    DIM tcount AS INTEGER = popw
    battle_attack_do_inflict targ, tcount, attack, bat, bslot(), formdata()
   CASE 11 'setz(who,z)
    ww = popw
    bslot(ww).z = popw
   CASE 12 '???(n,n,n,n,n)
    'unimplemented
   CASE 13 'wait(ticks)
    bat.wait_frames = popw
   CASE 14 'walktoggle(who)
    ww = popw
    bslot(ww).frame = 0
    IF is_hero(ww) THEN bslot(ww).walk XOR= 1
   CASE 15 'zmove(who,zm,zstep)
    ww = popw
    bslot(ww).zmov = popw
    bslot(ww).zspeed = popw
   CASE 16 'sound(which)
    playsfx(popw)
   CASE 17 'align(who, target, edge, offset)
    w1 = popw
    w2 = popw
    select case popw 'which edge?
    case dirUp
     bslot(w1).y = bslot(w2).y + popw
    case dirDown
     bslot(w1).y = bslot(w2).y + bslot(w2).h - bslot(w1).h + popw
    case dirLeft
     bslot(w1).x = bslot(w2).x + popw
    case dirRight
     bslot(w1).x = bslot(w2).x + bslot(w2).w - bslot(w1).w + popw
    end select
   CASE 18 'setcenter(who, target, offx, offy)
    w1 = popw
    w2 = popw
    bslot(w1).x = (bslot(w2).w - bslot(w1).w) / 2 + bslot(w2).x + popw
    bslot(w1).y = (bslot(w2).h - bslot(w1).h) / 2 + bslot(w2).y + popw
  CASE 19 'align2(who, target, edgex, edgey, offx, offy)
   w1 = popw
   w2 = popw
   DIM xd AS INTEGER = popw
   DIM yd AS INTEGER = popw
   if xd then
    bslot(w1).x = bslot(w2).x + bslot(w2).w - bslot(w1).w + popw
   else
    bslot(w1).x = bslot(w2).x + popw
   end if
   if yd then
    bslot(w1).y = bslot(w2).y + bslot(w2).h - bslot(w1).h + popw
   else
    bslot(w1).y = bslot(w2).y + popw
   end if
  CASE 20 'relmove(who, x, y, sx, sy)
   ww = popw  'who
   DIM movedist AS XYPair
   movedist.x = popw
   movedist.y = popw
   DIM movestep AS XYPair
   movestep.x = popw
   movestep.y = popw
   with bslot(ww)
    if movestep.x <> 0 then .xspeed = movedist.x / movestep.x
    if movestep.y <> 0 then .yspeed = movedist.y / movestep.y
    .xmov = movedist.x
    .ymov = movedist.y
   end with
  CASE 21 'setdir(who, d)
   ww = popw
   DIM newdir AS INTEGER = popw
   bslot(ww).d = newdir
  END SELECT
 LOOP UNTIL bat.wait_frames <> 0 OR bat.atk.id = -1

 IF bat.atk.id = -1 THEN
  battle_attack_anim_cleanup attack, bat, bslot(), formdata()
 END IF
END SUB

SUB battle_attack_do_inflict(targ AS INTEGER, tcount AS INTEGER, BYREF attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata())
 'targ is the target slot number
 'tcount is the total number of targets (used only for dividing spread damage)

 DIM i AS INTEGER
 'set tag, if there is one
 checkTagCond attack.tagset(0), 1
 checkTagCond attack.tagset(1), 1
 
 '--attempt inflict the damage to the target
 IF inflict(bat.acting, targ, bslot(bat.acting), bslot(targ), attack, tcount, attack_can_hit_dead(bat.acting, attack)) THEN
  '--attack succeeded
  IF attack.transmog_enemy > 0 ANDALSO is_enemy(targ) THEN
   changefoe targ - 4, attack.transmog_enemy, formdata(), bslot(), attack.transmog_hp, attack.transmog_stats
  END IF
  battle_attack_cancel_target_attack targ, bat, bslot(), attack
  WITH bslot(targ).enemy.reward
   IF attack.erase_rewards = YES THEN
    .gold = 0
    .exper = 0
    .item_rate = 0
    .rare_item_rate = 0
   END IF
  END WITH
  IF attack.force_run = YES THEN
  'force heroes to run away
   IF checkNoRunBit(bslot()) THEN
    bat.alert = bat.cannot_run_caption
    bat.alert_ticks = 10
   ELSE
    bat.away = 1
   END IF
  END IF
  checkTagCond attack.tagset(0), 2
  checkTagCond attack.tagset(1), 2
  IF bslot(targ).stat.cur.hp = 0 THEN
   checkTagCond attack.tagset(0), 4
   checkTagCond attack.tagset(1), 4
  END IF

  IF trytheft(bat, bat.acting, targ, attack, bslot()) THEN
   IF bat.hero_turn >= 0 THEN
    checkitemusability bat.iuse(), bslot(), bat.hero_turn
   END IF
  END IF
 ELSE
  checkTagCond attack.tagset(0), 3
  checkTagCond attack.tagset(1), 3
 END IF
 triggerfade targ, bslot()
 IF bslot(targ).stat.cur.hp > 0 THEN
  '---REVIVE---
  bslot(targ).vis = 1
  bslot(targ).dissolve = 0
 END IF
 IF is_enemy(targ) AND attack.no_spawn_on_attack = NO THEN battle_spawn_on_hit targ, bat, bslot(), formdata()
 'FIXME: this would probably be the right place to trigger counterattacks
 IF bat.atk.has_consumed_costs = NO THEN
  '--if the attack costs MP, we want to actually consume MP
  IF attack.mp_cost > 0 THEN bslot(bat.acting).stat.cur.mp = large(bslot(bat.acting).stat.cur.mp - focuscost(attack.mp_cost, bslot(bat.acting).stat.cur.foc), 0)
 
  '--ditto for HP
  IF attack.hp_cost > 0 THEN
    WITH bslot(bat.acting)
      .stat.cur.hp = large(.stat.cur.hp - attack.hp_cost, 0)
      .harm.ticks = gen(genDamageDisplayTicks)
      .harm.pos.x = .x + (.w * .5)
      .harm.pos.y = .y + (.h * .5)
      .harm.text = STR(attack.hp_cost)
    END WITH
  END IF
 
  '--ditto for money
  IF attack.money_cost <> 0 THEN
    gold = large(gold - attack.money_cost, 0)
    WITH bslot(bat.acting)
      .harm.ticks = gen(genDamageDisplayTicks)
      .harm.pos.x = .x + (.w * .5)
      .harm.pos.y = .y + (.h * .5)
      .harm.text = ABS(attack.money_cost) & "$"
      IF attack.money_cost < 0 THEN .harm.text  += "+"
    END WITH
    IF gold > 2000000000 THEN gold = 2000000000
    IF gold < 0 THEN gold = 0
 
  END IF
 
  '--if the attack consumes items, we want to consume those too
  FOR i = 0 to 2
   WITH attack.item(i)
    IF .id > 0 THEN 'this slot is used
     IF .number > 0 THEN 'remove items
      delitem(.id, .number)
     ELSEIF .number < 0 THEN 'add items
      getitem(.id, abs(.number))
     END IF
     'Update tags when items have changed because it could affect chain conditionals
     evalitemtag
    END IF
   END WITH
  NEXT i
 
  '--set the flag to prevent re-consuming MP
  bat.atk.has_consumed_costs = YES
 END IF
 IF bslot(bat.acting).consume_lmp > 0 THEN
  lmp(bat.acting, bslot(bat.acting).consume_lmp - 1) -= 1
  bslot(bat.acting).consume_lmp = 0
 END IF
 IF bslot(bat.acting).consume_item >= 0 THEN
  IF consumeitem(bslot(bat.acting).consume_item) THEN
   setbit bat.iuse(), 0, bslot(bat.acting).consume_item, 0
   evalitemtag
  END IF
  bslot(bat.acting).consume_item = -1
 END IF
 
 IF liveherocount(bslot()) = 0 THEN bat.atk.id = -1
 
END SUB

SUB battle_attack_anim_cleanup (BYREF attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
 
 '--hide the caption when the animation is done
 IF attack.caption_time = 0 THEN
  '--clear duration-timed caption
  bat.caption_time = 0
  bat.caption_delay = 0
 END IF
 
 '--check to see if anybody is dead
 fulldeathcheck bat.atk.was_id, bat, bslot(), formdata()
 
 '--FIXME: further cleanup to remove was_id entirely?
 bat.atk.was_id = -1

 '--clean up animation stack
 'DEBUG debug "discarding " & (stackpos - bstackstart) \ 2 & " from stack"
 DIM dummy AS INTEGER
 WHILE stackpos > bstackstart: dummy = popw: WEND
 
 '--spawn the next after-chained attack (if any)
 IF spawn_chained_attack(attack.chain, attack, bat, bslot()) = NO THEN
  spawn_chained_attack(attack.elsechain, attack, bat, bslot())
 END IF

END SUB

SUB battle_spawn_on_hit(targ AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
 DIM i AS INTEGER
 DIM j AS INTEGER
 DIM slot AS INTEGER
 
 '--atktype holds the elementality of the currently animating attack
 DIM atktype(8) AS INTEGER
 atktype(0) = bat.atk.non_elemental
 FOR i = 0 TO 7
  atktype(i + 1) = bat.atk.elemental(i)
 NEXT i
 
 WITH bslot(targ)
  '--non-elemental hit
  IF .enemy.spawn.non_elemental_hit > 0 AND atktype(0) = YES THEN
   FOR j = 1 TO .enemy.spawn.how_many
    slot = find_empty_enemy_slot(formdata())
    IF slot > -1 THEN
     formdata(slot * 4) = .enemy.spawn.non_elemental_hit
     loadfoe slot, formdata(), bat, bslot()
    END IF
   NEXT j
   EXIT SUB '--skip further checks
  END IF
  FOR i = 0 TO 7
   IF .enemy.spawn.elemental_hit(i) > 0 AND atktype(i + 1) = YES THEN
    FOR j = 1 TO .enemy.spawn.how_many
     slot = find_empty_enemy_slot(formdata())
     IF slot > -1 THEN
      formdata(slot * 4) = .enemy.spawn.elemental_hit(i)
      loadfoe slot, formdata(), bat, bslot()
     END IF
    NEXT j
    EXIT FOR
   END IF
  NEXT i
 END WITH
END SUB

SUB battle_targetting(BYREF bat AS BattleState, bslot() AS BattleSprite)

 DIM i AS INTEGER

 'cancel
 IF carray(ccMenu) > 1 THEN
  bslot(bat.hero_turn).attack = 0
  bslot(bat.hero_turn).consume_lmp = 0
  bat.targ.mode = targNONE
  flusharray carray(), 7, 0
  EXIT SUB
 END IF

 IF bat.targ.mode = targSETUP THEN setup_targetting bat, bslot()

 IF bat.targ.mode = targAUTO THEN
  DIM t(11) AS INTEGER
  autotarget bat.hero_turn, bslot(bat.hero_turn).attack - 1, bslot(), t()
  bslot(bat.hero_turn).ready_meter = 0
  bslot(bat.hero_turn).ready = NO
  bat.hero_turn = -1
  bat.targ.mode = targNONE
  EXIT SUB
 END IF

 'no valid targs available
 IF targetmaskcount(bat.targ.mask()) = 0 THEN
  EXIT SUB
 END IF

 'random target
 IF bat.targ.roulette THEN
  FOR i = 0 TO INT(RND * 2)
   bat.targ.pointer = loopvar(bat.targ.pointer, 0, 11, 1)
   WHILE bat.targ.mask(bat.targ.pointer) = 0
    bat.targ.pointer = loopvar(bat.targ.pointer, 0, 11, 1)
   WEND
  NEXT i
 END IF

 'first target
 IF bat.targ.force_first THEN
  bat.targ.pointer = 0
  WHILE bat.targ.mask(bat.targ.pointer) = 0
   bat.targ.pointer = loopvar(bat.targ.pointer, 0, 11, 1)
  WEND
 END IF

 'optional spread targetting
 IF bat.targ.opt_spread = 2 AND (carray(ccLeft) > 1 OR carray(ccRight) > 1) AND bat.targ.roulette = NO AND bat.targ.force_first = NO THEN
  FOR i = 0 TO 11
   bat.targ.selected(i) = 0
  NEXT i
  bat.targ.opt_spread = 1
  flusharray carray(), 7, 0
 END IF
 
 'arrow keys to select
 IF bat.targ.interactive = YES AND bat.targ.opt_spread < 2 AND bat.targ.roulette = NO AND bat.targ.force_first = NO THEN
  IF carray(ccUp) > 1 THEN
   battle_target_arrows -1, 1, bslot(), bat.targ, NO
  END IF
  IF carray(ccDown) > 1 THEN
   battle_target_arrows 1, 1, bslot(), bat.targ, NO
  END IF
  IF carray(ccLeft) > 1 THEN
   battle_target_arrows -1, 0, bslot(), bat.targ, YES
  END IF
  IF carray(ccRight) > 1 THEN
   battle_target_arrows 1, 0, bslot(), bat.targ, YES
  END IF
 END IF
 
 'confirm
 IF carray(ccUse) > 1 THEN battle_confirm_target bat, bslot()
 
END SUB
 
SUB battle_confirm_target(BYREF bat AS BattleState, bslot() AS BattleSprite)
 bat.targ.selected(bat.targ.pointer) = 1

 DIM t(11) AS INTEGER
 flusharray t(), 11, -1
 DIM o AS INTEGER = 0
 FOR i AS INTEGER = 0 TO 11
  IF bat.targ.selected(i) = 1 THEN
   t(o) = i
   o = o + 1
  END IF
 NEXT i
 queue_attack bslot(bat.hero_turn).attack - 1, bat.hero_turn, t()
 bslot(bat.hero_turn).attack = 0
 
 bslot(bat.hero_turn).ready_meter = 0
 bslot(bat.hero_turn).ready = NO
 bat.hero_turn = -1
 bat.targ.mode = targNONE
 bat.targ.hit_dead = NO
END SUB

SUB battle_display (BYREF bat AS BattleState, bslot() AS BattleSprite, menubits() AS INTEGER, st() AS HeroDef)
 'display:
 '--this sub currently draws the user-interface. In the future it will update
 '--user interface slices
 DIM i AS INTEGER
 DIM col AS INTEGER
 IF bat.vic.state = 0 THEN 'only display interface till you win
  FOR i = 0 TO 3 '--for each hero
   IF hero(i) > 0 THEN '--FIXME: should use some battle state instead of global state to
                       '--determine if the hero is present.
    IF readbit(gen(), 101, 6) = 0 THEN
     '--show the ready meter
     col = uilook(uiTimeBar)
     IF bslot(i).ready = YES THEN col = uilook(uiTimeBarFull)
     edgeboxstyle 1, 4 + i * 10, 132, 11, 0, dpage, YES, YES
     IF bslot(i).stat.cur.hp > 0 THEN
      DIM j AS INTEGER = bslot(i).ready_meter / 7.7
      IF blocked_by_attack(i) OR bslot(i).attack > 0 OR (bat.atk.id >= 0 AND bat.acting = i) THEN
       col = uilook(uiTimeBar)
       j = 130
      END IF
      rectangle 2, 5 + i * 10, j, 9, col, dpage
     END IF
    END IF
    IF readbit(gen(), 101, 7) = 0 THEN
     '--hp-meter--
     col = uiLook(uiHealthBar)
     IF bslot(i).lifemeter < INT((87 / large(bslot(i).stat.max.hp, 1)) * bslot(i).stat.cur.hp) THEN bslot(i).lifemeter += 1
     IF bslot(i).lifemeter > INT((87 / large(bslot(i).stat.max.hp, 1)) * bslot(i).stat.cur.hp) THEN bslot(i).lifemeter -= 1
     IF bslot(i).lifemeter > 87 THEN
      bslot(i).lifemeter = 87
      col = uiLook(uiHealthBar + bat.tog)
     END IF
     edgeboxstyle 136, 4 + i * 10, 89, 11, 0, dpage, YES, YES
     rectangle 137, 5 + i * 10, bslot(i).lifemeter, 9, col, dpage
    END IF
    '--name--
    col = uilook(uiMenuItem): IF i = bat.hero_turn THEN col = uilook(uiSelectedItem + bat.tog)
    edgeprint bslot(i).name, 128 - LEN(bslot(i).name) * 8, 5 + i * 10, col, dpage
    '--hp--
    edgeprint bslot(i).stat.cur.hp & "/" & bslot(i).stat.max.hp, 136, 5 + i * 10, col, dpage
    WITH bslot(i).stat
     DIM indicatorpos AS INTEGER = 217
     'poison indicator
     IF .cur.poison < .max.poison THEN
      edgeprint CHR(gen(genPoison)), indicatorpos, 5 + i * 10, col, dpage
      indicatorpos -= 8
     END IF
     'stun indicator
     IF .cur.stun < .max.stun THEN
      edgeprint CHR(gen(genStun)), indicatorpos, 5 + i * 10, col, dpage
      indicatorpos -= 8
     END IF
     'mute indicator
     IF .cur.mute < .max.mute THEN
      edgeprint CHR(gen(genMute)), indicatorpos, 5 + i * 10, col, dpage
     END IF
    END WITH
   END IF
  NEXT i
  IF bat.caption_time > 0 THEN
   bat.caption_time -= 1
   IF bat.caption_delay > 0 THEN
    bat.caption_delay -= 1
   ELSE
    centerbox 160, 186, 310, 16, 1, dpage
    edgeprint bat.caption, xstring(bat.caption, 160), 181, uilook(uiText), dpage
   END IF
  END IF
  IF bat.hero_turn >= 0 THEN
   centerbox 268, 5 + (4 * (bslot(bat.hero_turn).menu_size + 2)), 88, 8 * (bslot(bat.hero_turn).menu_size + 2), 1, dpage
   DIM bg AS INTEGER
   DIM fg AS INTEGER
   FOR i = 0 TO bslot(bat.hero_turn).menu_size
    bg = 0
    fg = uilook(uiMenuItem)
    IF bat.pt = i THEN
     fg = uilook(uiSelectedItem + bat.tog)
     bg = uilook(uiHighlight)
    END IF
    IF readbit(menubits(), 0, bat.hero_turn * 4 + i) THEN
     fg = uilook(uiDisabledItem)
     IF bat.pt = i THEN fg = uilook(uiSelectedDisabled + bat.tog)
    END IF
    textcolor fg, bg
    printstr bslot(bat.hero_turn).menu(i).caption, 228, 9 + i * 8, dpage
   NEXT i
   IF bat.targ.mode = targNONE AND readbit(gen(), genBits, 14) = 0 THEN
    edgeprint CHR$(24), bslot(bat.hero_turn).x + (bslot(bat.hero_turn).w / 2) - 4, bslot(bat.hero_turn).y - 5 + (bat.tog * 2), uilook(uiSelectedItem + bat.tog), dpage
   END IF
   IF bat.menu_mode = batMENUSPELL THEN '--draw spell menu
    centerbox 160, 53, 310, 95, 1, dpage
    IF bat.sptr < 24 THEN
     IF bat.spell.slot(bat.sptr).desc <> "" THEN rectangle 5, 74, 311, 1, uilook(uiTextBox + 1), dpage
    END IF
    rectangle 5, 87, 310, 1, uilook(uiTextBox + 1), dpage
    FOR i = 0 TO 23
     textcolor uilook(uiDisabledItem + bat.spell.slot(i).enable), 0
     IF bat.sptr = i THEN
      textcolor uilook(uiSelectedDisabled - (2 * ABS(bat.spell.slot(i).enable)) + bat.tog), uilook(uiHighlight)
     END IF
     printstr bat.spell.slot(i).name, 16 + (i MOD 3) * 104, 8 + (i \ 3) * 8, dpage
    NEXT i
    textcolor uilook(uiMenuItem), 0
    IF bat.sptr = 24 THEN textcolor uilook(uiSelectedItem + bat.tog), uilook(uiHighlight)
    printstr bat.cancel_spell_caption, 9, 90, dpage
    textcolor uilook(uiDescription), 0
    IF bat.sptr < 24 THEN
     printstr bat.spell.slot(bat.sptr).desc, 9, 77, dpage
     printstr bat.spell.slot(bat.sptr).cost, 308 - LEN(bat.spell.slot(bat.sptr).cost) * 8, 90, dpage
    END IF
   END IF
   'if keyval(scS) > 1 then gen(genMaxInventory) += 3
   'if keyval(scA) > 1 then gen(genMaxInventory) -= 3
   IF bat.menu_mode = batMENUITEM THEN '--draw item menu
    DIM inv_height AS INTEGER = small(78, 8 + INT((last_inv_slot() + 1) / 3) * 8)
    WITH bat.inv_scroll
     .top = INT(bat.item.top / 3)
     .pt = INT(bat.item.pt / 3)
     .last = INT(last_inv_slot() / 3)
    END WITH
    WITH bat.inv_scroll_rect
     .high = inv_height - 10
    END WITH
    edgeboxstyle 8, 4, 304, inv_height, 0, dpage
    draw_scrollbar bat.inv_scroll, bat.inv_scroll_rect, bat.inv_scroll.last, , dpage
    FOR i = bat.item.top TO small(bat.item.top + 26, last_inv_slot())
     if i < lbound(inventory) or i > ubound(inventory) then continue for
     textcolor uilook(uiDisabledItem - readbit(bat.iuse(), 0, i)), 0
     IF bat.item.pt = i THEN textcolor uilook(uiSelectedDisabled - (2 * readbit(bat.iuse(), 0, i)) + bat.tog), uilook(uiHighlight)
     printstr inventory(i).text, 20 + 96 * (i MOD 3), 8 + 8 * ((i - bat.item.top) \ 3), dpage
    NEXT i
    edgeboxstyle 8, 4 + inv_height, 304, 16, 0, dpage
    textcolor uilook(uiDescription), 0
    printstr bat.item_desc, 12, 8 + inv_height, dpage
   END IF
   IF bat.targ.mode > targNONE THEN
    FOR i = 0 TO 11
     IF bat.targ.selected(i) = 1 OR bat.targ.pointer = i THEN
      edgeprint CHR$(24), bslot(i).x + bslot(i).cursorpos.x - 4, bslot(i).y + bslot(i).cursorpos.y - 6, uilook(uiSelectedItem + bat.tog), dpage
      edgeprint bslot(i).name, xstring(bslot(i).name, bslot(i).x + bslot(i).cursorpos.x), bslot(i).y + bslot(i).cursorpos.y - 16, uilook(uiSelectedItem + bat.tog), dpage
     END IF
    NEXT i
   END IF
  END IF
 END IF'--end if bat.vic.state = 0
END SUB


SUB battle_meters (BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
 IF bat.away > 0 THEN EXIT SUB '--skip all this if the heroes have already run away
 
 '--if a menu is up, and pause-on-menus is ON then no time passes (as long as at least one visible targetable enemy is alive)
 DIM isdeepmenu AS INTEGER = (bat.menu_mode > 0 AND readbit(gen(), genBits, 0) <> 0)
 DIM isbattlemenu AS INTEGER = (bat.menu_mode >= 0 AND bat.hero_turn >= 0 AND readbit(gen(), genBits, 13) <> 0)
 DIM isenemytargs AS INTEGER = (targenemycount(bslot()) > 0)
 IF (isdeepmenu OR isbattlemenu) AND isenemytargs THEN EXIT SUB

 DIM i AS INTEGER

 FOR i = 0 TO 11
 
  '--poison
  WITH bslot(i).stat
   IF .cur.poison < .max.poison THEN
    bslot(i).poison_repeat += large(.cur.spd, 7)
    IF bslot(i).poison_repeat >= 1500 THEN
     bslot(i).poison_repeat = 0
     DIM harm AS INTEGER = .max.poison - .cur.poison
     harm = range(harm, 20)
     quickinflict harm, i, bslot()
     triggerfade i, bslot()
     fulldeathcheck -1, bat, bslot(), formdata()
    END IF
   END IF
  END WITH
 
  '--regen
  WITH bslot(i).stat
   IF .cur.regen < .max.regen THEN
    bslot(i).regen_repeat += large(.cur.spd, 7)
    IF bslot(i).regen_repeat >= 1500 THEN
     bslot(i).regen_repeat = 0
     DIM heal AS INTEGER = .max.regen - .cur.regen
     heal = heal * -1
     heal = range(heal, 20)
     quickinflict heal, i, bslot()
     triggerfade i, bslot()
     fulldeathcheck -1, bat, bslot(), formdata()
    END IF
   END IF
  END WITH

  '--if not doing anything, not dying, not ready, and not stunned
  IF ready_meter_may_grow(bslot(), i) THEN
   '--increment ctr by speed
   bslot(i).ready_meter = small(1000, bslot(i).ready_meter + bslot(i).stat.cur.spd)
   IF bslot(i).ready_meter = 1000 AND bat.wait_frames = 0 THEN bslot(i).ready = YES
  END IF

 NEXT i

 '--decrement stun and mute

 IF TIMER > bat.laststun + 1 THEN
  FOR i = 0 TO 11
   bslot(i).stat.cur.mute = small(bslot(i).stat.cur.mute + 1, bslot(i).stat.max.mute)
   bslot(i).stat.cur.stun = small(bslot(i).stat.cur.stun + 1, bslot(i).stat.max.stun)
   IF bslot(i).stat.cur.stun < bslot(i).stat.max.stun THEN
    bslot(i).ready = NO
    IF bat.hero_turn = i THEN bat.hero_turn = -1
    IF bat.enemy_turn = i THEN bat.enemy_turn = -1
   END IF
  NEXT i
  bat.laststun = TIMER
 END IF

 '--decrement attack queue delays

 FOR i = 0 TO UBOUND(atkq)
  WITH atkq(i)
   IF .used THEN
    IF bslot(.attacker).stat.cur.stun >= bslot(.attacker).stat.max.stun THEN   
     .delay = large(0, .delay - 1)
    END IF
   END IF
  END WITH
 NEXT i

END SUB

SUB battle_animate(BYREF bat AS BattleState, bslot() AS BattleSprite)
 'This sub is intended to apply animation effects triggered elsewhere.
 'FIXME: due to messy code, some stuff animation stuff might still happen elsewhere
 DIM i AS INTEGER
 '--First, things that only heroes can do
 FOR i = 0 TO 3
  IF bslot(i).walk = 1 THEN bslot(i).frame = bslot(i).frame XOR bat.tog
  IF bat.acting <> i THEN enforce_weak_picture i, bslot(), bat
  IF bat.vic.state > 0 AND bslot(i).stat.cur.hp > 0 AND bat.tog = 0 THEN
   if bslot(i).frame = 0 then bslot(i).frame = 2 else bslot(i).frame = 0
  END IF
 NEXT i
 '--Then apply movement forces for all things, heroes, enemies, attacks, weapons
 FOR i = 0 TO 23
  WITH bslot(i)
   IF .xmov <> 0 THEN .x = .x + (.xspeed * SGN(.xmov)): .xmov = .xmov - SGN(.xmov)
   IF .ymov <> 0 THEN .y = .y + (.yspeed * SGN(.ymov)): .ymov = .ymov - SGN(.ymov)
   IF .zmov <> 0 THEN .z = .z + (.zspeed * SGN(.zmov)): .zmov = .zmov - SGN(.zmov)
  END WITH
 NEXT i
 '--then, stuff that only attacks can do
 FOR i = 12 TO 23 '--for each attack sprite
  IF bslot(i).vis = 1 THEN
   WITH bslot(i)
    .anim_index += 1
    '--each pattern ends with a -1. If we have found it, loop around
    IF bat.animpat(.anim_pattern).frame(.anim_index) = -1 THEN .anim_index = 0
    .frame = bat.animpat(.anim_pattern).frame(.anim_index)
    IF .frame = -1 THEN
     '--if the frame get set to -1 that indicates an empty pattern, so randomize instead
     .frame = INT(RND * 3)
    END IF
   END WITH
  END IF
 NEXT i
 '--Then death animations
 FOR i = 0 TO 11
  IF bslot(i).dissolve > 0 THEN
   'ENEMIES DEATH THROES
   IF is_enemy(i) THEN
    IF bslot(i).flee THEN
     'running away
     bslot(i).x = bslot(i).x - 10: bslot(i).d = 1
    END IF
    bslot(i).dissolve -= 1
    IF bslot(i).dissolve = 0 THEN
     'make dead enemy invisible (the check_death code will actually do the final removal, which might happen before the enemy has finished dissolving)
     bslot(i).vis = 0
    END IF
   END IF
   IF is_hero(i) THEN bslot(i).frame = 7
  END IF
 NEXT i
END SUB

SUB show_enemy_meters(bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
 'This shows meters and extra debug info info when you press F10 the first time
 DIM c AS INTEGER
 DIM info AS STRING
 FOR i AS INTEGER = 0 TO 11
  WITH bslot(i)
   c = uilook(uiSelectedDisabled)
   IF is_hero(i) THEN c = uilook(uiSelectedItem)
   rectangle 0, 80 + (i * 10), .ready_meter / 10, 4, c, dpage
   info = "v=" & .vis & " tm=" & bat.targ.mask(i) & " hp=" & .stat.cur.hp & " dz=" & .dissolve & " a=" & .attack 
   IF is_enemy(i) THEN info &= " fm=" & formdata((i-4)*4) 
   edgeprint info, 20, 80 + i * 10, c, dpage
  END WITH
 NEXT i
END SUB

SUB battle_crappy_run_handler(BYREF bat AS BattleState, bslot() AS BattleSprite)
 '--Current running system sucks about as bad as a running system conceivably CAN suck
 IF carray(ccMenu) > 1 AND readbit(gen(), genBits2, 1) = 0 THEN
  bat.flee = bat.flee + 1
 END IF
 DIM i AS INTEGER
 IF bat.flee > 0 AND bat.flee < 4 THEN
  IF carray(ccRun) = 0 THEN
   bat.flee = 0
   FOR i = 0 TO 3
    bslot(i).d = 0
    bslot(i).walk = 0
   NEXT i
  END IF
 END IF
 IF bat.flee = 4 THEN
  IF checkNoRunBit(bslot()) THEN
   bat.flee = 0
   bat.alert = bat.cannot_run_caption
   bat.alert_ticks = 10
  END IF
 END IF
 IF bat.flee > 4 THEN
  FOR i = 0 TO 3
   '--if alive turn around
   IF bslot(i).stat.cur.hp THEN bslot(i).d = 1
   bslot(i).walk = 1
   bslot(i).attack = 0
   bslot(i).ready = NO
   bslot(i).ready_meter = large(0, bslot(i).ready_meter - bslot(i).stat.cur.spd * 2)
  NEXT i
  IF carray(ccRun) = 0 THEN
   bat.flee = 0
   FOR i = 0 TO 3
    bslot(i).d = 0
    bslot(i).walk = 0
   NEXT i
  END IF
  DIM stupid_run_threshold AS INTEGER = 400
  FOR i = 4 TO 11
   stupid_run_threshold += bslot(i).stat.cur.spd
  NEXT i
  IF RND * stupid_run_threshold < bat.flee THEN
   bat.away = 1
   bat.flee = 2
   FOR i = 0 TO 3
    bslot(i).ready_meter = 0
    bslot(i).ready = NO
   NEXT i
  END IF
 END IF
END SUB

SUB draw_battle_sprites(bslot() AS BattleSprite)
 DIM zbuf(24) AS INTEGER
 DIM basey(24) AS INTEGER
 DIM harm_text_offset AS INTEGER = 0
 
 FOR i AS INTEGER = 0 TO 24
  basey(i) = bslot(i).y + bslot(i).h
 NEXT i
 sort_integers_indices(zbuf(), @basey(0))
 FOR i AS INTEGER = 0 TO 24
 IF (bslot(zbuf(i)).vis = 1 OR bslot(zbuf(i)).dissolve > 0) THEN
   dim w as BattleSprite ptr
   w = @bslot(zbuf(i))
   with bslot(zbuf(i))
    dim spr as frame ptr = .sprites
    
    if .sprites = 0 then continue for
    
    'debug(str(zbuf(i)))
    
    if .frame < .sprite_num then spr += .frame
    
    if .d then
     spr = frame_duplicate(spr)
     frame_flip_horiz(spr)
    else
     spr = frame_reference(spr)
    end if
    
    if is_enemy(zbuf(i)) and .dissolve > 0 and .flee = 0 then
     dim spr2 as frame ptr = spr
     spr = frame_dissolved(spr2, .deathtime, .deathtime - .dissolve, .deathtype)
     frame_unload(@spr2)
    end if
    
    frame_draw(spr, .pal, .x, .y - .z, 1, -1, dpage)
    
    frame_unload(@spr)
   end with
  END IF
 NEXT i
 FOR i AS INTEGER = 0 TO 11
  WITH bslot(i).harm
   IF .ticks > 0 THEN
    IF gen(genDamageDisplayTicks) <> 0 THEN
     harm_text_offset = gen(genDamageDisplayRise) / gen(genDamageDisplayTicks) * (gen(genDamageDisplayTicks) - .ticks)
    ELSE
     harm_text_offset = 0 'Avoid div by zero (which shouldn't be possible anyway)
    END IF
    edgeprint .text, .pos.x - LEN(.text) * 4, .pos.y - harm_text_offset, .col, dpage
    .ticks -= 1
    IF .ticks = 0 THEN .col = uilook(uiText)
   END IF
  END WITH
 NEXT i
END SUB

SUB battle_loadall(BYVAL form AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, st() AS HeroDef, formdata())
 DIM i AS INTEGER

 setpicstuf formdata(), 80, -1
 loadset tmpdir & "for.tmp", form, 0

 for i = 0 to 24
  bslot(i).frame = 0
  bslot(i).sprites = 0
  bslot(i).pal = 0
  bslot(i).attack = 0
 next i

 IF formdata(33) > 0 THEN wrappedsong formdata(33) - 1
 
 DIM attack AS AttackData
 DIM newm AS INTEGER
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   loadherodata @st(i), hero(i) - 1
   newm = 0
   
   'Loop through hero battle menu, populating bslot().menu() with the ones that should be displayed
   'Note that bmenu() is a global.
   FOR oldm AS INTEGER = 0 TO 5
    IF bmenu(i, oldm) < 0 AND bmenu(i, oldm) > -5 AND readbit(st(i).bits(),0,26) <> 0 THEN
     'this is a spell list, and the hide empty spell lists bitset is on...
     'count the spells, and skip if empty
     IF count_available_spells(i, ABS(bmenu(i, oldm)) - 1) = 0 THEN CONTINUE FOR
    END IF
    bslot(i).menu(newm).menu = bmenu(i,oldm)
    newm += 1
   NEXT oldm
  
   '--init bslot() for each hero
   WITH bslot(i)
    .basex = (240 + i * 8)
    .basey = (82 + i * 20)
    .x =  .basex
    .y =  .basey
    .w = 32
    .h = 40
    .vis = 1
    'load hero sprites
    .sprite_num = 8
    .sprites = frame_load(0, gam.hero(i).battle_pic)
    .pal = palette16_load(gam.hero(i).battle_pal, 0, gam.hero(i).battle_pic)
    .frame = 0
    .death_sfx = -1 'No death sounds for heroes (for now)
    .cursorpos.x = .w / 2
    .cursorpos.y = 0
   END WITH
   
   '--copy hero's outside-battle stats to their inside-battle stats
   FOR o AS INTEGER = 0 TO 11
    bslot(i).stat.cur.sta(o) = gam.hero(i).stat.cur.sta(o)
    bslot(i).stat.max.sta(o) = gam.hero(i).stat.max.sta(o)
   NEXT o
   
   herobattlebits bslot(i), i
   bslot(i).name = names(i)
   
   '--load hero menu captions
   FOR o AS INTEGER = 0 TO 5
    WITH bslot(i).menu(o)
     .caption = ""
     .atk = -1 'none
     .spell_list = -1 'none
     IF .menu > 0 THEN
      '--positive number is a fixed attack number (currently only for equipped weapon)
      .atk = .menu - 1
      loadattackdata attack, .atk
      .caption = attack.name
     ELSEIF .menu < 0 AND .menu > -5 THEN
      .spell_list = ABS(.menu) - 1
      .caption = st(i).list_name(.spell_list)
     ELSEIF .menu = -10 THEN
      .caption = readglobalstring$(34, "Item", 10)
      .spell_list = -10
      bslot(i).menu_size = o
     END IF
     .caption = rpad(.caption, " ", 10)
    END WITH
   NEXT o

   'wipe spells learnt and levels gained
   FOR o AS INTEGER = i * 6 TO i * 6 + 5
    learnmask(o) = 0
   NEXT
   gam.hero(i).lev_gain = 0
   
  ELSE
   '--blank empty hero slots
   bslot(i).sprites = 0
  END IF
 NEXT i
 
 '--load monsters
 FOR i = 0 TO 7
  loadfoe i, formdata(), bat, bslot(), YES
 NEXT i
 
 FOR i = 0 TO 11
  IF readbit(gen(), genBits2, 6) = 0 THEN
   bslot(i).ready_meter = INT(RND * 500) '--randomize ready-meter
  END IF
 NEXT i
 
 '--size attack sprites
 FOR i = 12 TO 23
  bslot(i).w = 50
  bslot(i).h = 50
 NEXT i
 
 bat.curbg = formdata(32)
 loadmxs game + ".mxs", bat.curbg, vpages(2)
 
 '--This checks weak/dead status for heroes
 '-- who are already weak/dead at the beginning of battle
 FOR i = 0 TO 3
  enforce_weak_picture i, bslot(), bat
  IF hero(i) > 0 AND bslot(i).stat.cur.hp = 0 THEN
   '--hero starts the battle dead
   bslot(i).dissolve = 1 'Keeps the dead hero from vanishing
   bslot(i).frame = 7
  END IF
  bslot(i).lifemeter = (88 / large(bslot(i).stat.max.hp, 1)) * bslot(i).stat.cur.hp
 NEXT i

 '--size the weapon sprite 
 bslot(24).w = 24
 bslot(24).h = 24
 
 'trigger fades for dead enemies
 'fulldeathcheck fades out only enemies set to die without a boss
 'so additionally call triggerfade on 0 hp enemies here
 'or might that be expected behaviour in some games?
 FOR i = 4 TO 11
  IF bslot(i).stat.cur.hp <= 0 THEN
   triggerfade i, bslot()
  END IF
 NEXT i
 fulldeathcheck -1, bat, bslot(), formdata()
END SUB

SUB fulldeathcheck (killing_attack AS INTEGER, bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
 '--Runs check_death on all enemies, checks all heroes for death, and sets bat.death_mode if necessary
 'killing_attack is the attack ID that was just used, or -1 for none
 DIM deadguy AS INTEGER
 DIM dead_enemies AS INTEGER
 DIM dead_heroes AS INTEGER
 FOR deadguy = 4 TO 11
  IF bslot(deadguy).stat.cur.hp > 0 THEN
   'this enemy hasn't just spawned; it should fade out
   IF dieWOboss(deadguy, bslot()) THEN
    triggerfade deadguy, bslot()
   END IF
  END IF
 NEXT
 FOR deadguy = 0 TO 11
  check_death deadguy, killing_attack, bat, bslot(), formdata()
 NEXT
 dead_enemies = 0
 FOR deadguy = 4 TO 11
  IF bslot(deadguy).stat.cur.hp = 0 OR bslot(deadguy).hero_untargetable = YES OR bslot(deadguy).death_unneeded = YES THEN dead_enemies += 1
 NEXT
 IF dead_enemies >= 8 THEN bat.death_mode = deathENEMIES
 dead_heroes = 0
 FOR deadguy = 0 TO 3
  IF bslot(deadguy).stat.cur.hp = 0 THEN dead_heroes += 1
 NEXT deadguy
 IF dead_heroes = 4 THEN bat.death_mode = deathHEROES
END SUB

SUB trigger_victory(BYREF bat AS BattleState, bslot() AS BattleSprite)
 DIM AS INTEGER i, numheroes
 '--Play the victory music
 IF gen(genVictMus) > 0 THEN wrappedsong gen(genVictMus) - 1
 '--Collect gold (which is capped at 2 billion max)
 gold = gold + bat.rew.plunder
 IF gold < 0 OR gold > 2000000000 THEN gold = 2000000000
 '--Divide experience by heroes
 IF readbit(gen(), genBits2, 3) THEN 'dead heroes get experience
  numheroes = herocount()
 ELSE
  numheroes = liveherocount(bslot())
 END IF
 IF numheroes > 0 THEN bat.rew.exper = bat.rew.exper / numheroes
 '--Collect experience and apply levelups
 FOR i = 0 TO 3
  IF readbit(gen(), genBits2, 3) <> 0 OR bslot(i).stat.cur.hp > 0 THEN giveheroexperience i, bat.rew.exper
  updatestatslevelup i, bslot(i).stat, 0
 NEXT i
 '--Trigger the display of end-of-battle rewards
 bat.vic.state = vicGOLDEXP
END SUB

SUB show_victory (BYREF bat AS BattleState, bslot() AS BattleSprite)
DIM tempstr AS STRING
DIM AS INTEGER i, o
IF bat.vic.box THEN centerfuz 160, 30, 280, 50, 1, dpage
SELECT CASE bat.vic.state
 CASE vicGOLDEXP
  '--print acquired gold and experience
  IF bat.rew.plunder > 0 OR bat.rew.exper > 0 THEN bat.vic.box = YES: centerfuz 160, 30, 280, 50, 1, dpage
  IF bat.rew.plunder > 0 THEN
   tempstr = bat.vic.gold_caption & " " & bat.rew.plunder & " " & bat.vic.gold_name & "!"
   edgeprint tempstr, xstring(tempstr, 160), 16, uilook(uiText), dpage
  END IF
  IF bat.rew.exper > 0 THEN
   tempstr = bat.vic.exp_caption & " " & bat.rew.exper & " " & bat.vic.exp_name & "!"
   edgeprint tempstr, xstring(tempstr, 160), 28, uilook(uiText), dpage
  END IF
  IF carray(ccUse) > 1 OR carray(ccMenu) > 1 OR (bat.rew.plunder = 0 AND bat.rew.exper = 0) THEN
   bat.vic.state = vicLEVELUP
  END IF
 CASE vicLEVELUP
  '--print levelups
  o = 0
  FOR i = 0 TO 3
   IF o = 0 AND gam.hero(i).lev_gain <> 0 THEN centerfuz 160, 30, 280, 50, 1, dpage: bat.vic.box = YES
   SELECT CASE gam.hero(i).lev_gain
    CASE 1
     tempstr = bat.vic.level_up_caption & " " & bslot(i).name
    CASE IS > 1
     tempstr = gam.hero(i).lev_gain & " " & bat.vic.levels_up_caption & " " & bslot(i).name
   END SELECT
   IF gam.hero(i).lev_gain > 0 THEN
    edgeprint tempstr, xstring(tempstr, 160), 12 + i * 10, uilook(uiText), dpage
    o = 1
   END IF
  NEXT i
  IF o = 0 OR (carray(ccUse) > 1 OR carray(ccMenu) > 1) THEN
   bat.vic.state = vicSPELLS
   bat.vic.showlearn = NO
   bat.vic.learnwho = 0
   bat.vic.learnlist = 0
   bat.vic.learnslot = -1
  END IF
 CASE vicSPELLS
  '--print learned spells, one at a time
  IF bat.vic.showlearn = NO THEN '--Not showing a spell yet. find the next one
   DO
    bat.vic.learnslot += 1
    IF bat.vic.learnslot > 23 THEN bat.vic.learnslot = 0: bat.vic.learnlist = bat.vic.learnlist + 1
    IF bat.vic.learnlist > 3 THEN bat.vic.learnlist = 0: bat.vic.learnwho = bat.vic.learnwho + 1
    IF bat.vic.learnwho > 3 THEN ' We have iterated through all spell lists for all heroes, time to move on
     bat.vic.state = vicITEMS
     bat.vic.item_name = ""
     bat.vic.found_index = 0
     bat.vic.box = NO
     EXIT DO
    END IF
    IF readbit(learnmask(), 0, bat.vic.learnwho * 96 + bat.vic.learnlist * 24 + bat.vic.learnslot) THEN
     'found a learned spell
     DIM learn_attack AS AttackData
     loadattackdata learn_attack, spell(bat.vic.learnwho, bat.vic.learnlist, bat.vic.learnslot) - 1
     bat.vic.item_name = bslot(bat.vic.learnwho).name + bat.vic.learned_caption
     bat.vic.item_name = bat.vic.item_name & learn_attack.name
     bat.vic.showlearn = YES
     bat.vic.box = YES
     IF learn_attack.learn_sound_effect > 0 THEN playsfx learn_attack.learn_sound_effect - 1
     EXIT DO
    END IF
   LOOP
  ELSE' Found a learned spell to display, show it until a keypress
   IF carray(ccUse) > 1 OR carray(ccMenu) > 1 THEN
    bat.vic.showlearn = NO ' hide the display (which causes us to search for the next learned spell)
   END IF
   edgeprint bat.vic.item_name, xstring(bat.vic.item_name, 160), 22, uilook(uiText), dpage
  END IF
 CASE vicITEMS
  '--print found items, one at a time
  '--check to see if we are currently displaying a gotten item
  IF bat.vic.item_name = "" THEN
   '--if not, check to see if there are any more gotten items to display
   IF bat.rew.found(bat.vic.found_index).num = 0 THEN bat.vic.state = vicEXITDELAY: EXIT SUB
   '--get the item name
   bat.vic.item_name = readitemname$(bat.rew.found(bat.vic.found_index).id)
   '--actually aquire the item
   getitem bat.rew.found(bat.vic.found_index).id + 1, bat.rew.found(bat.vic.found_index).num
  END IF
  '--if the present item is gotten, show the caption
  IF bat.rew.found(bat.vic.found_index).num = 1 THEN
   tempstr = bat.vic.item_caption & " " & bat.vic.item_name
  ELSE
   tempstr = bat.vic.plural_item_caption & " " & bat.rew.found(bat.vic.found_index).num & " " & bat.vic.item_name
  END IF
  IF LEN(tempstr) THEN centerfuz 160, 30, 280, 50, 1, dpage
  edgeprint tempstr, xstring(tempstr, 160), 22, uilook(uiText), dpage
  '--check for a keypress
  IF carray(ccUse) > 1 OR carray(ccMenu) > 1 THEN
   IF bat.rew.found(bat.vic.found_index).num = 0 THEN
    '--if there are no further items, exit
    bat.vic.state = -1
   ELSE
    '--otherwize, increment the findpointer and reset the caption
    bat.vic.found_index = bat.vic.found_index + 1: bat.vic.item_name = ""
   END IF
  END IF
END SELECT
END SUB

SUB reset_battle_state (BYREF bat AS BattleState)
 'This could become a constructor for BattleState when we support the -lang fb dialect

 WITH bat

  WITH .inv_scroll
   .first = 0
   .last = INT(last_inv_slot() / 3)
   .size = 8
  END WITH
  WITH .inv_scroll_rect
   .x = 20
   .y = 8
   .wide = 292
   '.high set later
  END WITH

  .anim_ready = NO
  .wait_frames = 0

  .level_mp_caption = readglobalstring(160, "Level MP", 20)
  .cancel_spell_caption = readglobalstring(51, "(CANCEL)", 10)
  .cannot_run_caption = readglobalstring(147, "CANNOT RUN!", 20)

  .caption_time = 0
  .caption_delay = 0
  .caption = ""

  .alert_ticks = 0
  .alert = ""

  .acting = 0
  .hero_turn = -1
  .enemy_turn = -1
  .next_attacker = 0
  .next_hero = 0
  .next_enemy = 0
  .menu_mode = batMENUHERO

  .laststun = TIMER

 END WITH
 
 reset_targetting bat
 reset_attack bat
 reset_victory_state bat.vic
 reset_rewards_state bat.rew
END SUB

SUB reset_targetting (BYREF bat AS BattleState)
 WITH bat.targ
  .hit_dead = NO
 END WITH
END SUB

SUB reset_attack (BYREF bat AS BattleState)
 DIM i AS INTEGER
 WITH bat.atk
  .id = -1
  .was_id = -1
  .non_elemental = NO
  FOR i = 0 TO UBOUND(.elemental)
  .elemental(i) = NO
  NEXT i
 END WITH
END SUB

SUB reset_rewards_state (BYREF rew AS RewardsState)
 'This could become a constructor for RewardsState when we support the -lang fb dialect
 DIM i AS INTEGER
 WITH rew
  .plunder = 0
  .exper = 0
  FOR i = 0 TO UBOUND(.found)
   .found(i).id = 0
   .found(i).num = 0
  NEXT i
 END WITH
END SUB

SUB reset_victory_state (BYREF vic AS VictoryState)
 'This could become a constructor for VictoryState when we support the -lang fb dialect
 WITH vic
  .state = 0
  .box = NO
  .showlearn = NO
  .learnwho = 0
  .learnlist = 0
  .learnslot = -1
  .item_name = ""
  .found_index = 0
  '--Cache some global strings here
  .gold_caption = readglobalstring$(125, "Found", 10)
  .exp_caption  = readglobalstring$(126, "Gained", 10)
  .item_caption = readglobalstring$(139, "Found a", 20)
  .plural_item_caption = readglobalstring$(141, "Found", 20)
  .gold_name    = readglobalstring(32, "Gold", 10)
  .exp_name     = readglobalstring(33, "Exp", 10)
  .level_up_caption  = readglobalstring$(149, "Level up for", 20)
  .levels_up_caption = readglobalstring$(151, "levels for", 20)
  .learned_caption = " " + readglobalstring$(124, "learned", 10) + " "
 END WITH
END SUB

SUB checkitemusability(iuse() AS INTEGER, bslot() AS BattleSprite, who AS INTEGER)
 'Iterate through the iuse() bitfield and mark any items that are usable
 DIM i AS INTEGER
 DIM itembuf(99) AS INTEGER
 DIM attack AS AttackData

 FOR i = 0 TO inventoryMax
  setbit iuse(), 0, i, 0 ' Default each slot to unusable
  IF inventory(i).used THEN
   loaditemdata itembuf(), inventory(i).id
   IF itembuf(47) > 0 THEN ' This item is usable in battle
    loadattackdata attack, itembuf(47) - 1
    IF attack.check_costs_as_item THEN
     '--This attack has the bitset that requires cost checking when used from an item
     IF atkallowed(attack, who, 0, 0, bslot()) THEN
      setbit iuse(), 0, i, 1
     END IF
    ELSE
     '--No cost checking for this item
     setbit iuse(), 0, i, 1
    END IF
   END IF
  END IF
 NEXT i
END SUB

FUNCTION checkNoRunBit (bslot() AS BattleSprite) as integer
 DIM i AS INTEGER
 FOR i = 4 TO 11
  IF bslot(i).stat.cur.hp > 0 AND bslot(i).vis = 1 AND bslot(i).unescapable = YES THEN RETURN 1
 NEXT i
 RETURN 0
END FUNCTION

SUB checkTagCond (t AS AttackDataTag, check AS INTEGER)
 't.condition - type
 'check = curtype
 't.tag - the tag to be set
 't.tagcheck - the tag to check
 IF t.condition = check THEN
  IF t.tagcheck <> 0 AND readbit(tag(), 0, ABS(t.tagcheck)) <> SGN(SGN(t.tagcheck) + 1) THEN EXIT SUB
  setbit tag(), 0, ABS(t.tag), SGN(SGN(t.tag) + 1) 'Set the original damned tag!
 END IF
END SUB

FUNCTION focuscost (cost, focus) as integer
IF focus > 0 THEN
 focuscost = cost - INT(cost / (100 / focus))
ELSE
 focuscost = cost
END IF
END FUNCTION

SUB herobattlebits_raw (bitbuf(), who)
 'This loads into a bit buffer. It is used both here and in the status menu
 DIM i AS INTEGER
 DIM j AS INTEGER

 '--native bits
 FOR i = 0 TO 4
  bitbuf(who, i) = nativehbits(who, i)
 NEXT i

 DIM itembuf(99) AS INTEGER

 '--merge equipment bits
 FOR j = 0 TO 4
  IF eqstuf(who, j) > 0 THEN
   loaditemdata itembuf(), eqstuf(who, j) - 1
   FOR i = 0 TO 4
    bitbuf(who, i) = (bitbuf(who, i) OR itembuf(70 + i))
   NEXT i
  END IF
 NEXT j

END SUB

SUB herobattlebits (bspr AS BattleSprite, who)
DIM i AS INTEGER
DIM bitbuf(11, 4) AS INTEGER ' Temporary buffer for getting bits. FIXME: remove this eventually
DIM tempbits(1) AS INTEGER  ' Temporary buffer for reading bits

herobattlebits_raw bitbuf(), who

'--Copy elemental bits to bspr
tempbits(0) = bitbuf(who, 0)
tempbits(1) = bitbuf(who, 1)
FOR i = 0 TO 7
 WITH bspr
  .weak(i) = xreadbit(tempbits(), i)
  .strong(i) = xreadbit(tempbits(), 8 + i)
  .absorb(i) = xreadbit(tempbits(), 16 + i)
  .enemytype(i) = xreadbit(tempbits(), 24 + i)
 END WITH
NEXT i

END SUB

SUB invertstack
'--this is a hack so I can use the stack like a fifo
DIM i AS INTEGER
DIM stackdepth AS INTEGER

stackdepth = (stackpos - bstackstart) \ 2

FOR i = 0 TO stackdepth - 1
 buffer(i) = popw
NEXT i

FOR i = 0 TO stackdepth - 1
 pushw buffer(i)
NEXT i

END SUB

SUB quickinflict (harm, targ, bslot() AS BattleSprite)
 '--quick damage infliction to hp. no bells and whistles
 DIM max_bound AS INTEGER
 WITH bslot(targ)

  IF gen(genDamageCap) > 0 THEN harm = small(harm, gen(genDamageCap))

  .harm.ticks = gen(genDamageDisplayTicks)
  .harm.pos.x = .x + (.w * .5)
  .harm.pos.y = .y + (.h * .5)
  IF harm < 0 THEN
   .harm.text = "+" & ABS(harm)
  ELSE
   .harm.text = STR(harm)
  END IF

  max_bound = large(.stat.cur.hp, .stat.max.hp)
  .stat.cur.hp = bound(.stat.cur.hp - harm, 0, max_bound)
 END WITH
END SUB

SUB anim_end()
 pushw 0
END SUB

SUB anim_inflict(who AS INTEGER, target_count AS INTEGER)
 pushw 10: pushw who: pushw target_count
END SUB

SUB anim_disappear(who)
 pushw 6: pushw who
END SUB

SUB anim_setpos(who, x, y, d)
 pushw 3: pushw who: pushw x: pushw y: pushw d
END SUB

SUB anim_setz(who, z)
 pushw 11: pushw who: pushw z
END SUB

SUB anim_appear(who)
 pushw 5: pushw who
END SUB

SUB anim_setmove(who, xm, ym, xstep, ystep)
 pushw 2: pushw who: pushw xm: pushw ym: pushw xstep: pushw ystep
END SUB

SUB anim_absmove(who, tox, toy, xspeed, yspeed)
 pushw 8: pushw who: pushw tox: pushw toy: pushw xspeed: pushw yspeed
END SUB

SUB anim_zmove(who, zm, zstep)
 pushw 15: pushw who: pushw zm: pushw zstep
END SUB

SUB anim_wait(ticks)
 pushw 13: pushw ticks
END SUB

SUB anim_setframe(who, frame)
 pushw 7: pushw who: pushw frame
END SUB

SUB anim_waitforall()
 pushw 9
END SUB

SUB anim_walktoggle(who)
 pushw 14: pushw who
END SUB

SUB anim_sound(which)
 pushw 16: pushw which
END SUB

SUB anim_align(who, target, dire, offset)
 pushw 17: pushw who: pushw target: pushw dire: pushw offset
END SUB

SUB anim_setcenter(who, target, offx, offy)
 pushw 18: pushw who: pushw target: pushw offx: pushw offy
END SUB

SUB anim_align2(who, target, edgex, edgey, offx, offy)
 pushw 19: pushw who: pushw target: pushw edgex: pushw edgey: pushw offx: pushw offy
END SUB

SUB anim_relmove(who, tox, toy, xspeed, yspeed)
 pushw 20: pushw who: pushw tox: pushw toy: pushw xspeed: pushw yspeed
END SUB

SUB anim_setdir(who, d)
 pushw 21: pushw who: pushw d
END SUB

SUB anim_flinchstart(who AS INTEGER, bslot() AS BattleSprite, attack AS AttackData)
 '--If enemy can flinch and if attack allows flinching
 IF bslot(who).never_flinch = NO AND attack.targ_does_not_flinch = NO THEN
  DIM flinch_x_dist AS INTEGER
  flinch_x_dist = 3
  IF is_enemy(who) THEN flinch_x_dist = -3
  anim_setmove who, flinch_x_dist, 0, 2, 0
  IF is_hero(who) THEN
   IF attack.cure_instead_of_harm = NO THEN
    '--Show Harmed frame
    anim_setframe who, 5
   ELSE
    '--Show attack1 frame when healed
    anim_setframe who, 2
   END IF
  END IF
 END IF
END SUB

SUB anim_flinchdone(who AS INTEGER, bslot() AS BattleSprite, attack AS AttackData)
 '--If enemy can flinch and if attack allows flinching
 IF bslot(who).never_flinch = NO AND attack.targ_does_not_flinch = NO THEN
  DIM flinch_x_dist AS INTEGER
  flinch_x_dist = -3
  IF is_enemy(who) THEN flinch_x_dist = 3
  anim_setmove who, flinch_x_dist, 0, 2, 0
  anim_setframe who, 0
 END IF
END SUB

FUNCTION count_dissolving_enemies(bslot() AS BattleSprite) AS INTEGER
 DIM i AS INTEGER
 DIM count AS INTEGER = 0
 FOR i = 4 TO 11
  IF bslot(i).dissolve > 0 THEN count += 1
 NEXT i
 RETURN count
END FUNCTION

SUB spawn_on_death(deadguy AS INTEGER, killing_attack AS INTEGER, BYREF bat AS BattleState, formdata(), bslot() AS BattleSprite)
 'killing_attack is the id of the attack that killed the target or -1 if the target died without a specific attack
 DIM attack AS AttackData
 DIM slot AS INTEGER
 DIM i AS INTEGER
 IF NOT is_enemy(deadguy) THEN EXIT SUB ' Only works for enemies
 IF killing_attack >= 0 THEN
  'This death is the result of an attack
  loadattackdata attack, killing_attack
  IF attack.no_spawn_on_kill <> 0 THEN
   'This attack had the "Do not trigger spawning on death" bitset
   EXIT SUB
  END IF
 END IF
 WITH bslot(deadguy)
  IF .enemy.spawn.non_elemental_death > 0 AND bat.atk.non_elemental = YES THEN ' spawn on non-elemental death
   FOR i = 1 TO .enemy.spawn.how_many
    slot = find_empty_enemy_slot(formdata())
    IF slot > -1 THEN
     formdata(slot * 4) = .enemy.spawn.non_elemental_death
     loadfoe(slot, formdata(), bat, bslot())
    END IF
   NEXT i
   .enemy.spawn.non_elemental_death = 0
  END IF
  IF .enemy.spawn.on_death > 0 THEN ' spawn on death
   FOR i = 1 TO .enemy.spawn.how_many
    slot = find_empty_enemy_slot(formdata())
    IF slot > -1 THEN
     formdata(slot * 4) = .enemy.spawn.on_death
     loadfoe(slot, formdata(), bat, bslot())
    END IF
   NEXT i
   .enemy.spawn.on_death = 0
  END IF
 END WITH
END SUB

FUNCTION find_empty_enemy_slot(formdata() AS INTEGER) AS INTEGER
 'Returns index of first empty slot, or -1 if none was found
 DIM i AS INTEGER
 FOR i = 0 TO 7
  IF formdata(i * 4) = 0 THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION dieWOboss(BYVAL who, bslot() AS BattleSprite) as integer
 DIM AS INTEGER j
 '--count bosses
 FOR j = 4 TO 11
  '--is it a boss?
  IF bslot(j).is_boss = YES THEN
   '-- is it alive?
   IF bslot(j).stat.cur.hp > 0 THEN
    RETURN NO
   END IF
  END IF
 NEXT j
 '--if there are no bossess...
 '--should it die without a boss?
 IF bslot(who).die_without_boss = YES THEN
  bslot(who).stat.cur.hp = 0
  RETURN YES
 END IF
END FUNCTION

SUB triggerfade(BYVAL who, bslot() AS BattleSprite)
 'If the target is really dead...
 IF bslot(who).stat.cur.hp = 0 THEN
  IF is_hero(who) THEN
   '--for heroes, a .dissolve > 1 is used to trigger the animate code
   '  to force the hero to use their death frame
   bslot(who).dissolve = 1
  END IF
  IF is_enemy(who) THEN
   bslot(who).dissolve = bslot(who).deathtime
   '--flee as alternative to death
   IF bslot(who).flee_instead_of_die = YES THEN
    bslot(who).flee = 1
    'the number of ticks it takes an enemy to run away is based on its distance
    'from the left side of the screen and its width. Enemys flee at 10 pixels per tick
    bslot(who).dissolve = (bslot(who).w + bslot(who).x) / 10
   END IF
  END IF
 END IF
END SUB

SUB check_death(deadguy AS INTEGER, BYVAL killing_attack AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata())
'killing_attack contains attack id or -1 when no attack is relevant.

 IF is_enemy(deadguy) THEN
  IF formdata((deadguy - 4) * 4) = 0 THEN EXIT SUB
 END IF
 IF bslot(deadguy).stat.cur.hp <> 0 THEN EXIT SUB
 '--deadguy is really dead (includes already dead and empty hero slots??)
 'Death animation is not done yet here, so be cautious about what gets cleand up here.
 'Full cleanup of bslot() records belongs in loadfoe
 bslot(deadguy).vis = 0
 bslot(deadguy).ready = NO
 bslot(deadguy).attack = 0
 bslot(deadguy).d = 0
 '--reset poison/regen/stun/mute
 WITH bslot(deadguy).stat
  .cur.poison = .max.poison
  .cur.regen  = .max.regen
  .cur.stun   = .max.stun
  .cur.mute   = .max.mute
 END WITH
 '-- remove any attack queue entries
 FOR i AS INTEGER = 0 TO UBOUND(atkq)
  WITH atkq(i)
   IF .used ANDALSO .attacker = deadguy THEN
    clear_attack_queue_slot i
   END IF
  END WITH
 NEXT i
 '-- if it is a dead hero's turn, cancel menu
 IF bat.hero_turn = deadguy THEN
  bat.hero_turn = -1
  bat.menu_mode = batMENUHERO
  bat.targ.mode = targNONE
 END IF
 '-- if it is a dead enemy's turn, cancel ai
 IF bat.enemy_turn = deadguy THEN bat.enemy_turn = -1
 IF is_enemy(deadguy) THEN '------PLUNDER AND EXPERIENCE AND ITEMS------
  IF bslot(deadguy).death_sfx = 0 THEN
   IF gen(genDefaultDeathSFX) > 0 THEN
    playsfx gen(genDefaultDeathSFX) - 1
   END IF
  ELSEIF bslot(deadguy).death_sfx > 0 THEN
   playsfx bslot(deadguy).death_sfx - 1
  END IF
  dead_enemy deadguy, killing_attack, bat, bslot(), formdata()
 END IF'------------END PLUNDER-------------------
 battle_reevaluate_dead_targets deadguy, bat, bslot()
END SUB

SUB dead_enemy(deadguy AS INTEGER, killing_attack AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, formdata())
 '--give rewards, spawn enemies, clear formdata slot, but NO other cleanup!
 'killing_attack is the id of the attack that killed the target or -1 if the target died without a specific attack
 DIM AS INTEGER j
 DIM enemynum AS INTEGER = deadguy - 4
 '--spawn enemies before freeing the formdata slot to avoid infinite loops. however this might need to be changed to fix morphing enemies?
 spawn_on_death deadguy, killing_attack, bat, formdata(), bslot()
 IF formdata(enemynum * 4) > 0 THEN
  WITH bslot(deadguy)
   bat.rew.plunder += .enemy.reward.gold
   bat.rew.exper += .enemy.reward.exper
   IF bat.rew.exper > 1000000 THEN bat.rew.exper = 1000000 '--this one million limit is totally arbitrary
   IF INT(RND * 100) < .enemy.reward.item_rate THEN '---GET ITEMS FROM FOES-----
    FOR j = 0 TO 16
     IF bat.rew.found(j).num = 0 THEN bat.rew.found(j).id = .enemy.reward.item: bat.rew.found(j).num = 1: EXIT FOR
     IF bat.rew.found(j).id = .enemy.reward.item THEN bat.rew.found(j).num = bat.rew.found(j).num + 1: EXIT FOR
    NEXT j
   ELSE '------END NORMAL ITEM---------------
    IF INT(RND * 100) < .enemy.reward.rare_item_rate THEN
     FOR j = 0 TO 16
      IF bat.rew.found(j).num = 0 THEN bat.rew.found(j).id = .enemy.reward.rare_item: bat.rew.found(j).num = 1: EXIT FOR
      IF bat.rew.found(j).id = .enemy.reward.rare_item THEN bat.rew.found(j).num = bat.rew.found(j).num + 1: EXIT FOR
     NEXT j
    END IF '---END RARE ITEM-------------
   END IF '----END GET ITEMS----------------
  END WITH
 END IF
 ' remove dead enemy from formation
 formdata(enemynum * 4) = 0
END SUB

SUB enemy_ai (BYREF bat AS BattleState, bslot() AS BattleSprite, formdata() AS INTEGER)
 DIM ai AS INTEGER = 0

 'if HP is less than 20% go into desperation mode
 IF bslot(bat.enemy_turn).stat.cur.hp < bslot(bat.enemy_turn).stat.max.hp / 5 THEN ai = 1

 'if targetable enemy count is 1, go into alone mode
 IF targenemycount(bslot(), YES) = 1 THEN ai = 2

 DIM slot AS INTEGER = 0
 'spawn allies when alone
 WITH bslot(bat.enemy_turn)
  IF ai = 2 AND .enemy.spawn.when_alone > 0 THEN
   FOR j AS INTEGER = 1 TO .enemy.spawn.how_many
    slot = find_empty_enemy_slot(formdata())
    IF slot > -1 THEN
     formdata(slot * 4) = .enemy.spawn.when_alone
     loadfoe slot, formdata(), bat, bslot()
    END IF
   NEXT j
  END IF
 END WITH

 'make sure that the current ai set is valid
 'otherwise fall back on another
 IF countai(ai, bat.enemy_turn, bslot()) = 0 THEN
  ai = 0
  IF bslot(bat.enemy_turn).stat.cur.hp < bslot(bat.enemy_turn).stat.max.hp / 5 THEN
   ai = 1
   IF countai(ai, bat.enemy_turn, bslot()) = 0 THEN ai = 0
  END IF
 END IF

 'if no valid ai set is available, the enemy loses its turn
 IF countai(ai, bat.enemy_turn, bslot()) = 0 THEN bat.enemy_turn = -1 : EXIT SUB

 'pick a random attack
 DIM atk_id AS INTEGER
 DIM atk AS AttackData
 DIM safety AS INTEGER = 0
 DO
  WITH bslot(bat.enemy_turn)
   SELECT CASE ai
    CASE 0: atk_id = .enemy.regular_ai(INT(RND * 5))
    CASE 1: atk_id = .enemy.desperation_ai(INT(RND * 5))
    CASE 2: atk_id = .enemy.alone_ai(INT(RND * 5))
   END SELECT
  END WITH
  IF atk_id > 0 THEN
   'load the data for this attack
   loadattackdata atk, atk_id - 1
  
   IF atkallowed(atk, bat.enemy_turn, 0, 0, bslot()) THEN
    'this attack is good, continue on to target selection
    EXIT DO
   ELSE
    'this attack is unusable
    atk_id = 0
    IF bslot(bat.enemy_turn).stat.cur.mp - atk.mp_cost < 0 THEN
     'inadequate MP was the reason for the failure
     'MP-idiot loses its turn
     IF bslot(bat.enemy_turn).mp_idiot = YES THEN
       bslot(bat.enemy_turn).ready = NO
       bslot(bat.enemy_turn).ready_meter = 0
       bat.enemy_turn = -1
       EXIT SUB
     END IF
    END IF
    'currently, item requirements are disregarded. should they be? Maybe they should
    'come out of theft items?
   END IF
  END IF
  safety += 1
  IF safety > 99 THEN
   'give up eventually
   bat.enemy_turn = -1
   EXIT SUB
  END IF
 LOOP

 DIM t(11) AS INTEGER
 autotarget bat.enemy_turn, atk, bslot(), t()

 'ready for next attack
 bslot(bat.enemy_turn).ready = NO
 bslot(bat.enemy_turn).ready_meter = 0
 bat.enemy_turn = -1

END SUB

SUB heromenu (BYREF bat AS BattleState, bslot() AS BattleSprite, menubits() AS INTEGER, st() as herodef)

 DIM mp_name AS STRING = readglobalstring(1, "MP", 10)
     
 DIM atk AS AttackData
 DIM spellcount AS INTEGER = 0
 DIM i AS INTEGER
 
 FOR i = 0 TO 5
  setbit menubits(), 0, bat.hero_turn * 4 + i, 0
  IF bslot(bat.hero_turn).menu(i).atk >= 0 THEN
   loadattackdata atk, bslot(bat.hero_turn).menu(i).atk
   IF atk.check_costs_as_weapon THEN
    IF atkallowed(atk, bat.hero_turn, 0, 0, bslot()) = 0 THEN
     setbit menubits(), 0, bat.hero_turn * 4 + i, 1
    END IF
   END IF
  END IF
 NEXT i
 
 IF carray(ccMenu) > 1 THEN
  '--skip turn
  bat.next_hero = bat.hero_turn
  bat.hero_turn = -1
  EXIT SUB
 END IF
 IF carray(ccUp) > 1 THEN
  '--up
  bat.pt -= 1
  IF bat.pt < 0 THEN bat.pt = bslot(bat.hero_turn).menu_size
 END IF
 IF carray(ccDown) > 1 THEN
  bat.pt += 1
  IF bat.pt > bslot(bat.hero_turn).menu_size THEN bat.pt = 0
 END IF
 
 IF carray(ccUse) > 1 THEN
  '--use menu item
  IF bslot(bat.hero_turn).menu(bat.pt).atk >= 0 THEN 'simple attack
   IF readbit(menubits(), 0, bat.hero_turn * 4 + bat.pt) = 0 THEN
    bslot(bat.hero_turn).attack = bslot(bat.hero_turn).menu(bat.pt).atk + 1
    bat.targ.mode = targSETUP
    flusharray carray(), 7, 0
    EXIT SUB
   END IF
  END IF
  IF bslot(bat.hero_turn).menu(bat.pt).spell_list >= 0 THEN '--this is a spell list
   bat.listslot = bslot(bat.hero_turn).menu(bat.pt).spell_list
   IF st(bat.hero_turn).list_type(bat.listslot) < 2 THEN '--the type of this spell list is one that displays a menu
    '--init spell menu
    bat.menu_mode = batMENUSPELL
    bat.sptr = 0
    FOR i = 0 TO 23 '-- loop through the spell list setting up menu items for each
     WITH bat.spell.slot(i)
      .name = ""
      .desc = ""
      .cost = ""
      .atk_id = -1
     END WITH
     bat.spell.slot(i).enable = NO
     IF spell(bat.hero_turn, bat.listslot, i) > 0 THEN '--there is a spell in this slot
      spellcount += 1
      bat.spell.slot(i).atk_id = spell(bat.hero_turn, bat.listslot, i) - 1
      loadattackdata atk, bat.spell.slot(i).atk_id
      bat.spell.slot(i).name = atk.name
      bat.spell.slot(i).desc = atk.description
      IF st(bat.hero_turn).list_type(bat.listslot) = 0 THEN
       '--regular MP
       bat.spell.slot(i).cost = " " & focuscost(atk.mp_cost, bslot(bat.hero_turn).stat.cur.foc) & " " & mp_name & " " & bslot(bat.hero_turn).stat.cur.mp & "/" & bslot(bat.hero_turn).stat.max.mp
      END IF
      IF st(bat.hero_turn).list_type(bat.listslot) = 1 THEN
       '--level MP
       bat.spell.slot(i).cost = bat.level_mp_caption & " " & (INT(i / 3) + 1) & ":  " & lmp(bat.hero_turn, INT(i / 3))
      END IF
      IF atkallowed(atk, bat.hero_turn, st(bat.hero_turn).list_type(bat.listslot), INT(i / 3), bslot()) THEN
       '-- check whether or not the spell is allowed
       bat.spell.slot(i).enable = YES
      END IF
     END IF
     bat.spell.slot(i).name = rpad(bat.spell.slot(i).name, " ", 10) '-- pad the spell menu caption
    NEXT i
   ELSEIF st(bat.hero_turn).list_type(bat.listslot) = 2 THEN '-- this is a random spell list
    spellcount = 0
    FOR i = 0 TO 23 '-- loop through the spell list storing attack ID numbers
     bat.spell.slot(i).atk_id = -1
     IF spell(bat.hero_turn, bat.listslot, i) > 0 THEN '--there is a spell in this slot
      spellcount += 1
      bat.spell.slot(i).atk_id = spell(bat.hero_turn, bat.listslot, i) - 1
     END IF
    NEXT i
    IF spellcount > 0 THEN '-- don't attempt to pick randomly from an empty list
     DIM safety AS INTEGER
     DIM rptr AS INTEGER
     rptr = INT(RND * 24)
     FOR i = 0 TO INT(RND * spellcount)
      safety = 0
      DO
       rptr = loopvar(rptr, 0, 23, 1)
       safety += 1
      LOOP UNTIL bat.spell.slot(rptr).atk_id > -1 OR safety > 999 '--loop until we have found a spell (or give up after 999 tries)
     NEXT i
     bslot(bat.hero_turn).attack = bat.spell.slot(rptr).atk_id + 1
     bat.targ.mode = targSETUP
     flusharray carray(), 7, 0
    END IF
   END IF
  ELSEIF bslot(bat.hero_turn).menu(bat.pt).spell_list = -10 THEN '--items menu
   bat.menu_mode = batMENUITEM
   bat.item.pt = 0
   bat.item.top = 0
   checkitemusability bat.iuse(), bslot(), bat.hero_turn
   bat.item_desc = ""
   IF inventory(bat.item.pt).used THEN
    loaditemdata buffer(), inventory(bat.item.pt).id
    bat.item_desc = readbadbinstring$(buffer(), 9, 35, 0)
   END IF
  END IF
 END IF
END SUB

SUB spellmenu (BYREF bat AS BattleState, st() as HeroDef, bslot() AS BattleSprite)
 IF carray(ccMenu) > 1 THEN '--cancel
  bat.menu_mode = batMENUHERO
  flusharray carray(), 7, 0
  EXIT SUB
 END IF
 
 WITH bat
  IF carray(ccUp) > 1 THEN
   IF .sptr > 2 THEN .sptr -= 3 ELSE .sptr = 24
  END IF
  IF carray(ccDown) > 1 THEN
   IF .sptr < 24 THEN .sptr = small(.sptr + 3, 24) ELSE .sptr = 0
  END IF
  IF carray(ccLeft) > 1 AND .sptr < 24 AND .sptr > 0 THEN
   .sptr -= 1
  END IF
  IF carray(ccRight) > 1 AND .sptr < 24 THEN
   .sptr += 1
  END IF
 END WITH
 
 IF carray(ccUse) > 1 THEN
  '--use selected spell
  IF bat.sptr = 24 THEN
   '--used cancel
   bat.menu_mode = batMENUHERO
   flusharray carray(), 7, 0
   EXIT SUB
  END IF

  DIM atk AS AttackData
  '--can-I-use-it? checking
  IF bat.spell.slot(bat.sptr).atk_id > -1 THEN
   '--list-entry is non-empty
   loadattackdata atk, bat.spell.slot(bat.sptr).atk_id
   IF atkallowed(atk, bat.hero_turn, st(bat.hero_turn).list_type(bat.listslot), INT(bat.sptr / 3), bslot()) THEN
    '--attack is allowed
    '--if lmp then set lmp consume flag
    IF st(bat.hero_turn).list_type(bat.listslot) = 1 THEN bslot(bat.hero_turn).consume_lmp = INT(bat.sptr / 3) + 1
    '--queue attack
    bslot(bat.hero_turn).attack = bat.spell.slot(bat.sptr).atk_id + 1
    '--exit spell menu
    bat.targ.mode = targSETUP
    bat.menu_mode = batMENUHERO
    flusharray carray(), 7, 0
   END IF
  END IF
 END IF
END SUB

SUB itemmenu (BYREF bat AS BattleState, bslot() AS BattleSprite)
 IF carray(ccMenu) > 1 THEN
  bat.menu_mode = batMENUHERO
  flusharray carray(), 7, 0
  bslot(bat.hero_turn).consume_item = -1 ' -1 here indicates that this hero will not consume any item
 END IF

 DIM remember_pt AS INTEGER = bat.item.pt
 IF carray(ccUp) > 1 AND bat.item.pt > 2 THEN bat.item.pt = bat.item.pt - 3
 IF carray(ccDown) > 1 AND bat.item.pt <= last_inv_slot() - 3 THEN bat.item.pt = bat.item.pt + 3
 IF keyval(scPageUp) > 1 THEN
  bat.item.pt -= (bat.inv_scroll.size+1) * 3
  WHILE bat.item.pt < 0: bat.item.pt += 3: WEND
 END IF
 IF keyval(scPageDown) > 1 THEN
  bat.item.pt += (bat.inv_scroll.size+1) * 3
  WHILE bat.item.pt > last_inv_slot(): bat.item.pt -= 3: WEND
 END IF
 IF carray(ccLeft) > 1 AND bat.item.pt > 0 THEN
  bat.item.pt = bat.item.pt - 1
 END IF
 IF carray(ccRight) > 1 AND bat.item.pt < last_inv_slot() THEN
  bat.item.pt = bat.item.pt + 1
 END IF
 '--scroll when past top or bottom
 WHILE bat.item.pt < bat.item.top : bat.item.top = bat.item.top - 3 : WEND
 WHILE bat.item.pt >= bat.item.top + (bat.inv_scroll.size+1) * 3 : bat.item.top = bat.item.top + 3 : WEND

 DIM itembuf(99) AS INTEGER
 
 IF remember_pt <> bat.item.pt THEN
  IF inventory(bat.item.pt).used THEN
   loaditemdata itembuf(), inventory(bat.item.pt).id
   bat.item_desc = readbadbinstring$(itembuf(), 9, 35, 0)
  ELSE
   bat.item_desc = ""
  END IF
 END IF

 IF carray(ccUse) > 1 THEN
  IF readbit(bat.iuse(), 0, bat.item.pt) = 1 THEN
   loaditemdata itembuf(), inventory(bat.item.pt).id
   bslot(bat.hero_turn).consume_item = -1
   IF itembuf(73) = 1 THEN bslot(bat.hero_turn).consume_item = bat.item.pt
   
   bslot(bat.hero_turn).attack = itembuf(47)
   bat.targ.mode = targSETUP
   bat.menu_mode = batMENUHERO
   flusharray carray(), 7, 0
  END IF
 END IF
END SUB

SUB generate_atkscript(BYREF attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite, t() AS INTEGER)
 DIM i AS INTEGER

 '--check for item consumption
 IF bslot(bat.acting).consume_item >= 0 THEN
  IF inventory(bslot(bat.acting).consume_item).used = 0 THEN
   '--abort if item is gone
   bat.atk.id = -1
   EXIT SUB
  END IF
 END IF

 '--load attack
 loadattackdata attack, bat.atk.id

 DIM safety AS INTEGER = 0
 DO WHILE spawn_chained_attack(attack.instead, attack, bat, bslot())
  IF blocked_by_attack(bat.acting) THEN EXIT SUB
  loadattackdata attack, bat.atk.id
  safety += 1
  IF safety > 100 THEN
   debuginfo "Endless instead-chain loop detected for " & attack.name
   bat.atk.id = -1 '--cancel attack
   EXIT SUB
  END IF
 LOOP

 IF attack.recheck_costs_after_delay THEN
  'The "Re-check costs after attack delay" is on, so cancel the attack if we can't afford it now
  IF atkallowed(attack, bat.acting, 0, 0, bslot()) = NO THEN
   bat.atk.id = -1
   EXIT SUB
  END IF
 END IF

 '--setup attack sprite slots
 FOR i = 12 TO 23
  bslot(i).frame = 0
  bslot(i).z = 0
  'load battle sprites
  with bslot(i)
   .sprite_num = 3
   .frame = 0
   frame_unload(@.sprites)
   palette16_unload(@.pal)
   .sprites = frame_load(6, attack.picture)
   .pal = palette16_load(attack.pal, 6, attack.picture)
  end with
 NEXT i
 
 DIM tcount AS INTEGER = -1
 DIM pdir AS INTEGER = 0
 
 bat.atk.has_consumed_costs = NO
 IF is_enemy(bat.acting) THEN pdir = 1
 
 'CANNOT HIT INVISIBLE FOES
 FOR i = 0 TO 11
  IF t(i) > -1 THEN
   IF bslot(t(i)).vis = 0 AND attack_can_hit_dead(t(i), attack, bslot(bat.acting).stored_targs_can_be_dead) = NO THEN
    t(i) = -1
   END IF
  END IF
 NEXT i
 'MOVE EMPTY TARGET SLOTS TO THE BACK
 FOR o AS INTEGER = 0 TO UBOUND(t) - 1
  FOR i = 0 TO 10
   IF t(i) = -1 THEN SWAP t(i), t(i + 1)
  NEXT i
 NEXT o
 'COUNT TARGETS
 FOR i = 0 TO 11
  IF t(i) > -1 THEN tcount += 1
 NEXT i
 
 bat.atk.non_elemental = YES
 FOR i = 0 TO 7
  bat.atk.elemental(i) = NO
  IF attack.elemental_damage(i) = YES THEN
   bat.atk.elemental(i) = YES
   bat.atk.non_elemental = NO
  END IF
 NEXT i
 
 'ABORT IF TARGETLESS
 IF tcount = -1 THEN
  bat.atk.id = -1
  EXIT SUB
 END IF
 
 'Kill old target history
 FOR i = 0 TO 11
  bslot(bat.acting).last_targs(i) = NO
 NEXT i
 
 'BIG CRAZY SCRIPT CONSTRUCTION
 'DEBUG debug "begin script construction"
 IF is_hero(bat.acting) THEN
  'load weapon sprites
  with bslot(24)
   .sprite_num = 2
   frame_unload @.sprites
   .sprites = frame_load(5, gam.hero(bat.acting).wep_pic)
   palette16_unload @.pal
   .pal = palette16_load(gam.hero(bat.acting).wep_pal, 5, gam.hero(bat.acting).wep_pic)
   .frame = 0
  end with
 END IF
 
 DIM numhits AS INTEGER
 numhits = attack.hits + INT(RND * (bslot(bat.acting).stat.cur.hits + 1))
 IF attack.ignore_extra_hits THEN numhits = attack.hits

 DIM atkimgdirection AS INTEGER
 atkimgdirection = pdir
 IF attack.unreversable_picture = YES THEN atkimgdirection = 0
 
 DIM AS INTEGER xt, yt

 '----NULL ANIMATION
 IF attack.attack_anim = 10 THEN
  anim_advance bat.acting, attack, bslot(), t()
  if attack.sound_effect > 0  then anim_sound(attack.sound_effect - 1)
  FOR j AS INTEGER = 1 TO numhits
   IF is_hero(bat.acting) THEN anim_hero bat.acting, attack, bslot(), t()
   IF is_enemy(bat.acting) THEN anim_enemy bat.acting, attack, bslot(), t()
   FOR i = 0 TO tcount
    anim_inflict t(i), tcount
   NEXT i
   anim_disappear 24
  NEXT j
  anim_retreat bat.acting, attack, bslot()
  anim_end
 END IF

 '----NORMAL, DROP, SPREAD-RING, and SCATTER
 IF attack.attack_anim = 0 OR attack.attack_anim = 3 OR attack.attack_anim = 6 OR (attack.attack_anim = 4 AND tcount > 0) THEN
 FOR i = 0 TO tcount
  yt = (bslot(t(i)).h - 50) + 2
  xt = 0: IF t(i) = bat.acting AND is_hero(bat.acting) AND attack.attacker_anim <> 7 THEN xt = -20
  anim_setpos 12 + i, bslot(t(i)).x + xt, bslot(t(i)).y + yt, atkimgdirection
  IF attack.attack_anim = 3 THEN
   anim_setz 12 + i, 180
  END IF
  IF attack.attack_anim = 4 THEN
   anim_setpos 12 + i, bslot(t(i)).x + xt, bslot(t(i)).y + yt - bslot(t(i)).w, atkimgdirection
  END IF
 NEXT i
 anim_advance bat.acting, attack, bslot(), t()
 FOR j AS INTEGER = 1 TO numhits
  IF is_hero(bat.acting) THEN anim_hero bat.acting, attack, bslot(), t()
  IF is_enemy(bat.acting) THEN anim_enemy bat.acting, attack, bslot(), t()
   FOR i = 0 TO tcount
    anim_appear 12 + i
    IF attack.attack_anim = 4 THEN
     anim_absmove 12 + i, bslot(t(i)).x + xt - bslot(t(i)).w, bslot(t(i)).y + yt, 3, 3
    END IF
    IF attack.attack_anim = 3 THEN
     anim_zmove 12 + i, -10, 20
    END IF
    IF attack.attack_anim = 6 THEN
     anim_absmove 12 + i, INT(RND * 270), INT(RND * 150), 6, 6
    END IF
   NEXT i
   if attack.sound_effect > 0  then anim_sound(attack.sound_effect - 1)
   anim_wait 2
   IF attack.attack_anim = 3 THEN
    anim_wait 3
   END IF
   anim_setframe bat.acting, 0
   anim_disappear 24
   IF attack.attack_anim = 4 THEN
    FOR i = 0 TO tcount
     anim_absmove 12 + i, bslot(t(i)).x + xt, bslot(t(i)).y + yt + bslot(t(i)).w, 3, 3
    NEXT i
    anim_waitforall
    FOR i = 0 TO tcount
     anim_absmove 12 + i, bslot(t(i)).x + xt + bslot(t(i)).w, bslot(t(i)).y + yt, 3, 3
    NEXT i
    anim_waitforall
    FOR i = 0 TO tcount
     anim_absmove 12 + i, bslot(t(i)).x + xt, bslot(t(i)).y + yt - bslot(t(i)).w, 3, 3
    NEXT i
    anim_waitforall
   END IF
   FOR i = 0 TO tcount
    anim_inflict t(i), tcount
    anim_flinchstart t(i), bslot(), attack
   NEXT i
   IF attack.attack_anim <> 4 THEN
    anim_wait 3
   END IF
   FOR i = 0 TO tcount
    anim_disappear 12 + i
    anim_flinchdone t(i), bslot(), attack
   NEXT i
   anim_wait 2
  NEXT j
  anim_retreat bat.acting, attack, bslot()
  FOR i = 0 TO tcount
   anim_setframe t(i), 0
  NEXT i
  anim_end
 END IF

 '----SEQUENTIAL PROJECTILE
 IF attack.attack_anim = 7 THEN
  DIM startoffset AS INTEGER
  'attacker steps forward
  anim_advance bat.acting, attack, bslot(), t()
  'repeat the following for each attack
  FOR j AS INTEGER = 1 TO numhits
   'attacker animates
   IF is_hero(bat.acting) THEN anim_hero bat.acting, attack, bslot(), t()
   IF is_enemy(bat.acting) THEN anim_enemy bat.acting, attack, bslot(), t()
   'calculate where the projectile will start relative to the attacker
   startoffset = 50
   IF is_hero(bat.acting) THEN startoffset = -50
   'set the projectile position
   anim_setpos 12, bslot(bat.acting).x + startoffset, bslot(bat.acting).y, atkimgdirection
   anim_appear 12
   'play the sound effect
   IF attack.sound_effect > 0 THEN anim_sound(attack.sound_effect - 1)
   'repeat the following for each target...
   FOR i = 0 TO tcount
    'find the target's position
    yt = (bslot(t(i)).h - 50) + 2
    xt = 0
    IF t(i) = bat.acting AND is_hero(bat.acting) AND attack.attacker_anim <> 7 THEN xt = -20
    'make the projectile move to the target
    anim_absmove 12, bslot(t(i)).x + xt, bslot(t(i)).y + yt, 5, 5
    anim_waitforall
    'inflict damage
    anim_inflict t(i), tcount
    anim_flinchstart t(i), bslot(), attack
    anim_wait 3
    anim_flinchdone t(i), bslot(), attack
    IF i = 0 THEN
     'attacker's weapon picture vanishes after the first hit
     anim_disappear 24
    END IF
   NEXT i
   'after all hits are done, projectile flies off the side of the screen
   IF is_hero(bat.acting) THEN
    anim_absmove 12, -50, 100, 5, 5
   END IF
   IF is_enemy(bat.acting) THEN
    anim_absmove 12, 320, 100, 5, 5
   END IF
   anim_waitforall
   'hide projectile
   anim_disappear 12
  NEXT j
  'attacker steps back
  anim_retreat bat.acting, attack, bslot()
  anim_end
 END IF
 
 '----PROJECTILE, REVERSE PROJECTILE and METEOR
 IF attack.attack_anim = 1 OR attack.attack_anim = 2 OR attack.attack_anim = 8 THEN
  DIM projectile_start_x_offset AS INTEGER
  projectile_start_x_offset = 50
  IF is_hero(bat.acting) THEN projectile_start_x_offset = -50
  anim_advance bat.acting, attack, bslot(), t()
  FOR j AS INTEGER = 1 TO numhits
   FOR i = 0 TO tcount
    yt = (bslot(t(i)).h - 50) + 2
    xt = 0
    IF t(i) = bat.acting AND is_hero(bat.acting) AND attack.attacker_anim <> 7 THEN xt = -20
    IF attack.attack_anim = 1 THEN
     anim_setpos 12 + i, bslot(bat.acting).x + projectile_start_x_offset, bslot(bat.acting).y, atkimgdirection
    END IF
    IF attack.attack_anim = 2 THEN
     anim_setpos 12 + i, bslot(t(i)).x + xt, bslot(t(i)).y + yt, atkimgdirection
    END IF
    IF attack.attack_anim = 8 THEN
     IF is_hero(bat.acting) THEN
      anim_setpos 12 + i, 320, 100, atkimgdirection
     END IF
     IF is_enemy(bat.acting) THEN
      anim_setpos 12 + i, -50, 100, atkimgdirection
     END IF
     anim_setz 12 + i, 180
    END IF
   NEXT i
   IF is_hero(bat.acting) THEN anim_hero bat.acting, attack, bslot(), t()
   IF is_enemy(bat.acting) THEN anim_enemy bat.acting, attack, bslot(), t()
   FOR i = 0 TO tcount
    anim_appear 12 + i
    yt = (bslot(t(i)).h - 50) + 2
    xt = 0
    IF t(i) = bat.acting AND is_hero(bat.acting) AND attack.attacker_anim <> 7 THEN xt = -20
    IF attack.attack_anim = 1 OR attack.attack_anim = 8 THEN
     anim_absmove 12 + i, bslot(t(i)).x + xt, bslot(t(i)).y + yt, 6, 6
    END IF
    IF attack.attack_anim = 2 THEN
     anim_absmove 12 + i, bslot(bat.acting).x + projectile_start_x_offset, bslot(bat.acting).y, 6, 6
    END IF
    IF attack.attack_anim = 8 THEN
     anim_zmove 12 + i, -6, 30
    END IF
   NEXT i
   if attack.sound_effect > 0  then anim_sound(attack.sound_effect - 1)
   anim_wait 8
   anim_disappear 24
   anim_setframe bat.acting, 0
   FOR i = 0 TO tcount
    anim_inflict t(i), tcount
    anim_flinchstart t(i), bslot(), attack
   NEXT i
   anim_wait 3
   FOR i = 0 TO tcount
    anim_disappear 12 + i
    anim_flinchdone t(i), bslot(), attack
   NEXT i
   anim_wait 3
  NEXT j
  anim_retreat bat.acting, attack, bslot()
  FOR i = 0 TO tcount
   anim_setframe t(i), 0
  NEXT i
  anim_end
 END IF

 '----DRIVEBY
 IF attack.attack_anim = 9 THEN
  anim_advance bat.acting, attack, bslot(), t()
  FOR j AS INTEGER = 1 TO numhits
   FOR i = 0 TO tcount
    yt = (bslot(t(i)).h - 50) + 2
    IF is_hero(bat.acting) THEN
     anim_setpos 12 + i, 320, bslot(t(i)).y + yt, atkimgdirection
    END IF
    IF is_enemy(bat.acting) THEN
     anim_setpos 12 + i, -50, bslot(t(i)).y + yt, atkimgdirection
    END IF
   NEXT i
   IF is_hero(bat.acting) THEN anim_hero bat.acting, attack, bslot(), t()
   IF is_enemy(bat.acting) THEN anim_enemy bat.acting, attack, bslot(), t()
   FOR i = 0 TO tcount
    anim_appear 12 + i
    yt = (bslot(t(i)).h - 50) + 2
    anim_absmove 12 + i, bslot(t(i)).x + xt, bslot(t(i)).y + yt, 8, 8
   NEXT i
   if attack.sound_effect > 0  then anim_sound(attack.sound_effect - 1)
   anim_wait 4
   anim_disappear 24
   anim_setframe bat.acting, 0
   anim_waitforall
   FOR i = 0 TO tcount
    anim_inflict t(i), tcount
    anim_flinchstart t(i), bslot(), attack
    yt = (bslot(t(i)).h - 50) + 2
    IF is_hero(bat.acting) THEN
     anim_absmove 12 + i, -50, bslot(t(i)).y + yt, 5, 7
    END IF
    IF is_enemy(bat.acting) THEN
     anim_absmove 12 + i, 320, bslot(t(i)).y + yt, 5, 7
    END IF
   NEXT i
   anim_waitforall
   FOR i = 0 TO tcount
    anim_disappear 12 + i
    anim_flinchdone t(i), bslot(), attack
   NEXT i
   anim_wait 3
  NEXT j
  anim_retreat bat.acting, attack, bslot()
  FOR i = 0 TO tcount
   anim_setframe t(i), 0
  NEXT i
  anim_end
 END IF
 
 '----FOCUSED RING
 IF attack.attack_anim = 4 AND tcount = 0 THEN
  anim_advance bat.acting, attack, bslot(), t()
  FOR j AS INTEGER = 1 TO numhits
   i = 0
   yt = (bslot(t(i)).h - 50) + 2
   xt = 0
   IF t(i) = bat.acting AND is_hero(bat.acting) AND attack.attacker_anim <> 7 THEN xt = -20
   DIM ringpos AS XYPair
   ringpos.x = bslot(t(i)).x + xt
   ringpos.y = bslot(t(i)).y + yt
   anim_setpos 12 + 0, ringpos.x + 0, ringpos.y - 50, atkimgdirection
   anim_setpos 12 + 1, ringpos.x + 30, ringpos.y - 30, atkimgdirection
   anim_setpos 12 + 2, ringpos.x + 50, ringpos.y + 0, atkimgdirection
   anim_setpos 12 + 3, ringpos.x + 30, ringpos.y + 30, atkimgdirection
   anim_setpos 12 + 4, ringpos.x - 0, ringpos.y + 50, atkimgdirection
   anim_setpos 12 + 5, ringpos.x - 30, ringpos.y + 30, atkimgdirection
   anim_setpos 12 + 6, ringpos.x - 50, ringpos.y - 0, atkimgdirection
   anim_setpos 12 + 7, ringpos.x - 30, ringpos.y - 30, atkimgdirection
   IF is_hero(bat.acting) THEN anim_hero bat.acting, attack, bslot(), t()
   IF is_enemy(bat.acting) THEN anim_enemy bat.acting, attack, bslot(), t()
   yt = (bslot(t(0)).h - 50) + 2
   xt = 0
   IF t(i) = bat.acting AND is_hero(bat.acting) AND attack.attacker_anim <> 7 THEN xt = -20
   FOR i = 0 TO 7
    anim_appear 12 + i
    anim_absmove 12 + i, bslot(t(0)).x + xt, bslot(t(0)).y + yt, 4, 4
   NEXT i
   if attack.sound_effect > 0  then anim_sound(attack.sound_effect - 1)
   anim_wait 8
   anim_disappear 24
   anim_setframe bat.acting, 0
   FOR i = 0 TO tcount
    anim_inflict t(i), tcount
    anim_flinchstart t(i), bslot(), attack
   NEXT i
   anim_wait 3
   FOR i = 0 TO 7
    anim_disappear 12 + i
   NEXT i
   FOR i = 0 TO tcount
    anim_flinchdone t(i), bslot(), attack
   NEXT i
   anim_wait 3
  NEXT j
  anim_retreat bat.acting, attack, bslot()
  FOR i = 0 TO tcount
   anim_setframe t(i), 0
  NEXT i
  anim_end
 END IF
 
 '----WAVE
 IF attack.attack_anim = 5 THEN
  DIM wave_start_x AS INTEGER
  wave_start_x = -50
  IF is_hero(bat.acting) THEN wave_start_x = 320
  DIM pushback_x AS INTEGER
  pushback_x = 24
  IF is_hero(bat.acting) THEN pushback_x = -24
  yt = bslot(t(0)).y + (bslot(t(0)).h - 50) + 2
  anim_advance bat.acting, attack, bslot(), t()
  FOR j AS INTEGER = 1 TO numhits
   FOR i = 0 TO 11
    IF tcount > 0 OR attack.targ_set = 1 THEN
     anim_setpos 12 + i, wave_start_x, i * 15, atkimgdirection
    ELSE
     anim_setpos 12 + i, wave_start_x, yt, atkimgdirection
    END IF
   NEXT i
   IF is_hero(bat.acting) THEN anim_hero bat.acting, attack, bslot(), t()
   IF is_enemy(bat.acting) THEN anim_enemy bat.acting, attack, bslot(), t()
   if attack.sound_effect > 0  then anim_sound(attack.sound_effect - 1)
   FOR i = 0 TO 11
    anim_appear 12 + i
    anim_setmove 12 + i, pushback_x, 0, 16, 0
    anim_wait 1
   NEXT i
   anim_wait 15
   anim_disappear 24
   anim_setframe bat.acting, 0
   FOR i = 0 TO tcount
    anim_inflict t(i), tcount
    anim_flinchstart t(i), bslot(), attack
   NEXT i
   anim_waitforall
   FOR i = 0 TO 11
    anim_disappear 12 + i
   NEXT i
   FOR i = 0 TO tcount
    anim_flinchdone t(i), bslot(), attack
   NEXT i
   anim_wait 2
  NEXT j
  anim_retreat bat.acting, attack, bslot()
  FOR i = 0 TO tcount
   anim_setframe t(i), 0
  NEXT i
  anim_end
 END IF
 
 '--setup animation pattern
 FOR i = 12 TO 23 '--for each attack sprite
  bslot(i).anim_index = 0
  bslot(i).anim_pattern = attack.anim_pattern
 NEXT i
 
 '--if caption has length and is set to display
 IF attack.caption <> "" AND attack.caption_time >= 0 THEN
  '--load caption
  setbatcap bat, attack.caption, 0, attack.caption_delay
  SELECT CASE attack.caption_time
   CASE 0
    '--full duration
    bat.caption_time = 16383 + attack.caption_delay
   CASE IS > 0
    '--timed
    bat.caption_time = attack.caption_time + attack.caption_delay
  END SELECT
 END IF
 
 'DEBUG debug "stackpos = " & (stackpos - bstackstart) \ 2
 invertstack
 
 '--Remember the attack ID for later call to fulldeathcheck
 bat.atk.was_id = bat.atk.id
 
 '--indicates that animation is set and that we should proceed to "action"
 bat.anim_ready = YES
END SUB

SUB enforce_weak_picture(who AS INTEGER, bslot() AS BattleSprite, bat AS BattleState)
 '--Heroes only, since enemies don't currently have a weak frame
 IF is_hero(who) THEN
  '--enforce weak picture
  IF bslot(who).stat.cur.hp < bslot(who).stat.max.hp / 5 AND bat.vic.state = 0 THEN bslot(who).frame = 6
 END IF
END SUB

SUB setup_targetting (BYREF bat AS BattleState, bslot() AS BattleSprite)
 'setuptarg (heroes only)
 DIM i AS INTEGER

 'init
 bat.targ.opt_spread = 0
 bat.targ.interactive = NO
 bat.targ.roulette = NO
 bat.targ.force_first = NO
 bat.targ.pointer = 0
 FOR i = 0 TO 11
  bat.targ.selected(i) = 0 ' clear list of selected targets
 NEXT i

 bat.targ.hit_dead = NO

 DIM attack AS AttackData
 'load attack
 loadattackdata attack, bslot(bat.hero_turn).attack - 1

 get_valid_targs bat.targ.mask(), bat.hero_turn, attack, bslot()
 bat.targ.hit_dead = attack_can_hit_dead(bat.hero_turn, attack, bslot(bat.hero_turn).stored_targs_can_be_dead)

 '--attacks that can target all should default to the first enemy
 IF attack.targ_class = 3 THEN
  bat.targ.pointer = 4
 END IF

 'fail if there are no targets
 IF targetmaskcount(bat.targ.mask()) = 0 THEN
  bat.targ.mode = targNONE
  EXIT SUB
 END IF

 'autoattack
 IF attack.automatic_targ THEN
  bat.targ.mode = targAUTO
  EXIT SUB
 END IF

 IF attack.targ_set = 0 THEN bat.targ.interactive = YES
 IF attack.targ_set = 1 THEN FOR i = 0 TO 11: bat.targ.selected(i) = bat.targ.mask(i): NEXT i
 IF attack.targ_set = 2 THEN bat.targ.interactive = YES: bat.targ.opt_spread = 1
 IF attack.targ_set = 3 THEN bat.targ.roulette = YES
 IF attack.targ_set = 4 THEN bat.targ.force_first = YES

 bat.targ.pointer = find_preferred_target(bat.targ.mask(), bat.hero_turn, attack, bslot())
 'fail if no targets are found
 IF bat.targ.pointer = -1 THEN
  bat.targ.mode = targNONE
  EXIT SUB
 END IF

 'ready to choose bat.targ.selected() from bat.targ.mask()
 bat.targ.mode = targMANUAL
END SUB

FUNCTION valid_statnum(statnum AS INTEGER, context AS STRING) AS INTEGER
 RETURN bound_arg(statnum, 0, 15, "stat number", context, NO)
END FUNCTION

FUNCTION check_attack_chain(ch AS AttackDataChain, bat AS BattleState, bslot() AS BattleSprite) AS INTEGER
 'Returns YES if the chain may proceed, or NO if it fails
 
 IF INT(RND * 100) >= ch.rate THEN RETURN NO '--random percentage failed
 
 IF ch.must_know = YES THEN
  IF knows_attack(bat.acting, ch.atk_id - 1, bslot()) = NO THEN RETURN NO
 END IF
 
 SELECT CASE ch.mode
  CASE 0 '--no special conditions
   RETURN YES
  CASE 1 '--tag checks
   IF ABS(ch.val1) <= UBOUND(tag) AND ABS(ch.val2) <= UBOUND(tag) THEN
    RETURN istag(ch.val1, YES) AND istag(ch.val2, YES)
   ELSE
    debug "chain: invalid tag check " & ch.val1 & " " & ch.val2
   END IF
  CASE 2 '--attacker stat greater than value
   IF valid_statnum(ch.val1, "check_attack_chain() [>n]") THEN
    RETURN bslot(bat.acting).stat.cur.sta(ch.val1) > ch.val2
   END IF
  CASE 3 '--attacker stat less than value
   IF valid_statnum(ch.val1, "check_attack_chain() [<n]") THEN
    RETURN bslot(bat.acting).stat.cur.sta(ch.val1) < ch.val2
   END IF
  CASE 4 '--attacker stat greater than value % of max
   IF valid_statnum(ch.val1, "check_attack_chain() [>n%]") THEN
    WITH bslot(bat.acting).stat
     RETURN .cur.sta(ch.val1) > .max.sta(ch.val1) * (ch.val2 / 100)
    END WITH
   END IF
  CASE 5 '--attacker stat less than value % of max
   IF valid_statnum(ch.val1, "check_attack_chain() [<n%]") THEN
    WITH bslot(bat.acting).stat
     RETURN .cur.sta(ch.val1) < .max.sta(ch.val1) * (ch.val2 / 100)
    END WITH
   END IF
  CASE ELSE
   debug "attack chain mode " & ch.mode & " unsupported"
 END SELECT
 RETURN NO
END FUNCTION

FUNCTION spawn_chained_attack(ch AS AttackDataChain, attack AS AttackData, BYREF bat AS BattleState, bslot() AS BattleSprite) AS INTEGER
 IF ch.atk_id <= 0 THEN RETURN NO '--no chain defined
 IF bslot(bat.acting).stat.cur.hp <= 0 THEN RETURN NO '--attacker is dead
 IF attack.no_chain_on_failure = YES AND bslot(bat.acting).attack_succeeded = 0 THEN
  'attack failed, and this chain configured to fail too
  RETURN NO
 END IF
  
 IF check_attack_chain(ch, bat, bslot()) THEN
  '--The conditions for this chain are passed
  
  bat.wait_frames = 0
  bat.anim_ready = NO
  
  DIM chained_attack AS AttackData
  loadattackdata chained_attack, ch.atk_id - 1

  DIM delayed_attack_id AS INTEGER = 0
  IF chained_attack.attack_delay > 0 AND ch.no_delay = NO THEN
   '--chain is delayed, queue the attack
   bat.atk.id = -1 '--terminate the attack that lead to this chain
   delayed_attack_id = ch.atk_id
  ELSE
   '--chain is immediate, prep it now!
   bat.atk.id = ch.atk_id - 1
   bat.anim_ready = NO
  END IF
  
  DIM blocking AS INTEGER
  IF bat.anim_blocking_delay = NO THEN
   '--chains from non-blocking attacks are always non-blocking
   blocking = NO
  ELSE
   blocking = NOT chained_attack.nonblocking
  END IF
  
  IF chained_attack.targ_set <> attack.targ_set OR _
     chained_attack.targ_class <> attack.targ_class OR _
     chained_attack.targ_set = 3 OR chained_attack.prefer_targ > 0 THEN
   'if the chained attack has a different target class/type then re-target
   'also retarget if the chained attack has target setting "random roulette"
   'also retarget if the chained attack's preferred target is explicitly set
   DIM t(11) AS INTEGER
   autotarget bat.acting, chained_attack, bslot(), t(), ,blocking
   bat.atk.id = -1
  ELSEIF delayed_attack_id > 0 THEN
   'if the old target info is reused, and this is not an immediate chain, copy it to the queue right away
   queue_attack delayed_attack_id - 1, bat.acting, bat.anim_t(), blocking
  END IF

  RETURN YES '--chained attack okay
 END IF

 RETURN NO '--chained attack failed
END FUNCTION '.attack

FUNCTION knows_attack(BYVAL who AS INTEGER, BYVAL atk AS INTEGER, bslot() AS BattleSprite) AS INTEGER
 'who is bslot index
 'atk is attack id
 'bslot() hero and enemy data
 'spell() is a global array

 '--different handling for heroes and monsters
 
 IF is_hero(who) THEN
  FOR i AS INTEGER = 0 TO 5
   WITH bslot(who).menu(i)
    IF .atk = atk THEN RETURN YES 'Knows the attack by an equipped weapon
    IF .spell_list >= 0 THEN
     FOR j AS INTEGER = 0 TO 23
      IF spell(who, .spell_list, j) = atk THEN RETURN YES 'Knows the attack in a spell list
     NEXT j
    END IF
   END WITH
  NEXT i
 END IF
 
 IF is_enemy(who) THEN
  FOR i AS INTEGER = 0 TO 4
   'check if enemy knows this attack for one of the three ai sets
   IF bslot(who).enemy.regular_ai(i) - 1 = atk THEN RETURN YES
   IF bslot(who).enemy.desperation_ai(i) - 1 = atk THEN RETURN YES
   IF bslot(who).enemy.alone_ai(i) - 1 = atk THEN RETURN YES
  NEXT i
 END IF
 
 RETURN NO
END FUNCTION

SUB queue_attack(attack AS INTEGER, who AS INTEGER, targs() AS INTEGER, override_blocking AS INTEGER=-2)
 DIM atk AS AttackData
 loadattackdata atk, attack
 DIM blocking AS INTEGER = (atk.nonblocking = NO)
 IF override_blocking > -2 THEN blocking = override_blocking
 queue_attack attack, who, atk.attack_delay, targs(), blocking
END SUB

SUB queue_attack(attack AS INTEGER, who AS INTEGER, delay AS INTEGER, targs() AS INTEGER, blocking AS INTEGER=YES)
 'DIM targstr AS STRING = ""
 'FOR i AS INTEGER = 0 TO UBOUND(targs)
 ' IF targs(i) > -1 THEN targstr &= " " & i & "=" & targs(i)
 'NEXT i
 'debug "queue_attack " & readattackname(attack) & ", " & who & ", " & targstr
 
 FOR i AS INTEGER = 0 TO UBOUND(atkq)
  IF atkq(i).used = NO THEN
   'Recycle a queue slot
   set_attack_queue_slot i, attack, who, delay, targs(), blocking
   EXIT SUB
  END IF
 NEXT i
 'No spaces to recycle, grow the queue
 DIM oldbound AS INTEGER = UBOUND(atkq)
 REDIM PRESERVE atkq(oldbound + 16) AS AttackQueue
 FOR i AS INTEGER = oldbound + 2 TO UBOUND(atkq)
  clear_attack_queue_slot i
 NEXT i
 set_attack_queue_slot oldbound + 1, attack, who, delay, targs(), blocking
END SUB

SUB set_attack_queue_slot(slot AS INTEGER, attack AS INTEGER, who AS INTEGER, delay AS INTEGER, targs() AS INTEGER, blocking AS INTEGER=YES)
 WITH atkq(slot)
  .used = YES
  .attack = attack
  .attacker = who
  .delay = delay
  FOR i AS INTEGER = 0 TO UBOUND(.t)
   .t(i) = targs(i)
  NEXT i
  .blocking = blocking
 END WITH
END SUB

SUB clear_attack_queue()
 FOR i AS INTEGER = 0 TO UBOUND(atkq)
  clear_attack_queue_slot i
 NEXT i
END SUB

SUB clear_attack_queue_slot(slot AS INTEGER)
 WITH atkq(slot)
  .used = NO
  .attack = -1
  .attacker = -1
  FOR i AS INTEGER = 0 TO UBOUND(.t)
   .t(i) = -1
  NEXT i
  .blocking = YES
 END WITH
END SUB

SUB display_attack_queue (bslot() AS BattleSprite)
 DIM s AS STRING
 DIM targstr AS STRING
 FOR i AS INTEGER = 0 TO UBOUND(atkq)
  WITH atkq(i)
   IF .used THEN
    s = .delay & " " & bslot(.attacker).name & "(" & .attacker & ") " & readattackname(.attack) & "(" & .attack & ") "
    targstr = ""
    FOR j AS INTEGER = 0 TO UBOUND(.t)
     IF .t(j) > -1 THEN
      targstr &= CHR(24) & .t(j)
     END IF
    NEXT j
    s & = targstr & " " & yesorno(.blocking, "B", "Q")
   ELSE
    s = "-"
   END IF
   edgeprint s, 0, i * 10, uilook(uiText), dpage
  END WITH
 NEXT i
END SUB

FUNCTION battle_time_can_pass(bat AS BattleState) AS INTEGER
 IF bat.atk.id <> -1 THEN RETURN NO 'an attack animation is going on right now
 IF bat.vic.state <> 0 THEN RETURN NO 'victory has already happened
 IF readbit(gen(), genBits2, 5) <> 0 AND bat.caption_time > 0 THEN RETURN NO 'pause on captions
 RETURN YES
END FUNCTION

SUB battle_background_anim(BYREF bat AS BattleState, formdata() AS INTEGER)
 IF formdata(34) > 0 and gen(genVersion) >= 6 THEN
  bat.bgspeed = loopvar(bat.bgspeed, 0, formdata(35), 1)
  IF bat.bgspeed = 0 THEN
   bat.curbg = loopvar(bat.curbg, formdata(32), formdata(32) + formdata(34), 1)
   loadmxs game + ".mxs", bat.curbg MOD gen(genNumBackdrops), vpages(2)
  END IF
 END IF
END SUB

FUNCTION battle_run_away(BYREF bat AS BattleState, bslot() AS BattleSprite) AS INTEGER
 '--this function is called every tick of battle. It returns YES if
 '-- a successful run has completed, thus ending battle.
 
 battle_crappy_run_handler bat, bslot()
 
 '--bat.away will be set to a positive number if running has succeeded
 IF bat.away > 0 THEN
  battle_animate_running_away bslot()
  bat.away += 1
  IF bat.away > 10 THEN
   RETURN YES
  END IF
 END IF
 
 RETURN NO
END FUNCTION

SUB battle_animate_running_away (bslot() AS BattleSprite)
 FOR i AS INTEGER = 0 TO 3
  '--if alive, animate running away
  IF bslot(i).stat.cur.hp > 0 THEN
   WITH bslot(i)
    IF .vis THEN
     .xmov = 10
     .xspeed = 6
     bslot(i).walk = 1
     .d = 1
    END IF
   END WITH
  END IF
 NEXT i
END SUB

SUB battle_check_delays(BYREF bat AS BattleState, bslot() AS BattleSprite)
 '--check the attack queue delays
 FOR i AS INTEGER = 0 TO UBOUND(atkq)
  WITH atkq(i)
   IF .used THEN
    IF .delay <= 0 THEN
     'debug "queue trigger! " & bslot(.attacker).name & .attacker & ":" & readattackname(.attack)
     IF .t(0) = -1 THEN
      debuginfo "queued attack " & readattackname(.attack) & " for " & bslot(.attacker).name & .attacker & " in slot " & i & " has null target."
      clear_attack_queue_slot i
      CONTINUE FOR
     END IF
     IF bslot(.t(0)).stat.cur.hp <= 0 AND NOT attack_can_hit_dead(.attacker, .attack, bslot(.attacker).stored_targs_can_be_dead) THEN
      'debuginfo "queued attack " & readattackname(.attack) & " for " & bslot(.attacker).name & .attacker & " in slot " & i & " has dead target, retargetting."
      autotarget .attacker, .attack, bslot(), .t(), NO
     END IF
     bat.atk.id = .attack
     bat.acting = .attacker
     FOR j AS INTEGER = 0 TO UBOUND(.t)
      bat.anim_t(j) = .t(j)
     NEXT j
     bat.anim_blocking_delay = .blocking
     bat.anim_ready = NO
     clear_attack_queue_slot i
     EXIT FOR
    END IF
   END IF
  END WITH
 NEXT i
END SUB

SUB battle_check_for_hero_turns(BYREF bat AS BattleState, bslot() AS BattleSprite)
 bat.next_hero = loopvar(bat.next_hero, 0, 3, 1)
 IF bat.hero_turn > -1 THEN
  '--somebody is already taking their turn
  EXIT SUB
 END IF

 IF xreadbit(gen(), 7, genBits2) AND bat.atk.id > -1 THEN
  '--an attack is currently animating, and the bitset tells us we must wait for it
  EXIT SUB
 END IF

 '--if it is not currently any hero's turn, check to see if anyone is alive and ready
 DIM turn_started AS INTEGER = NO
 FOR i AS INTEGER = bat.next_hero TO 3
  IF battle_check_a_hero_turn(bat, bslot(), i) THEN
   turn_started = YES
   EXIT FOR
  END IF
 NEXT i
 IF turn_started = NO THEN
  FOR i AS INTEGER = 0 TO bat.next_hero - 1
   IF battle_check_a_hero_turn(bat, bslot(), i) THEN
    EXIT FOR
   END IF
  NEXT i
 END IF
 
END SUB

FUNCTION battle_check_a_hero_turn(BYREF bat AS BattleState, bslot() AS BattleSprite, index AS INTEGER)
 IF bslot(index).ready = YES AND bslot(index).stat.cur.hp > 0 AND bat.death_mode = deathNOBODY THEN
  bat.hero_turn = index
  bat.pt = 0
  bat.menu_mode = batMENUHERO
  RETURN YES
 END IF
 RETURN NO
END FUNCTION

SUB battle_check_for_enemy_turns(BYREF bat AS BattleState, bslot() AS BattleSprite)
 bat.next_enemy = loopvar(bat.next_enemy, 4, 11, 1)
 IF bat.enemy_turn = -1 THEN
  '--if no enemy is currently taking their turn, check to find an enemy who is ready

  DIM turn_started AS INTEGER = NO
  FOR i AS INTEGER = bat.next_enemy TO 11
   IF battle_check_an_enemy_turn(bat, bslot(), i) THEN
    turn_started = YES
    EXIT FOR
   END IF
  NEXT i
  IF turn_started = NO THEN
   FOR i AS INTEGER = 4 TO bat.next_enemy - 1
    IF battle_check_an_enemy_turn(bat, bslot(), i) THEN
     EXIT FOR
    END IF
   NEXT i
  END IF

 END IF
END SUB

FUNCTION battle_check_an_enemy_turn(BYREF bat AS BattleState, bslot() AS BattleSprite, index AS INTEGER)
 IF bslot(index).ready = YES AND bslot(index).stat.cur.hp > 0 AND bat.death_mode = deathNOBODY THEN
  bat.enemy_turn = index
  RETURN YES
 END IF
 RETURN NO
END FUNCTION

FUNCTION blocked_by_attack (who AS INTEGER) AS INTEGER
 FOR i AS INTEGER = 0 TO UBOUND(atkq)
  WITH atkq(i)
   IF .used ANDALSO .attacker = who ANDALSO .delay > 0 ANDALSO .blocking THEN RETURN YES
  END WITH
 NEXT i
 RETURN NO
END FUNCTION

FUNCTION ready_meter_may_grow (bslot() AS BattleSprite, who AS INTEGER) AS INTEGER
 WITH bslot(who)
  IF .attack <> 0 THEN RETURN NO
  IF .dissolve <> 0 THEN RETURN NO
  IF .stat.cur.stun < .stat.max.stun THEN RETURN NO
  IF .ready = YES THEN RETURN NO
 END WITH
 IF blocked_by_attack(who) THEN RETURN NO
 RETURN YES
END FUNCTION

SUB battle_attack_cancel_target_attack(targ as INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite, BYREF attack AS AttackData)
 IF attack.cancel_targets_attack THEN
  '--try to cancel target's attack
  DIM targets_attack AS AttackData
  FOR i AS INTEGER = 0 TO UBOUND(atkq)
   WITH atkq(i)
    IF .used ANDALSO .attacker = targ THEN
     loadattackdata targets_attack, .attack
     IF targets_attack.not_cancellable_by_attacks = NO THEN
      'Okay to cancel target's attack
      clear_attack_queue_slot i
     END IF
    END IF
   END WITH
  NEXT i
 END IF
 IF attack.cancel_targets_attack OR bslot(targ).stat.cur.stun < bslot(targ).stat.max.stun THEN
  '--If the currently targeting hero is the one hit, stop targetting
  '--note that stunning implies cancellation of untargetted attacks,
  '--but does not imply cancellation of already-targeted attacks.
  IF bat.hero_turn = targ THEN
   bat.targ.mode = targNONE
   bat.hero_turn = -1
   bslot(targ).attack = 0
  END IF
 END IF
END SUB

SUB battle_reevaluate_dead_targets (deadguy AS INTEGER, BYREF bat AS BattleState, bslot() AS BattleSprite)
 '--check for queued attacks that target the dead target
 FOR i AS INTEGER = 0 TO UBOUND(atkq)
  WITH atkq(i)
   IF .used THEN
    DIM attack AS AttackData
    loadattackdata attack, .attack
    
    'DIM s AS STRING
    's = attack.name & " of " & bslot(.attacker).name
    'dim showdebug as integer=NO
    'for j as integer = 0 to ubound(.t)
    ' if deadguy = .t(j) then s &= " was targeting " & bslot(deadguy).name & deadguy & " who died": showdebug=YES
    'next j
    'if showdebug then debug s
    
    IF NOT attack_can_hit_dead(.attacker, .attack, bslot(.attacker).stored_targs_can_be_dead) THEN
     battle_sort_away_dead_t_target deadguy, .t()
    END IF
    IF .t(0) = -1 THEN
     'if no targets left, auto-re-target
     autotarget .attacker, .attack, bslot(), .t(), NO
    END IF
   END IF
  END WITH
 NEXT i
 
 '--cancel current interactive targetting that points to the dead target (unless the attack is allowed to target dead)
 IF bat.targ.hit_dead = NO THEN
  IF bat.targ.mask(deadguy) = 1 THEN bat.targ.mask(deadguy) = 0
  IF bat.targ.selected(deadguy) = 1 THEN bat.targ.selected(deadguy) = 0
  '--if current interactive targeting points to the dead target, find a new target
  WITH bat.targ
   IF .pointer = deadguy THEN
    .pointer = 0
    WHILE .mask(.pointer) = 0
     .pointer += 1
     IF .pointer > 11 THEN
      .mode = targNONE
      EXIT WHILE
     END IF
    WEND
   END IF
  END WITH
 END IF  '----END ONLY WHEN bat.targ.hit_dead = NO
END SUB

SUB battle_sort_away_dead_t_target(deadguy AS INTEGER, t() AS INTEGER)
 '--FIXME: la la la! James loves Bogo-sorts!
 FOR i AS INTEGER = 0 TO UBOUND(t) - 1
  '--crappy bogo-sort dead target away
  IF t(i) = deadguy THEN SWAP t(i), t(i + 1)
 NEXT i
 IF t(UBOUND(t)) = deadguy THEN t(UBOUND(t)) = -1
END SUB
