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
#INCLUDE "scancodes.bi"

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
DECLARE SUB spawn_on_death(deadguy AS INTEGER, killing_attack AS INTEGER, BYREF bat AS BattleState, es(), formdata(), bslot() AS BattleSprite, bstat() AS BattleStats, BYREF rew AS RewardsState)
DECLARE SUB triggerfade(BYVAL who, bstat() AS BattleStats, bslot() AS BattleSprite)
DECLARE SUB check_death(deadguy AS INTEGER, BYVAL killing_attack AS INTEGER, BYREF bat AS BattleState, BYREF rew AS RewardsState, bstat() AS BattleStats, bslot() AS BattleSprite, es(), formdata())
DECLARE SUB checkitemusability(iuse() AS INTEGER, bstat() AS BattleStats, who AS INTEGER)
DECLARE SUB reset_battle_state (BYREF bat AS BattleState)
DECLARE SUB reset_targetting (BYREF bat AS BattleState)
DECLARE SUB reset_attack (BYREF bat AS BattleState)
DECLARE SUB reset_victory_state (BYREF vic AS VictoryState)
DECLARE SUB reset_rewards_state (BYREF rew AS RewardsState)
DECLARE SUB show_victory (BYREF vic AS VictoryState, BYREF rew AS RewardsState, exstat() AS INTEGER, bslot() AS BattleSprite)
DECLARE SUB trigger_victory(BYREF vic AS VictoryState, BYREF rew AS RewardsState, bstat() As BattleStats, exstat() AS INTEGER)
DECLARE SUB fulldeathcheck (killing_attack AS INTEGER, bat AS BattleState, bslot() AS BattleSprite, bstat() As BattleStats, rew AS RewardsState, es() AS INTEGER, formdata() AS INTEGER)
DECLARE SUB anim_flinchstart(who AS INTEGER, bslot() AS BattleSprite, atk() AS INTEGER)
DECLARE SUB anim_flinchdone(who AS INTEGER, bslot() AS BattleSprite, atk() AS INTEGER)

'these are the battle global variables
DIM as string battlecaption
dim as integer battlecaptime, battlecapdelay, bstackstart, learnmask(245) '6 shorts of bits per hero

REM $STATIC
FUNCTION battle (form, fatal, exstat()) as integer

REMEMBERSTATE

lastformation = form

'--prepare stack
bstackstart = stackpos

battle = 1
DIM formdata(40)
DIM atk(40 + dimbinsize(binATTACK))
DIM attack AS AttackData
DIM targets_attack AS AttackData
DIM st(3) as herodef, es(7, 160), zbuf(24), ctr(11)
DIM menu$(3, 5), menubits(2), mend(3), spel$(23), speld$(23), spel(23), cost$(23), delay(11), cycle(24), walk(3), aframe(11, 11)
DIM fctr(24), harm$(11), hc(23), hx(11), hy(11), conlmp(11), icons(11), lifemeter(3), prtimer(11,1), spelmask(1)
DIM iuse(inventoryMax / 16) AS INTEGER
DIM laststun AS DOUBLE
DIM bat AS BattleState
DIM bslot(24) AS BattleSprite
DIM bstat(11) AS BattleStats
DIM vic AS VictoryState
DIM as double timinga, timingb
DIM mapsong
DIM nmenu(3,5) as integer 'new battle menu
DIM rew AS RewardsState
DIM tcount AS INTEGER 'FIXME: This is used locally in atkscript and action GOSUB blocks. Move DIMs there when those are SUBified
DIM atktype(8) AS INTEGER 'FIXME: this used locally in sponhit: move the DIM there when SUBifiying it
DIM inv_height AS INTEGER 'FIXME: this is only used in display:
DIM inv_scroll AS MenuState 'FIXME: this is only used in display:
WITH inv_scroll
 .first = 0
 .last = INT(last_inv_slot() / 3)
 .size = 8
END WITH
DIM inv_scroll_rect AS RectType 'FIXME: this is only used in display:
WITH inv_scroll_rect
 .x = 20
 .y = 8
 .wide = 292
 '.high set later
END WITH

'Remember the music that was playing on the map so that the prepare_map() sub can restart it later
gam.remembermusic = presentsong

timinga = 0
timingb = 0

cancelspell$ = readglobalstring$(51, "(CANCEL)", 10)
pause$ = readglobalstring$(54, "PAUSE", 10)
cannotrun$ = readglobalstring$(147, "CANNOT RUN!", 20)

battlecaptime = 0
battlecapdelay = 0
battlecaption = ""

alert = 0
alert$ = ""

fadeout 240, 240, 240
needf = 1: fiptr = 0
reset_battle_state bat
reset_victory_state vic
reset_rewards_state rew
aset = 0: wf = 0

FOR i = 0 TO 11
 icons(i) = -1
 bslot(i).revenge = -1
 bslot(i).thankvenge = -1
NEXT i

'hc(0-11) is harm count... hc(12-23) is harm color... I know, tacky :(
FOR i = 0 TO 11
 hc(i + 12) = 15
NEXT i

'--init affliction registers
'--it should be clear by the fact that BattleStats is a separate type that
'--that bstat() inside battle is not the same array as stat() outside battle
FOR i = 0 TO 11
 WITH bstat(i).cur
  .poison = 1000
  .regen  = 1000
  .stun   = 1000
  .mute   = 1000
 END WITH
 WITH bstat(i).max
  .poison = 1000
  .regen  = 1000
  .stun   = 1000
  .mute   = 1000
 END WITH
 prtimer(i, 0) = INT(RND * 2000)
 prtimer(i, 1) = INT(RND * 2000)
NEXT i
laststun = TIMER
IF gen(genPoison) <= 0 THEN gen(genPoison) = 161
IF gen(genStun) <= 0 THEN gen(genStun) = 159
IF gen(genMute) <= 0 THEN gen(genMute) = 163

clearpage 0
clearpage 1
clearpage 2
clearpage 3
GOSUB loadall
copypage 2, dpage

'--main battle loop----------------------------------------------------------
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 flash = loopvar(flash, 0, 14, 1)

 '--background animation hack
 IF formdata(34) > 0 and gen(genVersion) >= 6 THEN
  bgspeed = loopvar(bgspeed, 0, formdata(35), 1)
  IF bgspeed = 0 THEN
   curbg = loopvar(curbg, formdata(32), formdata(32) + formdata(34), 1)
   loadpage game + ".mxs", curbg MOD gen(genMaxBackdrop), 2
  END IF
 END IF

 IF readbit(gen(), 101, 8) = 0 THEN
  '--debug keys
  IF keyval(62) > 1 THEN away = 11 ' Instant-cheater-running
  IF keyval(63) > 1 THEN rew.exper = 1000000  'Million experience!
  IF keyval(87) > 1 THEN vis = vis XOR 1  'Draw debug info
 END IF
 IF keyval(69) > 1 THEN GOSUB pgame '--PAUSE
 '--running away
 IF carray(5) > 1 AND readbit(gen(), genBits2, 1) = 0 THEN
  flee = flee + 1
 END IF
 GOSUB tryrun
 IF away > 0 THEN
  FOR i = 0 TO 3
   '--if alive, animate running away
   IF bstat(i).cur.hp > 0 THEN
    WITH bslot(i)
     IF .vis THEN
      .xmov = 10
      .xspeed = 6
      walk(i) = 1
      .d = 1
     END IF
    END WITH
   END IF
  NEXT i
  away = away + 1
  IF away > 10 THEN
   battle = 0
   EXIT DO
  END IF
 END IF
 IF bat.atk.id >= 0 AND aset = 0 AND vic.state = 0 THEN GOSUB atkscript
 IF bat.atk.id >= 0 AND aset = 1 AND vic.state = 0 AND away = 0 THEN GOSUB action
 GOSUB animate
 na = loopvar(na, 0, 11, 1)
 IF bat.atk.id = -1 AND vic.state = 0 THEN
  GOSUB meters
  IF bslot(na).attack > 0 AND delay(na) = 0 THEN
   '--next attacker has an attack selected and the delay is over
   bat.atk.id = bslot(na).attack - 1
   bat.acting = na
   aset = 0
   bslot(na).attack = 0
  END IF
 END IF
 bat.next_hero = loopvar(bat.next_hero, 0, 3, 1)
 IF bat.hero_turn = -1 THEN
  '--if it is no heros turn, check to see if anyone is alive and ready
  IF bslot(bat.next_hero).ready = YES AND bstat(bat.next_hero).cur.hp > 0 AND bat.death_mode = deathNOBODY THEN
   bat.hero_turn = bat.next_hero
   bat.pt = 0
   bat.menu_mode = batMENUHERO
  END IF
 END IF
 bat.next_enemy = loopvar(bat.next_enemy, 4, 11, 1)
 IF bat.enemy_turn = -1 THEN
  IF bslot(bat.next_enemy).ready = YES AND bstat(bat.next_enemy).cur.hp > 0 AND bat.death_mode = deathNOBODY THEN bat.enemy_turn = bat.next_enemy
 END IF
 IF vic.state = 0 THEN
  IF bat.enemy_turn >= 0 THEN enemy_ai bat, bstat(), bslot(), es(), formdata(), rew, ctr(), delay()
  IF bat.hero_turn >= 0 AND bat.targ.mode = targNONE THEN
   IF bat.menu_mode = batMENUITEM THEN GOSUB itemmenu
   IF bat.menu_mode = batMENUSPELL THEN spellmenu bat, spel(), st(), bstat(), bslot(), delay(), conlmp()
   IF bat.menu_mode = batMENUHERO THEN heromenu bat, bslot(), bstat(), menubits(), nmenu(), mend(), delay(), spel$(), speld$(), cost$(), spel(), spelmask(), iuse(), st()
  END IF
  IF bat.hero_turn >= 0 AND bat.targ.mode > targNONE THEN GOSUB picktarg
 END IF

 '--Begin display 
 copypage 2, dpage
 GOSUB sprite
 GOSUB display
 IF vic.state = vicEXITDELAY THEN vic.state = vicEXIT
 IF vic.state > 0 THEN show_victory vic, rew, exstat(), bslot()
 IF vis = 1 THEN GOSUB seestuff
 IF bat.death_mode = deathENEMIES AND vic.state = 0 THEN
  IF count_dissolving_enemies(bslot()) = 0 THEN trigger_victory vic, rew, bstat(), exstat()
 END IF
 IF vic.state = vicEXIT THEN EXIT DO 'normal victory exit
 IF bat.death_mode = deathHEROES THEN
  fatal = 1
  EXIT DO
 END IF
 IF alert > 0 THEN
  alert = alert - 1
  centerfuz 160, 190, 100, 16, 3, dpage
  edgeprint alert$, 160 - LEN(alert$) * 4, 185, uilook(uiSelectedItem + tog), dpage
 END IF

 if dotimerbattle then
  fatal = 0
  exit do
 end if

 '--show the timer
 FOR i = 0 to 15
   if timers(i).speed > 0 and timers(i).st > -1 and timers(i).flags and 2 = 2 then
     edgeprint plotstr(timers(i).st-1).s, 320 - len(plotstr(timers(i).st-1).s) * 10, 185, uilook(uiText), dpage
     exit for
   end if
 NEXT

 SWAP vpage, dpage
 setvispage vpage
 IF needf = 1 THEN
  needf = 0
  fadein
  setkeys
 END IF
 dowait
LOOP
donebattle:
writestats exstat(), bstat()
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
 fatalerror "bstack underflow" + XSTR$(stackpos) + XSTR$(bstackstart)
END IF

fadeout 0, 0, 0

clearpage 0
clearpage 1
clearpage 2
clearpage 3


for i = lbound(bslot) to ubound(bslot)
	sprite_unload(@bslot(i).sprites)
	palette16_unload(@bslot(i).pal)
next


RETRIEVESTATE
EXIT FUNCTION '---------------------------------------------------------------

pgame:
fuzzyrect 0, 0, 320, 200, uilook(uiTextBox), vpage
edgeprint pause$, xstring(pause$, 160), 95, uilook(uiText), vpage
setvispage vpage
'--wait for a key
wk = getkey
RETRACE

atkscript: '---------------------------------------------------------------
tcount = 0 'This should be dimmed locally when this is SUBified
'--check for item consumption
IF icons(bat.acting) >= 0 THEN
 IF inventory(icons(bat.acting)).used = 0 THEN
  '--abort if item is gone
  bat.atk.id = -1: RETRACE
 END IF
END IF
'--load attack
loadattackdata atk(), bat.atk.id

IF readbit(atk(), 65, 17) <> 0 THEN
 'The "Re-check costs after attack delay" is on, so cancel the attack if we can't afford it now
 IF atkallowed(atk(), bat.acting, 0, 0, bstat()) = NO THEN
  bat.atk.id = -1: RETRACE
 END IF
END IF

'--load palette
FOR i = 12 TO 23
 bslot(i).frame = 0
 cycle(i) = -1
 bslot(i).z = 0
 'load battle sprites
 with bslot(i)
		.sprite_num = 3
		.frame = 0
		sprite_unload(@.sprites)
		palette16_unload(@.pal)
		.sprites = sprite_load(game + ".pt6", atk(0), 3, 50, 50)
		if not sprite_is_valid(.sprites) then debug "Failed to load attack sprites (#" & i & ")"
		.pal = palette16_load(game + ".pal", atk(1), 6, atk(0))
		if .pal = 0 then debug "Failed to load palette (#" & i & ")"
 end with
NEXT i
tcount = -1: pdir = 0: conmp = 1
IF is_enemy(bat.acting) THEN pdir = 1
FOR i = 0 TO 11
 bslot(bat.acting).keep_dead_targs(i) = NO
NEXT i
'CANNOT HIT INVISIBLE FOES
FOR i = 0 TO 11
 IF bslot(bat.acting).t(i) > -1 THEN
  IF bslot(bslot(bat.acting).t(i)).vis = 0 AND (atk(3) <> 4 AND atk(3) <> 10) THEN
   bslot(bat.acting).t(i) = -1
  END IF
 END IF
NEXT i
'MOVE EMPTY TARGET SLOTS TO THE BACK
FOR o = 0 TO 10
 FOR i = 0 TO 10
  IF bslot(bat.acting).t(i) = -1 THEN SWAP bslot(bat.acting).t(i), bslot(bat.acting).t(i + 1)
 NEXT i
NEXT o
'COUNT TARGETS
FOR i = 0 TO 11
 IF bslot(bat.acting).t(i) > -1 THEN tcount = tcount + 1
 cycle(i) = -1
NEXT i
bat.atk.non_elemental = YES
FOR i = 0 TO 7
 bat.atk.elemental(i) = NO
 IF readbit(atk(), 20, 5 + i) = 1 THEN bat.atk.elemental(i) = YES: bat.atk.non_elemental = NO
NEXT i
'ABORT IF TARGETLESS
IF tcount = -1 THEN bat.atk.id = -1: RETRACE
'Kill old target history
FOR i = 0 TO 11
 bslot(bat.acting).last_targs(i) = NO
NEXT i
' BIG CRAZY SCRIPT CONSTRUCTION
'DEBUG debug "begin script construction"
IF is_hero(bat.acting) THEN
 'load weapon sprites
 with bslot(24)
  .sprite_num = 2
  sprite_unload @.sprites
  .sprites = sprite_load(game & ".pt5", exstat(bat.acting, 0, 13), 2, 24, 24)
  if not sprite_is_valid(.sprites) then debug "Could not load weapon sprite: " & game & ".pt5#" & exstat(bat.acting, 0, 13)
  palette16_unload @.pal
  .pal = palette16_load(game + ".pal", exstat(bat.acting, 1, 13), 5, exstat(bat.acting, 0, 13))
  if .pal = 0 then debug "Failed to load palette (#" & 24 & ")"
  .frame = 0
  
  
 end with
END IF
numhits = atk(17) + INT(RND * (bstat(bat.acting).cur.hits + 1))
IF readbit(atk(), 20, 49) THEN numhits = atk(17)
'----------------------------NULL ANIMATION
IF atk(15) = 10 THEN
 advance bat.acting, atk(), bslot()
 if atk(99) > 0  then anim_sound(atk(99) - 1)
 FOR j = 1 TO numhits
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot()
  FOR i = 0 TO tcount
   anim_inflict bslot(bat.acting).t(i), tcount
  NEXT i
  anim_disappear 24
 NEXT j
 retreat bat.acting, atk(), bslot()
 anim_end
END IF
'----------------------------NORMAL, DROP, SPREAD-RING, and SCATTER
IF atk(15) = 0 OR atk(15) = 3 OR atk(15) = 6 OR (atk(15) = 4 AND tcount > 0) THEN
 atkimgdirection = 0: IF readbit(atk(), 20, 3) = 0 THEN atkimgdirection = pdir
 FOR i = 0 TO tcount
  yt = (bslot(bslot(bat.acting).t(i)).h - 50) + 2
  xt = 0: IF bslot(bat.acting).t(i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
  anim_setpos 12 + i, bslot(bslot(bat.acting).t(i)).x + xt, bslot(bslot(bat.acting).t(i)).y + yt, atkimgdirection
  IF atk(15) = 3 THEN
   anim_setz 12 + i, 180
  END IF
  IF atk(15) = 4 THEN
   anim_setpos 12 + i, bslot(bslot(bat.acting).t(i)).x + xt, bslot(bslot(bat.acting).t(i)).y + yt - bslot(bslot(bat.acting).t(i)).w, atkimgdirection
  END IF
 NEXT i
 advance bat.acting, atk(), bslot()
 FOR j = 1 TO numhits
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   IF atk(15) = 4 THEN
    anim_absmove 12 + i, bslot(bslot(bat.acting).t(i)).x + xt - bslot(bslot(bat.acting).t(i)).w, bslot(bslot(bat.acting).t(i)).y + yt, 3, 3
   END IF
   IF atk(15) = 3 THEN
    anim_zmove 12 + i, -10, 20
   END IF
   IF atk(15) = 6 THEN
    anim_absmove 12 + i, INT(RND * 270), INT(RND * 150), 6, 6
   END IF
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 2
  IF atk(15) = 3 THEN
   anim_wait 3
  END IF
  anim_setframe bat.acting, 0
  anim_disappear 24
  IF atk(15) = 4 THEN
   FOR i = 0 TO tcount
    anim_absmove 12 + i, bslot(bslot(bat.acting).t(i)).x + xt, bslot(bslot(bat.acting).t(i)).y + yt + bslot(bslot(bat.acting).t(i)).w, 3, 3
   NEXT i
   anim_waitforall
   FOR i = 0 TO tcount
    anim_absmove 12 + i, bslot(bslot(bat.acting).t(i)).x + xt + bslot(bslot(bat.acting).t(i)).w, bslot(bslot(bat.acting).t(i)).y + yt, 3, 3
   NEXT i
   anim_waitforall
   FOR i = 0 TO tcount
    anim_absmove 12 + i, bslot(bslot(bat.acting).t(i)).x + xt, bslot(bslot(bat.acting).t(i)).y + yt - bslot(bslot(bat.acting).t(i)).w, 3, 3
   NEXT i
   anim_waitforall
  END IF
  FOR i = 0 TO tcount
   anim_inflict bslot(bat.acting).t(i), tcount
   anim_flinchstart bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  IF atk(15) <> 4 THEN
   anim_wait 3
  END IF
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   anim_flinchdone bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  anim_wait 2
 NEXT j
 retreat bat.acting, atk(), bslot()
 FOR i = 0 TO tcount
  anim_setframe bslot(bat.acting).t(i), 0
 NEXT i
 anim_end
END IF
'----------------------------SEQUENTIAL PROJECTILE
IF atk(15) = 7 THEN
 'attacker steps forward
 advance bat.acting, atk(), bslot()
 'repeat the following for each attack
 FOR j = 1 TO numhits
  'attacker animates
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot()
  'calculate where the projectile will start relative to the attacker
  startoffset = 50: IF is_hero(bat.acting) THEN startoffset = -50
  'calculate the direction the projectile should be facing
  atkimgdirection = 0: IF readbit(atk(), 20, 3) = 0 THEN atkimgdirection = pdir
  'set the projectile position
  anim_setpos 12, bslot(bat.acting).x + startoffset, bslot(bat.acting).y, atkimgdirection
  anim_appear 12
  'play the sound effect
  IF atk(99) > 0 THEN anim_sound(atk(99) - 1)
  'repeat the following for each target...
  FOR i = 0 TO tcount
   'find the target's position
   yt = (bslot(bslot(bat.acting).t(i)).h - 50) + 2
   xt = 0: IF bslot(bat.acting).t(i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
   'make the projectile move to the target
   anim_absmove 12, bslot(bslot(bat.acting).t(i)).x + xt, bslot(bslot(bat.acting).t(i)).y + yt, 5, 5
   anim_waitforall
   'inflict damage
   anim_inflict bslot(bat.acting).t(i), tcount
   anim_flinchstart bslot(bat.acting).t(i), bslot(), atk()
   anim_wait 3
   anim_flinchdone bslot(bat.acting).t(i), bslot(), atk()
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
 retreat bat.acting, atk(), bslot()
 anim_end
END IF
'-----------------PROJECTILE, REVERSE PROJECTILE and METEOR
IF (atk(15) >= 1 AND atk(15) <= 2) OR atk(15) = 8 THEN
 advance bat.acting, atk(), bslot()
 FOR j = 1 TO numhits
  FOR i = 0 TO tcount
   temp = 50: IF is_hero(bat.acting) THEN temp = -50
   dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
   yt = (bslot(bslot(bat.acting).t(i)).h - 50) + 2
   xt = 0: IF bslot(bat.acting).t(i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 THEN
    anim_setpos 12 + i, bslot(bat.acting).x + temp, bslot(bat.acting).y, dtemp
   END IF
   IF atk(15) = 2 THEN
    anim_setpos 12 + i, bslot(bslot(bat.acting).t(i)).x + xt, bslot(bslot(bat.acting).t(i)).y + yt, dtemp
   END IF
   IF atk(15) = 8 THEN
    IF is_hero(bat.acting) THEN
     anim_setpos 12 + i, 320, 100, dtemp
    END IF
    IF is_enemy(bat.acting) THEN
     anim_setpos 12 + i, -50, 100, dtemp
    END IF
    anim_setz 12 + i, 180
   END IF
  NEXT i
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   temp = 50: IF is_hero(bat.acting) THEN temp = -50
   yt = (bslot(bslot(bat.acting).t(i)).h - 50) + 2
   xt = 0: IF bslot(bat.acting).t(i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 OR atk(15) = 8 THEN
    anim_absmove 12 + i, bslot(bslot(bat.acting).t(i)).x + xt, bslot(bslot(bat.acting).t(i)).y + yt, 6, 6
   END IF
   IF atk(15) = 2 THEN
    anim_absmove 12 + i, bslot(bat.acting).x + temp, bslot(bat.acting).y, 6, 6
   END IF
   IF atk(15) = 8 THEN
    anim_zmove 12 + i, -6, 30
   END IF
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 8
  anim_disappear 24
  anim_setframe bat.acting, 0
  FOR i = 0 TO tcount
   anim_inflict bslot(bat.acting).t(i), tcount
   anim_flinchstart bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  anim_wait 3
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   anim_flinchdone bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  anim_wait 3
 NEXT j
 retreat bat.acting, atk(), bslot()
 FOR i = 0 TO tcount
  anim_setframe bslot(bat.acting).t(i), 0
 NEXT i
 anim_end
END IF
'--------------------------------------DRIVEBY
IF atk(15) = 9 THEN
 advance bat.acting, atk(), bslot()
 FOR j = 1 TO numhits
  dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
  FOR i = 0 TO tcount
   yt = (bslot(bslot(bat.acting).t(i)).h - 50) + 2
   IF is_hero(bat.acting) THEN
    anim_setpos 12 + i, 320, bslot(bslot(bat.acting).t(i)).y + yt, dtemp
   END IF
   IF is_enemy(bat.acting) THEN
    anim_setpos 12 + i, -50, bslot(bslot(bat.acting).t(i)).y + yt, dtemp
   END IF
  NEXT i
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   temp = 50: IF is_hero(bat.acting) THEN temp = -50
   yt = (bslot(bslot(bat.acting).t(i)).h - 50) + 2
   anim_absmove 12 + i, bslot(bslot(bat.acting).t(i)).x + xt, bslot(bslot(bat.acting).t(i)).y + yt, 8, 8
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 4
  anim_disappear 24
  anim_setframe bat.acting, 0
  anim_waitforall
  FOR i = 0 TO tcount
   anim_inflict bslot(bat.acting).t(i), tcount
   anim_flinchstart bslot(bat.acting).t(i), bslot(), atk()
   yt = (bslot(bslot(bat.acting).t(i)).h - 50) + 2
   IF is_hero(bat.acting) THEN
    anim_absmove 12 + i, -50, bslot(bslot(bat.acting).t(i)).y + yt, 5, 7
   END IF
   IF is_enemy(bat.acting) THEN
    anim_absmove 12 + i, 320, bslot(bslot(bat.acting).t(i)).y + yt, 5, 7
   END IF
  NEXT i
  anim_waitforall
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   anim_flinchdone bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  anim_wait 3
 NEXT j
 retreat bat.acting, atk(), bslot()
 FOR i = 0 TO tcount
  anim_setframe bslot(bat.acting).t(i), 0
 NEXT i
 anim_end
END IF
'--------------------------------FOCUSED RING
IF atk(15) = 4 AND tcount = 0 THEN
 dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
 advance bat.acting, atk(), bslot()
 FOR j = 1 TO numhits
  i = 0
  yt = (bslot(bslot(bat.acting).t(i)).h - 50) + 2
  xt = 0: IF bslot(bat.acting).t(i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
  tempx = bslot(bslot(bat.acting).t(i)).x + xt
  tempy = bslot(bslot(bat.acting).t(i)).y + yt
  anim_setpos 12 + 0, tempx + 0, tempy - 50, dtemp
  anim_setpos 12 + 1, tempx + 30, tempy - 30, dtemp
  anim_setpos 12 + 2, tempx + 50, tempy + 0, dtemp
  anim_setpos 12 + 3, tempx + 30, tempy + 30, dtemp
  anim_setpos 12 + 4, tempx - 0, tempy + 50, dtemp
  anim_setpos 12 + 5, tempx - 30, tempy + 30, dtemp
  anim_setpos 12 + 6, tempx - 50, tempy - 0, dtemp
  anim_setpos 12 + 7, tempx - 30, tempy - 30, dtemp
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot()
  yt = (bslot(bslot(bat.acting).t(0)).h - 50) + 2
  xt = 0: IF bslot(bat.acting).t(i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
  FOR i = 0 TO 7
   anim_appear 12 + i
   anim_absmove 12 + i, bslot(bslot(bat.acting).t(0)).x + xt, bslot(bslot(bat.acting).t(0)).y + yt, 4, 4
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 8
  anim_disappear 24
  anim_setframe bat.acting, 0
  FOR i = 0 TO tcount
   anim_inflict bslot(bat.acting).t(i), tcount
   anim_flinchstart bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  anim_wait 3
  FOR i = 0 TO 7
   anim_disappear 12 + i
  NEXT i
  FOR i = 0 TO tcount
   anim_flinchdone bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  anim_wait 3
 NEXT j
 retreat bat.acting, atk(), bslot()
 FOR i = 0 TO tcount
  anim_setframe bslot(bat.acting).t(i), 0
 NEXT i
 anim_end
END IF
'--------------------------------WAVE
IF atk(15) = 5 THEN
 yt = bslot(bslot(bat.acting).t(0)).y + (bslot(bslot(bat.acting).t(0)).h - 50) + 2
 advance bat.acting, atk(), bslot()
 'calculate the direction the wave sprite should be facing
 atkimgdirection = 0: IF readbit(atk(), 20, 3) = 0 THEN atkimgdirection = pdir
 FOR j = 1 TO numhits
  FOR i = 0 TO 11
   temp = -50: IF is_hero(bat.acting) THEN temp = 320
   IF tcount > 0 OR atk(4) = 1 THEN
    anim_setpos 12 + i, temp, i * 15, atkimgdirection
   ELSE
    anim_setpos 12 + i, temp, yt, atkimgdirection
   END IF
  NEXT i
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot()
  temp = 24: IF is_hero(bat.acting) THEN temp = -24
  if atk(99) > 0  then anim_sound(atk(99) - 1)

  FOR i = 0 TO 11
   anim_appear 12 + i
   anim_setmove 12 + i, temp, 0, 16, 0
   anim_wait 1
  NEXT i
  anim_wait 15
  anim_disappear 24
  anim_setframe bat.acting, 0
  FOR i = 0 TO tcount
   anim_inflict bslot(bat.acting).t(i), tcount
   anim_flinchstart bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  anim_waitforall
  FOR i = 0 TO 11
   anim_disappear 12 + i
  NEXT i
  FOR i = 0 TO tcount
   anim_flinchdone bslot(bat.acting).t(i), bslot(), atk()
  NEXT i
  anim_wait 2
 NEXT j
 retreat bat.acting, atk(), bslot()
 FOR i = 0 TO tcount
  anim_setframe bslot(bat.acting).t(i), 0
 NEXT i
 anim_end
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
 setbatcap readbinstring$(atk(), 37, 38), 0, atk(57)
 SELECT CASE atk(36)
  CASE 0
   '--full duration
   battlecaptime = 16383 + atk(57)
  CASE IS > 0
   '--timed
   battlecaptime = atk(36) + atk(57)
 END SELECT
END IF
'DEBUG debug "stackpos =" + XSTR$((stackpos - bstackstart) \ 2)
invertstack
'--Remember the attack ID for later call to fulldeathcheck
bat.atk.was_id = bat.atk.id
'--aset indicates that animation is set and that we should proceed to "action"
aset = 1
RETRACE

action:
tcount = 0 'This should be dimmed locally when this is SUBified
IF wf > 0 THEN wf = wf - 1: IF wf > 0 THEN RETRACE
IF wf = -1 THEN
 wf = 0
 FOR i = 0 TO 23
  IF bslot(i).xmov <> 0 OR bslot(i).ymov <> 0 OR bslot(i).zmov <> 0 THEN wf = -1
 NEXT i
 IF wf = -1 THEN RETRACE
END IF
wf = 0

DO: 'INTERPRET THE ANIMATION SCRIPT
 act = popw
 SELECT CASE act
  CASE 0 '--end()
   FOR i = 0 TO 3
    '--enforce weak picture
    IF bstat(i).cur.hp < bstat(i).max.hp / 5 AND vic.state = 0 THEN bslot(i).frame = 6
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
   fr = popw
   bslot(ww).frame = fr
   IF is_hero(ww) THEN walk(ww) = 0
  CASE 8 'absmove(who,n,n,n,n)
   ww = popw
   tmp1 = popw
   tmp2 = popw
   tmp3 = popw
   tmp4 = popw
   bslot(ww).xspeed = (tmp1 - bslot(ww).x) / tmp3
   bslot(ww).yspeed = (tmp2 - bslot(ww).y) / tmp4
   bslot(ww).xmov = tmp3
   bslot(ww).ymov = tmp4
  CASE 9 'waitforall()
   wf = -1
  CASE 10 'inflict(targ, target_count)
   targ = popw
   tcount = popw
   'set tag, if there is one
   checkTagCond atk(60), 1, atk(59), atk(61)
   checkTagCond atk(63), 1, atk(62), atk(64)
   IF inflict(bat.acting, targ, bstat(), bslot(), harm$(), hc(), hx(), hy(), atk(), tcount) THEN
    '--attack succeeded
    IF readbit(atk(), 65, 12) THEN
     '--try to cancel target's attack
     IF bslot(targ).attack > 0 THEN
      'Check if the attack is cancelable
      loadattackdata targets_attack, bslot(targ).attack - 1
      IF targets_attack.not_cancellable_by_attacks = NO THEN
       'Okay to cancel target's attack
       bslot(targ).attack = 0
      END IF
     ELSE
      'just cancel the attack
      bslot(targ).attack = 0
     END IF
    END IF
    IF readbit(atk(), 20, 50) = 1 THEN
     es(targ - 4, 56) = 0
     es(targ - 4, 57) = 0
     es(targ - 4, 59) = 0
     es(targ - 4, 61) = 0
    END IF
    IF readbit(atk(), 20, 63) = 1 THEN
    'force heroes to run away
     IF checkNoRunBit(bstat(), bslot()) THEN
      alert$ = cannotrun$
      alert = 10
     ELSE
      away = 1
     END IF
    END IF
    checkTagCond atk(60), 2, atk(59), atk(61)
    checkTagCond atk(63), 2, atk(62), atk(64)
    IF bstat(targ).cur.hp = 0 THEN
     checkTagCond atk(60), 4, atk(59), atk(61)
     checkTagCond atk(63), 4, atk(62), atk(64)
    END IF

    IF trytheft(bat.acting, targ, atk(), es()) THEN
     IF bat.hero_turn >= 0 THEN
      checkitemusability iuse(), bstat(), bat.hero_turn
     END IF
    END IF
   ELSE
    checkTagCond atk(60), 3, atk(59), atk(61)
    checkTagCond atk(63), 3, atk(62), atk(64)
   END IF
   triggerfade targ, bstat(), bslot()
   IF bstat(targ).cur.hp > 0 THEN
    '---REVIVE---
    bslot(targ).vis = 1
    bslot(targ).dissolve = 0
   END IF
   IF is_enemy(targ) AND readbit(atk(), 65, 14) = 0 THEN GOSUB sponhit
   IF conmp = 1 THEN
    '--if the attack costs MP, we want to actually consume MP
    IF atk(8) > 0 THEN bstat(bat.acting).cur.mp = large(bstat(bat.acting).cur.mp - focuscost(atk(8), bstat(bat.acting).cur.foc), 0)

    '--ditto for HP
    IF atk(9) > 0 THEN
      bstat(bat.acting).cur.hp = large(bstat(bat.acting).cur.hp - atk(9), 0)
      hc(bat.acting) = 7
      hx(bat.acting) = bslot(bat.acting).x + (bslot(bat.acting).w * .5)
      hy(bat.acting) = bslot(bat.acting).y + (bslot(bat.acting).h * .5)
      harm$(bat.acting) = STR$(atk(9))
    END IF

    '--ditto for money
    IF atk(10) <> 0 THEN
      gold = large(gold - atk(10), 0)
      hc(bat.acting) = 7
      hx(bat.acting) = bslot(bat.acting).x + (bslot(bat.acting).w * .5)
      hy(bat.acting) = bslot(bat.acting).y + (bslot(bat.acting).h * .5)
      harm$(bat.acting) = STR$(atk(10)) + "$"
      IF atk(10) < 0 THEN harm$(bat.acting) += "+"
      IF gold > 2000000000 THEN gold = 2000000000
      IF gold < 0 THEN gold = 0

    END IF

    '--if the attack consumes items, we want to consume those too
    FOR i = 0 to 2
      IF atk(93 + i * 2) > 0 THEN 'this slot is used
        IF atk(94 + i * 2) > 0 THEN 'remove items
          delitem(atk(93 + i * 2), atk(94 + i * 2))
        ELSEIF atk(94 + i * 2) < 0 THEN 'add items
          getitem(atk(93 + i * 2), abs(atk(94 + i * 2)))
        'ELSE 'uh...
        END IF
      END IF
    NEXT i

    '--set the flag to prevent re-consuming MP
    conmp = 0
   END IF
   IF conlmp(bat.acting) > 0 THEN lmp(bat.acting, conlmp(bat.acting) - 1) = lmp(bat.acting, conlmp(bat.acting) - 1) - 1: conlmp(bat.acting) = 0
   IF icons(bat.acting) >= 0 THEN
    IF consumeitem(icons(bat.acting)) THEN setbit iuse(), 0, icons(bat.acting), 0
    icons(bat.acting) = -1
   END IF
   o = 0
   FOR i = 0 TO 3
    IF bstat(i).cur.hp = 0 THEN o = o + 1
   NEXT i
   IF o = 4 THEN bat.atk.id = -1
   o = 0
   FOR i = 4 TO 11
    IF bstat(i).cur.hp = 0 THEN o = o + 1
   NEXT i
   IF bstat(targ).cur.hp = 0 AND o < 8 AND bat.atk.id > -1 THEN'
    '--if the target is already dead, auto-pick a new target
    '--FIXME: why are we doing this after the attack? Does this even do anything?
    '--       it was passing garbage attack data at least some of the time until r2104
    autotarget bat.acting, atk(), bslot(), bstat()
   END IF
  CASE 11 'setz(who,z)
   ww = popw
   bslot(ww).z = popw
  CASE 12 '???(n,n,n,n,n)
   'unimplemented
  CASE 13 'wait(ticks)
   wf = popw
  CASE 14 'walktoggle(who)
   ww = popw
   bslot(ww).frame = 0
   IF is_hero(ww) THEN walk(ww) = walk(ww) XOR 1
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
	 xd = popw
	 yd = popw
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
		tmp1 = popw'x
		tmp2 = popw'y
		tmp3 = popw'sx
		tmp4 = popw'sy
		with bslot(ww)
			if tmp2 <> 0 then .xspeed = tmp1 / tmp3
			if tmp4 <> 0 then .yspeed = tmp2 / tmp4
			.xmov = tmp1' .x + tmp3
			.ymov = tmp2' .y + tmp4
		end with
	CASE 21 'setdir(who, d)
	 ww = popw
	 tmp1 = popw
	 'debug "blsot(" & ww & ").d = " & tmp1
	 bslot(ww).d = tmp1
 END SELECT
LOOP UNTIL wf <> 0 OR bat.atk.id = -1

IF bat.atk.id = -1 THEN
 GOSUB afterdone
 '--clean up stack
 'DEBUG debug "discarding" + XSTR$((stackpos - bstackstart) \ 2) + " from stack"
 WHILE stackpos > bstackstart: dummy = popw: WEND
 '-------Spawn a Chained Attack--------
 IF atk(12) > 0 AND INT(RND * 100) < atk(13) AND bstat(bat.acting).cur.hp > 0 AND (bslot(bat.acting).attack_succeeded <> 0 AND readbit(atk(),65,7) OR readbit(atk(),65,7) = 0)THEN
  wf = 0: aset = 0
  loadattackdata buffer(), atk(12) - 1
  IF buffer(16) > 0 THEN
   bslot(bat.acting).attack = atk(12)
   delay(bat.acting) = buffer(16)
  ELSE
   bat.atk.id = atk(12) - 1: aset = 0: bslot(bat.acting).attack = 0
  END IF
  o = 0
  FOR i = 4 TO 11
   IF bstat(i).cur.hp = 0 THEN o = o + 1
  NEXT i
  IF o < 8 THEN
   IF buffer(4) <> atk(4) OR buffer(3) <> atk(3) THEN
    'if the chained attack has a different target class/type then re-target
    autotarget bat.acting, buffer(), bslot(), bstat()
   END IF
  END IF
 END IF
END IF
RETRACE

afterdone:
'--hide the caption when the animation is done
IF atk(36) = 0 THEN
 '--clear duration-timed caption
 battlecaptime = 0
 battlecapdelay = 0
END IF
fulldeathcheck bat.atk.was_id, bat, bslot(), bstat(), rew, es(), formdata()
bat.atk.was_id = -1
RETRACE

setuptarg: '--identify valid targets (heroes only)

'init
spred = 0
aim = 0
randomtarg = 0
firsttarg = 0
bat.targ.pointer = 0
FOR i = 0 TO 11
 bat.targ.selected(i) = 0 ' clear list of selected targets
 bslot(bat.hero_turn).t(i) = -1 'clear list of confirmed targets
NEXT i

'load attack
loadattackdata buffer(), bslot(bat.hero_turn).attack - 1

bat.targ.hit_dead = NO
FOR i = 0 to 11
 bslot(bat.hero_turn).keep_dead_targs(i) = NO
NEXT i

get_valid_targs bat.targ.mask(), bat.hero_turn, buffer(), bslot(), bstat()
bat.targ.hit_dead = attack_can_hit_dead(bat.hero_turn, buffer())

'--attacks that can target all should default to the first enemy
IF buffer(3) = 3 THEN
 bat.targ.pointer = 4
END IF

'fail if there are no targets
IF targetmaskcount(bat.targ.mask()) = 0 THEN
 bat.targ.mode = targNONE
 RETRACE
END IF

'autoattack
IF readbit(buffer(), 20, 54) THEN
 bat.targ.mode = targAUTO
 RETRACE
END IF

IF buffer(4) = 0 THEN aim = 1
IF buffer(4) = 1 THEN FOR i = 0 TO 11: bat.targ.selected(i) = bat.targ.mask(i): NEXT i
IF buffer(4) = 2 THEN aim = 1: spred = 1
IF buffer(4) = 3 THEN randomtarg = -1
IF buffer(4) = 4 THEN firsttarg = -1

bat.targ.pointer = find_preferred_target(bat.targ.mask(), bat.hero_turn, buffer(), bslot(), bstat())
'fail if no targets are found
IF bat.targ.pointer = -1 THEN
 bat.targ.mode = targNONE
 RETRACE
END IF

'ready to choose bat.targ.selected() from bat.targ.mask()
bat.targ.mode = targMANUAL
RETRACE

sponhit:
'atktype should be DIMed locally here when this is SUBified
atktype(0) = bat.atk.non_elemental
FOR i = 0 TO 7
 atktype(i + 1) = bat.atk.elemental(i)
NEXT i
FOR i = 0 TO 8
 IF es(targ - 4, 82 + i) > 0 AND atktype(i) = YES THEN
  FOR j = 1 TO es(targ - 4, 91)
   slot = find_empty_enemy_slot(formdata())
   IF slot > -1 THEN
    formdata(slot * 4) = es(targ - 4, 82 + i)
    loadfoe slot, formdata(), es(), bat, bslot(), bstat(), rew
   END IF
  NEXT j
  EXIT FOR
 END IF
NEXT i
RETRACE

itemmenu:
IF carray(5) > 1 THEN
 bat.menu_mode = batMENUHERO
 flusharray carray(), 7, 0
 icons(bat.hero_turn) = -1 '--is this right?
END IF
oldiptr = bat.item.pt
IF carray(0) > 1 AND bat.item.pt > 2 THEN bat.item.pt = bat.item.pt - 3
IF carray(1) > 1 AND bat.item.pt <= last_inv_slot() - 3 THEN bat.item.pt = bat.item.pt + 3
IF keyval(scPageUp) > 1 THEN
 bat.item.pt -= (inv_scroll.size+1) * 3
 WHILE bat.item.pt < 0: bat.item.pt += 3: WEND
END IF
IF keyval(scPageDown) > 1 THEN
 bat.item.pt += (inv_scroll.size+1) * 3
 WHILE bat.item.pt > last_inv_slot(): bat.item.pt -= 3: WEND
END IF
IF carray(2) > 1 AND bat.item.pt > 0 THEN
 bat.item.pt = bat.item.pt - 1
END IF
IF carray(3) > 1 AND bat.item.pt < last_inv_slot() THEN
 bat.item.pt = bat.item.pt + 1
END IF
'--scroll when past top or bottom
WHILE bat.item.pt < bat.item.top : bat.item.top = bat.item.top - 3 : WEND
WHILE bat.item.pt >= bat.item.top + (inv_scroll.size+1) * 3 : bat.item.top = bat.item.top + 3 : WEND

IF oldiptr <> bat.item.pt THEN
 IF inventory(bat.item.pt).used THEN
  loaditemdata buffer(), inventory(bat.item.pt).id
  bat.item_desc = readbadbinstring$(buffer(), 9, 35, 0)
 ELSE
  bat.item_desc = ""
 END IF
END IF

IF carray(4) > 1 THEN
 IF readbit(iuse(), 0, bat.item.pt) = 1 THEN
  loaditemdata buffer(), inventory(bat.item.pt).id
  icons(bat.hero_turn) = -1: IF buffer(73) = 1 THEN icons(bat.hero_turn) = bat.item.pt
  temp = buffer(47)
  loadattackdata buffer(), temp - 1
  bslot(bat.hero_turn).attack = temp
  delay(bat.hero_turn) = large(buffer(16), 1)
  bat.targ.mode = targSETUP
  bat.menu_mode = batMENUHERO
  flusharray carray(), 7, 0
 END IF
END IF
RETRACE

picktarg: '-----------------------------------------------------------

'cancel
IF carray(5) > 1 THEN
 bslot(bat.hero_turn).attack = 0
 conlmp(bat.hero_turn) = 0
 bat.targ.mode = targNONE
 flusharray carray(), 7, 0
 RETRACE
END IF

IF bat.targ.mode = targSETUP THEN GOSUB setuptarg

'autotarget
IF bat.targ.mode = targAUTO THEN
 autotarget bat.hero_turn, buffer(), bslot(), bstat()
 ctr(bat.hero_turn) = 0
 bslot(bat.hero_turn).ready = NO
 bat.hero_turn = -1
 bat.targ.mode = targNONE
 RETRACE
END IF

IF targetmaskcount(bat.targ.mask()) = 0 THEN
 RETRACE
END IF

'random target
IF randomtarg THEN
 FOR i = 0 TO INT(RND * 2)
  bat.targ.pointer = loopvar(bat.targ.pointer, 0, 11, 1)
  WHILE bat.targ.mask(bat.targ.pointer) = 0
   bat.targ.pointer = loopvar(bat.targ.pointer, 0, 11, 1)
  WEND
 NEXT i
END IF

'first target
IF firsttarg THEN
 bat.targ.pointer = 0
 WHILE bat.targ.mask(bat.targ.pointer) = 0
  bat.targ.pointer = loopvar(bat.targ.pointer, 0, 11, 1)
 WEND
END IF

IF spred = 2 AND (carray(2) > 1 OR carray(3) > 1) AND randomtarg = 0 AND firsttarg = 0 THEN
 FOR i = 0 TO 11
  bat.targ.selected(i) = 0
 NEXT i
 spred = 1
 flusharray carray(), 7, 0
END IF
IF aim = 1 AND spred < 2 AND randomtarg = 0 AND firsttarg = 0 THEN
 IF carray(0) > 1 THEN
  smartarrows -1, 1, bslot(), bat.targ, 0
 END IF
 IF carray(1) > 1 THEN
  smartarrows 1, 1, bslot(), bat.targ, 0
 END IF
 IF carray(2) > 1 THEN
  smartarrows -1, 0, bslot(), bat.targ, spred
 END IF
 IF carray(3) > 1 THEN
  smartarrows 1, 0, bslot(), bat.targ, spred
 END IF
END IF
IF carray(4) > 1 THEN GOSUB gottarg
RETRACE

gottarg: '-----------------------------------------------------------------
bat.targ.selected(bat.targ.pointer) = 1
o = 0
FOR i = 0 TO 11
 IF bat.targ.selected(i) = 1 THEN
  bslot(bat.hero_turn).t(o) = i: o = o + 1
  IF bat.targ.hit_dead THEN bslot(bat.hero_turn).keep_dead_targs(i) = YES
 END IF
NEXT i
ctr(bat.hero_turn) = 0
bslot(bat.hero_turn).ready = NO
bat.hero_turn = -1
bat.targ.mode = targNONE
bat.targ.hit_dead = NO
RETRACE

display:
IF vic.state = 0 THEN 'only display interface till you win
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   IF readbit(gen(), 101, 6) = 0 THEN
    '--speed meter--
    col = uilook(uiTimeBar): IF bslot(i).ready = YES THEN col = uilook(uiTimeBarFull)
    edgeboxstyle 1, 4 + i * 10, 132, 11, 0, dpage, YES, YES
    IF bstat(i).cur.hp > 0 THEN
     j = ctr(i) / 7.7
     IF delay(i) > 0 OR bslot(i).attack > 0 OR (bat.atk.id >= 0 AND bat.acting = i) THEN
      col = uilook(uiTimeBar)
      j = 130
     END IF
     rectangle 2, 5 + i * 10, j, 9, col, dpage
    END IF
   END IF
   IF readbit(gen(), 101, 7) = 0 THEN
    '--hp-meter--
    col = uiLook(uiHealthBar)
    IF lifemeter(i) < INT((87 / large(bstat(i).max.hp, 1)) * bstat(i).cur.hp) THEN lifemeter(i) = lifemeter(i) + 1
    IF lifemeter(i) > INT((87 / large(bstat(i).max.hp, 1)) * bstat(i).cur.hp) THEN lifemeter(i) = lifemeter(i) - 1
    IF lifemeter(i) > 87 THEN
     lifemeter(i) = 87
     col = uiLook(uiHealthBar + tog)
    END IF
    edgeboxstyle 136, 4 + i * 10, 89, 11, 0, dpage, YES, YES
    rectangle 137, 5 + i * 10, lifemeter(i), 9, col, dpage
   END IF
   '--name--
   col = uilook(uiMenuItem): IF i = bat.hero_turn THEN col = uilook(uiSelectedItem + tog)
   edgeprint bslot(i).name, 128 - LEN(bslot(i).name) * 8, 5 + i * 10, col, dpage
   '--hp--
   edgeprint STR$(bstat(i).cur.hp) + "/" + STR$(bstat(i).max.hp), 136, 5 + i * 10, col, dpage
   WITH bstat(i)
    indicatorpos = 217
    'poison indicator
    IF .cur.poison < .max.poison THEN
     edgeprint CHR$(gen(genPoison)), indicatorpos, 5 + i * 10, col, dpage
     indicatorpos -= 8
    END IF
    'stun indicator
    IF .cur.stun < .max.stun THEN
     edgeprint CHR$(gen(genStun)), indicatorpos, 5 + i * 10, col, dpage
     indicatorpos -= 8
    END IF
    'mute indicator
    IF .cur.mute < .max.mute THEN
     edgeprint CHR$(gen(genMute)), indicatorpos, 5 + i * 10, col, dpage
    END IF
   END WITH
  END IF
 NEXT i
 IF battlecaptime > 0 THEN
  battlecaptime = battlecaptime - 1
  IF battlecapdelay > 0 THEN
   battlecapdelay = battlecapdelay - 1
  ELSE
   centerbox 160, 186, 310, 16, 1, dpage
   edgeprint battlecaption, xstring(battlecaption, 160), 181, uilook(uiText), dpage
  END IF
 END IF
 IF bat.hero_turn >= 0 THEN
  centerbox 268, 5 + (4 * (mend(bat.hero_turn) + 2)), 88, 8 * (mend(bat.hero_turn) + 2), 1, dpage
  FOR i = 0 TO mend(bat.hero_turn)
   bg = 0
   fg = uilook(uiMenuItem)
   IF bat.pt = i THEN
     fg = uilook(uiSelectedItem + tog)
     bg = uilook(uiHighlight)
   END IF
   IF readbit(menubits(), 0, bat.hero_turn * 4 + i) THEN
     fg = uilook(uiDisabledItem)
     IF bat.pt = i THEN fg = uilook(uiSelectedDisabled + tog)
   END IF
   textcolor fg, bg
   printstr menu$(bat.hero_turn, i), 228, 9 + i * 8, dpage
  NEXT i
  IF bat.targ.mode = targNONE AND readbit(gen(), genBits, 14) = 0 THEN
   edgeprint CHR$(24), bslot(bat.hero_turn).x + (bslot(bat.hero_turn).w / 2) - 4, bslot(bat.hero_turn).y - 5 + (tog * 2), uilook(uiSelectedItem + tog), dpage
  END IF
  IF bat.menu_mode = batMENUSPELL THEN '--draw spell menu
   centerbox 160, 53, 310, 95, 1, dpage
   IF bat.sptr < 24 THEN
    IF speld$(bat.sptr) <> "" THEN rectangle 5, 74, 311, 1, uilook(uiTextBox + 1), dpage
   END IF
   rectangle 5, 87, 310, 1, uilook(uiTextBox + 1), dpage
   FOR i = 0 TO 23
    textcolor uilook(uiDisabledItem - readbit(spelmask(), 0, i)), 0
    IF bat.sptr = i THEN textcolor uilook(uiSelectedDisabled - (2 * readbit(spelmask(), 0, i)) + tog), uilook(uiHighlight)
    printstr spel$(i), 16 + (i MOD 3) * 104, 8 + (i \ 3) * 8, dpage
   NEXT i
   textcolor uilook(uiMenuItem), 0
   IF bat.sptr = 24 THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
   printstr cancelspell$, 9, 90, dpage
   textcolor uilook(uiDescription), 0
   IF bat.sptr < 24 THEN
    printstr speld$(bat.sptr), 9, 77, dpage
    printstr cost$(bat.sptr), 308 - LEN(cost$(bat.sptr)) * 8, 90, dpage
   END IF
  END IF
  'if keyval(scS) > 1 then gen(genMaxInventory) += 3
  'if keyval(scA) > 1 then gen(genMaxInventory) -= 3
  IF bat.menu_mode = batMENUITEM THEN '--draw item menu
   inv_height = small(78, 8 + INT((last_inv_slot() + 1) / 3) * 8)
   WITH inv_scroll
    .top = INT(bat.item.top / 3)
    .pt = INT(bat.item.pt / 3)
    .last = INT(last_inv_slot() / 3)
   END WITH
   WITH inv_scroll_rect
    .high = inv_height - 10
   END WITH
   edgeboxstyle 8, 4, 304, inv_height, 0, dpage
   draw_scrollbar inv_scroll, inv_scroll_rect, inv_scroll.last, , dpage
   FOR i = bat.item.top TO small(bat.item.top + 26, last_inv_slot())
    if i < lbound(inventory) or i > ubound(inventory) then continue for
    textcolor uilook(uiDisabledItem - readbit(iuse(), 0, i)), 0
    IF bat.item.pt = i THEN textcolor uilook(uiSelectedDisabled - (2 * readbit(iuse(), 0, i)) + tog), uilook(uiHighlight)
    printstr inventory(i).text, 20 + 96 * (i MOD 3), 8 + 8 * ((i - bat.item.top) \ 3), dpage
   NEXT i
   edgeboxstyle 8, 4 + inv_height, 304, 16, 0, dpage
   textcolor uilook(uiDescription), 0
   printstr bat.item_desc, 12, 8 + inv_height, dpage
  END IF
  IF bat.targ.mode > targNONE THEN
   FOR i = 0 TO 11
    IF bat.targ.selected(i) = 1 OR bat.targ.pointer = i THEN
     edgeprint CHR$(24), bslot(i).x + bslot(i).cursorpos.x - 4, bslot(i).y + bslot(i).cursorpos.y - 6, uilook(uiSelectedItem + tog), dpage
     edgeprint bslot(i).name, xstring(bslot(i).name, bslot(i).x + bslot(i).cursorpos.x), bslot(i).y + bslot(i).cursorpos.y - 16, uilook(uiSelectedItem + tog), dpage
    END IF
   NEXT i
  END IF
 END IF
END IF'--end if vic.state = 0
RETRACE

meters:
IF away = 1 THEN RETRACE
'--if a menu is up, and pause-on-menus is ON then no time passes (as long as at least one visible targetable enemy is alive)
isdeepmenu = (bat.menu_mode > 0 AND readbit(gen(), genBits, 0))
isbattlemenu = (bat.menu_mode >= 0 AND bat.hero_turn >= 0 AND readbit(gen(), genBits, 13))
isenemytargs = (targenemycount(bslot(), bstat()) > 0)
IF (isdeepmenu OR isbattlemenu) AND isenemytargs THEN RETRACE

FOR i = 0 TO 11

 'delays for attacks already selected
 IF bat.hero_turn <> i THEN delay(i) = large(delay(i) - 1, 0)

 '--poison
 WITH bstat(i)
  IF .cur.poison < .max.poison THEN
   prtimer(i, 0) = prtimer(i, 0) + large(bstat(i).cur.spd, 7)
   IF prtimer(i, 0) >= 1500 THEN
    prtimer(i, 0) = 0
    harm = .max.poison - .cur.poison
    harm = range(harm, 20)
    quickinflict harm, i, hc(), hx(), hy(), bslot(), harm$(), bstat()
    triggerfade i, bstat(), bslot()
    fulldeathcheck -1, bat, bslot(), bstat(), rew, es(), formdata()
    '--WARNING: WITH pointer probably corrupted
   END IF
  END IF
 END WITH
 
 '--regen
 WITH bstat(i)
  IF .cur.regen < .max.regen THEN
   prtimer(i, 1) = prtimer(i, 1) + large(bstat(i).cur.spd, 7)
   IF prtimer(i, 1) >= 1500 THEN
    prtimer(i, 1) = 0
    heal = .max.regen - .cur.regen
    heal = heal * -1
    heal = range(heal, 20)
    quickinflict heal, i, hc(), hx(), hy(), bslot(), harm$(), bstat()
    triggerfade i, bstat(), bslot()
    fulldeathcheck -1, bat, bslot(), bstat(), rew, es(), formdata()
    '--WARNING: WITH pointer probably corrupted
   END IF
  END IF
 END WITH

 '--if not doing anything, not dying, not ready, and not stunned
 IF bslot(i).attack = 0 AND bslot(i).dissolve = 0 AND bslot(i).ready = NO AND bstat(i).cur.stun = bstat(i).max.stun THEN
  '--increment ctr by speed
  ctr(i) = small(1000, ctr(i) + bstat(i).cur.spd)
  IF ctr(i) = 1000 AND wf = 0 THEN bslot(i).ready = YES
 END IF

NEXT i

'--decrement stun and mute

IF TIMER > laststun + 1 THEN
 FOR i = 0 TO 11
  bstat(i).cur.mute = small(bstat(i).cur.mute + 1, bstat(i).max.mute)
  bstat(i).cur.stun = small(bstat(i).cur.stun + 1, bstat(i).max.stun)
  IF bstat(i).cur.stun < bstat(i).max.stun THEN
   bslot(i).ready = NO
   IF bat.hero_turn = i THEN bat.hero_turn = -1
   IF bat.enemy_turn = i THEN bat.enemy_turn = -1
  END IF
 NEXT i
 laststun = TIMER
END IF

RETRACE

animate:
FOR i = 0 TO 3
 IF walk(i) = 1 THEN bslot(i).frame = bslot(i).frame xor tog
 IF bat.acting <> i AND bstat(i).cur.hp < bstat(i).max.hp / 5 AND vic.state = 0 THEN bslot(i).frame = 6
 IF vic.state > 0 AND bstat(i).cur.hp > 0 AND tog = 0 THEN
  if bslot(i).frame = 0 then bslot(i).frame = 2 else bslot(i).frame = 0
 END IF
NEXT i
FOR i = 0 TO 23
 WITH bslot(i)
  IF .xmov <> 0 THEN .x = .x + (.xspeed * SGN(.xmov)): .xmov = .xmov - SGN(.xmov)
  IF .ymov <> 0 THEN .y = .y + (.yspeed * SGN(.ymov)): .ymov = .ymov - SGN(.ymov)
  IF .zmov <> 0 THEN .z = .z + (.zspeed * SGN(.zmov)): .zmov = .zmov - SGN(.zmov)
 END WITH
NEXT i
FOR i = 0 TO 11
 IF bslot(i + 12).vis = 1 THEN
  fctr(i) = fctr(i) + 1: IF aframe(i, fctr(i)) = -1 THEN fctr(i) = 0
  bslot(i + 12).frame = aframe(i, fctr(i))
  IF atk(2) = 3 THEN
   bslot(i + 12).frame = INT(RND * 3)
  END IF
 END IF
 IF bslot(i).dissolve > 0 THEN
  'ENEMIES DEATH THROES
  IF is_enemy(i) THEN
   IF bslot(i).flee = 0 THEN
    'not running away, normal fade
    FOR ii = 0 TO bslot(i).w * .5
     putpixel INT(RND * (bslot(i).h * bslot(i).w * .5)), 64 + 10 * (i - 4), 0, 3
    NEXT ii
   ELSE
    'running away
    bslot(i).x = bslot(i).x - 10: bslot(i).d = 1
   END IF
   bslot(i).dissolve -= 1
   IF bslot(i).dissolve = 0 THEN
    'formdata((i-4) * 4) = 0 'disabled to fix bug 184
    'make dead enemy invisible (the check_death code will actually do the final removal, which might happen before the enemy has finished dissolving)
    bslot(i).vis = 0
   END IF
  END IF
  IF is_hero(i) THEN bslot(i).frame = 7
 END IF
NEXT i
RETRACE

sprite:
FOR i = 0 TO 24 'set zbuf to 0 through 24
 zbuf(i) = i
NEXT i
FOR o = 1 TO 24
 insertval = zbuf(o)
 searchval = bslot(insertval).y + bslot(insertval).h
 FOR i = o - 1 TO 0 STEP -1
  IF searchval < bslot(zbuf(i)).y + bslot(zbuf(i)).h THEN
   zbuf(i + 1) = zbuf(i)
  ELSE
   EXIT FOR
  END IF
 NEXT
 zbuf(i + 1) = insertval
NEXT
FOR i = 0 TO 24
	IF (bslot(zbuf(i)).vis = 1 OR bslot(zbuf(i)).dissolve > 0) THEN
			dim w as BattleSprite ptr
			w = @bslot(zbuf(i))
			with bslot(zbuf(i))
				dim spr as frame ptr, custspr as integer
				custspr = 0
				spr = .sprites
				
				if .sprites = 0 then continue for
				
				'debug(str(zbuf(i)))
				
				if .frame < .sprite_num then spr += .frame
				
				if .d then
					spr = sprite_flip_horiz(spr)
					custspr = -1
				end if
				
				if is_enemy(zbuf(i)) and .dissolve > 0 and .flee = 0 then
					dim as integer dtype, dtime
					if .deathtype = 0 then dtype = gen(genEnemyDissolve) else dtype = .deathtype - 1
					if .deathtime = 0 then dtime = spr->w/2 else dtime = .deathtime
					spr = sprite_dissolve(spr,dtime,dtime - .dissolve, dtype, custspr)
					custspr = -1
				end if
				
				sprite_draw(spr, .pal, .x, .y - .z, 1, -1, dpage)
				
				if custspr then
					sprite_unload(@spr)
				end if
			end with
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
RETRACE

seestuff:
FOR i = 0 TO 11
 c = uilook(uiSelectedDisabled)
 IF is_hero(i) THEN c = uilook(uiSelectedItem)
 rectangle 0, 80 + (i * 10), ctr(i) / 10, 4, c, dpage
 IF is_enemy(i) THEN edgeprint XSTR$(es(i - 4, 82)), 0, 80 + i * 10, c, dpage
 info$ = "v=" & bslot(i).vis & " dly=" & delay(i) & " tm=" & bat.targ.mask(i) & " hp=" & bstat(i).cur.hp & " dis=" & bslot(i).dissolve
 IF is_enemy(i) THEN  info$ = info$ & " fm=" & formdata((i-4)*4) 
 edgeprint info$, 20, 80 + i * 10, c, dpage
NEXT i
RETRACE

tryrun:
IF flee > 0 AND flee < 4 THEN
 IF carray(6) = 0 THEN
  flee = 0
  FOR i = 0 TO 3
   bslot(i).d = 0
   walk(i) = 0
  NEXT i
 END IF
END IF
IF flee = 4 THEN
 IF checkNoRunBit(bstat(), bslot()) THEN
  flee = 0
  alert$ = cannotrun$
  alert = 10
 END IF
END IF
IF flee > 4 THEN
 FOR i = 0 TO 3
  '--if alive turn around
  IF bstat(i).cur.hp THEN bslot(i).d = 1
  walk(i) = 1
  bslot(i).attack = 0
  bslot(i).ready = NO
  ctr(i) = large(0, ctr(i) - bstat(i).cur.spd * 2)
 NEXT i
 IF carray(6) = 0 THEN flee = 0: FOR i = 0 TO 3: bslot(i).d = 0: walk(i) = 0: NEXT i
 temp = 400
 FOR i = 4 TO 11
  temp = temp + bstat(i).cur.spd
 NEXT i
 IF RND * temp < flee THEN away = 1: flee = 2: FOR i = 0 TO 3: ctr(i) = 0: bslot(i).ready = NO: NEXT i
END IF
RETRACE

loadall:
setpicstuf formdata(), 80, -1
loadset tmpdir & "for.tmp", form, 0

for i = 0 to 24
	bslot(i).frame = 0
	bslot(i).sprites = 0
	bslot(i).pal = 0
	bslot(i).attack = 0
next

mapsong = presentsong
IF formdata(33) > 0 THEN wrappedsong formdata(33) - 1
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  loadherodata @st(i), hero(i) - 1
  oldm = 0
  newm = 0
  'Loop through hero battle menu, populating nmenu() with the ones that should be displayed
  FOR oldm = 0 TO 5
   IF bmenu(i, oldm) < 0 AND bmenu(i, oldm) > -5 AND readbit(st(i).bits(),0,26) <> 0 THEN
    'this is a spell list, and the hide empty spell lists bitset is on...
    temp = ABS(bmenu(i, oldm)) - 1
    'count the spells, and skip if empty
    IF count_available_spells(i, temp) = 0 THEN CONTINUE FOR
   END IF
   nmenu(i,newm) = bmenu(i,oldm)
   newm += 1
  NEXT oldm
  
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
   .sprites = sprite_load(game & ".pt0", exstat(i, 0, 14), .sprite_num, 32, 40)
   if not sprite_is_valid(.sprites) then debug "Couldn't load hero sprite: " & game & ".pt0#" & exstat(i,0,14)
   .pal = palette16_load(game + ".pal", exstat(i, 0, 15), 0, exstat(i, 0, 14))
   if .pal = 0 then debug "Failed to load palette (#" & i & ")"
   .frame = 0
   .death_sfx = -1 'No death sounds for heroes (for now)
   .cursorpos.x = .w / 2
   .cursorpos.y = 0
  END WITH
  FOR o = 0 TO 11
   bstat(i).cur.sta(o) = exstat(i, 0, o)
   bstat(i).max.sta(o) = exstat(i, 1, o)
  NEXT o
  herobattlebits bslot(), i
  bslot(i).name = names(i)
  FOR o = 0 TO 5
   menu$(i, o) = ""
   IF nmenu(i, o) > 0 THEN
    loadattackdata atk(), nmenu(i, o) - 1
    menu$(i, o) = readbadbinstring$(atk(), 24, 10, 1)
   END IF
   IF nmenu(i, o) < 0 AND nmenu(i, o) > -5 THEN
    temp = (nmenu(i, o) + 1) * -1
    menu$(i,o) = st(i).list_name(temp)
   END IF
   IF nmenu(i, o) = -10 THEN menu$(i, o) = readglobalstring$(34, "Item", 10): mend(i) = o
   WHILE LEN(menu$(i, o)) < 10: menu$(i, o) = menu$(i, o) + " ": WEND
  NEXT o

  'wipe spells learnt and levels gained
  FOR o = i * 6 TO i * 6 + 5
   learnmask(o) = 0
  NEXT
  exstat(i, 1, 12) = 0
 ELSE
  bslot(i).sprites = 0
 END IF
NEXT i
FOR i = 0 TO 7
 loadfoe i, formdata(), es(), bat, bslot(), bstat(), rew, YES
NEXT i
FOR i = 0 TO 11
 ctr(i) = INT(RND * 500)
 bslot(i).t(12) = -1
NEXT i
FOR i = 12 TO 23
 bslot(i).w = 50
 bslot(i).h = 50
NEXT i
curbg = formdata(32)
loadpage game + ".mxs", curbg, 2
FOR i = 0 TO 3
 IF bstat(i).cur.hp < bstat(i).max.hp / 5 AND vic.state = 0 THEN bslot(i).frame = 6
 IF hero(i) > 0 AND bstat(i).cur.hp = 0 THEN
  '--hero starts the battle dead
  bslot(i).dissolve = 1 'Keeps the dead hero from vanishing
  bslot(i).frame = 7
 END IF
 lifemeter(i) = (88 / large(bstat(i).max.hp, 1)) * bstat(i).cur.hp
NEXT i
bslot(24).w = 24
bslot(24).h = 24
'trigger fades for dead enemies
'fulldeathcheck fades out only enemies set to die without a boss
'so additionally call triggerfade on 0 hp enemies here
'or might that be expected behaviour in some games?
FOR i = 0 TO 7
 IF bstat(i).cur.hp <= 0 THEN
  triggerfade i, bstat(), bslot()
 END IF
NEXT i
fulldeathcheck -1, bat, bslot(), bstat(), rew, es(), formdata()
RETRACE

END FUNCTION

'FIXME: This affects the rest of the file. Move it up as above functions are cleaned up
OPTION EXPLICIT

SUB fulldeathcheck (killing_attack AS INTEGER, bat AS BattleState, bslot() AS BattleSprite, bstat() As BattleStats, rew AS RewardsState, es() AS INTEGER, formdata() AS INTEGER)
 '--Runs check_death on all enemies, checks all heroes for death, and sets bat.death_mode if necessary
 'killing_attack is the attack ID that was just used, or -1 for none
 DIM deadguy AS INTEGER
 DIM dead_enemies AS INTEGER
 DIM dead_heroes AS INTEGER
 FOR deadguy = 4 TO 11
  IF bstat(deadguy).cur.hp > 0 THEN
   'this enemy hasn't just spawned; it should fade out
   IF dieWOboss(deadguy, bstat(), bslot()) THEN
    triggerfade deadguy, bstat(), bslot()
   END IF
  END IF
 NEXT
 FOR deadguy = 0 TO 11
  check_death deadguy, killing_attack, bat, rew, bstat(), bslot(), es(), formdata()
 NEXT
 dead_enemies = 0
 FOR deadguy = 4 TO 11
  IF bstat(deadguy).cur.hp = 0 OR bslot(deadguy).hero_untargetable = YES OR bslot(deadguy).death_unneeded = YES THEN dead_enemies += 1
 NEXT
 IF dead_enemies >= 8 THEN bat.death_mode = deathENEMIES
 dead_heroes = 0
 FOR deadguy = 0 TO 3
  IF bstat(deadguy).cur.hp = 0 THEN dead_heroes += 1
 NEXT deadguy
 IF dead_heroes = 4 THEN bat.death_mode = deathHEROES
END SUB

SUB trigger_victory(BYREF vic AS VictoryState, BYREF rew AS RewardsState, bstat() As BattleStats, exstat() AS INTEGER)
 DIM AS INTEGER i, numheroes
 '--Play the victory music
 IF gen(genVictMus) > 0 THEN wrappedsong gen(genVictMus) - 1
 '--Collect gold (and cap out at 1 billion max)
 gold = gold + rew.plunder
 IF gold > 1000000000 THEN gold = 1000000000
 '--Divide experience by heroes
 IF readbit(gen(), genBits2, 3) THEN 'dead heroes get experience
  numheroes = herocount()
 ELSE
  numheroes = liveherocount(bstat())
 END IF
 IF numheroes > 0 THEN rew.exper = rew.exper / numheroes
 '--Collect experience and apply levelups
 FOR i = 0 TO 3
  IF readbit(gen(), genBits2, 3) <> 0 OR bstat(i).cur.hp > 0 THEN giveheroexperience i, exstat(), rew.exper
  updatestatslevelup i, exstat(), bstat(), 0
 NEXT i
 '--Trigger the display of end-of-battle rewards
 vic.state = vicGOLDEXP
END SUB

SUB show_victory (BYREF vic AS VictoryState, BYREF rew AS RewardsState, exstat() AS INTEGER, bslot() AS BattleSprite)
DIM tempstr AS STRING
DIM AS INTEGER i, o
IF vic.box THEN centerfuz 160, 30, 280, 50, 1, dpage
SELECT CASE vic.state
 CASE vicGOLDEXP
  '--print acquired gold and experience
  IF rew.plunder > 0 OR rew.exper > 0 THEN vic.box = YES: centerfuz 160, 30, 280, 50, 1, dpage
  IF rew.plunder > 0 THEN
   tempstr = vic.gold_caption & " " & rew.plunder & " " & vic.gold_name & "!"
   edgeprint tempstr, xstring(tempstr, 160), 16, uilook(uiText), dpage
  END IF
  IF rew.exper > 0 THEN
   tempstr = vic.exp_caption & " " & rew.exper & " " & vic.exp_name & "!"
   edgeprint tempstr, xstring(tempstr, 160), 28, uilook(uiText), dpage
  END IF
  IF carray(4) > 1 OR carray(5) > 1 OR (rew.plunder = 0 AND rew.exper = 0) THEN
   vic.state = vicLEVELUP
  END IF
 CASE vicLEVELUP
  '--print levelups
  o = 0
  FOR i = 0 TO 3
   IF o = 0 AND exstat(i, 1, 12) THEN centerfuz 160, 30, 280, 50, 1, dpage: vic.box = YES
   SELECT CASE exstat(i, 1, 12)
    CASE 1
     tempstr = vic.level_up_caption & " " & bslot(i).name
    CASE IS > 1
     tempstr = exstat(i, 1, 12) & " " & vic.levels_up_caption & " " & bslot(i).name
   END SELECT
   IF exstat(i, 1, 12) > 0 THEN
    edgeprint tempstr, xstring(tempstr, 160), 12 + i * 10, uilook(uiText), dpage
    o = 1
   END IF
  NEXT i
  IF o = 0 OR (carray(4) > 1 OR carray(5) > 1) THEN
   vic.state = vicSPELLS
   vic.showlearn = NO
   vic.learnwho = 0
   vic.learnlist = 0
   vic.learnslot = -1
  END IF
 CASE vicSPELLS
  '--print learned spells, one at a time
  IF vic.showlearn = NO THEN '--Not showing a spell yet. find the next one
   DO
    vic.learnslot = vic.learnslot + 1
    IF vic.learnslot > 23 THEN vic.learnslot = 0: vic.learnlist = vic.learnlist + 1
    IF vic.learnlist > 3 THEN vic.learnlist = 0: vic.learnwho = vic.learnwho + 1
    IF vic.learnwho > 3 THEN ' We have iterated through all spell lists for all heroes, time to move on
     vic.state = vicITEMS
     vic.item_name = ""
     vic.found_index = 0
     vic.box = NO
     EXIT DO
    END IF
    IF readbit(learnmask(), 0, vic.learnwho * 96 + vic.learnlist * 24 + vic.learnslot) THEN
     'found a learned spell
     vic.item_name = bslot(vic.learnwho).name + vic.learned_caption
     vic.item_name = vic.item_name & readattackname$(spell(vic.learnwho, vic.learnlist, vic.learnslot) -1)
     vic.showlearn = YES
     vic.box = YES
     EXIT DO
    END IF
   LOOP
  ELSE' Found a learned spell to display, show it until a keypress
   IF carray(4) > 1 OR carray(5) > 1 THEN
    vic.showlearn = NO ' hide the display (which causes us to search for the next learned spell)
   END IF
   edgeprint vic.item_name, xstring(vic.item_name, 160), 22, uilook(uiText), dpage
  END IF
 CASE vicITEMS
  '--print found items, one at a time
  '--check to see if we are currently displaying a gotten item
  IF vic.item_name = "" THEN
   '--if not, check to see if there are any more gotten items to display
   IF rew.found(vic.found_index).num = 0 THEN vic.state = vicEXITDELAY: EXIT SUB
   '--get the item name
   vic.item_name = readitemname$(rew.found(vic.found_index).id)
   '--actually aquire the item
   getitem rew.found(vic.found_index).id + 1, rew.found(vic.found_index).num
  END IF
  '--if the present item is gotten, show the caption
  IF rew.found(vic.found_index).num = 1 THEN
   tempstr = vic.item_caption & " " & vic.item_name
  ELSE
   tempstr = vic.plural_item_caption & " " & rew.found(vic.found_index).num & " " & vic.item_name
  END IF
  IF LEN(tempstr) THEN centerfuz 160, 30, 280, 50, 1, dpage
  edgeprint tempstr, xstring(tempstr, 160), 22, uilook(uiText), dpage
  '--check for a keypress
  IF carray(4) > 1 OR carray(5) > 1 THEN
   IF rew.found(vic.found_index).num = 0 THEN
    '--if there are no further items, exit
    vic.state = -1
   ELSE
    '--otherwize, increment the findpointer and reset the caption
    vic.found_index = vic.found_index + 1: vic.item_name = ""
   END IF
  END IF
END SELECT
END SUB

SUB reset_battle_state (BYREF bat AS BattleState)
 'This could become a constructor for BattleState when we support the -lang fb dialect
 WITH bat
  .acting = 0
  .hero_turn = -1
  .enemy_turn = -1
  .next_hero = 0
  .next_enemy = 0
  .menu_mode = batMENUHERO
 END WITH
 reset_targetting bat
 reset_attack bat
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

SUB checkitemusability(iuse() AS INTEGER, bstat() AS BattleStats, who AS INTEGER)
 'Iterate through the iuse() bitfield and mark any items that are usable
 DIM i AS INTEGER
 DIM itembuf(99) AS INTEGER
 DIM atkbuf(40 + dimbinsize(binATTACK)) AS INTEGER

 FOR i = 0 TO inventoryMax
  setbit iuse(), 0, i, 0 ' Default each slot to unusable
  IF inventory(i).used THEN
   loaditemdata itembuf(), inventory(i).id
   IF itembuf(47) > 0 THEN ' This item is usable in battle
    loadattackdata atkbuf(), itembuf(47) - 1
    IF readbit(atkbuf(), 65, 16) THEN
     '--This attack has the bitset that requires cost checking when used from an item
     IF atkallowed(atkbuf(), who, 0, 0, bstat()) THEN
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

FUNCTION checkNoRunBit (bstat() AS BattleStats, bslot() AS BattleSprite) as integer
 DIM i AS INTEGER
 FOR i = 4 TO 11
  IF bstat(i).cur.hp > 0 AND bslot(i).vis = 1 AND bslot(i).unescapable = YES THEN RETURN 1
 NEXT i
 RETURN 0
END FUNCTION

SUB checkTagCond (t, check, tg, tagand)
 't - type, check = curtype, tg - the tag to be set, tagand - the tag to check
 IF t = check THEN
  IF tagand <> 0 AND readbit(tag(), 0, ABS(tagand)) <> SGN(SGN(tagand) + 1) THEN EXIT SUB
  setbit tag(), 0, ABS(tg), SGN(SGN(tg) + 1) 'Set the original damned tag!
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

'--merge equipment bits
FOR j = 0 TO 4
 IF eqstuf(who, j) > 0 THEN
  loaditemdata buffer(), eqstuf(who, j) - 1
  FOR i = 0 TO 4
   bitbuf(who, i) = (bitbuf(who, i) OR buffer(70 + i))
  NEXT i
 END IF
NEXT j

END SUB

SUB herobattlebits (bslot() AS BattleSprite, who)
DIM i AS INTEGER
DIM bitbuf(11, 4) AS INTEGER ' Temporary buffer for getting bits. FIXME: remove this eventually
DIM tempbits(1) AS INTEGER  ' Temporary buffer for reading bits

herobattlebits_raw bitbuf(), who

'--Copy elemental bits to bslot()
tempbits(0) = bitbuf(who, 0)
tempbits(1) = bitbuf(who, 1)
FOR i = 0 TO 7
 WITH bslot(who)
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

SUB quickinflict (harm, targ, hc(), hx(), hy(), bslot() AS BattleSprite, harm$(), bstat() AS BattleStats)
'--quick damage infliction to hp. no bells and whistles
DIM max_bound AS INTEGER
hc(targ) = 7
hx(targ) = bslot(targ).x + (bslot(targ).w * .5)
hy(targ) = bslot(targ).y + (bslot(targ).h * .5)
IF harm < 0 THEN
 harm$(targ) = "+" + STR$(ABS(harm))
ELSE
 harm$(targ) = STR$(harm)
END IF

if gen(genDamageCap) > 0 THEN harm = small(harm, gen(genDamageCap))

max_bound = large(bstat(targ).cur.hp, bstat(targ).max.hp)
bstat(targ).cur.hp = bound(bstat(targ).cur.hp - harm, 0, max_bound)
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

SUB anim_flinchstart(who AS INTEGER, bslot() AS BattleSprite, atk() AS INTEGER)
 '--If enemy can flinch and if attack allows flinching
 IF bslot(who).never_flinch = NO AND readbit(atk(), 65, 18) = NO THEN
  DIM flinch_x_dist AS INTEGER
  flinch_x_dist = 3
  IF is_enemy(who) THEN flinch_x_dist = -3
  anim_setmove who, flinch_x_dist, 0, 2, 0
  IF is_hero(who) THEN
   IF readbit(atk(), 20, 0) = 0 THEN
    '--Show Harmed frame
    anim_setframe who, 5
   ELSE
    '--Show attack1 frame when healed
    anim_setframe who, 2
   END IF
  END IF
 END IF
END SUB

SUB anim_flinchdone(who AS INTEGER, bslot() AS BattleSprite, atk() AS INTEGER)
 '--If enemy can flinch and if attack allows flinching
 IF bslot(who).never_flinch = NO AND readbit(atk(), 65, 18) = NO THEN
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

SUB spawn_on_death(deadguy AS INTEGER, killing_attack AS INTEGER, BYREF bat AS BattleState, es(), formdata(), bslot() AS BattleSprite, bstat() AS BattleStats, BYREF rew AS RewardsState)
 'killing_attack is the id of the attack that killed the target or -1 if the target died without a specific attack
 DIM atkbuf(40 + dimbinsize(binATTACK))
 DIM slot AS INTEGER
 DIM i AS INTEGER
 IF NOT is_enemy(deadguy) THEN EXIT SUB ' Only works for enemies
 IF killing_attack >= 0 THEN
  'This death is the result of an attack
  loadattackdata atkbuf(), killing_attack
  IF readbit(atkbuf(), 65, 15) <> 0 THEN
   'This attack had the "Do not trigger spawning on death" bitset
   EXIT SUB
  END IF
 END IF
 IF es(deadguy - 4, 80) > 0 AND bat.atk.non_elemental = YES THEN ' spawn on non-elemental death
  FOR i = 1 TO es(deadguy - 4, 91)
   slot = find_empty_enemy_slot(formdata())
   IF slot > -1 THEN
    formdata(slot * 4) = es(deadguy - 4, 80)
    loadfoe(slot, formdata(), es(), bat, bslot(), bstat(), rew)
   END IF
  NEXT i
  es(deadguy - 4, 80) = 0
 END IF
 IF es(deadguy - 4, 79) > 0 THEN ' spawn on death
  FOR i = 1 TO es(deadguy - 4, 91)
   slot = find_empty_enemy_slot(formdata())
   IF slot > -1 THEN
    formdata(slot * 4) = es(deadguy - 4, 79)
    loadfoe(slot, formdata(), es(), bat, bslot(), bstat(), rew)
   END IF
  NEXT i
  es(deadguy - 4, 79) = 0
 END IF
END SUB

FUNCTION find_empty_enemy_slot(formdata() AS INTEGER) AS INTEGER
 'Returns index of first empty slot, or -1 if none was found
 DIM i AS INTEGER
 FOR i = 0 TO 7
  IF formdata(i * 4) = 0 THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION dieWOboss(BYVAL who, bstat() AS BattleStats, bslot() AS BattleSprite) as integer
 DIM AS INTEGER j
 '--count bosses
 FOR j = 4 TO 11
  '--is it a boss?
  IF bslot(j).is_boss = YES THEN
   '-- is it alive?
   IF bstat(j).cur.hp > 0 THEN
    RETURN NO
   END IF
  END IF
 NEXT j
 '--if there are no bossess...
 '--should it die without a boss?
 IF bslot(who).die_without_boss = YES THEN
  bstat(who).cur.hp = 0
  RETURN YES
 END IF
END FUNCTION

SUB triggerfade(BYVAL who, bstat() AS BattleStats, bslot() AS BattleSprite)
 'If the target is really dead...
 IF bstat(who).cur.hp = 0 THEN
  'the number of ticks it takes the enemy to fade away is equal to half its width
  'bslot(who).dissolve = bslot(who).w * .5
  if bslot(who).deathtime = 0 then
   bslot(who).dissolve = bslot(who).w / 2
  else
   bslot(who).dissolve = bslot(who).deathtime
  end if
  IF is_enemy(who) THEN
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

SUB check_death(deadguy AS INTEGER, BYVAL killing_attack AS INTEGER, BYREF bat AS BattleState, BYREF rew AS RewardsState, bstat() AS BattleStats, bslot() AS BattleSprite, es(), formdata())
'killing_attack is not used yet, but will contain attack id or -1 when no attack is relevant.
 DIM AS INTEGER j,k 'for loop counters

 IF is_enemy(deadguy) THEN
  IF formdata((deadguy - 4) * 4) = 0 THEN EXIT SUB
 END IF
 IF bstat(deadguy).cur.hp <> 0 THEN EXIT SUB
 '--deadguy is really dead (includes already dead and empty hero slots??)
 'Death animation is not done yet here, so be cautios about what gets cleand up here.
 'Full cleanup of bslot() records belongs in loadfoe
 bslot(deadguy).vis = 0
 bslot(deadguy).ready = NO
 bslot(deadguy).attack = 0
 bslot(deadguy).d = 0
 '--reset poison/regen/stun/mute
 WITH bstat(deadguy)
  .cur.poison = .max.poison
  .cur.regen  = .max.regen
  .cur.stun   = .max.stun
  .cur.mute   = .max.mute
 END WITH
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
  dead_enemy deadguy, killing_attack, bat, rew, bstat(), bslot(), es(), formdata()
 END IF'------------END PLUNDER-------------------
 IF bat.targ.hit_dead = NO THEN '---THIS IS NOT DONE FOR ALLY+DEAD------
  FOR j = 0 TO 11
   '--Search through each hero and enemy to see if any of them are targetting the
   '--guy who just died
   bslot(j).t(12) = -1
   FOR k = 0 TO 11
    '--sort dead target away
    IF bslot(j).t(k) = deadguy AND bslot(j).keep_dead_targs(deadguy) = NO THEN SWAP bslot(j).t(k), bslot(j).t(k + 1)
   NEXT k
   IF bslot(j).t(0) = -1 AND bat.acting <> j AND bslot(j).attack > 0 THEN
    'if no targets left, auto-re-target
    loadattackdata buffer(), bslot(j).attack - 1
    autotarget j, buffer(), bslot(), bstat()
   END IF
   IF bat.targ.mask(deadguy) = 1 THEN bat.targ.mask(deadguy) = 0
   IF bat.targ.selected(deadguy) = 1 THEN bat.targ.selected(deadguy) = 0
  NEXT j
  IF bat.targ.pointer = deadguy THEN
   WHILE bat.targ.mask(bat.targ.pointer) = 0
    bat.targ.pointer = bat.targ.pointer + 1: IF bat.targ.pointer > 11 THEN bat.targ.mode = targNONE: EXIT SUB
   WEND
  END IF
 END IF  '----END ONLY WHEN bat.targ.hit_dead = NO
END SUB

SUB dead_enemy(deadguy AS INTEGER, killing_attack AS INTEGER, BYREF bat AS BattleState, BYREF rew AS RewardsState, bstat() AS BattleStats, bslot() AS BattleSprite, es(), formdata())
 '--give rewards, spawn enemies, clear formdata slot, but NO other cleanup!
 'killing_attack is the id of the attack that killed the target or -1 if the target died without a specific attack
 DIM AS INTEGER j
 DIM enemynum AS INTEGER = deadguy - 4
 '--spawn enemies before freeing the formdata slot to avoid infinite loops. however this might need to be changed to fix morphing enemies?
 spawn_on_death deadguy, killing_attack, bat, es(), formdata(), bslot(), bstat(), rew
 IF formdata(enemynum * 4) > 0 THEN
  rew.plunder = rew.plunder + es(enemynum, 56)
  IF rew.plunder > 1000000000 THEN rew.plunder = 1000000000
  rew.exper = rew.exper + es(enemynum, 57)
  IF rew.exper > 1000000 THEN rew.exper = 1000000
  IF INT(RND * 100) < es(enemynum, 59) THEN '---GET ITEMS FROM FOES-----
   FOR j = 0 TO 16
    IF rew.found(j).num = 0 THEN rew.found(j).id = es(enemynum, 58): rew.found(j).num = 1: EXIT FOR
    IF rew.found(j).id = es(enemynum, 58) THEN rew.found(j).num = rew.found(j).num + 1: EXIT FOR
   NEXT j
  ELSE '------END NORMAL ITEM---------------
   IF INT(RND * 100) < es(enemynum, 61) THEN
    FOR j = 0 TO 16
     IF rew.found(j).num = 0 THEN rew.found(j).id = es(enemynum, 60): rew.found(j).num = 1: EXIT FOR
     IF rew.found(j).id = es(enemynum, 60) THEN rew.found(j).num = rew.found(j).num + 1: EXIT FOR
    NEXT j
   END IF '---END RARE ITEM-------------
  END IF '----END GET ITEMS----------------
 END IF
 ' remove dead enemy from formation
 formdata(enemynum * 4) = 0
END SUB

SUB enemy_ai (BYREF bat AS BattleState, bstat() AS BattleStats, bslot() AS BattleSprite, es() AS INTEGER, formdata() AS INTEGER, BYREF rew AS RewardsState, ctr() AS INTEGER, delay() AS INTEGER)
 DIM ai AS INTEGER = 0

 'if HP is less than 20% go into desperation mode
 IF bstat(bat.enemy_turn).cur.hp < bstat(bat.enemy_turn).max.hp / 5 THEN ai = 1

 'if targetable enemy count is 1, go into alone mode
 IF targenemycount(bslot(), bstat(), YES) = 1 THEN ai = 2

 DIM slot AS INTEGER = 0
 'spawn allies when alone
 IF ai = 2 AND es(bat.enemy_turn - 4, 81) THEN
  FOR j AS INTEGER = 1 TO es(bat.enemy_turn - 4, 91)
   slot = find_empty_enemy_slot(formdata())
   IF slot > -1 THEN
    formdata(slot * 4) = es(bat.enemy_turn - 4, 81)
    loadfoe slot, formdata(), es(), bat, bslot(), bstat(), rew
   END IF
  NEXT j
 END IF

 'make sure that the current ai set is valid
 'otherwise fall back on another
 IF countai(ai, bat.enemy_turn, es()) = 0 THEN
  ai = 0
  IF bstat(bat.enemy_turn).cur.hp < bstat(bat.enemy_turn).max.hp / 5 THEN
   ai = 1
   IF countai(ai, bat.enemy_turn, es()) = 0 THEN ai = 0
  END IF
 END IF

 'if no valid ai set is available, the enemy loses its turn
 IF countai(ai, bat.enemy_turn, es()) = 0 THEN bat.enemy_turn = -1 : EXIT SUB

 'pick a random attack
 DIM atk AS AttackData
 DIM safety AS INTEGER = 0
 DO
  bslot(bat.enemy_turn).attack = es(bat.enemy_turn - 4, 92 + (ai * 5) + INT(RND * 5))
  IF bslot(bat.enemy_turn).attack > 0 THEN
   'load the data for this attack
   loadattackdata atk, bslot(bat.enemy_turn).attack - 1
  
   IF atkallowed(atk, bat.enemy_turn, 0, 0, bstat()) THEN
    'this attack is good, continue on to target selection
    EXIT DO
   ELSE
    'this attack is unusable
    bslot(bat.enemy_turn).attack = 0
    IF bstat(bat.enemy_turn).cur.mp - atk.mp_cost < 0 THEN
     'inadequate MP was the reason for the failure
     'MP-idiot loses its turn
     IF bslot(bat.enemy_turn).mp_idiot = YES THEN
       bslot(bat.enemy_turn).ready = NO
       ctr(bat.enemy_turn) = 0
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

 'get the delay to wait for this attack
 delay(bat.enemy_turn) = atk.attack_delay

 autotarget bat.enemy_turn, atk, bslot(), bstat()

 'ready for next attack
 bslot(bat.enemy_turn).ready = NO
 ctr(bat.enemy_turn) = 0
 bat.enemy_turn = -1

END SUB

SUB heromenu (BYREF bat AS BattleState, bslot() AS BattleSprite, bstat() AS BattleStats, menubits() AS INTEGER, nmenu() AS INTEGER, mend() AS INTEGER, delay() AS INTEGER, spel$(), speld$(), cost$(), spel(), spelmask(), iuse(), st() as herodef)

 DIM mp_name AS STRING = readglobalstring(1, "MP", 10)
     
 DIM atk AS AttackData
 DIM wepatkid AS INTEGER = 0
 DIM spellcount AS INTEGER = 0
 DIM i AS INTEGER
 
 FOR i = 0 TO 5
  setbit menubits(), 0, bat.hero_turn * 4 + i, 0
  IF nmenu(bat.hero_turn, i) > 0 THEN
   IF wepatkid <> nmenu(bat.hero_turn, i) THEN
    wepatkid = nmenu(bat.hero_turn, i)
    loadattackdata atk, wepatkid - 1
   END IF
   IF atk.check_costs_as_weapon THEN
    IF atkallowed(atk, bat.hero_turn, 0, 0, bstat()) = 0 THEN
     setbit menubits(), 0, bat.hero_turn * 4 + i, 1
    END IF
   END IF
  END IF
 NEXT i
 
 IF carray(5) > 1 THEN
  '--skip turn
  bat.next_hero = bat.hero_turn
  bat.hero_turn = -1
  EXIT SUB
 END IF
 IF carray(0) > 1 THEN
  '--up
  bat.pt -= 1
  IF bat.pt < 0 THEN bat.pt = mend(bat.hero_turn)
 END IF
 IF carray(1) > 1 THEN
  bat.pt += 1
  IF bat.pt > mend(bat.hero_turn) THEN bat.pt = 0
 END IF
 
 IF carray(4) > 1 THEN
  '--use menu item
  IF nmenu(bat.hero_turn, bat.pt) > 0 THEN 'simple attack
   IF readbit(menubits(), 0, bat.hero_turn * 4 + bat.pt) = 0 THEN
    bslot(bat.hero_turn).attack = nmenu(bat.hero_turn, bat.pt)
    loadattackdata atk, bslot(bat.hero_turn).attack - 1
    delay(bat.hero_turn) = large(atk.attack_delay, 1)
    bat.targ.mode = targSETUP
    flusharray carray(), 7, 0
    EXIT SUB
   END IF
  END IF
  IF nmenu(bat.hero_turn, bat.pt) < 0 AND nmenu(bat.hero_turn, bat.pt) >= -4 THEN '--this is a spell list
   bat.listslot = (nmenu(bat.hero_turn, bat.pt) + 1) * -1
   IF st(bat.hero_turn).list_type(bat.listslot) < 2 THEN '--the type of this spell list is one that displays a menu
    '--init spell menu
    bat.menu_mode = batMENUSPELL
    bat.sptr = 0
    FOR i = 0 TO 23 '-- loop through the spell list setting up menu items for each
     spel$(i) = ""
     speld$(i) = ""
     cost$(i) = ""
     spel(i) = -1
     setbit spelmask(), 0, i, 0
     IF spell(bat.hero_turn, bat.listslot, i) > 0 THEN '--there is a spell in this slot
      spellcount += 1
      spel(i) = spell(bat.hero_turn, bat.listslot, i) - 1
      loadattackdata atk, spel(i)
      spel$(i) = atk.name
      speld$(i) = atk.description
      IF st(bat.hero_turn).list_type(bat.listslot) = 0 THEN
       '--regular MP
       cost$(i) = " " & focuscost(atk.mp_cost, bstat(bat.hero_turn).cur.foc) & " " & mp_name & " " & bstat(bat.hero_turn).cur.mp & "/" & bstat(bat.hero_turn).max.mp
      END IF
      IF st(bat.hero_turn).list_type(bat.listslot) = 1 THEN
       '--level MP
       cost$(i) = "Level " & (INT(i / 3) + 1) & ":   " & lmp(bat.hero_turn, INT(i / 3))
      END IF
      IF atkallowed(atk, bat.hero_turn, st(bat.hero_turn).list_type(bat.listslot), INT(i / 3), bstat()) THEN
       '-- check whether or not the spell is allowed
       setbit spelmask(), 0, i, 1
      END IF
     END IF
     spel$(i) = rpad(spel$(i), " ", 10) '-- pad the spell menu caption
    NEXT i
   ELSEIF st(bat.hero_turn).list_type(bat.listslot) = 2 THEN '-- this is a random spell list
    spellcount = 0
    FOR i = 0 TO 23 '-- loop through the spell list storing attack ID numbers
     spel(i) = -1
     IF spell(bat.hero_turn, bat.listslot, i) > 0 THEN '--there is a spell in this slot
      spellcount += 1
      spel(i) = spell(bat.hero_turn, bat.listslot, i) - 1
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
      LOOP UNTIL spel(rptr) > -1 OR safety > 999 '--loop until we have found a spell (or give up after 999 tries)
     NEXT i
     bslot(bat.hero_turn).attack = spel(rptr) + 1
     loadattackdata atk, bslot(bat.hero_turn).attack - 1
     delay(bat.hero_turn) = large(atk.attack_delay, 1)
     bat.targ.mode = targSETUP
     flusharray carray(), 7, 0
    END IF
   END IF
  ELSEIF nmenu(bat.hero_turn, bat.pt) = -10 THEN '--items menu
   bat.menu_mode = batMENUITEM
   bat.item.pt = 0
   bat.item.top = 0
   checkitemusability iuse(), bstat(), bat.hero_turn
   bat.item_desc = ""
   IF inventory(bat.item.pt).used THEN
    loaditemdata buffer(), inventory(bat.item.pt).id
    bat.item_desc = readbadbinstring$(buffer(), 9, 35, 0)
   END IF
  END IF
 END IF
END SUB

SUB spellmenu (BYREF bat AS BattleState, spel(), st() as HeroDef, bstat() AS BattleStats, bslot() AS BattleSprite, delay(), conlmp())
 IF carray(5) > 1 THEN '--cancel
  bat.menu_mode = batMENUHERO
  flusharray carray(), 7, 0
  EXIT SUB
 END IF
 
 WITH bat
  IF carray(0) > 1 THEN
   IF .sptr > 2 THEN .sptr -= 3 ELSE .sptr = 24
  END IF
  IF carray(1) > 1 THEN
   IF .sptr < 24 THEN .sptr = small(.sptr + 3, 24) ELSE .sptr = 0
  END IF
  IF carray(2) > 1 AND .sptr < 24 AND .sptr > 0 THEN
   .sptr -= 1
  END IF
  IF carray(3) > 1 AND .sptr < 24 THEN
   .sptr += 1
  END IF
 END WITH
 
 IF carray(4) > 1 THEN
  '--use selected spell
  IF bat.sptr = 24 THEN
   '--used cancel
   bat.menu_mode = batMENUHERO
   flusharray carray(), 7, 0
   EXIT SUB
  END IF

  DIM atk AS AttackData
  '--can-I-use-it? checking
  IF spel(bat.sptr) > -1 THEN
   '--list-entry is non-empty
   loadattackdata atk, spel(bat.sptr)
   IF atkallowed(atk, bat.hero_turn, st(bat.hero_turn).list_type(bat.listslot), INT(bat.sptr / 3), bstat()) THEN
    '--attack is allowed
    '--if lmp then set lmp consume flag
    IF st(bat.hero_turn).list_type(bat.listslot) = 1 THEN conlmp(bat.hero_turn) = INT(bat.sptr / 3) + 1
    '--queue attack
    bslot(bat.hero_turn).attack = spel(bat.sptr) + 1
    delay(bat.hero_turn) = large(atk.attack_delay, 1)
    '--exit spell menu
    bat.targ.mode = targSETUP
    bat.menu_mode = batMENUHERO
    flusharray carray(), 7, 0
   END IF
  END IF
 END IF
END SUB
