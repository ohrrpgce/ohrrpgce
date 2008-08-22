'OHRRPGCE GAME - Main battle-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

'modules
#include "bmod.bi"
#include "bmodsubs.bi"
#include "game.bi"
#INCLUDE "allmodex.bi"
#include "menustuf.bi"
#include "yetmore.bi"
#include "moresubs.bi"
DECLARE FUNCTION count_available_spells(who AS INTEGER, list AS INTEGER) AS INTEGER

'misc
#include "common.bi"
#INCLUDE "compat.bi"
#INCLUDE "gglobals.bi"
#INCLUDE "const.bi"
#INCLUDE "uiconst.bi"

'--local subs and functions
DECLARE FUNCTION count_dissolving_enemies(bslot() AS BattleSprite) AS INTEGER
DECLARE FUNCTION find_empty_enemy_slot(formdata() AS INTEGER) AS INTEGER
DECLARE SUB spawn_on_death(deadguy AS INTEGER, killing_attack AS INTEGER, es(), atktype(), formdata(), bslot() AS BattleSprite, p(), bits(), bstat() AS BattleStats, ebits(), batname$(), BYREF rew AS RewardsState)
DECLARE SUB triggerfade(BYVAL who, bstat() AS BattleStats, bslot() AS BattleSprite, ebits())
DECLARE SUB check_death(deadguy AS INTEGER, BYVAL killing_attack AS INTEGER, BYREF bat AS BattleState, noifdead AS INTEGER, BYREF rew AS RewardsState, BYREF tcount AS INTEGER, bstat() AS BattleStats, bslot() AS BattleSprite, ready(), godo(), es(), atktype(), formdata(), p(), bits(), ebits(), batname$(), t(), autotmask(), revenge(), revengemask(), targmem(), BYREF tptr AS INTEGER, BYREF ptarg AS INTEGER, ltarg(), tmask(), targs())
DECLARE SUB checkitemusability(iuse() AS INTEGER)
DECLARE SUB reset_battle_state (BYREF bat AS BattleState)
DECLARE SUB reset_victory_state (BYREF vic AS VictoryState)
DECLARE SUB reset_rewards_state (BYREF rew AS RewardsState)
DECLARE SUB show_victory (BYREF vic AS VictoryState, BYREF rew AS RewardsState, exstat() AS INTEGER, batname() AS STRING)
DECLARE SUB trigger_victory(BYREF vic AS VictoryState, BYREF rew AS RewardsState, bstat() As BattleStats, exstat() AS INTEGER)

'these are the battle global variables
DIM as string battlecaption
dim as integer battlecaptime, battlecapdelay, bstackstart, learnmask(245) '6 shorts of bits per hero

dim shared battle_draw_style as integer = 0'0 = new, 1 = old

REM $STATIC
FUNCTION battle (form, fatal, exstat())

REMEMBERSTATE

lastformation = form

'--prepare stack
bstackstart = stackpos

battle = 1
DIM formdata(40), atktemp(40 + dimbinsize(binATTACK)), atk(40 + dimbinsize(binATTACK)), wepatk(40 + dimbinsize(binATTACK)), wepatkid, st(3) as herodef, es(7, 160), zbuf(24),  p(24), of(24), ctr(11)
DIM ready(11), batname$(11), menu$(3, 5), menubits(2), mend(3), itemd$, spel$(23), speld$(23), spel(23), cost$(23), godo(11), delay(11), cycle(24), walk(3), aframe(11, 11)
DIM fctr(24), harm$(11), hc(23), hx(11), hy(11), conlmp(11), bits(11, 4), atktype(8), iuse(15), icons(11), ebits(40), ltarg(11), lifemeter(3), revenge(11), revengemask(11), revengeharm(11), repeatharm(11 _
), targmem(23), prtimer(11,1), spelmask(1)
DIM laststun AS DOUBLE
DIM bat AS BattleState
DIM bslot(24) AS BattleSprite
DIM bstat(11) AS BattleStats
DIM vic AS VictoryState
DIM as double timinga, timingb
DIM dead, mapsong
DIM spellcount AS INTEGER '--only used in heromenu GOSUB block
DIM listslot AS INTEGER
DIM nmenu(3,5) as integer 'new battle menu
DIM rew AS RewardsState

DIM autotmask(11) ' A list of true/false values indicating
              ' which targets are valid for the currently targetting attack
DIM tmask(11) ' For the currently targetting hero, a list of true/false values indicating
              ' which targets are valid for the currently targetting attack
DIM targs(11) ' For the currently targetting hero, a list of true/false valuse indicating
              ' which targets from tmask() are currently selected.
DIM t(11, 12) ' For each hero and enemy, a list of currently selected targets id numbers.
              ' -1 means no target. targets must be sorted to the beginning of the list when it changes

timinga = 0
timingb = 0

mpname$ = readglobalstring(1, "MP", 10)
cancelspell$ = readglobalstring$(51, "(CANCEL)", 10)
pause$ = readglobalstring$(54, "PAUSE", 10)
cannotrun$ = readglobalstring$(147, "CANNOT RUN!", 20)

battlecaptime = 0
battlecapdelay = 0
battlecaption = ""

alert = 0
alert$ = ""

fadeout 240, 240, 240
vpage = 0: dpage = 1: needf = 1: anim = -1: fiptr = 0
reset_battle_state bat
reset_victory_state vic
reset_rewards_state rew
aset = 0: wf = 0: noifdead = 0

ptarg = 0 ' ptarg=0 means hero not currently picking a target
          ' ptarg>0 means hero picking a target
          ' ptarg=1 means targetting needs set-up
          ' ptarg=2 means normal manual targetting
          ' ptarg=3 means autotargeting

FOR i = 0 TO 11
 icons(i) = -1
 revenge(i) = -1
NEXT i
checkitemusability iuse()
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
  IF keyval(41) > 1 THEN battle_draw_style = (battle_draw_style + 1) mod 2 'Switch between old and new sprite code
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
 IF anim >= 0 AND aset = 0 AND vic.state = 0 THEN GOSUB atkscript
 IF anim >= 0 AND aset = 1 AND vic.state = 0 AND away = 0 THEN GOSUB action
 GOSUB animate
 na = loopvar(na, 0, 11, 1)
 IF anim = -1 AND vic.state = 0 THEN
  GOSUB meters
  IF godo(na) > 0 AND delay(na) = 0 THEN
   '--next attacker has an attack selected and the delay is over
   anim = godo(na) - 1
   bat.acting = na
   aset = 0
   godo(na) = 0
  END IF
 END IF
 bat.next_hero = loopvar(bat.next_hero, 0, 3, 1)
 IF bat.hero_turn = -1 THEN
  '--if it is no heros turn, check to see if anyone is alive and ready
  IF ready(bat.next_hero) = 1 AND bstat(bat.next_hero).cur.hp > 0 AND dead = 0 THEN
   bat.hero_turn = bat.next_hero
   pt = 0
   bat.menu_mode = batMENUHERO
  END IF
 END IF
 bat.next_enemy = loopvar(bat.next_enemy, 4, 11, 1)
 IF bat.enemy_turn = -1 THEN
  IF ready(bat.next_enemy) = 1 AND bstat(bat.next_enemy).cur.hp > 0 AND dead = 0 THEN bat.enemy_turn = bat.next_enemy
 END IF
 IF vic.state = 0 THEN
  IF bat.enemy_turn >= 0 THEN GOSUB enemyai
  IF bat.hero_turn >= 0 AND ptarg = 0 THEN
   IF bat.menu_mode = batMENUITEM THEN GOSUB itemmenu
   IF bat.menu_mode = batMENUSPELL THEN GOSUB spellmenu
   IF bat.menu_mode = batMENUHERO THEN GOSUB heromenu
  END IF
  IF bat.hero_turn >= 0 AND ptarg > 0 THEN GOSUB picktarg
 END IF
 GOSUB sprite
 GOSUB display
 IF vic.state = vicEXITDELAY THEN vic.state = vicEXIT
 IF vic.state > 0 THEN show_victory vic, rew, exstat(), batname$()
 IF vis = 1 THEN GOSUB seestuff
 IF dead = 1 AND vic.state = 0 THEN
  IF count_dissolving_enemies(bslot()) = 0 THEN trigger_victory vic, rew, bstat(), exstat()
 END IF
 IF vic.state = vicEXIT THEN EXIT DO 'normal victory exit
 IF dead = 2 THEN
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
 copypage 2, dpage
 IF needf = 1 THEN
  needf = 0
  IF formdata(33) > 0 THEN fademusic fmvol
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

IF (formdata(33) >= 0 AND formdata(33) - 1 <> mapsong) OR (gen(3) > 0 AND fatal = 0) THEN fademusic 0

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

enemyai: '-------------------------------------------------------------------
ai = 0

'if HP is less than 20% go into desperation mode
IF bstat(bat.enemy_turn).cur.hp < bstat(bat.enemy_turn).max.hp / 5 THEN ai = 1

'if targetable enemy count is 1, go into alone mode
IF targenemycount(bslot(), bstat()) = 1 THEN ai = 2

'spawn allys when alone
IF ai = 2 AND es(bat.enemy_turn - 4, 81) THEN
 FOR j = 1 TO es(bat.enemy_turn - 4, 91)
  slot = find_empty_enemy_slot(formdata())
  IF slot > -1 THEN
   formdata(slot * 4) = es(bat.enemy_turn - 4, 81)
   loadfoe slot, formdata(), es(), bslot(), p(), bits(), bstat(), ebits(), batname$(), rew
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
IF countai(ai, bat.enemy_turn, es()) = 0 THEN bat.enemy_turn = -1: RETRACE

'pick a random attack
lim = 0
DO
 godo(bat.enemy_turn) = es(bat.enemy_turn - 4, 92 + (ai * 5) + INT(RND * 5))
 IF godo(bat.enemy_turn) > 0 THEN
  'load the data for this attack
  loadattackdata atktemp(), godo(bat.enemy_turn) - 1
  IF atkallowed(atktemp(), bat.enemy_turn, 0, 0, bstat()) THEN
   'this attack is good, continue on to target selection
   EXIT DO
  ELSE
   'this attack is unusable
   godo(bat.enemy_turn) = 0
   IF bstat(bat.enemy_turn).cur.mp - atktemp(8) < 0 THEN
    'inadequate MP was the reason for the failure
    'MP-idiot loses its turn
    IF readbit(ebits(), (bat.enemy_turn - 4) * 5, 55) THEN
      ready(bat.enemy_turn) = 0
      ctr(bat.enemy_turn) = 0
      bat.enemy_turn = -1
      RETRACE
    END IF
   END IF
   'currently, item requirements are disregarded. should they be? Maybe they should
   'come out of theft items?
  END IF
 END IF
 lim = lim + 1
 IF lim > 99 THEN
  'give up eventually
  bat.enemy_turn = -1
  RETRACE
 END IF
LOOP

'get the delay to wait for this attack
delay(bat.enemy_turn) = atktemp(16)

get_valid_targs autotmask(), bat.enemy_turn, atktemp(), bslot(), bstat(), revenge(), revengemask(), targmem()
autotarget t(), autotmask(), bat.enemy_turn, atktemp(), bslot(), bstat()

'ready for next attack
ready(bat.enemy_turn) = 0
ctr(bat.enemy_turn) = 0
bat.enemy_turn = -1

RETRACE


heromenu: '-----------------------------------------------------------------
FOR i = 0 TO 5
 setbit menubits(), 0, bat.hero_turn * 4 + i, 0
 IF nmenu(bat.hero_turn, i) > 0 THEN
  'IF foo then
   IF wepatkid <> nmenu(bat.hero_turn, i) THEN
    wepatkid = nmenu(bat.hero_turn, i)
    loadattackdata wepatk(), wepatkid - 1
   END IF
   IF readbit(wepatk(), 65, 6) THEN
    IF atkallowed(wepatk(), bat.hero_turn, 0, 0, bstat()) = 0 THEN
     setbit menubits(), 0, bat.hero_turn * 4 + i, 1
    END IF
   END IF
  'END IF
 END IF
NEXT i
IF carray(5) > 1 THEN bat.next_hero = bat.hero_turn: bat.hero_turn = -1: RETRACE
IF carray(0) > 1 THEN pt = pt - 1: IF pt < 0 THEN pt = mend(bat.hero_turn)
IF carray(1) > 1 THEN pt = pt + 1: IF pt > mend(bat.hero_turn) THEN pt = 0
IF carray(4) > 1 THEN
 IF nmenu(bat.hero_turn, pt) > 0 THEN 'simple attack
  IF readbit(menubits(), 0, bat.hero_turn * 4 + pt) = 0 THEN
   godo(bat.hero_turn) = nmenu(bat.hero_turn, pt)
   loadattackdata buffer(), godo(bat.hero_turn) - 1
   delay(bat.hero_turn) = large(buffer(16), 1)
   ptarg = 1
   flusharray carray(), 7, 0
   RETRACE
  END IF
 END IF
 IF nmenu(bat.hero_turn, pt) < 0 AND nmenu(bat.hero_turn, pt) >= -4 THEN '--this is a spell list
  listslot = (nmenu(bat.hero_turn, pt) + 1) * -1
  IF st(bat.hero_turn).list_type(listslot) < 2 THEN '--the type of this spell list is one that displays a menu
   '--init spell menu
   bat.menu_mode = batMENUSPELL: sptr = 0
   FOR i = 0 TO 23 '-- loop through the spell list setting up menu items for each
    spel$(i) = ""
    speld$(i) = ""
    cost$(i) = ""
    spel(i) = -1
    setbit spelmask(), 0, i, 0
    IF spell(bat.hero_turn, listslot, i) > 0 THEN '--there is a spell in this slot
     spellcount += 1
     spel(i) = spell(bat.hero_turn, listslot, i) - 1
     loadattackdata atktemp(), spel(i)
     spel$(i) = readbadbinstring$(atktemp(), 24, 10, 1)
     speld$(i) = readbinstring$(atktemp(), 73, 38)
     IF st(bat.hero_turn).list_type(listslot) = 0 THEN
      '--regular MP
      cost$(i) = XSTR$(focuscost(atktemp(8), bstat(bat.hero_turn).cur.foc)) + " " + mpname$ + " " + STR$(bstat(bat.hero_turn).cur.mp) + "/" + STR$(bstat(bat.hero_turn).max.mp)
     END IF
     IF st(bat.hero_turn).list_type(listslot) = 1 THEN
      '--level MP
      cost$(i) = "Level" + XSTR$(INT(i / 3) + 1) + ":  " + XSTR$(lmp(bat.hero_turn, INT(i / 3)))
     END IF
     IF atkallowed(atktemp(), bat.hero_turn, st(bat.hero_turn).list_type(listslot), INT(i / 3), bstat()) THEN
      '-- check whether or not the spell is allowed
      setbit spelmask(), 0, i, 1
     END IF
    END IF
    spel$(i) = rpad$(spel$(i), " ", 10) '-- pad the spell menu caption
   NEXT i
  ELSEIF st(bat.hero_turn).list_type(listslot) = 2 THEN '-- this is a random spell list
   spellcount = 0
   FOR i = 0 TO 23 '-- loop through the spell list storing attack ID numbers
    spel(i) = -1
    IF spell(bat.hero_turn, listslot, i) > 0 THEN '--there is a spell in this slot
     spellcount += 1
     spel(i) = spell(bat.hero_turn, listslot, i) - 1
    END IF
   NEXT i
   IF spellcount > 0 THEN '-- don't attempt to pick randomly from an empty list
    rptr = INT(RND * 24)
    FOR i = 0 TO INT(RND * spellcount)
     ol = 0
     DO
      rptr = loopvar(rptr, 0, 23, 1)
      ol = ol + 1
     LOOP UNTIL spel(rptr) > -1 OR ol > 999 '--loop until we have found a spell (or give up after 999 tries)
    NEXT i
    godo(bat.hero_turn) = spel(rptr) + 1
    loadattackdata buffer(), godo(bat.hero_turn) - 1
    delay(bat.hero_turn) = large(buffer(16), 1)
    ptarg = 1
    flusharray carray(), 7, 0
   END IF
  END IF
 ELSEIF nmenu(bat.hero_turn, pt) = -10 THEN '--items menu
  bat.menu_mode = batMENUITEM
  iptr = 0
  itop = 0
  itemd$ = ""
  IF inventory(iptr).used THEN
   loaditemdata buffer(), inventory(iptr).id
   itemd$ = readbadbinstring$(buffer(), 9, 35, 0)
  END IF
 END IF
END IF
RETRACE

atkscript: '---------------------------------------------------------------
'--check for item consumption
IF icons(bat.acting) >= 0 THEN
 IF inventory(icons(bat.acting)).used = 0 THEN
  '--abort if item is gone
  anim = -1: RETRACE
 END IF
END IF
'--load attack
loadattackdata atk(), anim
'--load picture
setpicstuf buffer(), 3750, 3
loadset game + ".pt6", atk(0), 144

'--load palette
getpal16 pal16(), 53, atk(1), 6, atk(0)
FOR i = 12 TO 23
 p(i) = 53
 of(i) = 0
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
ltarg(bat.acting) = 0
'CANNOT HIT INVISIBLE FOES
FOR i = 0 TO 11
 IF t(bat.acting, i) > -1 THEN
  IF bslot(t(bat.acting, i)).vis = 0 AND (atk(3) <> 4 AND atk(3) <> 10) THEN
   t(bat.acting, i) = -1
  END IF
 END IF
NEXT i
'MOVE EMPTY TARGET SLOTS TO THE BACK
FOR o = 0 TO 10
 FOR i = 0 TO 10
  IF t(bat.acting, i) = -1 THEN SWAP t(bat.acting, i), t(bat.acting, i + 1)
 NEXT i
NEXT o
'COUNT TARGETS
FOR i = 0 TO 11
 IF t(bat.acting, i) > -1 THEN tcount = tcount + 1
 cycle(i) = -1
NEXT i
atktype(0) = 1
FOR i = 0 TO 7
 atktype(i + 1) = 0
 IF readbit(atk(), 20, 5 + i) = 1 THEN atktype(i + 1) = 1: atktype(0) = 0
NEXT i
'ABORT IF TARGETLESS
IF tcount = -1 THEN anim = -1: RETRACE
'Kill Target history
targmem(bat.acting) = 0
' BIG CRAZY SCRIPT CONSTRUCTION
'DEBUG debug "begin script construction"
IF is_hero(bat.acting) THEN
 'load weapon sprites
 setpicstuf buffer(), 576, 3
 loadset game + ".pt5" , exstat(bat.acting, 0, 13), 156
 p(24) = 52
 getpal16 pal16(), 52, exstat(bat.acting, 1, 13), 5, exstat(bat.acting, 0, 13)
 
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
 advance bat.acting, atk(), bslot(), t()
 if atk(99) > 0  then anim_sound(atk(99) - 1)
 FOR j = 1 TO numhits
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot(), t()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot(), t()
  FOR i = 0 TO tcount
   anim_inflict t(bat.acting,i)
  NEXT i
  anim_disappear 24
 NEXT j
 retreat bat.acting, atk(), bslot(), t()
 anim_end
END IF
'----------------------------NORMAL, DROP, SPREAD-RING, and SCATTER
IF atk(15) = 0 OR atk(15) = 3 OR atk(15) = 6 OR (atk(15) = 4 AND tcount > 0) THEN
 FOR i = 0 TO tcount
  yt = (bslot(t(bat.acting, i)).h - 50) + 2
  xt = 0: IF t(bat.acting, i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
  anim_setpos 12 + i, bslot(t(bat.acting, i)).x + xt, bslot(t(bat.acting, i)).y + yt, pdir
  IF atk(15) = 3 THEN
   anim_setz 12 + i, 180
  END IF
  IF atk(15) = 4 THEN
   anim_setpos 12 + i, bslot(t(bat.acting, i)).x + xt, bslot(t(bat.acting, i)).y + yt - bslot(t(bat.acting, i)).w, pdir
  END IF
 NEXT i
 advance bat.acting, atk(), bslot(), t()
 FOR j = 1 TO numhits
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot(), t()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot(), t()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   IF atk(15) = 4 THEN
    anim_absmove 12 + i, bslot(t(bat.acting, i)).x + xt - bslot(t(bat.acting, i)).w, bslot(t(bat.acting, i)).y + yt, 3, 3
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
    anim_absmove 12 + i, bslot(t(bat.acting, i)).x + xt, bslot(t(bat.acting, i)).y + yt + bslot(t(bat.acting, i)).w, 3, 3
   NEXT i
   anim_waitforall
   FOR i = 0 TO tcount
    anim_absmove 12 + i, bslot(t(bat.acting, i)).x + xt + bslot(t(bat.acting, i)).w, bslot(t(bat.acting, i)).y + yt, 3, 3
   NEXT i
   anim_waitforall
   FOR i = 0 TO tcount
    anim_absmove 12 + i, bslot(t(bat.acting, i)).x + xt, bslot(t(bat.acting, i)).y + yt - bslot(t(bat.acting, i)).w, 3, 3
   NEXT i
   anim_waitforall
  END IF
  FOR i = 0 TO tcount
   anim_inflict t(bat.acting, i)
   temp = 3: IF is_enemy(t(bat.acting, i)) THEN temp = -3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   if is_hero(t(bat.acting, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(bat.acting, i), 5
    END IF
    IF readbit(atk(), 20, 0) = 1 THEN
     anim_setframe t(bat.acting, i), 2
    END IF
   end if
  NEXT i
  IF atk(15) <> 4 THEN
   anim_wait 3
  END IF
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   temp = -3: IF is_enemy(t(bat.acting, i)) THEN temp = 3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   anim_setframe t(bat.acting, i), 0
  NEXT i
  anim_wait 2
 NEXT j
 retreat bat.acting, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(bat.acting, i), 0
 NEXT i
 anim_end
END IF
'----------------------------SEQUENTIAL PROJECTILE
IF atk(15) = 7 THEN
 'attacker steps forward
 advance bat.acting, atk(), bslot(), t()
 'repeat the following for each attack
 FOR j = 1 TO numhits
  'attacker animates
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot(), t()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot(), t()
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
   yt = (bslot(t(bat.acting, i)).h - 50) + 2
   xt = 0: IF t(bat.acting, i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
   'make the projectile move to the target
   anim_absmove 12, bslot(t(bat.acting, i)).x + xt, bslot(t(bat.acting, i)).y + yt, 5, 5
   anim_waitforall
   'inflict damage
   anim_inflict t(bat.acting, i)
   'make the target flinch back
   targetflinch = 3: IF is_enemy(t(bat.acting, i)) THEN targetflinch = -3
   anim_setmove t(bat.acting, i), targetflinch, 0, 2, 0
   'show harm animation
   if is_hero(t(bat.acting, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(bat.acting, i), 5
    END IF
    IF readbit(atk(), 20, 0) = 1 THEN
     anim_setframe t(bat.acting, i), 2
    END IF
   end if
   anim_wait 3
   'recover from flinch
   targetflinch = -3: IF is_enemy(t(bat.acting, i)) THEN targetflinch = 3
   anim_setmove t(bat.acting, i), targetflinch, 0, 2, 0
   anim_setframe t(bat.acting, i), 0
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
 retreat bat.acting, atk(), bslot(), t()
 anim_end
END IF
'-----------------PROJECTILE, REVERSE PROJECTILE and METEOR
IF (atk(15) >= 1 AND atk(15) <= 2) OR atk(15) = 8 THEN
 advance bat.acting, atk(), bslot(), t()
 FOR j = 1 TO numhits
  FOR i = 0 TO tcount
   temp = 50: IF is_hero(bat.acting) THEN temp = -50
   dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
   yt = (bslot(t(bat.acting, i)).h - 50) + 2
   xt = 0: IF t(bat.acting, i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 THEN
    anim_setpos 12 + i, bslot(bat.acting).x + temp, bslot(bat.acting).y, dtemp
   END IF
   IF atk(15) = 2 THEN
    anim_setpos 12 + i, bslot(t(bat.acting, i)).x + xt, bslot(t(bat.acting, i)).y + yt, dtemp
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
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot(), t()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot(), t()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   temp = 50: IF is_hero(bat.acting) THEN temp = -50
   yt = (bslot(t(bat.acting, i)).h - 50) + 2
   xt = 0: IF t(bat.acting, i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 OR atk(15) = 8 THEN
    anim_absmove 12 + i, bslot(t(bat.acting, i)).x + xt, bslot(t(bat.acting, i)).y + yt, 6, 6
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
   anim_inflict t(bat.acting, i)
   temp = 3: IF is_enemy(t(bat.acting, i)) THEN temp = -3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   if is_hero(t(bat.acting, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(bat.acting, i), 5
    ELSE
     anim_setframe t(bat.acting, i), 2
    END IF
   end if
  NEXT i
  anim_wait 3
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   temp = -3: IF is_enemy(t(bat.acting, i)) THEN temp = 3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   anim_setframe t(bat.acting, i), 0
  NEXT i
  anim_wait 3
 NEXT j
 retreat bat.acting, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(bat.acting, i), 0
 NEXT i
 anim_end
END IF
'--------------------------------------DRIVEBY
IF atk(15) = 9 THEN
 advance bat.acting, atk(), bslot(), t()
 FOR j = 1 TO numhits
  dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
  FOR i = 0 TO tcount
   yt = (bslot(t(bat.acting, i)).h - 50) + 2
   IF is_hero(bat.acting) THEN
    anim_setpos 12 + i, 320, bslot(t(bat.acting, i)).y + yt, dtemp
   END IF
   IF is_enemy(bat.acting) THEN
    anim_setpos 12 + i, -50, bslot(t(bat.acting, i)).y + yt, dtemp
   END IF
  NEXT i
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot(), t()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot(), t()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   temp = 50: IF is_hero(bat.acting) THEN temp = -50
   yt = (bslot(t(bat.acting, i)).h - 50) + 2
   anim_absmove 12 + i, bslot(t(bat.acting, i)).x + xt, bslot(t(bat.acting, i)).y + yt, 8, 8
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 4
  anim_disappear 24
  anim_setframe bat.acting, 0
  anim_waitforall
  FOR i = 0 TO tcount
   anim_inflict t(bat.acting, i)
   temp = 3: IF is_enemy(t(bat.acting, i)) THEN temp = -3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   if is_hero(t(bat.acting, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(bat.acting, i), 5
    ELSE
     anim_setframe t(bat.acting, i), 2
    END IF
   end if
   yt = (bslot(t(bat.acting, i)).h - 50) + 2
   IF is_hero(bat.acting) THEN
    anim_absmove 12 + i, -50, bslot(t(bat.acting, i)).y + yt, 5, 7
   END IF
   IF is_enemy(bat.acting) THEN
    anim_absmove 12 + i, 320, bslot(t(bat.acting, i)).y + yt, 5, 7
   END IF
  NEXT i
  anim_waitforall
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   temp = -3: IF is_enemy(t(bat.acting, i)) THEN temp = 3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   anim_setframe t(bat.acting, i), 0
  NEXT i
  anim_wait 3
 NEXT j
 retreat bat.acting, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(bat.acting, i), 0
 NEXT i
 anim_end
END IF
'--------------------------------FOCUSED RING
IF atk(15) = 4 AND tcount = 0 THEN
 dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
 advance bat.acting, atk(), bslot(), t()
 FOR j = 1 TO numhits
  i = 0
  yt = (bslot(t(bat.acting, i)).h - 50) + 2
  xt = 0: IF t(bat.acting, i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
  tempx = bslot(t(bat.acting, i)).x + xt
  tempy = bslot(t(bat.acting, i)).y + yt
  anim_setpos 12 + 0, tempx + 0, tempy - 50, dtemp
  anim_setpos 12 + 1, tempx + 30, tempy - 30, dtemp
  anim_setpos 12 + 2, tempx + 50, tempy + 0, dtemp
  anim_setpos 12 + 3, tempx + 30, tempy + 30, dtemp
  anim_setpos 12 + 4, tempx - 0, tempy + 50, dtemp
  anim_setpos 12 + 5, tempx - 30, tempy + 30, dtemp
  anim_setpos 12 + 6, tempx - 50, tempy - 0, dtemp
  anim_setpos 12 + 7, tempx - 30, tempy - 30, dtemp
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot(), t()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot(), t()
  yt = (bslot(t(bat.acting, 0)).h - 50) + 2
  xt = 0: IF t(bat.acting, i) = bat.acting AND is_hero(bat.acting) AND atk(14) <> 7 THEN xt = -20
  FOR i = 0 TO 7
   anim_appear 12 + i
   anim_absmove 12 + i, bslot(t(bat.acting, 0)).x + xt, bslot(t(bat.acting, 0)).y + yt, 4, 4
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 8
  anim_disappear 24
  anim_setframe bat.acting, 0
  FOR i = 0 TO tcount
   anim_inflict t(bat.acting, i)
   temp = 3: IF is_enemy(t(bat.acting, i)) THEN temp = -3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   if is_hero(t(bat.acting, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(bat.acting, i), 5
    ELSE
     anim_setframe t(bat.acting, i), 2
    END IF
   end if
  NEXT i
  anim_wait 3
  FOR i = 0 TO 7
   anim_disappear 12 + i
  NEXT i
  FOR i = 0 TO tcount
   temp = -3: IF is_enemy(t(bat.acting, i)) THEN temp = 3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   anim_setframe t(bat.acting, i), 0
  NEXT i
  anim_wait 3
 NEXT j
 retreat bat.acting, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(bat.acting, i), 0
 NEXT i
 anim_end
END IF
'--------------------------------WAVE
IF atk(15) = 5 THEN
 yt = bslot(t(bat.acting, 0)).y + (bslot(t(bat.acting, 0)).h - 50) + 2
 advance bat.acting, atk(), bslot(), t()
 FOR j = 1 TO numhits
  FOR i = 0 TO 11
   temp = -50: IF is_hero(bat.acting) THEN temp = 320
   IF tcount > 0 OR atk(4) = 1 THEN
    anim_setpos 12 + i, temp, i * 15, pdir
   ELSE
    anim_setpos 12 + i, temp, yt, pdir
   END IF
  NEXT i
  IF is_hero(bat.acting) THEN heroanim bat.acting, atk(), bslot(), t()
  IF is_enemy(bat.acting) THEN etwitch bat.acting, atk(), bslot(), t()
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
   anim_inflict t(bat.acting, i)
   temp = 3: IF is_enemy(t(bat.acting, i)) THEN temp = -3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   if is_hero(t(bat.acting, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(bat.acting, i), 5
    ELSE
     anim_setframe t(bat.acting, i), 2
    END IF
   end if
  NEXT i
  anim_waitforall
  FOR i = 0 TO 11
   anim_disappear 12 + i
  NEXT i
  FOR i = 0 TO tcount
   temp = -3: IF is_enemy(t(bat.acting, i)) THEN temp = 3
   anim_setmove t(bat.acting, i), temp, 0, 2, 0
   anim_setframe t(bat.acting, i), 0
  NEXT i
  anim_wait 2
 NEXT j
 retreat bat.acting, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(bat.acting, i), 0
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
'DEBUG debug "stackpos =" + XSTR$((stackpos - bstackstart) \ 2)
invertstack
'--aset indicates that animation is set and that we should proceed to "action"
aset = 1
RETRACE

action:
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
    IF bstat(i).cur.hp < bstat(i).max.hp / 5 AND vic.state = 0 THEN of(i) = 6: bslot(i).frame = 6
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
   anim = -1
  CASE 1 '???()
   FOR i = 0 TO 3
    formdata(i * 4 + 1) = bslot(4 + i).x
    formdata(i * 4 + 2) = bslot(4 + i).y
   NEXT i
   anim = -1
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
   IF is_hero(ww) THEN walk(ww) = 0: of(ww) = fr
   IF ww > 23 THEN of(ww) = fr '--is this right?
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
  CASE 10 'inflict(targ)
   targ = popw
   'set tag, if there is one
   checkTagCond atk(60), 1, atk(59), atk(61)
   checkTagCond atk(63), 1, atk(62), atk(64)
   IF inflict(bat.acting, targ, bstat(), bslot(), harm$(), hc(), hx(), hy(), atk(), tcount, bits(), revenge(), revengemask(), targmem(), revengeharm(), repeatharm()) THEN
    '--attack succeeded
    IF readbit(atk(), 65, 12) THEN
     '--try to cancel target's attack
     IF godo(targ) > 0 THEN
      'Check if the attack is cancelable
      loadattackdata atktemp(), godo(targ) - 1
      IF readbit(atktemp(), 65, 13) = 0 THEN
       'Okay to cancel target's attack
       godo(targ) = 0
      END IF
     ELSE
      'just cancel the attack (FIXME: is this needed? What is the meaning of a negative godo value?)
      godo(targ) = 0
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
     IF checkNoRunBit(bstat(), ebits(), bslot()) THEN
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
     checkitemusability iuse()
    END IF
   ELSE
    checkTagCond atk(60), 3, atk(59), atk(61)
    checkTagCond atk(63), 3, atk(62), atk(64)
   END IF
   triggerfade targ, bstat(), bslot(), ebits()
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
   IF o = 4 THEN anim = -1
   o = 0
   FOR i = 4 TO 11
    IF bstat(i).cur.hp = 0 THEN o = o + 1
   NEXT i
   IF bstat(targ).cur.hp = 0 AND o < 8 AND anim > -1 THEN'
    '--if the target is already dead, auto-pick a new target
    '--FIXME: why are we doing this after the attack? Does this even do anything?
    '--       it was passing garbage attack data at least some of the time until r2104
    get_valid_targs autotmask(), bat.acting, atk(), bslot(), bstat(), revenge(), revengemask(), targmem()
    autotarget t(), autotmask(), bat.acting, atk(), bslot(), bstat()
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
   of(ww) = 0
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
LOOP UNTIL wf <> 0 OR anim = -1

IF anim = -1 THEN
 GOSUB afterdone
 '--clean up stack
 'DEBUG debug "discarding" + XSTR$((stackpos - bstackstart) \ 2) + " from stack"
 WHILE stackpos > bstackstart: dummy = popw: WEND
 '-------Spawn a Chained Attack--------
 IF atk(12) > 0 AND INT(RND * 100) < atk(13) AND bstat(bat.acting).cur.hp > 0 AND (bslot(bat.acting).attack_succeeded <> 0 AND readbit(atk(),65,7) OR readbit(atk(),65,7) = 0)THEN
  wf = 0: aset = 0
  loadattackdata buffer(), atk(12) - 1
  IF buffer(16) > 0 THEN
   godo(bat.acting) = atk(12)
   delay(bat.acting) = buffer(16)
  ELSE
   anim = atk(12) - 1: aset = 0: godo(bat.acting) = 0
  END IF
  o = 0
  FOR i = 4 TO 11
   IF bstat(i).cur.hp = 0 THEN o = o + 1
  NEXT i
  IF o < 8 THEN
   IF buffer(4) <> atk(4) OR buffer(3) <> atk(3) THEN
    'if the chained attack has a different target class/type then re-target
    get_valid_targs autotmask(), bat.acting, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
    autotarget t(), autotmask(), bat.acting, buffer(), bslot(), bstat()
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
GOSUB fulldeathcheck
RETRACE

setuptarg: '--identify valid targets (heroes only)

'init
spred = 0
aim = 0
randomtarg = 0
firsttarg = 0
tptr = 0
FOR i = 0 TO 11
 targs(i) = 0 ' clear list of selected targets
 t(bat.hero_turn, i) = -1 'clear list of confirmed targets
NEXT i

'load attack
loadattackdata buffer(), godo(bat.hero_turn) - 1

noifdead = 0
ltarg(bat.hero_turn) = 0

get_valid_targs tmask(), bat.hero_turn, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
noifdead = attack_can_hit_dead(bat.hero_turn, buffer())

'--attacks that can target all should default to the first enemy
IF buffer(3) = 3 THEN
 tptr = 4
END IF

'fail if there are no targets
IF targetmaskcount(tmask()) = 0 THEN
 ptarg = 0
 RETRACE
END IF

'autoattack
IF readbit(buffer(), 20, 54) THEN
 ptarg = 3
 RETRACE
END IF

IF buffer(4) = 0 THEN aim = 1
IF buffer(4) = 1 THEN FOR i = 0 TO 11: targs(i) = tmask(i): NEXT i
IF buffer(4) = 2 THEN aim = 1: spred = 1
IF buffer(4) = 3 THEN randomtarg = -1
IF buffer(4) = 4 THEN firsttarg = -1

tptr = find_preferred_target(tmask(), bat.hero_turn, buffer(), bslot(), bstat())
'fail if no targets are found
IF tptr = -1 THEN
 ptarg = 0
 RETRACE
END IF

'ready to choose targs() from tmask()
ptarg = 2
RETRACE

fulldeathcheck:
FOR deadguy = 4 TO 11
 IF bstat(deadguy).cur.hp > 0 THEN
  'this enemy hasn't just spawned; it should fade out
  IF dieWOboss(deadguy, bstat(), ebits()) THEN
   triggerfade deadguy, bstat(), bslot(), ebits()
  END IF
 END IF
NEXT
FOR deadguy = 0 TO 11
 check_death deadguy, 0, bat, noifdead, rew, tcount, bstat(), bslot(), ready(), godo(), es(), atktype(), formdata(), p(), bits(), ebits(), batname$(), t(), autotmask(), revenge(), revengemask(), targmem(), tptr, ptarg, ltarg(), tmask(), targs()
NEXT
deadguycount = 0
FOR deadguy = 4 TO 11
 IF bstat(deadguy).cur.hp = 0 OR bslot(deadguy).hero_untargetable OR bslot(deadguy).death_unneeded THEN deadguycount += 1
NEXT
IF deadguycount >= 8 THEN dead = 1
deadguycount = 0
FOR deadguy = 0 TO 3
 IF bstat(deadguy).cur.hp = 0 THEN deadguycount += 1
NEXT deadguy
IF deadguycount = 4 THEN dead = 2
RETRACE

sponhit:
FOR i = 0 TO 8
 IF es(targ - 4, 82 + i) > 0 AND atktype(i) = 1 THEN
  FOR j = 1 TO es(targ - 4, 91)
   slot = find_empty_enemy_slot(formdata())
   IF slot > -1 THEN
    formdata(slot * 4) = es(targ - 4, 82 + i)
    loadfoe slot, formdata(), es(), bslot(), p(), bits(), bstat(), ebits(), batname$(), rew
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
oldiptr = iptr
IF carray(0) > 1 AND iptr > 2 THEN iptr = iptr - 3
IF carray(1) > 1 AND iptr <= inventoryMax - 3 THEN iptr = iptr + 3
IF keyval(73) > 1 THEN
 iptr = iptr - 27: itop = itop - 27
 IF itop < 0 THEN itop = 0
 WHILE iptr < 0: iptr = iptr + 3: WEND
END IF
IF keyval(81) > 1 THEN
 iptr = iptr + 27: itop = itop + 27
 IF itop > 171 THEN itop = 171
 WHILE iptr > inventoryMax: iptr = iptr - 3: WEND
END IF
IF carray(2) > 1 AND iptr > 0 THEN
 iptr = iptr - 1
END IF
IF carray(3) > 1 AND iptr < inventoryMax THEN
 iptr = iptr + 1
END IF
'--scroll when past top or bottom
IF iptr < itop THEN itop = itop - 3
IF iptr > itop + 26 THEN itop = itop + 3

IF oldiptr <> iptr THEN
 IF inventory(iptr).used THEN
  loaditemdata buffer(), inventory(iptr).id
  itemd$ = readbadbinstring$(buffer(), 9, 35, 0)
 ELSE
  itemd$ = ""
 END IF
END IF

IF carray(4) > 1 THEN
 IF readbit(iuse(), 0, iptr) = 1 THEN
  loaditemdata buffer(), inventory(iptr).id
  icons(bat.hero_turn) = -1: IF buffer(73) = 1 THEN icons(bat.hero_turn) = iptr
  temp = buffer(47)
  loadattackdata buffer(), temp - 1
  godo(bat.hero_turn) = temp
  delay(bat.hero_turn) = large(buffer(16), 1)
  ptarg = 1
  bat.menu_mode = batMENUHERO
  flusharray carray(), 7, 0
 END IF
END IF
RETRACE

spellmenu:
IF carray(5) > 1 THEN '--cancel
 bat.menu_mode = batMENUHERO
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
  bat.menu_mode = batMENUHERO
  flusharray carray(), 7, 0
  RETRACE
 END IF

 '--can-I-use-it? checking
 IF spel(sptr) > -1 THEN
  '--list-entry is non-empty
  loadattackdata atktemp(), spel(sptr)
  IF atkallowed(atktemp(), bat.hero_turn, st(bat.hero_turn).list_type(listslot), INT(sptr / 3), bstat()) THEN
   '--attack is allowed
   '--if lmp then set lmp consume flag
   IF st(bat.hero_turn).list_type(listslot) = 1 THEN conlmp(bat.hero_turn) = INT(sptr / 3) + 1
   '--queue attack
   godo(bat.hero_turn) = spel(sptr) + 1
   delay(bat.hero_turn) = large(atktemp(16), 1)
   '--exit spell menu
   ptarg = 1: bat.menu_mode = batMENUHERO
   flusharray carray(), 7, 0
  END IF
 END IF
END IF
RETRACE

picktarg: '-----------------------------------------------------------

'cancel
IF carray(5) > 1 THEN
 godo(bat.hero_turn) = 0
 conlmp(bat.hero_turn) = 0
 ptarg = 0
 flusharray carray(), 7, 0
 RETRACE
END IF

IF ptarg = 1 THEN GOSUB setuptarg

'autotarget
IF ptarg = 3 THEN
 get_valid_targs autotmask(), bat.hero_turn, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
 autotarget t(), autotmask(), bat.hero_turn, buffer(), bslot(), bstat()
 ctr(bat.hero_turn) = 0
 ready(bat.hero_turn) = 0
 bat.hero_turn = -1
 ptarg = 0
 RETRACE
END IF

IF targetmaskcount(tmask()) = 0 THEN
 RETRACE
END IF

'random target
IF randomtarg THEN
 FOR i = 0 TO INT(RND * 2)
  tptr = loopvar(tptr, 0, 11, 1)
  WHILE tmask(tptr) = 0
   tptr = loopvar(tptr, 0, 11, 1)
  WEND
 NEXT i
END IF

'first target
IF firsttarg THEN
 tptr = 0
 WHILE tmask(tptr) = 0
  tptr = loopvar(tptr, 0, 11, 1)
 WEND
END IF

IF spred = 2 AND (carray(2) > 1 OR carray(3) > 1) AND randomtarg = 0 AND firsttarg = 0 THEN
 FOR i = 0 TO 11
  targs(i) = 0
 NEXT i
 spred = 1
 flusharray carray(), 7, 0
END IF
IF aim = 1 AND spred < 2 AND randomtarg = 0 AND firsttarg = 0 THEN
 IF carray(0) > 1 THEN
  smartarrows tptr, -1, 1, bslot(), targs(), tmask(), 0
 END IF
 IF carray(1) > 1 THEN
  smartarrows tptr, 1, 1, bslot(), targs(), tmask(), 0
 END IF
 IF carray(2) > 1 THEN
  smartarrows tptr, -1, 0, bslot(), targs(), tmask(), spred
 END IF
 IF carray(3) > 1 THEN
  smartarrows tptr, 1, 0, bslot(), targs(), tmask(), spred
 END IF
END IF
IF carray(4) > 1 THEN GOSUB gottarg
RETRACE

gottarg: '-----------------------------------------------------------------
targs(tptr) = 1
o = 0
FOR i = 0 TO 11
 IF targs(i) = 1 THEN
  t(bat.hero_turn, o) = i: o = o + 1
  IF noifdead THEN setbit ltarg(), bat.hero_turn, i, 1
 END IF
NEXT i
ctr(bat.hero_turn) = 0
ready(bat.hero_turn) = 0
bat.hero_turn = -1
ptarg = 0
noifdead = 0
RETRACE

display:
IF vic.state = 0 THEN 'only display interface till you win
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   IF readbit(gen(), 101, 6) = 0 THEN
    '--speed meter--
    col = uilook(uiTimeBar): IF ready(i) = 1 THEN col = uilook(uiTimeBarFull)
    centerfuz 66, 9 + i * 10, 131, 10, 1, dpage
    IF bstat(i).cur.hp > 0 THEN
     j = ctr(i) / 7.7
     IF delay(i) > 0 OR godo(i) > 0 OR (anim >= 0 AND bat.acting = i) THEN
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
    centerfuz 180, 9 + i * 10, 88, 10, 1, dpage
    rectangle 137, 5 + i * 10, lifemeter(i), 9, col, dpage
   END IF
   '--name--
   col = uilook(uiMenuItem): IF i = bat.hero_turn THEN col = uilook(uiSelectedItem + tog)
   edgeprint batname$(i), 128 - LEN(batname$(i)) * 8, 5 + i * 10, col, dpage
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
   centerbox 160, 186, 310, 14, 1, dpage
   edgeprint battlecaption, xstring(battlecaption, 160), 182, uilook(uiText), dpage
  END IF
 END IF
 IF bat.hero_turn >= 0 THEN
  centerbox 268, 5 + (4 * (mend(bat.hero_turn) + 2)), 88, 8 * (mend(bat.hero_turn) + 2), 1, dpage
  FOR i = 0 TO mend(bat.hero_turn)
   bg = 0
   fg = uilook(uiMenuItem)
   IF pt = i THEN
     fg = uilook(uiSelectedItem + tog)
     bg = uilook(uiHighlight)
   END IF
   IF readbit(menubits(), 0, bat.hero_turn * 4 + i) THEN
     fg = uilook(uiDisabledItem)
     IF pt = i THEN fg = uilook(uiSelectedDisabled + tog)
   END IF
   textcolor fg, bg
   printstr menu$(bat.hero_turn, i), 228, 9 + i * 8, dpage
  NEXT i
  IF ptarg = 0 AND readbit(gen(), genBits, 14) = 0 THEN
   edgeprint CHR$(24), bslot(bat.hero_turn).x + (bslot(bat.hero_turn).w / 2) - 4, bslot(bat.hero_turn).y - 5 + (tog * 2), uilook(uiSelectedItem + tog), dpage
  END IF
  IF bat.menu_mode = batMENUSPELL THEN '--draw spell menu
   centerbox 160, 52, 310, 94, 1, dpage
   IF sptr < 24 THEN
    IF speld$(sptr) <> "" THEN rectangle 5, 74, 311, 1, uilook(uiTextBox + 1), dpage
   END IF
   rectangle 5, 87, 310, 1, uilook(uiTextBox + 1), dpage
   FOR i = 0 TO 23
    textcolor uilook(uiDisabledItem - readbit(spelmask(), 0, i)), 0
    IF sptr = i THEN textcolor uilook(uiSelectedDisabled - (2 * readbit(spelmask(), 0, i)) + tog), uilook(uiHighlight)
    printstr spel$(i), 16 + (i MOD 3) * 104, 8 + (i \ 3) * 8, dpage
   NEXT i
   textcolor uilook(uiMenuItem), 0
   IF sptr = 24 THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
   printstr cancelspell$, 9, 90, dpage
   textcolor uilook(uiDescription), 0
   IF sptr < 24 THEN
    printstr speld$(sptr), 9, 77, dpage
    printstr cost$(sptr), 308 - LEN(cost$(sptr)) * 8, 90, dpage
   END IF
  END IF
  IF bat.menu_mode = batMENUITEM THEN '--draw item menu
   centerbox 160, 43, 304, 78, 1, dpage
   FOR i = itop TO itop + 26
    textcolor uilook(uiDisabledItem - readbit(iuse(), 0, i)), 0
    IF iptr = i THEN textcolor uilook(uiSelectedDisabled - (2 * readbit(iuse(), 0, i)) + tog), uilook(uiHighlight)
    printstr inventory(i).text, 20 + 96 * (i MOD 3), 8 + 8 * ((i - itop) \ 3), dpage
   NEXT i
   centerbox 160, 88, 304, 12, 1, dpage
   textcolor uilook(uiDescription), 0
   printstr itemd$, 12, 85, dpage
  END IF
  IF ptarg > 0 THEN
   FOR i = 0 TO 11
    IF targs(i) = 1 OR tptr = i THEN
     edgeprint CHR$(24), bslot(i).x + (bslot(i).w / 2) - 4, bslot(i).y - 6, uilook(uiSelectedItem + tog), dpage
     edgeprint batname$(i), xstring(batname$(i), bslot(i).x + (bslot(i).w / 2)), bslot(i).y - 16, uilook(uiSelectedItem + tog), dpage
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
    triggerfade i, bstat(), bslot(), ebits()
    GOSUB fulldeathcheck
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
    triggerfade i, bstat(), bslot(), ebits()
    GOSUB fulldeathcheck
    '--WARNING: WITH pointer probably corrupted
   END IF
  END IF
 END WITH

 '--if not doing anything, not dying, not ready, and not stunned
 IF godo(i) = 0 AND bslot(i).dissolve = 0 AND ready(i) = 0 AND bstat(i).cur.stun = bstat(i).max.stun THEN
  '--increment ctr by speed
  ctr(i) = small(1000, ctr(i) + bstat(i).cur.spd)
  IF ctr(i) = 1000 AND wf = 0 THEN ready(i) = 1
 END IF

NEXT i

'--decrement stun and mute

IF TIMER > laststun + 1 THEN
 FOR i = 0 TO 11
  bstat(i).cur.mute = small(bstat(i).cur.mute + 1, bstat(i).max.mute)
  bstat(i).cur.stun = small(bstat(i).cur.stun + 1, bstat(i).max.stun)
  IF bstat(i).cur.stun < bstat(i).max.stun THEN
   ready(i) = 0
   IF bat.hero_turn = i THEN bat.hero_turn = -1
   IF bat.enemy_turn = i THEN bat.enemy_turn = -1
  END IF
 NEXT i
 laststun = TIMER
END IF

RETRACE

animate:
FOR i = 0 TO 3
 IF walk(i) = 1 THEN of(i) = of(i) XOR tog : bslot(i).frame = bslot(i).frame xor tog
 IF bat.acting <> i AND bstat(i).cur.hp < bstat(i).max.hp / 5 AND vic.state = 0 THEN of(i) = 6 : bslot(i).frame = 6
 IF vic.state > 0 AND bstat(i).cur.hp > 0 AND tog = 0 THEN
  IF of(i) = 0 THEN of(i) = 2 ELSE of(i) = 0
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
  of(i + 12) = aframe(i, fctr(i))
  bslot(i + 12).frame = aframe(i, fctr(i))
  IF atk(2) = 3 THEN of(i + 12) = INT(RND * 3) : bslot(i + 12).frame = INT(RND * 3) 'so what if they won't both be the same?
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
  IF is_hero(i) THEN of(i) = 7 : bslot(i).frame = 7
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
		if battle_draw_style = 1 then
			temp = 64 + (zbuf(i) - 4) * 10
			IF is_hero(zbuf(i)) THEN temp = zbuf(i) * 16
			IF is_attack(zbuf(i)) THEN temp = 144
			IF is_weapon(zbuf(i)) THEN temp = 156
			loadsprite buffer(), 0, of(zbuf(i)) * (bslot(zbuf(i)).w * bslot(zbuf(i)).h * .5), temp, bslot(zbuf(i)).w, bslot(zbuf(i)).h, 3
			IF bslot(zbuf(i)).d = 0 THEN
				drawsprite buffer(), 0, pal16(), p(zbuf(i)) * 16, bslot(zbuf(i)).x, bslot(zbuf(i)).y - bslot(zbuf(i)).z, dpage
			ELSE
				wardsprite buffer(), 0, pal16(), p(zbuf(i)) * 16, bslot(zbuf(i)).x, bslot(zbuf(i)).y - bslot(zbuf(i)).z, dpage
			END IF
		else
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
		end if
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
 info$ = "v=" & bslot(i).vis & " dly=" & delay(i) & " tm=" & tmask(i) & " hp=" & bstat(i).cur.hp & " dis=" & bslot(i).dissolve
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
 IF checkNoRunBit(bstat(), ebits(), bslot()) THEN
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
  godo(i) = 0
  ready(i) = 0
  ctr(i) = large(0, ctr(i) - bstat(i).cur.spd * 2)
 NEXT i
 IF carray(6) = 0 THEN flee = 0: FOR i = 0 TO 3: bslot(i).d = 0: walk(i) = 0: NEXT i
 temp = 400
 FOR i = 4 TO 11
  temp = temp + bstat(i).cur.spd
 NEXT i
 IF RND * temp < flee THEN away = 1: flee = 2: FOR i = 0 TO 3: ctr(i) = 0: ready(i) = 0: NEXT i
END IF
RETRACE

loadall:
setpicstuf formdata(), 80, -1
loadset tmpdir & "for.tmp", form, 0

for i = 0 to 24
	bslot(i).frame = 0
	bslot(i).sprites = 0
	bslot(i).pal = 0
next

mapsong = presentsong
IF formdata(33) = 0 THEN fademusic 0
IF formdata(33) > 0 THEN wrappedsong formdata(33) - 1
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  loadherodata @st(i), hero(i) - 1
  oldm = 0
  newm = 0
  'Loop through hero battle menu, populating nmenu() with he ones that should be displayed
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
  
  setpicstuf buffer(), 5120, 3
  loadset game + ".pt0", exstat(i, 0, 14), i * 16
  getpal16 pal16(), 40 + i, exstat(i, 0, 15), 0, exstat(i, 0, 14)
  WITH bslot(i)
   .basex = (240 + i * 8)
   .basey = (82 + i * 20)
   .x =  .basex
   .y =  .basey
   .w = 32
   .h = 40
   p(i) = 40 + i
   .vis = 1
   'load hero sprites
   .sprite_num = 8
   .sprites = sprite_load(game & ".pt0", exstat(i, 0, 14), .sprite_num, 32, 40)
   if not sprite_is_valid(.sprites) then debug "Couldn't load hero sprite: " & game & ".pt0#" & exstat(i,0,14)
   .pal = palette16_load(game + ".pal", exstat(i, 0, 15), 0, exstat(i, 0, 14))
   if .pal = 0 then debug "Failed to load palette (#" & i & ")"
   .frame = 0
   .death_sfx = -1 'No death sounds for heroes (for now)
  END WITH
  FOR o = 0 TO 11
   bstat(i).cur.sta(o) = exstat(i, 0, o)
   bstat(i).max.sta(o) = exstat(i, 1, o)
  NEXT o
  herobattlebits bits(), i
  batname$(i) = names(i)
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
  BSLOT(I).sprites = 0
 END IF
NEXT i
FOR i = 0 TO 7
 loadfoe i, formdata(), es(), bslot(), p(), bits(), bstat(), ebits(), batname$(), rew, YES
NEXT i
FOR i = 0 TO 11
 ctr(i) = INT(RND * 500)
 t(i, 12) = -1
NEXT i
FOR i = 12 TO 23
 bslot(i).w = 50
 bslot(i).h = 50
NEXT i
curbg = formdata(32)
loadpage game + ".mxs", curbg, 2
FOR i = 0 TO 3
 IF bstat(i).cur.hp < bstat(i).max.hp / 5 AND vic.state = 0 THEN of(i) = 6 : bslot(i).frame = 6
 IF hero(i) > 0 AND bstat(i).cur.hp = 0 THEN
  '--hero starts the battle dead
  bslot(i).dissolve = 1 'Keeps the dead hero from vanishing
  of(i) = 7
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
  triggerfade i, bstat(), bslot(), ebits()
 END IF
NEXT i
GOSUB fulldeathcheck
RETRACE

END FUNCTION

'FIXME: This affects the rest of the file. Move it up as above functions are cleaned up
OPTION EXPLICIT

SUB trigger_victory(BYREF vic AS VictoryState, BYREF rew AS RewardsState, bstat() As BattleStats, exstat() AS INTEGER)
 DIM i AS INTEGER
 '--Play the victory music
 IF gen(genVictMus) > 0 THEN fademusic fmvol: wrappedsong gen(genVictMus) - 1
 '--Collect gold (and cap out at 1 billion max)
 gold = gold + rew.plunder
 IF gold > 1000000000 THEN gold = 1000000000
 '--Divide experience by heroes
 IF liveherocount(bstat()) > 0 THEN rew.exper = rew.exper / liveherocount(bstat())
 '--Collect experience and apply levelups
 FOR i = 0 TO 3
  IF bstat(i).cur.hp > 0 THEN giveheroexperience i, exstat(), rew.exper
  updatestatslevelup i, exstat(), bstat(), 0
 NEXT i
 '--Trigger the display of end-of-battle rewards
 vic.state = vicGOLDEXP
END SUB

SUB show_victory (BYREF vic AS VictoryState, BYREF rew AS RewardsState, exstat() AS INTEGER, batname() AS STRING)
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
     tempstr = vic.level_up_caption & " " & batname(i)
    CASE IS > 1
     tempstr = exstat(i, 1, 12) & " " & vic.levels_up_caption & " " & batname(i)
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
     vic.item_name = batname(vic.learnwho) + vic.learned_caption
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

SUB checkitemusability(iuse() AS INTEGER)
 'Iterate through the iuse() bitfield and mark any items that are usable
 DIM i AS INTEGER
 DIM itemtemp(100) AS INTEGER

 FOR i = 0 TO inventoryMax
  IF inventory(i).used THEN
   loaditemdata itemtemp(), inventory(i).id
   IF itemtemp(47) > 0 THEN setbit iuse(), 0, i, 1
  END IF
 NEXT i
END SUB

FUNCTION checkNoRunBit (bstat() AS BattleStats, ebits(), bslot() AS BattleSprite)
 DIM i AS INTEGER
 FOR i = 4 TO 11
  IF bstat(i).cur.hp > 0 AND bslot(i).vis = 1 AND readbit(ebits(), (i - 4) * 5, 57) = 1 THEN RETURN 1
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

FUNCTION focuscost (cost, focus)
IF focus > 0 THEN
 focuscost = cost - INT(cost / (100 / focus))
ELSE
 focuscost = cost
END IF
END FUNCTION

SUB herobattlebits (bitbuf(), who)
DIM i AS INTEGER
DIM j AS INTEGER

'--native bits
FOR i = 0 TO 4
 bitbuf(who, i) = nativehbits(who, i)
NEXT i

'--equipment bits
FOR j = 0 TO 4
 IF eqstuf(who, j) > 0 THEN
  loaditemdata buffer(), eqstuf(who, j) - 1
  FOR i = 0 TO 4
   bitbuf(who, i) = (bitbuf(who, i) OR buffer(70 + i))
  NEXT i
 END IF
NEXT j

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

SUB anim_inflict(who)
 pushw 10: pushw who
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

FUNCTION count_dissolving_enemies(bslot() AS BattleSprite) AS INTEGER
 DIM i AS INTEGER
 DIM count AS INTEGER = 0
 FOR i = 4 TO 11
  IF bslot(i).dissolve > 0 THEN count += 1
 NEXT i
 RETURN count
END FUNCTION

SUB spawn_on_death(deadguy AS INTEGER, killing_attack AS INTEGER, es(), atktype(), formdata(), bslot() AS BattleSprite, p(), bits(), bstat() AS BattleStats, ebits(), batname$(), BYREF rew AS RewardsState)
'atktype() is an old hack for elemental checking. It will be replaced with killing_attack eventually
 'killing_attack is the id+1 of the attack that killed the target or 0 if the target died without a specific attack
 IF NOT is_enemy(deadguy) THEN EXIT SUB ' Only works for enemies
 DIM slot AS INTEGER
 DIM i AS INTEGER
 IF es(deadguy - 4, 80) > 0 AND atktype(0) = 1 THEN ' spawn on non-elemental death
  FOR i = 1 TO es(deadguy - 4, 91)
   slot = find_empty_enemy_slot(formdata())
   IF slot > -1 THEN
    formdata(slot * 4) = es(deadguy - 4, 80)
    loadfoe(slot, formdata(), es(), bslot(), p(), bits(), bstat(), ebits(), batname$(), rew)
   END IF
  NEXT i
  es(deadguy - 4, 80) = 0
 END IF
 IF es(deadguy - 4, 79) > 0 THEN ' spawn on death
  FOR i = 1 TO es(deadguy - 4, 91)
   slot = find_empty_enemy_slot(formdata())
   IF slot > -1 THEN
    formdata(slot * 4) = es(deadguy - 4, 79)
    loadfoe(slot, formdata(), es(), bslot(), p(), bits(), bstat(), ebits(), batname$(), rew)
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

FUNCTION dieWOboss(BYVAL who, bstat() AS BattleStats, ebits())
 DIM AS INTEGER j
 '--count bosses
 FOR j = 4 TO 11
  '--is it a boss?
  IF readbit(ebits(), (j - 4) * 5, 56) = 1 THEN
   '-- is it alive?
   IF bstat(j).cur.hp > 0 THEN
    RETURN NO
   END IF
  END IF
 NEXT j
 '--if there are no bossess...
 '--should it die without a boss?
 IF readbit(ebits(), (who - 4) * 5, 58) = 1 THEN
  bstat(who).cur.hp = 0
  RETURN YES
 END IF
END FUNCTION

SUB triggerfade(BYVAL who, bstat() AS BattleStats, bslot() AS BattleSprite, ebits())
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
   IF readbit(ebits(), (who - 4) * 5, 59) = 1 THEN
    bslot(who).flee = 1
    'the number of ticks it takes an enemy to run away is based on its distance
    'from the left side of the screen and its width. Enemys flee at 10 pixels per tick
    bslot(who).dissolve = (bslot(who).w + bslot(who).x) / 10
   END IF
  END IF
 END IF
END SUB

SUB check_death(deadguy AS INTEGER, BYVAL killing_attack AS INTEGER, BYREF bat AS BattleState, noifdead AS INTEGER, BYREF rew AS RewardsState, BYREF tcount AS INTEGER, bstat() AS BattleStats, bslot() AS BattleSprite, ready(), godo(), es(), atktype(), formdata(), p(), bits(), ebits(), batname$(), t(), autotmask(), revenge(), revengemask(), targmem(), BYREF tptr AS INTEGER, BYREF ptarg AS INTEGER, ltarg(), tmask(), targs())
'killing_attack is not used yet, but will contain attack id + 1 or 0 when no attack is relevant.
 DIM AS INTEGER j,k 'for loop counters

 IF is_enemy(deadguy) THEN
  IF formdata((deadguy - 4) * 4) = 0 THEN EXIT SUB
 END IF
 IF bstat(deadguy).cur.hp <> 0 THEN EXIT SUB
 '--deadguy is really dead (includes already dead and empty hero slots??)
 'Death animation is not done yet here, so be cautios about what gets cleand up here.
 'Full cleanup of bslot() records belongs in loadfoe
 bslot(deadguy).vis = 0
 ready(deadguy) = 0
 godo(deadguy) = 0
 bslot(deadguy).d = 0
 '--reset poison/regen/stun/mute
 WITH bstat(deadguy)
  .cur.poison = .max.poison
  .cur.regen  = .max.regen
  .cur.stun   = .max.stun
  .cur.mute   = .max.mute
 END WITH
 '-- if it is a dead hero's turn, cancel menu
 IF bat.hero_turn = deadguy THEN bat.hero_turn = -1: bat.menu_mode = batMENUHERO
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
  dead_enemy deadguy, rew, bstat(), bslot(), es(), atktype(), formdata(), p(), bits(), ebits(), batname$()
 END IF'------------END PLUNDER-------------------
 IF noifdead = 0 THEN '---THIS IS NOT DONE FOR ALLY+DEAD------
  tcount = tcount - 1
  FOR j = 0 TO 11
   '--Search through each hero and enemy to see if any of them are targetting the
   '--guy who just died
   t(j, 12) = -1
   FOR k = 0 TO 11
    '--sort dead target away
    IF t(j, k) = deadguy AND readbit(ltarg(), j, deadguy) = 0 THEN SWAP t(j, k), t(j, k + 1)
   NEXT k
   IF t(j, 0) = -1 AND bat.acting <> j AND godo(j) > 0 THEN
    'if no targets left, auto-re-target
    loadattackdata buffer(), godo(j) - 1
    get_valid_targs autotmask(), j, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
    autotarget t(), autotmask(), j, buffer(), bslot(), bstat()
   END IF
   IF tmask(deadguy) = 1 THEN tmask(deadguy) = 0
   IF targs(deadguy) = 1 THEN targs(deadguy) = 0
  NEXT j
  IF tptr = deadguy THEN
   WHILE tmask(tptr) = 0
    tptr = tptr + 1: IF tptr > 11 THEN ptarg = 0: EXIT SUB
   WEND
  END IF
 END IF  '----END ONLY WHEN NOIFDEAD = 0
END SUB

SUB dead_enemy(deadguy AS INTEGER, BYREF rew AS RewardsState, bstat() AS BattleStats, bslot() AS BattleSprite, es(), atktype(), formdata(), p(), bits(), ebits(), batname$())
 '--give rewards, spawn enemies, clear formdata slot, but NO other cleanup!
 DIM AS INTEGER j
 DIM enemynum AS INTEGER = deadguy - 4
 '--spawn enemies before freeing the formdata slot to avoid infinite loops. however this might need to be changed to fix morphing enemies?
 spawn_on_death deadguy, 0, es(), atktype(), formdata(), bslot(), p(), bits(), bstat(), ebits(), batname$(), rew
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
