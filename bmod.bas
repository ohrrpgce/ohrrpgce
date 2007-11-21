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
#include "music.bi"
DECLARE FUNCTION count_available_spells(who AS INTEGER, list AS INTEGER) AS INTEGER

'misc
#include "common.bi"
#INCLUDE "compat.bi"
#INCLUDE "gglobals.bi"
#INCLUDE "const.bi"
#INCLUDE "uiconst.bi"

'--local subs and functions
DECLARE FUNCTION count_dissolving_enemies(bslot() AS BattleSprite) AS INTEGER

'these are the battle global variables
DIM battlecaption$, battlecaptime, battlecapdelay, bstackstart, learnmask(29)

dim shared battle_draw_style as integer = 0'0 = old, 1 = new, 2 = toggle
dim shared battle_draw_toggle as integer = 0

REM $STATIC
FUNCTION battle (form, fatal, exstat())

REMEMBERSTATE

'--prepare stack
bstackstart = stackpos

battle = 1
DIM formdata(40), atktemp(40 + dimbinsize(binATTACK)), atk(40 + dimbinsize(binATTACK)), wepatk(40 + dimbinsize(binATTACK)), wepatkid, st(3) as herodef, es(7, 160), zbuf(24),  p(24), of(24), ext$(7), ctr(11)
DIM ready(11), batname$(11), menu$(3, 5), menubits(2), mend(3), spel$(23), speld$(23), spel(23), cost$(23), godo(11), delay(11), cycle(24), walk(3), aframe(11, 11)
DIM fctr(24), harm$(11), hc(23), hx(11), hy(11), conlmp(11), bits(11, 4), atktype(8), iuse(15), icons(11), ebits(40), eflee(24), ltarg(11), found(16, 1), lifemeter(3), revenge(11), revengemask(11), revengeharm(11), repeatharm(11 _
), targmem(23), prtimer(11,1), spelmask(1)
DIM laststun AS DOUBLE
DIM bslot(24) AS BattleSprite
DIM bstat(11) AS BattleStats
DIM as double timinga, timingb
DIM dead, mapsong
DIM spellcount AS INTEGER '--only used in heromenu GOSUB block
DIM listslot AS INTEGER
DIM nmenu(3,5) as integer 'new battle menu

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
goldname$ = readglobalstring(32, "Gold", 10)
expname$ = readglobalstring(33, "Exp", 10)
cancelspell$ = readglobalstring$(51, "(CANCEL)", 10)
pause$ = readglobalstring$(54, "PAUSE", 10)
learned$ = " " + readglobalstring$(124, "learned", 10) + " "
goldcap$ = readglobalstring$(125, "Found", 10)
expcap$ = readglobalstring$(126, "Gained", 10)
foundcap$ = readglobalstring$(139, "Found a", 20)
foundpcap$ = readglobalstring$(141, "Found", 20)
cannotrun$ = readglobalstring$(147, "CANNOT RUN!", 20)
level1up$ = readglobalstring$(149, "Level up for", 20)
levelXup$ = readglobalstring$(151, "levels for", 20)

battlecaptime = 0
battlecapdelay = 0
battlecaption$ = ""

alert = 0
alert$ = ""

fadeout 240, 240, 240
vpage = 0: dpage = 1: needf = 1: anim = -1: you = -1: them = -1: fiptr = 0
vdance = 0: drawvicbox = 0: aset = 0: wf = 0: noifdead = 0

ptarg = 0 ' ptarg=0 means hero not currently picking a target
          ' ptarg>0 means hero picking a target
          ' ptarg=1 means targetting needs set-up
          ' ptarg=2 means normal manual targetting
          ' ptarg=3 means autotargeting

FOR i = 0 TO 11
 icons(i) = -1
 revenge(i) = -1
NEXT i
GOSUB checkitemusability
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
 setwait timing(), speedcontrol
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
   loadpage game$ + ".mxs", curbg, 2
  END IF
 END IF

 IF readbit(gen(), 101, 8) = 0 THEN
  '--debug keys
  IF keyval(62) > 1 THEN away = 11 ' Instant-cheater-running
  IF keyval(63) > 1 THEN exper& = 1000000  'Million experience!
  IF keyval(41) > 1 THEN battle_draw_style = (battle_draw_style + 1) mod 3
  if battle_draw_style = 2 then battle_draw_toggle = NOT battle_draw_toggle
 END IF
 IF keyval(69) > 1 THEN GOSUB pgame '--PAUSE
 '--running away
 IF carray(5) > 1 THEN
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
 IF keyval(87) > 1 AND readbit(gen(), 101, 8) = 0 THEN vis = vis XOR 1
 IF anim >= 0 AND aset = 0 AND vdance = 0 THEN GOSUB atkscript
 IF anim >= 0 AND aset = 1 AND vdance = 0 AND away = 0 THEN GOSUB action
 GOSUB animate
 na = loopvar(na, 0, 11, 1)
 IF anim = -1 AND vdance = 0 THEN
  GOSUB meters
  IF godo(na) > 0 AND delay(na) = 0 THEN
   '--next attacker has an attack selected and the delay is over
   anim = godo(na) - 1
   who = na
   aset = 0
   godo(na) = 0
  END IF
 END IF
 yn = loopvar(yn, 0, 3, 1)
 IF you = -1 THEN
  '--if it is no heros turn, check to see if anyone is alive and ready
  IF ready(yn) = 1 AND bstat(yn).cur.hp > 0 AND dead = 0 THEN
   you = yn
   pt = 0
   mset = 0
  END IF
 END IF
 en = loopvar(en, 4, 11, 1)
 IF them = -1 THEN
  IF ready(en) = 1 AND bstat(en).cur.hp > 0 AND dead = 0 THEN them = en
 END IF
 IF vdance = 0 THEN
  IF them >= 0 THEN GOSUB enemyai
  IF you >= 0 AND ptarg = 0 THEN
   IF mset = 2 THEN GOSUB itemmenu
   IF mset = 1 THEN GOSUB spellmenu
   IF mset = 0 THEN GOSUB heromenu
  END IF
  IF you >= 0 AND ptarg > 0 THEN GOSUB picktarg
 END IF
 GOSUB sprite
 GOSUB display
 IF vdance = -1 THEN vdance = -2
 IF vdance > 0 THEN GOSUB vicdance
 IF vis = 1 THEN GOSUB seestuff
 IF dead = 1 AND vdance = 0 THEN
  IF count_dissolving_enemies(bslot()) = 0 THEN GOSUB victory
 END IF
 IF vdance = -2 THEN EXIT DO 'normal victory exit
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
  IF formdata(33) >= 0 THEN fademusic fmvol
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
IF bstat(them).cur.hp < bstat(them).max.hp / 5 THEN ai = 1

'if targetable enemy count is 1, go into alone mode
IF targenemycount(bslot(), bstat()) = 1 THEN ai = 2

'spawn allys when alone
IF ai = 2 AND es(them - 4, 81) THEN
 FOR j = 1 TO es(them - 4, 91)
  slot = -1
  FOR k = 7 TO 0 STEP -1
   IF formdata(k * 4) = 0 THEN slot = k
  NEXT k
  IF slot > -1 THEN
   formdata(slot * 4) = es(them - 4, 81)
   loadfoe slot, formdata(), es(), bslot(), p(), ext$(), bits(), bstat(), ebits(), batname$()
  END IF
 NEXT j
END IF

'make sure that the current ai set is valid
'otherwise fall back on another
IF countai(ai, them, es()) = 0 THEN
 ai = 0
 IF bstat(them).cur.hp < bstat(them).max.hp / 5 THEN
  ai = 1
  IF countai(ai, them, es()) = 0 THEN ai = 0
 END IF
END IF

'if no valid ai set is available, the enemy loses its turn
IF countai(ai, them, es()) = 0 THEN them = -1: RETRACE

'pick a random attack
lim = 0
DO
 godo(them) = es(them - 4, 92 + (ai * 5) + INT(RND * 5))
 IF godo(them) > 0 THEN
  'load the data for this attack
  loadattackdata atktemp(), godo(them) - 1
  IF atkallowed(atktemp(), them, 0, 0, bstat()) THEN
   'this attack is good, continue on to target selection
   EXIT DO
  ELSE
   'this attack is unusable
   godo(them) = 0
   IF bstat(them).cur.mp - atktemp(8) < 0 THEN
    'inadequate MP was the reason for the failure
    'MP-idiot loses its turn
    IF readbit(ebits(), (them - 4) * 5, 55) THEN
      ready(them) = 0
      ctr(them) = 0
      them = -1
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
  them = -1
  RETRACE
 END IF
LOOP

'get the delay to wait for this attack
delay(them) = atktemp(16)

get_valid_targs autotmask(), them, atktemp(), bslot(), bstat(), revenge(), revengemask(), targmem()
autotarget t(), autotmask(), them, atktemp(), bslot(), bstat()

'ready for next attack
ready(them) = 0
ctr(them) = 0
them = -1

RETRACE


heromenu: '-----------------------------------------------------------------
FOR i = 0 TO 5
 setbit menubits(), 0, you*4+i, 0
 IF nmenu(you, i) > 0 THEN
  'IF foo then
   IF wepatkid <> nmenu(you, i) THEN
    wepatkid = nmenu(you, i)
    loadattackdata wepatk(), wepatkid - 1
   END IF
   IF readbit(wepatk(), 65, 6) THEN
    IF atkallowed(wepatk(), you, 0, 0, bstat()) = 0 THEN
     setbit menubits(), 0, you*4+i, 1
    END IF
   END IF
  'END IF
 END IF
NEXT i
IF carray(5) > 1 THEN yn = you: you = -1: RETRACE
IF carray(0) > 1 THEN pt = pt - 1: IF pt < 0 THEN pt = mend(you)
IF carray(1) > 1 THEN pt = pt + 1: IF pt > mend(you) THEN pt = 0
IF carray(4) > 1 THEN
 IF nmenu(you, pt) > 0 THEN 'simple attack
  IF readbit(menubits(), 0, you*4+pt) = 0 THEN
   godo(you) = nmenu(you, pt)
   loadattackdata buffer(), godo(you) - 1
   delay(you) = large(buffer(16), 1)
   ptarg = 1
   flusharray carray(), 7, 0
   RETRACE
  END IF
 END IF
 IF nmenu(you, pt) < 0 AND nmenu(you, pt) >= -4 THEN '--this is a spell list
  listslot = (nmenu(you, pt) + 1) * -1
  IF st(you).list_type(listslot) < 2 THEN '--the type of this spell list is one that displays a menu
   '--init spell menu
   mset = 1: sptr = 0
   FOR i = 0 TO 23 '-- loop through the spell list setting up menu items for each
    spel$(i) = ""
    speld$(i) = ""
    cost$(i) = ""
    spel(i) = -1
    setbit spelmask(), 0, i, 0
    IF spell(you, listslot, i) > 0 THEN '--there is a spell in this slot
     spellcount += 1
     spel(i) = spell(you, listslot, i) - 1
     loadattackdata atktemp(), spel(i)
     spel$(i) = readbadbinstring$(atktemp(), 24, 10, 1)
     speld$(i) = readbinstring$(atktemp(), 73, 38)
     IF st(you).list_type(listslot) = 0 THEN
      '--regular MP
      cost$(i) = XSTR$(focuscost(atktemp(8), bstat(you).cur.foc)) + " " + mpname$ + " " + STR$(bstat(you).cur.mp) + "/" + STR$(bstat(you).max.mp)
     END IF
     IF st(you).list_type(listslot) = 1 THEN
      '--level MP
      cost$(i) = "Level" + XSTR$(INT(i / 3) + 1) + ":  " + XSTR$(lmp(you, INT(i / 3)))
     END IF
     IF atkallowed(atktemp(), you, st(you).list_type(listslot), INT(i / 3), bstat()) THEN
      '-- check whether or not the spell is allowed
      setbit spelmask(), 0, i, 1
     END IF
    END IF
    spel$(i) = rpad$(spel$(i), " ", 10) '-- pad the spell menu caption
   NEXT i
  ELSEIF st(you).list_type(listslot) = 2 THEN '-- this is a random spell list
   spellcount = 0
   FOR i = 0 TO 23 '-- loop through the spell list storing attack ID numbers
    spel(i) = -1
    IF spell(you, listslot, i) > 0 THEN '--there is a spell in this slot
     spellcount += 1
     spel(i) = spell(you, listslot, i) - 1
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
    godo(you) = spel(rptr) + 1
    loadattackdata buffer(), godo(you) - 1
    delay(you) = large(buffer(16), 1)
    ptarg = 1
    flusharray carray(), 7, 0
   END IF
  END IF
 ELSEIF nmenu(you, pt) = -10 THEN
  mset = 2: iptr = 0: itop = 0
 END IF
END IF
RETRACE

atkscript: '---------------------------------------------------------------
'--check for item consumption
IF icons(who) >= 0 THEN
 IF inventory(icons(who)).used = 0 THEN
  '--abort if item is gone
  anim = -1: RETRACE
 END IF
END IF
'--load attack
loadattackdata atk(), anim
'--load picture
setpicstuf buffer(), 3750, 3
loadset game$ + ".pt6", atk(0), 144

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
		.sprites = sprite_load(game$ + ".pt6", atk(0), 3, 50, 50)
		if .sprites = 0 then debug "Failed to load attack sprites (#" & i & ")"
		.pal = palette16_load(game$ + ".pal", atk(1), 6, atk(0))
		if .pal = 0 then debug "Failed to load palette (#" & i & ")"
 end with
NEXT i
tcount = -1: pdir = 0: conmp = 1
IF is_enemy(who) THEN pdir = 1
ltarg(who) = 0
'CANNOT HIT INVISIBLE FOES
FOR i = 0 TO 11
 IF t(who, i) > -1 THEN
  IF bslot(t(who, i)).vis = 0 AND (atk(3) <> 4 AND atk(3) <> 10) THEN
   t(who, i) = -1
  END IF
 END IF
NEXT i
'MOVE EMPTY TARGET SLOTS TO THE BACK
FOR o = 0 TO 10
 FOR i = 0 TO 10
  IF t(who, i) = -1 THEN SWAP t(who, i), t(who, i + 1)
 NEXT i
NEXT o
'COUNT TARGETS
FOR i = 0 TO 11
 IF t(who, i) > -1 THEN tcount = tcount + 1
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
targmem(who) = 0
' BIG CRAZY SCRIPT CONSTRUCTION
'DEBUG debug "begin script construction"
IF is_hero(who) THEN
 'load weapon sprites
 setpicstuf buffer(), 576, 3
 loadset game$ + ".pt5" , exstat(who, 0, 13), 156
 p(24) = 52
 getpal16 pal16(), 52, exstat(who, 1, 13), 5, exstat(who, 0, 13)
 
 with bslot(24)
  .sprite_num = 2
  .sprites = sprite_load(game$ & ".pt5", exstat(who, 0, 13), 2, 24, 24)
  if .sprites = 0 then debug "Could not load weapon sprite: " & game$ & ".pt5#" & exstat(who, 0, 13)
  .pal = palette16_load(game$ + ".pal", exstat(who, 1, 13), 5, exstat(who, 0, 13))
  if .pal = 0 then debug "Failed to load palette (#" & 24 & ")"
  .frame = 0
  
  
 end with
END IF
numhits = atk(17) + INT(RND * (bstat(who).cur.hits + 1))
IF readbit(atk(), 20, 49) THEN numhits = atk(17)
'----------------------------NULL ANIMATION
IF atk(15) = 10 THEN
 advance who, atk(), bslot(), t()
 if atk(99) > 0  then anim_sound(atk(99) - 1)
 FOR j = 1 TO numhits
  IF is_hero(who) THEN heroanim who, atk(), bslot(), t()
  IF is_enemy(who) THEN etwitch who, atk(), bslot(), t()
  FOR i = 0 TO tcount
   anim_inflict t(who,i)
  NEXT i
  anim_disappear 24
 NEXT j
 retreat who, atk(), bslot(), t()
 anim_end
END IF
'----------------------------NORMAL, DROP, SPREAD-RING, and SCATTER
IF atk(15) = 0 OR atk(15) = 3 OR atk(15) = 6 OR (atk(15) = 4 AND tcount > 0) THEN
 FOR i = 0 TO tcount
  yt = (bslot(t(who, i)).h - 50) + 2
  xt = 0: IF t(who, i) = who AND is_hero(who) AND atk(14) <> 7 THEN xt = -20
  anim_setpos 12 + i, bslot(t(who, i)).x + xt, bslot(t(who, i)).y + yt, pdir
  IF atk(15) = 3 THEN
   anim_setz 12 + i, 180
  END IF
  IF atk(15) = 4 THEN
   anim_setpos 12 + i, bslot(t(who, i)).x + xt, bslot(t(who, i)).y + yt - bslot(t(who, i)).w, pdir
  END IF
 NEXT i
 advance who, atk(), bslot(), t()
 FOR j = 1 TO numhits
  IF is_hero(who) THEN heroanim who, atk(), bslot(), t()
  IF is_enemy(who) THEN etwitch who, atk(), bslot(), t()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   IF atk(15) = 4 THEN
    anim_absmove 12 + i, bslot(t(who, i)).x + xt - bslot(t(who, i)).w, bslot(t(who, i)).y + yt, 3, 3
   END IF
   IF atk(15) = 3 THEN
    anim_zmove 12 + i, -10, 20
   END IF
   IF atk(15) = 6 THEN
    anim_absmove 12 + i, INT(RND * 270), INT(RND * 150), 6, 6
   END IF
  NEXT i
  anim_wait 2
  IF atk(15) = 3 THEN
   anim_wait 3
  END IF
  anim_setframe who, 0
  anim_disappear 24
  IF atk(15) = 4 THEN
   FOR i = 0 TO tcount
    anim_absmove 12 + i, bslot(t(who, i)).x + xt, bslot(t(who, i)).y + yt + bslot(t(who, i)).w, 3, 3
   NEXT i
   anim_waitforall
   FOR i = 0 TO tcount
    anim_absmove 12 + i, bslot(t(who, i)).x + xt + bslot(t(who, i)).w, bslot(t(who, i)).y + yt, 3, 3
   NEXT i
   anim_waitforall
   FOR i = 0 TO tcount
    anim_absmove 12 + i, bslot(t(who, i)).x + xt, bslot(t(who, i)).y + yt - bslot(t(who, i)).w, 3, 3
   NEXT i
   anim_waitforall
  END IF
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  FOR i = 0 TO tcount
   anim_inflict t(who, i)
   temp = 3: IF is_enemy(t(who, i)) THEN temp = -3
   anim_setmove t(who, i), temp, 0, 2, 0
   if is_hero(t(who, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(who, i), 5
    END IF
    IF readbit(atk(), 20, 0) = 1 THEN
     anim_setframe t(who, i), 2
    END IF
   end if
  NEXT i
  IF atk(15) <> 4 THEN
   anim_wait 3
  END IF
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   temp = -3: IF is_enemy(t(who, i)) THEN temp = 3
   anim_setmove t(who, i), temp, 0, 2, 0
   anim_setframe t(who, i), 0
  NEXT i
  anim_wait 2
 NEXT j
 retreat who, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(who, i), 0
 NEXT i
 anim_end
END IF
'----------------------------SEQUENTIAL PROJECTILE
IF atk(15) = 7 THEN
 'attacker steps forward
 advance who, atk(), bslot(), t()
 'repeat the following for each attack
 FOR j = 1 TO numhits
  'attacker animates
  IF is_hero(who) THEN heroanim who, atk(), bslot(), t()
  IF is_enemy(who) THEN etwitch who, atk(), bslot(), t()
  'calculate where the projectile will start relative to the attacker
  startoffset = 50: IF is_hero(who) THEN startoffset = -50
  'calculate the direction the projectile should be facing
  atkimgdirection = 0: IF readbit(atk(), 20, 3) = 0 THEN atkimgdirection = pdir
  'set the projectile position
  anim_setpos 12, bslot(who).x + startoffset, bslot(who).y, atkimgdirection
  anim_appear 12
  'play the sound effect
  IF atk(99) > 0 THEN anim_sound(atk(99) - 1)
  'repeat the following for each target...
  FOR i = 0 TO tcount
   'find the target's position
   yt = (bslot(t(who, i)).h - 50) + 2
   xt = 0: IF t(who, i) = who AND is_hero(who) AND atk(14) <> 7 THEN xt = -20
   'make the projectile move to the target
   anim_absmove 12, bslot(t(who, i)).x + xt, bslot(t(who, i)).y + yt, 5, 5
   anim_waitforall
   'inflict damage
   anim_inflict t(who, i)
   'make the target flinch back
   targetflinch = 3: IF is_enemy(t(who, i)) THEN targetflinch = -3
   anim_setmove t(who, i), targetflinch, 0, 2, 0
   'show harm animation
   if is_hero(t(who, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(who, i), 5
    END IF
    IF readbit(atk(), 20, 0) = 1 THEN
     anim_setframe t(who, i), 2
    END IF
   end if
   anim_wait 3
   'recover from flinch
   targetflinch = -3: IF is_enemy(t(who, i)) THEN targetflinch = 3
   anim_setmove t(who, i), targetflinch, 0, 2, 0
   anim_setframe t(who, i), 0
   IF i = 0 THEN
    'attacker's weapon picture vanishes after the first hit
    anim_disappear 24
   END IF
  NEXT i
  'after all hits are done, projectile flies off the side of the screen
  IF is_hero(who) THEN
   anim_absmove 12, -50, 100, 5, 5
  END IF
  IF is_enemy(who) THEN
   anim_absmove 12, 320, 100, 5, 5
  END IF
  anim_waitforall
  'hide projectile
  anim_disappear 12
 NEXT j
 'attacker steps back
 retreat who, atk(), bslot(), t()
 anim_end
END IF
'-----------------PROJECTILE, REVERSE PROJECTILE and METEOR
IF (atk(15) >= 1 AND atk(15) <= 2) OR atk(15) = 8 THEN
 advance who, atk(), bslot(), t()
 FOR j = 1 TO numhits
  FOR i = 0 TO tcount
   temp = 50: IF is_hero(who) THEN temp = -50
   dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
   yt = (bslot(t(who, i)).h - 50) + 2
   xt = 0: IF t(who, i) = who AND is_hero(who) AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 THEN
    anim_setpos 12 + i, bslot(who).x + temp, bslot(who).y, dtemp
   END IF
   IF atk(15) = 2 THEN
    anim_setpos 12 + i, bslot(t(who, i)).x + xt, bslot(t(who, i)).y + yt, dtemp
   END IF
   IF atk(15) = 8 THEN
    IF is_hero(who) THEN
     anim_setpos 12 + i, 320, 100, dtemp
    END IF
    IF is_enemy(who) THEN
     anim_setpos 12 + i, -50, 100, dtemp
    END IF
    anim_setz 12 + i, 180
   END IF
  NEXT i
  IF is_hero(who) THEN heroanim who, atk(), bslot(), t()
  IF is_enemy(who) THEN etwitch who, atk(), bslot(), t()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   temp = 50: IF is_hero(who) THEN temp = -50
   yt = (bslot(t(who, i)).h - 50) + 2
   xt = 0: IF t(who, i) = who AND is_hero(who) AND atk(14) <> 7 THEN xt = -20
   IF atk(15) = 1 OR atk(15) = 8 THEN
    anim_absmove 12 + i, bslot(t(who, i)).x + xt, bslot(t(who, i)).y + yt, 6, 6
   END IF
   IF atk(15) = 2 THEN
    anim_absmove 12 + i, bslot(who).x + temp, bslot(who).y, 6, 6
   END IF
   IF atk(15) = 8 THEN
    anim_zmove 12 + i, -6, 30
   END IF
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 8
  anim_disappear 24
  anim_setframe who, 0
  FOR i = 0 TO tcount
   anim_inflict t(who, i)
   temp = 3: IF is_enemy(t(who, i)) THEN temp = -3
   anim_setmove t(who, i), temp, 0, 2, 0
   if is_hero(t(who, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(who, i), 5
    ELSE
     anim_setframe t(who, i), 2
    END IF
   end if
  NEXT i
  anim_wait 3
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   temp = -3: IF is_enemy(t(who, i)) THEN temp = 3
   anim_setmove t(who, i), temp, 0, 2, 0
   anim_setframe t(who, i), 0
  NEXT i
  anim_wait 3
 NEXT j
 retreat who, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(who, i), 0
 NEXT i
 anim_end
END IF
'--------------------------------------DRIVEBY
IF atk(15) = 9 THEN
 advance who, atk(), bslot(), t()
 FOR j = 1 TO numhits
  dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
  FOR i = 0 TO tcount
   yt = (bslot(t(who, i)).h - 50) + 2
   IF is_hero(who) THEN
    anim_setpos 12 + i, 320, bslot(t(who, i)).y + yt, dtemp
   END IF
   IF is_enemy(who) THEN
    anim_setpos 12 + i, -50, bslot(t(who, i)).y + yt, dtemp
   END IF
  NEXT i
  IF is_hero(who) THEN heroanim who, atk(), bslot(), t()
  IF is_enemy(who) THEN etwitch who, atk(), bslot(), t()
  FOR i = 0 TO tcount
   anim_appear 12 + i
   temp = 50: IF is_hero(who) THEN temp = -50
   yt = (bslot(t(who, i)).h - 50) + 2
   anim_absmove 12 + i, bslot(t(who, i)).x + xt, bslot(t(who, i)).y + yt, 8, 8
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 4
  anim_disappear 24
  anim_setframe who, 0
  anim_waitforall
  FOR i = 0 TO tcount
   anim_inflict t(who, i)
   temp = 3: IF is_enemy(t(who, i)) THEN temp = -3
   anim_setmove t(who, i), temp, 0, 2, 0
   if is_hero(t(who, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(who, i), 5
    ELSE
     anim_setframe t(who, i), 2
    END IF
   end if
   yt = (bslot(t(who, i)).h - 50) + 2
   IF is_hero(who) THEN
    anim_absmove 12 + i, -50, bslot(t(who, i)).y + yt, 5, 7
   END IF
   IF is_enemy(who) THEN
    anim_absmove 12 + i, 320, bslot(t(who, i)).y + yt, 5, 7
   END IF
  NEXT i
  anim_waitforall
  FOR i = 0 TO tcount
   anim_disappear 12 + i
   temp = -3: IF is_enemy(t(who, i)) THEN temp = 3
   anim_setmove t(who, i), temp, 0, 2, 0
   anim_setframe t(who, i), 0
  NEXT i
  anim_wait 3
 NEXT j
 retreat who, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(who, i), 0
 NEXT i
 anim_end
END IF
'--------------------------------FOCUSED RING
IF atk(15) = 4 AND tcount = 0 THEN
 dtemp = 0: IF readbit(atk(), 20, 3) = 0 THEN dtemp = pdir
 advance who, atk(), bslot(), t()
 FOR j = 1 TO numhits
  i = 0
  yt = (bslot(t(who, i)).h - 50) + 2
  xt = 0: IF t(who, i) = who AND is_hero(who) AND atk(14) <> 7 THEN xt = -20
  tempx = bslot(t(who, i)).x + xt
  tempy = bslot(t(who, i)).y + yt
  anim_setpos 12 + 0, tempx + 0, tempy - 50, dtemp
  anim_setpos 12 + 1, tempx + 30, tempy - 30, dtemp
  anim_setpos 12 + 2, tempx + 50, tempy + 0, dtemp
  anim_setpos 12 + 3, tempx + 30, tempy + 30, dtemp
  anim_setpos 12 + 4, tempx - 0, tempy + 50, dtemp
  anim_setpos 12 + 5, tempx - 30, tempy + 30, dtemp
  anim_setpos 12 + 6, tempx - 50, tempy - 0, dtemp
  anim_setpos 12 + 7, tempx - 30, tempy - 30, dtemp
  IF is_hero(who) THEN heroanim who, atk(), bslot(), t()
  IF is_enemy(who) THEN etwitch who, atk(), bslot(), t()
  yt = (bslot(t(who, 0)).h - 50) + 2
  xt = 0: IF t(who, i) = who AND is_hero(who) AND atk(14) <> 7 THEN xt = -20
  FOR i = 0 TO 7
   anim_appear 12 + i
   anim_absmove 12 + i, bslot(t(who, 0)).x + xt, bslot(t(who, 0)).y + yt, 4, 4
  NEXT i
  if atk(99) > 0  then anim_sound(atk(99) - 1)
  anim_wait 8
  anim_disappear 24
  anim_setframe who, 0
  FOR i = 0 TO tcount
   anim_inflict t(who, i)
   temp = 3: IF is_enemy(t(who, i)) THEN temp = -3
   anim_setmove t(who, i), temp, 0, 2, 0
   if is_hero(t(who, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(who, i), 5
    ELSE
     anim_setframe t(who, i), 2
    END IF
   end if
  NEXT i
  anim_wait 3
  FOR i = 0 TO 7
   anim_disappear 12 + i
  NEXT i
  FOR i = 0 TO tcount
   temp = -3: IF is_enemy(t(who, i)) THEN temp = 3
   anim_setmove t(who, i), temp, 0, 2, 0
   anim_setframe t(who, i), 0
  NEXT i
  anim_wait 3
 NEXT j
 retreat who, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(who, i), 0
 NEXT i
 anim_end
END IF
'--------------------------------WAVE
IF atk(15) = 5 THEN
 yt = bslot(t(who, 0)).y + (bslot(t(who, 0)).h - 50) + 2
 advance who, atk(), bslot(), t()
 FOR j = 1 TO numhits
  FOR i = 0 TO 11
   temp = -50: IF is_hero(who) THEN temp = 320
   IF tcount > 0 OR atk(4) = 1 THEN
    anim_setpos 12 + i, temp, i * 15, pdir
   ELSE
    anim_setpos 12 + i, temp, yt, pdir
   END IF
  NEXT i
  IF is_hero(who) THEN heroanim who, atk(), bslot(), t()
  IF is_enemy(who) THEN etwitch who, atk(), bslot(), t()
  temp = 24: IF is_hero(who) THEN temp = -24
  if atk(99) > 0  then anim_sound(atk(99) - 1)

  FOR i = 0 TO 11
   anim_appear 12 + i
   anim_setmove 12 + i, temp, 0, 16, 0
   anim_wait 1
  NEXT i
  anim_wait 15
  anim_disappear 24
  anim_setframe who, 0
  FOR i = 0 TO tcount
   anim_inflict t(who, i)
   temp = 3: IF is_enemy(t(who, i)) THEN temp = -3
   anim_setmove t(who, i), temp, 0, 2, 0
   if is_hero(t(who, i)) then
    IF readbit(atk(), 20, 0) = 0 THEN
     anim_setframe t(who, i), 5
    ELSE
     anim_setframe t(who, i), 2
    END IF
   end if
  NEXT i
  anim_waitforall
  FOR i = 0 TO 11
   anim_disappear 12 + i
  NEXT i
  FOR i = 0 TO tcount
   temp = -3: IF is_enemy(t(who, i)) THEN temp = 3
   anim_setmove t(who, i), temp, 0, 2, 0
   anim_setframe t(who, i), 0
  NEXT i
  anim_wait 2
 NEXT j
 retreat who, atk(), bslot(), t()
 FOR i = 0 TO tcount
  anim_setframe t(who, i), 0
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
    IF bstat(i).cur.hp < bstat(i).max.hp / 5 AND vdance = 0 THEN of(i) = 6: bslot(i).frame = 6
    '--re-enforce party's X/Y positions...
    bslot(i).x = bslot(i).basex
    bslot(i).y = bslot(i).basey
   NEXT i
   FOR i = 0 TO 7
    IF eflee(4 + i) = 0 THEN
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
   IF inflict(who, targ, bstat(), bslot(), harm$(), hc(), hx(), hy(), atk(), tcount, bits(), revenge(), revengemask(), targmem(), revengeharm(), repeatharm()) THEN
    '--attack succeeded
    IF readbit(atk(), 65, 12) THEN
     '--cancel target's attack
     godo(targ) = 0
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

    IF trytheft(who, targ, atk(), es()) THEN
     GOSUB checkitemusability
    END IF
   ELSE
	checkTagCond atk(60), 3, atk(59), atk(61)
	checkTagCond atk(63), 3, atk(62), atk(64)
   END IF
   tdwho = targ
   GOSUB triggerfade
   IF bstat(targ).cur.hp > 0 THEN
    '---REVIVE---
    bslot(targ).vis = 1
    bslot(targ).dissolve = 0
   END IF
   IF is_enemy(targ) THEN GOSUB sponhit
   IF conmp = 1 THEN
    '--if the attack costs MP, we want to actually consume MP
    IF atk(8) > 0 THEN bstat(who).cur.mp = large(bstat(who).cur.mp - focuscost(atk(8), bstat(who).cur.foc), 0)

    '--ditto for HP
    IF atk(9) > 0 THEN
      bstat(who).cur.hp = large(bstat(who).cur.hp - atk(9), 0)
      hc(who) = 7
      hx(who) = bslot(who).x + (bslot(who).w * .5)
      hy(who) = bslot(who).y + (bslot(who).h * .5)
      harm$(who) = STR$(atk(9))
    END IF

    '--ditto for money
    IF atk(10) <> 0 THEN
      gold& = large(gold& - atk(10), 0)
      hc(who) = 7
      hx(who) = bslot(who).x + (bslot(who).w * .5)
      hy(who) = bslot(who).y + (bslot(who).h * .5)
      harm$(who) = STR$(atk(10)) + "$"
      IF atk(10) < 0 THEN harm$(who) += "+"
      IF gold& > 1000000000 THEN gold& = 1000000000
      IF gold& < 0 THEN gold& = 0

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
   IF conlmp(who) > 0 THEN lmp(who, conlmp(who) - 1) = lmp(who, conlmp(who) - 1) - 1: conlmp(who) = 0
   IF icons(who) >= 0 THEN
    IF consumeitem(icons(who)) THEN setbit iuse(), 0, icons(who), 0
    icons(who) = -1
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
    get_valid_targs autotmask(), who, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
    autotarget t(), autotmask(), who, buffer(), bslot(), bstat()
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
   sound_play(popw,0)
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
	 debug "blsot(" & ww & ").d = " & tmp1
	 bslot(ww).d = tmp1
 END SELECT
LOOP UNTIL wf <> 0 OR anim = -1

IF anim = -1 THEN
 GOSUB afterdone
 '--clean up stack
 'DEBUG debug "discarding" + XSTR$((stackpos - bstackstart) \ 2) + " from stack"
 WHILE stackpos > bstackstart: dummy = popw: WEND
 '-------Spawn a Chained Attack--------
 IF atk(12) > 0 AND INT(RND * 100) < atk(13) AND bstat(who).cur.hp > 0 AND (bslot(who).attack_succeeded <> 0 AND readbit(atk(),65,7) OR readbit(atk(),65,7) = 0)THEN
  wf = 0: aset = 0
  loadattackdata buffer(), atk(12) - 1
  IF buffer(16) > 0 THEN
   godo(who) = atk(12)
   delay(who) = buffer(16)
  ELSE
   anim = atk(12) - 1: aset = 0: godo(who) = 0
  END IF
  o = 0
  FOR i = 4 TO 11
   IF bstat(i).cur.hp = 0 THEN o = o + 1
  NEXT i
  IF o < 8 THEN
   IF buffer(4) <> atk(4) OR buffer(3) <> atk(3) THEN
    'if the chained attack has a different target class/type then re-target
    get_valid_targs autotmask(), who, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
    autotarget t(), autotmask(), who, buffer(), bslot(), bstat()
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
GOSUB dieWOboss
GOSUB fulldeathcheck
RETRACE

dieWOboss:
bosses = 0
'--count bosses
FOR j = 4 TO 11
 '--is it a boss?
 IF readbit(ebits(), (j - 4) * 5, 56) = 1 THEN
  '-- is it alive?
  IF bstat(j).cur.hp > 0 THEN
   bosses = bosses + 1
  END IF
 END IF
NEXT j
'--if there are no bossess...
IF bosses = 0 THEN
 '--for each foe...
 FOR j = 4 TO 11
  '--should it die without a boss?
  IF readbit(ebits(), (j - 4) * 5, 58) = 1 THEN
   '-- is it still alive?
   IF bstat(j).cur.hp > 0 THEN
    '--trigger death fade
    tdwho = j
    bstat(tdwho).cur.hp = 0
    GOSUB triggerfade
   END IF
  END IF
 NEXT j
END IF
RETRACE

triggerfade:
'If the target is really dead...
IF bstat(tdwho).cur.hp = 0 THEN
 'the number of ticks it takes the enemy to fade away is equal to half its width
 bslot(tdwho).dissolve = bslot(tdwho).w * .5
 IF is_enemy(tdwho) THEN
  '--flee as alternative to death
  IF readbit(ebits(), (tdwho - 4) * 5, 59) = 1 THEN
   eflee(tdwho) = 1
   'the number of ticks it takes an enemy to run away is based on its distance
   'from the left side of the screen and its width. Enemys flee at 10 pixels per tick
   bslot(tdwho).dissolve = (bslot(tdwho).w + bslot(tdwho).x) / 10
  END IF
 END IF
END IF
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
 t(you, i) = -1 'clear list of confirmed targets
NEXT i

'load attack
loadattackdata buffer(), godo(you) - 1

noifdead = 0
ltarg(you) = 0

get_valid_targs tmask(), you, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
noifdead = attack_can_hit_dead(you, buffer())

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

tptr = find_preferred_target(tmask(), you, buffer(), bslot(), bstat())
'fail if no targets are found
IF tptr = -1 THEN
 ptarg = 0
 RETRACE
END IF

'ready to choose targs() from tmask()
ptarg = 2
RETRACE

fulldeathcheck:
deadguycount = 0
FOR deadguy = 4 TO 11
 GOSUB ifdead
NEXT deadguy
IF deadguycount >= 8 THEN dead = 1
deadguycount = 0
FOR deadguy = 0 TO 3
 GOSUB ifdead
NEXT deadguy
IF deadguycount = 4 THEN dead = 2
RETRACE

ifdead:
deadguyhp = bstat(deadguy).cur.hp
IF is_enemy(deadguy) THEN
 isenemy = 1
 enemynum = deadguy - 4
 formslotused = formdata((deadguy - 4) * 4)
 IF bstat(deadguy).cur.hp > 0 AND bslot(deadguy).hero_untargetable <> 0 THEN deadguycount = deadguycount + 1
ELSE
 isenemy = 0
 enemynum = -1
 formslotused = -1
END IF
IF deadguyhp = 0 THEN deadguycount = deadguycount + 1
IF deadguyhp = 0 and formslotused <> 0 THEN
 '--deadguy is really dead
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
 IF you = deadguy THEN you = -1: mset = 0
 '-- if it is a dead enemy's turn, cancel ai
 IF them = deadguy THEN them = -1
 IF isenemy THEN '------PLUNDER AND EXPERIENCE AND ITEMS------
  GOSUB spawnally
  IF formslotused > 0 THEN
   plunder& = plunder& + es(enemynum, 56)
   IF plunder& > 1000000000 THEN plunder& = 1000000000
   exper& = exper& + es(enemynum, 57)
   IF exper& > 1000000 THEN exper& = 1000000
   IF INT(RND * 100) < es(enemynum, 59) THEN '---GET ITEMS FROM FOES-----
    FOR j = 0 TO 16
     IF found(j, 1) = 0 THEN found(j, 0) = es(enemynum, 58): found(j, 1) = 1: EXIT FOR
     IF found(j, 0) = es(enemynum, 58) THEN found(j, 1) = found(j, 1) + 1: EXIT FOR
    NEXT j
   ELSE '------END NORMAL ITEM---------------
    IF INT(RND * 100) < es(enemynum, 61) THEN
     FOR j = 0 TO 16
      IF found(j, 1) = 0 THEN found(j, 0) = es(enemynum, 60): found(j, 1) = 1: EXIT FOR
      IF found(j, 0) = es(enemynum, 60) THEN found(j, 1) = found(j, 1) + 1: EXIT FOR
     NEXT j
    END IF '---END RARE ITEM-------------
   END IF '----END GET ITEMS----------------
  END IF
  ' remove dead enemy from formation
  formdata((deadguy - 4) * 4) = 0
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
   IF t(j, 0) = -1 AND who <> j AND godo(j) > 0 THEN
    'if no targets left, a-to-re-target
    loadattackdata buffer(), godo(j) - 1
    get_valid_targs autotmask(), j, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
    autotarget t(), autotmask(), j, buffer(), bslot(), bstat()
   END IF
   IF tmask(deadguy) = 1 THEN tmask(deadguy) = 0
   IF targs(deadguy) = 1 THEN targs(deadguy) = 0
  NEXT j
  IF tptr = deadguy THEN
   WHILE tmask(tptr) = 0
    tptr = tptr + 1: IF tptr > 11 THEN ptarg = 0: RETRACE
   WEND
  END IF
 END IF  '----END ONLY WHEN NOIFDEAD = 0
END IF  '----END (deadguy) IS DEAD
RETRACE

sponhit:
FOR i = 0 TO 8
 IF es(targ - 4, 82 + i) > 0 AND atktype(i) = 1 THEN
  FOR j = 1 TO es(targ - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF formdata(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    formdata(slot * 4) = es(targ - 4, 82 + i)
    loadfoe slot, formdata(), es(), bslot(), p(), ext$(), bits(), bstat(), ebits(), batname$()
   END IF
  NEXT j
  EXIT FOR
 END IF
NEXT i
RETRACE

spawnally:
IF is_enemy(deadguy) THEN
 IF es(deadguy - 4, 80) > 0 AND atktype(0) = 1 THEN ' spawn on non-elemental death
  FOR j = 1 TO es(deadguy - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF formdata(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    formdata(slot * 4) = es(deadguy - 4, 80)
    deadguycount = deadguycount - 1
    loadfoe slot, formdata(), es(), bslot(), p(), ext$(), bits(), bstat(), ebits(), batname$()
   END IF
  NEXT j
  es(deadguy - 4, 80) = 0
 END IF
 IF es(deadguy - 4, 79) > 0 THEN ' spawn on elemental death
  FOR j = 1 TO es(deadguy - 4, 91)
   slot = -1
   FOR k = 7 TO 0 STEP -1
    IF formdata(k * 4) = 0 THEN slot = k
   NEXT k
   IF slot > -1 THEN
    formdata(slot * 4) = es(deadguy - 4, 79)
    deadguycount = deadguycount - 1
    loadfoe slot, formdata(), es(), bslot(), p(), ext$(), bits(), bstat(), ebits(), batname$()
   END IF
  NEXT j
  es(deadguy - 4, 79) = 0
 END IF
END IF
RETRACE

itemmenu:
IF carray(5) > 1 THEN
 mset = 0
 flusharray carray(), 7, 0
 icons(you) = -1 '--is this right?
END IF
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

IF carray(4) > 1 THEN
 IF readbit(iuse(), 0, iptr) = 1 THEN
  loaditemdata buffer(), inventory(iptr).id
  icons(you) = -1: IF buffer(73) = 1 THEN icons(you) = iptr
  temp = buffer(47)
  loadattackdata buffer(), temp - 1
  godo(you) = temp
  delay(you) = large(buffer(16), 1)
  ptarg = 1
  mset = 0
  flusharray carray(), 7, 0
 END IF
END IF
RETRACE

spellmenu:
IF carray(5) > 1 THEN '--cancel
 mset = 0
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
  mset = 0
  flusharray carray(), 7, 0
  RETRACE
 END IF

 '--can-I-use-it? checking
 IF spel(sptr) > -1 THEN
  '--list-entry is non-empty
  loadattackdata atktemp(), spel(sptr)
  IF atkallowed(atktemp(), you, st(you).list_type(listslot), INT(sptr / 3), bstat()) THEN
   '--attack is allowed
   '--if lmp then set lmp consume flag
   IF st(you).list_type(listslot) = 1 THEN conlmp(you) = INT(sptr / 3) + 1
   '--queue attack
   godo(you) = spel(sptr) + 1
   delay(you) = large(atktemp(16), 1)
   '--exit spell menu
   ptarg = 1: mset = 0
   flusharray carray(), 7, 0
  END IF
 END IF
END IF
RETRACE

picktarg: '-----------------------------------------------------------

'cancel
IF carray(5) > 1 THEN
 godo(you) = 0
 ptarg = 0
 flusharray carray(), 7, 0
 RETRACE
END IF

IF ptarg = 1 THEN GOSUB setuptarg

'autotarget
IF ptarg = 3 THEN
 get_valid_targs autotmask(), you, buffer(), bslot(), bstat(), revenge(), revengemask(), targmem()
 autotarget t(), autotmask(), you, buffer(), bslot(), bstat()
 ctr(you) = 0
 ready(you) = 0
 you = -1
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
  t(you, o) = i: o = o + 1
  IF noifdead THEN setbit ltarg(), you, i, 1
 END IF
NEXT i
ctr(you) = 0
ready(you) = 0
you = -1
ptarg = 0
noifdead = 0
RETRACE

display:
IF vdance = 0 THEN 'only display interface till you win
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   IF readbit(gen(), 101, 6) = 0 THEN
    '--speed meter--
    col = uilook(uiTimeBar): IF ready(i) = 1 THEN col = uilook(uiTimeBarFull)
    centerfuz 66, 9 + i * 10, 131, 10, 1, dpage
    IF bstat(i).cur.hp > 0 THEN
     j = ctr(i) / 7.7
     IF delay(i) > 0 OR godo(i) > 0 OR (anim >= 0 AND who = i) THEN
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
   col = uilook(uiMenuItem): IF i = you THEN col = uilook(uiSelectedItem + tog)
   edgeprint batname$(i), 128 - LEN(batname$(i)) * 8, 5 + i * 10, col, dpage
   '--hp--
   edgeprint STR$(bstat(i).cur.hp) + "/" + STR$(bstat(i).max.hp), 136, 5 + i * 10, col, dpage
   WITH bstat(i)
    'poison indicator
    IF .cur.poison < .max.poison THEN
     edgeprint CHR$(gen(genPoison)), 209, 5 + i * 10, col, dpage
    END IF
    'stun indicator
    IF .cur.stun < .max.stun THEN
     edgeprint CHR$(gen(genStun)), 217, 5 + i * 10, col, dpage
    END IF
    'mute indicator
    IF .cur.mute < .max.mute THEN
     edgeprint CHR$(gen(genMute)), 217, 5 + i * 10, col, dpage
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
   edgeprint battlecaption$, xstring(battlecaption$, 160), 182, uilook(uiText), dpage
  END IF
 END IF
 IF you >= 0 THEN
  centerbox 268, 5 + (4 * (mend(you) + 2)), 88, 8 * (mend(you) + 2), 1, dpage
  FOR i = 0 TO mend(you)
   bg = 0
   fg = uilook(uiMenuItem)
   IF pt = i THEN
     fg = uilook(uiSelectedItem + tog)
     bg = uilook(uiHighlight)
   END IF
   IF readbit(menubits(), 0, you*4+i) THEN
     fg = uilook(uiDisabledItem)
     IF pt = i THEN fg = uilook(uiSelectedDisabled + tog)
   END IF
   textcolor fg, bg
   printstr menu$(you, i), 228, 9 + i * 8, dpage
  NEXT i
  IF ptarg = 0 AND readbit(gen(), genBits, 14) = 0 THEN
   edgeprint CHR$(24), bslot(you).x + (bslot(you).w / 2) - 4, bslot(you).y - 5 + (tog * 2), uilook(uiSelectedItem + tog), dpage
  END IF
  IF mset = 1 THEN '--draw spell menu
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
  IF mset = 2 THEN
   centerbox 160, 45, 304, 80, 1, dpage
   FOR i = itop TO itop + 26
    textcolor uilook(uiDisabledItem - readbit(iuse(), 0, i)), 0
    IF iptr = i THEN textcolor uilook(uiSelectedDisabled - (2 * readbit(iuse(), 0, i)) + tog), uilook(uiHighlight)
    printstr inventory(i).text, 20 + 96 * (i MOD 3), 8 + 8 * ((i - itop) \ 3), dpage
   NEXT i
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
END IF'--end if vdance=0
RETRACE

meters:
IF away = 1 THEN RETRACE
'--if a menu is up, and pause-on-menus is ON then no time passes (as long as at least one visible targetable enemy is alive)
isdeepmenu = (mset > 0 AND readbit(gen(), genBits, 0))
isbattlemenu = (mset >= 0 AND you >= 0 AND readbit(gen(), genBits, 13))
isenemytargs = (targenemycount(bslot(), bstat()) > 0)
IF (isdeepmenu OR isbattlemenu) AND isenemytargs THEN RETRACE

FOR i = 0 TO 11

 'delays for attacks already selected
 IF you <> i THEN delay(i) = large(delay(i) - 1, 0)

 '--poison
 WITH bstat(i)
  IF .cur.poison < .max.poison THEN
   prtimer(i, 0) = prtimer(i, 0) + large(bstat(i).cur.spd, 7)
   IF prtimer(i, 0) >= 1500 THEN
    prtimer(i, 0) = 0
    harm = .max.poison - .cur.poison
    harm = range(harm, 20)
    quickinflict harm, i, hc(), hx(), hy(), bslot(), harm$(), bstat()
    tdwho = i
    GOSUB triggerfade
    GOSUB dieWOboss
    GOSUB fulldeathcheck
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
    tdwho = i
    GOSUB triggerfade
    GOSUB dieWOboss
    GOSUB fulldeathcheck
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
   IF you = i THEN you = -1
   IF them = i THEN them = -1
  END IF
 NEXT i
 laststun = TIMER
END IF

RETRACE

animate:
FOR i = 0 TO 3
 IF walk(i) = 1 THEN of(i) = of(i) XOR tog : bslot(i).frame = bslot(i).frame xor tog
 IF who <> i AND bstat(i).cur.hp < bstat(i).max.hp / 5 AND vdance = 0 THEN of(i) = 6 : bslot(i).frame = 6
 IF vdance > 0 AND bstat(i).cur.hp > 0 AND tog = 0 THEN
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
   IF eflee(i) = 0 THEN
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
    'make dead enemy invisible (the ifdead code will actually do the final removal)
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
		if battle_draw_style = 0 OR (battle_draw_style = 2 and battle_draw_toggle = 0) then
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
		#define DEFAULT_TRANSITION 1
			with bslot(zbuf(i))
				if .d = 0 then
					if .sprites <> 0 AND .frame < .sprite_num then
						if .dissolve and eflee(zbuf(i)) = 0 then
							sprite_draw_dissolved(.sprites + .frame, .pal, .x, .y - .z,.dissolve, DEFAULT_TRANSITION, 1, -1, dpage)
						else
							sprite_draw(.sprites + .frame, .pal, .x, .y - .z, 1, -1, dpage)
						end if
						'debug "sprite_draw(" & .sprites & " + " & .frame & ", " & .pal & ", " & .x & ", " & .y & " - " & .z & ")"
					end if
				else
					if bslot(zbuf(i)).sprites <> 0 AND bslot(zbuf(i)).frame < bslot(zbuf(i)).sprite_num then
						if .dissolve and eflee(zbuf(i)) = 0 then
							sprite_draw_dissolved(.sprites + .frame, .pal, .x, .y - .z,.dissolve, DEFAULT_TRANSITION, 1, -1, dpage)
						else
							sprite_draw(.sprites + .frame, .pal, .x, .y - .z, 1, -1, dpage)
						end if
						'debug "sprite_draw(" & .sprites & " + " & .frame & ", " & .pal & ", " & .x & ", " & .y & " - " & .z & ")"
					end if
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
loadset tmpdir$ & "for.tmp", form, 0

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
  loadset game$ + ".pt0", exstat(i, 0, 14), i * 16
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
   .sprites = sprite_load(game$ & ".pt0", exstat(i, 0, 14), .sprite_num, 32, 40)
   if .sprites = 0 then debug "Couldn't load hero sprite: " & game$ & ".pt0#" & exstat(i,0,14)
   .pal = palette16_load(game$ + ".pal", exstat(i, 0, 15), 0, exstat(i, 0, 14))
   if .pal = 0 then debug "Failed to load palette (#" & i & ")"

   .frame = 0
  END WITH
  FOR o = 0 TO 11
   bstat(i).cur.sta(o) = exstat(i, 0, o)
   bstat(i).max.sta(o) = exstat(i, 1, o)
  NEXT o
  herobattlebits bits(), i
  batname$(i) = names$(i)
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
 END IF
NEXT i
FOR i = 0 TO 7
 loadfoe i, formdata(), es(), bslot(), p(), ext$(), bits(), bstat(), ebits(), batname$()
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
setdiskpages buffer(), 200, 0
loadpage game$ + ".mxs", curbg, 2
FOR i = 0 TO 3
 IF bstat(i).cur.hp < bstat(i).max.hp / 5 AND vdance = 0 THEN of(i) = 6 : bslot(i).frame = 6
 IF hero(i) > 0 AND bstat(i).cur.hp = 0 THEN bslot(i).dissolve = 1
 lifemeter(i) = (88 / large(bstat(i).max.hp, 1)) * bstat(i).cur.hp
NEXT i
bslot(24).w = 24
bslot(24).h = 24
bos = 0
FOR i = 4 TO 11
 IF readbit(ebits(), (i - 4) * 5, 56) = 1 THEN bos = 1
NEXT i
IF bos = 0 THEN
 FOR i = 4 TO 11
  ' check the "die without boss" bitset
  IF readbit(ebits(), (i - 4) * 5, 58) = 1 THEN
   tdwho = i
   bstat(tdwho).cur.hp = 0
   GOSUB triggerfade
  END IF
 NEXT i
 deadguycount = 0
 FOR deadguy = 4 TO 11
  GOSUB ifdead
 NEXT deadguy
 IF deadguycount = 8 THEN dead = 1
END IF
RETRACE

victory: '------------------------------------------------------------------
IF gen(3) > 0 THEN wrappedsong gen(3) - 1
gold& = gold& + plunder&
IF gold& > 1000000000 THEN gold& = 1000000000
IF liveherocount(bstat()) > 0 THEN exper& = exper& / liveherocount(bstat())
FOR i = 0 TO 3
 IF bstat(i).cur.hp > 0 THEN giveheroexperience i, exstat(), exper&
 updatestatslevelup i, exstat(), bstat(), 0
NEXT i
vdance = 1
RETRACE

vicdance:
IF drawvicbox THEN centerfuz 160, 30, 280, 50, 1, dpage
SELECT CASE vdance
 CASE 1
  '--print acquired gold and experience
  IF plunder& > 0 OR exper& > 0 THEN drawvicbox = 1: centerfuz 160, 30, 280, 50, 1, dpage
  IF plunder& > 0 THEN
   temp$ = goldcap$ + XSTR$(plunder&) + " " + goldname$ + "!"
   edgeprint temp$, xstring(temp$, 160), 16, uilook(uiText), dpage
  END IF
  IF exper& > 0 THEN
   temp$ = expcap$ + XSTR$(exper&) + " " + expname$ + "!"
   edgeprint temp$, xstring(temp$, 160), 28, uilook(uiText), dpage
  END IF
  IF carray(4) > 1 OR carray(5) > 1 OR (plunder& = 0 AND exper& = 0) THEN
   vdance = 2
  END IF
 CASE 2
  '--print levelups
  o = 0
  FOR i = 0 TO 3
   IF o = 0 AND exstat(i, 1, 12) THEN centerfuz 160, 30, 280, 50, 1, dpage: drawvicbox = 1
   SELECT CASE exstat(i, 1, 12)
    CASE 1
     temp$ = level1up$ + " " + batname$(i)
     edgeprint temp$, xstring(temp$, 160), 12 + i * 10, uilook(uiText), dpage
     o = 1
    CASE IS > 1
     temp$ = STR$(exstat(i, 1, 12)) + " " + levelXup$ + " " + batname$(i)
     edgeprint temp$, xstring(temp$, 160), 12 + i * 10, uilook(uiText), dpage
     o = 1
   END SELECT
  NEXT i
  IF o = 0 OR (carray(4) > 1 OR carray(5) > 1) THEN
   vdance = 3
   showlearn = 0
   learnwho = 0: learnlist = 0: learnslot = -1
  END IF
 CASE 3
  '--print learned spells, one at a time
  IF showlearn = 0 THEN '--Not showing a spell yet. find the next one
   DO
    learnslot = learnslot + 1
    IF learnslot > 23 THEN learnslot = 0: learnlist = learnlist + 1
    IF learnlist > 3 THEN learnlist = 0: learnwho = learnwho + 1
    IF learnwho > 3 THEN ' We have iterated through all spell lists for all heroes, time to move on
     vdance = 4
     found$ = ""
     fptr = 0
     drawvicbox = 0
     EXIT DO
    END IF
    IF readbit(learnmask(), 0, learnwho * 96 + learnlist * 24 + learnslot) THEN
     'found a learned spell
     found$ = batname$(learnwho) + learned$
     found$ = found$ & readattackname$(spell(learnwho, learnlist, learnslot) -1)
     showlearn = 1
     drawvicbox = 1
     EXIT DO
    END IF
   LOOP
  ELSE' Found a learned spell to display, show it until a keypress
   IF carray(4) > 1 OR carray(5) > 1 THEN
    showlearn = 0 ' hide the display (which causes us to search for the next learned spell)
   END IF
   edgeprint found$, xstring(found$, 160), 22, uilook(uiText), dpage
  END IF
 CASE 4
  '--print found items, one at a time
  '--check to see if we are currently displaying a gotten item
  IF found$ = "" THEN
   '--if not, check to see if there are any more gotten items to display
   IF found(fptr, 1) = 0 THEN vdance = -1: RETRACE
   '--get the item name
   found$ = readitemname$(found(fptr, 0))
   '--actually aquire the item
   getitem found(fptr, 0) + 1, found(fptr, 1)
  END IF
  '--if the present item is gotten, show the caption
  IF found(fptr, 1) = 1 THEN
   temp$ = foundcap$ + " " + found$
  ELSE
   temp$ = foundpcap$ + XSTR$(found(fptr, 1)) + " " + found$
  END IF
  IF LEN(temp$) THEN centerfuz 160, 30, 280, 50, 1, dpage
  edgeprint temp$, xstring(temp$, 160), 22, uilook(uiText), dpage
  '--check for a keypress
  IF carray(4) > 1 OR carray(5) > 1 THEN
   IF found(fptr, 1) = 0 THEN
    '--if there are no further items, exit
    vdance = -1
   ELSE
    '--otherwize, increment the findpointer and reset the caption
    fptr = fptr + 1: found$ = ""
   END IF
  END IF
END SELECT
RETRACE

checkitemusability:
FOR i = 0 TO inventoryMax
 IF inventory(i).used THEN
  loaditemdata buffer(), inventory(i).id
  IF buffer(47) > 0 THEN setbit iuse(), 0, i, 1
 END IF
NEXT i
RETRACE

'afflictions
'12=poison
'13=regen
'14=stun
'15=limit (?)
'16=unused
'17=unused

END FUNCTION

FUNCTION checkNoRunBit (bstat() AS BattleStats, ebits(), bslot() AS BattleSprite)
 checkNoRunBit = 0
 FOR i = 4 TO 11
  IF bstat(i).cur.hp > 0 AND bslot(i).vis = 1 AND readbit(ebits(), (i - 4) * 5, 57) = 1 THEN checkNoRunBit = 1
 NEXT i
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
hc(targ) = 7
hx(targ) = bslot(targ).x + (bslot(targ).w * .5)
hy(targ) = bslot(targ).y + (bslot(targ).h * .5)
IF harm < 0 THEN
 harm$(targ) = "+" + STR$(ABS(harm))
ELSE
 harm$(targ) = STR$(harm)
END IF

if gen(genDamageCap) > 0 THEN harm = small(harm, gen(genDamageCap))

bstat(targ).cur.hp = bound(bstat(targ).cur.hp - harm, 0, bstat(targ).max.hp)
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
  IF bslot(i).dissolve THEN count += 1
 NEXT i
 RETURN count
END FUNCTION
