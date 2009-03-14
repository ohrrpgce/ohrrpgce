'OHRRPGCE GAME - More various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"

#include "game.bi"
#include "yetmore.bi"
#include "yetmore2.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "bmodsubs.bi"

'FIXME: this should not be called directly here. needs wrapping in allmodex.bi
'Mike: why? it's already wrapped in gfx_*.bas
#include "gfx.bi"

'FIXME: Why does DIMing plotslices() in game.bas not work?
DIM plotslices() AS Slice Ptr
'Useing a lower bound of 1 because 0 is considered an invalid handle
'The size of 64 is just so we won't have to reallocate for a little while
REDIM plotslices(1 TO 64) 

'these variables hold information used by breakpoint to step to the desired position
DIM SHARED waitforscript, waitfordepth, stepmode, lastscriptnum

REM $STATIC

SUB add_rem_swap_lock_hero (box AS TextBox, stat())
'---ADD/REMOVE/SWAP/LOCK
'---ADD---
IF box.hero_addrem > 0 THEN
 i = findhero(0, 0, 40, 1)
 IF i > -1 THEN
  addhero box.hero_addrem, i, stat()
  vishero stat()
 END IF
END IF '---end if > 0
'---REMOVE---
IF box.hero_addrem < 0 THEN
 IF herocount(40) > 1 THEN
  i = findhero(-box.hero_addrem, 0, 40, 1)
  IF i > -1 THEN hero(i) = 0
  IF herocount(3) = 0 THEN forceparty stat()
 END IF
END IF '---end if < 0
vishero stat()
'---SWAP-IN---
IF box.hero_swap > 0 THEN
 i = findhero(box.hero_swap, 40, 0, -1)
 IF i > -1 THEN
  FOR o = 0 TO 3
   IF hero(o) = 0 THEN
    doswap i, o, stat()
    EXIT FOR
   END IF
  NEXT o
 END IF
END IF '---end if > 0
'---SWAP-OUT---
IF box.hero_swap < 0 THEN
 i = findhero(-box.hero_swap, 0, 40, 1)
 IF i > -1 THEN
  FOR o = 40 TO 4 STEP -1
   IF hero(o) = 0 THEN
    doswap i, o, stat()
    IF herocount(3) = 0 THEN forceparty stat()
    EXIT FOR
   END IF
  NEXT o
 END IF
END IF '---end if < 0
'---UNLOCK HERO---
IF box.hero_lock > 0 THEN
 temp = findhero(box.hero_lock, 0, 40, 1)
 IF temp > -1 THEN setbit hmask(), 0, temp, 0
END IF '---end if > 0
'---LOCK HERO---
IF box.hero_lock < 0 THEN
 temp = findhero(-box.hero_lock, 0, 40, 1)
 IF temp > -1 THEN setbit hmask(), 0, temp, 1
END IF '---end if > 0
END SUB

FUNCTION checksaveslot (slot as integer) as integer
 DIM AS SHORT saveversion
 sg$ = savefile
 savh = FREEFILE
 OPEN sg$ FOR BINARY AS #savh
 GET #savh, 1 + 60000 * slot, saveversion
 CLOSE #savh
 checksaveslot = saveversion
END FUNCTION

SUB erasesaveslot (slot)
 DIM AS SHORT saveversion = 0
 sg$ = savefile
 IF fileisreadable(sg$) = 0 THEN EXIT SUB
 savh = FREEFILE
 OPEN sg$ FOR BINARY AS #savh
 IF LOF(savh) > 60000 * slot THEN
  PUT #savh, 1 + 60000 * slot, saveversion
 END IF
 CLOSE #savh
END SUB

SUB doihavebits
dim her as herodef
FOR i = 0 TO small(gen(genMaxHero), 59)
 loadherodata @her, i
 herobits(i, 0) = her.have_tag    'have hero tag
 herobits(i, 1) = her.alive_tag   'is alive tag
 herobits(i, 2) = her.leader_tag  'is leader tag
 herobits(i, 3) = her.active_tag  'is in active party tag
NEXT i
DIM item_data(99) AS INTEGER
FOR i = 0 TO gen(genMaxItem)
 loaditemdata item_data(), i
 itembits(i, 0) = item_data(74)   'when have tag
 itembits(i, 1) = item_data(75)   'is in inventory
 itembits(i, 2) = item_data(76)   'is equiped tag
 itembits(i, 3) = item_data(77)   'is equiped by hero in active party
NEXT i
END SUB

SUB embedtext (text$, limit=0)
start = 1
DO WHILE start < LEN(text$)
 '--seek an embed spot
 embedbegin = INSTR(start, text$, "${")
 IF embedbegin = 0 THEN EXIT DO '--failed to find an embed spot
 embedend = INSTR(embedbegin + 4, text$, "}")
 IF embedend = 0 THEN EXIT DO '--embed spot has no end
 '--break apart the string
 before$ = MID$(text$, 1, large(embedbegin - 1, 0))
 after$ = MID$(text$, embedend + 1)
 '--extract the command and arg
 act$ = MID$(text$, embedbegin + 2, 1)
 arg$ = MID$(text$, embedbegin + 3, large(embedend - (embedbegin + 3), 0))
 '--convert the arg to a number
 arg = str2int(arg$)
 '--discourage bad arg values (not perfect)
 IF NOT (arg = 0 AND arg$ <> STRING$(LEN(arg$), "0")) THEN
  IF arg >= 0 THEN '--only permit postive args
   '--by default the embed is unchanged
   insert$ = "${" + act$ + arg$ + "}"
   '--evalued possible actions
   SELECT CASE UCASE$(act$)
    CASE "H": '--Hero name by ID
     '--defaults blank if not found
     insert$ = ""
     where = findhero(arg + 1, 0, 40, 1)
     IF where >= 0 THEN
      insert$ = names(where)
     END IF
    CASE "P": '--Hero name by Party position
     IF arg < 40 THEN
      '--defaults blank if not found
      insert$ = ""
      IF hero(arg) > 0 THEN
       insert$ = names(arg)
      END IF
     END IF
    CASE "C": '--Hero name by caterpillar position
     '--defaults blank if not found
     insert$ = ""
     where = partybyrank(arg)
     IF where >= 0 THEN
      insert$ = names(where)
     END IF
    CASE "V": '--global variable by ID
     '--defaults blank if out-of-range
     insert$ = ""
     IF arg >= 0 AND arg <= 4095 THEN
      insert$ = STR$(global(arg))
     END IF
    CASE "S": '--string variable by ID
     insert$ = ""
     IF bound_plotstr(arg, "${S#} text box insert") THEN
      insert$ = plotstr(arg).s
     END IF
   END SELECT
   text$ = before$ + insert$ + after$
   embedend = LEN(before$) + LEN(insert$) + 1
  END IF
 END IF
 '--skip past this embed
 start = embedend + 1
LOOP
'--enforce limit (if set)
IF limit > 0 THEN
 text$ = LEFT$(text$, limit)
END IF
END SUB

SUB scriptstat (id, stat())
'contains an assortment of scripting commands that
'depend on access to the hero stat array stat()

SELECT CASE AS CONST id
 CASE 64'--get hero stat
  scriptret = stat(bound(retvals(0), 0, 40), bound(retvals(2), 0, 1), bound(retvals(1), 0, 13))
 CASE 66'--add hero
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxHero) THEN
   FOR i = 37 TO 0 STEP -1
    IF hero(i) = 0 THEN slot = i
   NEXT i
   'retvals(0) is the real hero id, addhero removes the 1 again
   addhero retvals(0) + 1, slot, stat()
   vishero stat()
  END IF
 CASE 67'--delete hero
  IF herocount(40) > 1 THEN
   i = findhero(bound(retvals(0), 0, 59) + 1, 0, 40, 1)
   IF i > -1 THEN hero(i) = 0
   IF herocount(3) = 0 THEN forceparty stat()
   vishero stat()
  END IF
 CASE 68'--swap out hero
  i = findhero(retvals(0) + 1, 0, 40, 1)
  IF i > -1 THEN
   FOR o = 40 TO 4 STEP -1
    IF hero(o) = 0 THEN
     doswap i, o, stat()
     IF herocount(3) = 0 THEN forceparty stat()
     vishero stat()
     EXIT FOR
    END IF
   NEXT o
  END IF
 CASE 69'--swap in hero
  i = findhero(retvals(0) + 1, 40, 0, -1)
  IF i > -1 THEN
   FOR o = 0 TO 3
    IF hero(o) = 0 THEN
     doswap i, o, stat()
     vishero stat()
     EXIT FOR
    END IF
   NEXT o
  END IF
 CASE 83'--set hero stat
  stat(bound(retvals(0), 0, 40), bound(retvals(3), 0, 1), bound(retvals(1), 0, 13)) = retvals(2)
 CASE 89'--swap by position
  doswap bound(retvals(0), 0, 40), bound(retvals(1), 0, 40), stat()
  vishero stat()
 CASE 110'--set hero picture
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   i = bound(retvals(0), 0, 40)
   j = bound(retvals(2), 0, 1)
   stat(i, j, 14) = bound(retvals(1), 0, gen(26 + (j * 4)))
   IF i < 4 THEN
    vishero stat()
   END IF
  END IF
 CASE 111'--set hero palette
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   i = bound(retvals(0), 0, 40)
   j = bound(retvals(2), 0, 1)
   stat(i, j, 15) = bound(retvals(1), -1, 32767)
   IF i < 4 THEN
    vishero stat()
   END IF
  END IF
 CASE 112'--get hero picture
  scriptret = stat(bound(retvals(0), 0, 40), bound(retvals(1), 0, 1), 14)
 CASE 113'--get hero palette
  scriptret = stat(bound(retvals(0), 0, 40), bound(retvals(1), 0, 1), 15)
 CASE 150'--status screen
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF hero(retvals(0)) > 0 THEN
    status retvals(0), stat()
   END IF
  END IF
 CASE 152'--spells menu
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF hero(retvals(0)) > 0 THEN
    spells retvals(0), stat()
   END IF
  END IF
 CASE 154'--equip menu
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF hero(retvals(0)) > 0 THEN
    equip retvals(0), stat()
   END IF
  END IF
 CASE 157'--order menu
  heroswap 0, stat()
 CASE 158'--team menu
  heroswap 1, stat()
 CASE 183'--setherolevel (who, what, allow forgetting spells)
  IF retvals(0) >= 0 AND retvals(0) <= 40 AND retvals(1) >= 0 THEN  'we should make the regular level limit customisable anyway
   DIM dummystats(40) as BattleStats 'just need HP and MP
   stat(retvals(0), 1, 12) = retvals(1) - stat(retvals(0), 0, 12)
   stat(retvals(0), 0, 12) = retvals(1)
   exlev(retvals(0), 1) = exptolevel(retvals(1))
   exlev(retvals(0), 0) = 0  'XP attained towards the next level
   updatestatslevelup retvals(0), stat(), dummystats(), retvals(2) 'updates stats and spells
  END IF
 CASE 184'--give experience (who, how much)
  DIM dummystats(40) as BattleStats 'just need HP and MP
  IF retvals(0) = -1 AND liveherocount(stat()) > 0 THEN retvals(1) = retvals(1) / liveherocount(stat())
  FOR i = 0 TO 40
   IF i = retvals(0) OR (retvals(0) = -1 AND i <= 3) THEN
    'dead heroes should be recorded as not gaining levels.
    stat(i, 1, 12) = 0
    'give the XP to the hero only if it is alive if party is target
    IF retvals(0) <> -1 OR stat(i, 0, 0) > 0 THEN giveheroexperience i, stat(), retvals(1)
    updatestatslevelup i, stat(), dummystats(), 0
   END IF
  NEXT i
 CASE 185'--hero levelled (who)
  scriptret = stat(bound(retvals(0), 0, 40), 1, 12)
 CASE 186'--spells learnt
  found = 0
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   FOR i = retvals(0) * 96 TO retvals(0) * 96 + 95
    IF readbit(learnmask(), 0, i) THEN
     IF retvals(1) = found THEN
      scriptret = spell(retvals(0), (i \ 24) MOD 4, i MOD 24) - 1
      EXIT FOR
     END IF
     found = found + 1
    END IF
   NEXT
   IF retvals(1) = -1 THEN scriptret = found  'getcount
  END IF
 CASE 269'--totalexperience
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   scriptret = 0
   FOR i = 0 TO stat(retvals(0), 0, 12) - 1
    scriptret += exptolevel(i)
   NEXT
   scriptret += exlev(retvals(0), 0)
  END IF
 CASE 270'--experiencetolevel
  scriptret = 0
  FOR i = 0 TO retvals(0) - 1
   scriptret += exptolevel(i)
  NEXT
 CASE 271'--experiencetonextlevel
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   scriptret = exlev(retvals(0), 1) - exlev(retvals(0), 0)
  END IF
 CASE 272'--setexperience  (who, what, allowforget)
  IF retvals(0) >= 0 AND retvals(0) <= 40 AND retvals(1) >= 0 THEN
   setheroexperience retvals(0), retvals(1), retvals(2), stat(), exlev()
  END IF
END SELECT
END SUB

SUB forceparty (stat())
'---MAKE SURE YOU HAVE AN ACTIVE PARTY---
fpi = findhero(-1, 0, 40, 1)
IF fpi > -1 THEN
 FOR fpo = 0 TO 3
  IF hero(fpo) = 0 THEN
   doswap fpi, fpo, stat()
   EXIT FOR
  END IF
 NEXT fpo
END IF
END SUB

FUNCTION functiondone ()  as integer
'returns 0 when returning a value to a caller
'returns 1 when all scripts are finished
'returns 2 when reactivating a suspended script

'IF nowscript + 1 < 128 THEN
' '-- when a script terminates, the script directly above it in
' '-- the script buffer must be invalidated for caching
' scrat(nowscript + 1).id = -1
'END IF

'--if the finishing script is at the top of the script buffer,
'--then nextscroff needs to be changed
'IF scrat(nowscript).size <> 0 THEN nextscroff = scrat(nowscript).off

script(scrat(nowscript).scrnum).refcount -= 1
nowscript = nowscript - 1

IF nowscript < 0 THEN
 functiondone = 1'--no scripts are running anymore
ELSE
 'check if script needs reloading
 reloadscript scrat(nowscript)
 curcmd = cast(ScriptCommand ptr, scrat(nowscript).scrdata + scrat(nowscript).ptr)
 IF scrat(nowscript).state < 0 THEN
  '--suspended script is resumed
  scrat(nowscript).state = ABS(scrat(nowscript).state)
  functiondone = 2'--reactivating a supended script
 ELSE
  scriptret = scrat(nowscript + 1).ret
  scrat(nowscript).state = streturn'---return
  functiondone = 0'--returning a value to a caller
 END IF
END IF

END FUNCTION

SUB subread (si as ScriptInst)
'this sets up a new script by preparing to run at the root command (which should be do)
curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)
scriptret = 0'--default returnvalue is zero
si.curargn = 0
si.state = stnext

'+5 just-in-case for extra state stuff pushed to stack (atm just switch, +1 ought to be sufficient)
checkoverflow(scrst, curcmd->argc + 5)

IF curcmd->kind <> tyflow THEN
 scripterr "Root script command not flow, but " & curcmd->kind
 si.state = sterror
END IF
END SUB

SUB subdoarg (si as ScriptInst)
'read/load arguments, evaluating immediate values, in a depth-first manner, until either:
'-all args for a command have been pushed, stnext to evaluate
'-certain flow & math commands need special logic after every evaluated arg, stnext to handle
si.state = stnext

DIM as integer ptr dataptr = si.scrdata

quickrepeat:
DIM as ScriptCommand ptr cmdptr = cast(ScriptCommand ptr, dataptr + curcmd->args(si.curargn))

SELECT CASE cmdptr->kind
 CASE tynumber
  pushs(scrst, cmdptr->value)
 CASE tyglobal
  IF cmdptr->value < 0 OR cmdptr->value > 4095 THEN
   scripterr "Illegal global variable id " & cmdptr->value
   si.state = sterror
   EXIT SUB
  END IF
  pushs(scrst, global(cmdptr->value))
 CASE tylocal
  pushs(scrst, heap(si.heap + cmdptr->value))
 CASE IS >= tymath, tyflow
  si.depth += 1
  '2 for state + args + 5 just-in-case for extra state stuff pushed to stack (atm just switch, +1 ought to be sufficient)
  checkoverflow(scrst, 7 + cmdptr->argc)
  pushs(scrst, si.ptr)
  pushs(scrst, si.curargn)
  curcmd = cmdptr
  si.ptr = (cast(integer, cmdptr) - cast(integer, dataptr)) shr 2
  si.curargn = 0
  scriptret = 0'--default returnvalue is zero

  'this breakpoint is a perfect duplicate of breakstnext, but originally it also caught
  'streturn on evaluating numbers, locals and globals
  'edit: it's moved about even more now. needs rewriting
  'IF scrwatch AND breakstread THEN breakpoint scrwatch, 3
  'scriptdump "subdoarg"


  'even for flow, first arg always needs evaluation, so don't leave yet!
  'EXIT SUB
  IF curcmd->argc = 0 THEN EXIT SUB
  GOTO quickrepeat
 CASE ELSE
  scripterr "Illegal statement type " & cmdptr->kind
  si.state = sterror
  EXIT SUB
END SELECT

si.curargn += 1
IF si.curargn >= curcmd->argc THEN EXIT SUB
IF curcmd->kind = tyflow THEN IF curcmd->value = flowif OR curcmd->value >= flowfor THEN EXIT SUB
IF curcmd->kind = tymath THEN IF curcmd->value >= 20 THEN EXIT SUB
GOTO quickrepeat
END SUB

FUNCTION gethighbyte (n) as integer
RETURN n SHL 8
END FUNCTION

FUNCTION getnpcref (seekid as integer, offset as integer) as integer
SELECT CASE seekid

 CASE -300 TO -1'--direct reference
  getnpcref = (seekid + 1) * -1
  EXIT FUNCTION

 CASE 0 TO max_npc_defs 'ID
  found = 0
  FOR i = 0 TO 299
   IF npc(i).id - 1 = seekid THEN
    IF found = offset THEN
     getnpcref = i
     EXIT FUNCTION
    END IF
    found = found + 1
   END IF
  NEXT i

END SELECT

'--failure
getnpcref = -1
END FUNCTION

SUB greyscalepal
FOR i = bound(retvals(0), 0, 255) TO bound(retvals(1), 0, 255)
 master(i).r = bound((master(i).r + master(i).g + master(i).b) / 3, 0, 255)
 master(i).g = master(i).r
 master(i).b = master(i).r
NEXT i
END SUB

FUNCTION herobyrank (slot as integer) as integer
result = -1
IF slot >= 0 AND slot <= 3 THEN
 j = -1
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN j = j + 1
  IF j = slot THEN
   result = hero(i) - 1
   EXIT FOR
  END IF
 NEXT i
END IF
herobyrank = result
END FUNCTION

SUB initgame
'--back compat game
game = workingdir + SLASH + LCASE(trimextension$(trimpath$(sourcerpg)))
'--set game according to the archinym
IF isfile(workingdir + SLASH + "archinym.lmp") THEN
 fh = FREEFILE
 OPEN workingdir + SLASH + "archinym.lmp" FOR INPUT AS #fh
 LINE INPUT #fh, a$
 CLOSE #fh
 a$ = LCASE$(a$)
 IF LEN(a$) <= 8 THEN game = workingdir + SLASH + a$
END IF
displayname$ = getdisplayname$(sourcerpg)
setwindowtitle displayname$
clear_box_border_cache
END SUB

SUB interpolatecat
'given the current positions of the caterpillar party, interpolate their inbetween frames
FOR o = 0 TO 10 STEP 5
 FOR i = o + 1 TO o + 4
  catx(i) = catx(i - 1) + ((catx(o + 5) - catx(o)) / 4)
  caty(i) = caty(i - 1) + ((caty(o + 5) - caty(o)) / 4)
  catd(i) = catd(o)
 NEXT i
NEXT o
END SUB

SUB npcplot
FOR i = 0 TO 299
 curnpc = ABS(npc(i).id) - 1

 IF npc(i).id < 0 THEN
  '--check reappearance tags for existing but hidden NPCs
  IF istag(npcs(curnpc).tag1, 1) AND istag(npcs(curnpc).tag2, 1) AND istag(1000 + npcs(curnpc).usetag, 0) = 0 THEN
   npc(i).id = ABS(npc(i).id)
  END IF
 END IF

 IF npc(i).id > 0 THEN
  '--check removal tags for existing visible NPCs
  IF istag(npcs(curnpc).tag1, 1) = 0 OR istag(npcs(curnpc).tag2, 1) = 0 OR istag(1000 + npcs(curnpc).usetag, 0) THEN
   npc(i).id = npc(i).id * -1
  END IF
  'IF readbit(tag(), 0, ABS(npcs(curnpc * 15 + 9))) <> SGN(SGN(npcs(curnpc * 15 + 9)) + 1) AND npcs(curnpc * 15 + 9) <> 0 THEN
  '  npcl(i + 600) = npcl(i + 600) * -1
  'END IF
  'IF readbit(tag(), 0, ABS(npcs(curnpc * 15 + 10))) <> SGN(SGN(npcs(curnpc * 15 + 10)) + 1) AND npcs(curnpc * 15 + 10) <> 0 THEN
  '  npcl(i + 600) = npcl(i + 600) * -1
  'END IF
  'IF npcs(curnpc * 15 + 11) > 0 THEN
  '  IF readbit(tag(), 0, 1000 + npcs(curnpc * 15 + 11)) = 1 THEN
  '    npcl(i + 600) = npcl(i + 600) * -1
  '  END IF
  'END IF
 END IF

NEXT i
END SUB

SUB onkeyscript (scriptnum)
doit = 0
FOR i = 0 TO 5
 IF carray(i) THEN doit = 1: EXIT FOR
NEXT i

IF doit = 0 THEN
 FOR i = 1 TO 127
  IF keyval(i) THEN doit = 1: EXIT FOR
 NEXT i
END IF

IF nowscript >= 0 THEN
 IF scrat(nowscript).state = stwait AND scrat(nowscript).curvalue = 9 THEN
  '--never trigger a onkey script when the previous script
  '--has a "wait for key" command active
  doit = 0
 END IF
END IF

IF doit = 1 THEN
 rsr = runscript(scriptnum, nowscript + 1, -1, "on-key", plottrigger)
END IF

END SUB

FUNCTION partybyrank (slot as integer) as integer
result = -1
IF slot >= 0 AND slot <= 3 THEN
 j = -1
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN j = j + 1
  IF j = slot THEN
   result = i
   EXIT FOR
  END IF
 NEXT i
END IF
partybyrank = result
END FUNCTION

FUNCTION playtime (d as integer, h as integer, m as integer) as string
s$ = ""

SELECT CASE d
 CASE 1
  s$ = s$ + STR$(d) + " " + readglobalstring$(154, "day", 10) + " "
 CASE IS > 1
  s$ = s$ + STR$(d) + " " + readglobalstring$(155, "days", 10) + " "
END SELECT

SELECT CASE h
 CASE 1
  s$ = s$ + STR$(h) + " " + readglobalstring$(156, "hour", 10) + " "
 CASE IS > 1
  s$ = s$ + STR$(h) + " " + readglobalstring$(157, "hours", 10) + " "
END SELECT

SELECT CASE m
 CASE 1
  s$ = s$ + STR$(m) + " " + readglobalstring$(158, "minute", 10) + " "
 CASE IS > 1
  s$ = s$ + STR$(m) + " " + readglobalstring$(159, "minutes", 10) + " "
END SELECT

playtime$ = s$

END FUNCTION

SUB playtimer
STATIC n AS DOUBLE

IF TIMER >= n + 1 OR n - TIMER > 3600 THEN
 n = INT(TIMER)
 gen(54) = gen(54) + 1
 WHILE gen(54) >= 60
  gen(54) = gen(54) - 60
  gen(53) = gen(53) + 1
 WEND
 WHILE gen(53) >= 60
  gen(53) = gen(53) - 60
  gen(52) = gen(52) + 1
 WEND
 WHILE gen(52) >= 24
  gen(52) = gen(52) - 24
  IF gen(51) < 32767 THEN gen(51) = gen(51) + 1
 WEND
END IF

END SUB

FUNCTION rankincaterpillar (heroid as integer) as integer
result = -1
o = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  IF hero(i) - 1 = heroid THEN result = o
  o = o + 1
 END IF
NEXT i
rankincaterpillar = result
END FUNCTION

FUNCTION readfoemap (x as integer, y as integer, wide as integer, high as integer, fh as integer) as integer

a$ = CHR$(0)

o = 12 + (y * wide) + x

IF o <= LOF(fh) THEN
 GET #fh, o, a$
 readfoemap = ASC(a$)
ELSE
 readfoemap = 0
END IF

END FUNCTION

SUB scriptadvanced (id)

'contains advanced scripting stuff such as pixel-perfect movement

SELECT CASE AS CONST id

 CASE 135'--puthero
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   cropposition retvals(1), retvals(2), 20
   catx(retvals(0) * 5) = retvals(1)
   caty(retvals(0) * 5) = retvals(2)
  END IF
 CASE 136'--putnpc
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   cropposition retvals(1), retvals(2), 20
   npc(npcref).x = retvals(1)
   npc(npcref).y = retvals(2)
  END IF
 CASE 137'--putcamera
  gen(cameramode) = stopcam
  mapx = retvals(0)
  mapy = retvals(1)
  limitcamera
 CASE 138'--heropixelx
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = catx(retvals(0))
  END IF
 CASE 139'--heropixely
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = caty(retvals(0))
  END IF
 CASE 140'--npcpixelx
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = npc(npcref).x
  END IF
 CASE 141'--npcpixely
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = npc(npcref).y
  END IF
 CASE 142'--camerapixelx
  scriptret = mapx
 CASE 143'--camerapixely
  scriptret = mapy
 CASE 147'--read general
  IF retvals(0) >= 0 AND retvals(0) <= 104 THEN
   scriptret = gen(retvals(0))
  END IF
 CASE 148'--write general
  IF retvals(0) >= 0 AND retvals(0) <= 104 THEN
   gen(retvals(0)) = retvals(1)
  END IF
 CASE 159'--init mouse
  IF setmouse(mouse()) THEN scriptret = 1 ELSE scriptret = 0
  mouserect 0, 319, 0, 199
  readmouse mouse()
 CASE 160'--get mouse x
  scriptret = mouse(0)
 CASE 161'--get mouse y
  scriptret = mouse(1)
 CASE 162'--mouse button
  IF retvals(0) <= 1 THEN
   IF mouse(2) AND 2 ^ retvals(0) THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 163'--put mouse
  movemouse bound(retvals(0), 0, 319), bound(retvals(1), 0, 199)
  readmouse mouse()
 CASE 164'--mouse region
  mouserect bound(retvals(0), 0, 319), bound(retvals(1), 0, 319), bound(retvals(2), 0, 199), bound(retvals(3), 0, 199)
  readmouse mouse()
 CASE 178'--readgmap
  IF retvals(0) >= 0 AND retvals(0) <= 19 THEN
   scriptret = gmap(retvals(0))
  END IF
 CASE 179'--writegmap
  IF retvals(0) >= 0 AND retvals(0) <= 19 THEN
   gmap(retvals(0)) = retvals(1)
   IF retvals(0) = 5 THEN setoutside -1  'hint: always use the wrapper
   IF retvals(0) = 6 AND gmap(5) = 2 THEN setoutside retvals(1)
  END IF
END SELECT

END SUB

SUB scriptdump (s$)

DIM statestr$(7)
statestr$(0) = "none"
statestr$(1) = "wait"
statestr$(2) = "read"
statestr$(3) = "return"
statestr$(4) = "next"
statestr$(5) = "doarg"
statestr$(6) = "done"
statestr$(7) = "error"

IF scrat(nowscript).depth >= 0 THEN
  indent$ = STRING$(scrat(nowscript).depth, " ")
ELSE
  indent$ = STRING$(ABS(scrat(nowscript).depth), "<")
END IF

SELECT CASE scrat(nowscript).state
 CASE 0 TO 7
   state$ = statestr$(scrat(nowscript).state)
 CASE ELSE
   state$ = "illegal: " & scrat(nowscript).state
END SELECT

debug indent$ + "[" + s$ + "]"
debug indent$ + "script = " & nowscript
debug indent$ + "id     = " & scrat(nowscript).id & " " & scriptname$(scrat(nowscript).id)
debug indent$ + "ptr    = " & scrat(nowscript).ptr
debug indent$ + "state  = " & state$
debug indent$ + "kind   = " & curcmd->kind
debug indent$ + "value  = " & curcmd->value
debug indent$ + "argn   = " & scrat(nowscript).curargn
debug indent$ + "argc   = " & curcmd->argc

END SUB

SUB scriptmisc (id)
'contains a whole mess of scripting commands that do not depend on
'any main-module level local variables or GOSUBs

SELECT CASE AS CONST id

 CASE 0'--noop
  scripterr "encountered clean noop"
 CASE 1'--Wait (cycles)
  IF retvals(0) > 0 THEN
   GOSUB setwaitstate
  END IF
 CASE 2'--wait for all
  GOSUB setwaitstate
 CASE 3'--wait for hero
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   GOSUB setwaitstate
  END IF
 CASE 4'--wait for NPC
  IF retvals(0) >= -300 AND retvals(0) <= max_npc_defs THEN
   GOSUB setwaitstate
  END IF
 CASE 5'--suspend npcs
  setbit gen(), 44, suspendnpcs, 1
 CASE 6'--suspend player
  setbit gen(), 44, suspendplayer, 1
 CASE 7'--resume npcs
  setbit gen(), 44, suspendnpcs, 0
 CASE 8'--resume player
  setbit gen(), 44, suspendplayer, 0
 CASE 9'--wait for key
  GOSUB setwaitstate
 CASE 10'--walk hero
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   SELECT CASE retvals(1)
    CASE 0'--north
     catd(retvals(0) * 5) = 0
     ygo(retvals(0)) = retvals(2) * 20
    CASE 1'--east
     catd(retvals(0) * 5) = 1
     xgo(retvals(0)) = (retvals(2) * 20) * -1
    CASE 2'--south
     catd(retvals(0) * 5) = 2
     ygo(retvals(0)) = (retvals(2) * 20) * -1
    CASE 3'--west
     catd(retvals(0) * 5) = 3
     xgo(retvals(0)) = retvals(2) * 20
   END SELECT
  END IF
 CASE 12'--check tag
  scriptret = ABS(istag(retvals(0), 0))
 CASE 13'--set tag
  IF retvals(0) > 1 AND retvals(0) < 2000 THEN  'there are actually 2048 tags
   setbit tag(), 0, retvals(0), retvals(1)
   npcplot
  END IF
 CASE 17'--get item
  IF bound_item(retvals(0), "get item") THEN
   IF retvals(1) >= 1 THEN
    getitem retvals(0) + 1, retvals(1)
   END IF
  END IF
 CASE 18'--delete item
  IF bound_item(retvals(0), "delete item") THEN
   IF retvals(1) >= 1 THEN
    delitem retvals(0) + 1, retvals(1)
   END IF
  END IF
 CASE 19'--leader
  FOR i = 0 TO 3
   IF hero(0) > 0 THEN scriptret = hero(0) - 1: EXIT FOR
  NEXT i
 CASE 20'--get money
  gold = gold + retvals(0)
 CASE 21'--lose money
  gold = gold - retvals(0)
  IF gold < 0 THEN gold = 0
 CASE 22'--pay money
  IF gold - retvals(0) >= 0 THEN
   gold = gold - retvals(0)
   scriptret = -1
  ELSE
   scriptret = 0
  END IF
 CASE 25'--set hero frame
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   wtog(retvals(0)) = bound(retvals(1), 0, 1) * 2
  END IF
 CASE 27'--suspend overlay
  setbit gen(), 44, suspendoverlay, 1
 CASE 28'--play song
  'loadsong game + "." + STR$(retvals(0))
  wrappedsong retvals(0)
 CASE 29'--stop song
  stopsong
 CASE 30'--keyval
  IF retvals(0) >= 0 AND retvals(0) < 127 THEN
   scriptret = keyval(retvals(0)) AND 3
  ELSE
   debug "invalid scancode keyval(" & retvals(0) & ")"
  END IF
 CASE 31'--rank in caterpillar
  scriptret = rankincaterpillar(retvals(0))
 CASE 38'--camera follows hero
  gen(cameramode) = herocam
  gen(cameraArg) = bound(retvals(0), 0, 3) * 5
 CASE 40'--pan camera
  gen(cameramode) = pancam
  gen(cameraArg) = small(large(retvals(0), 0), 3)
  gen(cameraArg2) = large(retvals(1), 0) * (20 / large(retvals(2), 1))
  gen(cameraArg3) = large(retvals(2), 1)
 CASE 41'--focus camera
  gen(cameramode) = focuscam
  gen(cameraArg) = (retvals(0) * 20) - 150
  gen(cameraArg2) = (retvals(1) * 20) - 90
  gen(cameraArg3) = ABS(retvals(2))
  gen(cameraArg4) = ABS(retvals(2))
 CASE 42'--wait for camera
  GOSUB setwaitstate
 CASE 43'--hero x
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = catx(retvals(0) * 5) \ 20
  END IF
 CASE 44'--hero y
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = caty(retvals(0) * 5) \ 20
  END IF
 CASE 47'--suspend obstruction
  setbit gen(), 44, suspendobstruction, 1
 CASE 48'--resume obstruction
  setbit gen(), 44, suspendobstruction, 0
 CASE 49'--suspend hero walls
  setbit gen(), 44, suspendherowalls, 1
 CASE 50'--suspend NPC walls
  setbit gen(), 44, suspendnpcwalls, 1
 CASE 51'--resume hero walls
  setbit gen(), 44, suspendherowalls, 0
 CASE 53'--set hero direction
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   catd(retvals(0) * 5) = ABS(retvals(1)) MOD 4
  END IF
 CASE 57, 118'--suspend caterpillar
  setbit gen(), 44, suspendcatapillar, 1
 CASE 58, 119'--resume caterpillar
  setbit gen(), 44, suspendcatapillar, 0
  interpolatecat
 CASE 59'--wait for text box
  IF readbit(gen(), 44, suspendboxadvance) = 0 THEN
   GOSUB setwaitstate
  END IF
 CASE 60'--equip where
  scriptret = 0
  IF bound_item(retvals(1), "equip where") THEN
   IF bound_hero_party(retvals(0), "equip where") THEN
    loaditemdata buffer(), retvals(1)
    i = hero(retvals(0)) - 1
    IF i >= 0 THEN
     IF readbit(buffer(), 66, i) THEN
      scriptret = buffer(49)
     END IF
    END IF
   END IF
  END IF
 CASE 62, 168'--suspend random enemies
  setbit gen(), 44, suspendrandomenemies, 1
  '--resume random enemies is not here! it works different!
 CASE 65'--resume overlay
  setbit gen(), 44, suspendoverlay, 0
 CASE 70'--room in active party
  scriptret = 4 - herocount(3)
 CASE 71'--lock hero
  temp = findhero(retvals(0) + 1, 0, 40, 1)
  IF temp > -1 THEN setbit hmask(), 0, temp, 1
 CASE 72'--unlock hero
  temp = findhero(retvals(0) + 1, 0, 40, 1)
  IF temp > -1 THEN setbit hmask(), 0, temp, 0
 CASE 74'--set death script
  gen(42) = large(retvals(0), 0)
 CASE 75'--fade screen out
  FOR i = 0 TO 2
   retvals(i) = bound(iif(retvals(i), retvals(i) * 4 + 3, 0), 0, 255)
  NEXT
  fadeout retvals(0), retvals(1), retvals(2)
 CASE 76'--fade screen in
  fadein
 CASE 81'--set hero speed
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   herospeed(retvals(0)) = bound(retvals(1), 0, 20)
  END IF
 CASE 82'--inventory
  scriptret = countitem(retvals(0) + 1)
 CASE 84'--suspend box advance
  setbit gen(), 44, suspendboxadvance, 1
 CASE 85'--resume box advance
  setbit gen(), 44, suspendboxadvance, 0
 CASE 87'--set hero position
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
  cropposition retvals(1), retvals(2), 1
   FOR i = 0 TO 4
    catx(small(retvals(0) * 5 + i, 15)) = retvals(1) * 20
    caty(small(retvals(0) * 5 + i, 15)) = retvals(2) * 20
   NEXT i
  END IF
 CASE 90'--find hero
  scriptret = findhero(retvals(0) + 1, 0, 40, 1)
 CASE 91'--check equipment
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   scriptret = eqstuf(retvals(0), bound(retvals(1) - 1, 0, 4)) - 1
  ELSE
   scriptret = 0
  END IF
 CASE 92'--days of play
  scriptret = gen(51)
 CASE 93'--hours of play
  scriptret = gen(52)
 CASE 94'--minutes of play
  scriptret = gen(53)
 CASE 95'--resume NPC walls
  setbit gen(), 44, suspendnpcwalls, 0
 CASE 96'--set hero Z
  catz(bound(retvals(0), 0, 3) * 5) = retvals(1)
 CASE 102'--hero direction
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = catd(retvals(0) * 5)
  END IF
 CASE 103'--reset palette
  loadpalette master(), gen(genMasterPal)
  LoadUIColors uilook(), gen(genMasterPal)
 CASE 104'--tweak palette
  tweakpalette
 CASE 105'--read color
  IF retvals(0) >= 0 AND retvals(0) < 256 THEN
   IF retvals(1) = 0 THEN scriptret = master(retvals(0)).r / 4
   IF retvals(1) = 1 THEN scriptret = master(retvals(0)).g / 4
   IF retvals(1) = 2 THEN scriptret = master(retvals(0)).b / 4
  END IF
 CASE 106'--write color
  IF retvals(0) >= 0 AND retvals(0) < 256 THEN
   temp = bound(retvals(2), 0, 63)
   IF retvals(1) = 0 THEN master(retvals(0)).r = iif(temp, temp * 4 + 3, 0)
   IF retvals(1) = 1 THEN master(retvals(0)).g = iif(temp, temp * 4 + 3, 0)
   IF retvals(1) = 2 THEN master(retvals(0)).b = iif(temp, temp * 4 + 3, 0)
  END IF
 CASE 107'--update palette
  setpal master()
 CASE 108'--reseed random
  IF retvals(0) THEN
   RANDOMIZE retvals(0)
  ELSE
   RANDOMIZE TIMER
  END IF
 CASE 109'--grey scale palette
  greyscalepal
 CASE 114'--read global
  IF retvals(0) >= 0 AND retvals(0) <= 4095 THEN
   scriptret = global(retvals(0))
  ELSE
   scripterr "readglobal: Cannot read global " & retvals(0) & ". Out of range"
  END IF
 CASE 115'--write global
  IF retvals(0) >= 0 AND retvals(0) <= 4095 THEN
   global(retvals(0)) = retvals(1)
  ELSE
   scripterr "writeglobal: Cannot write global " & retvals(0) & ". Out of range"
  END IF
 CASE 116'--hero is walking
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   IF (xgo(retvals(0)) OR ygo(retvals(0))) THEN
    scriptret = 1
   ELSE
    scriptret = 0
   END IF
  END IF
 CASE 127'--teach spell
  scriptret = trylearn(bound(retvals(0), 0, 40), retvals(1), retvals(2))
 CASE 128'--forget spell
  scriptret = 0
  retvals(0) = bound(retvals(0), 0, 40)
  FOR i = 0 TO 3
   FOR j = 0 TO 23
    IF spell(retvals(0), i, j) = retvals(1) THEN
     spell(retvals(0), i, j) = 0
     scriptret = 1
    END IF
   NEXT j
  NEXT i
 CASE 129'--read spell
  IF retvals(0) >= 0 AND retvals(0) <= 40 AND retvals(1) >= 0 AND retvals(1) <= 3 AND retvals(2) >= 0 AND retvals(2) <= 23 THEN
   scriptret = spell(retvals(0), retvals(1), retvals(2))
  ELSE
   scriptret = 0
  END IF
 CASE 130'--write spell
  IF retvals(0) >= 0 AND retvals(0) <= 40 AND retvals(1) >= 0 AND retvals(1) <= 3 AND retvals(2) >= 0 AND retvals(2) <= 23 AND retvals(3) >= 0 THEN
   spell(retvals(0), retvals(1), retvals(2)) = retvals(3)
  END IF
 CASE 131'--knows spell
  scriptret = 0
  retvals(0) = bound(retvals(0), 0, 40)
  IF retvals(1) > 0 THEN
   FOR i = 0 TO 3
    FOR j = 0 TO 23
     IF spell(retvals(0), i, j) = retvals(1) THEN
      scriptret = 1
      EXIT FOR
     END IF
    NEXT j
   NEXT i
  END IF
 CASE 132'--can learn spell
  scriptret = 0
  DIM partyslot AS INTEGER
  DIM heroID as INTEGER
  partyslot = bound(retvals(0), 0, 40)
  heroID = hero(partyslot) - 1
  IF heroID = -1 THEN
   debug "can learn spell: fail on empty party slot " & partyslot
  ELSE
   IF retvals(1) > 0 THEN
    DIM her as herodef ptr
    her = Allocate(sizeof(herodef))
    loadherodata her, heroID
    FOR i = 0 TO 3
     FOR j = 0 TO 23
      IF spell(partyslot, i, j) = 0 THEN
       IF her->spell_lists(i,j).attack = retvals(1) AND her->spell_lists(i,j).learned = retvals(2) THEN
        scriptret = 1
        EXIT FOR
       END IF
      END IF
     NEXT j
    NEXT i
    Deallocate(her)
   END IF
  END IF
 CASE 133'--hero by slot
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   scriptret = hero(retvals(0)) - 1
  ELSE
   scriptret = -1
  END IF
 CASE 134'--hero by rank
  scriptret = herobyrank(retvals(0))
 CASE 145'--pick hero
  scriptret = onwho(readglobalstring$(135, "Which Hero?", 20), 1)
 CASE 146'--rename hero by slot
  IF bound_hero_party(retvals(0), "rename hero by slot") THEN
   IF hero(retvals(0)) > 0 THEN
    renamehero retvals(0)
   END IF
  END IF
 CASE 171'--saveslotused
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   IF checksaveslot(retvals(0) - 1) = 3 THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 172'--importglobals
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   IF retvals(1) = -1 THEN 'importglobals(slot)
    retvals(1) = 0
    retvals(2) = 1024
   END IF
   IF retvals(1) >= 0 AND retvals(1) <= 1024 THEN
    IF retvals(2) = -1 THEN 'importglobals(slot,id)
     remval = global(retvals(1))
     loadglobalvars retvals(0) - 1, retvals(1), retvals(1)
     scriptret = global(retvals(1))
     global(retvals(1)) = remval
    ELSE                    'importglobals(slot,first,last)
     IF retvals(2) <= 1024 AND retvals(1) <= retvals(2) THEN
      loadglobalvars retvals(0) - 1, retvals(1), retvals(2)
     END IF
    END IF
   END IF
  END IF
 CASE 173'--exportglobals
  IF retvals(0) >= 1 AND retvals(0) <= 32 AND retvals(1) >= 0 AND retvals(2) <= 1024 AND retvals(1) <= retvals(2) THEN
   saveglobalvars retvals(0) - 1, retvals(1), retvals(2)
  END IF
 CASE 175'--deletesave
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   erasesaveslot retvals(0) - 1
  END IF
 CASE 176'--runscriptbyid
  rsr = runscript(retvals(0), nowscript + 1, 0, "indirect", plottrigger) 'possible to get ahold of triggers
  IF rsr = 1 THEN
   '--fill heap with return values
   FOR i = 1 TO scrat(nowscript - 1).curargc - 1  'flexible argument number! (note that argc has been saved here by runscript)
    setScriptArg i - 1, retvals(i)
   NEXT i
  ELSE
   scriptret = -1
  END IF
 CASE 180'--mapwidth
  scriptret = scroll(0)
 CASE 181'--mapheight
  scriptret = scroll(1)
 CASE 187'--getmusicvolume
  scriptret = fmvol * 17
 CASE 188'--setmusicvolume
  fmvol = bound(retvals(0), 0, 255) \ 16
  setfmvol fmvol
 CASE 189, 307'--get formation song
  fh = FREEFILE
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxFormation) THEN
   OPEN tmpdir & "for.tmp" FOR BINARY AS #fh
   scriptret = readshort(fh, retvals(0) * 80 + 67)
   IF id = 307 THEN scriptret -= 1
   CLOSE #fh
  END IF
 CASE 190'--set formation song
  'set formation song never worked, so don't bother with backwards compatibility
  fh = FREEFILE
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxFormation) AND retvals(1) >= -2 AND retvals(1) <= gen(genMaxSong) THEN
   OPEN tmpdir & "for.tmp" FOR BINARY AS #fh
   WriteShort fh, retvals(0) * 80 + 67, retvals(1) + 1
   CLOSE #fh
  ELSE
   scriptret = -1
  END IF
 CASE 191'--hero frame
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = wtog(retvals(0)) \ 2
  END IF
 CASE 195'--load sound (BACKWARDS COMPATABILITY HACK )
  'This opcode is not exposed in plotscr.hsd and should not be used in any new scripts
  IF retvals(0) >= 0 AND retvals(0) <= 7 THEN
   backcompat_sound_slot_mode = -1
   backcompat_sound_slots(retvals(0)) = retvals(1) + 1
  END IF
 CASE 196'--free sound (BACKWARDS COMPATABILITY HACK)
  'This opcode is not exposed in plotscr.hsd and should not be used in any new scripts
  IF retvals(0) >= 0 AND retvals(0) <= 7 THEN
   backcompat_sound_slots(retvals(0)) = 0
  END IF
 CASE 197'--play sound
  sfxid = backcompat_sound_id(retvals(0))
  IF sfxid >= 0 AND sfxid <= gen(genMaxSFX) THEN
   if retvals(2) then stopsfx sfxid
   playsfx sfxid, retvals(1)
   scriptret = -1
  END IF
 CASE 198'--pause sound
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxSFX) THEN
   pausesfx retvals(0)
   scriptret = -1
  END IF
 CASE 199'--stop sound
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxSFX) THEN
   stopsfx retvals(0)
   scriptret = -1
  END IF
 CASE 200'--system hour (time$ is always hh:mm:ss)
  scriptret = str2int(MID$(TIME$, 1, 2))
 CASE 201'--system minute
  scriptret = str2int(MID$(TIME$, 4, 2))
 CASE 202'--system second
  scriptret = str2int(MID$(TIME$, 7, 2))
 CASE 203'--current song
  scriptret = presentsong
 CASE 204'--get hero name(str,her)
  IF bound_plotstr(retvals(0), "get hero name") AND bound_hero_party(retvals(1), "get hero name") THEN
   plotstr(retvals(0)).s = names(retvals(1))
   scriptret = 1
  ELSE
   scriptret = 0
  END IF
 CASE 205'--set hero name
  IF bound_plotstr(retvals(0), "set hero name") AND bound_hero_party(retvals(1), "set hero name") THEN
   names(retvals(1)) = plotstr(retvals(0)).s
   scriptret = 1
  ELSE
   scriptret = 0
  END IF
 CASE 206'--get item name(str,itm)
  scriptret = 0
  IF bound_plotstr(retvals(0), "get item name") THEN
   IF bound_item(retvals(1), "get item name") THEN
    plotstr(retvals(0)).s = readitemname(retvals(1))
    scriptret = 1
   END IF
  END IF
 CASE 207'--get map name(str,map)
   IF bound_plotstr(retvals(0), "get map name") = NO OR retvals(1) < 0 OR retvals(1) > gen(genMaxMap) THEN
   scriptret = 0
  ELSE
   plotstr(retvals(0)).s = getmapname$(retvals(1))
   scriptret = 1
  END IF
 CASE 208'--get attack name(str,atk)
  IF bound_plotstr(retvals(0), "get attack name") = NO OR retvals(1) < 0 OR retvals(1) > gen(genMaxAttack) THEN
   scriptret = 0
  ELSE
   plotstr(retvals(0)).s = readattackname$(retvals(1) + 1)
   scriptret = 1
  END IF
 CASE 209'--get global string(str,glo)
  IF bound_plotstr(retvals(0), "get global string") = NO OR retvals(1) < 0 OR retvals(1) > 159 THEN
   scriptret = 0
  ELSE
   plotstr(retvals(0)).s = readglobalstring$(retvals(1), "", 255)
   scriptret = 1
  END IF
 CASE 211'--clear string
  IF bound_plotstr(retvals(0), "clear string") THEN plotstr(retvals(0)).s = ""
 CASE 212'--append ascii
  IF bound_plotstr(retvals(0), "append ascii") THEN
   IF retvals(1) >= 0 AND retvals(1) <= 255 THEN
    plotstr(retvals(0)).s = plotstr(retvals(0)).s + CHR$(retvals(1))
    scriptret = LEN(plotstr(retvals(0)).s)
   END IF
  END IF
 CASE 213'--append number
  IF bound_plotstr(retvals(0), "append number") THEN
   plotstr(retvals(0)).s = plotstr(retvals(0)).s & retvals(1)
   scriptret = LEN(plotstr(retvals(0)).s)
  END IF
 CASE 214'--copy string
  IF bound_plotstr(retvals(0), "copy string") AND bound_plotstr(retvals(1), "copy string") THEN
   plotstr(retvals(0)).s = plotstr(retvals(1)).s
  END IF
 CASE 215'--concatenate strings
  IF bound_plotstr(retvals(0), "concatenate string") AND bound_plotstr(retvals(1), "concatenate string") THEN
   plotstr(retvals(0)).s = plotstr(retvals(0)).s + plotstr(retvals(1)).s
   scriptret = LEN(plotstr(retvals(0)).s)
  END IF
 CASE 216'--string length
  IF bound_plotstr(retvals(0), "string length") THEN
   scriptret = LEN(plotstr(retvals(0)).s)
  END IF
 CASE 217'--delete char
  IF bound_plotstr(retvals(0), "delete char") THEN
   IF retvals(1) >= 1 AND retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
    temp2$ = LEFT$(plotstr(retvals(0)).s, retvals(1) - 1)
    temp3$ = MID$(plotstr(retvals(0)).s, retvals(1) + 1)
    plotstr(retvals(0)).s = temp2$ + temp3$
    temp3$ = ""
    temp2$ = ""
   END IF
  END IF
 CASE 218'--replace char
  IF bound_plotstr(retvals(0), "replace char") AND retvals(2) >= 0 AND retvals(2) <= 255 THEN
   IF retvals(1) >= 1 AND retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
    MID$(plotstr(retvals(0)).s, retvals(1), 1) = CHR$(retvals(2))
   END IF
  END IF
 CASE 219'--ascii from string
  IF bound_plotstr(retvals(0), "ascii from string") AND retvals(1) >= 1 AND retvals(1) <= LEN(plotstr(retvals(0)).s) THEN
   scriptret = plotstr(retvals(0)).s[retvals(1)-1]'you can index strings a la C
  END IF
 CASE 220'--position string
  IF bound_plotstr(retvals(0), "position string") THEN
   plotstr(retvals(0)).X = retvals(1)
   plotstr(retvals(0)).Y = retvals(2)
  END IF
 CASE 221'--set string bit
  IF bound_plotstr(retvals(0), "set string bit") AND retvals(1) >= 0 AND retvals(1) <= 15 THEN
   if retvals(2) then
    plotstr(retvals(0)).bits = plotstr(retvals(0)).bits or 2 ^ retvals(1)
   else
    plotstr(retvals(0)).bits = plotstr(retvals(0)).bits and not 2 ^ retvals(1)
   end if
  END IF
 CASE 222'--get string bit
  IF bound_plotstr(retvals(0), "get string bit") AND retvals(1) >= 0 AND retvals(1) <= 15 THEN
   'scriptret = readbit(plotstrBits(), retvals(0), retvals(1))
   scriptret = plotstr(retvals(0)).bits AND 2 ^ retvals(1)
   IF scriptret THEN scriptret = 1
  END IF
 CASE 223'--string color
  IF bound_plotstr(retvals(0), "string color") THEN
   plotstr(retvals(0)).Col = bound(retvals(1), 0, 255)
   plotstr(retvals(0)).BGCol = bound(retvals(2), 0, 255)
  END IF
 CASE 224'--string X
  IF bound_plotstr(retvals(0), "string X") THEN
   scriptret = plotstr(retvals(0)).X
  END IF
 CASE 225'--string Y
  IF bound_plotstr(retvals(0), "string Y") THEN
   scriptret = plotstr(retvals(0)).Y
  END IF
 CASE 226'--system day (date$ is always mm-dd-yyyy)
  scriptret = str2int(MID$(DATE$, 4, 2))
 CASE 227'--system month
  scriptret = str2int(MID$(DATE$, 1, 2))
 CASE 228'--system year
  scriptret = str2int(MID$(DATE$, 7, 4))
 CASE 229'--string compare
  IF bound_plotstr(retvals(0), "string compare") AND bound_plotstr(retvals(1), "string compare") THEN
   scriptret = (plotstr(retvals(0)).s = plotstr(retvals(1)).s)
  END IF
 CASE 230'--read enemy data
  f = FREEFILE
  OPEN tmpdir & "dt1.tmp" FOR BINARY AS #f
  scriptret = ReadShort(f, (CLNG(bound(retvals(0), 0, gen(genMaxEnemy))) * CLNG(320)) + (bound(retvals(1), 0, 159) * 2) + 1)
  CLOSE #f
 CASE 231'--write enemy data
  f = FREEFILE
  OPEN tmpdir & "dt1.tmp" FOR BINARY AS #f
  Writeshort f, (CLNG(bound(retvals(0), 0, gen(genMaxEnemy))) * CLNG(320)) + (bound(retvals(1), 0, 159) * 2) + 1, retvals(2)
  CLOSE #f
 CASE 232'--trace
  IF bound_plotstr(retvals(0), "trace") THEN
   debug "TRACE: " + plotstr(retvals(0)).s
  END IF
 CASE 233'--get song name
  IF bound_plotstr(retvals(0), "get song name") AND retvals(1) >= 0 THEN
   plotstr(retvals(0)).s = getsongname$(retvals(1))
  END IF
 CASE 235'--key is pressed
  SELECT CASE retvals(0)
  CASE 1 TO 127 'keyboard
   IF keyval(retvals(0)) THEN scriptret = 1 ELSE scriptret = 0
  CASE 128 TO 147 'joystick
   dim b as integer, xaxis as integer, yaxis as integer '0 >= x and y, >= 100
   IF io_readjoysane(bound(retvals(1),0,3),b,xaxis,yaxis) THEN
    IF retvals(0) >= 128 AND retvals(0) <= 143 THEN
     scriptret = (b SHR (retvals(0) - 128)) AND 1
    ELSEIF retvals(0) = 144 THEN 'x left
     'debug XSTR$(xaxis)
     scriptret = abs(xaxis <= -50) 'true = -1...
    ELSEIF retvals(0) = 145 THEN 'x right
     scriptret = abs(xaxis >= 50)
    ELSEIF retvals(0) = 146 THEN 'y up
     scriptret = abs(yaxis <= -50)
    ELSEIF retvals(0) = 147 THEN 'y down
     scriptret = abs(yaxis >= 50)
    END IF
   ELSE
    scriptret = 0
   END IF
  CASE ELSE
   scriptret = 0
  END SELECT
 CASE 236'--sound is playing
  sfxid = backcompat_sound_id(retvals(0))
  IF sfxid >= 0 AND sfxid <= gen(genMaxSFX) THEN
   scriptret = sfxisplaying(sfxid)
  END IF
 CASE 237'--sound slots (BACKWARDS COMPATABILITY HACK)
  'This opcode is not exposed in plotscr.hsd and should not be used in any new scripts
  IF backcompat_sound_slot_mode THEN
    scriptret = 8
  END IF
 CASE 238'--search string
  IF bound_plotstr(retvals(0), "search string") AND bound_plotstr(retvals(1), "search string") THEN
    WITH plotstr(retvals(0))
     scriptret = instr(bound(retvals(2), 1, LEN(.s)), .s, plotstr(retvals(1)).s)
    END WITH
  ELSE
   scriptret = 0
  END IF
 CASE 239'--trim string
  IF bound_plotstr(retvals(0), "trim string") THEN
   IF retvals(1) = -1 THEN
    plotstr(retvals(0)).s = trim$(plotstr(retvals(0)).s)
   ELSE
    IF retvals(1) <= LEN(plotstr(retvals(0)).s) AND retvals(2) >= 1 THEN
     retvals(1) = large(retvals(1),1)
     'retvals(2) = bound(retvals(2),1,LEN(plotstr(retvals(0)).s))
     plotstr(retvals(0)).s = MID$(plotstr(retvals(0)).s,retvals(1),retvals(2))
    ELSE
     plotstr(retvals(0)).s = ""
    END IF
   END IF
  END IF
 CASE 240'-- string from textbox
  IF bound_plotstr(retvals(0), "string from textbox") THEN
   DIM box AS TextBox
   retvals(1) = bound(retvals(1),0,gen(genMaxTextbox))
   retvals(2) = bound(retvals(2),0,7)
   LoadTextBox box, retvals(1)
   plotstr(retvals(0)).s = box.text(retvals(2))
   IF NOT retvals(3) THEN embedtext plotstr(retvals(0)).s
   plotstr(retvals(0)).s = trim$(plotstr(retvals(0)).s)
  END IF
 CASE 241'-- expand string(id)
  IF bound_plotstr(retvals(0), "expand string") THEN
   embedtext plotstr(retvals(0)).s
  END IF
 CASE 242'-- joystick button
  retvals(0) = bound(retvals(0)-1,0,15)
  retvals(1) = bound(retvals(1),0,3)

  b = 0
  IF io_readjoysane(retvals(1),b,0,0) THEN
   scriptret = (b SHR retvals(0)) AND 1
  ELSE
   scriptret = 0
  END IF
 CASE 243'-- joystick axis
  retvals(0) = bound(retvals(0),0,1)
  retvals(2) = bound(retvals(2),0,3)

  xaxis = 0
  yaxis = 0
  IF io_readjoysane(retvals(2),0,xaxis,yaxis) THEN
   IF retvals(0) = 0 THEN'x axis
    'debug "x" + XSTR$(xaxis)
    scriptret = int((xaxis / 100) * retvals(1)) 'normally, xaxis * 100
   ELSEIF retvals(0) = 1 THEN 'y axis
    'debug "y" + XSTR$(yaxis)
    scriptret = int((yaxis / 100) * retvals(1)) 'normally, yaxis * 100
   END IF
  ELSE
   'debug "joystick failed"
   scriptret = 0
  END IF
 CASE 244'--wait for scancode
  GOSUB setwaitstate
 CASE 249'--party money
  scriptret = gold
 CASE 250'--set money
  IF retvals(0) >= 0 THEN gold = retvals(0)
 CASE 251'--set string from table
  IF bound_plotstr(retvals(0), "set string from table") AND script(scrat(nowscript).scrnum).strtable THEN
   plotstr(retvals(0)).s = read32bitstring$(scrat(nowscript).scrdata + script(scrat(nowscript).scrnum).strtable + retvals(1))
  END IF
 CASE 252'--append string from table
  IF bound_plotstr(retvals(0), "append string from table") AND script(scrat(nowscript).scrnum).strtable THEN
   plotstr(retvals(0)).s += read32bitstring$(scrat(nowscript).scrdata + script(scrat(nowscript).scrnum).strtable + retvals(1))
  END IF
 CASE 256'--suspend map music
  setbit gen(), 44, suspendambientmusic, 1
 CASE 257'--resume map music
  setbit gen(), 44, suspendambientmusic, 0
 CASE 260'--settimer(id, count, speed, trigger, string, flags)
  IF retvals(0) >= 0 AND retvals(0) < 16 THEN
    WITH timers(retvals(0))
      IF retvals(1) > -1 THEN .count = retvals(1): .ticks = 0
      IF retvals(2) > -1 THEN
        .speed = retvals(2)
      ELSEIF retvals(2) = -1 AND .speed = 0 THEN
        .speed = 18
      END IF
      IF retvals(3) <> -1 THEN .trigger = retvals(3)
      IF retvals(4) <> -1 THEN
       IF bound_plotstr(retvals(4), "set timer") THEN
        .st = retvals(4) + 1
        plotstr(retvals(4)).s = seconds2str(.count)
       END IF
      END IF
      IF retvals(5)<> -1 THEN .flags = retvals(5)
      IF .speed < -1 THEN .speed *= -1: .speed -= 1
    END WITH
  END IF
 CASE 261'--stoptimer
  IF retvals(0) >= 0 AND retvals(0) < 16 THEN timers(retvals(0)).speed = 0
 CASE 262'--readtimer
  IF retvals(0) >= 0 AND retvals(0) < 16 THEN scriptret = timers(retvals(0)).count
 CASE 263'--getcolor
  IF retvals(0) >= 0 AND retvals(0) < 256 THEN
   scriptret = master(retvals(0)).col
  END IF
 CASE 264'--setcolor
  IF retvals(0) >= 0 AND retvals(0) < 256 THEN
   retvals(1) = retvals(1) OR &HFF000000 'just in case, set the alpha
   master(retvals(0)).col = retvals(1)
  END IF
 CASE 265'--rgb
  scriptret = RGB(bound(retvals(0),0,255), bound(retvals(1),0,255), bound(retvals(2),0,255))
 CASE 266'--extractcolor
  dim c as rgbcolor
  c.col = retvals(0)
  SELECT CASE retvals(1)
   CASE 0
    scriptret = c.r
   CASE 1
    scriptret = c.g
   CASE 2
    scriptret = c.b
  END SELECT
 CASE 268'--loadpalette
  IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxMasterPal) THEN
   loadpalette master(), retvals(0)
   LoadUIColors uilook(), retvals(0)
  END IF
 CASE 273'--milliseconds
  scriptret = fmod((TIMER * 1000) + 2147483648.0, 4294967296.0) - 2147483648.0
 CASE 308'--add enemy to formation (formation, enemy id, x, y, slot = -1)
  scriptret = -1
  IF bound_formation(retvals(0), "add enemy to formation") AND retvals(1) >= 0 AND retvals(1) <= gen(genMaxEnemy) THEN
   loadrecord buffer(), tmpdir & "for.tmp", 40, retvals(0)
   temp = -1
   FOR i = 0 TO 7
    IF buffer(i * 4) = 0 THEN temp = i: EXIT FOR
   NEXT
   IF retvals(4) >= 0 AND retvals(4) <= 7 THEN
    IF buffer(retvals(4) * 4) = 0 THEN temp = retvals(4)
   END IF
   IF temp >= 0 THEN
    szindex = ReadShort(tmpdir & "dt1.tmp", retvals(1) * 320 + 111) 'picture size
    IF szindex = 0 THEN size = 34
    IF szindex = 1 THEN size = 50
    IF szindex = 2 THEN size = 80
    buffer(temp * 4) = retvals(1) + 1
    buffer(temp * 4 + 1) = large( (small(retvals(2), 230) - size / 2) , 0)  'approximately the 0 - 250 limit of the formation editor
    buffer(temp * 4 + 2) = large( (small(retvals(3), 199) - size) , 0)
   END IF
   storerecord buffer(), tmpdir & "for.tmp", 40, retvals(0)
   scriptret = temp
  END IF
 CASE 309'--find enemy in formation (formation, enemy id, number)
  IF bound_formation(retvals(0), "find enemy in formation") THEN
   loadrecord buffer(), tmpdir & "for.tmp", 40, retvals(0)
   temp = 0
   scriptret = -1
   FOR i = 0 TO 7
    IF buffer(i * 4) > 0 AND (retvals(1) = buffer(i * 4) - 1 OR retvals(1) = -1) THEN
     IF retvals(2) = temp THEN scriptret = i: EXIT FOR
     temp += 1
    END IF
   NEXT
   IF retvals(2) = -1 THEN scriptret = temp
  END IF
 CASE 310'--delete enemy from formation (formation, slot)
  IF bound_formation_slot(retvals(0), retvals(1), "delete enemy from formation") THEN
   WriteShort tmpdir & "for.tmp", retvals(0) * 80 + retvals(1) * 8 + 1, 0
  END IF
 CASE 311'--formation slot enemy (formation, slot)
  scriptret = -1
  IF bound_formation_slot(retvals(0), retvals(1), "formation slot enemy") THEN
   scriptret = ReadShort(tmpdir & "for.tmp", retvals(0) * 80 + retvals(1) * 8 + 1) - 1
  END IF
 CASE 312, 313'--formation slot x (formation, slot), formation slot y (formation, slot)
  IF bound_formation_slot(retvals(0), retvals(1), "formation slot x/y") THEN
   temp = ReadShort(tmpdir & "for.tmp", retvals(0) * 80 + retvals(1) * 8 + 1) 'enemy id + 1
   scriptret = ReadShort(tmpdir & "for.tmp", retvals(0) * 80 + retvals(1) * 8 + (id - 311) * 2 + 1) 'x or y
   'now find the position of the bottom center of the enemy sprite
   IF temp THEN
    temp = ReadShort(tmpdir & "dt1.tmp", (temp - 1) * 320 + 111) 'picture size
    IF temp = 0 THEN size = 34
    IF temp = 1 THEN size = 50
    IF temp = 2 THEN size = 80
    IF id = 312 THEN scriptret += size \ 2 ELSE scriptret += size
   END IF
  END IF
 CASE 314'--set formation background (formation, background, animation frames, animation ticks)
  IF bound_formation(retvals(0), "set formation background") AND retvals(1) >= 0 AND retvals(1) <= gen(genMaxBackdrop) - 1 THEN 
   loadrecord buffer(), tmpdir & "for.tmp", 40, retvals(0)
   buffer(32) = retvals(1)
   buffer(34) = bound(retvals(2) - 1, 0, 49)
   buffer(35) = bound(retvals(3), 0, 1000)
   storerecord buffer(), tmpdir & "for.tmp", 40, retvals(0)
  END IF
 CASE 315'--get formation background (formation)
  IF bound_formation(retvals(0), "get formation background") THEN
   scriptret = ReadShort(tmpdir & "for.tmp", retvals(0) * 80 + retvals(1) * 8 + 32 + 1)
  END IF
 CASE 316'--last formation
  scriptret = lastformation
 CASE 317'--random formation (formation set)
  IF retvals(0) >= 1 AND retvals(0) <= 255 THEN
   scriptret = random_formation(retvals(0) - 1)
  END IF
 CASE 318'--formation set frequency (formation set)
  IF retvals(0) >= 1 AND retvals(0) <= 255 THEN
   scriptret = ReadShort(game + ".efs", (retvals(0) - 1) * 50 + 1)
  END IF
 CASE 319'--formation probability (formation set, formation)
  IF retvals(0) >= 1 AND retvals(0) <= 255 THEN
   loadrecord buffer(), game + ".efs", 25, retvals(0) - 1
   temp = 0
   scriptret = 0
   FOR i = 1 TO 20
    IF buffer(i) = retvals(1) + 1 THEN scriptret += 1
    IF buffer(i) > 0 THEN temp += 1
   NEXT
   'probability in percentage points
   IF temp > 0 THEN scriptret = (scriptret * 100) / temp
  END IF
 CASE 321'--get hero speed (hero)
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = herospeed(retvals(0))
  END IF
 CASE 322'--load hero sprite
  scriptret = load_sprite_plotslice(0, retvals(0), retvals(1))
 CASE 323'--free sprite
  IF valid_plotslice(retvals(0), "free sprite") THEN
   IF plotslices(retvals(0))->SliceType = slSprite THEN
    DeleteSlice(@plotslices(retvals(0)))
   ELSE
    debug "free sprite: slice " & retvals(0) & " is a " & SliceTypeName(plotslices(retvals(0)))
   END IF
  END IF
 CASE 324 '--place sprite
  IF valid_plotslice(retvals(0), "place sprite") THEN
   WITH *plotslices(retvals(0))
    .x = retvals(1)
    .y = retvals(2)
   END WITH
  END IF
 CASE 325 '--set sprite visible
  IF valid_plotslice(retvals(0), "set sprite visible") THEN
   WITH *plotslices(retvals(0))
    .Visible = (retvals(1) <> 0)
   END WITH
  END IF
 CASE 326 '--set sprite palette
  IF valid_plotslice(retvals(0), "set sprite palette") THEN
   ChangeSpriteSlice plotslices(retvals(0)), , ,retvals(1)
  END IF
 CASE 327 '--replace hero sprite
  change_sprite_plotslice retvals(0), 0, retvals(1), retvals(2)
 CASE 328 '--set sprite frame
  IF valid_plotslice(retvals(0), "set sprite frame") THEN
   ChangeSpriteSlice plotslices(retvals(0)), , , , retvals(1)
  END IF
 CASE 329'--load walkabout sprite
  scriptret = load_sprite_plotslice(4, retvals(0), retvals(1))
 CASE 330 '--replace walkabout sprite
  change_sprite_plotslice retvals(0), 4, retvals(1), retvals(2)
 CASE 331'--load weapon sprite
  scriptret = load_sprite_plotslice(5, retvals(0), retvals(1))
 CASE 332 '--replace weapon sprite
  change_sprite_plotslice retvals(0), 5, retvals(1), retvals(2)
 CASE 333'--load small enemy sprite
  scriptret = load_sprite_plotslice(1, retvals(0), retvals(1))
 CASE 334 '--replace small enemy sprite
  change_sprite_plotslice retvals(0), 1, retvals(1), retvals(2)
 CASE 335'--load medium enemy sprite
  scriptret = load_sprite_plotslice(2, retvals(0), retvals(1))
 CASE 336 '--replace medium enemy sprite
  change_sprite_plotslice retvals(0), 2, retvals(1), retvals(2)
 CASE 337'--load large enemy sprite
  scriptret = load_sprite_plotslice(3, retvals(0), retvals(1))
 CASE 338 '--replace large enemy sprite
  change_sprite_plotslice retvals(0), 3, retvals(1), retvals(2)
 CASE 339'--load attack sprite
  scriptret = load_sprite_plotslice(6, retvals(0), retvals(1))
 CASE 340 '--replace attack sprite
  change_sprite_plotslice retvals(0), 6, retvals(1), retvals(2)
 CASE 341'--load border sprite
  scriptret = load_sprite_plotslice(7, retvals(0), retvals(1))
 CASE 342 '--replace border sprite
  change_sprite_plotslice retvals(0), 7, retvals(1), retvals(2)
 CASE 343'--load portrait sprite
  scriptret = load_sprite_plotslice(8, retvals(0), retvals(1))
 CASE 344 '--replace portrait sprite
  change_sprite_plotslice retvals(0), 8, retvals(1), retvals(2)
 CASE 345 '--clone sprite
  IF valid_plotsprite(retvals(0), "clone sprite") THEN
   DIM dat AS SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   WITH *dat
    scriptret = load_sprite_plotslice(.spritetype, .record, .pal)
    change_sprite_plotslice scriptret, .spritetype, .record, .pal, .frame, .flipHoriz, .flipVert
   END WITH
  END IF
 CASE 346 '--get sprite frame
  IF valid_plotsprite(retvals(0), "get sprite frame") THEN
   DIM dat AS SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->frame
  END IF
 CASE 347 '--sprite frame count
  IF valid_plotsprite(retvals(0), "sprite frame count") THEN
   DIM dat AS SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   WITH *dat
    scriptret = sprite_sizes(.spritetype).frames
   END WITH
  END IF
 CASE 348 '--slice x
  IF valid_plotslice(retvals(0), "slice x") THEN
   scriptret = plotslices(retvals(0))->X
  END IF
 CASE 349 '--slice y
  IF valid_plotslice(retvals(0), "slice y") THEN
   scriptret = plotslices(retvals(0))->Y
  END IF
 CASE 350 '--set slice x
  IF valid_plotslice(retvals(0), "set slice x") THEN
   plotslices(retvals(0))->X = retvals(1)
  END IF
 CASE 351 '--set slice y
  IF valid_plotslice(retvals(0), "set slice y") THEN
   plotslices(retvals(0))->Y = retvals(1)
  END IF
 CASE 352 '--slice width
  IF valid_plotslice(retvals(0), "slice width") THEN
   scriptret = plotslices(retvals(0))->Width
  END IF
 CASE 353 '--slice height
  IF valid_plotslice(retvals(0), "slice height") THEN
   scriptret = plotslices(retvals(0))->Height
  END IF
 CASE 354 '--set horiz align
  IF valid_plotslice(retvals(0), "set horiz align") THEN
   plotslices(retvals(0))->AlignHoriz = retvals(1)
  END IF
 CASE 355 '--set vert align
  IF valid_plotslice(retvals(0), "set vert align") THEN
   plotslices(retvals(0))->AlignVert = retvals(1)
  END IF
 CASE 356 '--set horiz anchor
  IF valid_plotslice(retvals(0), "set horiz anchor") THEN
   plotslices(retvals(0))->AnchorHoriz = retvals(1)
  END IF
 CASE 357 '--set vert anchor
  IF valid_plotslice(retvals(0), "set vert anchor") THEN
   plotslices(retvals(0))->AnchorVert = retvals(1)
  END IF
 CASE 358 '--number from string
  IF bound_plotstr(retvals(0), "number from string") THEN
   scriptret = str2int(plotstr(retvals(0)).s, retvals(1))
  END IF
 CASE 359 '--slice is sprite
  IF valid_plotslice(retvals(0), "slice is sprite") THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slSprite THEN scriptret = 1
  END IF
 CASE 360 '--sprite layer
  scriptret = find_plotslice_handle(SliceTable.ScriptSprite)
 CASE 361 '--free slice
  IF valid_plotslice(retvals(0), "free slice") THEN
   DIM sl AS Slice Ptr
   sl = plotslices(retvals(0))
   IF sl->SliceType = slRoot OR sl->SliceType = slSpecial THEN
    debug "free slice: cannot free " & SliceTypeName(sl) & " slice " & retvals(0)
   ELSE
    DeleteSlice(@plotslices(retvals(0)))
   END IF
  END IF
 CASE 362 '--first child
  IF valid_plotslice(retvals(0), "first child") THEN
   DIM sl AS Slice Ptr
   sl = plotslices(retvals(0))
   scriptret = find_plotslice_handle(sl->FirstChild)
  END IF
 CASE 363 '--next sibling
  IF valid_plotslice(retvals(0), "next sibling") THEN
   DIM sl AS Slice Ptr
   sl = plotslices(retvals(0))
   scriptret = find_plotslice_handle(sl->NextSibling)
  END IF
 CASE 364 '--create container
  DIM sl AS Slice Ptr
  sl = NewSliceOfType(slContainer, SliceTable.scriptsprite)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
  scriptret = create_plotslice_handle(sl)
 CASE 365 '--set parent
  IF valid_plotslice(retvals(0), "set parent") AND valid_plotslice(retvals(1), "set parent") THEN
   IF verifySliceLineage(plotslices(retvals(0)), plotslices(retvals(1))) THEN
    SetSliceParent plotslices(retvals(0)), plotslices(retvals(1))
   ELSE
    debug "set parent: cannot make slice " & retvals(0) & " a child of its own child " & retvals(1)
   END IF
  END IF
 CASE 366 '--check parentage
  IF valid_plotslice(retvals(0), "check parentage") AND valid_plotslice(retvals(1), "check parentage") THEN
   IF verifySliceLineage(plotslices(retvals(0)), plotslices(retvals(1))) THEN
    scriptret = 1
   END IF
  END IF
 CASE 367 '--slice screen x
  IF valid_plotslice(retvals(0), "slice screen x") THEN
   DIM sl AS Slice Ptr
   sl = plotslices(retvals(0))
   RefreshSliceScreenPos sl
   scriptret = sl->ScreenX + SliceXAnchor(sl)
  END IF
 CASE 368 '--slice screen y
  IF valid_plotslice(retvals(0), "slice screen y") THEN
   DIM sl AS Slice Ptr
   sl = plotslices(retvals(0))
   RefreshSliceScreenPos sl
   scriptret = sl->ScreenY + SliceYAnchor(sl)
  END IF
 CASE 369 '--slice is container
  IF valid_plotslice(retvals(0), "slice is container") THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slContainer THEN scriptret = 1
  END IF
 CASE 370 '--create rect
  DIM sl AS Slice Ptr
  sl = NewSliceOfType(slRectangle, SliceTable.scriptsprite)
  sl->Width = retvals(0)
  sl->Height = retvals(1)
  IF bound_arg(retvals(2), -1, 14, "create rect", "style") THEN
   ChangeRectangleSlice sl, retvals(2)
  END IF
  scriptret = create_plotslice_handle(sl)
 CASE 371 '--slice is rect
  IF valid_plotslice(retvals(0), "slice is rect") THEN
   scriptret = 0
   IF plotslices(retvals(0))->SliceType = slRectangle THEN scriptret = 1
  END IF
 CASE 372 '--set slice width
  IF valid_resizeable_slice(retvals(0), "set slice width") THEN
   plotslices(retvals(0))->Width = retvals(1)
  END IF
 CASE 373 '--set slice height
  IF valid_resizeable_slice(retvals(0), "set slice height") THEN
   plotslices(retvals(0))->Height = retvals(1)
  END IF
 CASE 374 '--get rect style
  IF valid_plotrect(retvals(0), "get rect style") THEN
   DIM dat AS RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->style
  END IF
 CASE 375 '--set rect style
  IF bound_arg(retvals(1), -1, 14, "set rect style", "style") THEN
   change_rect_plotslice retvals(0), retvals(1)
  END IF
 CASE 376 '--get rect fgcol
  IF valid_plotrect(retvals(0), "get rect fgcol") THEN
   DIM dat AS RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->fgcol
  END IF
 CASE 377 '--set rect fgcol
  IF bound_arg(retvals(1), 0, 255, "set rect fgcol", "fgcol") THEN
   change_rect_plotslice retvals(0), , ,retvals(1)
  END IF
 CASE 378 '--get rect bgcol
  IF valid_plotrect(retvals(0), "get rect bgcol") THEN
   DIM dat AS RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->bgcol
  END IF
 CASE 379 '--set rect bgcol
  IF bound_arg(retvals(1), 0, 255, "get rect bgcol", "bgcol") THEN
   change_rect_plotslice retvals(0), ,retvals(1)
  END IF
 CASE 380 '--get rect border
  IF valid_plotrect(retvals(0), "get rect border") THEN
   DIM dat AS RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->border
  END IF
 CASE 381 '--set rect border
  IF bound_arg(retvals(1), -1, 14, "get rect border", "border") THEN
   change_rect_plotslice retvals(0), , , ,retvals(1)
  END IF
 CASE 382 '--get rect trans
  IF valid_plotrect(retvals(0), "get rect trans") THEN
   DIM dat AS RectangleSliceData ptr
   dat = plotslices(retvals(0))->SliceData
   scriptret = dat->translucent
  END IF
 CASE 383 '--set rect trans
  change_rect_plotslice retvals(0), , , , ,retvals(1)
 CASE 384 '--slice collide point
  IF valid_plotslice(retvals(0), "slice collide point") THEN
   DIM sl AS Slice Ptr
   sl = plotslices(retvals(0))
   RefreshSliceScreenPos sl
   scriptret = ABS(SliceCollidePoint(sl, retvals(1), retvals(2)))
  END IF
 CASE 385 '--slice collide
  IF valid_plotslice(retvals(0), "slice collide") THEN
   IF valid_plotslice(retvals(1), "slice collide") THEN
    scriptret = ABS(SliceCollide(plotslices(retvals(0)), plotslices(retvals(1))))
   END IF
  END IF
 CASE 386 '--slice contains
  IF valid_plotslice(retvals(0), "slice contains") THEN
   IF valid_plotslice(retvals(1), "slice contains") THEN
    scriptret = ABS(SliceContains(plotslices(retvals(0)), plotslices(retvals(1))))
   END IF
  END IF
 CASE 387 '--clamp slice
  IF valid_plotslice(retvals(0), "clamp slice") THEN
   IF valid_plotslice(retvals(1), "clamp slice") THEN
    SliceClamp plotslices(retvals(1)), plotslices(retvals(0))
   END IF
  END IF
 CASE 388 '--horiz flip sprite
  IF valid_plotsprite(retvals(0), "horiz flip sprite") THEN
   DIM dat AS SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   change_sprite_plotslice retvals(0), dat->spritetype, dat->record, , , (retvals(1) <> 0)
  END IF
 CASE 389 '--vert flip sprite
  IF valid_plotsprite(retvals(0), "vert flip sprite") THEN
   DIM dat AS SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   change_sprite_plotslice retvals(0), dat->spritetype, dat->record, , , , (retvals(1) <> 0)
  END IF
 CASE 390 '--sprite is horiz flipped
  IF valid_plotsprite(retvals(0), "sprite is horiz flipped") THEN
   DIM dat AS SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat->flipHoriz THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 391 '--sprite is vert flipped
  IF valid_plotsprite(retvals(0), "sprite is vert flipped") THEN
   DIM dat AS SpriteSliceData Ptr
   dat = plotslices(retvals(0))->SliceData
   IF dat->flipVert THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 392 '--set top padding
  IF valid_plotslice(retvals(0), "set top padding") THEN
   plotslices(retvals(0))->PaddingTop = retvals(1)
  END IF
 CASE 393 '--get top padding
  IF valid_plotslice(retvals(0), "get top padding") THEN
   scriptret = plotslices(retvals(0))->PaddingTop
  END IF
 CASE 394 '--set left padding
  IF valid_plotslice(retvals(0), "set left padding") THEN
   plotslices(retvals(0))->PaddingLeft = retvals(1)
  END IF
 CASE 395 '--get left padding
  IF valid_plotslice(retvals(0), "get left padding") THEN
   scriptret = plotslices(retvals(0))->PaddingLeft
  END IF
 CASE 396 '--set bottom padding
  IF valid_plotslice(retvals(0), "set bottom padding") THEN
   plotslices(retvals(0))->PaddingBottom = retvals(1)
  END IF
 CASE 397 '--get bottom padding
  IF valid_plotslice(retvals(0), "get bottom padding") THEN
   scriptret = plotslices(retvals(0))->PaddingBottom
  END IF
 CASE 398 '--set right padding
  IF valid_plotslice(retvals(0), "set right padding") THEN
   plotslices(retvals(0))->PaddingRight = retvals(1)
  END IF
 CASE 399 '--get right padding
  IF valid_plotslice(retvals(0), "get right padding") THEN
   scriptret = plotslices(retvals(0))->PaddingRight
  END IF
 CASE 400 '--fill parent
  IF valid_resizeable_slice(retvals(0), "fill parent", YES) THEN
   plotslices(retvals(0))->Fill = (retvals(1) <> 0)
  END IF
 CASE 401 '--is filling parent
  IF valid_plotslice(retvals(0), "is filling parent") THEN
   IF plotslices(retvals(0))->Fill THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 402 '--slice to front
  IF valid_plotslice(retvals(0), "slice to front") THEN
   DIM sl AS Slice Ptr
   sl = plotslices(retvals(0))->Parent
   SetSliceParent plotslices(retvals(0)), sl
  END IF
 CASE 403 '--slice to back
  IF valid_plotslice(retvals(0), "slice to back") THEN
   DIM sl AS Slice Ptr
   sl = plotslices(retvals(0))
   IF sl->Parent = 0 THEN
    debug "slice to back: null parent"
   ELSE
    InsertSiblingSlice sl->Parent->FirstChild, sl
   END IF
  END IF
 CASE 404 '--last child
  IF valid_plotslice(retvals(0), "last child") THEN
   scriptret = find_plotslice_handle(LastChild(plotslices(retvals(0))))
  END IF
 CASE 405 '--y sort children
  IF valid_plotslice(retvals(0), "y sort children") THEN
   YSortChildSlices plotslices(retvals(0))
  END IF
 CASE 406 '--set sort order
  IF valid_plotslice(retvals(0), "set sort order") THEN
   plotslices(retvals(0))->Sorter = retvals(1)
  END IF
 CASE 407 '--sort children
  IF valid_plotslice(retvals(0), "sort children") THEN
   CustomSortChildSlices plotslices(retvals(0)), retvals(1)
  END IF
 CASE 408 '--previous sibling
  IF valid_plotslice(retvals(0), "previous sibling") THEN
   scriptret = find_plotslice_handle(plotslices(retvals(0))->PrevSibling)
  END IF 
 CASE 409 '--get sort order
  IF valid_plotslice(retvals(0), "get sort order") THEN
   scriptret = plotslices(retvals(0))->Sorter
  END IF
 CASE 410 '--get slice extra (handle, extra)
  IF valid_plotslice(retvals(0), "get slice extra") THEN
   IF retvals(1) >= 0 AND retvals(1) <= 2 THEN
    scriptret = plotslices(retvals(0))->Extra(retvals(1))
   END IF
  END IF
 CASE 411 '--set slice extra (handle, extra, val)
  IF valid_plotslice(retvals(0), "set slice extra") THEN
   IF retvals(1) >= 0 AND retvals(1) <= 2 THEN
    plotslices(retvals(0))->Extra(retvals(1)) = retvals(2)
   END IF
  END IF
 CASE 412 '--get sprite type
  IF valid_plotslice(retvals(0), "get sprite type") THEN
   IF plotslices(retvals(0))->SliceType = slSprite THEN
    DIM dat AS SpriteSliceData Ptr = plotslices(retvals(0))->SliceData
    scriptret = dat->spritetype
   ELSE
    scriptret = -1
   END IF
  END IF
 CASE 413 '--get sprite set number
  IF valid_plotsprite(retvals(0), "get sprite set number") THEN
   DIM dat AS SpriteSliceData Ptr = plotslices(retvals(0))->SliceData
   scriptret = dat->record
  END IF 
 CASE 414 '--get sprite palette
  IF valid_plotsprite(retvals(0), "get sprite palette") THEN
   DIM dat AS SpriteSliceData Ptr = plotslices(retvals(0))->SliceData
   scriptret = dat->pal
  END IF 

END SELECT

EXIT SUB

setwaitstate:
scrat(nowscript).waitarg = retvals(0)
scrat(nowscript).state = stwait
RETRACE

END SUB

SUB scriptnpc (id)

'contains npc related scripting commands

SELECT CASE AS CONST id

 CASE 26'--set NPC frame
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN npc(npcref).frame = bound(retvals(1), 0, 1) * 2
 CASE 39'--camera follows NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   gen(cameramode) = npccam
   gen(cameraArg) = npcref
  END IF
 CASE 45'--NPC x
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).x \ 20
 CASE 46'--NPC y
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).y \ 20
 CASE 52'--walk NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   SELECT CASE retvals(1)
    CASE 0'--north
     npc(npcref).dir = 0
     npc(npcref).ygo = retvals(2) * 20
    CASE 1'--east
     npc(npcref).dir = 1
     npc(npcref).xgo = retvals(2) * -20
    CASE 2'--south
     npc(npcref).dir = 2
     npc(npcref).ygo = retvals(2) * -20
    CASE 3'--west
     npc(npcref).dir = 3
     npc(npcref).xgo = retvals(2) * 20
   END SELECT
  END IF
 CASE 54'--set NPC direction
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN npc(npcref).dir = ABS(retvals(1)) MOD 4
 CASE 88'--set NPC position
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   cropposition retvals(1), retvals(2), 1
   npc(npcref).x = retvals(1) * 20
   npc(npcref).y = retvals(2) * 20
  END IF
 CASE 101'--NPC direction
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).dir
 CASE 117, 177'--NPC is walking
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   IF npc(npcref).xgo = 0 AND npc(npcref).ygo = 0 THEN
    scriptret = 0
   ELSE
    scriptret = 1
   END IF
   IF id = 117 THEN scriptret = scriptret XOR 1 'Backcompat hack
  END IF
 CASE 120'--NPC reference
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= max_npc_defs THEN
   found = 0
   FOR i = 0 TO 299
    IF npc(i).id - 1 = retvals(0) THEN
     IF found = retvals(1) THEN
      scriptret = (i + 1) * -1
      EXIT FOR
     END IF
     found = found + 1
    END IF
   NEXT i
  END IF
 CASE 121'--NPC at spot
  scriptret = 0
  found = 0
  FOR i = 0 TO 299
   IF npc(i).id > 0 THEN
    IF npc(i).x \ 20 = retvals(0) THEN 
     IF npc(i).y \ 20 = retvals(1) THEN
      IF found = retvals(2) THEN
       scriptret = (i + 1) * -1
       EXIT FOR
      END IF
      found = found + 1
     END IF
    END IF
   END IF
  NEXT i
  IF retvals(2) = -1 THEN scriptret = found
 CASE 122'--get NPC ID
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = ABS(npc(npcref).id) - 1
  ELSE
   scriptret = -1
  END IF
 CASE 123'--NPC copy count
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= max_npc_defs THEN
   FOR i = 0 TO 299
    IF npc(i).id - 1 = retvals(0) THEN
     scriptret = scriptret + 1
    END IF
   NEXT i
  END IF
 CASE 124'--change NPC ID
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 AND retvals(1) >= 0 AND retvals(1) <= max_npc_defs THEN npc(npcref).id = retvals(1) + 1
 CASE 125'--create NPC
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= max_npc_defs THEN
   FOR i = 299 TO 0 STEP -1
    IF npc(i).id = 0 THEN EXIT FOR
   NEXT
   'for backwards compatibility with games that max out the number of NPCs, try to overwrite tag-disabled NPCs
   'delete this bit once we raise the NPC limit
   IF i = -1 THEN
    FOR i = 299 TO 0 STEP -1
     IF npc(i).id <= 0 THEN EXIT FOR
    NEXT
    'I don't want to raise a scripterr here, again because it probably happens in routine in games like SoJ
    debug "create NPC: trying to create NPC id " & retvals(0) & " at " & retvals(1)*20 & "," & retvals(2)*20
    IF i = -1 THEN 
     debug "create NPC error: couldn't create NPC: too many NPCs exist"
    ELSE
     debug "create NPC warning: had to overwrite tag-disabled NPC id " & ABS(npc(i).id)-1 & " at " & npc(i).x & "," & npc(i).y & ": too many NPCs exist"
    END IF
   END IF
   IF i > -1 THEN
    npc(i).id = retvals(0) + 1
    cropposition retvals(1), retvals(2), 1
    npc(i).x = retvals(1) * 20
    npc(i).y = retvals(2) * 20
    npc(i).dir = ABS(retvals(3)) MOD 4
    npc(i).xgo = 0
    npc(i).ygo = 0
    scriptret = (i + 1) * -1
   END IF
  END IF
 CASE 126 '--destroy NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN npc(npcref).id = 0
 CASE 165'--NPC at pixel
  scriptret = 0
  found = 0
  FOR i = 0 TO 299
   IF npc(i).id > 0 THEN 
    IF npc(i).x <= retvals(0) AND npc(i).x > (retvals(0) - 20) THEN 
     IF npc(i).y <= retvals(1) AND npc(i).y > (retvals(1) - 20) THEN
      IF found = retvals(2) THEN
       scriptret = (i + 1) * -1
       EXIT FOR
      END IF
      found = found + 1
     END IF
    END IF
   END IF
  NEXT i
  IF retvals(2) = -1 THEN scriptret = found
 CASE 182'--read NPC
  IF retvals(1) >= 0 AND retvals(1) <= 14 THEN
   IF retvals(0) >= 0 AND retvals(0) <= max_npc_defs THEN
    scriptret = GetNPCD(npcs(retvals(0)), retvals(1))
   ELSE
    npcref = getnpcref(retvals(0), 0)
    IF npcref >= 0 THEN
     IF npc(npcref).id THEN scriptret = GetNPCD(npcs(ABS(npc(npcref).id) - 1), retvals(1))
    END IF
   END IF
  END IF
 CASE 192'--NPC frame
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).frame \ 2
 CASE 193'--NPC extra
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   IF retvals(1) >= 0 AND retvals(1) <= 2 THEN
    scriptret = npc(npcref).extra(retvals(1))
   END IF
  END IF
 CASE 194'--set NPC extra
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   IF retvals(1) >= 0 AND retvals(1) <= 2 THEN
    npc(npcref).extra(retvals(1)) = retvals(2)
   END IF
  END IF
'  CASE ???'--get NPC raw
'   npcref = getnpcref(retvals(0), 0)
'   s = bound(retvals(1),0,8)
'   'scriptret = npc(npcref).rawdat(s)
'   scriptret = CPtr(integer ptr,@npc(npcref))[s]
'  CASE ???'--set NPC raw
'   npcref = getnpcref(retvals(0), 0)
'   s = bound(retvals(1),0,8)
'   CPtr(integer ptr,@npc(npcref))[s] = retvals(2)
END SELECT

END SUB

SUB breakpoint (mode, callspot)
' callspot = 1  stnext
' callspot = 2  stdone
' callspot = 3  stread  - (skipping to end of a command?)
' callspot = 4  at top of main loop, after loading onkeypress

IF stepmode = 0 THEN GOTO breakin
IF nowscript = -1 AND stepmode <> stepscript THEN
 stepmode = 0
 waitforscript = 999
 waitfordepth = 999
 EXIT SUB
END IF

'STEPPING LOGIC
'some generic logic for going up scripts/commands
IF waitforscript <> 999 THEN
 IF nowscript > waitforscript THEN 
  EXIT SUB
 ELSEIF nowscript < waitforscript THEN 
  waitforscript = 999
  waitfordepth = 999
 ELSE
  'if final objective is a script, not a depth, stop
  IF waitfordepth = 999 THEN waitforscript = 999
 END IF

 IF scrat(nowscript).depth > waitfordepth THEN
  EXIT SUB
 ELSE
  waitforscript = 999
  waitfordepth = 999
 END IF
END IF

SELECT CASE stepmode
 CASE stependscript
  GOTO breakin
 CASE stepscript
'  IF callspot = 1 THEN IF curcmd->kind = tyscript THEN GOTO breakin
'  IF callspot = 2 THEN GOTO breakin
  IF lastscriptnum <> nowscript THEN GOTO breakin
 CASE stepargsdone, stepup, stepnext
  IF callspot = 1 THEN
   IF scrat(nowscript).curargn < curcmd->argc OR scrat(nowscript).curargn = 0 THEN EXIT SUB
   IF curcmd->kind = tyflow AND curcmd->value = flowif THEN EXIT SUB
  END IF
  GOTO breakin
END SELECT

'IF callspot = 1 THEN 'stnext
'' IF (mode AND 4) AND curcmd->kind <> tyscript THEN EXIT SUB
' 'only used to print off evaluated list of arguments
'' IF ((mode AND breakreadcmd) <> 0) AND (scrat(nowscript).curargn < curcmd->argc OR scrat(nowscript).curargn = 0) THEN EXIT SUB
' IF scrat(nowscript).curargn < curcmd->argc OR scrat(nowscript).curargn = 0 THEN EXIT SUB
'' IF (mode AND breakargsdone) THEN EXIT SUB
' IF curcmd->kind = tyflow AND curcmd->value = flowif THEN EXIT SUB
'END IF

'END STEPPING LOGIC

EXIT SUB

breakin:

'clear breakpoint bits
mode = mode AND 3
stepmode = 0
scriptwatcher mode, 0

END SUB

SUB scriptwatcher (mode, drawloop)
STATIC localsscroll, globalsscroll
STATIC selectedscript, bottom, viewmode, lastscript
'viewmode: 0 = script state, 1 = local variables, 2 = global variables

IF nowscript >= 0 THEN
 WITH scrat(nowscript)
  .curkind = curcmd->kind
  .curvalue = curcmd->value
  .curargc = curcmd->argc
 END WITH
END IF

'debug "watch mode=" & mode & " callspot = " & callspot & " stepmode = " & stepmode _
'      & " curscript = " & nowscript & " curdepth = " & scrat(nowscript).depth & " waitscr = " & waitforscript & " waitdepth = " & waitfordepth

'initialise state
IF mode = 1 THEN waitforscript = 999: waitfordepth = 999: stepmode = 0


redraw:
'if in stepping mode, make a copy so debug info can be redrawn, need to keep dpage clean 
'if called from displayall, need to keep a clean copy of nearly-drawn page to be used next tick 
IF drawloop AND mode = 1 THEN
 page = dpage
ELSE
 copypage dpage, vpage
 page = vpage
END IF

'edgeprint callmode & " " & viewmode & " " & callspot, 140, 4, uilook(uiText), page

selectedscript = bound(selectedscript, 0, nowscript)
IF selectedscript = lastscript THEN selectedscript = nowscript
lastscript = nowscript

IF nowscript >= 0 THEN 
 SELECT CASE scrat(nowscript).curkind
  CASE tynumber, tyglobal, tylocal
   hasargs = 0
  CASE ELSE
   hasargs = 1
 END SELECT
END IF

'Note: the colours here are fairly arbitrary
rectangle 0, 0, 320, 4, uilook(uiBackground), page
rectangle 0, 0, (320 / scriptmemMax) * totalscrmem, 2, uilook(uiSelectedItem), page
rectangle 0, 2, (320 / 2048) * scrat(nowscript + 1).heap, 2, uilook(uiSelectedItem + 1), page

ol = 191

IF mode > 1 AND viewmode = 0 THEN
 IF nowscript = -1 THEN
  edgeprint "Advanced debug mode: no scripts", 0, ol, uilook(uiDescription), page
  ol -= 9 
 ELSE
  decmpl$ = scriptstate$(selectedscript)
  FOR i = 5 TO 0 STEP -1
   IF LEN(decmpl$) > i * 40 THEN
    edgeprint MID$(decmpl$, i * 40 + 1), 0, ol, uilook(uiDescription), page
    ol -= 9
   END IF
  NEXT
 ' edgeprint "Last return value: " & scriptret, 0, ol, uilook(uiDescription), page
 ' ol -= 9
 END IF
END IF

IF mode > 1 AND viewmode = 1 THEN
 reloadscript scrat(selectedscript)
 WITH scrat(selectedscript)
  IF script(.scrnum).vars = 0 THEN
   edgeprint "Has no variables", 0, ol, uilook(uiText), page
   ol -= 9
  ELSE
   scriptargs = script(.scrnum).args
   FOR i = small((script(.scrnum).vars - localsscroll - 1) \ 3, 3) TO 0 STEP -1
    FOR j = 0 TO 2
     localno = localsscroll + i * 3 + j
     IF localno < script(.scrnum).vars THEN
      temp$ = localvariablename$(localno, scriptargs) & "="
      edgeprint temp$, j * 96, ol, uilook(uiText), page
      edgeprint STR$(heap(.heap + localno)), j * 96 + 8 * LEN(temp$), ol, uilook(uiDescription), page
     END IF
    NEXT
    ol -= 9
   NEXT
   IF scriptargs = 999 THEN
    edgeprint script(.scrnum).vars & " local variables and arguments:", 0, ol, uilook(uiText), page
   ELSE
    edgeprint (script(.scrnum).vars - scriptargs) & " local variables and " & scriptargs & " arguments:", 0, ol, uilook(uiText), page
   END IF
   ol -= 9
  END IF
  edgeprint "Return value = " & .ret, 0, ol, uilook(uiText), page
  ol -= 9
 END WITH
END IF

IF mode > 1 AND viewmode = 2 THEN
 FOR i = 4 TO 0 STEP -1
  FOR j = 0 TO 3
   globalno = globalsscroll + i * 4 + j
   edgeprint globalno & "=", j * 72, ol, uilook(uiText), page
   edgeprint STR$(global(globalno)), j * 72 + 8 * LEN(globalno & "="), ol, uilook(uiDescription), page
  NEXT
  ol -= 9
 NEXT
 edgeprint "Global variables:", 0, ol, uilook(uiText), page
 ol -= 9
END IF

'IF mode > 1 THEN
' edgeprint "argc=" & scrat(selectedscript).curargc & " argn=" & scrat(selectedscript).curargn & " ptr=" & scrat(selectedscript).ptr, 0, ol, uilook(uiDescription), page
' ol -= 9
'END IF

edgeprint "# Name           Depth State CmdKn CmdID", 0, ol, uilook(uiText), page
ol -= 9

IF mode = 1 THEN
 bottom = nowscript - (ol - 6) \ 9
 selectedscript = nowscript
ELSE
 bottom = small(bottom, selectedscript)
 bottom = large(bottom, selectedscript - (ol - 6) \ 9)
END IF

FOR i = large(bottom, 0) TO nowscript
 'if script is about to be executed, don't show it as having been already
 IF scrat(i).curargn >= scrat(i).curargc AND i <> nowscript THEN lastarg = -1 ELSE lastarg = 0

 IF mode > 1 AND i = selectedscript THEN col = uilook(uiSelectedItem) ELSE col = uilook(uiText)
 edgeprint STR$(i), 0, ol, col, page
 edgeprint LEFT$(scriptname$(scrat(i).id), 14), 16, ol, col, page
 edgeprint STR$(scrat(i).depth), 136, ol, col, page
 IF scrat(i).state < 0 THEN
  edgeprint "Suspended", 184, ol, col, page
 ELSEIF scrat(i).state = stwait THEN
  SELECT CASE scrat(i).curvalue
   CASE 1'--wait number of ticks
    waitcause$ = "wait(" & scrat(i).waitarg & ")"
   CASE 2'--wait for all
    waitcause$ = "waitforall"
   CASE 3'--wait for hero
    waitcause$ = "waitforhero(" & scrat(i).waitarg & ")"
   CASE 4'--wait for NPC
    waitcause$ = "waitfornpc(" & scrat(i).waitarg & ")"
   CASE 9'--wait for key
    waitcause$ = "waitforkey(" & scrat(i).waitarg & ")"
   CASE 244'--wait for scancode
    waitcause$ = "waitscancode(" & scrat(i).waitarg & ")"
   CASE 42'--wait for camera
    waitcause$ = "waitforcamera"
   CASE 59'--wait for text box
    waitcause$ = "waitfortextbox"
   CASE ELSE
    waitcause$ = "Cmd " & scrat(i).curvalue & " caused wait"
  END SELECT
  edgeprint waitcause$, 184, ol, col, page
 ELSEIF scrat(i).state = stnext AND scrat(i).curkind = tyscript AND lastarg THEN
  edgeprint "Called #" & i + 1, 184, ol, col, page
 ELSEIF scrat(i).state = stnext AND scrat(i).curkind = tyfunct AND scrat(i).curvalue = 176 AND lastarg THEN
  edgeprint "Called #" & i + 1 & " by ID", 184, ol, col, page
 ELSE
  edgeprint STR$(scrat(i).state), 184, ol, col, page
  edgeprint STR$(scrat(i).curkind), 232, ol, col, page
  edgeprint STR$(scrat(i).curvalue), 280, ol, col, page
 END IF
 ol = ol - 9
 IF ol < 6 THEN EXIT FOR
NEXT i


IF mode > 1 AND drawloop = 0 THEN
 setpal master()
 setvispage page
 w = getkey
 IF w = 68 THEN mode = 0: clearkey(68) 'f10
 IF w = 47 THEN viewmode = loopvar(viewmode, 0, 2, 1): GOTO redraw 'v
 IF w = 73 THEN 'pgup
  selectedscript += 1
  localsscroll = 0
  GOTO redraw
 END IF
 IF w = 81 THEN 'pgdw
  selectedscript -= 1
  localsscroll = 0
  GOTO redraw
 END IF
 IF w = 12 OR w = 74 THEN '-
  IF viewmode = 1 THEN localsscroll = large(0, localsscroll - 4): GOTO redraw
  IF viewmode = 2 THEN globalsscroll = large(0, globalsscroll - 12): GOTO redraw
 END IF
 IF w = 13 OR w = 78 THEN '+
  IF viewmode = 1 THEN localsscroll = small(large(script(scrat(selectedscript).scrnum).vars - 11, 0), localsscroll + 4): GOTO redraw
  IF viewmode = 2 THEN globalsscroll = small(4076, globalsscroll + 12): GOTO redraw
 END IF

 'stepping
 IF w = 49 THEN 'n
  'step till next script
  mode or= breakstnext OR breakstdone OR breaklooptop
  stepmode = stepscript
  lastscriptnum = nowscript
 END IF
 IF w = 22 THEN 'u
  mode or= breakstnext
  stepmode = stepup
  waitfordepth = scrat(selectedscript).depth - 1
  waitforscript = nowscript
 END IF
 IF w = 17 THEN 'w
  mode or= breakstnext
  waitforscript = selectedscript
  stepmode = stependscript
 END IF
 IF w = 31 THEN 's
  mode or= breakstread OR breakstnext OR breakloopbrch
  stepmode = stepnext
 END IF
 IF w = 33 THEN 'f
  'mode or= breakstread
  mode or= breakstnext OR breakloopbrch
  stepmode = stepargsdone
  waitforscript = nowscript
  waitfordepth = scrat(selectedscript).depth
  'it would be more useful to wait for the calling command to finish
  IF hasargs = 0 THEN waitfordepth -= 1
 END IF
END IF

IF drawloop AND mode > 1 THEN
 'displayall: dpage was copied to vpage
 SWAP dpage, vpage
END IF

'just incase was swapped out above
IF nowscript >= 0 THEN
 reloadscript scrat(nowscript)
END IF
END SUB

SUB setdebugpan

gen(cameramode) = pancam
gen(cameraArg2) = 1
gen(cameraArg3) = 5

END SUB

SUB subreturn (si AS ScriptInst)
si.depth -= 1
IF si.depth < 0 THEN
 si.state = stdone
ELSE
 pops(scrst, si.curargn)
 pops(scrst, si.ptr)
 curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)
 '--push return value
 pushs(scrst, scriptret)
 si.curargn += 1
 si.state = stnext'---try next arg
 IF si.curargn >= curcmd->argc THEN EXIT SUB
 IF curcmd->kind = tyflow THEN IF curcmd->value = flowif OR curcmd->value >= flowfor THEN EXIT SUB
 IF curcmd->kind = tymath THEN IF curcmd->value >= 20 THEN EXIT SUB
 si.state = stdoarg
END IF
END SUB

SUB unwindtodo (si AS ScriptInst, levels)
'unwinds the stack until the specified number of dos have been stripped
'leaves the interpreter as if the last do block had successfully finished
'this means repeat in the case of for and while loops
'note: we assume the calling command has popped its args

WHILE levels > 0
 si.depth -= 1
 IF si.depth < 0 THEN
  si.state = stdone
  EXIT SUB
 END IF

 pops(scrst, si.curargn)
 pops(scrst, si.ptr)
 curcmd = cast(ScriptCommand ptr, si.scrdata + si.ptr)

 IF curcmd->kind = tyflow AND curcmd->value = flowdo THEN
  levels -= 1
  'first pop do's evaluated arguments before stopping
 END IF

 'pop arguments
 IF curcmd->kind = tyflow AND curcmd->value = flowswitch THEN
  'unlike all other flow, switch stack usage != argn
  scrst.pos -= 2 'state, matching value
 ELSE
  scrst.pos -= si.curargn
 END IF
WEND
'return to normality
subreturn si

END SUB

SUB templockexplain
PRINT "Either " + exename + " is already running in the background, or it"
PRINT "terminated incorrectly last time it was run, and was unable to clean up"
PRINT "its temporary files. The operating system is denying access to the"
PRINT "files in " + workingdir
PRINT
PRINT "If this problem persists, manually delete playing.tmp"
PRINT
PRINT "Error code"; ERR
END SUB

SUB tweakpalette
FOR i = bound(retvals(3), 0, 255) TO bound(retvals(4), 0, 255)
 master(i).r = bound(master(i).r + retvals(0) * 4, 0, 255)
 master(i).g = bound(master(i).g + retvals(1) * 4, 0, 255)
 master(i).b = bound(master(i).b + retvals(2) * 4, 0, 255)
NEXT i
END SUB

FUNCTION vehiclestuff (disx as integer, disy as integer, vehedge as integer) as integer
STATIC aheadx, aheady

result = 0
IF readbit(veh(), 6, 0) THEN '--scramble-----------------------
 '--part of the vehicle automount where heros scramble--
 IF npc(veh(5)).xgo = 0 AND npc(veh(5)).ygo = 0 THEN
  '--npc must stop before we mount
  targx = npc(veh(5)).x
  targy = npc(veh(5)).y
  untrigbit = 0
  GOSUB vehscramble
 END IF
END IF'--scramble mount
IF readbit(veh(), 6, 1) THEN '--rise----------------------
 tmp = 0
 FOR i = 0 TO 3
  IF catz(i * 5) < veh(21) THEN
   catz(i * 5) = catz(i * 5) + large(1, small(4, (veh(21) - catz(i * 5)) / 2))
  ELSE
   tmp = tmp + 1
  END IF
 NEXT i
 IF tmp = 4 THEN
  setbit veh(), 6, 1, 0
 END IF
END IF
IF readbit(veh(), 6, 2) THEN '--fall-------------------
 tmp = 0
 FOR i = 0 TO 3
  IF catz(i * 5) > 0 THEN
   catz(i * 5) = catz(i * 5) - large(1, small(4, (veh(21) - catz(i * 5)) / 2))
  ELSE
   tmp = tmp + 1
  END IF
 NEXT i
 IF tmp = 4 THEN
  FOR i = 0 TO 3
   catz(i * 5) = 0
  NEXT i
  setbit veh(), 6, 2, 0
  setbit veh(), 6, 3, 1 '--dismount
 END IF
END IF
IF readbit(veh(), 6, 3) THEN '--dismount---------------
 setbit veh(), 6, 3, 0
 IF vehpass(veh(20), readmapblock(disx, disy, 0), -1) THEN
  '--dismount point is landable
  FOR i = 0 TO 15
   catx(i) = catx(0)
   caty(i) = caty(0)
   catd(i) = catd(0)
   catz(i) = 0
  NEXT i
  IF readbit(veh(), 9, 6) = 1 THEN ' Dismount-ahead is true
   setbit veh(), 6, 5, 1'--ahead
   aheadx = disx * 20
   aheady = disy * 20
  ELSE
   setbit veh(), 6, 4, 1'--clear vehicle
  END IF
 ELSE
  '--dismount point is unlandable
  IF veh(21) THEN
   setbit veh(), 6, 1, 1'--riseagain
  END IF
 END IF
END IF
IF readbit(veh(), 6, 4) THEN '--clear
 IF veh(16) < 0 THEN result = veh(16)
 IF veh(16) > 0 THEN result = 1 + veh(16)
 IF veh(14) > 1 THEN setbit tag(), 0, veh(14), 0
 IF readbit(veh(), 9, 6) AND readbit(veh(), 9, 7) = 0 THEN
  '--dismount-ahead is true, dismount-passwalls is false
  SELECT CASE catd(0)
   CASE 0
    ygo(0) = 20
   CASE 1
    xgo(0) = -20
   CASE 2
    ygo(0) = -20
   CASE 3
    xgo(0) = 20
  END SELECT
 END IF
 herospeed(0) = veh(7)
 IF herospeed(0) = 3 THEN herospeed(0) = 10
 npc(veh(5)).xgo = 0
 npc(veh(5)).ygo = 0
 '--clear vehicle
 FOR i = 0 TO 21
  veh(i) = 0
 NEXT i
 FOR i = 0 TO 15
  catx(i) = catx(0)
  caty(i) = caty(0)
  catd(i) = catd(0)
  catz(i) = 0
 NEXT i
 gam.random_battle_countdown = range(100, 60)
END IF
IF readbit(veh(), 6, 5) THEN '--ahead
 targx = aheadx
 targy = aheady
 untrigbit = 5
 GOSUB vehscramble
END IF
IF veh(6) = 0 THEN
 IF txt.showing = NO AND readbit(gen(), 44, suspendplayer) = 0 THEN
  FOR i = 0 TO 1
   IF carray(4 + i) > 1 AND xgo(0) = 0 AND ygo(0) = 0 THEN
    SELECT CASE veh(12 + i)
     CASE -2
      '-disabled
     CASE -1
      result = 1
     CASE 0
      '--dismount
      xgo(0) = 0: ygo(0) = 0
      IF veh(21) THEN
       setbit veh(), 6, 2, 1
      ELSE
       setbit veh(), 6, 3, 1
      END IF
     CASE IS > 0
      result = veh(12 + i) * -1
    END SELECT
   END IF
  NEXT i
 END IF
END IF'--normal

RETURN result

EXIT FUNCTION

vehscramble:
catsize = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN catsize += 1
NEXT
tmp = 0
FOR i = 0 TO 3
 IF i >= catsize THEN
  tmp += 1
 ELSE
  scramx = catx(i * 5)
  scramy = caty(i * 5)
  IF ABS(scramx - targx) < large(herospeed(i), 4) THEN
   scramx = targx
   xgo(i) = 0
   ygo(i) = 0
  END IF
  IF ABS(scramy - targy) < large(herospeed(i), 4) THEN
   scramy = targy
   xgo(i) = 0
   ygo(i) = 0
  END IF
  IF ABS(targx - scramx) > 0 AND xgo(i) = 0 THEN
   xgo(i) = 20 * SGN(scramx - targx)
  END IF
  IF ABS(targy - scramy) > 0 AND ygo(i) = 0 THEN
   ygo(i) = 20 * SGN(scramy - targy)
  END IF
  IF vehedge THEN xgo(i) = xgo(i) * -1 : ygo(i) = ygo(i) * -1
  IF scramx - targx = 0 AND scramy - targy = 0 THEN tmp = tmp + 1
  catx(i * 5) = scramx
  caty(i * 5) = scramy
 END IF
NEXT i
IF tmp = 4 THEN
 setbit veh(), 6, untrigbit, 0
 IF veh(15) < 0 THEN result = veh(15)
 IF veh(15) > 0 THEN result = 1 + veh(15)
 herospeed(0) = veh(8)
 IF herospeed(0) = 3 THEN herospeed(0) = 10
 '--null out hero's movement
 FOR i = 0 TO 3
  xgo(i) = 0: ygo(i) = 0
 NEXT i
 '--transfer any residual NPC movement
 'xgo(0) = npcl(veh(5) + 1500)
 'ygo(0) = npcl(veh(5) + 1800)
 'npcl(veh(5) + 1500) = 0
 'npcl(veh(5) + 1800) = 0
 '--!!!
 IF untrigbit = 5 THEN setbit veh(), 6, 4, 1'--clear
 IF veh(21) THEN setbit veh(), 6, 1, 1'--rise
END IF
RETRACE

END FUNCTION

FUNCTION vehpass (n as integer, tile as integer, default as integer) as integer

'--true means passable
'--false means impassable

v = default

SELECT CASE n
 CASE 1
  v = (tile AND 16)
 CASE 2
  v = (tile AND 32)
 CASE 3
  v = ((tile AND 16) = 16) AND ((tile AND 32) = 32)
 CASE 4
  v = ((tile AND 16) = 16) OR ((tile AND 32) = 32)
 CASE 5
  v = NOT ((tile AND 16) = 16)
 CASE 6
  v = NOT ((tile AND 32) = 32)
 CASE 7
  v = NOT (((tile AND 16) = 16) OR ((tile AND 32) = 32))
 CASE 8
  v = -1
END SELECT

v = ABS(SGN(v)) * -1

vehpass = v

'tiles
'1   north
'2   east
'4   south
'8   west
'16  vehicle A
'32  vehicle B
'64  harm tile
'128 overhead

END FUNCTION

SUB vishero (stat())
'FIXME: this needs to be rewritten to use Frame objects rather than a screen page
o = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  getpal16 pal16(), o, stat(i, 1, 15), 4, stat(i, 1, 14)
  setpicstuf buffer(), 1600, 2
  loadset game + ".pt4", stat(i, 1, 14), 5 * o
  o = o + 1
 END IF
NEXT i
FOR i = o TO 3
 '--black out unused heros
 rectangle 0, i * 5, 320, 5, 0, 2
NEXT i
END SUB

SUB wrapaheadxy (x, y, direction, distance, unitsize)
'alters X and Y ahead by distance in direction, wrapping if neccisary
'unitsize is 20 for pixels, 1 for tiles

aheadxy x, y, direction, distance

IF gmap(5) = 1 THEN
 wrapxy x, y, scroll(0) * unitsize, scroll(1) * unitsize
END IF

END SUB

SUB cropposition (BYREF x, BYREF y, unitsize)

IF gmap(5) = 1 THEN
 wrapxy x, y, scroll(0) * unitsize, scroll(1) * unitsize
ELSE
 x = bound(x, 0, (scroll(0) - 1) * unitsize)
 y = bound(y, 0, (scroll(1) - 1) * unitsize)
END IF

END SUB

FUNCTION wrappass (x as integer, y as integer, xgo as integer, ygo as integer, isveh as integer) as integer
' returns true if blocked by terrain
DIM pd(3)

wrappass = 0

tilex = x: tiley = y
p = readpassblock(tilex, tiley)

FOR i = 0 TO 3
 tilex = x: tiley = y
 wrapaheadxy tilex, tiley, i, 1, 1
 IF tilex < 0 OR tilex >= scroll(0) OR tiley < 0 OR tiley >= scroll(1) THEN
  pd(i) = 15
 ELSE
  pd(i) = readpassblock(tilex, tiley)
 END IF
NEXT i

IF ygo > 0 AND movdivis(ygo) AND ((p AND 1) = 1 OR (pd(0) AND 4) = 4 OR (isveh AND vehpass(veh(18), pd(0), 0))) THEN ygo = 0: wrappass = 1
IF ygo < 0 AND movdivis(ygo) AND ((p AND 4) = 4 OR (pd(2) AND 1) = 1 OR (isveh AND vehpass(veh(18), pd(2), 0))) THEN ygo = 0: wrappass = 1
IF xgo > 0 AND movdivis(xgo) AND ((p AND 8) = 8 OR (pd(3) AND 2) = 2 OR (isveh AND vehpass(veh(18), pd(3), 0))) THEN xgo = 0: wrappass = 1
IF xgo < 0 AND movdivis(xgo) AND ((p AND 2) = 2 OR (pd(1) AND 8) = 8 OR (isveh AND vehpass(veh(18), pd(1), 0))) THEN xgo = 0: wrappass = 1

END FUNCTION


FUNCTION wrapcollision (xa as integer, ya as integer, xgoa as integer, ygoa as integer, xb as integer, yb as integer, xgob as integer, ygob as integer) as integer
 x1 = (xa - bound(xgoa, -20, 20)) \ 20
 x2 = (xb - bound(xgob, -20, 20)) \ 20
 y1 = (ya - bound(ygoa, -20, 20)) \ 20
 y2 = (yb - bound(ygob, -20, 20)) \ 20

 IF gmap(5) = 1 THEN
  wrapcollision = (x1 - x2) MOD scroll(0) = 0 AND (y1 - y2) MOD scroll(1) = 0
 ELSE
  wrapcollision = (x1 = x2) AND (y1 = y2)
 END IF

END FUNCTION

FUNCTION wraptouch (x1 as integer, y1 as integer, x2 as integer, y2 as integer, distance as integer) as integer
 'whether 2 walkabouts are within distance pixels horizontally + vertically
 wraptouch = 0
 IF gmap(5) = 1 THEN
  IF ABS((x1 - x2) MOD (scroll(0) * 20 - distance)) <= distance AND ABS((y1 - y2) MOD (scroll(1) * 20 - distance)) <= distance THEN wraptouch = 1
 ELSE
  IF ABS(x1 - x2) <= 20 AND ABS(y1 - y2) <= 20 THEN wraptouch = 1
 END IF
END FUNCTION

SUB wrappedsong (songnumber)

IF songnumber <> presentsong THEN
 playsongnum songnumber
 presentsong = songnumber
ELSE
 resumesong
END IF

END SUB

SUB stopsong
presentsong = -1
pausesong 'this is how you stop the music
END SUB

SUB wrapxy (x, y, wide, high)
'--wraps the given X and Y values within the bounds of width and height
x = ((x MOD wide) + wide) MOD wide  'negative modulo is the devil's creation and never helped me once
y = ((y MOD high) + high) MOD high
END SUB

SUB readstackcommand (state as ScriptInst, stk as Stack, i)
 state.curargn = reads(stk, i)
 state.ptr = reads(stk, i - 1)
 DIM cmdptr as ScriptCommand ptr = cast(ScriptCommand ptr, state.scrdata + state.ptr)
 state.curkind = cmdptr->kind
 state.curvalue = cmdptr->value
 state.curargc = cmdptr->argc
 i -= 2
END SUB

FUNCTION localvariablename (value as integer, scriptargs as integer) as string
 'get a variable name from a local variable number
 'locals (and args) numbered from 0
 IF scriptargs = 999 THEN
  'old HS file
  RETURN "local" & value
 ELSEIF value < scriptargs THEN
  RETURN "arg" & value
 ELSE
  RETURN "var" & (value - scriptargs)
 END IF
END FUNCTION

FUNCTION mathvariablename (value as integer, scriptargs as integer) as string
 'get a variable name from an variable id number passed to a math function or for
 'locals (and args) numbered from 0
 IF value >= 0 THEN
  mathvariablename$ = "global" & value
 ELSEIF scriptargs = 999 THEN
  'old HS file
  mathvariablename$ = "local" & (-value - 1)
 ELSEIF -value <= scriptargs THEN
  mathvariablename$ = "arg" & (-value - 1)
 ELSE
  mathvariablename$ = "var" & (-value - scriptargs - 1)
 END IF
END FUNCTION

FUNCTION scriptstate (targetscript as integer) as string
 IF nowscript <= -1 THEN EXIT FUNCTION

 IF targetscript = -1 OR targetscript = nowscript THEN
  recurse = 2
 ELSE
  recurse = 3
 END IF
 'recurse 0 = only top script
 'recurse 1 = top script plus calling scripts
 'recurse 2 = all scripts, including suspended ones
 'recurse 3 = only the specified script

 DIM flowname$(15), flowtype(15), state as ScriptInst, laststate as ScriptInst

 flowtype(0) = 0:	flowname$(0) = "do"
 flowtype(3) = 1:	flowname$(3) = "return"
 flowtype(4) = 3:	flowname$(4) = "if"
 flowtype(5) = 0:	flowname$(5) = "then"
 flowtype(6) = 0:	flowname$(6) = "else"
 flowtype(7) = 2:	flowname$(7) = "for"
 flowtype(10) = 2:	flowname$(10) = "while"
 flowtype(11) = 1:	flowname$(11) = "break"
 flowtype(12) = 1:	flowname$(12) = "continue"
 flowtype(13) = 1:	flowname$(13) = "exit"
 flowtype(14) = 1:	flowname$(14) = "exitreturn"
 flowtype(15) = 3:	flowname$(15) = "switch"

 DIM mathname$(22) = {_
         "random", "exponent", "mod", "divide", "multiply", "subtract"_
         ,"add", "xor", "or", "and", "equal", "!equal", "<<", ">>"_
         ,"<=", ">=", "setvar", "inc", "dec", "not", "&&", "||", "^^"_
 }

 stkbottom = -(scrst.pos - scrst.bottom)  'pointer arithmetic seems to be 31-bit signed (breakage on negative diff)!
 stkpos = 0

 wasscript = nowscript

 'macro disabled for fb 0.15 compat
 'copyobj(state, scrat(wasscript))
 memcpy(@(state),@(scrat(wasscript)),LEN(scrat(wasscript)))
 state.curkind = curcmd->kind
 state.curvalue = curcmd->value
 state.curargc = curcmd->argc
 memcpy(@(laststate),@(state),LEN(state))

 'so we can grab extra data on the current script
 reloadscript scrat(nowscript), 0

 'debug "state = " & state.state
 'debug "depth = " & state.depth
 'debug "kind = " & state.curkind
 'debug "val = " & state.curvalue
 'debug "argn = " & state.curargn
 'debug "argc = " & state.curargc
 'FOR i = stkbottom TO -1
 ' dstr$ = dstr$ + xstr$(readstackdw(i))
 'NEXT
 'debug "stack contents = " + dstr$


 IF state.state = stdoarg THEN GOTO jmpdoarg
 IF state.state = stnext THEN
  IF recurse <> 3 THEN

   IF state.curkind = tyflow AND state.curvalue = flowfor AND state.curargn = 4 THEN
    'print variable, start, end, and step of a for loop
    outstr$ = "(" & mathvariablename$(reads(scrst, stkpos - 3), script(state.scrnum).args)
    outstr$ += "," & reads(scrst, stkpos - 2)
    outstr$ += "," & reads(scrst, stkpos - 1)
    outstr$ += "," & reads(scrst, stkpos) & ")"
   ELSEIF state.curargn >= state.curargc AND iif(state.curkind <> tyflow, 1, flowtype(state.curvalue) = 1) THEN
    'print the evaluated list of arguments from the stack if they are all done
    outstr$ = "("
    IF state.curkind = tymath AND state.curvalue >= 16 AND state.curvalue <= 18 THEN
     outstr$ += mathvariablename$(reads(scrst, stkpos - 1), script(state.scrnum).args)
     outstr$ += "," & reads(scrst, stkpos)
    ELSE
     FOR i = state.curargn - 1 TO 0 STEP -1
      outstr$ += STR$(reads(scrst, stkpos - i))
      IF i <> 0 THEN outstr$ += ","
     NEXT
    END IF
    outstr$ += ")"
   END IF

  END IF
  IF state.curargn = 0 THEN hideoverride = -1
  GOTO jmpnext
 END IF

 DO
  jmpread:
  jmpreturn:
  jmpwait:

  cmd$ = ""
  hidearg = 0
  IF hideoverride THEN hidearg = -1: hideoverride = 0
  SELECT CASE state.curkind
    CASE tynumber
     outstr$ = STR$(state.curvalue)
    CASE tyflow
     cmd$ = flowname$(state.curvalue)
     IF state.depth = 0 THEN cmd$ = scriptname$(state.id)
     IF flowtype(state.curvalue) = 0 THEN IF state.curargc = 0 THEN hidearg = -1: cmd$ += "()"
     IF flowtype(state.curvalue) = 1 THEN hidearg = -1: IF state.curargn = 0 THEN cmd$ += ":"
     IF flowtype(state.curvalue) = 2 THEN
      IF state.curargn = state.curargc - 1 THEN hidearg = -1: cmd$ += "()"
      IF state.curvalue = flowwhile AND state.curargn = 0 THEN hidearg = -1
     END IF
     IF state.curvalue = flowif THEN
      hidearg = -1
      IF state.curargn > 0 AND state.curargn < state.curargc THEN cmd$ += "()"
     END IF
     IF state.curvalue = flowswitch THEN
      hidearg = -1
      IF state.curargn = 0 THEN
       cmd$ += ":"
      ELSE
       cmd$ += "(" & reads(scrst, stkpos + 1) & ")"   ' ????
       IF state.curargn + 1 = state.curargc THEN
        cmd$ += " else"
        'hack to replace the 'do' with 'else' (hspeak outputs a do instead of an else)
        IF LEN(outstr$) > 1 THEN outstr$ = MID$(outstr$, 3)
        hidearg = -2
       ELSEIF state.curargn >= state.curargc THEN
        'an extra step the stepper currently pauses on
       ELSEIF laststate.curkind = tyflow AND laststate.curvalue = flowdo THEN
        cmd$ += " case()"
       ELSE
        cmd$ += " case"
        IF state.curargn < state.curargc THEN cmd$ += ":" ELSE cmd$ += "()"
       END IF
      END IF
     END IF
    CASE tyglobal
     outstr$ = "global" + STR$(state.curvalue)
    CASE tylocal
     'locals can only appear in the topmost script, which we made sure is loaded
     outstr$ = localvariablename$(state.curvalue, script(state.scrnum).args)
    CASE tymath
     cmd$ = mathname$(state.curvalue)
    CASE tyfunct
     cmd$ = "cmd" + STR$(state.curvalue)
    CASE tyscript
     'IF recurse < 3 AND state.curargn >= state.curargc THEN
      'currently executing this script (must have already printed it out)
      'cmd$ = "==>>"
     'ELSE
      cmd$ = scriptname$(state.curvalue)
     'END IF
   END SELECT
   'debug "kind = " + STR$(state.curkind)
   'debug "cmd$ = " + cmd$

   IF cmd$ <> "" THEN
    IF outstr$ = "" THEN
     outstr$ = cmd$
    ELSE
     IF (state.curargc = 1) AND (state.curargn = 0) AND (hidearg = 0) THEN
      outstr$ = cmd$ + ": " + outstr$
     ELSEIF state.curargn < state.curargc AND hidearg = 0 THEN
      outstr$ = cmd$ + ":" + STR$(state.curargn + 1) + "/" + STR$(state.curargc) + " " + outstr$
     ELSEIF hidearg = -2 THEN
      outstr$ = cmd$ + outstr$
     ELSE
      outstr$ = cmd$ + " " + outstr$
     END IF
    END IF
   END IF

   'don't check this because script might be queued up due to timers and triggers but not run, consuming 0 stack
   'IF stkpos <= stkbottom THEN EXIT DO

   state.depth -= 1

   IF state.depth < 0 THEN
    IF recurse = 0 THEN EXIT DO
    'load next script
    wasscript -= 1
    IF wasscript = targetscript THEN outstr$ = ""
    IF wasscript < targetscript THEN IF recurse <> 2 THEN EXIT DO
    IF wasscript < 0 THEN EXIT DO
    'macro disabled for fb 0.15 compat
    'copyobj(state, scrat(wasscript))
    memcpy(@(state),@(scrat(wasscript)),LEN(scrat(wasscript)))
    reloadscript state, 0
    IF scrat(wasscript).state < 0 THEN
     IF recurse = 2 OR recurse = 3 THEN
      'deal with state   (can only be wait? - goto streturn)
      CONTINUE DO
     ELSE
      EXIT DO
     END IF
    ELSE
     CONTINUE DO
    ' state.depth -= 1  'returning from a script kind or runscriptbyid command
    END IF
   END IF

   memcpy(@(laststate),@(state),LEN(state))

   readstackcommand state, scrst, stkpos

  jmpnext:
  jmpdoarg:

   'ditch arguments
   IF state.curkind = tyflow AND state.curvalue = flowswitch AND state.curargn > 0 THEN
    IF state.curargn >= state.curargc THEN
     'result of last case/do remains (?)
     stkpos -= 3
    ELSE
     stkpos -= 2
    END IF
   ELSE
    stkpos -= state.curargn
   END IF

   IF stkpos < stkbottom THEN scripterr "state corrupt; stack underflow " & (stkpos - stkbottom): EXIT DO
 LOOP
 IF stkpos > stkbottom AND wasscript < 0 THEN scripterr "state corrupt; stack garbage " & (stkpos - stkbottom)

 scriptstate$ = TRIM$(outstr$)

 reloadscript scrat(nowscript)
END FUNCTION

FUNCTION backcompat_sound_id (id AS INTEGER) as integer
  IF backcompat_sound_slot_mode THEN
   'BACKWARDS COMPATABILITY HACK
   IF id >= 0 AND id <= 7 THEN
    RETURN backcompat_sound_slots(id) - 1
   END IF
  ELSE
   'Normal playsound mode
   RETURN id
  END IF
END FUNCTION

'======== FIXME: move this up as code gets cleaned up ===========
OPTION EXPLICIT

SUB loadsay (box_id)
DIM j AS INTEGER
DIM rsr AS INTEGER

DO '--This loop is where we find which box will be displayed right now
 gen(genTextboxBackdrop) = 0
 txt.choice_cursor = 0

 '--load data from the textbox lump
 LoadTextBox txt.box, box_id

 FOR j = 0 TO 7
  embedtext txt.box.text(j), 38
 NEXT j

 '-- evaluate "instead" conditionals
 IF istag(txt.box.instead_tag, 0) THEN
  '--do something else instead
  IF txt.box.instead < 0 THEN
   rsr = runscript(-txt.box.instead, nowscript + 1, -1, "instead", plottrigger)
   txt.sayer = -1
   EXIT SUB
  ELSE
   IF box_id <> txt.box.instead THEN
    box_id = txt.box.instead
    CONTINUE DO' Skip back to the top of the loop and get another box
   END IF
  END IF
 END IF

 EXIT DO'--We have the box we want to display, proceed
LOOP

'--Store box ID number for later reference
txt.id = box_id

'-- set tags indicating the text box has been seen.
IF istag(txt.box.settag_tag, 0) THEN
 IF ABS(txt.box.settag1) > 1 THEN setbit tag(), 0, ABS(txt.box.settag1), SGN(SGN(txt.box.settag1) + 1)
 IF ABS(txt.box.settag2) > 1 THEN setbit tag(), 0, ABS(txt.box.settag2), SGN(SGN(txt.box.settag2) + 1)
END IF

'--make a sound if the choicebox is enabled
IF txt.box.choice_enabled THEN MenuSound gen(genAcceptSFX)

'-- update backdrop if necessary
IF txt.box.backdrop > 0 THEN
 gen(genTextboxBackdrop) = txt.box.backdrop
 correctbackdrop
END IF
'-- change music if necessary
IF txt.box.music > 0 THEN
 txt.remember_music = presentsong
 wrappedsong txt.box.music - 1
END IF

'-- evaluate menu conditionals
IF istag(txt.box.menu_tag, 0) THEN
 add_menu txt.box.menu
END IF

'--Get the portrait
load_text_box_portrait txt.box, txt.portrait

txt.showing = YES
txt.fully_shown = NO
txt.show_lines = 0

END SUB

SUB load_text_box_portrait (BYREF box AS TextBox, BYREF gfx AS GraphicPair)
 'WARNING: There is another version of this in customsubs.bas
 'If you update this here, make sure to update that one too!
 DIM img_id AS INTEGER = -1
 DIM pal_id AS INTEGER = -1
 DIM hero_id AS INTEGER = -1
 DIM her AS HeroDef
 WITH gfx
  IF .sprite THEN sprite_unload @.sprite
  IF .pal    THEN palette16_unload @.pal
  SELECT CASE box.portrait_type
   CASE 1' Fixed ID number
    img_id = box.portrait_id
    pal_id = box.portrait_pal
   CASE 2' Hero by caterpillar
    hero_id = herobyrank(box.portrait_id)
   CASE 3' Hero by party slot
    IF box.portrait_id >= 0 AND box.portrait_id <= UBOUND(hero) THEN
     hero_id = hero(box.portrait_id) - 1
    END IF
  END SELECT
  IF hero_id >= 0 THEN
   loadherodata @her, hero_id
   img_id = her.portrait
   pal_id = her.portrait_pal
  END IF
  IF img_id >= 0 THEN
   .sprite = sprite_load(game & ".pt8", img_id, 1, 50, 50)
   .pal    = palette16_load(game & ".pal", pal_id, 8, img_id)
  END IF
 END WITH
END SUB

FUNCTION valid_plotslice(byval handle as integer, byval cmd as string) as integer
 IF handle < LBOUND(plotslices) OR handle > UBOUND(plotslices) THEN
  debug cmd & ": invalid slice handle " & handle
  RETURN NO
 END IF
 IF plotslices(handle) = 0 THEN
  debug cmd & ": slice handle " & handle & " has already been deleted"
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION valid_plotsprite(byval handle as integer, byval cmd as string) as integer
 IF valid_plotslice(handle, cmd) THEN
  IF plotslices(handle)->SliceType = slSprite THEN
   RETURN YES
  ELSE
   debug cmd & ": slice handle " & handle & " is not a sprite"
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_plotrect(byval handle as integer, byval cmd as string) as integer
 IF valid_plotslice(handle, cmd) THEN
  IF plotslices(handle)->SliceType = slRectangle THEN
   RETURN YES
  ELSE
   debug cmd & ": slice handle " & handle & " is not a rect"
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION valid_resizeable_slice(byval handle as integer, byval cmd as string, byval ignore_fill as integer=NO) as integer
 IF valid_plotslice(handle, cmd) THEN
  DIM sl AS Slice Ptr
  sl = plotslices(handle)
  IF sl->SliceType = slRectangle OR sl->SliceType = slContainer THEN
   IF sl->Fill = NO OR ignore_fill THEN
    RETURN YES
   ELSE
    debug cmd & ": slice handle " & handle & " cannot be resized while filling parent"
   END IF
  ELSE
   debug cmd & ": slice handle " & handle & " is not resizeable"
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION create_plotslice_handle(byval sl as Slice Ptr) AS INTEGER
 DIM i as integer
 'First search for an empty slice handle slot (which sucks because it means they get re-used)
 FOR i = LBOUND(plotslices) to UBOUND(plotslices)
  IF plotslices(i) = 0 THEN
   'Store the slice pointer in the handle slot and return the handle number
   plotslices(i) = sl
   RETURN i
  END IF
 NEXT
 'If no room is available, make the array bigger.
 REDIM PRESERVE plotslices(LBOUND(plotslices) TO UBOUND(plotslices) + 32)
 'Store the slice pointer in the handle slot and return the handle number
 plotslices(i) = sl
 RETURN i
END FUNCTION

FUNCTION find_plotslice_handle(BYVAL sl AS Slice Ptr) AS INTEGER
 'Search plotslices() for a specific slice handle. If it is not present, add it
 IF sl = 0 THEN RETURN 0 ' it would be silly to search for a null pointer
 FOR i AS INTEGER = LBOUND(plotslices) TO UBOUND(plotslices)
  IF plotslices(i) = sl THEN RETURN i
 NEXT i
 'slice not found in table, so create a new handle for it
 RETURN create_plotslice_handle(sl)
END FUNCTION

FUNCTION load_sprite_plotslice(BYVAL spritetype AS INTEGER, BYVAL record AS INTEGER, BYVAL pal AS INTEGER=-1) AS INTEGER
 WITH sprite_sizes(spritetype)
  IF bound_arg(record, 0, gen(.genmax), "load_sprite_plotslice/" & .name, "sprite number") THEN
   DIM sl AS Slice Ptr
   sl = NewSliceOfType(slSprite, SliceTable.scriptsprite)
   ChangeSpriteSlice sl, spritetype, record, pal
   RETURN create_plotslice_handle(sl)
  END IF
 END WITH
 RETURN 0 'Failure, return zero handle
END FUNCTION

SUB change_sprite_plotslice(BYVAL handle AS INTEGER, BYVAL spritetype AS INTEGER, BYVAL record AS INTEGER, BYVAL pal AS INTEGER=-1, BYVAL frame AS INTEGER=-1, BYVAL fliph AS INTEGER=-2, BYVAL flipv AS INTEGER=-2)
 WITH sprite_sizes(spritetype)
  IF valid_plotslice(handle, "change_sprite_plotslice/" & .name) THEN
   IF bound_arg(record, 0, gen(.genmax), "change_sprite_plotslice/" & .name, "sprite number") THEN
    ChangeSpriteSlice plotslices(handle), spritetype, record, pal, frame, fliph, flipv
   END IF
  END IF
 END WITH
END SUB

SUB change_rect_plotslice(BYVAL handle AS INTEGER, BYVAL style AS INTEGER=-2, BYVAL bgcol AS INTEGER=-1, BYVAL fgcol AS INTEGER=-1, BYVAL border AS INTEGER=-2, BYVAL translucent AS INTEGER=-2)
 IF valid_plotslice(handle, "change_rect_plotslice") THEN
  DIM sl AS Slice Ptr
  sl = plotslices(handle)
  IF sl->SliceType = slRectangle THEN
   ChangeRectangleSlice sl, style, bgcol, fgcol, border, translucent
  ELSE
   debug "change_rect_plotslice: " & SliceTypeName(sl) & " is not a rect" 
  END IF
 END IF
END SUB
