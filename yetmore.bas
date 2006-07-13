'OHRRPGCE GAME - More various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION str2lng& (stri$)
DECLARE SUB setScriptArg (arg%, value%)
DECLARE FUNCTION cropPlotStr% (s$)
DECLARE SUB wrapaheadxy (x%, y%, direction%, distance%, unitsize%)
DECLARE SUB aheadxy (x%, y%, direction%, distance%)
DECLARE SUB wrapxy (x%, y%, wide%, high%)
DECLARE SUB loadSayToBuffer (say%)
DECLARE SUB touchfile (f$)
DECLARE SUB keyhandleroff ()
DECLARE FUNCTION partybyrank% (slot%)
DECLARE FUNCTION herobyrank% (slot%)
DECLARE FUNCTION rankincaterpillar% (heroid%)
DECLARE SUB embedtext (text$, limit%)
DECLARE SUB renamehero (who%)
DECLARE FUNCTION vehiclestuff% (disx%, disy%, foep%)
DECLARE FUNCTION trylearn% (who%, atk%, learntype%)
DECLARE SUB correctbackdrop ()
DECLARE FUNCTION gethighbyte% (n%)
DECLARE FUNCTION readbadbinstring$ (array%(), offset%, maxlen%, skipword%)
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE SUB delitem (it%, num%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION getnpcref% (seekid%, offset%)
DECLARE SUB suspendresume (id%)
DECLARE SUB scriptwatcher (page%)
DECLARE SUB onkeyscript (scriptnum%)
DECLARE SUB waitcommands (id%)
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE SUB greyscalepal ()
DECLARE SUB tweakpalette ()
DECLARE SUB vishero (stat%())
DECLARE SUB forceparty (stat%())
DECLARE SUB scriptdump (s$)
DECLARE FUNCTION vehpass% (n%, tile%, default%)
DECLARE FUNCTION readfoemap% (x%, y%, wide%, high%, fh%)
DECLARE FUNCTION playtime$ (d%, h%, m%)
DECLARE FUNCTION functiondone% ()
DECLARE FUNCTION functionread% ()
DECLARE FUNCTION averagelev% (stat%())
DECLARE FUNCTION countitem% (it%)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION movdivis% (xygo%)
DECLARE FUNCTION onwho% (w$, alone)
DECLARE SUB minimap (mx%, my%, x%, y%, tastuf%())
DECLARE SUB heroswap (iAll%, stat%())
DECLARE FUNCTION shoption (inn%, price%, needf%, stat%())
DECLARE SUB savegame (slot%, map%, foep%, stat%(), stock%())
DECLARE FUNCTION runscript% (n%, index%, newcall%, er$)
DECLARE SUB scripterr (e$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE SUB itstr (i%)
DECLARE FUNCTION findhero% (who%, f%, l%, d%)
DECLARE FUNCTION howmanyh% (f%, l%)
DECLARE FUNCTION consumeitem% (index%)
DECLARE FUNCTION istag% (num%, zero%)
DECLARE SUB doswap (s%, d%, stat%())
DECLARE SUB control ()
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB getitem (getit%, num%)
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%())
DECLARE SUB spells (pt%, stat%())
DECLARE SUB status (pt%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
DECLARE SUB resetlmp (slot%, lev%)
DECLARE SUB loadfoe (i%, formdata%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), stat%(), ebits%(), batname$())
DECLARE FUNCTION inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%())
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE SUB addhero (who%, slot%, stat%())
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION xstring% (s$, x%)
DECLARE SUB snapshot ()
DECLARE FUNCTION checksaveslot (slot%)
DECLARE FUNCTION readitemname$ (itemnum%)
DECLARE FUNCTION readatkname$ (id%)
DECLARE SUB getmapname (mapname$, m%)
DECLARE FUNCTION exptolevel& (level%)
DECLARE SUB updatestatslevelup (i%, exstat%(), stat%(), allowforget%)
DECLARE SUB giveheroexperience (i%, exstat%(), exper&)
DECLARE FUNCTION liveherocount% (stat%())
DECLARE SUB cleanuptemp ()
DECLARE FUNCTION getsongname$ (num%)
DECLARE FUNCTION getdisplayname$ (default$)
DECLARE SUB interpolatecat ()

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'common.bi' 
'$INCLUDE: 'gglobals.bi'
'$INCLUDE: 'const.bi'
'$INCLUDE: 'scrconst.bi'
'$INCLUDE: 'uiconst.bi'
'$INCLUDE: 'gfx.bi'

REM $STATIC
SUB arslhero (saytag(), stat())
'---ADD/REMOVE/SWAP/LOCK
'---ADD---
IF saytag(10) > 0 THEN
 i = findhero(0, 0, 40, 1)
 IF i > -1 THEN
  addhero saytag(10), i, stat()
  vishero stat()
 END IF
END IF '---end if > 0
'---REMOVE---
IF saytag(10) < 0 THEN
 IF howmanyh(0, 40) > 1 THEN
  i = findhero(ABS(saytag(10)), 0, 40, 1)
  IF i > -1 THEN hero(i) = 0
  IF howmanyh(0, 3) = 0 THEN forceparty stat()
 END IF
END IF '---end if < 0
vishero stat()
'---SWAP-IN---
IF saytag(19) > 0 THEN
 i = findhero(ABS(saytag(19)), 40, 0, -1)
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
IF saytag(19) < 0 THEN
 i = findhero(ABS(saytag(19)), 0, 40, 1)
 IF i > -1 THEN
  FOR o = 40 TO 4 STEP -1
   IF hero(o) = 0 THEN
    doswap i, o, stat()
    IF howmanyh(0, 3) = 0 THEN forceparty stat()
    EXIT FOR
   END IF
  NEXT o
 END IF
END IF '---end if < 0
'---UNLOCK HERO---
IF saytag(20) > 0 THEN
 temp = findhero(ABS(saytag(20)), 0, 40, 1)
 IF temp > -1 THEN setbit hmask(), 0, temp, 0
END IF '---end if > 0
'---LOCK HERO---
IF saytag(20) < 0 THEN
 temp = findhero(ABS(saytag(20)), 0, 40, 1)
 IF temp > -1 THEN setbit hmask(), 0, temp, 1
END IF '---end if > 0
END SUB

FUNCTION checksaveslot (slot)
  fbdim checkslot
  sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
  savh = FREEFILE
  OPEN sg$ FOR BINARY AS #savh
  GET #savh, 1 + 60000 * (slot - 1), checkslot
  CLOSE #savh
  checksaveslot = checkslot
END FUNCTION

FUNCTION cropPlotStr (s$)
 cropPlotStr = large(0, LEN(s$) - 40)
 s$ = LEFT$(s$, 40)
END FUNCTION

SUB doihavebits
setpicstuf buffer(), 636, -1
FOR i = 0 TO large(gen(35), 59)
 loadset game$ + ".dt0" + CHR$(0), i, 0
 herobits(i, 0) = buffer(292)    'have hero tag
 herobits(i, 1) = buffer(293)    'is alive tag
 herobits(i, 2) = buffer(294)    'is leader tag
 herobits(i, 3) = buffer(295)    'is in active party tag
NEXT i
setpicstuf buffer(), 200, -1
FOR i = 0 TO 255
 loadset game$ + ".itm" + CHR$(0), i, 0
 itembits(i, 0) = buffer(74)   'when have tag
 itembits(i, 1) = buffer(75)   'is in inventory
 itembits(i, 2) = buffer(76)   'is equiped tag
 itembits(i, 3) = buffer(77)   'is equiped by hero in active party
NEXT i
END SUB

SUB embedtext (text$, limit)
'--Clobbers buffer() !
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
      insert$ = names$(where)
     END IF
    CASE "P": '--Hero name by Party position
     IF arg < 40 THEN
      '--defaults blank if not found
      insert$ = ""
      IF hero(arg) > 0 THEN
       insert$ = names$(arg)
      END IF
     END IF
    CASE "C": '--Hero name by caterpillar position
     '--defaults blank if not found
     insert$ = ""
     where = partybyrank(arg)
     IF where >= 0 THEN
      insert$ = names$(where)
     END IF
    CASE "V": '--global variable by ID
     '--defaults blank if out-of-range
     insert$ = ""
     IF arg >= 0 AND arg <= 1024 THEN
      insert$ = LTRIM$(STR$(global(arg)))
     END IF
    CASE "S": '--string variable by ID
     insert$ = ""
     IF arg >= 0 AND arg <= 31 THEN
      insert$ = plotstring$(arg)
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
STATIC spellmaskhero
DIM dummystats(40, 1, 1) 'just need HP and MP

SELECT CASE id
 CASE 64'--get hero stat
  scriptret = stat(bound(retvals(0), 0, 40), bound(retvals(2), 0, 1), bound(retvals(1), 0, 13))
 CASE 66'--add hero
  IF retvals(0) >= 0 THEN
   FOR i = 37 TO 0 STEP -1
    IF hero(i) = 0 THEN slot = i
   NEXT i
   addhero retvals(0) + 1, slot, stat()
   vishero stat()
  END IF
 CASE 67'--delete hero
  IF howmanyh(0, 40) > 1 THEN
   i = findhero(bound(retvals(0), 0, 59) + 1, 0, 40, 1)
   IF i > -1 THEN hero(i) = 0
   IF howmanyh(0, 3) = 0 THEN forceparty stat()
   vishero stat()
  END IF
 CASE 68'--swap out hero
  i = findhero(retvals(0) + 1, 0, 40, 1)
  IF i > -1 THEN
   FOR o = 40 TO 4 STEP -1
    IF hero(o) = 0 THEN
     doswap i, o, stat()
     IF howmanyh(0, 3) = 0 THEN forceparty stat()
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
   spellmaskhero = retvals(0)  'used for spells learnt
   stat(retvals(0), 1, 12) = retvals(1) - stat(retvals(0), 0, 12)
   stat(retvals(0), 0, 12) = retvals(1)
   exlev&(retvals(0), 1) = exptolevel(retvals(1))
   exlev&(retvals(0), 0) = 0  'XP attained towards the next level
   updatestatslevelup retvals(0), stat(), dummystats(), retvals(2) 'updates stats and spells
  END IF
 CASE 184'--give experience (who, how much)
  spellmaskhero = retvals(0)  'used for spells learnt
  IF retvals(0) = -1 AND liveherocount(stat()) > 0 THEN retvals(1) = retvals(1) / liveherocount(stat())
  FOR i = 0 TO 40
   IF i = retvals(0) OR (retvals(0) = -1 AND i <= 3) THEN
    'give the XP to the hero only if it is alive if party is target
    IF retvals(0) <> -1 OR stat(i, 0, 0) THEN giveheroexperience i, stat(), (retvals(1))
    updatestatslevelup i, stat(), dummystats(), 0
   END IF
  NEXT i
 CASE 185'--hero levelled (who)
  scriptret = stat(bound(retvals(0), 0, 40), 1, 12)
 CASE 186'--spells learnt
  IF retvals(0) >= 0 AND (retvals(0) <= 3 OR retvals(0) = spellmaskhero) THEN
   effectivehero = small(retvals(0), 4)  'if the hero num was greater than 3, use hero 4's learnmask slots
   FOR i = effectivehero * 96 TO effectivehero * 96 + 95
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
END SELECT
END SUB

SUB flusharray (array(), size, value)
FOR i = 0 TO size
 array(i) = value
NEXT i
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

FUNCTION functiondone
'returns 0 when returning a value to a caller
'returns 1 when all scripts are finished
'returns 2 when reactivating a suspended script

'IF nowscript + 1 < 128 THEN
' '-- when a script terminates, the script directly above it in
' '-- the script buffer must be invalidated for caching
' scrat(nowscript + 1, scrid) = -1
'END IF

'--if the finishing script is at the top of the script buffer,
'--then nextscroff needs to be changed
IF scrat(nowscript, scrsize) <> 0 THEN nextscroff = scrat(nowscript, scroff)

nowscript = nowscript - 1

IF nowscript < 0 THEN
 functiondone = 1'--no scripts are running anymore
ELSE
 IF scrat(nowscript, scrstate) < 0 THEN
  '--suspended script is resumed
  scrat(nowscript, scrstate) = ABS(scrat(nowscript, scrstate))
  functiondone = 2'--reactivating a supended script
 ELSE
  scriptret = scrat(nowscript + 1, scrret)
  scrat(nowscript, scrstate) = streturn'---return
  functiondone = 0'--returning a value to a caller
 END IF
END IF

END FUNCTION

FUNCTION functionread
'returns false normally, true when it should terminate
functionread = 0
scriptret = 0'--default returnvalue is zero
scrat(nowscript, curkind) = script(scrat(nowscript, scroff) + scrat(nowscript, scrptr))
scrat(nowscript, curvalue) = script(scrat(nowscript, scroff) + scrat(nowscript, scrptr) + 1)
scrat(nowscript, curargc) = script(scrat(nowscript, scroff) + scrat(nowscript, scrptr) + 2)
scrat(nowscript, curargn) = 0
'scriptdump "functionread"
SELECT CASE scrat(nowscript, curkind)
 CASE tystop
  scripterr "interpretloop encountered noop": nowscript = -1: functionread = -1: EXIT FUNCTION
 CASE tynumber
  scriptret = scrat(nowscript, curvalue)
  scrat(nowscript, scrstate) = streturn'---return
 CASE tyglobal
  scriptret = global(bound(scrat(nowscript, curvalue), 0, 1024))
  scrat(nowscript, scrstate) = streturn'---return
 CASE tylocal
  scriptret = heap(scrat(nowscript, scrheap) + scrat(nowscript, curvalue))'--get from heap
  scrat(nowscript, scrstate) = streturn'---return
  '--flow control would be a special case
 CASE tymath, tyfunct, tyscript, tyflow
  scrat(nowscript, scrstate) = stnext '---function
 CASE ELSE
  scripterr "Illegal statement type" + STR$(scrat(nowscript, curkind))
END SELECT
END FUNCTION

FUNCTION gethighbyte (n)

DIM buf(1)

buf(0) = 0
buf(1) = n

FOR i = 0 TO 7
 setbit buf(), 0, i, readbit(buf(), 1, 8 + i)
NEXT i

gethighbyte = buf(0)

END FUNCTION

FUNCTION getnpcref (seekid, offset)
SELECT CASE seekid

 CASE -300 TO -1'--direct reference
  getnpcref = (seekid + 1) * -1
  EXIT FUNCTION

 CASE 0 TO 35 'ID
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

SUB getpal16 (array(), aoffset, foffset)

setpicstuf buffer(), 16, -1
loadset game$ + ".pal" + CHR$(0), 0, 0

IF buffer(0) = 4444 THEN '--check magic number
 IF buffer(1) >= foffset AND foffset >= 0 THEN
  'palette is available
  loadset game$ + ".pal" + CHR$(0), 1 + foffset, 0
  FOR i = 0 TO 7
   array(aoffset * 8 + i) = buffer(i)
  NEXT i
 ELSE
  'palette is out of range, return blank
  FOR i = 0 TO 7
   array(aoffset * 8 + i) = 0
  NEXT i
 END IF
ELSE '--magic number not found, palette is still in BSAVE format
 xbload game$ + ".pal", buffer(), "16-color palletes missing from " + game$
 FOR i = 0 TO 7
  array(aoffset * 8 + i) = buffer(foffset * 8 + i)
 NEXT i
END IF

END SUB

SUB greyscalepal
FOR i = bound(retvals(0), 0, 255) TO bound(retvals(1), 0, 255)
 r = master(i * 3)
 g = master(i * 3 + 1)
 b = master(i * 3 + 2)
 FOR j = 0 TO 2
  master(i * 3 + j) = bound((r + g + b) / 3, 0, 63)
 NEXT j
NEXT i
END SUB

FUNCTION herobyrank (slot)
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

'--back compat game$
a$ = ""
i = 0
IF usepreunlump = 0 THEN
IF INSTR(a$, SLASH) > 0 THEN
 DO UNTIL LEFT$(a$, 1) = SLASH
  i = i + 1
  a$ = RIGHT$(sourcerpg$, i)
 LOOP
END IF
a$ = LEFT$(a$, LEN(a$) - 4)
game$ = workingdir$ + a$
END IF
'--set game$ according to the archinym
IF isfile(workingdir$ + SLASH + "archinym.lmp" + CHR$(0)) THEN
 fh = FREEFILE
 OPEN workingdir$ + SLASH + "archinym.lmp" FOR INPUT AS #fh
 LINE INPUT #fh, a$
 CLOSE #fh
 a$ = LCASE$(a$)
 IF LEN(a$) <= 8 THEN game$ = workingdir$ + SLASH + a$
END IF
displayname$ = getdisplayname$(sourcerpg$)
setwindowtitle displayname$
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

SUB keyhandleroff

regs.ax = &H2509: regs.ds = seg9: regs.dx = off9
CALL interruptx(&H21, regs, regs)

END SUB

SUB keyhandleron

regs.ax = &H2509: regs.ds = Keyseg: regs.dx = keyoff
CALL interruptx(&H21, regs, regs)

END SUB

SUB loadsay (choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh())
DIM temp$

loadsaybegin:
gen(58) = 0
choosep = 0

'--load data from the textbox lump
loadSayToBuffer say

'--read in the lines of text
FOR j = 0 TO 7
 say$(j) = STRING$(38, 0)
 array2str buffer(), j * 38, say$(j)
NEXT j

FOR j = 0 TO 7
 embedtext say$(j), 38
NEXT j

'--reload data from the textbox lump because embedtext clobbered it
loadSayToBuffer say

'-- get the block of data used for conditionals
temp$ = STRING$(42, 0)
array2str buffer(), 305, temp$
str2array temp$, buffer(), 0

'-- store the conditionals data in saytag()
FOR j = 0 TO 20
 saytag(j) = buffer(j)
NEXT j

'-- evaluate "instead" conditionals
IF istag(saytag(0), 0) THEN
 '--do something else instead
 IF saytag(1) < 0 THEN
  rsr = runscript(ABS(saytag(1)), nowscript + 1, -1, "instead")
  sayer = -1
  EXIT SUB
 ELSE
  IF say <> saytag(1) THEN say = saytag(1): GOTO loadsaybegin
 END IF
END IF

'-- set tags indicating the text box has been seen.
IF istag(saytag(2), 0) THEN
 IF ABS(saytag(3)) > 1 THEN setbit tag(), 0, ABS(saytag(3)), SGN(SGN(saytag(3)) + 1)
 IF ABS(saytag(4)) > 1 THEN setbit tag(), 0, ABS(saytag(4)), SGN(SGN(saytag(4)) + 1)
END IF

'-- load choicebox data
FOR j = 0 TO 1
 choose$(j) = STRING$(15, 0)
 array2str buffer(), 349 + (j * 18), choose$(j)
 WHILE RIGHT$(choose$(j), 1) = CHR$(0): choose$(j) = LEFT$(choose$(j), LEN(choose$(j)) - 1): WEND
 chtag(j) = buffer(182 + (j * 9))
NEXT j

'--the bitset that determines whether the choicebox is enabled
saybit(0) = buffer(174)

'--load box appearance into sayenh()
FOR j = 0 TO 6
 sayenh(j) = buffer(193 + j)
NEXT j
'-- update backdrop if necessary
IF sayenh(4) > 0 THEN
 gen(58) = sayenh(4)
 correctbackdrop
END IF
'-- change music if necessary
IF sayenh(5) > 0 THEN wrappedsong sayenh(5) - 1

showsay = 8

END SUB

SUB loadSayToBuffer (say)

'--load data from the textbox lump into the buffer

IF say > gen(39) THEN ' It's 39!
 str2array "Invalid textbox" + STRING$(385, 0), buffer(), 0
ELSE
 setpicstuf buffer(), 400, -1
 loadset game$ + ".say" + CHR$(0), say, 0
END IF

END SUB

SUB npcplot
FOR i = 0 TO 299
 curnpc = ABS(npc(i).id) - 1

 IF npc(i).id < 0 THEN
  '--check reappearance tags for existing but hidden NPCs
  IF istag(npcs(curnpc * 15 + 9), 1) AND istag(npcs(curnpc * 15 + 10), 1) AND istag(1000 + npcs(curnpc * 15 + 11), 0) = 0 THEN
   npc(i).id = ABS(npc(i).id)
  END IF
 END IF

 IF npc(i).id > 0 THEN
  '--check removal tags for existing visible NPCs
  IF istag(npcs(curnpc * 15 + 9), 1) = 0 OR istag(npcs(curnpc * 15 + 10), 1) = 0 OR istag(1000 + npcs(curnpc * 15 + 11), 0) THEN
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
 IF scrat(nowscript, scrstate) = stwait AND scrat(nowscript, curvalue) = 9 THEN
  '--never trigger a onkey script when the previous script
  '--has a "wait for key" command active
  doit = 0
 END IF
END IF

IF doit = 1 THEN
 rsr = runscript(scriptnum, nowscript + 1, -1, "on-key")
END IF

END SUB

FUNCTION partybyrank (slot)
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

FUNCTION playtime$ (d, h, m)
s$ = ""

SELECT CASE d
 CASE 1
  s$ = s$ + LTRIM$(STR$(d)) + " " + readglobalstring$(154, "day", 10) + " "
 CASE IS > 1
  s$ = s$ + LTRIM$(STR$(d)) + " " + readglobalstring$(155, "days", 10) + " "
END SELECT

SELECT CASE h
 CASE 1
  s$ = s$ + LTRIM$(STR$(h)) + " " + readglobalstring$(156, "hour", 10) + " "
 CASE IS > 1
  s$ = s$ + LTRIM$(STR$(h)) + " " + readglobalstring$(157, "hours", 10) + " "
END SELECT

SELECT CASE m
 CASE 1
  s$ = s$ + LTRIM$(STR$(m)) + " " + readglobalstring$(158, "minute", 10) + " "
 CASE IS > 1
  s$ = s$ + LTRIM$(STR$(m)) + " " + readglobalstring$(159, "minutes", 10) + " "
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

SUB quitcleanup

'DEBUG debug "Cleanup Routine"
'--open files
'DEBUG debug "Close foemap handle"
CLOSE #foemaph
'DEBUG debug "Close lockfile"
IF lockfile THEN CLOSE #lockfile
'--keyboard
'DEBUG debug "Shut off keyhandler"
keyhandleroff
'--script stack
'DEBUG debug "Release script stack"
releasestack
'--working files
'DEBUG debug "Touch killfile"
touchfile workingdir$ + SLASH + "kill.tmp"
'DEBUG debug "Kill working files"

cleanuptemp

'DEBUG debug "Remove working directory"
IF usepreunlump = 0 THEN RMDIR workingdir$
'--reset audio
'closefile
'DEBUG debug "Unload BAM player"
closemusic
closesound
'DEBUG debug "Restore original FM volume"
setfmvol fmvol

END SUB

FUNCTION rankincaterpillar (heroid)
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

FUNCTION readbadbinstring$ (array(), offset, maxlen, skipword)

result$ = ""
strlen = bound(array(offset), 0, maxlen)

FOR i = 1 TO strlen
 '--read and int
 n = array(offset + skipword + i)
 '--if the int is a char use it.
 IF n >= 0 AND n <= 255 THEN
  '--take the low byte
  n = (n AND &HFF)
  '--use it
  result$ = result$ + CHR$(n)
 END IF
NEXT i

readbadbinstring$ = result$

END FUNCTION

FUNCTION readbinstring$ (array(), offset, maxlen)

result$ = ""
strlen = bound(array(offset), 0, maxlen)

i = 1
DO WHILE LEN(result$) < strlen
 '--get an int
 n = array(offset + i)
 i = i + 1

 '--break apart the int
 lowbyte = (n AND &HFF)
 highbyte = gethighbyte(n)

 '--append the lowbyte as a char
 result$ = result$ + CHR$(lowbyte)

 '--if we still care about the highbyte, append it as a char too
 IF LEN(result$) < strlen THEN
  result$ = result$ + CHR$(highbyte)
 END IF

LOOP

readbinstring$ = result$


END FUNCTION

FUNCTION readfoemap (x, y, wide, high, fh)

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

DIM mouse(4)

SELECT CASE id

 CASE 135'--puthero
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   catx(retvals(0) * 5) = retvals(1)
   caty(retvals(0) * 5) = retvals(2)
  END IF
 CASE 136'--putnpc
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   npc(npcref).x = retvals(1)
   npc(npcref).y = retvals(2)
  END IF
 CASE 137'--putcamera
  gen(cameramode) = stopcam
  mapx = retvals(0)
  mapy = retvals(1)
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
 CASE 160'--get mouse x
  readmouse mouse()
  scriptret = mouse(0)
 CASE 161'--get mouse y
  readmouse mouse()
  scriptret = mouse(1)
 CASE 162'--mouse button
  IF retvals(0) <= 1 THEN
   readmouse mouse()
   IF mouse(2) AND 2 ^ retvals(0) THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 163'--put mouse
  movemouse retvals(0), retvals(1)
 CASE 164'--mouse region
  mouserect retvals(0), retvals(1), retvals(2), retvals(3)
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

DIM statestr$(6)
statestr$(0) = "none"
statestr$(1) = "wait"
statestr$(2) = "read"
statestr$(3) = "return"
statestr$(4) = "next"
statestr$(5) = "doarg"
statestr$(6) = "done"

IF scrat(nowscript, scrdepth) >= 0 THEN
  indent$ = STRING$(scrat(nowscript, scrdepth), " ")
ELSE
  indent$ = STRING$(ABS(scrat(nowscript, scrdepth)), "<")
END IF

SELECT CASE scrat(nowscript, scrstate)
 CASE 0 TO 6
   state$ = " " + statestr$(scrat(nowscript, scrstate))
 CASE ELSE
   state$ = STR$(scrat(nowscript, scrstate))
END SELECT

debug indent$ + "[" + s$ + "]"
debug indent$ + "script =" + STR$(nowscript)
debug indent$ + "ptr    =" + STR$(scrat(nowscript, scrptr))
debug indent$ + "state  =" + state$
debug indent$ + "kind   =" + STR$(scrat(nowscript, curkind))
debug indent$ + "value  =" + STR$(scrat(nowscript, curvalue))
debug indent$ + "argn   =" + STR$(scrat(nowscript, curargn))
debug indent$ + "argc   =" + STR$(scrat(nowscript, curargc))

END SUB

SUB scriptmisc (id)
 fbdim temp16 'required for FB to fix get and put

'contains a whole mess of scripting commands that do not depend on
'any main-module level local variables or GOSUBs, and therefore
'can be moved out here to save memory

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
  IF retvals(0) >= -300 AND retvals(0) <= 35 THEN
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
  IF retvals(0) > 1 THEN
   setbit tag(), 0, retvals(0), retvals(1)
   'reinitnpc 1,  map
   npcplot
  END IF
 CASE 17'--get item
  IF retvals(1) >= 1 THEN
   getitem retvals(0) + 1, retvals(1)
  END IF
 CASE 18'--delete item
  IF retvals(1) >= 1 THEN
   delitem retvals(0) + 1, retvals(1)
  END IF
 CASE 19'--leader
  FOR i = 0 TO 3
   IF hero(0) > 0 THEN scriptret = hero(0) - 1: EXIT FOR
  NEXT i
 CASE 20'--get money
  gold& = gold& + retvals(0)
 CASE 21'--lose money
  gold& = gold& - retvals(0)
  IF gold& < 0 THEN gold& = 0
 CASE 22'--pay money
  IF gold& - retvals(0) >= 0 THEN
   gold& = gold& - retvals(0)
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
  'loadsong game$ + "." + LTRIM$(STR$(retvals(0))) + CHR$(0)
  wrappedsong retvals(0)
 CASE 29'--stop song
  stopsong
 CASE 30'--keyval
  scriptret = keyval(retvals(0))
 CASE 31'--rank in caterpillar
  scriptret = rankincaterpillar(retvals(0))
 CASE 38'--camera follows hero
  gen(cameramode) = herocam
  gen(cameraArg) = bound(retvals(0), 0, 3) * 5
 CASE 40'--pan camera
  gen(cameramode) = pancam
  gen(cameraArg) = small(large(retvals(0), 0), 3)
  gen(cameraArg2) = large(retvals(1), 0) * (20 / large(retvals(2), 1))
  gen(cameraArg3) = large(retvals(2), 0)
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
  setpicstuf buffer(), 200, -1
  loadset game$ + ".itm" + CHR$(0), bound(retvals(1), 0, 255), 0
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
   i = hero(retvals(0)) - 1
   IF i >= 0 THEN
    IF readbit(buffer(), 66, i) THEN
     scriptret = buffer(49)
    END IF
   END IF
  END IF
 CASE 62, 168'--suspend random enemies
  setbit gen(), 44, suspendrandomenemies, 1
  '--resume random enemies is not here! it works different!
 CASE 65'--resume overlay
  setbit gen(), 44, suspendoverlay, 0
 CASE 70'--room in active party
  scriptret = 4 - howmanyh(0, 3)
 CASE 71'--lock hero
  temp = findhero(retvals(0) + 1, 0, 40, 1)
  IF temp > -1 THEN setbit hmask(), 0, temp, 1
 CASE 72'--unlock hero
  temp = findhero(retvals(0) + 1, 0, 40, 1)
  IF temp > -1 THEN setbit hmask(), 0, temp, 0
 CASE 74'--set death script
  gen(42) = large(retvals(0), 0)
 CASE 75'--fade screen out
  fadeout bound(retvals(0), 0, 63), bound(retvals(1), 0, 63), bound(retvals(2), 0, 63), -1
 CASE 76'--fade screen in
  fadein -1
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
  xbload game$ + ".mas", master(), "master palette missing from " + game$
 CASE 104'--tweak palette
  tweakpalette
 CASE 105'--read color
  scriptret = master(bound(retvals(0), 0, 255) * 3 + bound(retvals(1), 0, 3))
 CASE 106'--write color
  master(bound(retvals(0), 0, 255) * 3 + bound(retvals(1), 0, 3)) = bound(retvals(2), 0, 63)
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
  IF retvals(0) >= 0 AND retvals(0) <= 1024 THEN
   scriptret = global(retvals(0))
  ELSE
   scripterr "Cannot read global" + STR$(retvals(0)) + ". out of range"
  END IF
 CASE 115'--write global
  IF retvals(0) >= 0 AND retvals(0) <= 1024 THEN
   global(retvals(0)) = retvals(1)
  ELSE
   scripterr "Cannot write global" + STR$(retvals(0)) + ". out of range"
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
  retvals(0) = bound(retvals(0), 0, 40)
  IF retvals(1) > 0 THEN
   setpicstuf buffer(), 636, -1
   loadset game$ + ".dt0" + CHR$(0), hero(retvals(0)) - 1, 0
   FOR i = 0 TO 3
    FOR j = 0 TO 23
     IF spell(retvals(0), i, j) = 0 THEN
      k = (i * 48) + (j * 2)
      IF buffer(47 + k) = retvals(1) AND buffer(48 + k) = retvals(2) THEN
       scriptret = 1
       EXIT FOR
      END IF
     END IF
    NEXT j
   NEXT i
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
  IF hero(retvals(0)) > 0 THEN
   renamehero retvals(0)
  END IF
 CASE 171'--saveslotused
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   IF checksaveslot(retvals(0)) THEN scriptret = 1 ELSE scriptret = 0
  END IF
 CASE 172'--importglobals
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
   setpicstuf buffer(), 30000, -1
   loadset sg$ + CHR$(0), retvals(0) * 2 - 1, 0
   IF retvals(1) = -1 THEN 'importglobals(slot)
    retvals(1) = 0
    retvals(2) = 1024
   END IF
   IF retvals(2) = -1 THEN 'importglobals(slot,id)
    scriptret = buffer(retvals(1) + 5013)
   ELSE                    'importglobals(slot,first,last)
    IF retvals(1) >= 0 AND retvals(2) <= 1024 AND retvals(1) <= retvals(2) THEN
     FOR i = retvals(1) TO retvals(2)
      global(i) = buffer(i + 5013)
     NEXT i
    END IF
   END IF
  END IF
 CASE 173'--exportglobals
  IF retvals(0) >= 1 AND retvals(0) <= 32 AND retvals(1) >= 0 AND retvals(2) <= 1024 AND retvals(1) <= retvals(2) THEN
   setpicstuf buffer(), 30000, -1
   sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
   loadset sg$ + CHR$(0), retvals(0) * 2 - 1, 0
   FOR i = retvals(1) TO retvals(2)
    buffer(i + 5013) = global(i)
   NEXT i
   storeset sg$ + CHR$(0), retvals(0) * 2 - 1, 0
  END IF
 CASE 175'--deletesave
  IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
   IF checksaveslot(retvals(0)) THEN
    fbdim savver ' for FB
    sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
    savh = FREEFILE
    OPEN sg$ FOR BINARY AS #savh
    savver = 0
    PUT #savh, 1 + 60000 * (retvals(0) - 1), savver
    CLOSE #savh
   END IF
  END IF
 CASE 176'--runscriptbyid
  IF isfile(workingdir$ + SLASH + LTRIM$(STR$(retvals(0))) + ".hsx" + CHR$(0)) THEN
   rsr = runscript(retvals(0), nowscript + 1, 0, "indirect")
   IF rsr = 1 THEN
    '--fill heap with return values
    FOR i = scrat(nowscript - 1, curargc) - 1 TO 1 STEP -1  'flexible argument number!
     setScriptArg i - 1, retvals(i)
    NEXT i
   END IF
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
 CASE 189'--get formation song
  fh = FREEFILE
  IF retvals(0) >= 0 AND retvals(0) < gen(genMaxFormation) THEN
   OPEN game$ + ".FOR" FOR BINARY AS #fh
   'GET #fh,clng(retvals(0)) * 80 + 66,scriptret
   scriptret = readshort(fh,clng(retvals(0)) * 80 + 67)
   CLOSE #fh
  ELSE
   scriptret = -1
  END IF
 CASE 190'--set formation song
  fh = FREEFILE
  IF retvals(0) >= 0 AND retvals(0) < gen(genMaxFormation) AND retvals(1) >= 0 AND retvals(1) <= gen(genMaxSong) THEN
   OPEN game$ + ".FOR" FOR BINARY AS #fh
   'GET #fh,clng(retvals(0)) * 80 + 66,scriptret
   WriteShort fh,clng(retvals(0)) * 80 + 65,retvals(1)
   setbit lumpmod(),0,1,1
   CLOSE #fh
  ELSE
   scriptret = -1
  END IF
 CASE 191'--hero frame
  IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
   scriptret = wtog(retvals(0)) \ 2
  END IF
 CASE 195'--load sound
  IF retvals(0) >= 0 AND retvals(0) <= sfxslots THEN
   sfx$ = soundfile(retvals(1))
   slot = retvals(0) - 1
   IF sfx$ <> "" THEN
    loadsfx slot,sfx$
   END IF
   scriptret = slot + 1
  END IF
 CASE 196'--free sound
  IF retvals(0) >= 1 AND retvals(0) <= sfxslots THEN
   slot = retvals(0) - 1
   freesfx slot
   scriptret = -1
  END IF
 CASE 197'--play sound
  IF retvals(0) >= 1 AND retvals(0) <= sfxslots THEN
   slot = retvals(0) - 1
   if retvals(2) then stopsfx slot
   playsfx slot, retvals(1)
   scriptret = -1
  END IF
 CASE 198'--pause sound
  IF retvals(0) >= 1 AND retvals(0) <= sfxslots THEN
   slot = retvals(0) - 1
   pausesfx slot
   scriptret = -1
  END IF
 CASE 199'--stop sound
  IF retvals(0) >= 1 AND retvals(0) <= sfxslots THEN
   slot = retvals(0) - 1
   stopsfx slot
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
  IF retvals(0) > 31 OR retvals(0) < 0 OR retvals(1) < 0 OR retvals(1) > 39 THEN
   scriptret = 0
  ELSE
   plotstring$(retvals(0)) = names$(retvals(1))
   scriptret = 1
  END IF
 CASE 205'--set hero name
  IF retvals(0) > 31 OR retvals(0) < 0 OR retvals(1) < 0 OR retvals(1) > 39 THEN
   scriptret = 0
  ELSE
   names$(retvals(1)) = plotstring$(retvals(0))
   scriptret = 1
  END IF
 CASE 206'--get item name(str,itm)
  IF retvals(0) > 31 OR retvals(0) < 0 OR retvals(1) < 0 OR retvals(1) > 255 THEN
   scriptret = 0
  ELSE
   plotstring$(retvals(0)) = readitemname(retvals(1))
   scriptret = 1
  END IF
 CASE 207'--get map name(str,map)
   IF retvals(0) > 31 OR retvals(0) < 0 OR retvals(1) < 0 OR retvals(1) > gen(genMaxMap) THEN
   scriptret = 0
  ELSE
   getmapname plotstring$(retvals(0)), retvals(1)
   scriptret = 1
  END IF
 CASE 208'--get attack name(str,atk)
  IF retvals(0) > 31 OR retvals(0) < 0 OR retvals(1) < 0 OR retvals(1) > gen(genMaxAttack) THEN
   scriptret = 0
  ELSE
   plotstring$(retvals(0)) = readatkname$(retvals(1) + 1)
   scriptret = 1
  END IF
 CASE 209'--get global string(str,glo)
  IF retvals(0) > 31 OR retvals(0) < 0 OR retvals(1) < 0 OR retvals(1) > 80 THEN
   scriptret = 0
  ELSE
   plotstring$(retvals(0)) = readglobalstring$(retvals(1), "", 255)
   scriptret = 1
  END IF
 CASE 211'--clear string
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN plotstring$(retvals(0)) = ""
 CASE 212'--append ascii
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
   IF retvals(1) >= 0 AND retvals(1) <= 255 THEN
    plotstring$(retvals(0)) = plotstring$(retvals(0)) + CHR$(retvals(1))
    scriptret = cropPlotStr(plotstring$(retvals(0)))
   END IF
  END IF
 CASE 213'--append number
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
   plotstring$(retvals(0)) = plotstring$(retvals(0)) + LTRIM$(STR$(retvals(1)))
   scriptret = cropPlotStr(plotstring$(retvals(0)))
  END IF
 CASE 214'--copy string
  IF retvals(0) >= 0 AND retvals(0) <= 31 AND retvals(1) >= 0 AND retvals(1) <= 31 THEN
   plotstring$(retvals(0)) = plotstring$(retvals(1))
  END IF
 CASE 215'--concatenate strings
  IF retvals(0) >= 0 AND retvals(0) <= 31 AND retvals(1) >= 0 AND retvals(1) <= 31 THEN
   plotstring$(retvals(0)) = plotstring$(retvals(0)) + plotstring$(retvals(1))
   scriptret = cropPlotStr(plotstring$(retvals(0)))
  END IF
 CASE 216'--string length
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
   scriptret = LEN(plotstring$(retvals(0)))
  END IF
 CASE 217'--delete char
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
   IF retvals(1) >= 1 AND retvals(1) <= LEN(plotstring$(retvals(0))) THEN
    temp2$ = LEFT$(plotstring$(retvals(0)), retvals(1) - 1)
    temp3$ = MID$(plotstring$(retvals(0)), retvals(1) + 1)
    plotstring$(retvals(0)) = temp2$ + temp3$
    temp3$ = ""
    temp2$ = ""
   END IF
  END IF
 CASE 218'--replace char
  IF retvals(0) >= 0 AND retvals(0) <= 31 AND retvals(2) >= 0 AND retvals(2) <= 255 THEN
   IF retvals(1) >= 1 AND retvals(1) <= LEN(plotstring$(retvals(0))) THEN
    MID$(plotstring$(retvals(0)), retvals(1), 1) = CHR$(retvals(2))
   END IF
  END IF
 CASE 219'--ascii from string
  IF retvals(0) >= 0 AND retvals(0) <= 31 AND retvals(1) >= 1 AND retvals(1) <= LEN(plotstring$(retvals(0))) THEN
   scriptret = ASC(MID$(plotstring$(retvals(0)), retvals(1), 1))
  END IF
 CASE 220'--position string
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
   plotstrX(retvals(0)) = retvals(1)
   plotstrY(retvals(0)) = retvals(2)
  END IF
 CASE 221'--set string bit
  IF retvals(0) >= 0 AND retvals(0) <= 31 AND retvals(1) >= 0 AND retvals(1) <= 15 THEN
   IF retvals(2) THEN retvals(2) = 1
   setbit plotstrBits(), retvals(0), retvals(1), retvals(2)
  END IF
 CASE 222'--get string bit
  IF retvals(0) >= 0 AND retvals(0) <= 31 AND retvals(1) >= 0 AND retvals(1) <= 15 THEN
   scriptret = readbit(plotstrBits(), retvals(0), retvals(1))
   IF scriptret THEN scriptret = 1
  END IF
 CASE 223'--string color
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
   plotstrCol(retvals(0)) = bound(retvals(1), 0, 255)
   plotstrBGCol(retvals(0)) = bound(retvals(2), 0, 255)
  END IF
 CASE 224'--string X
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
   scriptret = plotstrX(retvals(0))
  END IF
 CASE 225'--string Y
  IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
   scriptret = plotstrY(retvals(0))
  END IF
 CASE 226'--system day (date$ is always mm-dd-yyyy)
  scriptret = str2int(MID$(DATE$, 4, 2))
 CASE 227'--system month
  scriptret = str2int(MID$(DATE$, 1, 2))
 CASE 228'--system year
  scriptret = str2int(MID$(DATE$, 7, 4))
 CASE 229'--string compare
  IF retvals(0) >= 0 AND retvals(0) <= 31 AND retvals(1) >= 0 AND retvals(1) <= 31 THEN
   scriptret = (plotstring$(retvals(0)) = plotstring$(retvals(1)))
  END IF
 CASE 230'--read enemy data
  f = FREEFILE
  OPEN game$ + ".dt1" FOR BINARY AS #f
  scriptret = ReadShort(f, (CLNG(bound(retvals(0), 0, gen(genMaxEnemy))) * CLNG(320)) + (bound(retvals(1), 0, 159) * 2) + 1)
  CLOSE #f
 CASE 231'--write enemy data
  f = FREEFILE
  OPEN game$ + ".dt1" FOR BINARY AS #f
  Writeshort f, (CLNG(bound(retvals(0), 0, gen(genMaxEnemy))) * CLNG(320)) + (bound(retvals(1), 0, 159) * 2) + 1, retvals(2)
  CLOSE #f
  setbit lumpmod(),0,0,1
 CASE 232'--trace
  debug "TRACE: " + plotstring$(bound(retvals(0),0,31))
 CASE 233'--get song name
  IF retvals(0) >= 0 AND retvals(0) <= 31 AND retvals(1) >= 0 THEN plotstring$(retvals(0)) = getsongname$(retvals(1))
 CASE 235'--key is pressed
  SELECT CASE AS CONST retvals(0)
  CASE 1 TO 127 'keyboard
   IF keyval(retvals(0)) THEN scriptret = 1 ELSE scriptret = 0
  CASE 128 TO 147 'joystick
   dim b as integer, xaxis as integer, yaxis as integer '0 >= x and y, >= 100
   IF io_readjoysane(bound(retvals(1),0,3),b,xaxis,yaxis) THEN
    IF retvals(0) >= 128 AND retvals(0) <= 143 THEN
     scriptret = (b SHR (retvals(0) - 128)) AND 1
    ELSEIF retvals(0) = 144 THEN 'x left
     'debug str$(xaxis)
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
  IF retvals(0) >= 1 AND retvals(0) <= sfxslots THEN
   slot = retvals(0) - 1
   scriptret = sfxisplaying(slot)
  END IF
 CASE 237'--sound slots
  scriptret = sfxslots
 CASE 238'--Search string
  scriptret = instr(bound(retvals(2),1,40),plotstring$(bound(retvals(0),0,31)),plotstring$(bound(retvals(1),0,31)))
 CASE 239'--Trim String
  retvals(0) = bound(retvals(0),0,31)
  if retvals(1) <> -1 then retvals(1) = bound(retvals(1),1,40)
  retvals(2) = bound(retvals(2),1,40)
  if retvals(1) = -1 then
   plotstring$(retvals(0)) = trim$(plotstring$(retvals(0)))
  else
   plotstring$(retvals(0)) = MID$(plotstring$(retvals(0)),retvals(1),retvals(2))
  end if
 CASE 240'-- String From Textbox
  retvals(0) = bound(retvals(0),0,31)
  retvals(1) = bound(retvals(1),0,gen(genMaxTextbox))
  retvals(2) = bound(retvals(2),0,7)
  retvals(3) = (retvals(3) = 0)
  
  loadsaytobuffer retvals(1)
  plotstring$(retvals(0)) = string$(38,0)
  array2str buffer() , retvals(2) * 38 , plotstring$(retvals(0))
  if retvals(3) then embedtext plotstring$(retvals(0)), 40 'this is 40, not 38, because strings can be up to 40 chars long
  plotstring$(retvals(0)) = trim$(plotstring$(retvals(0)))
 CASE 241'-- expand string(id)
  retvals(0) = bound(retvals(0),0,31)
  embedtext plotstring$(retvals(0)), 40 'same here
 CASE 242'-- joystick button
  retvals(0) = bound(retvals(0)-1,0,15)
  retvals(1) = bound(retvals(1),0,3)
  
  IF io_readjoysane(retvals(1),b,0,0) THEN
   scriptret = (b SHR retvals(0)) AND 1
  ELSE
   scriptret = 0
  END IF
 CASE 243'-- joystick axis
  retvals(0) = bound(retvals(0),0,1)
  retvals(2) = bound(retvals(2),0,3)
  
  IF io_readjoysane(retvals(2),0,xaxis,yaxis) THEN
   IF retvals(0) = 0 THEN'x axis
    'debug "x" + str$(xaxis)
    scriptret = int((xaxis / 100) * retvals(1)) 'normally, xaxis * 100
   ELSEIF retvals(0) = 1 THEN 'y axis
    'debug "y" + str$(yaxis)
    scriptret = int((yaxis / 100) * retvals(1)) 'normally, yaxis * 100
   END IF
  ELSE
   'debug "joystick failed"
   scriptret = 0
  END IF
  
END SELECT

EXIT SUB

setwaitstate:
scrat(nowscript, curwaitarg) = retvals(0)
scrat(nowscript, scrstate) = stwait
RETURN

END SUB

SUB scriptnpc (id)

'contains npc related scripting commands

SELECT CASE id

 CASE 26'--set NPC frame
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN npc(npcref).frame = bound(retvals(1), 0, 1) * 2
 CASE 39'--camera follows NPC
  gen(cameramode) = npccam
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN gen(cameraArg) = npcref
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
     npc(npcref).xgo = (retvals(2) * 20) * -1
    CASE 2'--south
     npc(npcref).dir = 2
     npc(npcref).ygo = (retvals(2) * 20) * -1
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
  IF retvals(0) >= 0 AND retvals(0) < 36 THEN
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
   IF npc(i).x \ 20 = retvals(0) AND npc(i).y \ 20 = retvals(1) and npc(i).id > 0 THEN
    IF found = retvals(2) THEN
     scriptret = (i + 1) * -1
     EXIT FOR
    END IF
    found = found + 1
   END IF
  NEXT i
  IF retvals(2) = -1 THEN scriptret = found
 CASE 122'--get NPC ID
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   scriptret = npc(npcref).id - 1
  ELSE
   scriptret = -1
  END IF
 CASE 123'--NPC copy count
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= 35 THEN
   FOR i = 0 TO 299
    IF npc(i).id - 1 = retvals(0) THEN
     scriptret = scriptret + 1
    END IF
   NEXT i
  END IF
 CASE 124'--change NPC ID
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 AND retvals(1) >= 0 AND retvals(1) <= 35 THEN npc(npcref).id = retvals(1) + 1
 CASE 125'--create NPC
  scriptret = 0
  IF retvals(0) >= 0 AND retvals(0) <= 35 THEN
   FOR i = 299 TO 0 STEP -1
    IF npc(i).id <= 0 THEN
     npc(i).id = retvals(0) + 1
     npc(i).x = retvals(1) * 20
     npc(i).y = retvals(2) * 20
     npc(i).dir = ABS(retvals(3)) MOD 4
     npc(i).xgo = 0
     npc(i).ygo = 0
     scriptret = (i + 1) * -1
     EXIT FOR
    END IF
   NEXT i
  END IF
 CASE 126 '--destroy NPC
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN npc(npcref).id = 0
 CASE 165'--NPC at pixel
  scriptret = 0
  found = 0
  FOR i = 0 TO 299
   IF npc(i).id > 0 AND npc(i).x <= retvals(0) AND npc(i).x > (retvals(0) - 20) AND npc(i).y <= retvals(1) AND npc(i).y > (retvals(1) - 20) THEN
    IF found = retvals(2) THEN
     scriptret = (i + 1) * -1
     EXIT FOR
    END IF
    found = found + 1
   END IF
  NEXT i
  IF retvals(2) = -1 THEN scriptret = found
 CASE 182'--read NPC
  IF retvals(1) >= 0 AND retvals(1) <= 14 THEN
   IF retvals(0) >= 0 AND retvals(0) <= 35 THEN
    scriptret = npcs(retvals(0) * 15 + retvals(1))
   ELSE
    npcref = getnpcref(retvals(0), 0)
    IF npcref >= 0 THEN
     scriptret = npcs((npc(npcref).id - 1) * 15 + retvals(1))
    END IF
   END IF
  END IF
 CASE 192'--NPC frame
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN scriptret = npc(npcref).frame \ 2
 CASE 193'--NPC extra
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   IF retvals(1) MOD 2 = 1 THEN '1
    scriptret = npc(npcref).extra1
   ELSEIF retvals(1) MOD 2 = 0 THEN '2
    scriptret = npc(npcref).extra2
   END IF
  END IF
 CASE 194'--set NPC extra
  npcref = getnpcref(retvals(0), 0)
  IF npcref >= 0 THEN
   IF retvals(1) MOD 2 = 1 THEN '1
    npc(npcref).extra1 = retvals(2)
   ELSEIF retvals(1) MOD 2 = 0 THEN '2
    npc(npcref).extra2 = retvals(2)
   END IF
  END IF
'  CASE 241'--get NPC raw
'   npcref = getnpcref(retvals(0), 0)
'   s = bound(retvals(1),0,8)
'   'scriptret = npc(npcref).rawdat(s)
'   scriptret = CPtr(integer ptr,@npc(npcref))[s]
'  CASE 242'--set NPC raw
'   npcref = getnpcref(retvals(0), 0)
'   s = bound(retvals(1),0,8)
'   CPtr(integer ptr,@npc(npcref))[s] = retvals(2)
 
END SELECT

END SUB

SUB scriptwatcher (page)

'Note: the colours here are fairly arbitrary
rectangle 0, 0, 320, 4, uilook(uiBackground), page
rectangle 0, 0, (320 / 4096) * nextscroff, 2, uilook(uiSelectedItem), page
rectangle 0, 2, (320 / 2048) * scrat(nowscript + 1, scrheap), 2, uilook(uiSelectedItem + 1), page

edgeprint " #     ID    Rtval CmdKn CmdID State", 0, 192, uilook(uiText), page
ol = 184
FOR i = large(nowscript - 23, 0) TO nowscript
 edgeprint STR$(i), 0, ol, uilook(uiText), page
 edgeprint STR$(scrat(i, scrid)), 48, ol, uilook(uiText), page
 edgeprint STR$(scrat(i, scrret)), 96, ol, uilook(uiText), page
 edgeprint STR$(scrat(i, curkind)), 144, ol, uilook(uiText), page
 edgeprint STR$(scrat(i, curvalue)), 192, ol, uilook(uiText), page
 edgeprint STR$(scrat(i, scrstate)), 240, ol, uilook(uiText), page
 ol = ol - 8
NEXT i
END SUB

SUB setdebugpan

gen(cameramode) = pancam
gen(cameraArg2) = 1
gen(cameraArg3) = 5

END SUB

SUB storekeyhandler

'DEBUG debug "store original keyhandler"
regs.ax = &H3509: CALL interruptx(&H21, regs, regs)
off9 = regs.bx: seg9 = regs.es

END SUB

SUB subdoarg
scrat(nowscript, scrdepth) = scrat(nowscript, scrdepth) + 1
pushw scrat(nowscript, scrptr)
pushw scrat(nowscript, curkind)
pushw scrat(nowscript, curvalue)
pushw scrat(nowscript, curargc)
pushw scrat(nowscript, curargn)
'--set script pointer to new offset
scrat(nowscript, scrptr) = script(scrat(nowscript, scroff) + scrat(nowscript, scrptr) + 3 + scrat(nowscript, curargn))
scrat(nowscript, scrstate) = stread '---read new statement
END SUB

SUB subreturn
scrat(nowscript, scrdepth) = scrat(nowscript, scrdepth) - 1
IF scrat(nowscript, scrdepth) < 0 THEN
 scrat(nowscript, scrstate) = stdone
ELSE
 scrat(nowscript, curargn) = popw
 scrat(nowscript, curargc) = popw
 scrat(nowscript, curvalue) = popw
 scrat(nowscript, curkind) = popw
 scrat(nowscript, scrptr) = popw
 '--push return value
 pushw scriptret
 scrat(nowscript, curargn) = scrat(nowscript, curargn) + 1
 scrat(nowscript, scrstate) = stnext'---try next arg
END IF
END SUB

SUB unwindtodo (levels)
'unwinds the stack until the specified number of dos have been stripped
'leaves the interpreter as if the last do block had successfully finished
'this means repeat in the case of for and while loops
'note: we assume the calling command has popped its args

WHILE levels > 0
 scrat(nowscript, scrdepth) = scrat(nowscript, scrdepth) - 1
 IF scrat(nowscript, scrdepth) < 0 THEN
  scrat(nowscript, scrstate) = stdone
  EXIT SUB
 END IF

 scrat(nowscript, curargn) = popw
 scrat(nowscript, curargc) = popw
 scrat(nowscript, curvalue) = popw
 scrat(nowscript, curkind) = popw
 scrat(nowscript, scrptr) = popw

 IF scrat(nowscript, curkind) = tyflow AND scrat(nowscript, curvalue) = flowdo THEN
  levels = levels - 1
  'first pop do's evaluated arguments before stopping
 END IF

 'pop arguments
 IF scrat(nowscript, curkind) = tyflow AND scrat(nowscript, curvalue) = flowswitch THEN
  'unlike all other flow, switch stack usage != argn
  dummy = popw 'state
  dummy = popw 'matching value
 ELSE
  FOR i = 1 TO scrat(nowscript, curargn)
   dummy = popw
  NEXT
 END IF
WEND
'return to normality
subreturn

END SUB

SUB templockexplain
PRINT "Either " + exename$ + " is already running in the background, or it"
PRINT "terminated incorrectly last time it was run, and was unable to clean up"
PRINT "its temporary files. The operating system is denying access to the"
PRINT "files in " + workingdir$
PRINT
PRINT "If this problem persists, manually delete playing.tmp"
PRINT
PRINT "Error code"; ERR
END SUB

SUB tweakpalette
FOR i = bound(retvals(3), 0, 255) TO bound(retvals(4), 0, 255)
 FOR j = 0 TO 2
  master(i * 3 + j) = bound(INT(master(i * 3 + j) + retvals(j)), 0, 63)
 NEXT j
NEXT i
END SUB

FUNCTION vehiclestuff (disx, disy, foep)
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
 IF vehpass(veh(20), readmapblock(disx, disy), -1) THEN
  '--dismount point is landable
  FOR i = 0 TO 15
   catx(i) = catx(0)
   caty(i) = caty(0)
   catd(i) = catd(0)
   catz(i) = 0
  NEXT i
  IF readbit(veh(), 9, 6) = 1 THEN
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
 foep = range(100, 60)
END IF
IF readbit(veh(), 6, 5) THEN '--ahead
 targx = aheadx
 targy = aheady
 untrigbit = 5
 GOSUB vehscramble
END IF
IF veh(6) = 0 THEN
 IF showsay = 0 AND readbit(gen(), 44, suspendplayer) = 0 THEN
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

vehiclestuff = result

EXIT FUNCTION

vehscramble:
tmp = 0
FOR i = 0 TO 3
 IF hero(i) <= 0 THEN
  tmp = tmp + 1
 ELSE
  IF ABS(catx(i * 5) - targx) < large(herospeed(i), 4) THEN
   catx(i * 5) = targx
   xgo(i) = 0
   ygo(i) = 0
  END IF
  IF ABS(caty(i * 5) - targy) < large(herospeed(i), 4) THEN
   caty(i * 5) = targy
   xgo(i) = 0
   ygo(i) = 0
  END IF
  IF ABS(targx - catx(i * 5)) > 0 AND xgo(i) = 0 THEN
   xgo(i) = 20 * SGN(catx(i * 5) - targx)
  END IF
  IF ABS(targy - caty(i * 5)) > 0 AND ygo(i) = 0 THEN
   ygo(i) = 20 * SGN(caty(i * 5) - targy)
  END IF
  IF catx(i * 5) - targx = 0 AND caty(i * 5) - targy = 0 THEN tmp = tmp + 1
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
RETURN

END FUNCTION

FUNCTION vehpass (n, tile, default)

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
o = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  getpal16 pal16(), o, stat(i, 1, 15)
  setpicstuf buffer(), 1600, 2
  loadset game$ + ".pt4" + CHR$(0), stat(i, 1, 14), 5 * o
  o = o + 1
 END IF
NEXT i
FOR i = o TO 3
 '--black out unused heros
 rectangle 0, i * 5, 320, 5, uilook(uiBackground), 2
NEXT i
END SUB

SUB wrapaheadxy (x, y, direction, distance, unitsize)
'alters X and Y ahead by distance in direction, wrapping if neccisary
'unitsize is 1 for pixels, 20 for tiles

aheadxy x, y, direction, distance

IF gmap(5) THEN
 wrapxy x, y, scroll(0) * unitsize, scroll(1) * unitsize
END IF

END SUB

FUNCTION wrappass (x, y, xgo, ygo, isveh)
' returns true if blocked by terrain
DIM pd(3)

wrappass = 0

tilex = x: tiley = y
p = readmapblock(tilex, tiley)

FOR i = 0 TO 3
 tilex = x: tiley = y
 wrapaheadxy tilex, tiley, i, 1, 1
 pd(i) = readmapblock(tilex, tiley)
NEXT i

IF ygo > 0 AND movdivis(ygo) AND ((p AND 1) = 1 OR (pd(0) AND 4) = 4 OR (isveh AND vehpass(veh(18), pd(0), 0))) THEN ygo = 0: wrappass = 1
IF ygo < 0 AND movdivis(ygo) AND ((p AND 4) = 4 OR (pd(2) AND 1) = 1 OR (isveh AND vehpass(veh(18), pd(2), 0))) THEN ygo = 0: wrappass = 1
IF xgo > 0 AND movdivis(xgo) AND ((p AND 8) = 8 OR (pd(3) AND 2) = 2 OR (isveh AND vehpass(veh(18), pd(3), 0))) THEN xgo = 0: wrappass = 1
IF xgo < 0 AND movdivis(xgo) AND ((p AND 2) = 2 OR (pd(1) AND 8) = 8 OR (isveh AND vehpass(veh(18), pd(1), 0))) THEN xgo = 0: wrappass = 1

END FUNCTION


FUNCTION wrapcollision (xa, ya, xgoa, ygoa, xb, yb, xgob, ygob)
 x1 = (xa - bound(xgoa, -20, 20)) \ 20
 x2 = (xb - bound(xgob, -20, 20)) \ 20
 y1 = (ya - bound(ygoa, -20, 20)) \ 20
 y2 = (yb - bound(ygob, -20, 20)) \ 20

 IF gmap(5) THEN
  wrapcollision = (x1 - x2) MOD scroll(0) = 0 AND (y1 - y2) MOD scroll(1) = 0
 ELSE
  wrapcollision = (x1 = x2) AND (y1 = y2)
 END IF

END FUNCTION

FUNCTION wraptouch (x1, y1, x2, y2, distance)
 'whether 2 walkabouts are within distance pixels horizontally + vertically
 wraptouch = 0
 IF gmap(5) THEN
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

SUB wrapxy (x, y, wide, high)
'--wraps the given X and Y values within the bounds of width and height
IF x < 0 THEN x = wide + x
IF x >= wide THEN x = x - wide
IF y < 0 THEN y = high + y
IF y >= high THEN y = y - high
END SUB
