'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION playtime$ (d%, h%, m%)
DECLARE FUNCTION functiondone% ()
DECLARE FUNCTION functionread% ()
DECLARE FUNCTION averagelev% (stat%(), hero%())
DECLARE FUNCTION countitem% (it%, item%(), item$())
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION movdivis% (xygo%)
DECLARE FUNCTION onwho% (w$, hero%(), a%(), dpage%, vpage%, timing%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), mapx%, x%, mapy%, y%, pal%())
DECLARE SUB minimap (vpage%, dpage%, scroll%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), mx%, my%, timing%(), gmap%(), x%, y%, tastuf%())
DECLARE SUB sellstuff (id%, a%(), vpage%, dpage%, timing%(), stock%(), gold&, item%(), item$(), tag%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), pal%(), hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(), eqstuf%(),  _
itembits%())
DECLARE SUB buystuff (id%, shoptype%, a%(), vpage%, dpage%, timing%(), stock%(), gold&, item%(), item$(), tag%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), pal%(), hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(),  _
eqstuf%(), herobits%(), itembits%())
DECLARE SUB heroswap (all%, hmask%(), hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(), eqstuf%(), timing%(), carray%(), vpage%, dpage%, csetup%(), gotm%, gotj%(), mouse%(), joy%(), pal%())
DECLARE FUNCTION shoption (inn%, price%, timing%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), gold&, needf%, hero%(), name$(), stat%(), vpage%, dpage%)
DECLARE SUB savegame (slot%, sourcerpg$, map%, x%(), y%(), d%(), foep%, leader%, mapx%, mapy%, gold&, npcl%(), tag%(), hero%(), a%(), stat%(), bmenu%(), spell%(), lmp%(), exlev&(), name$(), item%(), item$(), eqstuf%(), stock%(), hmask%(), global%()) _

DECLARE FUNCTION runscript% (n%, index%, newcall%, script%(), heap%(), scrat%())
DECLARE SUB scripterr (e$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE SUB itstr (i%, item%(), item$())
DECLARE FUNCTION findhero% (who%, hero%(), f%, l%, d%)
DECLARE FUNCTION howmanyh% (hero%(), f%, l%)
DECLARE FUNCTION consumeitem% (index%, item%(), item$())
DECLARE FUNCTION istag% (tag%(), num%, zero%)
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION usemenu% (ptr%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
DECLARE FUNCTION browse$ (fmask$, needf%, dpage%, vpage%, bpage%, timing%(), tmp$)
DECLARE SUB reloadhwp (hero%())
DECLARE SUB doswap (s%, d%, hmask%(), hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(), eqstuf%())
DECLARE SUB control (carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), herox%, heroy%, mmode%, timing%())
DECLARE FUNCTION pickload% (svcsr%, pal%(), timing%(), dpage%, vpage%, carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), sourcerpg$)
DECLARE FUNCTION picksave% (svcsr%, pal%(), timing%(), dpage%, vpage%, carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), sourcerpg$)
DECLARE SUB equip (ptr%, hero%(), stat%(), name$(), timing%(), vpage%, dpage%, item%(), item$(), eqstuf%(), bmenu%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), tag%(), itembits%())
DECLARE FUNCTION items% (item%(), item$(), hero%(), stat%(), name$(), timing%(), vpage%, dpage%, bmenu%(), spell%(), pal%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB getitem (getit%, item%(), item$())
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%(), hero%())
DECLARE SUB spells (ptr%, hero%(), stat%(), name$(), timing%(), vpage%, dpage%, bmenu%(), lmp%(), spell%(), pal%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB status (ptr%, hero%(), stat%(), name$(), exlev&(), gold&, timing%(), vpage%, dpage%, bmenu%(), lmp%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB getnames (stat$())
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
DECLARE SUB resetlmp (lmp%(), slot%, lev%)
DECLARE SUB loadfoe (i%, a%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), name$(), stat%(), ebits%())
DECLARE SUB inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%())
DECLARE SUB battle (form%, fatal%, hero%(), pal%(), timing%(), exstat%(), bmenu%(), spell%(), lmp%(), gold&, exlev&(), item%(), item$(), eqstuf%(), fmvol%, carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB addhero (who%, slot%, hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(), eqstuf%())
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION xstring% (s$, x%)
DECLARE SUB snapshot (vpage%)
'assembly subs and functions
DECLARE SUB setmodex ()
DECLARE SUB restoremode ()
DECLARE SUB setpicstuf (buf(), BYVAL b, BYVAL p)
DECLARE SUB loadset (fil$, BYVAL i, BYVAL l)
DECLARE SUB storeset (fil$, BYVAL i, BYVAL l)
DECLARE SUB copypage (BYVAL page1, BYVAL page2)
DECLARE SUB setvispage (BYVAL page)
DECLARE SUB drawsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB wardsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB getsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB loadsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB stosprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB setdiskpages (buf(), BYVAL h, BYVAL l)
DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
DECLARE SUB fadeto (palbuff(), BYVAL red, BYVAL green, BYVAL blue)
DECLARE SUB fadetopal (pal(), palbuff())
DECLARE SUB setpal (pal())
DECLARE SUB clearpage (BYVAL page)
DECLARE SUB setkeys ()
DECLARE SUB setfont (f())
DECLARE SUB printstr (s$, BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB textcolor (BYVAL f, BYVAL b)
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
'DECLARE SUB setitup (fil$, buff(), tbuff(), BYVAL p)
'DECLARE FUNCTION resetdsp
'DECLARE SUB playsnd (BYVAL n, BYVAL f)
'DECLARE SUB closefile
DECLARE SUB rectangle (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB fuzzyrect (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()
DECLARE SUB setmapdata (array(), pas(), BYVAL t, BYVAL b)
DECLARE SUB setmapblock (BYVAL x, BYVAL y, BYVAL v)
DECLARE FUNCTION readmapblock (BYVAL x, BYVAL y)
DECLARE SUB drawmap (BYVAL x, BYVAL y, BYVAL t, BYVAL p)
DECLARE SUB putpixel (BYVAL x, BYVAL y, BYVAL c, BYVAL p)
DECLARE FUNCTION readpixel (BYVAL x, BYVAL y, BYVAL p)
DECLARE FUNCTION Keyseg ()
DECLARE FUNCTION keyoff ()
DECLARE FUNCTION keyval (BYVAL a)
DECLARE FUNCTION getkey ()
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
'DECLARE SUB lumpfiles (listf$, lump$, path$, buffer())
DECLARE SUB unlump (lump$, ulpath$, buffer())
DECLARE SUB unlumpfile (lump$, fmask$, path$, buf())
DECLARE SUB copyfile (s$, d$, buf())
DECLARE FUNCTION isfile (n$)
DECLARE SUB setupmusic (mbuf())
DECLARE SUB closemusic ()
DECLARE SUB stopsong ()
DECLARE SUB resumesong ()
DECLARE SUB resetfm ()
DECLARE SUB loadsong (f$)
DECLARE SUB fademusic (BYVAL vol)
DECLARE FUNCTION getfmvol ()
DECLARE SUB setfmvol (BYVAL vol)
DECLARE SUB screenshot (f$, BYVAL p, maspal(), buf())
DECLARE FUNCTION readjoy (joybuf(), BYVAL jnum)
'DECLARE FUNCTION setmouse (mbuf())
'DECLARE SUB readmouse (mbuf())
'DECLARE SUB movemouse (BYVAL x, BYVAL y)
DECLARE SUB getstring (p$)
DECLARE FUNCTION envlength (e$)
DECLARE FUNCTION pathlength ()
DECLARE FUNCTION rpathlength ()
DECLARE FUNCTION drivelist (d())
DECLARE SUB setdrive (BYVAL n)
DECLARE FUNCTION isdir (dir$)
DECLARE FUNCTION isremovable (BYVAL d)
DECLARE FUNCTION isvirtual (BYVAL d)
DECLARE FUNCTION hasmedia (BYVAL d)
DECLARE SUB array2str (arr(), BYVAL o, s$)
DECLARE SUB str2array (s$, arr(), BYVAL o)
DECLARE SUB pushw (BYVAL word)
DECLARE FUNCTION popw ()

COMMON SHARED /trueglobals/ game$, buffer(), master(), gen()
COMMON SHARED /scriptarrays/ script(), heap(), global(), stack(), scrat(), retvals(), nowscript, scriptret

'---SCRIPT ALLOCATION CONSTANTS---
CONST scroff = 0     'position of the script in the buffer
CONST scrheap = 1    'position of the script's local vars in the buffer
CONST scrstate = 2   'what the script is doing right now
CONST scrptr = 3     'the execution pointer
CONST scrret = 5     'the scripts current return value
CONST curkind = 6    'kind of current statement
CONST curvalue = 7   'value of current stament
CONST curargc = 8    'number of args for current statement
CONST curargn = 9    'current arg for current statement
CONST scrdepth = 10  'stack depth of current script
CONST scrid = 11     'id number current script
'---INTERPRETER STATES---
'suspended scripts have negative states
CONST stnone = 0
CONST stwait = 1
CONST stread = 2
CONST streturn = 3
CONST stnext = 4
CONST stdoarg = 5
CONST stdone = 6
'--SCRIPT STATEMENT TYPES---
CONST tystop = 0    '0 terminate script
CONST tynumber = 1  '1 literal number
CONST tyflow = 2    '2 flow control
CONST tyglobal = 3  '3 global variable
CONST tylocal = 4   '4 local variable
CONST tymath = 5    '5 math function
CONST tyfunct = 6   '6 function call
CONST tyscript = 7  '7 script call
'--FLOW CONTROL TYPES---
CONST flowdo = 0
CONST flowif = 4
CONST flowthen = 5
CONST flowelse = 6
CONST flowfor = 7
CONST flowwhile = 10
'--SUSPEND--
CONST suspendnpcs = 0
CONST suspendplayer = 1
CONST suspendobstruction = 2
CONST suspendherowalls = 3
CONST suspendnpcwalls = 4
CONST suspendcatapillar = 5
CONST suspendrandomenemys = 6
'--CAMERA CONSTANTS--
CONST cameramode = 45
CONST cameraArg = 46
CONST cameraArg2 = 47
CONST cameraArg3 = 48
CONST cameraArg4 = 49
CONST herocam = 0
CONST npccam = 1
CONST pancam = 2
CONST focuscam = 3
CONST stopcam = -1

REM $STATIC
FUNCTION functiondone
'returns false normally, or true to break the DO
  functiondone = 0
  nowscript = nowscript - 1
  IF nowscript < 0 THEN
   functiondone = -1'--no scripts are running anymore
   EXIT FUNCTION
  ELSE
   IF scrat(nowscript, scrstate) < 0 THEN
    '--suspended script is resumed
    scrat(nowscript, scrstate) = ABS(scrat(nowscript, scrstate))
   ELSE
    scriptret = scrat(nowscript + 1, scrret)
    scrat(nowscript, scrstate) = streturn'---return
   END IF
  END IF
END FUNCTION

FUNCTION functionread
'functionread (scrat(), script(), nowscript, scriptret, global(), heap())
'returns false normally, true when it should terminate
  functionread = 0
  scriptret = 0'--default returnvalue is zero
  scrat(nowscript, curkind) = script(scrat(nowscript, scroff) + scrat(nowscript, scrptr))
  scrat(nowscript, curvalue) = script(scrat(nowscript, scroff) + scrat(nowscript, scrptr) + 1)
  scrat(nowscript, curargc) = script(scrat(nowscript, scroff) + scrat(nowscript, scrptr) + 2)
  scrat(nowscript, curargn) = 0
  SELECT CASE scrat(nowscript, curkind)
  CASE tystop
   scripterr "interpretloop encountered noop": nowscript = -1: functionread = -1: EXIT FUNCTION
  CASE tynumber
   scriptret = scrat(nowscript, curvalue)
   scrat(nowscript, scrstate) = streturn'---return
  CASE tyglobal
   scriptret = global(scrat(nowscript, curvalue))
   scrat(nowscript, scrstate) = streturn'---return
  CASE tylocal
   scriptret = heap(scrat(nowscript, scrheap) + scrat(nowscript, curvalue))'--get from heap
   scrat(nowscript, scrstate) = streturn'---return
  '--flow control would be a special case
  CASE tymath, tyfunct, tyscript, tyflow
   scrat(nowscript, scrstate) = stnext '---function
  CASE ELSE
   scripterr "Illegal statement type"
  END SELECT
END FUNCTION

FUNCTION playtime$ (d, h, m)
 s$ = ""

 SELECT CASE d
  CASE 1
   s$ = s$ + LTRIM$(STR$(d)) + " day "
  CASE IS > 1
   s$ = s$ + LTRIM$(STR$(d)) + " days "
 END SELECT

 SELECT CASE h
  CASE 1
   s$ = s$ + LTRIM$(STR$(h)) + " hour "
  CASE IS > 1
   s$ = s$ + LTRIM$(STR$(h)) + " hours "
 END SELECT

 SELECT CASE m
  CASE 1
   s$ = s$ + LTRIM$(STR$(m)) + " minute"
  CASE IS > 1
   s$ = s$ + LTRIM$(STR$(m)) + " minutes"
 END SELECT

 playtime$ = s$

END FUNCTION

SUB playtimer
STATIC n!

IF TIMER >= n! + 10 THEN
 n! = TIMER
 gen(54) = gen(54) + 10
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
   'debug "subreturn:" + STR$(scrat(nowscript, curargn)) + STR$(scrat(nowscript, curargc)) + STR$(scrat(nowscript, curvalue)) + STR$(scrat(nowscript, curkind)) + STR$(scrat(nowscript, scrptr))
   '--push return value
   pushw scriptret
   scrat(nowscript, curargn) = scrat(nowscript, curargn) + 1
   scrat(nowscript, scrstate) = stnext'---try next arg
  END IF
END SUB

