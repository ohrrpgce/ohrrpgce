'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB playtimer ()
DECLARE FUNCTION functiondone% ()
DECLARE FUNCTION functionread% ()
DECLARE SUB subreturn ()
DECLARE SUB subdoarg ()
DECLARE SUB resetgame (map%, x%, y%, d%, foep%, leader%, mapx%, mapy%, gold&, npcl%(), tag%(), hero%(), a%(), stat%(), bmenu%(), spell%(), lmp%(), exlev&(), name$(), item%(), item$(), eqstuf%(), stock%(), hmask%(), showsay%, scrat%(), global%(),  _
nowscript%, scriptout$)
DECLARE FUNCTION countitem% (it%, item%(), item$())
DECLARE SUB scriptmath (scrat%(), nowscript%, scriptret%, retvals%(), heap%(), global%())
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE SUB fatalerror (e$)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE FUNCTION movdivis% (xygo%)
DECLARE SUB scripterr (e$)
DECLARE SUB calibrate (gotj%(), joy%(), dpage%, vpage%, timing%())
DECLARE FUNCTION runscript% (n%, index%, newcall%, script%(), heap%(), scrat%())
DECLARE SUB getmapname (mapname$, m%)
DECLARE FUNCTION istag% (tag%(), num%, zero%)
DECLARE SUB evalitemtag (tag%(), itembits%(), hero%(), eqstuf%(), item%())
DECLARE SUB evalherotag (tag%(), herobits%(), hero%(), stat%(), leader%)
DECLARE SUB tagdisplay (tag%(), dpage%)
DECLARE SUB rpgversion (v%)
DECLARE FUNCTION browse$ (fmask$, needf%, dpage%, vpage%, bpage%, timing%(), tmp$)
DECLARE SUB cycletile (cycle%(), tastuf%(), ptr%(), skip%(), tag%())
DECLARE SUB loadtanim (n%, tastuf%())
DECLARE SUB loaddoor (map%, door%())
DECLARE SUB reinitnpc (remember%, npcl%(), map%, filenum$())
DECLARE FUNCTION findhero% (who%, hero%(), f%, l%, d%)
DECLARE SUB doswap (s%, d%, hmask%(), hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(), eqstuf%())
DECLARE FUNCTION howmanyh% (hero%(), f%, l%)
DECLARE SUB reloadhwp (hero%())
DECLARE SUB heroswap (all%, hmask%(), hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(), eqstuf%(), timing%(), carray%(), vpage%, dpage%, csetup%(), gotm%, gotj%(), mouse%(), joy%(), pal%())
DECLARE SUB patcharray (array%(), n$, max%, timing%(), vpage%, dpage%)
DECLARE SUB debug (s$)
DECLARE SUB drawsay (saybit%(), sayenh%(), dpage%, say$(), showsay%, choose$(), choosep%)
DECLARE SUB shop (id%, timing%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), gold&, vpage%, dpage%, needf%, hero%(), name$(), stock%(), tag%(), item%(), item$(), pal%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), eqstuf%(), svcsr%,  _
map%, x%(), y%(), d%(), foep%, leader%, mapx%, mapy%, aa%(), npcl%(), mx%, my%, scroll%(), gmap%(), hmask%(), tastuf%(), sourcerpg$, herobits%(), itembits%(), nowscript%, script%(), heap%(), scrat%(), global%())
DECLARE SUB minimap (vpage%, dpage%, scroll%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), mx%, my%, timing%(), gmap%(), x%, y%, tastuf%())
DECLARE FUNCTION onwho% (w$, hero%(), a%(), dpage%, vpage%, timing%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), mapx%, x%, mapy%, y%, pal%())
DECLARE SUB sellstuff (id%, a%(), vpage%, dpage%, timing%(), stock%(), gold&, item%(), item$(), tag%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), pal%(), hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(), eqstuf%(),  _
itembits%())
DECLARE SUB buystuff (id%, shoptype%, a%(), vpage%, dpage%, timing%(), stock%(), gold&, item%(), item$(), tag%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), pal%(), hero%(), bmenu%(), spell%(), stat%(), lmp%(), exlev&(), name$(),  _
eqstuf%(), herobits%(), itembits%())
DECLARE FUNCTION shoption (inn%, price%, timing%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), gold&, needf%, hero%(), name$(), stat%(), vpage%, dpage%)
DECLARE SUB itstr (i%, item%(), item$())
DECLARE SUB control (carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), herox%, heroy%, mmode%, timing%())
DECLARE FUNCTION pickload% (svcsr%, pal%(), timing%(), dpage%, vpage%, carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), sourcerpg$)
DECLARE FUNCTION picksave% (svcsr%, pal%(), timing%(), dpage%, vpage%, carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), sourcerpg$)
DECLARE SUB savegame (slot%, sourcerpg$, map%, x%(), y%(), d%(), foep%, leader%, mapx%, mapy%, gold&, npcl%(), tag%(), hero%(), a%(), stat%(), bmenu%(), spell%(), lmp%(), exlev&(), name$(), item%(), item$(), eqstuf%(), stock(), hmask(), global%())
DECLARE SUB loadgame (slot%, sourcerpg$, map%, x%(), y%(), d%(), foep%, leader%, mapx%, mapy%, gold&, npcl%(), tag%(), hero%(), a%(), stat%(), bmenu%(), spell%(), lmp%(), exlev&(), name$(), item%(), item$(), eqstuf%(), stock(), hmask(), global%())
DECLARE SUB equip (ptr%, hero%(), stat%(), name$(), timing%(), vpage%, dpage%, item%(), item$(), eqstuf%(), bmenu%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%(), tag%(), itembits%())
DECLARE FUNCTION items% (item%(), item$(), hero%(), stat%(), name$(), timing%(), vpage%, dpage%, bmenu%(), spell%(), pal%(), carray%(), csetup%(), gotm%, gotj%(), mouse%(), joy%())
DECLARE SUB getitem (getit%, item%(), item$())
DECLARE SUB delitem (it%, item%(), item$())
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
DECLARE SUB setanim (BYVAL cycle1, BYVAL cycle2)
DECLARE SUB drawmap (BYVAL x, BYVAL y, BYVAL t, BYVAL p)
DECLARE SUB setoutside (BYVAL defaulttile)
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
DECLARE SUB array2str (arr(), BYVAL o, s$)
DECLARE SUB str2array (s$, arr(), BYVAL o)

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
'---VIRTUAL STACK---
DECLARE SUB setupstack (buffer(), BYVAL size, file$)
DECLARE SUB releasestack ()
DECLARE SUB pushw (BYVAL word)
DECLARE FUNCTION popw ()
DECLARE FUNCTION stackpos ()

COMMON SHARED /trueglobals/ game$, buffer(), master(), gen()
COMMON /scriptarrays/ script(), heap(), global(), stack(), scrat(), retvals(), nowscript, scriptret

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
CONST flowreturn = 3
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
CONST suspendboxadvance = 7
CONST suspendoverlay = 8
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

'---WRITE COMMAND-LINE ARGS TO A TEMP FILE---
tmp$ = STRING$(envlength("TEMP"), 0): getstring tmp$
IF RIGHT$(tmp$, 1) <> "\" THEN tmp$ = tmp$ + "\"
OPEN tmp$ + "cline.___" FOR OUTPUT AS #1
 WRITE #1, COMMAND$
CLOSE #1

thestart:
CLEAR , , 2000
TYPE Regtype
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE
DIM SHARED regs AS Regtype
regs.ax = &H3509: CALL interruptx(&H21, regs, regs)
off9 = regs.bx: seg9 = regs.es

DIM font(1024), master(767), buffer(16384), pal(1584), timing(4), mouse(4), joy(14), music(32767)
DIM filenum$(99), door(300), link(1000), gen(500), npcl(2100), npcs(1500), saytag(21), tag(127), hero(40), a(500), stat(40, 1, 13), bmenu(40, 5), spell(40, 3, 24), lmp(40, 7), foef(99), menu$(20), exlev&(40, 1), name$(40), mi(10), gotj(2)
DIM item(-3 TO 199), item$(-3 TO 199), eqstuf(40, 4), gmap(20), csetup(20), carray(20), stock(99, 49), choose$(1), chtag(1), saybit(0), sayenh(6), zbuf(3), catx(15), caty(15), catz(15), catd(15), catp(3), xgo(3), ygo(3), herospeed(3), wtog(3), say$( _
7), hmask(3), tastuf(40), cycle(1), cycptr(1), cycskip(1), herobits(59, 3), itembits(255, 4)
DIM scroll(16002), pass(16002), emap(16002), mapname$
DIM script(2048), heap(2048), global(1024), stack(1024), scrat(128, 11), retvals(32)


'---GET CURRENT DIRECTORY AND PROGRAM DIRECTORY---
curdir$ = STRING$(pathlength, 0): getstring curdir$
IF RIGHT$(curdir$, 1) <> "\" THEN curdir$ = curdir$ + "\"
progdir$ = STRING$(rpathlength, 0): getstring progdir$
IF RIGHT$(progdir$, 1) <> "\" THEN progdir$ = progdir$ + "\"

'---RELOAD COMMAND LINE FROM TEMP FILE---
tmp$ = STRING$(envlength("TEMP"), 0): getstring tmp$
IF RIGHT$(tmp$, 1) <> "\" THEN tmp$ = tmp$ + "\"
OPEN tmp$ + "cline.___" FOR INPUT AS #1
 INPUT #1, cline$
CLOSE #1
IF LCASE$(cline$) <> "/keyonly" AND LCASE$(cline$) <> "-keyonly" THEN
' gotm = setmouse(mouse())
 FOR i = 0 TO 1
  gotj(i) = readjoy(joy(), i)
 NEXT i
END IF

dpage = 1: vpage = 0: rate = 160

RANDOMIZE TIMER
setmodex
setdiskpages buffer(), 200, 0
fadetopal master(), buffer()
xbload progdir$ + "ohrrpgce.mas", master(), "could not find master palette file " + progdir$ + "ohrrpgce.mas"
xbload progdir$ + "ohrrpgce.fnt", font(), "could not find default font file " + progdir$ + "ohrrpgce.fnt"
setfont font()
GOSUB switchon
textcolor 15, 0
'voices = resetdsp
setupmusic music()
resetfm
'setfmvol 7
fmvol = getfmvol
setfmvol 0

'---If PLAYING.TMP does not already exist, it must be created---
IF NOT isdir(progdir$ + "playing.tmp" + CHR$(0)) THEN MKDIR progdir$ + "playing.tmp"

'---SET UP DEFAULT CONTROLS---
GOSUB defaultc

'---IF A VALID RPG FILE WAS SPECIFIED ON THE COMMAND LINE, RUN IT, ELSE BROWSE---
a$ = cline$
IF MID$(a$, 2, 1) <> ":" THEN a$ = curdir$ + a$
IF RIGHT$(a$, 4) = ".RPG" AND isfile(a$ + CHR$(0)) THEN
 sourcerpg$ = a$
ELSE
 sourcerpg$ = browse$("*.rpg", 1, dpage, vpage, 2, timing(), tmp$)
 IF sourcerpg$ = "" THEN GOTO finis
END IF

rectangle 4, 3, 312, 14, 9, vpage
rectangle 5, 4, 310, 12, 1, vpage
edgeprint "Loading...", xstring("Loading...", 160), 6, 14, vpage

'---GAME SELECTED, PREPARING TO PLAY---
ERASE scroll, pass, emap
DIM lumpbuf(32767)
unlump sourcerpg$ + CHR$(0), "playing.tmp\", lumpbuf()
ERASE lumpbuf
DIM scroll(16002), pass(16002), emap(16002)

a$ = ""
i = 0
DO UNTIL LEFT$(a$, 1) = "\"
 i = i + 1
 a$ = RIGHT$(sourcerpg$, i)
LOOP
a$ = LEFT$(a$, LEN(a$) - 4)
game$ = "playing.tmp" + a$

fadeto buffer(), 0, 0, 0
needf = 1

xbload game$ + ".mas", master(), "master palette missing from " + game$
xbload game$ + ".fnt", font(), "font missing from " + game$
xbload game$ + ".pal", pal(), "16-color palletes missing from " + game$
xbload game$ + ".gen", gen(), "general data missing from " + game$

rpgversion gen(95)

setfont font()
setpicstuf buffer(), 50, -1
FOR i = 0 TO 99
 loadset game$ + ".efs" + CHR$(0), i, 0
 foef(i) = buffer(0)
NEXT i
j = 0
FOR i = 0 TO 9
 FOR o = 0 TO 9
  filenum$(j) = RIGHT$(STR$(i), LEN(STR$(i)) - 1) + RIGHT$(STR$(o), LEN(STR$(o)) - 1)
  j = j + 1
 NEXT
NEXT

beginplay:

item(-3) = 1: item$(-3) = "DONE       "
item(-2) = 1: item$(-2) = "AUTOSORT   "
item(-1) = 1: item$(-1) = "TRASH      "
FOR i = 0 TO 199: item$(i) = "           ": NEXT i

leader = 0: gold& = gen(96): svcsr = 0: fatal = 0: abortg = 0
addhero 1, 0, hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
'addhero 6, 1, hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
'addhero 7, 2, hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
'addhero 4, 3, hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()

FOR i = 0 TO 3
 herospeed(i) = 4
NEXT i
FOR i = 0 TO 15
 catx(i) = gen(102) * 20
 caty(i) = gen(103) * 20
 catd(i) = 2
NEXT i
map = gen(104)
foep = range(100, 60)

nowscript = -1
depth = 0
releasestack
setupstack stack(), 2048, tmp$ + "stack.tmp" + CHR$(0)

GOSUB titlescr
temp = pickload(svcsr, pal(), timing(), dpage, vpage, carray(), csetup(), gotm, gotj(), mouse(), joy(), sourcerpg$)
fademusic 0
IF temp = -2 THEN GOTO resetg
IF temp >= 0 THEN
 loadgame temp, sourcerpg$, map, catx(), caty(), catd(), foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), a(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), global()
ELSE
 IF gen(41) > 0 THEN
  IF runscript(gen(41), nowscript + 1, -1, script(), heap(), scrat()) THEN
   nowscript = nowscript + 1
  ELSE
   scripterr "failed to load script" + STR$(gen(41))
  END IF
 END IF
END IF
fadeto buffer(), 0, 0, 0
stopsong
ERASE scroll, pass, emap
GOSUB preparemap
GOSUB doihavebits
needf = 1: ng = 1

GOSUB movement
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
GOSUB interpret
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx - catx(0), mapy - caty(0), 0, timing()
IF keyval(73) > 0 AND keyval(81) > 0 THEN
 IF keyval(1) > 1 THEN GOTO finis
 IF keyval(2) > 1 THEN patcharray gen(), "gen", 499, timing(), vpage, dpage
 IF keyval(3) > 1 THEN patcharray hmask(), "hmask", 3, timing(), vpage, dpage
END IF
IF carray(5) > 1 AND showsay = 0 AND readbit(gen(), 44, suspendplayer) = 0 THEN
 GOSUB usermenu
 evalherotag tag(), herobits(), hero(), stat(), leader
 evalitemtag tag(), itembits(), hero(), eqstuf(), item()
 reinitnpc 1, npcl(), map, filenum$()
 GOSUB npcplot
END IF
IF carray(4) > 1 AND showsay = 1 AND readbit(gen(), 44, suspendboxadvance) = 0 THEN
 GOSUB nextsay
END IF
IF showsay = 0 AND readbit(gen(), 44, suspendplayer) = 0 THEN
 IF carray(0) > 0 AND xgo(0) = 0 AND ygo(0) = 0 THEN ygo(0) = 20: catd(0) = 0
 IF carray(1) > 0 AND xgo(0) = 0 AND ygo(0) = 0 THEN ygo(0) = -20: catd(0) = 2
 IF carray(2) > 0 AND xgo(0) = 0 AND ygo(0) = 0 THEN xgo(0) = 20: catd(0) = 3
 IF carray(3) > 0 AND xgo(0) = 0 AND ygo(0) = 0 THEN xgo(0) = -20: catd(0) = 1
 IF carray(4) > 1 AND xgo(0) = 0 AND ygo(0) = 0 THEN
  auto = 0
  GOSUB usething
 END IF
END IF
IF showsay = 1 THEN
 IF carray(0) > 1 THEN choosep = 0
 IF carray(1) > 1 THEN choosep = 1
END IF
setmapdata pass(), pass(), 0, 0
GOSUB movement
GOSUB movenpc
IF keyval(59) > 1 AND showsay = 0 THEN minimap vpage, dpage, scroll(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mx, my, timing(), gmap(), catx(0), caty(0), tastuf()
IF keyval(60) > 1 AND showsay = 0 THEN
 savegame 4, sourcerpg$, map, catx(), caty(), catd(), foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), a(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), global()
END IF
IF keyval(61) > 1 AND showsay = 0 THEN
 loadgame 4, sourcerpg$, map, catx(), caty(), catd(), foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), a(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), global()
 ERASE scroll, pass, emap
 GOSUB preparemap
END IF
IF keyval(62) > 1 THEN showtags = showtags XOR 1
IF keyval(63) > 1 THEN
 SELECT CASE gen(cameramode)
 CASE herocam
  IF gen(cameraArg) < 15 THEN
   gen(cameraArg) = gen(cameraArg) + 5
  ELSE
   gen(cameraArg) = 0
  END IF
 CASE ELSE
  gen(cameramode) = herocam
  gen(cameraArg) = 0
 END SELECT
END IF
IF keyval(64) > 1 THEN
 gen(cameramode) = focuscam
 gen(cameraArg) = (scroll(0) * 10) - 150
 gen(cameraArg2) = (scroll(1) * 10) - 100
 gen(cameraArg3) = 5
 gen(cameraArg4) = 5
END IF
IF keyval(87) > 1 THEN ghost = ghost XOR 1
IF keyval(88) > 1 THEN snapshot vpage
IF foep = 0 AND readbit(gen(), 44, suspendrandomenemys) = 0 THEN
 setmapdata emap(), pass(), 0, 0
 temp = readmapblock(INT(catx(0) / 20), INT(caty(0) / 20))
 IF temp > 0 THEN
  setpicstuf buffer(), 50, -1
  loadset game$ + ".efs" + CHR$(0), temp - 1, 0
  FOR i = 0 TO INT(RND * 20)
   foenext = loopvar(foenext, 0, 19, 1)
   WHILE buffer(1 + foenext) = 0: foenext = loopvar(foenext, 0, 19, 1): WEND
  NEXT i
  fatal = 0
  ERASE scroll, pass, emap
  battle buffer(1 + foenext) - 1, fatal, hero(), pal(), timing(), stat(), bmenu(), spell(), lmp(), gold&, exlev&(), item(), item$(), eqstuf(), fmvol, carray(), csetup(), gotm, gotj(), mouse(), joy()
  oldsong = 0
  afterbat = 1
  GOSUB preparemap: foep = range(100, 60): needf = 1
 END IF
END IF
IF fatal = 1 THEN
 '--this is what happens when you die in battle
 IF gen(42) > 0 THEN
  IF runscript(gen(42), nowscript + 1, -1, script(), heap(), scrat()) THEN
   nowscript = nowscript + 1
   fatal = 0
   needf = 2
  ELSE
   scripterr "failed to load script" + STR$(gen(42))
  END IF
 ELSE
  fadeto buffer(), 63, 0, 0
 END IF
END IF
IF sayenh(4) = 0 AND gen(50) = 0 THEN
 '---NORMAL DISPLAY---
 setmapdata scroll(), pass(), 1, 0
 setanim tastuf(0) + cycle(0), tastuf(20) + cycle(1)
 cycletile cycle(), tastuf(), cycptr(), cycskip(), tag()
 drawmap mapx, mapy, 0, dpage
 GOSUB drawnpc
 '---HIDE THE UNSIGHTLY LINE---
 rectangle 0, 0, 320, 1, 0, dpage
 'rectangle 0, 0, 20, 200, 0, dpage
 'rectangle 300, 0, 20, 200, 0, dpage
 GOSUB cathero
 IF readbit(gen(), 44, suspendoverlay) = 0 THEN drawmap mapx, mapy, 2, dpage
ELSE '---END NORMAL DISPLAY---
 copypage 3, dpage
END IF '---END BACKDROP DISPLAY---
IF showsay > 0 THEN drawsay saybit(), sayenh(), dpage, say$(), showsay, choose$(), choosep
IF showmapname > 0 THEN showmapname = showmapname - 1: edgeprint mapname$, xstring(mapname$, 160), 180, 15, dpage
'scriptout$ = STR$(gen(51)) + " days" + STR$(gen(52)) + " hours" + STR$(gen(53)) + " minutes" + STR$(gen(54)) + " seconds"
edgeprint scriptout$, 0, 190, 15, dpage
IF showtags > 0 THEN tagdisplay tag(), dpage
IF fatal = 1 OR abortg = 1 THEN
 resetgame map, catx(0), caty(0), catd(0), foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), a(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), showsay, scrat(), global(), nowscript,  _
scriptout$
 GOTO beginplay
END IF
SWAP vpage, dpage
setvispage vpage
IF needf = 1 AND fatal = 0 THEN needf = 0: fademusic fmvol: fadetopal master(), buffer(): setkeys
IF needf > 1 THEN needf = needf - 1
dowait
LOOP

usermenu:
copypage vpage, 3
copypage 3, dpage
menu$(0) = "Quit"
menu$(1) = "Status"
menu$(2) = "Map"
menu$(3) = "Spell"
menu$(4) = "Items"
menu$(5) = "Equip"
menu$(6) = "Save"
menu$(7) = "Volume"
menu$(8) = "Order": IF readbit(gen(), 101, 5) THEN menu$(8) = "Team"
'THIS STUFF WILL CHANGE LATER...I think...
mt = 8
mi(0) = 4
mi(1) = 3
mi(2) = 1
mi(3) = 5
mi(4) = 8
mi(5) = 2
mi(6) = 6
mi(7) = 0
mi(8) = 7

IF gmap(2) = 0 THEN
 o = 0
 FOR i = 0 TO mt
  IF mi(i) = 2 THEN o = 1: SWAP mi(i), mi(i + 1)
 NEXT i
 IF o = 1 THEN mt = mt - 1
END IF
IF gmap(3) = 0 THEN
 o = 0
 FOR i = 0 TO mt
  IF mi(i) = 6 THEN o = 1: SWAP mi(i), mi(i + 1)
 NEXT i
 IF o = 1 THEN mt = mt - 1
END IF
csr = 0: ptr = 0
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx - catx(0), mapy - caty(0), 1, timing()
IF carray(5) > 1 OR abortg = 1 THEN
 setkeys
 FOR i = 0 TO 7: carray(i) = 0: NEXT i
 loadpage game$ + ".til" + CHR$(0), gmap(0), 3: RETURN
END IF
IF carray(0) > 1 THEN ptr = loopvar(ptr, 0, mt, -1)
IF carray(1) > 1 THEN ptr = loopvar(ptr, 0, mt, 1)
IF mi(ptr) = 7 THEN
 IF carray(2) > 1 THEN fmvol = large(fmvol - 1, 0): setfmvol fmvol
 IF carray(3) > 1 THEN fmvol = small(fmvol + 1, 15): setfmvol fmvol
END IF
IF carray(4) > 1 THEN
 loadpage game$ + ".til" + CHR$(0), gmap(0), 3
 IF mi(ptr) = 4 THEN
  loadpage game$ + ".til" + CHR$(0), gmap(0), 3
  GOSUB updatescr
  say = items(item(), item$(), hero(), stat(), name$(), timing(), vpage, dpage, bmenu(), spell(), pal(), carray(), csetup(), gotm, gotj(), mouse(), joy())
  IF say THEN
   '--player has used an item that calls a text box--
   setkeys
   FOR i = 0 TO 7: carray(i) = 0: NEXT i
   loadpage game$ + ".til" + CHR$(0), gmap(0), 3
   IF say > 0 THEN
    GOSUB loadsay
   END IF
   RETURN
  END IF
 END IF
 IF mi(ptr) = 1 THEN
  w = onwho("Who's Status?", hero(), a(), dpage, vpage, timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx, catx(0), mapy, caty(0), pal())
  IF w >= 0 THEN
   loadpage game$ + ".til" + CHR$(0), gmap(0), 3
   GOSUB updatescr
   status w, hero(), stat(), name$(), exlev&(), gold&, timing(), vpage, dpage, bmenu(), lmp(), carray(), csetup(), gotm, gotj(), mouse(), joy()
  END IF
 END IF
 IF mi(ptr) = 3 THEN
  w = onwho("Who's Spells?", hero(), a(), dpage, vpage, timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx, catx(0), mapy, caty(0), pal())
  IF w >= 0 THEN
   loadpage game$ + ".til" + CHR$(0), gmap(0), 3
   GOSUB updatescr
   spells w, hero(), stat(), name$(), timing(), vpage, dpage, bmenu(), lmp(), spell(), pal(), carray(), csetup(), gotm, gotj(), mouse(), joy()
  END IF
 END IF
 IF mi(ptr) = 6 THEN
  GOSUB updatescr
  temp = picksave(svcsr, pal(), timing(), dpage, vpage, carray(), csetup(), gotm, gotj(), mouse(), joy(), sourcerpg$)
  IF temp >= 0 THEN savegame temp, sourcerpg$, map, catx(), caty(), catd(), foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), a(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), global()
  GOSUB reloadnpc
 END IF
 IF mi(ptr) = 5 THEN
  w = onwho("Equip Who?", hero(), a(), dpage, vpage, timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx, catx(0), mapy, caty(0), pal())
  IF w >= 0 THEN
   loadpage game$ + ".til" + CHR$(0), gmap(0), 3
   GOSUB updatescr
   equip w, hero(), stat(), name$(), timing(), vpage, dpage, item(), item$(), eqstuf(), bmenu(), carray(), csetup(), gotm, gotj(), mouse(), joy(), tag(), itembits()
  END IF
 END IF
 IF mi(ptr) = 2 THEN minimap vpage, dpage, scroll(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mx, my, timing(), gmap(), catx(0), caty(0), tastuf()
 IF mi(ptr) = 8 THEN
  loadpage game$ + ".til" + CHR$(0), gmap(0), 3
  GOSUB updatescr
  heroswap readbit(gen(), 101, 5), hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf(), timing(), carray(), vpage, dpage, csetup(), gotm, gotj(), mouse(), joy(), pal()
 END IF
 loadpage game$ + ".til" + CHR$(0), gmap(0), 3
 GOSUB updatescr
 IF mi(ptr) = 0 THEN GOSUB verquit
 '---After all sub-menus are done, re-evaluate the hero/item tags
 '---that way if you revive a hero, kill a hero swap out... whatever
 evalherotag tag(), herobits(), hero(), stat(), leader
 evalitemtag tag(), itembits(), hero(), eqstuf(), item()
END IF
centerfuz 160, 100, 120, (mt + 2) * 10, 1, dpage
FOR i = 0 TO mt
 col = 7
 IF mi(i) = 7 AND fmvol THEN centerbox 160, 110 - ((mt + 2) * 10) * .5 + (i * 10), fmvol * 6, 10, 1, dpage
 IF ptr = i THEN col = 14 + tog
 edgeprint menu$(mi(i)), xstring(menu$(mi(i)), 160), 106 - ((mt + 2) * 10) * .5 + (i * 10), col, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
dowait
LOOP

verquit:
dd = 2
ptr2 = 0
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 150 + ptr2, 90, 0, timing()
wtog(0) = loopvar(wtog(0), 0, 3, 1)
IF carray(5) > 1 THEN abortg = 0: setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: RETURN
IF carray(4) > 1 AND ABS(ptr2) > 20 THEN
 IF ptr2 < 0 THEN abortg = 1: fadeto buffer(), 0, 0, 0
 setkeys
 FOR i = 0 TO 7: carray(i) = 0: NEXT i
 RETURN
END IF
IF ABS(ptr2) > 50 THEN
 IF ptr2 < 0 THEN abortg = 1: fadeto buffer(), 0, 0, 0
 setkeys
 FOR i = 0 TO 7: carray(i) = 0: NEXT i
 RETURN
END IF
IF carray(2) > 0 THEN ptr2 = ptr2 - 5: dd = 3
IF carray(3) > 0 THEN ptr2 = ptr2 + 5: dd = 1
centerbox 160, 95, 200, 42, 15, dpage
loadsprite buffer(), 0, 200 * ((dd * 2) + INT(wtog(0) / 2)), 0 * 5, 20, 20, 2
drawsprite buffer(), 0, pal(), a(20) * 16, 150 + (ptr2), 90, dpage
edgeprint "Quit Playing?", xstring("Quit Playing?", 160), 80, 15, dpage
col = 7: IF ptr2 < -20 THEN col = 10 + tog * 5
edgeprint "Yes", 70, 96, col, dpage
col = 7: IF ptr2 > 20 THEN col = 10 + tog * 5
edgeprint "No", 240, 96, col, dpage
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
dowait
LOOP

updatescr:
GOSUB vishero
setmapdata scroll(), pass(), 0, 0
drawmap mapx, mapy, 1, dpage
GOSUB drawnpc
' rectangle 0, 0, 20, 200, 0, dpage
' rectangle 300, 0, 20, 200, 0, dpage
GOSUB cathero
drawmap mapx, mapy, 2, dpage
copypage dpage, 3
RETURN

vishero:
o = 0
FOR i = 0 TO 3
 catp(i) = -1
NEXT i
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  setpicstuf a(), 636, -1
  loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
  catp(o) = a(20)
  setpicstuf buffer(), 1600, 2
  loadset game$ + ".pt4" + CHR$(0), a(19), 5 * o
  o = o + 1
 END IF
NEXT i
FOR i = 3 TO 0 STEP -1
 IF hero(i) > 0 THEN leader = i
NEXT i
setpicstuf a(), 636, -1
loadset game$ + ".dt0" + CHR$(0), hero(leader) - 1, 0
RETURN

usething:
sayer = -1
IF auto = 0 THEN
 ux = catx(0)
 uy = caty(0)
 IF catd(0) = 0 THEN uy = uy - 20
 IF catd(0) = 1 THEN ux = ux + 20
 IF catd(0) = 2 THEN uy = uy + 20
 IF catd(0) = 3 THEN ux = ux - 20
END IF
j = -1
DO
 j = j + 1
 IF j > 299 THEN RETURN
LOOP UNTIL ABS(npcl(j) - ux) < 16 AND ABS(npcl(j + 300) - uy) < 16 AND npcl(j + 600) > 0
sayer = j
IF sayer >= 0 THEN
 '--Step-on NPCs cannot be used
 IF auto = 0 AND npcs((npcl(sayer + 600) - 1) * 15 + 8) = 2 THEN RETURN
 getit = npcs((npcl(sayer + 600) - 1) * 15 + 6)
 IF getit THEN getitem getit, item(), item$()
 '---DIRECTION CHANGING-----------------------
 IF npcs((npcl(sayer + 600) - 1) * 15 + 5) < 2 THEN
  recalld = npcl(sayer + 900)
  npcl(sayer + 900) = catd(0)
  npcl(sayer + 900) = loopvar(npcl(sayer + 900), 0, 3, 1): npcl(sayer + 900) = loopvar(npcl(sayer + 900), 0, 3, 1)
 END IF
 IF npcs((npcl(sayer + 600) - 1) * 15 + 11) > 0 THEN
  '--One-time-use tag
  setbit tag(), 0, 1000 + npcs((npcl(sayer + 600) - 1) * 15 + 11), 1
 END IF
 IF npcs((npcl(sayer + 600) - 1) * 15 + 12) > 0 THEN
  '--summon a script directly from and NPC
  IF runscript(npcs((npcl(sayer + 600) - 1) * 15 + 12), nowscript + 1, -1, script(), heap(), scrat()) THEN
   nowscript = nowscript + 1
   heap(scrat(nowscript, scrheap)) = npcs((npcl(sayer + 600) - 1) * 15 + 13)
  ELSE
   scripterr "failed to load" + STR$(npcs((npcl(sayer + 600) - 1) * 15 + 12))
  END IF
 END IF
 say = npcs((npcl(sayer + 600) - 1) * 15 + 4)
 SELECT CASE say
 CASE 0
  sayer = -1
 CASE IS > 0
  GOSUB loadsay
  evalherotag tag(), herobits(), hero(), stat(), leader
  evalitemtag tag(), itembits(), hero(), eqstuf(), item()
 END SELECT
END IF
RETURN

loadsay:
choosep = 0
setpicstuf buffer(), 400, -1
loadset game$ + ".say" + CHR$(0), say, 0
FOR j = 0 TO 7
 say$(j) = STRING$(38, 0)
 array2str buffer(), j * 38, say$(j)
NEXT j
temp$ = STRING$(42, 0)
array2str buffer(), 305, temp$
str2array temp$, buffer(), 0
FOR j = 0 TO 20
 saytag(j) = buffer(j)
NEXT j
IF istag(tag(), saytag(0), 0) THEN
 '--do something else instead
 IF saytag(1) < 0 THEN
  IF runscript(ABS(saytag(1)), nowscript + 1, -1, script(), heap(), scrat()) THEN
   nowscript = nowscript + 1
  ELSE
   scripterr "failed to load" + STR$(ABS(saytag(1)))
  END IF
  sayer = -1
  RETURN
 ELSE
  IF say <> saytag(1) THEN say = saytag(1): GOTO loadsay
 END IF
END IF
IF istag(tag(), saytag(2), 0) THEN
 IF ABS(saytag(3)) > 1 THEN setbit tag(), 0, ABS(saytag(3)), SGN(SGN(saytag(3)) + 1)
 IF ABS(saytag(4)) > 1 THEN setbit tag(), 0, ABS(saytag(4)), SGN(SGN(saytag(4)) + 1)
END IF
FOR j = 0 TO 1
 choose$(j) = STRING$(15, 0)
 array2str buffer(), 349 + (j * 18), choose$(j)
 WHILE RIGHT$(choose$(j), 1) = CHR$(0): choose$(j) = LEFT$(choose$(j), LEN(choose$(j)) - 1): WEND
 chtag(j) = buffer(182 + (j * 9))
NEXT j
saybit(0) = buffer(174)
FOR j = 0 TO 6
 sayenh(j) = buffer(193 + j)
NEXT j
IF sayenh(4) > 0 THEN loadpage game$ + ".mxs" + CHR$(0), sayenh(4) - 1, 3
IF sayenh(5) > 0 THEN loadsong game$ + "." + RIGHT$(STR$(sayenh(5) - 1), LEN(STR$(sayenh(5) - 1)) - 1) + CHR$(0)
showsay = 8
RETURN

nextsay:
'---ALWAYS RESET TILEMAP---
IF sayenh(4) > 0 THEN
 IF gen(50) THEN
  loadpage game$ + ".mxs" + CHR$(0), gen(50) - 1, 3
 ELSE
  loadpage game$ + ".til" + CHR$(0), gmap(0), 3
 END IF
END IF
'---IF MADE A CHOICE---
IF readbit(saybit(), 0, 0) THEN
 IF ABS(chtag(choosep)) > 1 THEN setbit tag(), 0, ABS(chtag(choosep)), SGN(SGN(chtag(choosep)) + 1)
END IF
'---RESET MUSIC----
IF readbit(saybit(), 0, 3) THEN
 IF gmap(1) > 0 THEN
  loadsong game$ + "." + RIGHT$(STR$(gmap(1) - 1), LEN(STR$(gmap(1) - 1)) - 1) + CHR$(0)
 ELSE
  stopsong
 END IF
END IF
'---STOP...???----------
'IF istag(tag(), saytag(4), 0) THEN
' reinitnpc 1, npcl(), map, filenum$()
' GOSUB npcplot
' IF sayenh(4) > 0 THEN loadpage game$ + ".til" + CHR$(0), gmap(0), 3
' FOR i = 0 TO 6: sayenh(i) = 0: NEXT i
' showsay = 0: sayer = -1: setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: RETURN
'END IF
'---GAIN/LOSE CASH-----
IF istag(tag(), saytag(13), 0) THEN
 gold& = gold& + saytag(14)
 IF gold& > 2000000000 THEN gold& = 2000000000
 IF gold& < 0 THEN gold& = 0
END IF
'---SPAWN BATTLE--------
IF istag(tag(), saytag(5), 0) THEN
 fatal = 0
 ERASE scroll, pass, emap
 battle saytag(6), fatal, hero(), pal(), timing(), stat(), bmenu(), spell(), lmp(), gold&, exlev&(), item(), item$(), eqstuf(), fmvol, carray(), csetup(), gotm, gotj(), mouse(), joy()
 oldsong = 0
 afterbat = 1
 GOSUB preparemap: foep = range(100, 60): needf = 1
END IF
'---GAIN/LOSE ITEM--------
IF istag(tag(), saytag(17), 0) THEN
 IF saytag(18) > 0 THEN getitem saytag(18), item(), item$()
 IF saytag(18) < 0 THEN delitem ABS(saytag(18)), item(), item$()
END IF
'---SHOP/INN/SAVE/ETC------------
IF istag(tag(), saytag(7), 0) THEN
 copypage vpage, 3
 IF saytag(8) > 0 THEN
  shop saytag(8) - 1, timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), gold&, vpage, dpage, needf, hero(), name$(), stock(), tag(), item(), item$(), pal(), bmenu(), spell(), stat(), lmp(), exlev&(), eqstuf(), svcsr, map, catx(), caty(),  _
catd(), foep, leader, mapx, mapy, a(), npcl(), mx, my, scroll(), gmap(), hmask(), tastuf(), sourcerpg$, herobits(), itembits(), nowscript, script(), heap(), scrat(), global()
  GOSUB reloadnpc
 END IF
 inn = 0
 IF saytag(8) < 0 THEN
  IF shoption(inn, ABS(saytag(8)), timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), gold&, needf, hero(), name$(), stat(), vpage, dpage) THEN
   fadeto buffer(), 0, 0, 20
   needf = 1
  END IF
 END IF
 IF saytag(8) <= 0 AND inn = 0 THEN
  FOR i = 0 TO 3
   IF hero(i) > 0 AND (stat(i, 0, 0) OR readbit(gen(), 101, 4) = 0) THEN
    stat(i, 0, 0) = stat(i, 1, 0)
    stat(i, 0, 1) = stat(i, 1, 1)
    resetlmp lmp(), i, stat(i, 0, 12)
   END IF
  NEXT i
 END IF
 reloadhwp hero()
 GOSUB vishero
 loadpage game$ + ".til" + CHR$(0), gmap(0), 3
END IF
'---ADD/REMOVE/SWAP/LOCK HERO-----------------
IF istag(tag(), saytag(9), 0) THEN GOSUB arslhero
'---FORCE DOOR------
IF istag(tag(), saytag(15), 0) THEN
 dforce = saytag(16) + 1
 GOSUB opendoor
 IF needf = 0 THEN
  setmapdata emap(), pass(), 0, 0
  temp = readmapblock(INT(catx(0) / 20), INT(caty(0) / 20))
  IF temp > 0 THEN foep = large(foep - foef(temp - 1), 0)
  setmapdata scroll(), pass(), 0, 0
 END IF
 GOSUB setmapxy
END IF
'---JUMP TO NEXT TEXT BOX--------
IF istag(tag(), saytag(11), 0) THEN
 IF saytag(12) < 0 THEN
  IF runscript(ABS(saytag(12)), nowscript + 1, -1, script(), heap(), scrat()) THEN
   nowscript = nowscript + 1
  ELSE
   scripterr "failed to load" + STR$(ABS(saytag(12)))
  END IF
 ELSE
  say = saytag(12)
  GOSUB loadsay
  RETURN
 END IF
END IF
evalherotag tag(), herobits(), hero(), stat(), leader
evalitemtag tag(), itembits(), hero(), eqstuf(), item()
'---DONE EVALUATING CONDITIONALS--------
reinitnpc 1, npcl(), map, filenum$()
GOSUB vishero
GOSUB npcplot
IF sayer >= 0 AND npcl(sayer + 600) > 0 THEN
 IF npcs((npcl(sayer + 600) - 1) * 15 + 5) = 1 THEN
  npcl(sayer + 900) = recalld
 END IF
END IF
IF sayenh(4) > 0 THEN loadpage game$ + ".til" + CHR$(0), gmap(0), 3
FOR i = 0 TO 6: sayenh(i) = 0: NEXT i
showsay = 0
sayer = -1
setkeys
FOR i = 0 TO 7: carray(i) = 0: NEXT i
RETURN

arslhero: '---ADD/REMOVE/SWAP/LOCK
 '---ADD---
 IF saytag(10) > 0 THEN
  i = findhero(0, hero(), 0, 40, 1)
  IF i > -1 THEN
   addhero saytag(10), i, hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
   FOR o = 0 TO 3
    IF hero(o) > 0 THEN
     setpicstuf a(), 636, -1
     loadset game$ + ".dt0" + CHR$(0), hero(o) - 1, 0
     setpicstuf buffer(), 1600, 2
     loadset game$ + ".pt4" + CHR$(0), a(19), 5 * o
    END IF
   NEXT o
   GOSUB vishero
  END IF
 END IF '---end if > 0
 '---REMOVE---
 IF saytag(10) < 0 THEN
  IF howmanyh(hero(), 0, 40) > 1 THEN
   i = findhero(ABS(saytag(10)), hero(), 0, 40, 1)
   IF i > -1 THEN hero(i) = 0
   IF howmanyh(hero(), 0, 3) = 0 THEN GOSUB forceparty
  END IF
 END IF '---end if < 0
 GOSUB vishero
 '---SWAP-IN---
 IF saytag(19) > 0 THEN
  i = findhero(ABS(saytag(19)), hero(), 40, 0, -1)
  IF i > -1 THEN
   FOR o = 0 TO 3
    IF hero(o) = 0 THEN
     doswap i, o, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
     EXIT FOR
    END IF
   NEXT o
  END IF
 END IF '---end if > 0
 '---SWAP-OUT---
 IF saytag(19) < 0 THEN
  i = findhero(ABS(saytag(19)), hero(), 0, 40, 1)
  IF i > -1 THEN
   FOR o = 40 TO 4 STEP -1
    IF hero(o) = 0 THEN
     doswap i, o, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
     IF howmanyh(hero(), 0, 3) = 0 THEN GOSUB forceparty
     EXIT FOR
    END IF
   NEXT o
  END IF
 END IF '---end if < 0
 '---UNLOCK HERO---
 IF saytag(20) > 0 THEN
  temp = findhero(ABS(saytag(20)), hero(), 0, 40, 1)
  IF temp > -1 THEN setbit hmask(), 0, temp, 0
 END IF '---end if > 0
 '---LOCK HERO---
 IF saytag(20) < 0 THEN
  temp = findhero(ABS(saytag(20)), hero(), 0, 40, 1)
  IF temp > -1 THEN setbit hmask(), 0, temp, 1
 END IF '---end if > 0
RETURN

forceparty:
'---MAKE SURE YOU HAVE AN ACTIVE PARTY---
 fpi = findhero(-1, hero(), 0, 40, 1)
 IF fpi > -1 THEN
  FOR fpo = 0 TO 3
   IF hero(fpo) = 0 THEN
    doswap fpi, fpo, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
    EXIT FOR
   END IF
  NEXT fpo
 END IF
RETURN

movement:
FOR whoi = 0 TO 3
 IF (movdivis(xgo(whoi)) OR movdivis(ygo(whoi))) AND ghost = 0 THEN
  IF readbit(gen(), 44, suspendherowalls) = 0 THEN
   '--this only happens if herowalls is on
   '--wrapping passability
   p = readmapblock(INT(catx(whoi * 5) / 20), INT(caty(whoi * 5) / 20))
   pasx = INT(catx(whoi * 5) / 20)
   pasy = INT(caty(whoi * 5) / 20) - 1: IF pasy < 0 THEN pasy = (scroll(1) - 1)
   pu = readmapblock(pasx, pasy)
   pasx = INT(catx(whoi * 5) / 20) + 1: IF pasx > (scroll(0) - 1) THEN pasx = 0
   pasy = INT(caty(whoi * 5) / 20)
   pr = readmapblock(pasx, pasy)
   pasx = INT(catx(whoi * 5) / 20)
   pasy = INT(caty(whoi * 5) / 20) + 1: IF pasy > (scroll(1) - 1) THEN pasy = 0
   pd = readmapblock(pasx, pasy)
   pasx = INT(catx(whoi * 5) / 20) - 1: IF pasx < 0 THEN pasx = (scroll(0) - 1)
   pasy = INT(caty(whoi * 5) / 20)
   pl = readmapblock(pasx, pasy)
   IF ygo(whoi) > 0 AND movdivis(ygo(whoi)) AND ((p AND 1) = 1 OR (pu AND 4) = 4) THEN ygo(whoi) = 0
   IF ygo(whoi) < 0 AND movdivis(ygo(whoi)) AND ((p AND 4) = 4 OR (pd AND 1) = 1) THEN ygo(whoi) = 0
   IF xgo(whoi) > 0 AND movdivis(xgo(whoi)) AND ((p AND 8) = 8 OR (pl AND 2) = 2) THEN xgo(whoi) = 0
   IF xgo(whoi) < 0 AND movdivis(xgo(whoi)) AND ((p AND 2) = 2 OR (pr AND 8) = 8) THEN xgo(whoi) = 0
  END IF
  IF readbit(gen(), 44, suspendobstruction) = 0 THEN
   '--this only happens if obstruction is on
   FOR i = 0 TO 299
   IF npcl(i + 600) > 0 THEN '---NPC EXISTS---
    IF npcs((npcl(i + 600) - 1) * 15 + 8) < 2 THEN '---NPC IS AN OBSTRUCTION---
     IF INT((npcl(i + 0) - bound(npcl(i + 1500), -20, 20)) / 20) = INT((catx(whoi * 5) - bound(xgo(whoi), -20, 20)) / 20) AND INT((npcl(i + 300) - bound(npcl(i + 1800), -20, 20)) / 20) = INT((caty(whoi * 5) - bound(ygo(whoi), -20, 20)) / 20) THEN
      xgo(whoi) = 0: ygo(whoi) = 0
      id = (npcl(i + 600) - 1)
      IF npcs(id * 15 + 7) > 0 AND npcl(i + 1500) = 0 AND npcl(i + 1800) = 0 THEN
       temp = npcs(id * 15 + 7)
       IF catd(whoi) = 0 THEN IF (temp = 1 OR temp = 2 OR temp = 4) THEN npcl(i + 1800) = 20
       IF catd(whoi) = 2 THEN IF (temp = 1 OR temp = 2 OR temp = 6) THEN npcl(i + 1800) = -20
       IF catd(whoi) = 3 THEN IF (temp = 1 OR temp = 3 OR temp = 7) THEN npcl(i + 1500) = 20
       IF catd(whoi) = 1 THEN IF (temp = 1 OR temp = 3 OR temp = 5) THEN npcl(i + 1500) = -20
      END IF
      IF npcs(id * 15 + 8) = 1 AND whoi = 0 THEN
       IF ABS(npcl(i + 0) - catx(0)) <= 20 AND ABS(npcl(i + 300) - caty(0)) <= 20 THEN
        ux = npcl(i + 0)
        uy = npcl(i + 300)
        auto = 1
        GOSUB usething
       END IF
      END IF '---autoactivate
     END IF ' ---NPC IS IN THE WAY
    END IF ' ---NPC IS AN OBSTRUCTION
   END IF '---NPC EXISTS
   NEXT i
  END IF
 END IF'--this only gets run when starting a movement to a new tile
NEXT whoi
'--if the leader moved last time, and catapillar is enabled then make others trail
IF readbit(gen(), 44, suspendcatapillar) = 0 THEN
 IF xgo(0) OR ygo(0) THEN
  FOR i = 15 TO 1 STEP -1
   catx(i) = catx(i - 1)
   caty(i) = caty(i - 1)
   catd(i) = catd(i - 1)
  NEXT i
  FOR whoi = 0 TO 3
   wtog(whoi) = loopvar(wtog(whoi), 0, 3, 1)
  NEXT whoi
 END IF
ELSE
 FOR whoi = 0 TO 3
  IF xgo(whoi) OR ygo(whoi) THEN wtog(whoi) = loopvar(wtog(whoi), 0, 3, 1)
 NEXT whoi
END IF
FOR whoi = 0 TO 3
 didgo(whoi) = 0
 IF xgo(whoi) OR ygo(whoi) THEN
  IF xgo(whoi) > 0 THEN xgo(whoi) = xgo(whoi) - herospeed(whoi): catx(whoi * 5) = catx(whoi * 5) - herospeed(whoi): didgo(whoi) = 1
  IF xgo(whoi) < 0 THEN xgo(whoi) = xgo(whoi) + herospeed(whoi): catx(whoi * 5) = catx(whoi * 5) + herospeed(whoi): didgo(whoi) = 1
  IF ygo(whoi) > 0 THEN ygo(whoi) = ygo(whoi) - herospeed(whoi): caty(whoi * 5) = caty(whoi * 5) - herospeed(whoi): didgo(whoi) = 1
  IF ygo(whoi) < 0 THEN ygo(whoi) = ygo(whoi) + herospeed(whoi): caty(whoi * 5) = caty(whoi * 5) + herospeed(whoi): didgo(whoi) = 1
 END IF
 '--if catapillar is on, only the leader matters
 o = whoi: IF readbit(gen(), 44, suspendcatapillar) = 0 THEN o = 0
 IF didgo(o) = 1 AND xgo(o) = 0 AND ygo(o) = 0 THEN '--this should only happen when you finish moving
  '---check for harm tile
  p = readmapblock(INT(catx(whoi * 5) / 20), INT(caty(whoi * 5) / 20))
  IF (p AND 64) THEN
   o = -1
   FOR i = 0 TO whoi
    o = o + 1
    WHILE hero(o) = 0 AND o < 4: o = o + 1: WEND
   NEXT i
   IF o < 4 THEN
    stat(o, 0, 0) = bound(stat(o, 0, 0) - gmap(9), 0, stat(o, 1, 0))
    IF gmap(10) THEN rectangle 0, 0, 320, 200, gmap(10), vpage
   END IF
   '--check for death
   o = 0
   FOR i = 0 TO 3
    IF hero(i) > 0 THEN
     o = o + 1
     IF stat(i, 0, 0) <= 0 AND stat(i, 1, 0) > 0 THEN o = o - 1
    END IF
   NEXT i
   IF o = 0 THEN fatal = 1
  END IF
 END IF'--this should only happen when you finish moving
 IF gmap(5) = 1 THEN
  '--wrap walking
  IF catx(whoi * 5) < 0 THEN catx(whoi * 5) = catx(whoi * 5) + scroll(0) * 20
  IF catx(whoi * 5) >= scroll(0) * 20 THEN catx(whoi * 5) = catx(whoi * 5) - scroll(0) * 20
  IF caty(whoi * 5) < 0 THEN caty(whoi * 5) = caty(whoi * 5) + scroll(1) * 20
  IF caty(whoi * 5) >= scroll(1) * 20 THEN caty(whoi * 5) = caty(whoi * 5) - scroll(1) * 20
 ELSE
  '--crop walking
  IF catx(whoi * 5) < 0 THEN catx(whoi * 5) = 0: xgo(whoi) = 0
  IF catx(whoi * 5) > (scroll(0) - 1) * 20 THEN catx(whoi * 5) = (scroll(0) - 1) * 20: xgo(whoi) = 0
  IF caty(whoi * 5) < 0 THEN caty(whoi * 5) = 0: ygo(whoi) = 0
  IF caty(whoi * 5) > (scroll(1) - 1) * 20 THEN caty(whoi * 5) = (scroll(1) - 1) * 20: ygo(whoi) = 0
 END IF
NEXT whoi
'--only the leader may activate NPCs
IF xgo(0) = 0 AND ygo(0) = 0 AND (didgo(0) = 1 OR ng = 1) THEN
 ng = 0
 IF readbit(gen(), 44, suspendobstruction) = 0 THEN
  '--this only happens if obstruction is on
  FOR i = 0 TO 299
   IF npcl(i + 600) > 0 THEN '---NPC EXISTS---
    IF npcs((npcl(i + 600) - 1) * 15 + 8) = 2 THEN '---NPC IS PASSABLE---
     IF npcl(i + 0) = catx(0) AND npcl(i + 300) = caty(0) THEN '---YOU ARE ON NPC---
      ux = npcl(i + 0)
      uy = npcl(i + 300)
      auto = 1
      GOSUB usething
     END IF'---YOU ARE ON NPC---
    END IF ' ---NPC IS PASSABLE---
   END IF '---NPC EXISTS
  NEXT i
 END IF
 GOSUB opendoor
 IF needf = 0 THEN
  setmapdata emap(), pass(), 0, 0
  temp = readmapblock(INT(catx(0) / 20), INT(caty(0) / 20))
  IF temp > 0 THEN foep = large(foep - foef(temp - 1), 0)
  setmapdata scroll(), pass(), 0, 0
 END IF
END IF
GOSUB setmapxy
RETURN

setmapxy:
SELECT CASE gen(cameramode)
CASE herocam
 mapx = catx(gen(cameraArg)) - 150
 mapy = caty(gen(cameraArg)) - 90
 GOSUB limitcamera
CASE npccam
 mapx = npcl(gen(cameraArg)) - 150
 mapy = npcl(gen(cameraArg) + 300) - 90
 GOSUB limitcamera
CASE pancam
 IF gen(cameraArg2) > 0 THEN
  SELECT CASE gen(cameraArg)
  CASE 0'north
   mapy = mapy - gen(cameraArg3)
  CASE 1'east
   mapx = mapx + gen(cameraArg3)
  CASE 2'south
   mapy = mapy + gen(cameraArg3)
  CASE 3'west
   mapx = mapx - gen(cameraArg3)
  END SELECT
  gen(cameraArg2) = gen(cameraArg2) - 1
  IF gen(cameraArg2) = 0 THEN gen(cameramode) = stopcam
 END IF
CASE focuscam
 IF mapx < gen(cameraArg) THEN
  mapx = mapx + gen(cameraArg3)
 ELSE
  IF mapx > gen(cameraArg) THEN mapx = mapx - gen(cameraArg3)
 END IF
 IF mapy < gen(cameraArg2) THEN
  mapy = mapy + gen(cameraArg4)
 ELSE
  IF mapy > gen(cameraArg2) THEN mapy = mapy - gen(cameraArg4)
 END IF
 IF mapx < gen(cameraArg) + gen(cameraArg3) AND mapx > gen(cameraArg) - gen(cameraArg3) AND ABS(gen(cameraArg3)) > 1 THEN gen(cameraArg3) = gen(cameraArg3) - 1
 IF mapy < gen(cameraArg2) + gen(cameraArg4) AND mapy > gen(cameraArg2) - gen(cameraArg4) AND ABS(gen(cameraArg4)) > 1 THEN gen(cameraArg4) = gen(cameraArg4) - 1
 IF mapx = gen(cameraArg) THEN gen(cameraArg3) = 0
 IF mapy = gen(cameraArg2) THEN gen(cameraArg4) = 0
 IF gen(cameraArg3) = 0 AND gen(cameraArg4) = 0 THEN gen(cameramode) = stopcam
 IF mapx <> bound(mapx, -320, scroll(0) * 20) OR mapy <> bound(mapy, -200, scroll(1) * 20) THEN gen(cameramode) = stopcam
END SELECT
RETURN

limitcamera:
IF gmap(5) = 0 THEN
 mapx = bound(mapx, 0, scroll(0) * 20 - 320)
 mapy = bound(mapy, 0, scroll(1) * 20 - 200)
END IF
RETURN

movenpc:
FOR o = 0 TO 299
IF npcl(o + 600) > 0 THEN
 id = (npcl(o + 600) - 1)
 IF npcs(id * 15 + 2) > 0 AND npcs(id * 15 + 3) > 0 AND sayer <> o AND readbit(gen(), 44, suspendnpcs) = 0 THEN
  IF npcl(o + 1500) = 0 AND npcl(o + 1800) = 0 THEN
   'RANDOM WANDER---
   IF npcs(id * 15 + 2) = 1 THEN
    rand = 25
    IF ABS(npcl(o + 0) - catx(0)) <= 20 AND ABS(npcl(o + 300) - caty(0)) <= 20 THEN rand = 5
    IF INT(RND * 100) < rand THEN
     temp = INT(RND * 4)
     npcl(o + 900) = temp
     IF temp = 0 THEN npcl(o + 1800) = 20
     IF temp = 2 THEN npcl(o + 1800) = -20
     IF temp = 3 THEN npcl(o + 1500) = 20
     IF temp = 1 THEN npcl(o + 1500) = -20
    END IF
   END IF '---RANDOM WANDER
   'ASSORTED PACING---
   IF npcs(id * 15 + 2) > 1 AND npcs(id * 15 + 2) < 6 THEN
    IF npcl(o + 900) = 0 THEN npcl(o + 1800) = 20
    IF npcl(o + 900) = 2 THEN npcl(o + 1800) = -20
    IF npcl(o + 900) = 3 THEN npcl(o + 1500) = 20
    IF npcl(o + 900) = 1 THEN npcl(o + 1500) = -20
   END IF '---ASSORTED PACING
   'CHASE/FLEE---
   IF npcs(id * 15 + 2) > 5 AND npcs(id * 15 + 2) < 8 THEN
    rand = 100
    IF INT(RND * 100) < rand THEN
     IF INT(RND * 100) < 50 THEN
      IF caty(0) < npcl(o + 300) THEN temp = 0
      IF caty(0) > npcl(o + 300) THEN temp = 2
      IF caty(0) = npcl(o + 300) THEN temp = INT(RND * 4)
     ELSE
      IF catx(0) < npcl(o + 0) THEN temp = 3
      IF catx(0) > npcl(o + 0) THEN temp = 1
      IF catx(0) = npcl(o + 0) THEN temp = INT(RND * 4)
     END IF
     IF npcs(id * 15 + 2) = 7 THEN temp = loopvar(temp, 0, 3, 2)
     npcl(o + 900) = temp
     IF temp = 0 THEN npcl(o + 1800) = 20
     IF temp = 2 THEN npcl(o + 1800) = -20
     IF temp = 3 THEN npcl(o + 1500) = 20
     IF temp = 1 THEN npcl(o + 1500) = -20
    END IF
   END IF '---CHASE/FLEE
  END IF
 END IF
 IF npcl(o + 1500) <> 0 OR npcl(o + 1800) <> 0 THEN GOSUB movenpcgo
END IF
NEXT o
RETURN

movenpcgo:
setmapdata pass(), pass(), 0, 0
npcl(o + 1200) = loopvar(npcl(o + 1200), 0, 3, 1)
'IF (ABS(npcl(o + 1500)) = 20 OR ABS(npcl(o + 1800)) = 20) THEN
IF movdivis(npcl(o + 1500)) OR movdivis(npcl(o + 1800)) THEN
 IF readbit(gen(), 44, suspendnpcwalls) = 0 THEN
  '--this only happens if NPC walls on
  p = readmapblock(INT(npcl(o + 0) / 20), INT(npcl(o + 300) / 20))
  pu = readmapblock(INT(npcl(o + 0) / 20), INT(npcl(o + 300) / 20) - 1)
  pr = readmapblock(INT(npcl(o + 0) / 20) + 1, INT(npcl(o + 300) / 20))
  pd = readmapblock(INT(npcl(o + 0) / 20), INT(npcl(o + 300) / 20) + 1)
  pl = readmapblock(INT(npcl(o + 0) / 20) - 1, INT(npcl(o + 300) / 20))
  IF npcl(o + 1800) > 0 AND movdivis(npcl(o + 1800)) AND ((p AND 1) = 1 OR (pu AND 4) = 4) THEN npcl(o + 1800) = 0: GOSUB hitwall: GOTO nogo
  IF npcl(o + 1800) < 0 AND movdivis(npcl(o + 1800)) AND ((p AND 4) = 4 OR (pd AND 1) = 1) THEN npcl(o + 1800) = 0: GOSUB hitwall: GOTO nogo
  IF npcl(o + 1500) > 0 AND movdivis(npcl(o + 1500)) AND ((p AND 8) = 8 OR (pl AND 2) = 2) THEN npcl(o + 1500) = 0: GOSUB hitwall: GOTO nogo
  IF npcl(o + 1500) < 0 AND movdivis(npcl(o + 1500)) AND ((p AND 2) = 2 OR (pr AND 8) = 8) THEN npcl(o + 1500) = 0: GOSUB hitwall: GOTO nogo
 END IF
 IF readbit(gen(), 44, suspendobstruction) = 0 THEN
  '--this only happens if obstruction is on
  FOR i = 0 TO 299
   IF npcl(i + 600) > 0 AND o <> i THEN
    IF INT((npcl(i + 0) - bound(npcl(i + 1500), -20, 20)) / 20) = INT((npcl(o + 0) - bound(npcl(o + 1500), -20, 20)) / 20) AND INT((npcl(i + 300) - bound(npcl(i + 1800), -20, 20)) / 20) = INT((npcl(o + 300) - bound(npcl(o + 1800), -20, 20)) / 20)  _
THEN npcl(o + 1500) = 0: npcl(o + 1800) = 0: GOSUB hitwall: GOTO nogo
   END IF
  NEXT i
  '---CHECK THAT NPC IS OBSTRUCTABLE-----
  IF npcs((npcl(o + 600) - 1) * 15 + 8) < 2 THEN
   IF INT((catx(0) - bound(xgo(0), -20, 20)) / 20) = INT((npcl(o + 0) - bound(npcl(o + 1500), -20, 20)) / 20) AND INT((caty(0) - bound(ygo(0), -20, 20)) / 20) = INT((npcl(o + 300) - bound(npcl(o + 1800), -20, 20)) / 20) THEN
    npcl(o + 1500) = 0
    npcl(o + 1800) = 0
    IF npcl(o + 1200) = 3 THEN GOSUB hitwall: GOTO nogo
   END IF
   IF INT((catx(0) = npcl(o + 0) - bound(npcl(o + 1500), -20, 20)) / 20) AND INT((caty(0) = npcl(o + 300) - bound(npcl(o + 1800), -20, 20)) / 20) THEN npcl(o + 1500) = 0: npcl(o + 1800) = 0
  END IF
 END IF
END IF
IF npcl(o + 1500) > 0 THEN npcl(o + 1500) = npcl(o + 1500) - npcs(id * 15 + 3): npcl(o + 0) = npcl(o + 0) - npcs(id * 15 + 3)
IF npcl(o + 1500) < 0 THEN npcl(o + 1500) = npcl(o + 1500) + npcs(id * 15 + 3): npcl(o + 0) = npcl(o + 0) + npcs(id * 15 + 3)
IF npcl(o + 1800) > 0 THEN npcl(o + 1800) = npcl(o + 1800) - npcs(id * 15 + 3): npcl(o + 300) = npcl(o + 300) - npcs(id * 15 + 3)
IF npcl(o + 1800) < 0 THEN npcl(o + 1800) = npcl(o + 1800) + npcs(id * 15 + 3): npcl(o + 300) = npcl(o + 300) + npcs(id * 15 + 3)
IF npcl(o + 0) < 0 THEN npcl(o + 0) = 0: npcl(o + 1500) = 0: GOSUB hitwall
IF npcl(o + 0) > (scroll(0) - 1) * 20 THEN npcl(o + 0) = (scroll(0) - 1) * 20: npcl(o + 1500) = 0: GOSUB hitwall
IF npcl(o + 300) < 0 THEN npcl(o + 300) = 0: npcl(o + 1800) = 0: GOSUB hitwall
IF npcl(o + 300) > (scroll(1) - 1) * 20 THEN npcl(o + 300) = (scroll(1) - 1) * 20: npcl(o + 1800) = 0: GOSUB hitwall
nogo:
IF npcs(id * 15 + 8) = 1 AND showsay = 0 THEN
 IF ABS(npcl(o + 0) - catx(0)) <= 20 AND ABS(npcl(o + 300) - caty(0)) <= 20 THEN
  ux = npcl(o + 0)
  uy = npcl(o + 300)
  auto = 1
  GOSUB usething
 END IF
END IF
RETURN

hitwall:
 IF npcs(id * 15 + 2) = 2 THEN npcl(o + 900) = loopvar(npcl(o + 900), 0, 3, 2)
 IF npcs(id * 15 + 2) = 3 THEN npcl(o + 900) = loopvar(npcl(o + 900), 0, 3, 1)
 IF npcs(id * 15 + 2) = 4 THEN npcl(o + 900) = loopvar(npcl(o + 900), 0, 3, -1)
 IF npcs(id * 15 + 2) = 5 THEN npcl(o + 900) = INT(RND * 4)
RETURN

cathero:
IF readbit(gen(), 101, 1) = 1 THEN
 FOR i = 0 TO 3
  zbuf(i) = 3 - i
 NEXT i
 FOR i = 0 TO 12 STEP 5
  temp = 200
  FOR o = 12 TO i STEP -5
   IF (caty(zbuf(o / 5)) - mapy) < temp THEN temp = (caty(zbuf(o / 5)) - mapy): j = o
  NEXT o
  SWAP zbuf(j / 5), zbuf(i / 5)
 NEXT i
 FOR i = 0 TO 3
  IF catp(zbuf(i)) > -1 AND catx(zbuf(i) * 5) = bound(catx(zbuf(i) * 5), mapx - 20, mapx + 320) AND caty(zbuf(i) * 5) = bound(caty(zbuf(i) * 5), mapy - 20, mapy + 200) THEN
   loadsprite buffer(), 0, 200 * ((catd(zbuf(i) * 5) * 2) + INT(wtog(zbuf(i)) / 2)), zbuf(i) * 5, 20, 20, 2
   drawsprite buffer(), 0, pal(), catp(zbuf(i)) * 16, catx(zbuf(i) * 5) - mapx, (caty(zbuf(i) * 5) - mapy) - catz(zbuf(i) * 5) + gmap(11), dpage
  END IF
 NEXT i
ELSE
 loadsprite buffer(), 0, 200 * ((catd(0) * 2) + INT(wtog(0) / 2)), 0, 20, 20, 2
 drawsprite buffer(), 0, pal(), catp(0) * 16, catx(0) - mapx, (caty(0) - mapy) - catz(0) + gmap(11), dpage
END IF
RETURN

drawnpc:
FOR i = 0 TO 299
IF npcl(i + 600) > 0 THEN
 o = npcl(i + 600) - 1
 IF npcl(i + 0) >= mapx - 20 AND npcl(i + 0) <= mapx + (17 * 20) AND npcl(i + 300) >= mapy - 20 AND npcl(i + 300) <= mapy + (10 * 20) THEN
  loadsprite buffer(), 0, (400 * npcl(i + 900)) + (200 * INT(npcl(i + 1200) / 2)), 20 + (5 * o), 20, 20, 2
  drawsprite buffer(), 0, pal(), 16 * npcs(o * 15 + 1), npcl(i + 0) - mapx, npcl(i + 300) - mapy + gmap(11), dpage
 END IF
END IF
NEXT i
RETURN

npcplot:
 FOR i = 0 TO 299
  curnpc = npcl(i + 600) - 1
  IF npcl(i + 600) > 0 THEN
   IF readbit(tag(), 0, ABS(npcs(curnpc * 15 + 9))) <> SGN(SGN(npcs(curnpc * 15 + 9)) + 1) AND npcs(curnpc * 15 + 9) <> 0 THEN
    npcl(i + 600) = 0
   END IF
   IF readbit(tag(), 0, ABS(npcs(curnpc * 15 + 10))) <> SGN(SGN(npcs(curnpc * 15 + 10)) + 1) AND npcs(curnpc * 15 + 10) <> 0 THEN
    npcl(i + 600) = 0
   END IF
   IF npcs(curnpc * 15 + 11) > 0 THEN
    IF readbit(tag(), 0, 1000 + npcs(curnpc * 15 + 11)) = 1 THEN
     npcl(i + 600) = 0
    END IF
   END IF
  END IF
 NEXT i
RETURN

opendoor:
i = 0
DO
 IF door(i + 200) = 1 THEN
  IF (door(i) = INT(catx(0) / 20) AND door(i + 100) = INT(caty(0) / 20) + 1) OR dforce - 1 = i THEN dforce = 0: GOSUB thrudoor: RETURN
 END IF
 i = i + 1
LOOP UNTIL i > 99
RETURN

thrudoor:
o = 0
DO
IF i = link(o) THEN
 'PLOT CHECKING FOR DOORS
 bad = 1
 IF (istag(tag(), link(o + 500), 0) OR link(o + 500) = 0) AND (istag(tag(), link(o + 600), 0) OR link(o + 600) = 0) THEN bad = 0
 IF bad = 0 THEN
  map = link(o + 400)
  loaddoor map, door()
  catx(0) = door(link(o + 200)) * 20
  caty(0) = (door(link(o + 200) + 100) - 1) * 20
  fadeto buffer(), 0, 0, 0
  needf = 1
  ERASE scroll, pass, emap
  afterbat = 0
  GOSUB preparemap
  foep = range(100, 60)
  RETURN
 END IF
END IF
o = o + 1
LOOP UNTIL o = 200
RETURN

preparemap:
DIM scroll(16002), pass(16002), emap(16002)
setpicstuf gmap(), 40, -1
loadset game$ + ".map" + CHR$(0), map, 0
getmapname mapname$, map
loadtanim gmap(0), tastuf()
FOR i = 0 TO 1
 cycle(i) = 0
 cycptr(i) = 0
 cycskip(i) = 0
NEXT i
xbload game$ + ".t" + filenum$(map), scroll(), "Oh no! Map" + filenum$(map) + " tilemap is missing"
xbload game$ + ".p" + filenum$(map), pass(), "Oh no! Map" + filenum$(map) + " passabilitymap is missing"
xbload game$ + ".e" + filenum$(map), emap(), "Oh no! Map" + filenum$(map) + " foemap is missing"
xbload game$ + ".d" + filenum$(map), link(), "Oh no! Map" + filenum$(map) + " doorlinks are missing"
loaddoor map, door()
IF afterbat = 0 THEN
 showmapname = gmap(4)
 xbload game$ + ".l" + filenum$(map), npcl(), "Oh no! Map" + filenum$(map) + " NPC locations are missing"
 xbload game$ + ".n" + filenum$(map), npcs(), "Oh no! Map" + filenum$(map) + " NPC definitions are missing"
 FOR i = 0 TO 299
  npcl(i + 0) = npcl(i + 0) * 20
  npcl(i + 300) = (npcl(i + 300) - 1) * 20
  npcl(i + 1500) = 0
  npcl(i + 1800) = 0
 NEXT
END IF
GOSUB npcplot
IF afterbat = 0 THEN
 FOR i = 0 TO 15
  catx(i) = catx(0)
  caty(i) = caty(0)
  catd(i) = catd(0)
 NEXT i
END IF
GOSUB vishero
FOR i = 0 TO 35
 IF npcs(i * 15 + 3) = 3 THEN npcs(i * 15 + 3) = 10
 setpicstuf buffer(), 1600, 2
 loadset game$ + ".pt4" + CHR$(0), npcs(i * 15 + 0), 20 + (5 * i)
NEXT i
 loadpage game$ + ".til" + CHR$(0), gmap(0), 3
SELECT CASE gmap(5) '--outer edge wrapping
 CASE 0, 1'--crop edges or wrap
  setoutside -1
 CASE 2
  setoutside gmap(6)
END SELECT
sayer = -1
IF gmap(1) <> oldsong THEN
 stopsong
 oldsong = gmap(1)
 IF gmap(1) > 0 THEN loadsong game$ + "." + RIGHT$(STR$(gmap(1) - 1), LEN(STR$(gmap(1) - 1)) - 1) + CHR$(0)
END IF
evalherotag tag(), herobits(), hero(), stat(), leader
evalitemtag tag(), itembits(), hero(), eqstuf(), item()
IF afterbat = 0 THEN
 IF gmap(7) > 0 THEN
  IF runscript(gmap(7), nowscript + 1, -1, script(), heap(), scrat()) THEN
   nowscript = nowscript + 1
   heap(scrat(nowscript, scrheap)) = gmap(8)
  ELSE
   scripterr "failed to load" + STR$(gmap(7))
  END IF
 END IF
END IF
afterbat = 0
RETURN

reloadnpc:
 FOR o = 0 TO 3
  IF hero(o) > 0 THEN
   setpicstuf a(), 636, -1
   loadset game$ + ".dt0" + CHR$(0), hero(o) - 1, 0
   setpicstuf buffer(), 1600, 2
   loadset game$ + ".pt4" + CHR$(0), a(19), 5 * o
  END IF
 NEXT o
 FOR i = 0 TO 35
  setpicstuf buffer(), 1600, 2
  loadset game$ + ".pt4" + CHR$(0), npcs(i * 15 + 0), 20 + (5 * i)
 NEXT i
RETURN

doihavebits:
FOR i = 0 TO large(gen(35), 59)
 setpicstuf buffer(), 636, -1
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
 itembits(i, 4) = buffer(0)    'tracks if item has a name or not
NEXT i
RETURN

titlescr:
clearpage 3
loadpage game$ + ".mxs" + CHR$(0), gen(1), 3
needf = 2: tt = 0: tt$ = "": col = 240
IF gen(2) > 0 THEN loadsong game$ + "." + RIGHT$(STR$(gen(2) - 1), LEN(STR$(gen(2) - 1)) - 1) + CHR$(0)
fademusic fmvol
setkeys
DO
setwait timing(), 80
setkeys
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 0, 0, 1, timing()
IF carray(5) > 1 THEN GOTO resetg
FOR i = 2 TO 88
 IF keyval(i) > 1 OR carray(4) > 1 OR carray(5) > 1 THEN RETURN
NEXT i
FOR i = 0 TO 1
 gotj(i) = readjoy(joy(), i)
 IF gotj(i) THEN
  IF joy(2) = 0 OR joy(3) = 0 THEN
   joy(2) = -1: joy(3) = -1
   calibrate gotj(), joy(), dpage, vpage, timing()
   joy(2) = -1: joy(3) = -1
   RETURN
  ELSE
   gotj(i) = 0
  END IF
 END IF
NEXT i
IF tt = 14 THEN tt$ = "Press any Key"
IF tt > 20 AND col < 255 THEN col = col + 1
edgeprint tt$, 160 - LEN(tt$) * 4, 190, col, dpage
IF tt < 1000 THEN tt = tt + 1
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
IF needf = 1 THEN needf = 0: fademusic fmvol: fadetopal master(), buffer()
IF needf = 2 THEN needf = 1
dowait
LOOP

defaultc:
RESTORE ctrldata
FOR i = 0 TO 12
 READ csetup(i)
NEXT i
FOR i = 9 TO 12
 READ joy(i)
NEXT i
RETURN
ctrldata:
DATA 72,80,75,77,57,28,29,1,56,1,15,36,51
DATA 150,650,150,650

resetg:
IF RIGHT$(cline$, 4) = ".RPG" THEN GOTO finis
fademusic 0
fadeto buffer(), 0, 0, 0
GOSUB shutoff
closemusic
'closefile
setfmvol fmvol
restoremode
GOTO thestart

finis:
fademusic 0
IF abortg = 0 THEN fadeto buffer(), 0, 0, 0
OPEN "playing.tmp\kill.me" FOR OUTPUT AS #1
 PRINT #1, "deletan por favor"
CLOSE #1
KILL progdir$ + "playing.tmp\*.*"
RMDIR progdir$ + "playing.tmp"
GOSUB shutoff
releasestack
'closefile
closemusic
IF isfile(tmp$ + "cline.___" + CHR$(0)) THEN KILL tmp$ + "cline.___"
resetfm
setfmvol fmvol
restoremode
SYSTEM

switchon:
regs.ax = &H2509: regs.ds = Keyseg: regs.dx = keyoff
CALL interruptx(&H21, regs, regs)
RETURN

shutoff:
regs.ax = &H2509: regs.ds = seg9: regs.dx = off9
CALL interruptx(&H21, regs, regs)
RETURN

'--this is what we have dimed for scripts
'--script(2048), heap(2048), global(1024), stack(1024), scrat(128, 11), nowscript
interpret:
 wantbox = 0
 IF nowscript >= 0 THEN
  SELECT CASE scrat(nowscript, scrstate)
  CASE IS < stnone
   scripterr "illegally suspended script"
   scrat(nowscript, scrstate) = scrat(nowscript, scrstate) * -1
  CASE stnone
   scripterr "script" + STR$(nowscript) + " became stateless"
  CASE stwait
   '--evaluate wait conditions
   SELECT CASE scrat(nowscript, curvalue)
   CASE 1'--wait number of ticks
    retvals(0) = retvals(0) - 1
    IF retvals(0) < 1 THEN scrat(nowscript, scrstate) = streturn
   CASE 2'--wait for all
    n = 0
    FOR i = 0 TO 3
     IF xgo(i) <> 0 OR ygo(i) <> 0 THEN n = 1
    NEXT i
    IF readbit(gen(), 44, suspendnpcs) = 1 THEN
     FOR i = 0 TO 299
      IF npcl(i + 1500) <> 0 OR npcl(i + 1800) <> 0 THEN n = 1
      EXIT FOR
     NEXT i
    END IF
    IF gen(cameramode) = pancam OR gen(cameramode) = focuscam THEN n = 1
    IF n = 0 THEN scrat(nowscript, scrstate) = streturn
   CASE 3'--wait for hero
    IF xgo(retvals(0)) = 0 AND ygo(retvals(0)) = 0 THEN scrat(nowscript, scrstate) = streturn
   CASE 4'--wait for NPC
    FOR i = 0 TO 299
     IF npcl(i + 600) - 1 = bound(retvals(0), 0, 35) THEN
      IF npcl(i + 1500) = 0 AND npcl(i + 1800) = 0 THEN scrat(nowscript, scrstate) = streturn
      EXIT FOR
     END IF
    NEXT i
    IF i = 300 THEN scrat(nowscript, scrstate) = streturn
   CASE 9'--wait for keypress
    playtimer
    control carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx - catx(0), mapy - caty(0), 1, timing()
    IF retvals(0) >= 0 AND retvals(0) <= 5 THEN
     IF carray(retvals(0)) > 1 THEN scrat(nowscript, scrstate) = streturn
    ELSE
     FOR i = 0 TO 5
      IF carray(i) > 1 THEN scrat(nowscript, scrstate) = streturn
     NEXT i
     FOR i = 1 TO 127
      IF keyval(i) > 1 THEN scrat(nowscript, scrstate) = streturn
     NEXT i
    END IF
   CASE 42'--wait for camera
    IF gen(cameramode) <> pancam AND gen(cameramode) <> focuscam THEN scrat(nowscript, scrstate) = streturn
   CASE 59'--wait for text box
    IF showsay = 0 OR readbit(gen(), 44, suspendboxadvance) = 1 THEN scrat(nowscript, scrstate) = streturn
   CASE 73'--game over
   CASE ELSE
    scripterr "illegal wait substate" + STR$(scrat(nowscript, curvalue))
   END SELECT
   IF scrat(nowscript, scrstate) <> stwait THEN GOSUB interpretloop
  CASE ELSE
   '--interpret script
   GOSUB interpretloop
  END SELECT
 END IF
 '--do spawned text boxes, battles, etc.
 IF wantbox > 0 THEN
  say = wantbox
  GOSUB loadsay
 END IF
RETURN

interpretloop:
DO
  'debug "state =" + STR$(scrat(nowscript, scrstate))
  'debug "kind =" + STR$(scrat(nowscript, curkind))
  'debug "value =" + STR$(scrat(nowscript, curvalue))
  'debug "argn =" + STR$(scrat(nowscript, curargn))
  'debug "argc =" + STR$(scrat(nowscript, curargc))
  'debug "depth =" + STR$(scrat(nowscript, scrdepth))
 SELECT CASE scrat(nowscript, scrstate)
'---------------------------------------------------------------------------
 CASE stwait'---begin waiting for something
  EXIT DO
'---------------------------------------------------------------------------
 CASE stdoarg'---do argument
  subdoarg
'---------------------------------------------------------------------------
 CASE stread'---read statement
  IF functionread THEN EXIT DO
'---------------------------------------------------------------------------
 CASE streturn'---return
  subreturn
'---------------------------------------------------------------------------
 CASE stnext'---check if all args are done
  IF scrat(nowscript, curargn) >= scrat(nowscript, curargc) THEN
   '--pop return values of each arg
   '--evaluate function, math, script, whatever
   '--scriptret would be set here, pushed at return
   SELECT CASE scrat(nowscript, curkind)
   CASE tystop
    scripterr "stnext encountered noop" + STR$(scrat(nowscript, value)): nowscript = -1: EXIT DO
   CASE tymath, tyfunct
    '--complete math and functions, nice and easy.
    FOR i = scrat(nowscript, curargc) - 1 TO 0 STEP -1
     retvals(i) = popw
    NEXT i
    GOSUB sfunctions
    '--unless you have switched to wait mode, return
    IF scrat(nowscript, scrstate) = stnext THEN scrat(nowscript, scrstate) = streturn'---return
   CASE tyflow
    '--finish flow control? tricky!
    SELECT CASE scrat(nowscript, curvalue)
    CASE flowwhile'--repeat or terminate while
     SELECT CASE scrat(nowscript, curargn)
     CASE 2
      '--if a while statement finishes normally (argn is 2) then it repeats.
      dummy = popw
      scrat(nowscript, curargn) = 0
     CASE 3
      '--if it is broken, argn will be three, and execution will continue as normal
      pushw 9999 '--while must pad the stack becuase it autodiscards the return value of it's conditional
      pushw 9998
      GOSUB dumpandreturn
     CASE ELSE
      scripterr "while fell out of bounds, landed on" + STR$(scrat(nowscript, curargn)): nowscript = -1: EXIT DO
     END SELECT
    CASE flowfor'--repeat or terminate for
     SELECT CASE scrat(nowscript, curargn)
     CASE 5
      '--normal for termination means repeat
      dummy = popw
      GOSUB incrementflow
      scrat(nowscript, curargn) = 4
     CASE 6
      '--return
      GOSUB dumpandreturn
     CASE ELSE
      scripterr "for fell out of bounds, landed on" + STR$(scrat(nowscript, curargn)): nowscript = -1: EXIT DO
     END SELECT
    CASE flowreturn
     scrat(nowscript, scrret) = dummy
     scriptret = 0
     scrat(nowscript, scrstate) = streturn'---return
    CASE ELSE
     '--do, then, etc... terminate normally
     scriptret = -1
     GOSUB dumpandreturn
    END SELECT
    'scrat(nowscript, scrstate) = streturn'---return
   CASE tyscript
    IF runscript(scrat(nowscript, curvalue), nowscript + 1, 0, script(), heap(), scrat()) THEN
     '--fill heap with return values
     nowscript = nowscript + 1
     FOR i = scrat(nowscript - 1, curargc) - 1 TO 0 STEP -1
      heap(scrat(nowscript, scrheap) + i) = popw
     NEXT i
    ELSE
     scripterr "failed to load script" + STR$(scrat(nowscript, curvalue)): nowscript = -1: EXIT DO
     scrat(nowscript, scrstate) = streturn'---return
    END IF
   CASE ELSE
    scripterr "illegal kind" + STR$(scrat(nowscript, curkind)) + STR$(scrat(nowscript, curvalue)) + " in stnext": nowscript = -1: EXIT DO
   END SELECT
  ELSE
   '--flow control is special, for all else, do next arg
   SELECT CASE scrat(nowscript, curkind)
   CASE tyflow
    SELECT CASE scrat(nowscript, curvalue)
    CASE flowif'--we got an if!
     SELECT CASE scrat(nowscript, curargn)
     CASE 0
      scrat(nowscript, scrstate) = stdoarg'---call conditional
     CASE 1
      r = popw
      pushw r
      IF r THEN
       scrat(nowscript, scrstate) = stdoarg'---call then block
      ELSE
       scrat(nowscript, curargn) = 2
       scrat(nowscript, scrstate) = stdoarg'---call else block
      END IF
      '--if-then-else needs one extra thing on the stack to account for the option that didnt get used.
      pushw 0
     CASE 2
      scrat(nowscript, curargn) = 3 '---done
     CASE ELSE
      scripterr "if statement overstepped bounds"
     END SELECT
    CASE flowwhile'--we got a while!
     SELECT CASE scrat(nowscript, curargn)
     CASE 0
      scrat(nowscript, scrstate) = stdoarg'---call condition
     CASE 1
      r = popw
      'pushw r
      IF r THEN
       scrat(nowscript, scrstate) = stdoarg'---call do block
      ELSE
       scrat(nowscript, curargn) = 3
      END IF
     CASE ELSE
      scripterr "while statement has jumped the curb"
     END SELECT
    CASE flowfor'--we got a for!
     SELECT CASE scrat(nowscript, curargn)
     '--argn 0 is var
     '--argn 1 is start
     '--argn 2 is end
     '--argn 3 is step
     '--argn 4 is do block
     '--argn 5 is repeat (normal termination)
     '--argn 6 is break (failed limit check)
     CASE 0
      '--get var
      scrat(nowscript, scrstate) = stdoarg
      pushw 0
     CASE 1, 3
      '--get start, and later step
      scrat(nowscript, scrstate) = stdoarg
     CASE 2
      '--set variable to start val
      tmpstart = popw
      tmpvar = popw
      pushw tmpvar
      pushw tmpstart
      IF tmpvar < 0 THEN 'local variable
       heap(scrat(nowscript, scrheap) + ABS(tmpvar) - 1) = tmpstart
      ELSE 'global variable
       global(tmpvar) = tmpstart - tmpstep
      END IF
      '---now get end value
      scrat(nowscript, scrstate) = stdoarg
     CASE 4
      tmpstep = popw
      tmpend = popw
      tmpstart = popw
      tmpvar = popw
      pushw tmpvar
      pushw tmpstart
      pushw tmpend
      pushw tmpstep
      IF tmpvar < 0 THEN 'local variable
       tmpnow = heap(scrat(nowscript, scrheap) + ABS(tmpvar) - 1)
      ELSE 'global variable
       tmpnow = global(tmpvar)
      END IF
      IF (tmpnow > tmpend AND tmpstep > 0) OR (tmpnow < tmpend AND tmpstep < 0) THEN
       scrat(nowscript, curargn) = 6
      ELSE
       scrat(nowscript, scrstate) = stdoarg'---execute the do block
      END IF
     CASE ELSE
      scripterr "for statement is being difficult"
     END SELECT
    CASE ELSE
     scrat(nowscript, scrstate) = stdoarg'---call argument
    END SELECT
   CASE ELSE
    scrat(nowscript, scrstate) = stdoarg'---call argument
   END SELECT
  END IF
'---------------------------------------------------------------------------
 CASE stdone'---script terminates
  IF functiondone THEN EXIT DO
'---------------------------------------------------------------------------
 END SELECT
LOOP
RETURN

incrementflow:
 tmpstep = popw
 tmpend = popw
 tmpstart = popw
 tmpvar = popw
 pushw tmpvar
 pushw tmpstart
 pushw tmpend
 pushw tmpstep
 IF tmpvar < 0 THEN 'local variable
  tmpnow = heap(scrat(nowscript, scrheap) + ABS(tmpvar) - 1)
 ELSE 'global variable
  tmpnow = global(tmpvar)
 END IF
 tmpnow = tmpnow + tmpstep
 IF tmpvar < 0 THEN 'local variable
  heap(scrat(nowscript, scrheap) + ABS(tmpvar) - 1) = tmpnow
 ELSE 'global variable
  global(tmpvar) = tmpnow
 END IF
RETURN

dumpandreturn:
 FOR i = scrat(nowscript, curargc) - 1 TO 0 STEP -1
  dummy = popw
 NEXT i
 scriptret = 0
 scrat(nowscript, scrstate) = streturn'---return
RETURN

'---DO THE ACTUAL EFFECTS OF MATH AND FUNCTIONS----
sfunctions:
 SELECT CASE scrat(nowscript, curkind)
'---MATH----------------------------------------------------------------------
 CASE tymath
  scriptmath scrat(), nowscript, scriptret, retvals(), heap(), global()
'---FUNCTIONS-----------------------------------------------------------------
 CASE tyfunct
  SELECT CASE scrat(nowscript, curvalue)
  CASE 0'--noop
   scripterr "encountered noop"
  CASE 1'--Wait (cycles)
   IF retvals(0) > 0 THEN scrat(nowscript, scrstate) = stwait
  CASE 2'--wait for all
   IF retvals(0) > 0 THEN scrat(nowscript, scrstate) = stwait
  CASE 3'--wait for hero
   IF retvals(0) >= 0 AND retvals(0) <= 3 THEN scrat(nowscript, scrstate) = stwait
  CASE 4'--wait for NPC
   IF retvals(0) >= 0 AND retvals(0) <= 36 THEN scrat(nowscript, scrstate) = stwait
  CASE 5'--suspend npcs
   setbit gen(), 44, suspendnpcs, 1
  CASE 6'--suspend player
   setbit gen(), 44, suspendplayer, 1
  CASE 7'--resume npcs
   setbit gen(), 44, suspendnpcs, 0
  CASE 8'--resume player
   setbit gen(), 44, suspendplayer, 0
  CASE 9'--wait for key
   scrat(nowscript, scrstate) = stwait
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
  CASE 11'--Show Text Box (box)
   wantbox = retvals(0)
  CASE 12'--check tag
   scriptret = istag(tag(), retvals(0), 0)
  CASE 13'--set tag
   IF retvals(0) > 1 THEN
    IF retvals(1) = 0 THEN
     setbit tag(), 0, retvals(0), 0
    ELSE
     setbit tag(), 0, retvals(0), 1
    END IF
    reinitnpc 1, npcl(), map, filenum$()
    GOSUB npcplot
   END IF
  CASE 15'--use door
   dforce = retvals(0) + 1
   GOSUB opendoor
   IF needf = 0 THEN
    setmapdata emap(), pass(), 0, 0
    temp = readmapblock(INT(catx(0) / 20), INT(caty(0) / 20))
    IF temp > 0 THEN foep = large(foep - foef(temp - 1), 0)
    setmapdata scroll(), pass(), 0, 0
   END IF
   GOSUB setmapxy
  CASE 16'--fight formation
   fatal = 0
   ERASE scroll, pass, emap
   battle retvals(0), fatal, hero(), pal(), timing(), stat(), bmenu(), spell(), lmp(), gold&, exlev&(), item(), item$(), eqstuf(), fmvol, carray(), csetup(), gotm, gotj(), mouse(), joy()
   oldsong = 0
   afterbat = 1
   GOSUB preparemap: foep = range(100, 60): needf = 1
  CASE 17'--get item
   FOR i = 1 TO retvals(1)
    getitem retvals(0) + 1, item(), item$()
   NEXT i
  CASE 18'--delete item
   FOR i = 1 TO retvals(1)
    delitem retvals(0) + 1, item(), item$()
   NEXT i
  CASE 19'--leader
   FOR i = 0 TO 3
    IF hero(0) > 0 THEN scriptret = hero(0) - 1
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
   wtog(bound(retvals(0), 0, 3)) = bound(retvals(1), 0, 1) * 2
  CASE 26'--set NPC frame
   FOR i = 0 TO 299
    IF npcl(i + 600) - 1 = large(retvals(0), 0) THEN
     npcl(i + 1200) = bound(retvals(1), 0, 1) * 2
    END IF
   NEXT i
  CASE 27'--suspend overlay
   setbit gen(), 44, suspendoverlay, 1
  CASE 28'--play song
   loadsong game$ + "." + LTRIM$(STR$(retvals(0))) + CHR$(0)
  CASE 29'--stop song
   stopsong
  CASE 32'--show backdrop
   gen(50) = bound(retvals(0) + 1, 0, gen(100))
   loadpage game$ + ".mxs" + CHR$(0), gen(50) - 1, 3
  CASE 33'--show map
   gen(50) = 0
   IF sayenh(4) THEN
    loadpage game$ + ".mxs" + CHR$(0), sayenh(4) - 1, 3
   ELSE
    loadpage game$ + ".til" + CHR$(0), gmap(0), 3
   END IF
  CASE 38'--camera follows hero
   gen(cameramode) = herocam
   gen(cameraArg) = large(retvals(0), 0)
   IF gen(cameraArg) > 3 THEN gen(cameraArg) = 0
  CASE 39'--camera follows NPC
   gen(cameramode) = npccam
   FOR i = 0 TO 299
    IF npcl(i + 600) - 1 = bound(retvals(0), 0, 35) THEN gen(cameraArg) = i: EXIT FOR
   NEXT i
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
   scrat(nowscript, scrstate) = stwait
  CASE 43'--hero x
   IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
    scriptret = catx(retvals(0) * 5) \ 20
   END IF
  CASE 44'--hero y
   IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
    scriptret = caty(retvals(0) * 5) \ 20
   END IF
  CASE 45'--NPC x
   FOR i = 0 TO 299
    IF npcl(i + 600) - 1 = bound(retvals(0), 0, 35) THEN scriptret = npcl(i) / 20: EXIT FOR
   NEXT i
  CASE 46'--NPC y
   FOR i = 0 TO 299
    IF npcl(i + 600) - 1 = bound(retvals(0), 0, 35) THEN scriptret = npcl(i + 300) / 20: EXIT FOR
   NEXT i
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
  CASE 52'--walk NPC
   FOR i = 0 TO 299
    IF npcl(i + 600) - 1 = bound(retvals(0), 0, 35) THEN
     SELECT CASE retvals(1)
     CASE 0'--north
      npcl(i + 900) = 0
      npcl(i + 1800) = retvals(2) * 20
     CASE 1'--east
      npcl(i + 900) = 1
      npcl(i + 1500) = (retvals(2) * 20) * -1
     CASE 2'--south
      npcl(i + 900) = 2
      npcl(i + 1800) = (retvals(2) * 20) * -1
     CASE 3'--west
      npcl(i + 900) = 3
      npcl(i + 1500) = retvals(2) * 20
     END SELECT
    END IF
   NEXT i
  CASE 53'--set hero direction
   IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
    catd(retvals(0) * 5) = retvals(1)
   END IF
  CASE 54'--set NPC direction
   FOR i = 0 TO 299
    IF npcl(i + 600) - 1 = large(retvals(0), 0) THEN
     npcl(i + 900) = bound(retvals(1), 0, 3)
    END IF
   NEXT i
  CASE 57'--suspend catapillar
   setbit gen(), 44, suspendcatapillar, 1
  CASE 58'--resume catapillar
   setbit gen(), 44, suspendcatapillar, 0
   FOR o = 0 TO 10 STEP 5
    FOR i = o + 1 TO o + 4
     catx(i) = catx(i - 1) + ((catx(o + 5) - catx(o)) / 4)
     caty(i) = caty(i - 1) + ((caty(o + 5) - caty(o)) / 4)
     catd(i) = catd(o)
    NEXT i
   NEXT o
  CASE 59'--wait for text box
   IF readbit(gen(), 44, suspendboxadvance) = 0 THEN scrat(nowscript, scrstate) = stwait
  CASE 61'--teleport to map
   map = bound(retvals(0), 0, gen(0))
   FOR i = 0 TO 3
    catx(i) = retvals(1)
    caty(i) = retvals(2)
   NEXT i
   ERASE scroll, pass, emap
   afterbat = 0
   GOSUB preparemap
   foep = range(100, 60)
  CASE 62'--suspend random enemys
   setbit gen(), 44, suspendrandomenemys, 1
  CASE 63'--resume random enemys
   setbit gen(), 44, suspendrandomenemys, 0
   foep = range(100, 60)
  CASE 64'--get hero stat
   scriptret = stat(bound(retvals(0), 0, 40), bound(retvals(2), 0, 1), bound(retvals(1), 0, 13))
  CASE 65'--resume overlay
   setbit gen(), 44, suspendoverlay, 0
  CASE 66'--add hero
   IF retvals(0) >= 0 THEN
    FOR i = 37 TO 0 STEP -1
     IF hero(i) = 0 THEN slot = i
    NEXT i
    addhero retvals(0) + 1, slot, hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
    GOSUB vishero
   END IF
  CASE 67'--delete hero
   IF howmanyh(hero(), 0, 40) > 1 THEN
    i = findhero(bound(retvals(0), 0, 59) + 1, hero(), 0, 40, 1)
    IF i > -1 THEN hero(i) = 0
    IF howmanyh(hero(), 0, 3) = 0 THEN GOSUB forceparty
    GOSUB vishero
   END IF
  CASE 68'--swap out hero
   i = findhero(retvals(0) + 1, hero(), 0, 40, 1)
   IF i > -1 THEN
    FOR o = 40 TO 4 STEP -1
     IF hero(o) = 0 THEN
      doswap i, o, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
      IF howmanyh(hero(), 0, 3) = 0 THEN GOSUB forceparty
      GOSUB vishero
      EXIT FOR
     END IF
    NEXT o
   END IF
  CASE 69'--swap in hero
   i = findhero(retvals(0) + 1, hero(), 40, 0, -1)
   IF i > -1 THEN
    FOR o = 0 TO 3
     IF hero(o) = 0 THEN
      doswap i, o, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
      GOSUB vishero
      EXIT FOR
     END IF
    NEXT o
   END IF
  CASE 70'--room in active party
   scriptret = 4 - howmanyh(hero(), 0, 3)
  CASE 71'--lock hero
   temp = findhero(retvals(0) + 1, hero(), 0, 40, 1)
   IF temp > -1 THEN setbit hmask(), 0, temp, 1
  CASE 72'--unlock hero
   temp = findhero(retvals(0) + 1, hero(), 0, 40, 1)
   IF temp > -1 THEN setbit hmask(), 0, temp, 0
  CASE 73'--game over
   abortg = 1
   scrat(nowscript, scrstate) = stwait
  CASE 74'--set death script
   gen(42) = large(retvals(0), 0)
  CASE 75'--fade screen out
   fadeto buffer(), bound(retvals(0), 0, 63), bound(retvals(1), 0, 63), bound(retvals(2), 0, 63)
  CASE 76'--fade screen in
   fadetopal master(), buffer()
  CASE 77'--show value
   scriptout$ = LTRIM$(STR$(retvals(0)))
  CASE 78'--alter NPC
   FOR i = 0 TO 299
    IF npcl(i + 600) - 1 = bound(retvals(0), 0, 36) THEN
     id = (npcl(i + 600) - 1)
     npcs(id * 15 + retvals(1)) = retvals(2)
    END IF
   NEXT i
   IF retvals(1) = 0 THEN GOSUB reloadnpc
  CASE 79'--show no value
   scriptout$ = ""
  CASE 80'--current map
   scriptret = map
  CASE 81'--set hero speed
   IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
    herospeed(retvals(0)) = bound(retvals(1), 0, 20)
   END IF
  CASE 82'--inventory
   scriptret = countitem(retvals(0) + 1, item(), item$())
  CASE 83'--set hero stat
   stat(bound(retvals(0), 0, 40), bound(retvals(3), 0, 1), bound(retvals(1), 0, 13)) = retvals(2)
  CASE 84'--suspend box advance
   setbit gen(), 44, suspendboxadvance, 1
  CASE 85'--resume box advance
   setbit gen(), 44, suspendboxadvance, 0
  CASE 86'--advance text box
   GOSUB nextsay
  CASE 87'--set hero position
   IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
    catx(retvals(0)) = retvals(1) * 20
    caty(retvals(0)) = retvals(2) * 20
   END IF
  CASE 88'--set NPC position
   FOR i = 0 TO 299
    IF npcl(i + 600) - 1 = bound(retvals(0), 0, 36) THEN
     npcl(i + 0) = retvals(1) * 20
     npcl(i + 300) = retvals(2) * 20
    END IF
   NEXT i
  CASE 89'--swap by position
   doswap bound(retvals(0), 0, 40), bound(retvals(1), 0, 40), hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
   GOSUB vishero
  CASE 90'--find hero
   scriptret = findhero(retvals(0) + 1, hero(), 0, 40, 1)
  CASE 92'--days of play
   scriptret = gen(51)
  CASE 93'--hours of play
   scriptret = gen(52)
  CASE 94'--minutes of play
   scriptret = gen(53)
  CASE 95'--resume NPC walls
   setbit gen(), 44, suspendnpcwalls, 0
  CASE 96'--set hero Z
   catz(bound(retvals(0), 0, 3)) = retvals(1)

  END SELECT
 END SELECT
RETURN

