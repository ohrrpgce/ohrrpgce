DECLARE SUB playtimer ()
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
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

COMMON SHARED /trueglobals/ game$, buffer(), master(), gen()

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

'---GENERIC LOOP HEAD---
'setkeys
'DO
'setwait timing(), 100
'setkeys
'tog = tog XOR 1
'IF keyval(1) > 1 THEN GOTO donewhatever

'---GENERIC LOOP TAIL---
'SWAP vpage, dpage
'setvispage vpage
'copypage 3, dpage
'dowait
'LOOP

REM $STATIC
SUB addhero (who, slot, hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf())
DIM wbuf(100)
setpicstuf buffer(), 636, -1
loadset game$ + ".dt0" + CHR$(0), who - 1, 0
setpicstuf wbuf(), 200, -1
loadset game$ + ".itm" + CHR$(0), buffer(22), 0

'--do average level enforcement
IF buffer(21) < 0 THEN buffer(21) = averagelev(stat(), hero())

'--formally add hero
hero(slot) = who

'---MUST SET DEFAULT EQUIP---
wep = large(wbuf(48), 1)
FOR i = 0 TO 4
 eqstuf(slot, i) = 0
NEXT i
eqstuf(slot, 0) = buffer(22) + 1

FOR i = 0 TO 11
stat(slot, 0, i) = atlevel(buffer(21), buffer(23 + i * 2), buffer(24 + i * 2)) + wbuf(54 + i)
stat(slot, 1, i) = stat(slot, 0, i)
NEXT i
stat(slot, 0, 13) = wbuf(52)
stat(slot, 1, 13) = wbuf(53)
bmenu(slot, 0) = wep
FOR i = 1 TO 5
bmenu(slot, i) = 0
NEXT i
o = 1
FOR i = 0 TO 3
IF buffer(243 + i * 11) > 0 THEN bmenu(slot, o) = (i + 1) * -1: o = o + 1
NEXT i
bmenu(slot, o) = -10
FOR i = 0 TO 3
 FOR o = 0 TO 23
  spell(slot, i, o) = 0
  IF buffer(47 + (i * 48) + (o * 2)) > 0 AND buffer(48 + (i * 48) + (o * 2)) - 1 <= buffer(21) AND buffer(48 + (i * 48) + (o * 2)) > 0 THEN spell(slot, i, o) = buffer(47 + (i * 48) + (o * 2))
 NEXT o
NEXT i
name$(slot) = ""
FOR i = 1 TO buffer(0)
 name$(slot) = name$(slot) + CHR$(small(large(buffer(i), 0), 255))
NEXT i
resetlmp lmp(), slot, buffer(21)
stat(slot, 0, 12) = buffer(21)
exlev&(slot, 0) = 0
exlev&(slot, 1) = 30
FOR i = 1 TO stat(slot, 0, 12)
 exlev&(slot, 1) = exlev&(slot, 1) * 1.2 + 5
 IF exlev&(slot, 1) > 1000000 THEN exlev&(slot, 1) = 1000000
NEXT i
END SUB

FUNCTION atlevel (now, a0, a99)

'CLS : a = 80: b = 8500: PRINT : FOR i = 0 TO 99 STEP 5: PRINT i; " "; atlevel(i, a, b): LINE (640, i)-(640 - atlevel(i, a, b) / 100, i), 4: NEXT i

'atlevel = (.8 + now / 50) * now * ((a99 - a0) / 100) + a0
IF now < 0 THEN atlevel = 0: EXIT FUNCTION
atlevel = (.8 + now / 50) * now * ((a99 - a0) / 275.222) + a0

END FUNCTION

FUNCTION averagelev (stat(), hero())
 average = 0
 count = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN average = average + stat(i, 0, 12): count = count + 1
 NEXT i
 IF count > 0 THEN average = average / count
 averagelev = average
END FUNCTION

FUNCTION browse$ (fmask$, needf, dpage, vpage, bpage, timing(), tmp$)
browse$ = ""

DIM drive(26), drive$(26), tree$(255), treec(255), true$(255), about$(255), catfg(6), catbg(6)

limit = 255

catfg(0) = 9: catbg(0) = 8    'drives
catfg(1) = 9: catbg(1) = 8    'directories
catfg(2) = 9: catbg(2) = 0    'subdirectories
catfg(3) = 7: catbg(3) = 0    'files
catfg(4) = 11: catbg(4) = 8   'root
catfg(5) = 10: catbg(5) = 8   'special
catfg(6) = 8: catbg(6) = 0   'bad

drivetotal = drivelist(drive())
drive(26) = 15

'GOSUB vlabels
remember$ = STRING$(pathlength, 0): getstring remember$
IF RIGHT$(remember$, 1) <> "\" THEN remember$ = remember$ + "\"
nowdir$ = remember$

GOSUB context

treeptr = 0
treetop = 0

setkeys
DO
 setwait timing(), 80
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN GOTO donebrowse
 dummy = usemenu(treeptr, treetop, 0, treesize, 17)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  SELECT CASE treec(treeptr)
   CASE 0
    IF hasmedia(ASC(LEFT$(tree$(treeptr), 1)) - 64) THEN
     nowdir$ = LEFT$(tree$(treeptr), 3)
     GOSUB context
    END IF
   CASE 1
    'nowdir$ = LEFT$(tree$(1), 3)
    nowdir$ = LEFT$(tree$(0), 3)
    'FOR i = 2 TO treeptr
    FOR i = 1 TO treeptr
     nowdir$ = nowdir$ + tree$(i)
    NEXT i
    GOSUB context
   CASE 2
    nowdir$ = nowdir$ + tree$(treeptr) + "\"
    GOSUB context
   CASE 3
    browse$ = nowdir$ + true$(treeptr)
    EXIT FUNCTION
   CASE 4
    nowdir$ = ""
    GOSUB context
    FOR i = 0 TO drivetotal - 1
     IF drive(i) = 3 THEN treeptr = i
    NEXT i
  END SELECT
 END IF
 rectangle 4, 3, 312, 14, 9, dpage
 rectangle 5, 4, 310, 12, 1, dpage
 edgeprint nowdir$, 8, 6, 15, dpage
 rectangle 4, 185, 312, 14, 9, dpage
 rectangle 5, 186, 310, 12, 1, dpage
 edgeprint about$(treeptr), 8, 188, 15, dpage
 textcolor 15, 0
 printstr ">", 0, 20 + (treeptr - treetop) * 9, dpage
 FOR i = treetop TO small(treetop + 17, treesize)
  textcolor catfg(treec(i)), catbg(treec(i))
  a$ = tree$(i)
  DO WHILE LEN(a$) < 38 AND catbg(treec(i)) > 0
   a$ = a$ + " "
  LOOP
  printstr a$, 10, 20 + (i - treetop) * 9, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 IF needf = 1 THEN fadetopal master(), buffer(): needf = 0: setkeys
 dowait
LOOP

context:
 treesize = 0
 IF nowdir$ = "" THEN
 ' FOR i = 0 TO drivetotal - 1
 '  tree$(i) = drive$(i)
 '  treec(i) = 0
 ' NEXT i
 ' treesize = drivetotal - 1
 ELSE
  'tree$(treesize) = "[ROOT]"
  'treec(treesize) = 4
  a = ASC(LEFT$(nowdir$, 1)) - 64
  FOR i = 0 TO drivetotal - 1
   'IF a = drive(i) THEN tree$(treesize) = drive$(i)
   IF a = drive(i) THEN
    tree$(treesize) = CHR$(64 + drive(i)) + ":\"
   END IF
  NEXT i
  treec(treesize) = 0
  about$(treesize) = "Drive"
  a$ = RIGHT$(nowdir$, LEN(nowdir$) - 3)
  b$ = ""
  DO UNTIL a$ = ""
   b$ = b$ + LEFT$(a$, 1)
   a$ = RIGHT$(a$, LEN(a$) - 1)
   IF RIGHT$(b$, 1) = "\" THEN
    treesize = small(treesize + 1, limit)
    tree$(treesize) = b$
    treec(treesize) = 1
    about$(treesize) = "Directory"
    b$ = ""
   END IF
  LOOP
'---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
  findfiles nowdir$ + "*.*" + CHR$(0), 16, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
  OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #1
   DO UNTIL EOF(1)
    treesize = small(treesize + 1, limit)
    treec(treesize) = 2
    about$(treesize) = "Subdirectory"
    INPUT #1, tree$(treesize)
    tree$(treesize) = UCASE$(tree$(treesize))
    IF tree$(treesize) = "." OR tree$(treesize) = ".." OR RIGHT$(tree$(treesize), 4) = ".TMP" THEN treesize = treesize - 1
   LOOP
  CLOSE #1
  KILL tmp$ + "hrbrowse.tmp"
'---FIND ALL FILES IN FILEMASK---
  findfiles nowdir$ + fmask$ + CHR$(0), 32, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
  OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #1
   DO UNTIL EOF(1)
    treesize = small(treesize + 1, limit)
    treec(treesize) = 3
    INPUT #1, true$(treesize)
    true$(treesize) = LCASE$(true$(treesize))
    unlumpfile nowdir$ + true$(treesize) + CHR$(0), "browse.txt", tmp$, buffer()
    IF isfile(tmp$ + "browse.txt" + CHR$(0)) THEN
     setpicstuf buffer(), 40, -1
     loadset tmp$ + "browse.txt" + CHR$(0), 0, 0
     tree$(treesize) = STRING$(bound(buffer(0), 0, 38), " ")
     array2str buffer(), 2, tree$(treesize)
     loadset tmp$ + "browse.txt" + CHR$(0), 1, 0
     about$(treesize) = STRING$(bound(buffer(0), 0, 38), " ")
     array2str buffer(), 2, about$(treesize)
     KILL tmp$ + "browse.txt"
     IF LEN(tree$(treesize)) = 0 THEN tree$(treesize) = true$(treesize)
    ELSE
     tree$(treesize) = true$(treesize)
     about$(treesize) = ""
    END IF
   LOOP
  CLOSE #1
  KILL tmp$ + "hrbrowse.tmp"
 END IF

 '--alphabetize
 FOR o = treesize TO 2 STEP -1
  FOR i = 1 TO o
   IF (treec(i) = 2 OR treec(i) = 3 OR treec(i) = 6) AND (treec(i - 1) = 2 OR treec(i - 1) = 3 OR treec(i - 1) = 6) THEN
    IF ASC(LCASE$(LEFT$(tree$(i), 1))) < ASC(LCASE$(LEFT$(tree$(i - 1), 1))) THEN
     SWAP tree$(i), tree$(i - 1)
     SWAP treec(i), treec(i - 1)
     SWAP true$(i), true$(i - 1)
     SWAP about$(i), about$(i - 1)
    END IF
   END IF
  NEXT i
 NEXT o

 '--sort by type
 FOR o = treesize TO 2 STEP -1
  FOR i = 1 TO o
   IF (treec(i) = 2 OR treec(i) = 3 OR treec(i) = 6) AND (treec(i - 1) = 2 OR treec(i - 1) = 3 OR treec(i - 1) = 6) THEN
    IF treec(i) < treec(i - 1) THEN
     SWAP tree$(i), tree$(i - 1)
     SWAP treec(i), treec(i - 1)
     SWAP true$(i), true$(i - 1)
     SWAP about$(i), about$(i - 1)
    END IF
   END IF
  NEXT i
 NEXT o

 '--set cursor
 IF treeptr > treesize THEN treeptr = 0: treetop = 0
 FOR i = 1 TO treesize
  IF treec(i) = 1 OR treec(i) = 0 THEN treeptr = i
 NEXT i
 FOR i = treesize TO 2 STEP -1
  IF treec(i) = 3 THEN treeptr = i
 NEXT i
 treetop = bound(treetop, treeptr - 19, treeptr)

 widest = 0
 FOR i = 0 TO treesize
  IF LEN(tree$(i)) > widest THEN widest = LEN(tree$(i))
 NEXT i
RETURN

vlabels:
 FOR i = 0 TO drivetotal - 1
  IF isremovable(drive(i)) = 0 THEN
   drive$(i) = CHR$(64 + drive(i)) + ":\ (removable)"
  ELSE '--not removable--
   IF hasmedia(drive(i)) THEN
    findfiles CHR$(64 + drive(i)) + ":\*.*" + CHR$(0), 8, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
    OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #1
     IF LOF(1) THEN
      INPUT #1, a$
      a$ = UCASE$(a$)
      b$ = ""
      FOR j = 1 TO LEN(a$)
       IF MID$(a$, j, 1) <> "." THEN b$ = b$ + MID$(a$, j, 1)
      NEXT j
      drive$(i) = CHR$(64 + drive(i)) + ":\ (" + b$ + ")"
     END IF
    CLOSE #1
    KILL tmp$ + "hrbrowse.tmp"
   ELSE '--no media--
    drive$(i) = CHR$(64 + drive(i)) + ":\ (not ready)"
   END IF'--check media--
  END IF'--check removable--
 NEXT i
RETURN

donebrowse:

END FUNCTION

SUB calibrate (gotj(), joy(), dpage, vpage, timing())

state = 0
state$ = "Center Joystick and Press Button"
midx = 400
midy = 400
button = 0
disabled = 10

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 FOR i = 0 TO 1
  IF readjoy(joy(), i) THEN EXIT FOR
 NEXT i
 SELECT CASE button
 CASE 0'no button
  IF joy(3) = 0 THEN joy(13) = 3: joy(14) = 2: button = 1
  IF joy(2) = 0 THEN joy(13) = 2: joy(14) = 3: button = 1
 CASE 1'button down
  IF joy(2) <> 0 AND joy(3) <> 0 THEN button = 2
 CASE 2
  button = 0
 END SELECT
 disabled = disabled - SGN(disabled)
 SELECT CASE state
 CASE 0
  IF (button = 2) AND (disabled = 0) THEN
   midx = joy(0)
   midy = joy(1)
   state$ = "Push UP and Press Button"
   tx = 160
   ty = 45
   state = 1
  END IF
 CASE 1
  IF button = 2 THEN
   joy(9) = joy(1) + (midy - joy(1)) * .33
   state$ = "Push DOWN and Press Button"
   ty = 155
   state = 2
  END IF
 CASE 2
  IF button = 2 THEN
   joy(10) = joy(1) - (joy(1) - midy) * .33
   state$ = "Push LEFT and Press Button"
   tx = 50
   ty = 110
   state = 3
  END IF
 CASE 3
  IF button = 2 THEN
   joy(11) = joy(0) + (midx - joy(0)) * .33
   state$ = "Push RIGHT and Press Button"
   tx = 260
   state = 4
  END IF
 CASE 4
  IF button = 2 THEN
   joy(12) = joy(0) - (joy(0) - midx) * .33
   state$ = "Press the USE button"
   state = 5
  END IF
 CASE 5
  IF button = 2 THEN
   disabled = 4
   state$ = ""
   state = 6
  END IF
 CASE 6
  IF NOT disabled THEN EXIT DO
 END SELECT
 centerbox 160, 100, 100, 80, 1, dpage
 centerbox 160, 100, 20, 20, 3, dpage
 IF state > 0 THEN
  centerbox 160 + (joy(0) - midx) * .1, 100 + (joy(1) - midy) * .1, 10, 10, 15, dpage
 END IF
 IF state > 0 AND state < 5 THEN
  edgeprint "This way!", tx - 36, ty - 5, 10, dpage
 END IF
 edgeprint "Calibrate Joystick", 88, 8, 15, dpage
 edgeprint state$, 160 - 4 * LEN(state$), 174, 14 + tog, dpage
 pos$ = "X=" + LTRIM$(STR$(joy(0))) + " Y=" + LTRIM$(STR$(joy(1)))
 edgeprint pos$, 160 - 4 * LEN(pos$), 184, 14 + tog, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

END SUB

FUNCTION consumeitem (index, item(), item$())
 '--subtracts one of an item at a location. If the item is depleted, returns true. If there are some of the item left, it returns false
 consumeitem = 0
 lb = (item(index) AND 255)
 hb = INT(item(index) / 256) - 1
 item(index) = lb + (hb * 256)
 item$(index) = LEFT$(item$(index), 9) + RIGHT$(STR$(hb), 2)
 IF hb = 0 THEN item(index) = 0: item$(index) = "           ": consumeitem = -1
END FUNCTION

FUNCTION countitem (it, item(), item$())
 c = 0
 FOR o = 0 TO 199
  lb = (item(o) AND 255)
  hb = INT(item(o) / 256)
  IF it = lb AND hb > 0 THEN
   c = c + hb
  END IF
 NEXT o
 countitem = c
END FUNCTION

SUB cycletile (cycle(), tastuf(), ptr(), skip(), tag())

FOR i = 0 TO 1
 IF NOT istag(tag(), tastuf(1 + 20 * i), 0) THEN
  skip(i) = large(skip(i) - 1, 0)
  IF skip(i) = 0 THEN
   notstuck = 10
   DO
    SELECT CASE tastuf(2 + 20 * i + ptr(i))
     CASE 0
      ptr(i) = 0
      cycle(i) = 0
     CASE 1
      cycle(i) = cycle(i) - tastuf(11 + 20 * i + ptr(i)) * 16
      ptr(i) = loopvar(ptr(i), 0, 8, 1)
     CASE 2
      cycle(i) = cycle(i) + tastuf(11 + 20 * i + ptr(i)) * 16
      ptr(i) = loopvar(ptr(i), 0, 8, 1)
     CASE 3
      cycle(i) = cycle(i) + tastuf(11 + 20 * i + ptr(i))
      ptr(i) = loopvar(ptr(i), 0, 8, 1)
     CASE 4
      cycle(i) = cycle(i) - tastuf(11 + 20 * i + ptr(i))
      ptr(i) = loopvar(ptr(i), 0, 8, 1)
     CASE 5
      skip(i) = tastuf(11 + 20 * i + ptr(i))
      ptr(i) = loopvar(ptr(i), 0, 8, 1)
     CASE 6
      IF istag(tag(), tastuf(11 + 20 * i + ptr(i)), 0) THEN
       ptr(i) = loopvar(ptr(i), 0, 8, 1)
      ELSE
       ptr(i) = 0
       cycle(i) = 0
      END IF
     CASE ELSE
      ptr(i) = loopvar(ptr(i), 0, 8, 1)
    END SELECT
    notstuck = large(notstuck - 1, 0)
   LOOP WHILE notstuck AND skip(i) = 0
  END IF
 END IF
NEXT i

END SUB

SUB debug (s$)
 OPEN "g_debug.txt" FOR APPEND AS #3
  PRINT #3, s$
 CLOSE #3
END SUB

SUB delitem (it, item(), item$())
 FOR o = 0 TO 199
  lb = (item(o) AND 255)
  hb = INT(item(o) / 256)
  IF it = lb AND hb > 0 THEN
   hb = hb - 1: IF hb = 0 THEN lb = 0
   item(o) = lb + (hb * 256)
   itstr o, item(), item$()
   EXIT FOR
  END IF
 NEXT o
END SUB

SUB doswap (s, d, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf())

'---swap hmask
a = readbit(hmask(), 0, d)
setbit hmask(), 0, d, readbit(hmask(), 0, s)
setbit hmask(), 0, s, a

'---Hero index
SWAP hero(s), hero(d)

'---Battle menu
FOR i = 0 TO 5
 SWAP bmenu(s, i), bmenu(d, i)
NEXT i

'---Spell lists
FOR i = 0 TO 3
 FOR o = 0 TO 24
  SWAP spell(s, i, o), spell(d, i, o)
 NEXT o
NEXT i

'---hero stats
FOR i = 0 TO 1
 FOR o = 0 TO 13
  SWAP stat(s, i, o), stat(d, i, o)
 NEXT o
NEXT i

'---Level-MP
FOR i = 0 TO 7
 SWAP lmp(s, i), lmp(d, i)
NEXT i

'---Experience
FOR i = 0 TO 1
 SWAP exlev&(s, i), exlev&(d, i)
NEXT i

'--name
SWAP name$(s), name$(d)

'---Equipment
FOR i = 0 TO 4
 SWAP eqstuf(s, i), eqstuf(d, i)
NEXT i

'---Swap palettes

'---RELOAD PICTURES---
reloadhwp hero()

'hmask(), hero(40), bmenu(40,5), spell(40,3,24), stat(40,1,13), lmp(40,7), exlev&(40,1), name$(40), eqstuf(40,4)
END SUB

SUB drawsay (saybit(), sayenh(), dpage, say$(), showsay, choose$(), choosep)
IF readbit(saybit(), 0, 1) = 0 THEN
 IF readbit(saybit(), 0, 2) = 0 THEN
  centerfuz 160, 48 + (sayenh(0) * 4) - (sayenh(1) * 2), 312, 88 - (sayenh(1) * 4), sayenh(3) + 1, dpage
 ELSE
  centerbox 160, 48 + (sayenh(0) * 4) - (sayenh(1) * 2), 312, 88 - (sayenh(1) * 4), sayenh(3) + 1, dpage
 END IF '---TO FUZZ OR NOT TO FUZZ?-----
END IF
col = 15: IF sayenh(2) > 0 THEN col = sayenh(2)
FOR i = 0 TO 8 - showsay
 edgeprint say$(i), 7, (8 + i * 10) + (sayenh(0) * 4), col, dpage
NEXT i
IF showsay > 1 THEN showsay = showsay - 1
IF readbit(saybit(), 0, 0) THEN
 tempy = 100 + (sayenh(0) * 4) - (sayenh(1) * 4)
 IF tempy > 160 THEN tempy = 20
 centerbox 160, tempy + 12, 10 + large(LEN(choose$(0)) * 8, LEN(choose$(1)) * 8), 24, sayenh(3) + 1, dpage
 FOR i = 0 TO 1
  col = 7: IF choosep = i THEN col = 14 + tog
  edgeprint choose$(i), xstring(choose$(i), 160), tempy + 2 + (i * 10), col, dpage
 NEXT i
END IF
END SUB

SUB edgeprint (s$, x, y, c, p)
textcolor 240, 0
printstr s$, x, y + 1, p
printstr s$, x + 1, y, p
printstr s$, x + 2, y + 1, p
printstr s$, x + 1, y + 2, p
textcolor c, 0
printstr s$, x + 1, y + 1, p
END SUB

SUB evalherotag (tag(), herobits(), hero(), stat(), leader)
FOR i = 0 TO large(gen(35), 59)
 FOR j = 0 TO 3
  IF herobits(i, j) > 1 THEN setbit tag(), 0, herobits(i, j), 0
 NEXT j
 FOR j = 0 TO 40
  IF hero(j) - 1 = i THEN
   IF herobits(i, 0) > 1 THEN setbit tag(), 0, herobits(i, 0), 1 '---HAVE HERO
   IF herobits(i, 1) > 1 AND stat(j, 0, 0) THEN setbit tag(), 0, herobits(i, 1), 1 '---IS ALIVE
   IF herobits(i, 2) > 1 AND j = leader THEN setbit tag(), 0, herobits(i, 2), 1 '---IS LEADER
   IF herobits(i, 3) > 1 AND j < 4 THEN setbit tag(), 0, herobits(i, 3), 1 '---IN PARTY
  END IF
 NEXT j
NEXT i

'--this may not be the best place to do this, but here we make sure that you do not have an all-dead party
falive = -1
fhave = -1
FOR i = 3 TO 0 STEP -1
 IF hero(i) > 0 THEN
  fhave = i
  IF stat(i, 1, 0) > 0 THEN falive = i
 END IF
NEXT i
IF falive = -1 THEN
 stat(fhave, 1, 0) = 1
END IF

'292     have hero tag
'293     is alive tag
'294     is leader tag
'295     is in active party tag
END SUB

SUB evalitemtag (tag(), itembits(), hero(), eqstuf(), item())

FOR i = 0 TO 255
 IF itembits(i, 4) > 0 THEN
  FOR j = 0 TO 3
   IF itembits(i, j) > 1 THEN setbit tag(), 0, itembits(i, j), 0
  NEXT j
  FOR j = 0 TO 199
   lb = (item(j) AND 255)
   IF i = lb - 1 THEN
    IF itembits(i, 0) > 1 THEN setbit tag(), 0, itembits(i, 0), 1  'you have it
    IF itembits(i, 1) > 1 THEN setbit tag(), 0, itembits(i, 1), 1 'it is in your inventory
    EXIT FOR
   END IF
  NEXT j
  FOR j = 0 TO 40
   FOR k = 0 TO 4
    IF i = eqstuf(j, k) - 1 THEN
     IF itembits(i, 0) > 1 THEN setbit tag(), 0, itembits(i, 0), 1  'you have it
     IF itembits(i, 2) > 1 THEN setbit tag(), 0, itembits(i, 2), 1  'it is equipped
     IF j < 4 AND itembits(i, 3) > 1 THEN setbit tag(), 0, itembits(i, 3), 1   'it is equipped by an active hero
     EXIT FOR
    END IF
   NEXT k
  NEXT j
 END IF '---Only checks items with names
NEXT i

'74      when have tag
'75      is in inventory
'76      is equiped tag
'77      is equiped by hero in active party

END SUB

SUB fatalerror (e$)

setvispage 0
centerbox 160, 100, 300, 180, 3, 0
edgeprint e$, xstring(e$, 160), 20, 15, 0
edgeprint "Press ESC to cleanly close GAME.EXE", 15, 40, 7, 0
edgeprint "or any other key to ignore the", 15, 50, 7, 0
edgeprint "error and try to continue playing.", 15, 60, 7, 0

w = getkey

IF w = 1 THEN
 '--close digital audio file
 'closefile
 '--close current BAM file
 closemusic
 '--reset FM synth chip
 resetfm
 '--replace Mode-X with previous screen mode
 restoremode
 '--display error message
 PRINT e$
 '--crash out
 SYSTEM
END IF

END SUB

FUNCTION findhero (who, hero(), f, l, d)
 temp = -1
 FOR i = f TO l STEP d
  IF hero(i) = who OR (who = -1 AND hero(i)) THEN temp = i: EXIT FOR
 NEXT i
 findhero = temp
END FUNCTION

SUB getmapname (mapname$, m)
 setpicstuf buffer(), 80, -1
 loadset game$ + ".mn" + CHR$(0), m, 0
 a$ = STRING$(small((buffer(0) AND 255), 39), " ")
 array2str buffer(), 1, a$
 mapname$ = a$
END SUB

SUB getnames (stat$())
IF isfile(game$ + ".stt" + CHR$(0)) THEN
 OPEN game$ + ".stt" FOR BINARY AS #1
 max = 32
 FOR i = 0 TO max
 temp$ = " "
 GET #1, 1 + (11 * i), temp$
 temp = 0: IF temp$ <> "" THEN temp = ASC(temp$)
 stat$(i) = ""
 FOR o = 1 TO temp
 temp$ = " "
 GET #1, 1 + (11 * i) + o, temp$
 stat$(i) = stat$(i) + temp$
 NEXT o
 NEXT i
 CLOSE #1
END IF
END SUB

SUB heroswap (all, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf(), timing(), carray(), vpage, dpage, csetup(), gotm, gotj(), mouse(), joy(), pal())

'Page 2 has the npcs, which dont need to be reloaded afterward
'Page 3 holds a copy of vpage.

DIM swindex(40), swname$(40), paloff(3)

swapme = -1
ecsr = -1

GOSUB resetswap

IF hero(acsr) THEN info$ = name$(acsr) ELSE info$ = ""

setkeys
DO
 setwait timing(), 80
 setkeys
 tog = tog XOR 1
 playtimer
 control carray(), csetup(), gotm, gotj(), mouse(), joy(), 0, 0, 1, timing()
 IF carray(5) > 1 THEN
  IF swapme >= 0 THEN
   swapme = -1
  ELSE
   EXIT SUB
  END IF
 END IF
 IF all THEN
  IF carray(0) > 1 THEN
   IF ecsr < 0 THEN
    ecsr = la
    GOSUB refreshemenu
   ELSE
    ecsr = loopvar(ecsr, -1, la, -1)
    GOSUB refreshemenu
   END IF
  END IF
  IF carray(1) > 1 THEN
   IF ecsr < 0 THEN
    ecsr = 0
    GOSUB refreshemenu
   ELSE
    ecsr = loopvar(ecsr, -1, la, 1)
    GOSUB refreshemenu
   END IF
  END IF
 END IF
 IF carray(2) > 1 AND ecsr < 0 THEN
  acsr = loopvar(acsr, 0, 3, -1)
  IF hero(acsr) AND ecsr < 0 THEN info$ = name$(acsr) ELSE info$ = ""
 END IF
 IF carray(3) > 1 AND ecsr < 0 THEN
  acsr = loopvar(acsr, 0, 3, 1)
  IF hero(acsr) AND ecsr < 0 THEN info$ = name$(acsr) ELSE info$ = ""
 END IF
 IF carray(4) > 1 THEN
  IF swapme = -1 THEN
   IF ecsr < 0 THEN
    swapme = acsr
   ELSE
    swapme = 4 + ecsr
   END IF
  ELSE
   DO
    IF swapme < 4 THEN
     IF (numhero < 2 AND ecsr = la) OR (readbit(hmask(), 0, swapme) AND ecsr > -1) THEN EXIT DO
    ELSE
     IF swapme - 4 = la AND ecsr = -1 AND numhero < 2 THEN EXIT DO
     IF readbit(hmask(), 0, acsr) AND ecsr = -1 THEN EXIT DO
    END IF
    '---IDENTIFY DESTINATION---
    IF ecsr < 0 THEN
     temp = acsr
    ELSE
     temp = swindex(ecsr)
    END IF
    '---IDENTIFY SOURCE---
    IF swapme < 4 THEN
     temp2 = swapme
    ELSE
     temp2 = swindex(swapme - 4)
    END IF
    doswap temp, temp2, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf()
    swapme = -1
    GOSUB resetswap
    EXIT DO
   LOOP
  END IF
 END IF

 GOSUB showswapmenu
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

refreshemenu:
 IF ecsr < top THEN top = large(ecsr, 0)
 IF ecsr > top + 7 THEN top = ecsr - 7
 IF hero(acsr) AND ecsr < 0 THEN info$ = name$(acsr) ELSE info$ = ""
RETURN

'---DRAWS SWAP MENU AND CURRENT SELECTION----
showswapmenu:
 centerbox 160, 66, 130, 38, 1, dpage
 o = 0
 FOR i = 0 TO 3
 IF i = swapme OR hero(i) > 0 THEN rectangle 105 + (30 * i), 60, 20, 20, 17, dpage
  IF hero(i) THEN
   loadsprite buffer(), 0, 200 * 4, o * 5, 20, 20, 2
   drawsprite buffer(), 0, pal(), paloff(i) * 16, 105 + (30 * i), 60 + (i = swapme) * 6, dpage
   o = o + 1
  END IF
 NEXT i
 IF ecsr < 0 THEN edgeprint CHR$(24), 111 + 30 * acsr, 52, 14 + tog, dpage
 IF all THEN
  centerbox 160, 100 + small(high, 8) * 5, wide * 8 + 16, small(high, 8) * 10 + 10, 1, dpage
  FOR i = top TO small(top + 7, la)
   c = 7
   IF swapme = i + 4 THEN c = 6
   IF ecsr = i THEN
    c = 14 + tog
    IF swapme = i + 4 THEN c = 6 + 8 * tog
   END IF
   IF swapme > -1 AND swapme < 4 THEN
    IF (numhero < 2 AND i = la) OR readbit(hmask(), 0, acsr) THEN c = 8 + ((ecsr = i) * tog)
   END IF
   edgeprint swname$(i), xstring(swname$(i), 160), 100 + (i - top) * 10, c, dpage
  NEXT i
 END IF
 IF LEN(info$) THEN
  centerbox 160, 44, (LEN(info$) + 2) * 8, 14, 1, dpage
  edgeprint info$, xstring(info$, 160), 39, 15, dpage
 END IF
RETURN

'---MAPS OUT ONLY VALID SWAPABLE HEROS PLUS A BLANK-----
resetswap:
 la = -1
 wide = 0
 FOR i = 4 TO 40
  IF readbit(hmask(), 0, i) = 0 AND hero(i) THEN
   la = la + 1
   swindex(la) = i
   swname$(la) = name$(i)
   wide = large(wide, LEN(swname$(la)))
  END IF
 NEXT i
 la = la + 1
 FOR i = 40 TO 4 STEP -1
  IF hero(i) = 0 THEN
   swindex(la) = i
   swname$(la) = "-REMOVE-"
   wide = large(wide, 7)
  END IF
 NEXT i
 high = small(8, la + 1)
 numhero = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   numhero = numhero + 1
   setpicstuf buffer(), 636, -1
   loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
   paloff(i) = buffer(20)
  END IF
 NEXT i
 IF hero(acsr) AND ecsr < 0 THEN info$ = name$(acsr) ELSE info$ = ""
RETURN

END SUB

FUNCTION howmanyh (hero(), f, l)
 temp = 0
 FOR i = f TO l
  IF hero(i) THEN temp = temp + 1
 NEXT i
 howmanyh = temp
END FUNCTION

SUB intgrabber (n, min, max, less, more)
 STATIC clip
 IF keyval(more) > 1 THEN n = loopvar(n, min, max, 1): EXIT SUB
 IF keyval(less) > 1 THEN n = loopvar(n, min, max, -1): EXIT SUB
 s = SGN(n)
 n$ = RIGHT$(STR$(n), LEN(STR$(n)) - 1)
 IF keyval(14) > 1 AND LEN(n$) > 0 THEN n$ = LEFT$(n$, LEN(n$) - 1)
 FOR i = 1 TO 9
  IF keyval(i + 1) > 1 THEN n$ = n$ + RIGHT$(STR$(i), LEN(STR$(i)) - 1)
 NEXT i
 IF keyval(11) > 1 THEN n$ = n$ + "0"
 IF min < 0 THEN IF keyval(12) > 1 OR keyval(13) > 1 OR keyval(74) > 1 OR keyval(78) > 1 THEN s = s * -1
 capper& = INT(VAL(n$))
 IF capper& > 32767 THEN capper& = 32767
 n = capper&
 IF s THEN n = n * s
 'CLIPBOARD
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip = n
 IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN n = clip
 n = large(min, n)
 n = small(max, n)
END SUB

FUNCTION istag (tag(), num, zero)
 istag = (readbit(tag(), 0, ABS(num)) = SGN(SGN(num) + 1))
 IF num = 1 THEN istag = 0
 IF num = -1 THEN istag = -1
 IF num = 0 THEN istag = zero
END FUNCTION

FUNCTION large (n1, n2)
large = n1
IF n2 > n1 THEN large = n2
END FUNCTION

SUB loaddoor (map, door())
IF gen(95) < 2 THEN
'--BLODDY BACKWARDS COMPATABILITY--
' DEF SEG = VARSEG(buffer(0)): BLOAD game$ + ".dor", VARPTR(buffer(0))
' FOR i = 0 TO 299
'  door(i) = buffer(map * 300 + i)
' NEXT
ELSE
'--THE RIGHT WAY--
 setpicstuf door(), 600, -1
 loadset game$ + ".dox" + CHR$(0), map, 0
END IF
END SUB

SUB loadgame (slot, sourcerpg$, map, x(), y(), d(), foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), a(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), global())

sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
setpicstuf buffer(), 30000, -1
loadset sg$ + CHR$(0), slot * 2, 0

version = buffer(0)
IF version < 2 OR version > 3 THEN EXIT SUB
map = buffer(1)
x(0) = buffer(2)
y(0) = buffer(3)
d(0) = buffer(4)
foep = buffer(5)
leader = buffer(6)
mapx = buffer(7)
mapy = buffer(8)

temp$ = ""
FOR i = 0 TO 24
 IF buffer(i + 9) < 0 OR buffer(i + 9) > 255 THEN buffer(i + 9) = 0
 IF buffer(i + 9) > 0 THEN temp$ = temp$ + CHR$(buffer(i + 9))
NEXT i
gold& = VAL(temp$)

z = 34
FOR i = 0 TO 500
 gen(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 2100
 npcl(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 126
 tag(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 40
 hero(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 500
 a(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 0 TO 13
   stat(i, o, j) = buffer(z): z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 5
  bmenu(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 24
   spell(i, o, j) = buffer(z): z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 7
  lmp(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  temp$ = ""
  FOR j = 0 TO 25
   IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
   IF buffer(z) > 0 THEN temp$ = temp$ + CHR$(buffer(z))
   z = z + 1
  NEXT j
  exlev&(i, o) = VAL(temp$)
 NEXT o
NEXT i
FOR i = 0 TO 40
 temp$ = ""
 FOR j = 0 TO 16
  IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
  IF buffer(z) > 0 THEN temp$ = temp$ + CHR$(buffer(z))
  z = z + 1
 NEXT j
 name$(i) = temp$
NEXT i
FOR i = -3 TO 199
 item(i) = buffer(z): z = z + 1
NEXT i
FOR i = -3 TO 199
 temp$ = ""
 FOR j = 0 TO 11
  IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
  IF buffer(z) > 0 THEN temp$ = temp$ + CHR$(buffer(z))
  z = z + 1
 NEXT j
 item$(i) = temp$
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 4
  eqstuf(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i

'RECORD 2

setpicstuf buffer(), 30000, -1
loadset sg$ + CHR$(0), slot * 2 + 1, 0

z = 0

FOR i = 0 TO 99
 FOR o = 0 TO 49
  stock(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 3
 hmask(i) = buffer(z): z = z + 1
NEXT i
FOR i = 1 TO 3
 x(i) = buffer(z): z = z + 1
 y(i) = buffer(z): z = z + 1
 d(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 1024
 global(i) = buffer(z): z = z + 1
NEXT i

'---BLODDY BACKWARD COMPATABILITY---
'fix doors...
IF version = 2 THEN gen(95) = 3

'ALL THE STUFF THAT MUST BE SAVED
'map,x,y,d,foep,leader,gold&,gen(500),npcl(2100),tag(126),hero(40),a(500),stat(40,1,13),bmenu(40,5),spell(40,3,24),lmp(40,7),exlev&(40,1),name$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4),hmask()
'ALL THE STUFF THAT MUST BE PASSED
'slot,map,x,y,d,foep,leader,gold&,npcl(),tag(),hero(),a(),stat(),bmenu(),spell(),lmp(),exlev&(),name$(),item(),item$(),eqstuf()
'30000
END SUB

SUB loadtanim (n, tastuf())
 setpicstuf tastuf(), 80, -1
 loadset game$ + ".tap" + CHR$(0), n, 0
END SUB

FUNCTION loopvar (var, min, max, inc)
a = var + inc
IF a > max THEN a = a - ((max - min) + 1): loopvar = a: EXIT FUNCTION
IF a < min THEN a = a + ((max - min) + 1): loopvar = a: EXIT FUNCTION
loopvar = a
END FUNCTION

SUB minimap (vpage, dpage, scroll(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mx, my, timing(), gmap(), x, y, tastuf())

loadpage game$ + ".til" + CHR$(0), gmap(0), 3
centerfuz 160, 100, 304, 184, 1, vpage
centerbox 159, 99, scroll(0) + 3, scroll(1) + 3, 15, vpage
setmapdata scroll(), buffer(), 0, 0
abort = 0
setkeys
FOR i = 0 TO scroll(1) - 1
 setkeys
 playtimer
 control carray(), csetup(), gotm, gotj(), mouse(), joy(), 0, 0, 1, timing()
 FOR o = 0 TO scroll(0) - 1
  IF carray(5) > 1 OR carray(4) > 1 THEN abort = 1
  block = readmapblock(o, i)
  IF block > 207 THEN block = (block - 207) + tastuf(20)
  IF block > 159 THEN block = (block - 159) + tastuf(0)
  mx = block - (INT(block / 16) * 16)
  my = INT(block / 16)
  loadsprite buffer(), 0, INT(RND * 7) + 7 + (mx * 20), INT(RND * 7) + 7 + (my * 20), 1, 1, 3
  stosprite buffer(), 0, 160 - INT(scroll(0) * .5) + o, 100 - INT(scroll(1) * .5) + i, vpage
  IF abort = 1 THEN EXIT FOR
 NEXT
 IF abort = 1 THEN EXIT FOR
NEXT
IF abort = 1 THEN setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: EXIT SUB
copypage vpage, dpage
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 0, 0, 1, timing()
i = 1: DO
IF keyval(i) > 1 OR carray(4) > 1 OR carray(5) > 1 THEN clearpage dpage: setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: EXIT SUB
i = i + 1: LOOP UNTIL i > 88
rectangle 160 - (scroll(0) * .5) + (x / 20), 100 - (scroll(1) * .5) + (y / 20), 1, 1, 15 + (tog * 5), dpage
copypage dpage, vpage
dowait
LOOP
END SUB

FUNCTION movdivis (xygo)
 IF (xygo \ 20) * 20 = xygo AND xygo <> 0 THEN
  movdivis = -1
 ELSE
  movdivis = 0
 END IF
END FUNCTION

FUNCTION onwho (w$, hero(), a(), dpage, vpage, timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx, x, mapy, y, pal())
DIM owpal(3)
o = -1
w = -1
FOR i = 3 TO 0 STEP -1
 IF hero(i) > 0 THEN
  o = o + 1
  w = i
  setpicstuf a(), 636, -1
  loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
  owpal(i) = a(20)
 END IF
NEXT i
IF o = 0 THEN onwho = w: setkeys: EXIT FUNCTION
copypage dpage, 3
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx - x, mapy - y, 0, timing()
wtog = loopvar(wtog, 0, 3, 1)
IF carray(5) > 1 THEN
 FOR i = 0 TO 7: carray(i) = 0: NEXT i
 onwho = -1: setkeys: EXIT FUNCTION
END IF
IF carray(2) > 1 THEN DO: w = loopvar(w, 0, 3, -1): LOOP UNTIL hero(w) > 0
IF carray(3) > 1 THEN DO: w = loopvar(w, 0, 3, 1): LOOP UNTIL hero(w) > 0
IF carray(4) > 1 THEN setkeys: FOR i = 0 TO 7: carray(i) = 0: NEXT i: onwho = w: EXIT FUNCTION
centerbox 160, 100, 140, 52, 1, dpage
o = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  wt = 0: IF w = i THEN wt = INT(wtog / 2)
  loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
  drawsprite buffer(), 0, pal(), owpal(i) * 16, 100 + i * 30, 100, dpage
  o = o + 1
 END IF
NEXT i
edgeprint CHR$(25), 106 + w * 30, 90, 14 + tog, dpage
edgeprint w$, xstring(w$, 160), 80, 15, dpage
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
dowait
LOOP

END FUNCTION

FUNCTION range (n, r)
a = (n / 100) * r
range = n + INT(RND * (a * 2)) - a
END FUNCTION

SUB reinitnpc (remember, npcl(), map, filenum$())
 IF remember THEN
  FOR i = 0 TO 299
   buffer(i + 0) = npcl(i + 0)
   buffer(i + 300) = npcl(i + 300)
   buffer(i + 900) = npcl(i + 900)
  NEXT i
 END IF
 xbload game$ + ".l" + filenum$(map), npcl(), "Oh No! Map" + filenum$(map) + " NPC locations are missing"
 IF remember THEN
  FOR i = 0 TO 299
   npcl(i + 0) = buffer(i + 0)
   npcl(i + 300) = buffer(i + 300)
   npcl(i + 900) = buffer(i + 900)
  NEXT i
 END IF
END SUB

SUB reloadhwp (hero())
 o = 0
 FOR i = 0 TO 3 '-----RELOAD HERO WALKABOUT GRAPHICS-----------
  rectangle 0, 5 * i, 320, 5, 0, 2
  IF hero(i) > 0 THEN
   setpicstuf buffer(), 636, -1
   loadset game$ + ".dt0" + CHR$(0), hero(i) - 1, 0
   temp = buffer(19)
   setpicstuf buffer(), 1600, 2
   loadset game$ + ".pt4" + CHR$(0), temp, 5 * o
   o = o + 1
  END IF
 NEXT i
END SUB

SUB resetgame (map, x, y, d, foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), a(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), showsay, scrat(), global(), nowscript, scriptout$)
map = 0
x = 0
y = 0
d = 0
foep = 0
leader = 0
mapx = 0
mapy = 0
gold& = 0
showsay = 0
nowscript = 0
scriptout$ = ""
'--return gen to defaults
xbload game$ + ".gen", gen(), "General data is missing from " + game$

FOR i = 0 TO 2100
 npcl(i) = 0
NEXT i
FOR i = 0 TO 126
 tag(i) = 0
NEXT i
FOR i = 0 TO 40
 hero(i) = 0
NEXT i
FOR i = 0 TO 500
 a(i) = 0
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 0 TO 13
   stat(i, o, j) = 0
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 5
  bmenu(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 24
   spell(i, o, j) = 0
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 7
  lmp(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  exlev&(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 40
 name$(i) = ""
NEXT i
FOR i = -3 TO 199
 item(i) = 0
NEXT i
FOR i = -3 TO 199
 item$(i) = ""
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 4
  eqstuf(i, o) = 0
 NEXT o
NEXT i

'RECORD 2 (applies only to saves)

FOR i = 0 TO 99
 FOR o = 0 TO 49
  stock(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 3
 hmask(i) = 0
NEXT i
FOR i = 0 TO 1024
 global(i) = 0
NEXT i
FOR i = 0 TO 128
 FOR o = 0 TO 9
  scrat(i, o) = 0
 NEXT o
NEXT i

'ALL THE STUFF THAT MUST BE RESET
'map,x,y,d,foep,leader,gold&,gen(500),npcl(2100),tag(126),hero(40),a(500),stat(40,1,13),bmenu(40,5),spell(40,3,24),lmp(40,7),exlev&(40,1),name$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4),hmask()
'30000

END SUB

SUB resetlmp (lmp(), slot, lev)
FOR i = 0 TO 7
 lmp(slot, i) = 0
NEXT i
o = 0: j = 0
FOR i = 0 TO lev
 lmp(slot, o) = lmp(slot, o) + 1
 o = o + 1
 IF o > j THEN o = 0: j = j + 1
 IF j > 7 THEN j = 0
NEXT i
END SUB

SUB rpgversion (v)
 current = 3
 'last added password scattertable

 IF v = current THEN EXIT SUB
 needf = 1
 clearpage 0
 clearpage 1
 setvispage 0
 centerbox 160, 100, 240, 100, 3, 0
 IF v < current THEN
  edgeprint "Obsolete RPG File", 52, 70, 14, 0
  textcolor 7, 0
  printstr "this game was created with", 52, 82, 0
  printstr "an obsolete version of the", 52, 90, 0
  printstr "OHRRPGCE. It may not run", 52, 98, 0
  printstr "as intended.", 52, 106, 0
 END IF
 IF v > current THEN
  edgeprint "Unsupported RPG File", 52, 70, 15, 0
  textcolor 7, 0
  printstr "this game has features", 52, 82, 0
  printstr "that are not supported in", 52, 90, 0
  printstr "this version of the", 52, 98, 0
  printstr "OHRRPGCE. Download the", 52, 106, 0
  printstr "latest version at", 52, 114, 0
  printstr "http://HamsterRepublic.com", 52, 122, 0
 END IF
 fadetopal master(), buffer()
 w = getkey
 fadeto buffer(), 0, 0, 0
END SUB

FUNCTION runscript (n, index, newcall, script(), heap(), scrat())
runscript = -1
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

IF index > 127 THEN
 scripterr "interpreter overloaded"
 runscript = 0
 EXIT FUNCTION
END IF

IF newcall AND index > 0 THEN
 IF n = scrat(index - 1, scrid) THEN scripterr "script" + STR$(n) + " is already running"
 runscript = 0
 EXIT FUNCTION
END IF

FOR i = 2 TO 5
 scrat(index, i) = 0
NEXT i
scrat(index, scrdepth) = 0
scrat(index, scrid) = n

IF unlumpone(game$ + ".hsp", LTRIM$(STR$(n)) + ".hsx", "playing.tmp\script.___") THEN
 f = FREEFILE
 OPEN "playing.tmp\script.___" FOR BINARY AS #f
  GET #f, 1, skip
  GET #f, 3, lvars
  scrat(index + 1, scroff) = scrat(index, scroff) + (LOF(f) - skip) / 2
  IF scrat(index + 1, scroff) >= 2048 THEN
   scripterr "Script buffer overflow"
   CLOSE #f
   runscript = 0
   EXIT FUNCTION
  END IF
  FOR i = skip TO LOF(f) STEP 2
   GET #f, 1 + i, script(scrat(index, scroff) + ((i - skip) / 2))
  NEXT i
 CLOSE #f
 IF isfile("playing.tmp\script.___" + CHR$(0)) THEN KILL "playing.tmp\script.___"
ELSE
 scripterr "failed to unlump " + LTRIM$(STR$(n)) + ".hsx"
END IF

scrat(index + 1, scrheap) = scrat(index, scrheap) + (lvars + 1)
IF scrat(index + 1, scrheap) >= 2048 THEN
 scripterr "Script heap overflow"
 runscript = 0
 EXIT FUNCTION
END IF

FOR i = 1 TO lvars
 heap(scrat(index, scrheap) + (i - 1)) = 0
NEXT i

scrat(index, scrstate) = stread

'--suspend the previous script
IF newcall AND index > 0 THEN scrat(index - 1, scrstate) = scrat(index - 1, scrstate) * -1

END FUNCTION

SUB savegame (slot, sourcerpg$, map, x(), y(), d(), foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), a(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), global())

'--FLUSH BUFFER---
FOR i = 0 TO 16000
 buffer(i) = 0
NEXT i

buffer(0) = 3        'SAVEGAME VERSION NUMBER
buffer(1) = map
buffer(2) = x(0)
buffer(3) = y(0)
buffer(4) = d(0)
buffer(5) = foep
buffer(6) = leader
buffer(7) = mapx
buffer(8) = mapy

temp$ = STR$(gold&)
FOR i = 0 TO 24
 IF i < LEN(temp$) THEN
  IF MID$(temp$, i + 1, 1) <> "" THEN buffer(i + 9) = ASC(MID$(temp$, i + 1, 1))
 ELSE
  buffer(i + 9) = 0
 END IF
NEXT i

z = 34
FOR i = 0 TO 500
 buffer(z) = gen(i): z = z + 1
NEXT i
FOR i = 0 TO 2100
 buffer(z) = npcl(i): z = z + 1
NEXT i
FOR i = 0 TO 126
 buffer(z) = tag(i): z = z + 1
NEXT i
FOR i = 0 TO 40
 buffer(z) = hero(i): z = z + 1
NEXT i
FOR i = 0 TO 500
 buffer(z) = a(i): z = z + 1
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 0 TO 13
   buffer(z) = stat(i, o, j): z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 5
  buffer(z) = bmenu(i, o): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 24
   buffer(z) = spell(i, o, j): z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 7
  buffer(z) = lmp(i, o): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  temp$ = STR$(exlev&(i, o))
  FOR j = 0 TO 25
   IF j < LEN(temp$) THEN
    IF MID$(temp$, j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(temp$, j + 1, 1))
   ELSE
    buffer(z) = 0
   END IF
   z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 temp$ = name$(i)
 FOR j = 0 TO 16
  IF j < LEN(temp$) THEN
   IF MID$(temp$, j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(temp$, j + 1, 1))
  END IF
  z = z + 1
 NEXT j
NEXT i
FOR i = -3 TO 199
 buffer(z) = item(i): z = z + 1
NEXT i
FOR i = -3 TO 199
 temp$ = item$(i)
 FOR j = 0 TO 11
  IF j < LEN(temp$) THEN
   IF MID$(temp$, j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(temp$, j + 1, 1))
  END IF
  z = z + 1
 NEXT j
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 4
  buffer(z) = eqstuf(i, o): z = z + 1
 NEXT o
NEXT i
setpicstuf buffer(), 30000, -1
sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
storeset sg$ + CHR$(0), slot * 2, 0

'---RECORD 2

'--FLUSH BUFFER---
FOR i = 0 TO 16000
 buffer(i) = 0
NEXT i

z = 0

FOR i = 0 TO 99
 FOR o = 0 TO 49
  buffer(z) = stock(i, o): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 3
 buffer(z) = hmask(i): z = z + 1
NEXT i
FOR i = 1 TO 3
 buffer(z) = x(i): z = z + 1
 buffer(z) = y(i): z = z + 1
 buffer(z) = d(i): z = z + 1
NEXT i
FOR i = 0 TO 1024
 buffer(z) = global(i): z = z + 1
NEXT i

setpicstuf buffer(), 30000, -1
sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
storeset sg$ + CHR$(0), slot * 2 + 1, 0


'ALL THE STUFF THAT MUST BE SAVED
'map,x,y,d,foep,leader,gold&,gen(500),npcl(2100),tag(126),hero(40),a(500),stat(40,1,13),bmenu(40,5),spell(40,3,24),lmp(40,7),exlev&(40,1),name$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4)
'ALL THE STUFF THAT MUST BE PASSED
'slot,map,x,y,d,foep,leader,gold&,npcl(),tag(),hero(),a(),stat(),bmenu(),spell(),lmp(),exlev&(),name$(),item(),item$(),eqstuf(),hmask()

'30000
END SUB

SUB scripterr (e$)

errormode = 1

SELECT CASE errormode
CASE 1'--show error on screen
 textcolor 15, 0
 FOR i = 0 TO 1
  clearpage i
  setpal master()
  centerbox 160, 20, 310, 30, 3, i
  printstr "Script Error!", 108, 10, i
  printstr e$, 160 - 4 * LEN(e$), 20, i
 NEXT i
 w = getkey
CASE 2'--write error to file
 debug e$
END SELECT

END SUB

SUB scriptmath (scrat(), nowscript, scriptret, retvals(), heap(), global())
  SELECT CASE scrat(nowscript, curvalue)
  CASE 0' random
   scriptret = retvals(0) + INT(RND * (retvals(1) + 1 - retvals(0)))
  CASE 1' exponent
   scriptret = retvals(0) ^ retvals(1)
  CASE 2' modulus
   IF retvals(1) = 0 THEN
    scripterr "division by zero"
   ELSE
    scriptret = retvals(0) MOD retvals(1)
   END IF
  CASE 3' divide
   IF retvals(1) = 0 THEN
    scripterr "division by zero"
   ELSE
    scriptret = retvals(0) \ retvals(1)
   END IF
  CASE 4'multiply
   scriptret = retvals(0) * retvals(1)
  CASE 5'subtract
   scriptret = retvals(0) - retvals(1)
  CASE 6'add
   scriptret = retvals(0) + retvals(1)
  CASE 7'xor
   scriptret = retvals(0) XOR retvals(1)
  CASE 8'or
   scriptret = retvals(0) OR retvals(1)
  CASE 9'and
   scriptret = retvals(0) AND retvals(1)
  CASE 10'equal
   scriptret = (retvals(0) = retvals(1))
  CASE 11'not equal
   scriptret = (retvals(0) <> retvals(1))
  CASE 12'less than
   scriptret = (retvals(0) < retvals(1))
  CASE 13'greater than
   scriptret = (retvals(0) > retvals(1))
  CASE 14'less than or equal to
   scriptret = (retvals(0) <= retvals(1))
  CASE 15'greater than or equal to
   scriptret = (retvals(0) >= retvals(1))
  CASE 16'set variable
   IF retvals(0) < 0 THEN 'local variable
    heap(scrat(nowscript, scrheap) + ABS(retvals(0)) - 1) = retvals(1)
   ELSE 'global variable
    global(retvals(0)) = retvals(1)
   END IF
  CASE 17'increment
   IF retvals(0) < 0 THEN 'local variable
    heap(scrat(nowscript, scrheap) + ABS(retvals(0)) - 1) = heap(scrat(nowscript, scrheap) + ABS(retvals(0)) - 1) + retvals(1)
   ELSE 'global variable
    global(retvals(0)) = global(retvals(0)) + retvals(1)
   END IF
  CASE 18'decrement
   IF retvals(0) < 0 THEN 'local variable
    heap(scrat(nowscript, scrheap) + ABS(retvals(0)) - 1) = heap(scrat(nowscript, scrheap) + ABS(retvals(0)) - 1) - retvals(1)
   ELSE 'global variable
    global(retvals(0)) = global(retvals(0)) - retvals(1)
   END IF
  CASE ELSE
   scripterr "unsupported math"
  END SELECT
END SUB

SUB shop (id, timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), gold&, vpage, dpage, needf, hero(), name$(), stock(), tag(), item(), item$(), pal(), bmenu(), spell(), stat(), lmp(), exlev&(), eqstuf(), svcsr, map, x(), y(), d(), foep,  _
leader, mapx, mapy, aa(), npcl(), mx, my, scroll(), gmap(), hmask(), tastuf(), sourcerpg$, herobits(), itembits(), nowscript, script(), heap(), scrat(), global())

DIM a(40), menu$(10)

menu$(0) = "Buy"
menu$(1) = "Sell"
menu$(2) = "Hire"
menu$(3) = "Inn"
menu$(4) = "Equip"
menu$(5) = "Save"
menu$(6) = "Map"
menu$(7) = "Team"

GOSUB initshop
IF last = -1 THEN GOTO exitshop
IF last = 0 THEN autopick = 1
last = last + 1: menu$(last) = "Exit"

loadpage game$ + ".til" + CHR$(0), gmap(0), 3
setmapdata scroll(), buffer(), 0, 0
drawmap mapx, mapy, 0, dpage
copypage dpage, 3

setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
IF carray(0) > 1 THEN ptr = large(ptr - 1, 0)
IF carray(1) > 1 THEN ptr = small(ptr + 1, last)
IF carray(5) > 1 THEN EXIT DO
IF carray(4) > 1 OR autopick THEN
 IF ptr = last THEN EXIT DO
 IF menu$(ptr) = "Buy" THEN
   buystuff id, 0, a(), vpage, dpage, timing(), stock(), gold&, item(), item$(), tag(), carray(), csetup(), gotm, gotj(), mouse(), joy(), pal(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf(), herobits(), itembits()
 END IF
 IF menu$(ptr) = "Sell" THEN
   sellstuff id, a(), vpage, dpage, timing(), stock(), gold&, item(), item$(), tag(), carray(), csetup(), gotm, gotj(), mouse(), joy(), pal(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf(), itembits()
 END IF
 IF menu$(ptr) = "Hire" THEN
   buystuff id, 1, a(), vpage, dpage, timing(), stock(), gold&, item(), item$(), tag(), carray(), csetup(), gotm, gotj(), mouse(), joy(), pal(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf(), herobits(), itembits()
 END IF
 IF menu$(ptr) = "Map" THEN minimap vpage, dpage, scroll(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mx, my, timing(), gmap(), x, y, tastuf()
 IF menu$(ptr) = "Team" THEN
  reloadhwp hero()
  heroswap 1, hmask(), hero(), bmenu(), spell(), stat(), lmp(), exlev&(), name$(), eqstuf(), timing(), carray(), vpage, dpage, csetup(), gotm, gotj(), mouse(), joy(), pal()
 END IF
 IF menu$(ptr) = "Equip" THEN
  FOR o = 0 TO 3
   rectangle 0, 5 * o, 320, 5, 0, 2
   IF hero(o) > 0 THEN
    setpicstuf aa(), 636, -1
    loadset game$ + ".dt0" + CHR$(0), hero(o) - 1, 0
    setpicstuf buffer(), 1600, 2
    loadset game$ + ".pt4" + CHR$(0), aa(19), 5 * o
   END IF
  NEXT o
  w = onwho("Equip Who?", hero(), aa(), dpage, vpage, timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), mapx, x, mapy, y, pal())
  IF w >= 0 THEN
   equip w, hero(), stat(), name$(), timing(), vpage, dpage, item(), item$(), eqstuf(), bmenu(), carray(), csetup(), gotm, gotj(), mouse(), joy(), tag(), itembits()
  END IF
 END IF
 IF menu$(ptr) = "Save" THEN
  temp = picksave(svcsr, pal(), timing(), dpage, vpage, carray(), csetup(), gotm, gotj(), mouse(), joy(), sourcerpg$)
  IF temp >= 0 THEN savegame temp, sourcerpg$, map, x(), y(), d(), foep, leader, mapx, mapy, gold&, npcl(), tag(), hero(), aa(), stat(), bmenu(), spell(), lmp(), exlev&(), name$(), item(), item$(), eqstuf(), stock(), hmask(), global()
 END IF
 IF menu$(ptr) = "Inn" THEN
  inn = 0
  IF shoption(inn, a(18), timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), gold&, needf, hero(), name$(), stat(), vpage, dpage) THEN
   IF inn = 0 THEN
    FOR i = 0 TO 3
     IF hero(i) > 0 THEN
      stat(i, 0, 0) = stat(i, 1, 0)
      stat(i, 0, 1) = stat(i, 1, 1)
      resetlmp lmp(), i, stat(i, 0, 12)
     END IF
    NEXT i
   END IF
   IF a(19) > 0 THEN
    '--Run animation for Inn
    IF runscript(a(19), nowscript + 1, -1, script(), heap(), scrat()) THEN
     nowscript = nowscript + 1
     EXIT DO
    ELSE
     scripterr "failed to load script" + STR$(a(19))
    END IF
   ELSE
    '--Inn has no script, do simple fade
    fadeto buffer(), 0, 0, 20
    needf = 1
   END IF
  END IF
 END IF
 IF autopick THEN EXIT SUB
 loadpage game$ + ".til" + CHR$(0), gmap(0), 3
 setmapdata scroll(), buffer(), 0, 0
 drawmap mapx, mapy, 0, dpage
 copypage dpage, 3
END IF
h = (last + 2) * 10
centerbox 160, 104 + (h * .5), 96, h, 1, dpage
centerbox 160, 90, LEN(sn$) * 8 + 8, 16, 1, dpage
edgeprint sn$, xstring(sn$, 160), 85, 15, dpage
FOR i = 0 TO last
 c = 7: IF ptr = i THEN c = 14 + tog
 edgeprint menu$(i), xstring(menu$(i), 160), 109 + i * 10, c, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
IF needf = 1 THEN needf = 0: fadetopal master(), buffer(): setkeys
dowait
LOOP

exitshop:
EXIT SUB

initshop:
setpicstuf a(), 40, -1
loadset game$ + ".sho" + CHR$(0), id, 0
sn$ = ""
FOR i = 1 TO small(a(0), 15)
 sn$ = sn$ + CHR$(a(i))
NEXT i
o = 0: last = -1
FOR i = 0 TO 7
 IF readbit(a(), 17, i) THEN SWAP menu$(i), menu$(o): last = o: o = o + 1
NEXT i
RETURN

END SUB

FUNCTION shoption (inn, price, timing(), carray(), csetup(), gotm, gotj(), mouse(), joy(), gold&, needf, hero(), name$(), stat(), vpage, dpage)
DIM menu$(1), sname$(40)

shoption = 0

getnames sname$()

menu$(0) = "Pay"
menu$(1) = "Cancel"
setkeys
DO
setwait timing(), 80
setkeys
tog = tog XOR 1
playtimer
control carray(), csetup(), gotm, gotj(), mouse(), joy(), 160, 100, 1, timing()
IF carray(5) > 1 THEN inn = 1: EXIT FUNCTION
IF carray(0) > 1 OR carray(1) > 1 OR carray(2) > 1 OR carray(3) > 1 THEN inn = inn XOR 1
IF carray(4) > 1 THEN
 IF inn = 0 AND gold& >= price THEN
  gold& = gold& - price
  shoption = -1
  EXIT FUNCTION
 END IF
 IF inn = 1 THEN EXIT FUNCTION
END IF
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  col = 15
  edgeprint name$(i), 128 - LEN(name$(i)) * 8, 5 + i * 10, col, dpage
  edgeprint RIGHT$(STR$(stat(i, 0, 0)), LEN(STR$(stat(i, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(i, 1, 0)), LEN(STR$(stat(i, 1, 0))) - 1), 136, 5 + i * 10, col, dpage
 END IF
NEXT i
centerfuz 160, 90, 200, 60, 1, dpage
rectangle 130, 92, 60, 22, 20, dpage
edgeprint "THE INN COSTS" + STR$(price) + " " + sname$(32), 160 - LEN("THE INN COSTS" + STR$(price) + " " + sname$(32)) * 4, 70, 15, dpage
edgeprint "You have" + STR$(gold&) + " " + sname$(32), 160 - LEN("You have" + STR$(gold&) + " " + sname$(32)) * 4, 80, 15, dpage
FOR i = 0 TO 1
 col = 7: IF inn = i THEN col = 14 + tog
 edgeprint menu$(i), 160 - LEN(menu$(i)) * 4, 94 + i * 8, col, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
copypage 3, dpage
IF needf = 1 THEN needf = 0: fadetopal master(), buffer(): setkeys
dowait
LOOP

END FUNCTION

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

SUB snapshot (vpage)
pre$ = RIGHT$(game$, LEN(game$) - 12)
pre$ = LEFT$(pre$, 6)

n = 0
DO
n$ = RIGHT$(STR$(n), LEN(STR$(n)) - 1)
shot$ = pre$ + n$ + ".bmp"
IF isfile(shot$ + CHR$(0)) = 0 THEN EXIT DO
n = n + 1
LOOP UNTIL n > 99
screenshot shot$ + CHR$(0), vpage, master(), buffer()
END SUB

SUB tagdisplay (tag(), dpage)
STATIC ptr
DIM buf(20)

ptr = large(ptr, 0)

IF keyval(74) > 1 OR keyval(12) > 1 THEN ptr = large(ptr - 1, 0)
IF keyval(78) > 1 OR keyval(13) > 1 THEN ptr = small(ptr + 1, 1999)

setpicstuf buf(), 42, -1
fuzzyrect 0, 0, 208, 50, 240, dpage
FOR i = ptr TO ptr + 4
 temp$ = STR$(i) + " "
 buf(0) = 0
 SELECT CASE i
 CASE 0, 1
  temp$ = temp$ + " Reserved Tag"
 CASE IS > 1
  loadset game$ + ".tmn" + CHR$(0), i, 0
  FOR j = 1 TO small(buf(0), 20)
   temp$ = temp$ + CHR$(large(small(buf(j), 255), 0))
  NEXT j
 END SELECT
 c = 8 + (-7 * istag(tag(), i, 0))
 edgeprint temp$, 0, (i - ptr) * 10, c, dpage
NEXT i

END SUB

FUNCTION unlumpone (lumpfile$, onelump$, asfile$)
unlumpone = 0

tmp$ = STRING$(envlength("TEMP"), 0): getstring tmp$
IF RIGHT$(tmp$, 1) <> "\" THEN tmp$ = tmp$ + "\"
unlumpfile lumpfile$ + CHR$(0), onelump$, tmp$, buffer()

IF isfile(tmp$ + onelump$ + CHR$(0)) THEN
 copyfile tmp$ + onelump$ + CHR$(0), asfile$ + CHR$(0), buffer()
 KILL tmp$ + onelump$
 unlumpone = -1
END IF

f = FREEFILE

END FUNCTION

SUB xbload (f$, array(), e$)

IF isfile(f$ + CHR$(0)) THEN
 DEF SEG = VARSEG(array(0)): BLOAD f$, VARPTR(array(0))
ELSE
 fatalerror e$
END IF

END SUB

FUNCTION xstring (s$, x)
xstring = small(large(x - LEN(s$) * 4, 0), 319 - LEN(s$) * 8)
END FUNCTION

