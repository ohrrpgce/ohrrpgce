'$DYNAMIC
DEFINT A-Z
'----------------------------
'basic subs and functions
DECLARE SUB bufdump (buf%(), last%)
DECLARE SUB betweenable (buf%(), ptr%)
DECLARE SUB operatortable (in$, out$)
DECLARE FUNCTION howmanyargs% (n$, f$)
DECLARE FUNCTION countlvars% (n$, num$)
DECLARE SUB writeheader (f$)
DECLARE SUB lumpout (f$)
DECLARE FUNCTION nameof$ (value%, kind%, num$)
DECLARE FUNCTION consultlist$ (v%, f$, args%)
DECLARE SUB decompile (ifile$, ofile$, num$)
DECLARE FUNCTION isoption% (o$)
DECLARE FUNCTION makebranch% (buf%(), last%, argoff%(), cache%(), inptr%, outptr%, cptr%, fptr%)
DECLARE SUB exetree (buf%(), last%, num$, name$)
DECLARE SUB absorbnext (buf%(), last%, paren%)
DECLARE SUB normalflow (buf%(), last%, f$)
DECLARE SUB verifymath (buf%(), last%, f$)
DECLARE SUB insert (buf%(), last%, dest%, v%)
DECLARE SUB savebuf2bin (f$, buf%(), size%)
DECLARE SUB loadbin2buf (f$, buf%(), size%)
DECLARE SUB seekparen (index%, last%, buf%(), bp%, ep%, numargs%, argoff%(), name$, limit%)
DECLARE SUB enforceargs (buf%(), last%, f$, id%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE SUB disinsert (buf%(), last%, index%, elements%)
DECLARE SUB onebyone ()
DECLARE SUB flush (b%(), index%)
DECLARE SUB makemathtable (f$)
DECLARE SUB makeflowtable (f$)
DECLARE SUB tobinary ()
DECLARE SUB identifymn (s$, f$, t%, v%)
DECLARE SUB tonumbers ()
DECLARE SUB localtables ()
DECLARE SUB separatescripts (in$)
DECLARE FUNCTION seekmatch% (n$, f$, args%)
DECLARE SUB stripdeclare (in$, out$)
DECLARE SUB globaltable (in$, out$)
DECLARE SUB functiontable (in$, out$, seek$)
DECLARE SUB constants (precon$, postcon$)
DECLARE SUB nametables (normalfile$, tablefile$)
DECLARE SUB makeworkdir (wd$)
DECLARE SUB spiterror (e$)
DECLARE SUB readall (infile$, outfile%)
DECLARE FUNCTION sed$ (s$, old$, new$)
DECLARE FUNCTION isnumeric% (s$)
DECLARE FUNCTION comment$ (s$, cmarker$)
DECLARE FUNCTION isamong% (s$, sep$)
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION permission% (prompt$)
DECLARE FUNCTION firststr$ (st$, sep$, cut%)
DECLARE FUNCTION parsecline% (cl$, infile$, outfile$, option$)

'----------------------------
'assembly subs and functions
'DECLARE SUB setmodex ()
'DECLARE SUB restoremode ()
DECLARE SUB setpicstuf (buf(), BYVAL b, BYVAL p)
DECLARE SUB loadset (fil$, BYVAL i, BYVAL l)
DECLARE SUB storeset (fil$, BYVAL i, BYVAL l)
'DECLARE SUB copypage (BYVAL page1, BYVAL page2)
'DECLARE SUB setvispage (BYVAL page)
'DECLARE SUB drawsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
'DECLARE SUB wardsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
'DECLARE SUB getsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
'DECLARE SUB loadsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
'DECLARE SUB stosprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL page)
'DECLARE SUB setdiskpages (buf(), BYVAL h, BYVAL l)
'DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
'DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
'DECLARE SUB fadeto (palbuff(), BYVAL red, BYVAL green, BYVAL blue)
'DECLARE SUB fadetopal (pal(), palbuff())
'DECLARE SUB setpal (pal())
'DECLARE SUB clearpage (BYVAL page)
'DECLARE SUB setkeys ()
'DECLARE SUB setfont (f())
'DECLARE SUB printstr (s$, BYVAL x, BYVAL y, BYVAL p)
'DECLARE SUB textcolor (BYVAL f, BYVAL b)
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
'DECLARE SUB setitup (fil$, buff(), tbuff(), BYVAL p)
'DECLARE FUNCTION resetdsp
'DECLARE SUB playsnd (BYVAL n, BYVAL f)
'DECLARE SUB closefile
'DECLARE SUB rectangle (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
'DECLARE SUB fuzzyrect (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()
'DECLARE SUB setmapdata (array(), pas(), BYVAL t, BYVAL b)
'DECLARE SUB setmapblock (BYVAL x, BYVAL y, BYVAL v)
'DECLARE FUNCTION readmapblock (BYVAL x, BYVAL y)
'DECLARE SUB setanim (BYVAL cycle1, BYVAL cycle2)
'DECLARE SUB drawmap (BYVAL x, BYVAL y, BYVAL t, BYVAL p)
'DECLARE SUB setoutside (BYVAL defaulttile)
'DECLARE SUB putpixel (BYVAL x, BYVAL y, BYVAL c, BYVAL p)
'DECLARE FUNCTION readpixel (BYVAL x, BYVAL y, BYVAL p)
'DECLARE FUNCTION Keyseg ()
'DECLARE FUNCTION keyoff ()
'DECLARE FUNCTION keyval (BYVAL a)
'DECLARE FUNCTION getkey ()
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
DECLARE SUB lumpfiles (listf$, lump$, path$, buffer())
DECLARE SUB unlump (lump$, ulpath$, buffer())
DECLARE FUNCTION isfile (n$)
'DECLARE SUB setupmusic (mbuf())
'DECLARE SUB closemusic ()
'DECLARE SUB stopsong ()
'DECLARE SUB resumesong ()
'DECLARE SUB resetfm ()
'DECLARE SUB loadsong (f$)
'DECLARE SUB fademusic (BYVAL vol)
'DECLARE FUNCTION getfmvol ()
'DECLARE SUB setfmvol (BYVAL vol)
'DECLARE SUB screenshot (f$, BYVAL p, maspal(), buf())
'DECLARE FUNCTION readjoy (joybuf(), BYVAL jnum)
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

CONST true = -1, false = 0, version = 0

DIM SHARED timing(4), option$, infix

infix = true

COLOR 7

progdir$ = STRING$(rpathlength, 0): getstring progdir$
IF RIGHT$(progdir$, 1) = "\" THEN progdir$ = LEFT$(progdir$, LEN(progdir$) - 1)
CHDIR progdir$
IF MID$(progdir$, 2, 1) = ":" THEN setdrive ASC(LEFT$(progdir$, 1)) - 65

IF parsecline(COMMAND$, infile$, outfile$, option$) THEN SYSTEM
makeworkdir "hspeak.tmp"

'---Normalize script---
OPEN "hspeak.tmp\step1.txt" FOR OUTPUT AS #1
 readall infile$, 1
CLOSE #1

'---Replace all constants---
constants "hspeak.tmp\step1.txt", "hspeak.tmp\step2.txt"

'---Generate flow table---
PRINT "generating tables: flow";
makeflowtable "hspeak.tmp\flow.txt"

'---Generate math table---
PRINT ",math";
makemathtable "hspeak.tmp\math.txt"

'---Generate function table---
PRINT ",function";
functiontable "hspeak.tmp\step2.txt", "hspeak.tmp\function.txt", "definefunction"

'---Generate script table---
PRINT ",script";
functiontable "hspeak.tmp\step2.txt", "hspeak.tmp\scripts.txt", "definescript"

'---Generate betweenable operator table---
PRINT ",operator";
operatortable "hspeak.tmp\step2.txt", "hspeak.tmp\step3.txt"

'---Generate global variable table---
PRINT ",variable"
globaltable "hspeak.tmp\step3.txt", "hspeak.tmp\global.txt"

'---strip declarations---
PRINT "stripping declarations"
stripdeclare "hspeak.tmp\step3.txt", "hspeak.tmp\step4.txt"

'---separate scripts---
separatescripts "hspeak.tmp\step4.txt"

'---generate local variable tables---
PRINT "generating local variable tables"
localtables

'---use tables to replace mnemonics---
PRINT "reducing scripts to numbers"
tonumbers

'---convert .num files into .bin files---
PRINT "reducing numbers to binary"
tobinary

'---load each file into a buffer one by one and finish it---
PRINT "taxing the peasants"
onebyone

'---create the header---
PRINT "writing header"
writeheader "hspeak.tmp\HS"

'---lump everything into the output file---
PRINT "lumping " + outfile$
lumpout outfile$

IF isoption("kK") THEN
 PRINT "keeping temporary files in HSPEAK.TMP"
ELSE
 PRINT "cleaning up temporary files"
 KILL "hspeak.tmp\*.*"
 RMDIR "hspeak.tmp"
END IF

PRINT "finished"

'----------------------------------------------------------------------------
'0 terminate script
'1 literal number
'2 flow control
'3 global variable
'4 local variable
'5 math function
'6 function call
'7 script call
'8 variable index

REM $STATIC
SUB absorbnext (buf(), last, paren)

dest = paren + 4'--point dest to the begin

'check for argless statement
IF buf(dest) = 2 AND buf(dest + 1) = 1 THEN
 depth = 1
 DO
  dest = dest + 2
  IF dest > last THEN spiterror "`begin' without a matching `end' or `(' with out a matching `)'": SYSTEM
  IF buf(dest) = 2 AND buf(dest + 1) = 1 THEN depth = depth + 1
  IF buf(dest) = 2 AND buf(dest + 1) = 2 THEN depth = depth - 1
  IF depth = 0 THEN EXIT DO
 LOOP
END IF

insert buf(), last, dest, 2
insert buf(), last, dest, 2
disinsert buf(), last, paren, 2

END SUB

SUB betweenable (buf(), last)

 OPEN "hspeak.tmp/operate.txt" FOR INPUT AS #2
 
 DO
  IF EOF(2) THEN EXIT DO
  LINE INPUT #2, op$
  LINE INPUT #2, name$
  identifymn name$, "", t, v
  SELECT CASE t
  CASE 5
   argc = howmanyargs(nameof$(v, 5, ""), "hspeak.tmp\math.txt")
  CASE 6
   argc = howmanyargs(nameof$(v, 6, ""), "hspeak.tmp\function.txt")
  CASE 7
   argc = howmanyargs(nameof$(v, 7, ""), "hspeak.tmp\scripts.txt")
  CASE ELSE
   argc = 0
  END SELECT
  IF (t = 5 OR t = 6 OR t = 7) AND argc = 1 THEN
   i = 0
   DO
    IF i > last THEN EXIT DO
    IF buf(i) = t AND buf(i + 1) = v THEN
     leftside = i - 2
     rightside = i + 2
     IF buf(leftside) = 2 AND buf(leftside + 1) = 2 THEN
      depth = 0
      DO
       IF buf(leftside) = 2 AND buf(leftside + 1) = 1 THEN depth = depth + 1
       IF buf(leftside) = 2 AND buf(leftside + 1) = 2 THEN depth = depth - 1
       leftside = leftside - 2
       IF leftside < 0 THEN leftside = 0: EXIT DO
      LOOP UNTIL depth = 0
     END IF
     IF buf(rightside) = 2 AND buf(rightside + 1) = 1 THEN
      depth = 0
      DO
       IF buf(rightside) = 2 AND buf(rightside + 1) = 1 THEN depth = depth + 1
       IF buf(rightside) = 2 AND buf(rightside + 1) = 2 THEN depth = depth - 1
       rightside = rightside + 2
       IF rightside > last THEN spiterror "unable to figure out where the right side of operator " + op$ + " is": SYSTEM
      LOOP UNTIL depth = 0
     ELSE
      SELECT CASE buf(rightside)
      CASE 5
       argc = howmanyargs(nameof$(buf(rightside + 1), 5, ""), "hspeak.tmp\math.txt")
      CASE 6
       argc = howmanyargs(nameof$(buf(rightside + 1), 6, ""), "hspeak.tmp\function.txt")
      CASE 7
       argc = howmanyargs(nameof$(buf(rightside + 1), 7, ""), "hspeak.tmp\scripts.txt")
      CASE ELSE
       argc = -1
      END SELECT
      IF (t = 5 OR t = 6 OR t = 7) AND argc >= 0 THEN
       rightside = rightside + 2
       depth = 0
       DO
        IF buf(rightside) = 2 AND buf(rightside + 1) = 1 THEN depth = depth + 1
        IF buf(rightside) = 2 AND buf(rightside + 1) = 2 THEN depth = depth - 1
        rightside = rightside + 2
        IF rightside > last THEN spiterror "unable to figure out where the right side of operator " + op$ + " is": SYSTEM
       LOOP UNTIL depth = 0
      ELSE
       rightside = rightside + 2
      END IF
     END IF
     insert buf(), last, rightside, 2
     insert buf(), last, rightside, 2
     disinsert buf(), last, i, 2
     insert buf(), last, leftside, 1
     insert buf(), last, leftside, 2
     insert buf(), last, leftside, v
     insert buf(), last, leftside, t
    END IF
    i = i + 2 '--next instruction
   LOOP
   
  END IF
 LOOP

 CLOSE #2

'bufdump buf(), last
 i = 0
 DO
  IF i > last THEN EXIT DO
  IF buf(i) = 2 AND buf(i + 1) = 1 THEN
   '--we have a left paren
   orphan = false
   IF i = 0 THEN orphan = true
   IF buf(i - 2) = 2 AND (buf(i - 1) = 1 OR buf(i - 1) = 2) THEN orphan = true
   argc = 0
   SELECT CASE buf(i - 2)
   CASE 1, 3, 4, 8
    '--number, global var, local var, and var index are argless
    argc = -1
   CASE 2
    '--break is argless
    IF buf(i - 1) = 11 THEN argc = -1
   CASE 5
    argc = howmanyargs(nameof$(buf(i - 1), 5, ""), "hspeak.tmp\math.txt")
   CASE 6
    argc = howmanyargs(nameof$(buf(i - 1), 6, ""), "hspeak.tmp\function.txt")
   CASE 7
    argc = howmanyargs(nameof$(buf(i - 1), 7, ""), "hspeak.tmp\scripts.txt")
   END SELECT
   IF argc = -1 THEN orphan = true
   IF orphan THEN
    '--we have an orphaned paren
    j = i
    depth = 0
    DO
     IF j > last THEN spiterror "gasp! an orphaned begin has no end": SYSTEM
     IF buf(j) = 2 AND buf(j + 1) = 1 THEN depth = depth + 1
     IF buf(j) = 2 AND buf(j + 1) = 2 THEN depth = depth - 1
     IF depth = 0 THEN EXIT DO
     j = j + 2
    LOOP
    disinsert buf(), last, j, 2
    disinsert buf(), last, i, 2
    i = i - 2
'bufdump buf(), last
   END IF
  END IF
  i = i + 2 '--next instruction
 LOOP

END SUB

SUB bufdump (buf(), last)

COLOR 14
 PRINT ">";
FOR i = 0 TO last - 2 STEP 2
 COLOR 7
 PRINT "|";
 COLOR 6
 PRINT LTRIM$(STR$(buf(i))) + STR$(buf(i + 1));
NEXT i
COLOR 7
PRINT ""

END SUB

REM $DYNAMIC
FUNCTION comment$ (s$, cmarker$)
out$ = ""

FOR i = 1 TO LEN(s$)
 IF isamong(MID$(s$, i, 1), cmarker$) THEN EXIT FOR
 out$ = out$ + MID$(s$, i, 1)
NEXT i

comment$ = out$
END FUNCTION

REM $STATIC
SUB constants (precon$, postcon$)

PRINT "hashing constants"

 OPEN precon$ FOR INPUT AS #1

 DO
  IF EOF(1) THEN EXIT DO
  LINE INPUT #1, a$
  SELECT CASE mode
   CASE 0'---Seek a defineconstant statement---
    IF a$ = "defineconstant" THEN
     LINE INPUT #1, b$
     IF b$ <> "begin" THEN spiterror "beginningless defineconstant statement": SYSTEM
     mode = 1
    END IF
   CASE 1'---parsing a defineconstant block---
    IF a$ = "end" THEN
     mode = 0
    ELSE
     LINE INPUT #1, mne$
     IF NOT isnumeric(a$) THEN spiterror "defineconstant: expected a number but instead found the name " + a$ + ". You may have typed your defineconstant statement wrong. the correct format is defineconstant(number,name)": SYSTEM
     IF isnumeric(mne$) THEN spiterror "defineconstant: expected name but found number " + mne$ + ". This may happen if you give two constants the same name": SYSTEM
     alpha$ = LCASE$(LEFT$(mne$, 1))
     IF NOT isamong(alpha$, "abcdefghijklmnopqrstuvwxyz") THEN alpha$ = "_"
     OPEN "hspeak.tmp\" + alpha$ + ".con" FOR APPEND AS #3
     PRINT #3, mne$
     PRINT #3, a$
     CLOSE #3
    END IF
  END SELECT
 LOOP

 CLOSE #1
 CLOSE #3

 IF mode = 1 THEN spiterror "unterminated defineconstant statement. Your defineconstant statement has a `begin' but does not have an `end'": SYSTEM
 mode = 0

PRINT "substituting constants"

 OPEN precon$ FOR INPUT AS #1
 OPEN postcon$ FOR OUTPUT AS #2

 DO
  IF EOF(1) THEN EXIT DO
  LINE INPUT #1, a$
  SELECT CASE mode
   CASE 0'---Seeking---
    IF a$ = "defineconstant" THEN
     LINE INPUT #1, b$
     IF b$ <> "begin" THEN spiterror "beginningless defineconstant statement": SYSTEM
     mode = 1
    ELSE
     alpha$ = LCASE$(LEFT$(a$, 1))
     IF NOT isamong(alpha$, "abcdefghijklmnopqrstuvwxyz") THEN alpha$ = "_"
     IF isfile("hspeak.tmp\" + alpha$ + ".con" + CHR$(0)) THEN
      OPEN "hspeak.tmp\" + alpha$ + ".con" FOR INPUT AS #3
      DO
       IF EOF(3) THEN EXIT DO
       LINE INPUT #3, mne$
       LINE INPUT #3, num$
       IF a$ = mne$ THEN a$ = num$: EXIT DO
      LOOP
      CLOSE #3
     END IF
     PRINT #2, a$
    END IF
   CASE 1'---skipping defineconstant block---
    IF a$ = "end" THEN mode = 0
  END SELECT
 LOOP

CLOSE #1
CLOSE #2

END SUB

FUNCTION consultlist$ (v, f$, args)

consultlist$ = "???"

handle = FREEFILE
OPEN f$ FOR INPUT AS #handle

DO
 IF EOF(handle) THEN EXIT DO
 LINE INPUT #handle, name$
 LINE INPUT #handle, number$
 IF v = VAL(number$) THEN consultlist$ = name$: EXIT DO
 IF args THEN
  LINE INPUT #handle, argc$
  FOR i = 1 TO VAL(argc$)
   LINE INPUT #handle, arg$
  NEXT i
 END IF
LOOP

CLOSE #handle

END FUNCTION

FUNCTION countlvars (n$, num$)
countlvars = -1

count = howmanyargs(n$, "hspeak.tmp\scripts.txt")

IF isfile("hspeak.tmp\" + num$ + ".var" + CHR$(0)) THEN
 handle = FREEFILE
 OPEN "hspeak.tmp\" + num$ + ".var" FOR INPUT AS #handle
 
 DO
  IF EOF(handle) THEN EXIT DO
  LINE INPUT #handle, name$
  LINE INPUT #handle, number$
  IF VAL(number$) + 1 > count THEN count = VAL(number$) + 1
 LOOP
 
 CLOSE #handle
END IF

countlvars = count

END FUNCTION

SUB decompile (ifile$, ofile$, num$)

DIM stack(1024)

skipwords = 2

iptr = FREEFILE
OPEN ifile$ FOR BINARY AS #iptr
optr = FREEFILE
OPEN ofile$ FOR OUTPUT AS #optr

setupstack stack(), 2048, "hspeak.tmp\stack.tmp" + CHR$(0)

mode = 0
value = 0
GET #iptr, 1, ptr '--skip to start of execution
argn = 0
depth = 0

DO
 SELECT CASE mode
'---------------------------------------------------------------------------
 CASE 0'---read statement
  GET #iptr, 1 + ptr + (2 * 0), kind
  GET #iptr, 1 + ptr + (2 * 1), value
  GET #iptr, 1 + ptr + (2 * 2), argc
  argn = 0
  SELECT CASE kind
  CASE 1, 3, 4
   mode = 8 '---argless primitive
  CASE ELSE
   mode = 9 '---function
  '--flow control would be a special case
  END SELECT
'---------------------------------------------------------------------------
 CASE 8'---primitive
  z$ = STRING$(depth, " ") + nameof$(value, kind, num$)
  PRINT #optr, z$
  IF LEFT$(LTRIM$(z$), 5) = "ERROR" THEN spiterror "decompilation for verification failed. This is a bug in HSPEAK.EXE please mail the script you where attempting to compile to ohrrpgce@Hamsterrepublic.com": SYSTEM
  '--return value would be set here, pushed at return
  mode = 11'---return
'---------------------------------------------------------------------------
 CASE 9'---function with args
  z$ = STRING$(depth, " ") + nameof$(value, kind, num$) + "("
  PRINT #optr, z$
  IF LEFT$(LTRIM$(z$), 5) = "ERROR" THEN spiterror "decompilation for verification failed. This is a bug in HSPEAK.EXE please mail the script you where attempting to compile to ohrrpgce@Hamsterrepublic.com": SYSTEM
  '--pop return values of each arg
  '--evaluate function, math, script, whatever
  '--return value would be set here, pushed at return
  mode = 12'---try next arg
'---------------------------------------------------------------------------
 CASE 10'---do argument
  depth = depth + 1
  pushw ptr
  pushw kind
  pushw value
  pushw argc
  pushw argn
  GET #iptr, 1 + ptr + (2 * (3 + argn)), ptr
  ptr = (ptr + skipwords) * 2
  mode = 0 '---read new statement
'---------------------------------------------------------------------------
 CASE 11'---return
  depth = depth - 1
  IF depth < 0 THEN EXIT DO
  argn = popw
  argc = popw
  value = popw
  kind = popw
  ptr = popw
  '--return value would be pushed here
  argn = argn + 1
  mode = 12'---try next arg
'---------------------------------------------------------------------------
 CASE 12'---check if all args are done
  IF argn + 1 > argc THEN
   PRINT #optr, STRING$(depth, " ") + ")"
   mode = 11'---return
  ELSE
   mode = 10'---call argument
  END IF
'---------------------------------------------------------------------------
 END SELECT
LOOP

releasestack

CLOSE #iptr
CLOSE #optr

'0 terminate script
'1 literal number
'2 flow control
'3 global variable
'4 local variable
'5 math function
'6 function call
'7 script call

END SUB

SUB disinsert (buf(), last, index, elements)

'--deletes elements at index from the binary script buffer

dest = index
source = index + elements
DO
 buf(dest) = buf(source)
 dest = dest + 1
 source = source + 1
 IF dest > last THEN EXIT DO
LOOP

last = last - elements

END SUB

SUB enforceargs (buf(), last, f$, id)

 DIM arg(31), argoff(31)

 OPEN f$ FOR INPUT AS #2

 DO
  IF EOF(2) THEN EXIT DO
  LINE INPUT #2, name$
  LINE INPUT #2, num$
  num = VAL(num$)
  LINE INPUT #2, argc$
  argc = small(VAL(argc$), 31)
  flush arg(), 31
  FOR i = 1 TO argc
   LINE INPUT #2, arg$
   arg(i - 1) = VAL(arg$)
  NEXT i
 
  i = 0
  DO
   IF i > last THEN EXIT DO
   IF buf(i) = id AND buf(i + 1) = num THEN
    SELECT CASE argc
     CASE 0
      IF buf(i + 2) = 2 AND buf(i + 3) = 1 THEN
       IF NOT (buf(i + 4) = 2 AND buf(i + 5) = 2) THEN
        spiterror "warning: " + name$ + " should not have arguments"
        WHILE (NOT (buf(i + 4) = 2 AND buf(i + 5) = 2))
         IF buf(i + 4) = 0 THEN spiterror "impossible error: unwarp reality and try again": SYSTEM
         disinsert buf(), last, i + 4, 2
        WEND
       END IF'---failed to find ) paren
      END IF'---found ( paren
     CASE ELSE
      flush argoff(), 31
      seekparen i, last, buf(), bpar, epar, numargs, argoff(), name$, 32
      IF numargs > argc THEN
       spiterror "Warning: " + name$ + " has" + STR$(numargs - argc) + " more arguments than it needs"
       disinsert buf(), last, argoff(argc), epar - argoff(argc)
      END IF
      IF numargs < argc THEN
       PRINT ".";
       FOR i = numargs TO argc - 1
        insert buf(), last, epar, arg(i) '--insert default
        insert buf(), last, epar, 1 '--make it a literal number
        epar = epar + 2
       NEXT i
      END IF
    END SELECT
   END IF
   i = i + 2 '--next instruction
  LOOP

 LOOP

 CLOSE #2

END SUB

REM $DYNAMIC
FUNCTION exclude$ (s$, x$)
 out$ = ""
 FOR i = 1 TO LEN(s$)
  ok = true
  FOR j = 1 TO LEN(x$)
   IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = false
  NEXT j
  IF ok THEN out$ = out$ + MID$(s$, i, 1)
 NEXT i
 exclude$ = out$
END FUNCTION

REM $STATIC
SUB exetree (buf(), last, num$, name$)
DIM argoff(16383), cache(1536)

fptr = FREEFILE
OPEN "hspeak.tmp\" + num$ + ".hsx" FOR BINARY AS #fptr

count = countlvars(name$, num$)
exstart = 4
PUT #fptr, 1, exstart  'first word is the byte-offset of the first execution byte
PUT #fptr, 3, count    'number of local vars to init

iptr = 0
optr = exstart \ 2'--start on third word. first two words are the header
cptr = -1
dummy = makebranch(buf(), last, argoff(), cache(), iptr, optr, cptr, fptr)

CLOSE #fptr


'0 terminate script
'1 literal number       cached
'2 flow control
'3 global variable      cached
'4 local variable       cached
'5 math function
'6 function call        cached if zero-arg
'7 script call          cached if zero-arg
END SUB

REM $DYNAMIC
FUNCTION firststr$ (st$, sep$, cut)
 s$ = st$
 out$ = ""
 DO WHILE LEN(s$) > 0
  IF isamong(s$, sep$) THEN
   s$ = RIGHT$(s$, LEN(s$) - 1)
  ELSE
   EXIT DO
  END IF
 LOOP
 DO WHILE LEN(s$) > 0
  IF isamong(LEFT$(s$, LEN(sep$)), sep$) THEN
   EXIT DO
  END IF
  out$ = out$ + LEFT$(s$, 1)
  s$ = RIGHT$(s$, LEN(s$) - 1)
 LOOP
 IF cut THEN st$ = s$
 firststr$ = out$
END FUNCTION

REM $STATIC
SUB flush (b(), index)

FOR i = 0 TO index
 b(i) = 0
NEXT i

END SUB

SUB functiontable (in$, out$, seek$)

 autonumber = 32767

 OPEN in$ FOR INPUT AS #1
 OPEN out$ FOR OUTPUT AS #3

 DO
  IF EOF(1) THEN EXIT DO
  LINE INPUT #1, a$
  SELECT CASE mode
   CASE 0'---Seek a declarefunctions/declarescript statement---
    IF a$ = seek$ THEN
     LINE INPUT #1, b$
     IF b$ <> "begin" THEN spiterror "beginningless " + seek$ + " statement. You have a " + seek$ + " statement that requires arguments, but you have not given it any": SYSTEM
     mode = 1
    END IF
   CASE 1'---parsing a declarefunctions/declarescript block---
    IF a$ = "end" THEN
     mode = 0
    ELSE
     IF VAL(a$) = -1 THEN a$ = LTRIM$(STR$(autonumber)): autonumber = autonumber - 1
     LINE INPUT #1, mne$
     LINE INPUT #1, argc$
     IF NOT isnumeric(a$) THEN spiterror seek$ + ": expected ID number but found name": SYSTEM
     IF isnumeric(mne$) THEN spiterror seek$ + ": expected name but found number " + mne$ + ". The most likely cause of this error is that you have defined a constant with the same name as a predefined function": SYSTEM
     IF NOT isnumeric(argc$) THEN spiterror seek$ + ": expected argument count but found the name " + argc$ + " instead": SYSTEM
     PRINT #3, mne$
     PRINT #3, a$
     PRINT #3, argc$
     FOR i = 1 TO VAL(argc$)
      LINE INPUT #1, arg$
      IF NOT isnumeric(arg$) THEN spiterror seek$ + ": expected default value but found the name " + arg$ + "instead": SYSTEM
      PRINT #3, arg$
     NEXT i
    END IF
  END SELECT
 LOOP

 CLOSE #1
 CLOSE #3

 IF mode = 1 THEN spiterror "unterminated " + seek$ + " statement": SYSTEM

END SUB

SUB globaltable (in$, out$)

 OPEN in$ FOR INPUT AS #1
 OPEN out$ FOR OUTPUT AS #3

 DO
  IF EOF(1) THEN EXIT DO
  LINE INPUT #1, a$
  SELECT CASE mode
   CASE 0'---Seek a globalvariable statement---
    IF a$ = "globalvariable" THEN
     LINE INPUT #1, b$
     IF b$ <> "begin" THEN spiterror "beginningless globalvariable statement": SYSTEM
     mode = 1
    END IF
   CASE 1'---parsing a globalvariable block---
    IF a$ = "end" THEN
     mode = 0
    ELSE
     LINE INPUT #1, mne$
     IF NOT isnumeric(a$) THEN spiterror "globalvariable: expected an ID number but found the name " + a$ + " instead": SYSTEM
     IF isnumeric(mne$) THEN spiterror "globalvariable: expected a name, but found the number " + mne$ + " instead. This might be the result of a naming conflict": SYSTEM
     PRINT #3, mne$
     PRINT #3, a$
    END IF
  END SELECT
 LOOP

 CLOSE #1
 CLOSE #3

 IF mode = 1 THEN spiterror "unterminated globalvariable statement": SYSTEM

END SUB

FUNCTION howmanyargs (n$, f$)
howmanyargs = -1

handle = FREEFILE
OPEN f$ FOR INPUT AS #handle

DO
 IF EOF(handle) THEN EXIT DO
 LINE INPUT #handle, name$
 LINE INPUT #handle, number$
 LINE INPUT #handle, argc$
 IF name$ = n$ THEN howmanyargs = VAL(argc$) - 1
 FOR i = 1 TO VAL(argc$)
  LINE INPUT #handle, arg$
 NEXT i
LOOP

CLOSE #handle
END FUNCTION

SUB identifymn (s$, f$, t, v)

t = 2
v = seekmatch(s$, "hspeak.tmp/flow.txt", false)
IF v >= 0 THEN EXIT SUB

t = 5
v = seekmatch(s$, "hspeak.tmp/math.txt", true)
IF v >= 0 THEN EXIT SUB

t = 6
v = seekmatch(s$, "hspeak.tmp/function.txt", true)
IF v >= 0 THEN EXIT SUB

t = 7
v = seekmatch(s$, "hspeak.tmp/scripts.txt", true)
IF v >= 0 THEN EXIT SUB

t = 3
v = seekmatch(s$, "hspeak.tmp/global.txt", false)
IF v >= 0 THEN EXIT SUB

IF isfile("hspeak.tmp/" + f$ + ".var" + CHR$(0)) THEN
 t = 4
 v = seekmatch(s$, "hspeak.tmp/" + f$ + ".var", false)
 IF v >= 0 THEN EXIT SUB
END IF

t = 0
v = -1

END SUB

SUB insert (buf(), last, dest, v)
 last = last + 1
 FOR i = last TO dest + 1 STEP -1
  buf(i) = buf(i - 1)
 NEXT i
 buf(dest) = v
END SUB

FUNCTION isamong (s$, sep$)
isamong = false
FOR i = 1 TO LEN(sep$)
 IF LEFT$(s$, 1) = MID$(sep$, i, 1) THEN
  isamong = true
 END IF
NEXT i
END FUNCTION

FUNCTION isnumeric (s$)
isnumeric = true

IF NOT isamong(MID$(s$, 1, 1), "-0123456789") THEN isnumeric = false
FOR i = 2 TO LEN(s$)
 IF NOT isamong(MID$(s$, i, 1), "0123456789") THEN isnumeric = false
NEXT i

IF s$ = "-" THEN isnumeric = false

END FUNCTION

FUNCTION isoption (o$)

isoption = false

FOR i = 1 TO LEN(option$)
 IF isamong(MID$(option$, i, 1), o$) THEN
  isoption = true
  EXIT FUNCTION
 END IF
NEXT i

END FUNCTION

SUB loadbin2buf (f$, buf(), size)
 fh = FREEFILE
 OPEN f$ FOR BINARY AS #fh
  ptr = -1
  DO
   ptr = ptr + 1
   IF EOF(fh) THEN EXIT DO
   GET #fh, (ptr * 2) + 1, buf(ptr)
  LOOP
 CLOSE #fh
 size = ptr
END SUB

SUB localtables

OPEN "hspeak.tmp\scripts.txt" FOR INPUT AS #1

DO
 IF EOF(1) THEN EXIT DO
 LINE INPUT #1, name$
 LINE INPUT #1, num$
 LINE INPUT #1, argc$
 FOR i = 1 TO VAL(argc$)
  LINE INPUT #1, arg$
 NEXT i
 count = VAL(argc$)
 IF NOT isfile("hspeak.tmp\" + num$ + ".txt" + CHR$(0)) THEN spiterror "script " + name$ + " does not exist": SYSTEM
 OPEN "hspeak.tmp\" + num$ + ".txt" FOR INPUT AS #2
 DO
  IF EOF(2) THEN EXIT DO
  LINE INPUT #2, a$
  SELECT CASE mode
   CASE 0'--seek variable declaration---
    IF a$ = "variable" THEN
     LINE INPUT #2, b$
     IF b$ <> "begin" THEN
      spiterror "beginningless variable statement": SYSTEM
     ELSE
      mode = 1
      OPEN "hspeak.tmp\" + num$ + ".var" FOR APPEND AS #3
     END IF
    END IF
   CASE 1
    IF a$ = "end" THEN
     mode = 0
     CLOSE #3
    ELSE
     IF isnumeric(a$) THEN spiterror "possible name conflict: expected name but found number " + a$ + " instead": SYSTEM
     PRINT #3, a$
     PRINT #3, LTRIM$(STR$(count))
     count = count + 1
    END IF
  END SELECT
 LOOP
 IF mode = 1 THEN spiterror "unterminated variable statement": SYSTEM
 CLOSE #2
LOOP

CLOSE #1

END SUB

SUB lumpout (f$)

DIM buf(32767)

flst$ = "hspeak.tmp\hsxlist.txt"
findfiles "hspeak.tmp\*.hsx" + CHR$(0), 32, flst$ + CHR$(0), buf()

lst$ = "hspeak.tmp\lumplist.txt"
IF isfile(lst$ + CHR$(0)) THEN KILL lst$

handle = FREEFILE
OPEN lst$ FOR APPEND AS #handle
 PRINT #handle, "HS" + CHR$(0)
 PRINT #handle, "scripts.txt" + CHR$(0)

 ihandle = FREEFILE
 OPEN flst$ FOR INPUT AS #ihandle
 DO
  IF EOF(ihandle) THEN EXIT DO
  LINE INPUT #ihandle, a$
  PRINT #handle, a$ + CHR$(0)
 LOOP
 CLOSE #ihandle
 
CLOSE #handle

lumpfiles lst$ + CHR$(0), f$ + CHR$(0), "hspeak.tmp\", buf()

END SUB

FUNCTION makebranch (buf(), last, argoff(), cache(), inptr, outptr, cptr, fptr)
skip = 2
'recursive
n$ = "statement" + LTRIM$(STR$(iptr))

'---remeber input/output pointer
iptr = inptr
optr = outptr

'---look to see if statement was already cached
FOR i = 0 TO cptr
 IF cache(i * 3 + 0) = buf(iptr) AND cache(i * 3 + 1) = buf(iptr + 1) THEN
  makebranch = cache(i * 3 + 2)
  EXIT FUNCTION
 END IF
NEXT i

PUT #fptr, 1 + (2 * optr), buf(iptr)          '--write the type
PUT #fptr, 1 + (2 * (optr + 1)), buf(iptr + 1)'--write the value

'--get the arguments
seekparen iptr, last, buf(), bp, ep, numargs, argoff(), n$, 16383

'--figure out if caching is neccisary
IF buf(iptr) = 1 OR buf(iptr) = 3 OR buf(iptr) = 4 OR (buf(iptr) = 6 AND numargs = 0) OR (buf(iptr) = 7 AND numargs = 0) THEN
 IF cptr < 512 THEN '--dont overflow the cache!
  cptr = cptr + 1
  cache(cptr * 3 + 0) = buf(iptr)
  cache(cptr * 3 + 1) = buf(iptr + 1)
  cache(cptr * 3 + 2) = optr - skip
 END IF
END IF

'write arguments for those that need it
IF buf(iptr) = 2 OR buf(iptr) = 5 OR buf(iptr) = 6 OR buf(iptr) = 7 THEN
 '--write the argument count
 PUT #fptr, 1 + (2 * (optr + 2)), numargs
 '--advance the output pointer
 outptr = optr + 3 + numargs
 FOR i = 0 TO numargs - 1
  '--reget the arguments
  seekparen iptr, last, buf(), bp, ep, numargs, argoff(), n$, 16383
  inptr = argoff(i)
  branch = makebranch(buf(), last, argoff(), cache(), inptr, outptr, cptr, fptr)
  PUT #fptr, 1 + (2 * (optr + 3 + i)), branch
 NEXT i
ELSE
 '--advance the output pointer for argless stuff
 outptr = optr + 2
END IF

'---the return value will be the zero-based byte offset of the current statement
makebranch = optr - skip

'0 terminate script
'1 literal number       cached
'2 flow control
'3 global variable      cached
'4 local variable       cached
'5 math function
'6 function call        cached if zero-arg
'7 script call          cached if zero-arg
END FUNCTION

SUB makeflowtable (f$)

DIM flow$(15)
flow$(0) = "do"     'a function of unspecified size that returns true when complete, or false if broken
flow$(1) = "begin"  'not interpreted
flow$(2) = "end"    'not interpreted
flow$(3) = "return" 'exit current script and return enclosed var
flow$(4) = "if"     'absorbs if and then
flow$(5) = "then"   'absorbed into if, breakless do
flow$(6) = "else"   'absorbed into if, breakless do
flow$(7) = "for"    'absorbs do
flow$(8) = "cfor"   'reserved
flow$(9) = "foreach"'reserved
flow$(10) = "while" 'absorbs do
flow$(11) = "break" 'prematurely terminates a do block

OPEN f$ FOR OUTPUT AS #1

FOR i = 0 TO 11
 PRINT #1, flow$(i)
 PRINT #1, LTRIM$(STR$(i))
NEXT i

CLOSE #1

END SUB

SUB makemathtable (f$)

DIM math$(18), default$(18)
math$(0) = "random":                 default$(0) = "1"
math$(1) = "exponent":               default$(1) = "2"
math$(2) = "modulus":                default$(2) = "1"
math$(3) = "divide":                 default$(3) = "1"
math$(4) = "multiply":               default$(4) = "1"
math$(5) = "subtract":               default$(5) = "0"
math$(6) = "add":                    default$(6) = "0"
math$(7) = "xor":                    default$(7) = "1"
math$(8) = "or":                     default$(8) = "0"
math$(9) = "and":                    default$(9) = "1"
math$(10) = "equal":                 default$(10) = "0"
math$(11) = "notequal":              default$(11) = "0"
math$(12) = "lessthan":              default$(12) = "0"
math$(13) = "greaterthan":           default$(13) = "0"
math$(14) = "lessthanorequalto":     default$(14) = "0"
math$(15) = "greaterthanorequalto":  default$(15) = "0"
math$(16) = "setvariable":           default$(16) = "0"
math$(17) = "increment":             default$(17) = "1"
math$(18) = "decrement":             default$(18) = "1"

OPEN f$ FOR OUTPUT AS #1

FOR i = 0 TO 18
 PRINT #1, math$(i)
 PRINT #1, LTRIM$(STR$(i))
 PRINT #1, "2"
 PRINT #1, "0"
 PRINT #1, default$(i)
NEXT i

CLOSE #1

END SUB

SUB makeworkdir (wd$)
IF NOT isdir(wd$ + CHR$(0)) THEN
 MKDIR wd$
ELSE
 OPEN wd$ + "\kill.me" FOR BINARY AS #1: CLOSE #1
 KILL wd$ + "\*.*"
END IF
END SUB

FUNCTION nameof$ (value, kind, num$)

n$ = ""

SELECT CASE kind
CASE 1
 n$ = LTRIM$(STR$(value))
CASE 2
 n$ = consultlist$(value, "hspeak.tmp\flow.txt", false)
 'n$ = "FLOW" + LTRIM$(STR$(value))
CASE 3
 n$ = consultlist$(value, "hspeak.tmp\global.txt", false)
 'n$ = "GLOBAL" + LTRIM$(STR$(value))
CASE 4
 n$ = consultlist$(value, "hspeak.tmp\" + num$ + ".var", false)
 'n$ = "LOCAL" + LTRIM$(STR$(value))
CASE 5
 n$ = consultlist$(value, "hspeak.tmp\math.txt", true)
 'n$ = "MATH" + LTRIM$(STR$(value))
CASE 6
 n$ = consultlist$(value, "hspeak.tmp\function.txt", true)
 'n$ = "FUNCTION" + LTRIM$(STR$(value))
CASE 7
 n$ = consultlist$(value, "hspeak.tmp\scripts.txt", true)
 'n$ = "SCRIPT" + LTRIM$(STR$(value))
CASE ELSE
 n$ = "ERROR"
END SELECT

nameof = n$

'0 terminate script
'1 literal number
'2 flow control
'3 global variable
'4 local variable
'5 math function
'6 function call
'7 script call

END FUNCTION

SUB normalflow (buf(), last, f$)

 DIM argoff(16383)

 OPEN f$ FOR INPUT AS #2

 DO
  IF EOF(2) THEN EXIT DO
  LINE INPUT #2, name$
  LINE INPUT #2, num$
  num = VAL(num$)

  i = 0
  DO
   IF i > last THEN EXIT DO
   IF buf(i) = 2 AND buf(i + 1) = num THEN
    SELECT CASE buf(i + 1)
    CASE 7' ---for
     flush argoff(), 16383
     seekparen i, last, buf(), bpar, epar, numargs, argoff(), name$, 16383
     IF numargs > 0 THEN
      SELECT CASE buf(argoff(0))
      CASE 3
       spiterror "warning: using global variable as `for' counter"
       buf(argoff(0)) = 1
      CASE 4
       buf(argoff(0)) = 1
       buf(argoff(0) + 1) = (buf(argoff(0) + 1) + 1) * -1
      CASE ELSE
       spiterror "`for' counter must be a variable": SYSTEM
      END SELECT
     END IF
     SELECT CASE numargs
     CASE 3
      'add step 1 if none was specified
      insert buf(), last, epar, 1
      insert buf(), last, epar, 1
     CASE 4
      'already have start end and step
     CASE ELSE
      spiterror "`for' statement without (variable, start, end)": SYSTEM
     END SELECT '--check numargs
     flush argoff(), 16383
     seekparen i, last, buf(), bpar, epar, numargs, argoff(), name$, 16383
     IF buf(epar + 2) = 2 AND buf(epar + 3) = 0 THEN
      '---do block found after for
      absorbnext buf(), last, epar
     ELSE
      spiterror "`for' without `do'": SYSTEM
     END IF
    CASE 10' ---while
     flush argoff(), 16383
     seekparen i, last, buf(), bpar, epar, numargs, argoff(), name$, 16383
     IF numargs <> 1 THEN
      IF numargs = 0 THEN
       spiterror "`while' statement without condition": SYSTEM
      ELSE
       spiterror "`while' statement should have only one argument": SYSTEM
      END IF
     ELSE
      IF buf(epar + 2) = 2 AND buf(epar + 3) = 0 THEN
       '---do block found after while
       absorbnext buf(), last, epar
      ELSE
       spiterror "`while' without `do'": SYSTEM
      END IF
     END IF'--check numargs
    CASE 4' ---if
     flush argoff(), 16383
     seekparen i, last, buf(), bpar, epar, numargs, argoff(), name$, 16383
     IF numargs <> 1 THEN
      IF numargs = 0 THEN
       spiterror "`if' statement without condition": SYSTEM
      ELSE
       spiterror "`if' statement should have only one argument": SYSTEM
      END IF
     ELSE
      IF buf(epar + 2) = 2 AND buf(epar + 3) = 5 THEN
       '---normal if-then
       absorbnext buf(), last, epar
      ELSE
       '---check for if-else
       IF buf(epar + 2) = 2 AND buf(epar + 3) = 6 THEN
        '--if-else found, inserting empty then
        insert buf(), last, epar, 5
        insert buf(), last, epar, 2
       ELSE
        spiterror "`if' statement must be followed by `then' or `else'": SYSTEM
       END IF
      END IF
      '--done with then, checking for else
      flush argoff(), 16383
      seekparen i, last, buf(), bpar, epar, numargs, argoff(), name$, 16383
      IF buf(epar + 2) = 2 AND buf(epar + 3) = 6 THEN
       '---found else
       absorbnext buf(), last, epar
      ELSE
       '---no else present, filling in empty else
       insert buf(), last, epar, 6
       insert buf(), last, epar, 2
      END IF
     END IF'--check numargs
    CASE 3'--return
     flush argoff(), 16383
     seekparen i, last, buf(), bpar, epar, numargs, argoff(), name$, 16383
     IF numargs < 1 THEN
      spiterror "`return' statement needs a value": SYSTEM
     END IF
     IF numargs > 1 THEN
      spiterror "`return' statement can only return one value": SYSTEM
     END IF
    END SELECT
   END IF
   i = i + 2 '--next instruction
  LOOP

 LOOP
 CLOSE #2

 '--enclose the script in a do
 i = last - 1
 insert buf(), last, i, 2
 insert buf(), last, i, 2
 insert buf(), last, 0, 1
 insert buf(), last, 0, 2
 insert buf(), last, 0, 0
 insert buf(), last, 0, 2

END SUB

SUB onebyone

DIM buf(-2 TO 16383)

IF isoption("v") THEN verbose = true ELSE verbose = false

OPEN "hspeak.tmp\scripts.txt" FOR INPUT AS #1

DO
 IF EOF(1) THEN EXIT DO
 LINE INPUT #1, name$
 LINE INPUT #1, num$
 num = VAL(num$)
 LINE INPUT #1, argc$
 FOR i = 1 TO VAL(argc$)
  LINE INPUT #1, arg$
 NEXT i
 '---Load the Bin file
 flush buf(), 16383
 loadbin2buf "hspeak.tmp\" + num$ + ".bin", buf(), ptr
 IF verbose THEN PRINT name$ + ": framing operators" ELSE PRINT ".";
 betweenable buf(), ptr
' savebuf2bin "hspeak.tmp\" + num$ + ".bin", buf(), ptr
 IF verbose THEN PRINT name$ + ": enforcing arguments" ELSE PRINT ".";
 enforceargs buf(), ptr, "hspeak.tmp\function.txt", 6
 enforceargs buf(), ptr, "hspeak.tmp\scripts.txt", 7
 IF verbose THEN PRINT name$ + ": verifying math" ELSE PRINT ".";
 verifymath buf(), ptr, "hspeak.tmp\math.txt"
 IF verbose THEN PRINT name$ + ": normalizing flow control" ELSE PRINT ".";
 normalflow buf(), ptr, "hspeak.tmp\flow.txt"
 '---Save the spiffyed-up bin file
 savebuf2bin "hspeak.tmp\" + num$ + ".bin", buf(), ptr
 IF verbose THEN PRINT name$ + ": climbing the execution tree" ELSE PRINT ".";
 exetree buf(), ptr, num$, name$
 IF verbose THEN PRINT name$ + ": decompiling for verification" ELSE PRINT ".";
 decompile "hspeak.tmp\" + num$ + ".hsx", "hspeak.tmp\" + num$ + ".dec", num$
 lst = FREEFILE
LOOP

IF NOT verbose THEN PRINT ""

CLOSE #1

END SUB

SUB operatortable (in$, out$)

 DIM op$(-1 TO 255, 2)
 opptr = 0

 OPEN in$ FOR INPUT AS #1

 DO
  IF EOF(1) THEN EXIT DO
  LINE INPUT #1, a$
  SELECT CASE mode
   CASE 0'---Seek a defineoperator statement---
    IF a$ = "defineoperator" THEN
     LINE INPUT #1, b$
     IF b$ <> "begin" THEN spiterror "beginningless defineoperator statement": SYSTEM
     mode = 1
    END IF
   CASE 1'---parsing a defineoperator block---
    IF a$ = "end" THEN
     mode = 0
    ELSE
     LINE INPUT #1, oper$
     LINE INPUT #1, funct$
     IF NOT isnumeric(a$) THEN spiterror "defineoperator: expected priority number but found name " + a$: SYSTEM
     IF isnumeric(oper$) THEN spiterror "defineoperator: expected operator but found number": SYSTEM
     IF isnumeric(funct$) THEN spiterror "defineoperator: expected function name but found number": SYSTEM
     IF opptr > 255 THEN spiterror "defineoperator: too many operators defined. Only 255 are allowed... wow. What do you need with so many?": SYSTEM
     op$(opptr, 0) = oper$
     op$(opptr, 1) = funct$
     op$(opptr, 2) = a$
     opptr = opptr + 1
    END IF
  END SELECT
 LOOP

 CLOSE #1

 IF mode = 1 THEN spiterror "unterminated defineoperator statement": SYSTEM

 '--sort by priority
 FOR i = 1 TO opptr - 1
  j = i
  WHILE VAL(op$(j, 2)) < VAL(op$(j - 1, 2)) AND j > 0
   SWAP op$(j, 0), op$(j - 1, 0)
   SWAP op$(j, 1), op$(j - 1, 1)
   SWAP op$(j, 2), op$(j - 1, 2)
   j = j - 1
  WEND
 NEXT i

 OPEN "hspeak.tmp/operate.txt" FOR OUTPUT AS #3
 FOR i = 0 TO opptr - 1
  PRINT #3, op$(i, 0)
  PRINT #3, op$(i, 1)
 NEXT i
 CLOSE #3

 '--now we substitute in all of the functions for the operators
 OPEN in$ FOR INPUT AS #1
 OPEN out$ FOR OUTPUT AS #2
 DO
  IF EOF(1) THEN EXIT DO
  LINE INPUT #1, a$
  FOR i = 0 TO opptr - 1
   IF a$ = op$(i, 0) THEN a$ = op$(i, 1)
  NEXT i
  PRINT #2, a$
 LOOP

CLOSE #1
CLOSE #2

END SUB

REM $DYNAMIC
FUNCTION parsecline (cl$, infile$, outfile$, option$)
cl$ = LCASE$(cl$)
IF cl$ = "" OR cl$ = "/?" OR cl$ = "-?" OR cl$ = "?" OR cl$ = "help" OR cl$ = "/help" OR cl$ = "-help" THEN
 PRINT "HamsterSpeak semicompiler (C)1999 James Paige, Hamster Republic Productions"
 PRINT " HSPEAK [-kvy] scriptfile [output.hs]"
 PRINT ""
 PRINT " k  ...keep temporary files for debugging purposes"
 PRINT " v  ...verbose output"
 PRINT " y  ...overwrite existing output file without asking"
 parsecline = true: EXIT FUNCTION
END IF

a$ = firststr(cl$, " ", false)
IF LEFT$(a$, 1) = "-" THEN
 option$ = RIGHT$(a$, LEN(a$) - 1)
 a$ = firststr(cl$, " ", true)
END IF

infile$ = firststr(cl$, " ", true)
outfile$ = firststr(cl$, " ", true)

IF NOT isfile(infile$ + CHR$(0)) THEN
 PRINT "input file ";
 COLOR 14
 PRINT infile$;
 COLOR 7
 PRINT " does not exist"
 parsecline = -1: EXIT FUNCTION
END IF

IF outfile$ = "" THEN
 outfile$ = firststr(infile$, ".", false) + ".hs"
END IF

IF isfile(outfile$ + CHR$(0)) THEN
 IF permission(outfile$ + " already exists. overwrite it? (y/n)") THEN
  KILL outfile$
 ELSE
  PRINT outfile$ + " will not be overwritten."
  parsecline = true: EXIT FUNCTION
 END IF
END IF

PRINT "semicompiling " + infile$ + " to " + outfile$

END FUNCTION

FUNCTION permission (prompt$)

IF isoption("yY") THEN permission = true: EXIT FUNCTION

PRINT prompt$

DO
w$ = INPUT$(1)
w$ = LCASE$(w$)
SELECT CASE w$
 CASE "y"
  permission = true: EXIT FUNCTION
 CASE "n"
  permission = false: EXIT FUNCTION
END SELECT
LOOP

END FUNCTION

REM $STATIC
SUB readall (infile$, outfile)

fhandle = FREEFILE
OPEN infile$ FOR INPUT AS fhandle

DIM sep$(11)

sep$(1) = "+"
sep$(2) = "--"
sep$(3) = "/"
sep$(4) = "*"
sep$(5) = "^"
sep$(6) = "=="
sep$(7) = "<>"
sep$(8) = ">>"
sep$(9) = "<<"
sep$(10) = "<="
sep$(11) = ">="

DO
 IF EOF(fhandle) THEN EXIT DO
 LINE INPUT #fhandle, a$
 a$ = LCASE$(exclude(comment$(a$, "#"), " " + CHR$(9)))
 a$ = sed(a$, ")", ",end,")
 a$ = sed(a$, "(", ",begin,")
 FOR i = 1 TO 11
  a$ = sed(a$, sep$(i), "," + sep$(i) + ",")
 NEXT i
 DO WHILE LEN(a$) > 0
  b$ = firststr(a$, ",", true)
  IF b$ = "include" THEN
   f$ = firststr(a$, ",()", true)
   IF isfile(f$ + CHR$(0)) THEN
    PRINT "including " + f$
    readall f$, outfile
   ELSE
    IF RIGHT$(f$, 4) = ".hsi" THEN
     spiterror "include file " + f$ + " was not found. You can create it by opening your RPG file in CUSTOM.EXE and picking `Export' from the `Script Management' menu"
    ELSE
     spiterror "include file " + f$ + " was not found!"
    END IF
   END IF
  ELSE '---not an include
   IF b$ <> "" THEN
    PRINT #outfile, b$
   END IF
  END IF '--is include?
 LOOP
LOOP

CLOSE fhandle

END SUB

SUB savebuf2bin (f$, buf(), size)
 IF isfile(f$ + CHR$(0)) THEN KILL f$
 fh = FREEFILE
 OPEN f$ FOR BINARY AS #fh
  ptr = -1
  DO
   ptr = ptr + 1
   IF ptr > size THEN EXIT DO
   PUT #fh, (ptr * 2) + 1, buf(ptr)
  LOOP
 CLOSE #fh
END SUB

FUNCTION sed$ (s$, old$, new$)

out$ = s$

i = 0
DO
 i = i + 1
 IF i > LEN(out$) THEN EXIT DO
 IF MID$(out$, i, LEN(old$)) = old$ THEN
  out$ = LEFT$(out$, i - 1) + new$ + RIGHT$(out$, LEN(out$) - (i + LEN(old$) - 1))
  IF LEN(new$) > 1 THEN i = i + (LEN(new$) - 1)
 END IF
LOOP

sed$ = out$

END FUNCTION

FUNCTION seekmatch (n$, f$, args)

seekmatch = -1

handle = FREEFILE
OPEN f$ FOR INPUT AS #handle

DO
 IF EOF(handle) THEN EXIT DO
 LINE INPUT #handle, name$
 LINE INPUT #handle, number$
 IF n$ = name$ THEN seekmatch = VAL(number$): EXIT DO
 IF args THEN
  LINE INPUT #handle, argc$
  FOR i = 1 TO VAL(argc$)
   LINE INPUT #handle, arg$
  NEXT i
 END IF
LOOP

CLOSE #handle

END FUNCTION

SUB seekparen (index, last, buf(), bp, ep, numargs, argoff(), name$, limit)

bp = index + 2

'---allow default filling for parenless calls
IF buf(bp) <> 2 OR buf(bp + 1) <> 1 THEN
 insert buf(), last, bp, 2
 insert buf(), last, bp, 2 'end
 insert buf(), last, bp, 1
 insert buf(), last, bp, 2 'begin
END IF

depth = 1
i = bp + 2
numargs = 0
DO
 IF i > last THEN spiterror name$ + " statement never ends": SYSTEM
 IF buf(i) = 2 AND (buf(i + 1) = 1 OR buf(i + 1) = 2) THEN
  IF buf(i + 1) = 1 THEN depth = depth + 1
  IF buf(i + 1) = 2 THEN depth = depth - 1
 ELSE
  IF depth = 1 THEN
   argoff(numargs) = i
   numargs = numargs + 1
   IF numargs > limit THEN spiterror "too many statments in block " + name$ + " the limit is" + STR$(limit): SYSTEM
  END IF
 END IF
 IF depth = 0 THEN ep = i: EXIT DO
 i = i + 2
LOOP

END SUB

SUB separatescripts (in$)

PRINT "separating scripts";

OPEN in$ FOR INPUT AS #1

 DO
  IF EOF(1) THEN EXIT DO
  LINE INPUT #1, a$
  SELECT CASE mode
   CASE 0'---seek script---
    IF a$ = "script" THEN
     mode = 1
    ELSE
     spiterror "invalid declaration " + a$ + ". It is possible that you may have an uneccisary `end' or `)' that is prematurely ending the script " + thisscript$: SYSTEM
    END IF
   CASE 1'---separate script---
    id = seekmatch(a$, "hspeak.tmp\scripts.txt", true)
    IF id < 0 THEN
     spiterror a$ + " has not been defined as a script": SYSTEM
    ELSE
     thisscript$ = a$
     argc = howmanyargs(a$, "hspeak.tmp\scripts.txt")
     '--this reads local variable names for each of your arguments
     IF argc > -1 THEN
      varfile = FREEFILE
      OPEN "hspeak.tmp\" + LTRIM$(STR$(id)) + ".var" FOR OUTPUT AS #varfile
      FOR i = 0 TO argc
       LINE INPUT #1, arg$
       IF arg$ = "begin" OR arg$ = "end" THEN spiterror a$ + " needs a name for argument" + STR$(i): SYSTEM
       PRINT #varfile, arg$
       PRINT #varfile, LTRIM$(STR$(i))
      NEXT i
      CLOSE #varfile
     END IF
     LINE INPUT #1, b$
     IF b$ <> "begin" THEN
      spiterror a$ + ": beginningless script. Perhaps you tried to name undeclared arguments? " + a$ + " is only supposed to have" + STR$(argc) + " arguments": SYSTEM
     ELSE
      PRINT ".";
      depth = 1
      mode = 2
      count = count + 1
      outfile = FREEFILE
      OPEN "hspeak.tmp\" + LTRIM$(STR$(id)) + ".txt" FOR OUTPUT AS #outfile
     END IF
    END IF
   CASE 2'---writing single script---
    IF a$ = "script" THEN spiterror thisscript$ + ": script is missing end": SYSTEM
    IF a$ = "begin" THEN depth = depth + 1
    IF a$ = "end" THEN depth = depth - 1
    IF depth = 0 THEN
     CLOSE #outfile
     mode = 0
    ELSE
     PRINT #outfile, a$
    END IF
  END SELECT
 LOOP

IF mode <> 0 THEN spiterror thisscript$ + ": script is missing end": SYSTEM

CLOSE #1

PRINT ""

END SUB

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

SUB spiterror (e$)
s$ = e$
li$ = ""
PRINT ""
COLOR 4
DO
 IF LEN(s$) = 0 THEN EXIT DO
 tmp$ = firststr$(s$, " ", true)
 IF LEN(li$) + LEN(tmp$) >= 79 THEN
  PRINT li$
  li$ = tmp$
 ELSE
  IF LEN(li$) > 0 THEN li$ = li$ + " "
  li$ = li$ + tmp$
 END IF
LOOP
IF LEN(li$) > 0 THEN PRINT li$
COLOR 7
END SUB

SUB stripdeclare (in$, out$)

 OPEN in$ FOR INPUT AS #1
 OPEN out$ FOR OUTPUT AS #2

 DO
  IF EOF(1) THEN EXIT DO
  LINE INPUT #1, a$
  SELECT CASE mode
   CASE 0'---Seeking---
    IF a$ = "definefunction" OR a$ = "definescript" OR a$ = "globalvariable" OR a$ = "defineoperator" THEN
     LINE INPUT #1, b$
     IF b$ <> "begin" THEN spiterror "logically impossible error: unwarp reality and try again": SYSTEM
     mode = 1
    ELSE
     PRINT #2, a$
    END IF
   CASE 1'---skipping definition block---
    IF a$ = "end" THEN mode = 0
  END SELECT
 LOOP

CLOSE #1
CLOSE #2

END SUB

SUB tobinary

OPEN "hspeak.tmp\scripts.txt" FOR INPUT AS #1

DO
 IF EOF(1) THEN EXIT DO
 LINE INPUT #1, name$
 LINE INPUT #1, num$
 LINE INPUT #1, argc$
 FOR i = 1 TO VAL(argc$)
  LINE INPUT #1, arg$
 NEXT i
 OPEN "hspeak.tmp\" + num$ + ".num" FOR INPUT AS #2
 OPEN "hspeak.tmp\" + num$ + ".bin" FOR BINARY AS #3
 ptr = 1
 DO
  IF EOF(2) THEN EXIT DO
  LINE INPUT #2, a$
  cl = VAL(firststr(a$, ",", true))
  vl = VAL(RIGHT$(a$, LEN(a$) - 1))
  PUT #3, ptr, cl
  PUT #3, ptr + 2, vl
  ptr = ptr + 4
 LOOP
 CLOSE #2
 CLOSE #3
LOOP

CLOSE #1

END SUB

SUB tonumbers

ptr = 0

OPEN "hspeak.tmp\scripts.txt" FOR INPUT AS #1

DO
 IF EOF(1) THEN EXIT DO
 LINE INPUT #1, name$
 LINE INPUT #1, num$
 LINE INPUT #1, argc$
 FOR i = 1 TO VAL(argc$)
  LINE INPUT #1, arg$
 NEXT i
 OPEN "hspeak.tmp\" + num$ + ".txt" FOR INPUT AS #2
 OPEN "hspeak.tmp\" + num$ + ".num" FOR OUTPUT AS #3
 DO
  IF EOF(2) THEN EXIT DO
  LINE INPUT #2, a$
  class$ = "0"
  value$ = "0"
  IF a$ = "variable" THEN
   DO
    IF EOF(2) THEN spiterror "Impossible loop condition: Stop writing impossible code!": SYSTEM
    LINE INPUT #2, a$
   LOOP UNTIL a$ = "end"
  ELSE
   '---strip local variable declarations
   IF isnumeric(a$) THEN
    class$ = "1"
    value$ = a$
   ELSE
    identifymn a$, num$, cl, vl
    IF vl = -1 THEN
     spiterror a$ + " is not a keyword, and has not been defined as a script, constant, global variable, local variable, argument, or aything else :(": SYSTEM
    END IF
    class$ = LTRIM$(STR$(cl))
    value$ = LTRIM$(STR$(vl))
   END IF
   PRINT #3, class$ + "," + value$
  END IF
 LOOP
 CLOSE #2
 CLOSE #3
LOOP

CLOSE #1

END SUB

SUB verifymath (buf(), last, f$)

 DIM argoff(31)

 OPEN f$ FOR INPUT AS #2

 DO
  IF EOF(2) THEN EXIT DO
  LINE INPUT #2, name$
  LINE INPUT #2, num$
  LINE INPUT #2, argc$
  FOR i = 1 TO VAL(argc$)
   INPUT #2, dummy$
  NEXT i
  num = VAL(num$)

  i = 0
  DO
   IF i > last THEN EXIT DO
   IF buf(i) = 5 AND buf(i + 1) = num THEN
    flush argoff(), 31
    seekparen i, last, buf(), bpar, epar, numargs, argoff(), name$, 32
    IF numargs <> 2 AND num <= 16 THEN
     spiterror name$ + " must have two arguments. You may be using the obsolete form of the command. Instead of " + name$ + "(n,n) you should write n," + name$ + ",n": SYSTEM
    END IF
    '--variable setting routines
    IF num >= 16 THEN
     SELECT CASE buf(argoff(0))
     CASE 3'global
      buf(argoff(0)) = 1
     CASE 4'local
      buf(argoff(0)) = 1
      buf(argoff(0) + 1) = (buf(argoff(0) + 1) + 1) * -1
     CASE ELSE
      spiterror "the first argument of " + name$ + " must be a variable": SYSTEM
     END SELECT
     IF numargs > 2 THEN
      spiterror name$ + " has" + STR$(numargs - argc) + " more arguments than it is supposed to have": SYSTEM
     END IF
     IF numargs < 2 THEN
      SELECT CASE num
      CASE 16 '--it is setvariable
       default = 0
      CASE 17, 18 '--it is increment or decrement
       default = 1
      END SELECT
      insert buf(), last, epar, default '--insert default
      insert buf(), last, epar, 1 '--make it a literal number
      epar = epar + 2
     END IF
    END IF
   END IF
   i = i + 2 '--next instruction
  LOOP

 LOOP

 CLOSE #2

END SUB

SUB writeheader (f$)

IF isfile(f$ + CHR$(0)) THEN KILL f$

handle = FREEFILE
OPEN f$ FOR BINARY AS #handle

a$ = "HamsterSpeak"
PUT #handle, 1, a$
n = version
PUT #handle, 13, n

CLOSE #handle

END SUB

