'OHRRPGCE CUSTOM - Even more misc unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION readshopname$ (shopnum%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB writeconstant (filehandle%, num%, name$, unique$(), prefix$)
DECLARE SUB safekill (f$)
DECLARE SUB touchfile (f$)
DECLARE SUB romfontchar (font%(), char%)
DECLARE SUB standardmenu (menu$(), size%, vis%, ptr%, top%, x%, y%, page%, edge%)
DECLARE FUNCTION readitemname$ (index%)
DECLARE FUNCTION readattackname$ (index%)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION getShortName$ (filename$)
DECLARE FUNCTION getLongName$ (filename$)
DECLARE SUB textfatalerror (e$)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION scriptname$ (num%, f$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE FUNCTION getmapname$ (m%)
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION isunique% (s$, u$(), r%)
DECLARE FUNCTION loadname$ (length%, offset%)
DECLARE SUB exportnames (gamedir$, song$())
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE FUNCTION needaddset (ptr%, check%, what$)
DECLARE FUNCTION browse$ (special, default$, fmask$, tmp$)
DECLARE SUB cycletile (cycle%(), tastuf%(), ptr%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%())
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION intstr$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB debug (s$)
DECLARE SUB bitset (array%(), wof%, last%, name$())
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB formation (song$())
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage (song$())
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (master%(), font%())
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)

'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'cglobals.bi'

'$INCLUDE: 'const.bi'

REM $STATIC
SUB debug (s$)
OPEN "c_debug.txt" FOR APPEND AS #3
PRINT #3, s$
CLOSE #3
END SUB

FUNCTION exclude$ (s$, x$)
out$ = ""
FOR i = 1 TO LEN(s$)
 ok = -1
 FOR j = 1 TO LEN(x$)
  IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = 0
 NEXT j
 IF ok THEN out$ = out$ + MID$(s$, i, 1)
NEXT i
exclude$ = out$
END FUNCTION

FUNCTION exclusive$ (s$, x$)
out$ = ""
FOR i = 1 TO LEN(s$)
 ok = 0
 FOR j = 1 TO LEN(x$)
  IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = 1
 NEXT j
 IF ok THEN out$ = out$ + MID$(s$, i, 1)
NEXT i
exclusive$ = out$
END FUNCTION

SUB flusharray (array(), size, value)
FOR i = 0 TO size
 array(i) = value
NEXT i
END SUB

FUNCTION getLongName$ (filename$)
'--given a filename, returns its longname.
'  it will always return the filename only, without the path
'  even though it can accept a fully qualified filename as input

'--has a bug that prevents it from returning files that are longer
'  than 260 chars including pathname

failed = 0
result$ = ""
length = LongNameLength(filename$ + CHR$(0))
IF length = -1 THEN
 '--failed to get any name at all
 failed = -1
ELSE
 a$ = STRING$(length, 0)
 getstring a$
 FOR i = LEN(a$) TO 1 STEP -1
  IF MID$(a$, i, 1) = "\" OR MID$(a$, i, 1) = ":" THEN EXIT FOR
  IF MID$(a$, i, 1) <> CHR$(0) THEN
   result$ = MID$(a$, i, 1) + result$
  END IF
 NEXT i
 IF result$ = "" THEN
  '--never return a null result!
  failed = -1
 END IF
END IF
IF failed THEN
 '--failed, return input (minus path)
 FOR i = LEN(filename$) TO 1 STEP -1
  IF MID$(filename$, i, 1) = "\" OR MID$(filename$, i, 1) = ":" THEN EXIT FOR
  result$ = MID$(filename$, i, 1) + result$
 NEXT i
END IF
getLongName$ = result$
END FUNCTION

FUNCTION intstr$ (n)
IF n < 0 THEN
 intstr$ = STR$(n)
ELSE
 intstr$ = RIGHT$(STR$(n), LEN(STR$(n)) - 1)
END IF
END FUNCTION

FUNCTION numbertail$ (s$)

DIM n AS LONG

IF s$ = "" THEN
 out$ = "BLANK"
ELSE
 a = ASC(RIGHT$(s$, 1))
 IF a < 48 OR a > 57 THEN
  out$ = s$ + "2"
 ELSE
  a$ = s$
  b$ = ""
  DO WHILE ASC(RIGHT$(a$, 1)) >= 48 AND ASC(RIGHT$(a$, 1)) <= 57
   b$ = RIGHT$(a$, 1) + b$
   a$ = LEFT$(a$, LEN(a$) - 1)
   IF LEN(a$) = 0 THEN EXIT DO
  LOOP
  IF LEN(b$) > 9 THEN b$ = "0"
  n = VAL(b$)
  n = n + 1
  out$ = a$ + LTRIM$(STR$(n))
 END IF
END IF

numbertail$ = out$

END FUNCTION

FUNCTION readglobalstring$ (index, default$, maxlen)

fh = FREEFILE
OPEN game$ + ".stt" FOR BINARY AS #fh

a$ = CHR$(0)
GET #fh, 1 + index * 11, a$
namelen = 0: IF a$ <> "" THEN namelen = ASC(a$)

IF index * 11 + i > LOF(fh) THEN
 result$ = default$
ELSE
 result$ = STRING$(small(namelen, maxlen), CHR$(0))
 GET #fh, 2 + index * 11, result$
END IF

CLOSE #fh

readglobalstring = result$
END FUNCTION

FUNCTION rotascii$ (s$, o)

temp$ = ""

FOR i = 1 TO LEN(s$)
 temp$ = temp$ + CHR$(loopvar(ASC(MID$(s$, i, 1)), 0, 255, o))
NEXT i

rotascii$ = temp$

END FUNCTION

SUB textfatalerror (e$)

debug "fatal error:" + e$

touchfile workingdir$ + "\__danger.tmp"

PRINT "fatal error:"
PRINT e$

KILL workingdir$ + "\*.*"
RMDIR workingdir$

SYSTEM

END SUB

FUNCTION unlumpone (lumpfile$, onelump$, asfile$)
unlumpone = 0

IF NOT isdir("unlump1.tmp" + CHR$(0)) THEN MKDIR "unlump1.tmp"
CALL unlump(lumpfile$ + CHR$(0), "unlump1.tmp\", buffer())

IF isfile("unlump1.tmp\" + onelump$ + CHR$(0)) THEN
 copyfile "unlump1.tmp\" + onelump$ + CHR$(0), asfile$ + CHR$(0), buffer()
 unlumpone = -1
END IF

touchfile "unlump1.tmp\nothing.tmp"

KILL "unlump1.tmp\*.*"
RMDIR "unlump1.tmp"

END FUNCTION

SUB writeglobalstring (index, s$, maxlen)

fh = FREEFILE

OPEN game$ + ".stt" FOR BINARY AS #fh

a$ = CHR$(small(LEN(s$), small(maxlen, 255)))
PUT #fh, 1 + index * 11, a$
a$ = LEFT$(s$, small(maxlen, 255))
PUT #fh, 2 + index * 11, a$

CLOSE #fh

END SUB

SUB xbload (f$, array(), e$)

IF isfile(f$ + CHR$(0)) THEN
 handle = FREEFILE
 OPEN f$ FOR BINARY AS #handle
 bytes = LOF(handle)
 CLOSE #handle
 IF bytes THEN
  OPEN f$ FOR BINARY AS #handle
  a$ = " "
  GET #handle, 1, a$
  CLOSE #handle
  IF a$ = CHR$(253) THEN
   DEF SEG = VARSEG(array(0)): BLOAD f$, VARPTR(array(0))
  ELSE
   fatalerror e$ + "(unbloadable)"
  END IF
 ELSE
  fatalerror e$ + "(zero byte)"
 END IF
ELSE
 fatalerror e$
END IF

END SUB

