'OHRRPGCE UNLUMP - RPG File unlumping utility
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' This utility is semi-obsolete, and could easily be replaced with a
' Euphoria version based on the unlumping code in hsspiffy.e
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION readkey$ ()
DECLARE FUNCTION editstr$ (stri$, key$, cur%, max%, number%)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION rightafter$ (s$, d$)
DECLARE SUB forcewd (wd$)
DECLARE FUNCTION getcurdir$ ()
DECLARE FUNCTION getrundir$ ()
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB readscatter (s$, lhold%, array%(), start%)
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
'assembly subs and functions
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
DECLARE SUB copyfile (s$, d$, buf())
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
DECLARE SUB lumpfiles (listf$, lump$, path$, buffer())
DECLARE SUB unlump (lump$, ulpath$, buffer())
DECLARE SUB unlumpfile (lump$, fmask$, path$, buf())
DECLARE SUB array2str (arr(), BYVAL o, s$)
DECLARE SUB str2array (s$, arr(), BYVAL o)
DECLARE SUB getstring (path$)
DECLARE FUNCTION pathlength ()
DECLARE FUNCTION rpathlength ()
DECLARE FUNCTION envlength (e$)
DECLARE FUNCTION drivelist (drbuf())
DECLARE SUB setdrive (BYVAL drive)
DECLARE FUNCTION isfile (n$)
DECLARE FUNCTION isdir (dir$)
DECLARE FUNCTION isremovable (BYVAL d)
DECLARE FUNCTION isvirtual (BYVAL d)
DECLARE FUNCTION hasmedia (BYVAL d)

CONST true = -1
CONST false = 0

DIM buffer(16383)

olddir$ = getcurdir
forcewd getrundir

IF COMMAND$ = "" THEN
  PRINT "O.H.R.RPG.C.E. game unlumping utility"
  PRINT ""
  PRINT "syntax:"
  PRINT "unlump filename.rpg directory"
  PRINT ""
  PRINT "A utility to extract the contents of an RPG file to a directory"
  PRINT "so that advanced users can hack the delicious morsels inside."
  PRINT "If a password is required, you will be prompted to enter it."
  fatalerror ""
END IF

lump$ = LTRIM$(RTRIM$(LEFT$(COMMAND$, INSTR(COMMAND$, " "))))
dest$ = LTRIM$(RTRIM$(RIGHT$(COMMAND$, LEN(COMMAND$) - INSTR(COMMAND$, " "))))
IF lump$ = "" THEN lump$ = dest$: dest$ = ""
IF dest$ = "" THEN
  dest$ = LEFT$(lump$, LEN(lump$) - (LEN(rightafter(lump$, ".")) + 1))
  IF LEN(rightafter(lump$, ".")) > 3 OR dest$ = "" THEN fatalerror "please specify an output directory"
END IF

IF NOT isfile(lump$ + CHR$(0)) THEN fatalerror "lump file `" + lump$ + "' was not found"

game$ = rightafter(lump$, "\")
IF game$ = "" THEN game$ = lump$
IF INSTR(game$, ".") THEN game$ = LEFT$(game$, INSTR(game$, ".") - 1)

IF isfile(dest$ + CHR$(0)) THEN fatalerror "destination directory `" + dest$ + "' already exists as a file"

IF isdir(dest$ + CHR$(0)) THEN
 PRINT "destination directory `" + dest$ + "' already exists. use it anyway? (y/n)"
 w$ = readkey
 IF w$ <> "Y" AND w$ <> "y" THEN SYSTEM
ELSE
 MKDIR dest$
END IF

IF NOT isdir(dest$ + CHR$(0)) THEN fatalerror "unable to create destination directory `" + dest$ + "'"

unlumpfile lump$ + CHR$(0), "archinym.lmp", dest$ + "\", buffer()

'--set game$ according to the archinym
IF isfile(dest$ + "\archinym.lmp" + CHR$(0)) THEN
 fh = FREEFILE
 OPEN dest$ + "\archinym.lmp" FOR INPUT AS #fh
 LINE INPUT #fh, a$
 CLOSE #fh
 IF LEN(a$) <= 8 THEN
  game$ = a$
 END IF
END IF

unlumpfile lump$ + CHR$(0), game$ + ".gen", dest$ + "\", buffer()

xbload dest$ + "\" + game$ + ".gen", buffer(), "unable to open general data"

KILL dest$ + "\" + game$ + ".gen"

passokay = true

IF buffer(94) > -1 THEN
 passokay = false
 '----load password-----
 readscatter rpas$, buffer(94), buffer(), 200
 rpas$ = rotascii(rpas$, buffer(93) * -1)
 'PRINT rpas$
 '-----get inputed password-----
 pas$ = ""
 DO
   w$ = readkey$
   IF w$ = CHR$(13) THEN
     PRINT ""
     IF pas$ <> rpas$ THEN fatalerror "password mismatch"
     passokay = true
     EXIT DO
   END IF
   LOCATE , 1: FOR i = 1 TO LEN(pas$): PRINT " "; : NEXT i
   pas$ = editstr(pas$, w$, cur, 17, false)
   LOCATE , 1: FOR i = 1 TO LEN(pas$): PRINT "*"; : NEXT i
   dowait
 LOOP
END IF

IF passokay THEN
 REDIM buffer(32767)
 unlump lump$ + CHR$(0), dest$ + "\", buffer()
END IF

forcewd olddir$

SYSTEM

'---DOCUMENTATION OF GENERAL DATA---
'* denotes obsolete fields
'0        number of maps
'1-20    *tilesetassignments
'1        title screen
'2        title music
'3        victory music
'4        default battle music
'5-25    *passcode
'26       max hero graphics 40
'27       max small enemy graphics 149
'28       max med enemy graphics 79
'29       max large graphics 29
'30       max npc graphics 119
'31       max weapon graphics 149
'32       max attack graphics 99
'33       max tilesets 14
'34       max attack definitions 200
'35       max hero definitions 59
'36       max enemy definitions 500
'37       max formations 1000
'38       max palettes 99
'39       max text boxes 999
'40       total available plotscripts
'41       new-game plotscript
'42       game-over plotscript
'43       highest numbered plotscript
'44       suspendstuff bits
'45       cameramode
'46       cameraarg1
'47       cameraarg2
'48       cameraarg3
'49       cameraarg4
'50       script backdrop
'51       days of play
'52       hours of play
'53       minutes of play
'54       seconds of play
'55       max vehicle types

'93       new passcode offset
'94       new passcode length
'95       RPG file format version ID
'96       starting gold
'97       last shop
'98      *old passcode offset
'99      *old passcode length
'100      last screen
'101      general bitsets
'102      starting X
'103      starting Y
'104      starting Map
'105      one-time-NPC indexer
'106-170  one-time-NPC placeholders
'199      start of password scattertable
'200-359  password mess

REM $STATIC
FUNCTION bound (num, min, max)

bound = num
IF num < min THEN bound = min
IF num > max THEN bound = max

END FUNCTION

FUNCTION editstr$ (stri$, key$, cur, max, number)

pre$ = LEFT$(stri$, cur)
post$ = RIGHT$(stri$, LEN(stri$) - cur)

SELECT CASE key$
 CASE CHR$(8)
   'backspace
   IF LEN(pre$) > 0 THEN pre$ = LEFT$(pre$, LEN(pre$) - 1): cur = cur - 1
 CASE CHR$(0) + CHR$(83)
   'delete
   IF LEN(post$) > 0 THEN post$ = RIGHT$(post$, LEN(post$) - 1)
 CASE ELSE
  IF LEN(key$) > 0 THEN
   IF (ASC(key$) >= 32 AND ASC(key$) < 127 AND key$ <> "," AND key$ <> "~" AND NOT number) OR (ASC(key$) >= 48 AND ASC(key$) <= 57 AND number) THEN
    IF LEN(post$) = 0 AND LEN(pre$) < max THEN post$ = " "
    IF LEN(post$) > 0 THEN
     MID$(post$, 1, 1) = key$
     cur = bound(cur + 1, 0, LEN(pre$ + post$))
    END IF
   END IF
  END IF
END SELECT

editstr$ = pre$ + post$


END FUNCTION

SUB fatalerror (e$)

 IF e$ <> "" THEN PRINT "ERROR: " + e$
 SYSTEM

END SUB

SUB forcewd (wd$)

CHDIR wd$
setdrive ASC(UCASE$(LEFT$(wd$, 1))) - 65

END SUB

FUNCTION getcurdir$

sCurdir$ = STRING$(pathlength, 0)
getstring sCurdir$
IF RIGHT$(sCurdir$, 1) = "\" AND LEN(sCurdir$) > 3 THEN sCurdir$ = LEFT$(sCurdir$, LEN(sCurdir$) - 1)

getcurdir$ = sCurdir$

END FUNCTION

FUNCTION getrundir$

rundir$ = STRING$(rpathlength, 0)
getstring rundir$
IF RIGHT$(rundir$, 1) = "\" AND LEN(rundir$) > 3 THEN rundir$ = LEFT$(rundir$, LEN(rundir$) - 1)

getrundir$ = rundir$

END FUNCTION

FUNCTION large (n1, n2)
large = n1
IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION loopvar (var, min, max, inc)
a = var + inc
IF a > max THEN a = a - ((max - min) + 1): loopvar = a: EXIT FUNCTION
IF a < min THEN a = a + ((max - min) + 1): loopvar = a: EXIT FUNCTION
loopvar = a
END FUNCTION

FUNCTION readkey$

 w$ = ""
 WHILE w$ = ""
  w$ = INKEY$
 WEND

 readkey$ = w$

END FUNCTION

SUB readscatter (s$, lhold, array(), start)
DIM stray(10)
s$ = STRING$(20, "!")

FOR i = 0 TO lhold
 setbit stray(), 0, i, readbit(array(), start - 1, array(start + i))
NEXT i

array2str stray(), 0, s$
s$ = LEFT$(s$, INT((lhold + 1) / 8))

END SUB

FUNCTION rightafter$ (s$, d$)

rightafter$ = ""
new$ = ""

FOR i = LEN(s$) TO 1 STEP -1
 IF MID$(s$, i, 1) = d$ THEN
  rightafter$ = new$
  EXIT FOR
 END IF
 new$ = MID$(s$, i, 1) + new$
NEXT i

END FUNCTION

FUNCTION rotascii$ (s$, o)

temp$ = ""

FOR i = 1 TO LEN(s$)
 temp$ = temp$ + CHR$(loopvar(ASC(MID$(s$, i, 1)), 0, 255, o))
NEXT i

rotascii$ = temp$

END FUNCTION

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

SUB xbload (f$, array(), e$)

IF isfile(f$ + CHR$(0)) THEN
 handle = FREEFILE
 OPEN f$ FOR BINARY AS #handle
 bytes = LOF(handle)
 CLOSE #handle
 IF bytes THEN
  DEF SEG = VARSEG(array(0)): BLOAD f$, VARPTR(array(0))
 ELSE
  fatalerror e$ + "(zero byte)"
 END IF
ELSE
 fatalerror e$
END IF

END SUB

