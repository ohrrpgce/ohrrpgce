'OHRRPGCE UNLUMP - RPG File unlumping utility
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' To compile:
'        fbc -lang deprecated unlump.bas util.bas
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION readkey$ ()
DECLARE FUNCTION editstr$ (stri$, key$, cur%, max%, number%)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION rightafter$ (s$, d$)
DECLARE SUB readscatter (s$, lhold%, array%(), start%)
'DECLARE FUNCTION readpassword$ ()

#include "compat.bi"
#include "util.bi"
#include "const.bi"
#include "lumpfile.bi"

DIM SHARED createddir = 0, dest$, olddir$

rpas$ = ""
cur = 0

olddir$ = curdir

IF COMMAND$ = "" THEN
 PRINT "O.H.R.RPG.C.E. game unlumping utility"
 PRINT ""
 PRINT "syntax:"
 PRINT "unlump filename.rpg directory"
 PRINT ""
 PRINT "A utility to extract the contents of an RPG file or other lumped"
 PRINT "file to a directory so that advanced users can hack the delicious"
 PRINT "morsels inside."
 PRINT "If a password is required, you will be prompted to enter it."
 PRINT ""
 PRINT "Windows users can drag-and-drop their RPG file onto this program"
 PRINT "to unlump it."
 PRINT ""
 PRINT "[Press a Key]"
 dummy$ = readkey$()
 fatalerror ""
END IF

lumped$ = COMMAND$(1)
dest$ = COMMAND$(2)

'check whether it is an RPG file (assume all RPG files contain BROWSE.TXT)
isrpg = islumpfile(lumped$, "browse.txt")

IF dest$ = "" THEN
 IF LEN(rightafter(lumped$, ".")) = LEN(lumped$) - 1 THEN fatalerror "please specify an output directory"
 IF isrpg THEN
  dest$ = trimextension$(lumped$) + ".rpgdir"
 ELSE
  dest$ = trimextension$(lumped$) + ".unlmp"
 END IF
END IF

IF NOT isfile(lumped$) THEN fatalerror "lump file `" + lumped$ + "' was not found"

PRINT "From " + lumped$ + " to " + dest$

'--Get old-style game (only matters for ancient RPG files that are missing the archinym.lmp)
dim game as string
game = trimextension(trimpath(lumped$))

IF isfile(dest$) THEN fatalerror "destination directory `" + dest$ + "' already exists as a file"

IF isdir(dest$) THEN
 PRINT "Destination directory `" + dest$ + "' already exists. Delete it? (y/n)"
 w$ = readkey
 IF w$ <> "Y" AND w$ <> "y" THEN SYSTEM
 killdir dest$
END IF
makedir dest$
createddir = -1

IF NOT isdir(dest$) THEN fatalerror "unable to create destination directory `" + dest$ + "'"

IF NOT isrpg THEN
 unlump lumped$, dest$ + SLASH
 CHDIR olddir$
 PRINT "Done."
 SYSTEM
END IF
 
unlumpfile lumped$, "archinym.lmp", dest$ + SLASH

'--set game according to the archinym
IF isfile(dest$ + SLASH + "archinym.lmp") THEN
 fh = FREEFILE
 OPEN dest$ + SLASH + "archinym.lmp" FOR INPUT AS #fh
 LINE INPUT #fh, a$
 CLOSE #fh
 IF LEN(a$) <= 8 THEN
  game = a$
 END IF
 KILL dest$ + SLASH + "archinym.lmp"
END IF

unlumpfile lumped$, game + ".gen", dest$ + SLASH
DIM gen(360)
xbload dest$ + SLASH + LCASE(game) + ".gen", gen(), "unable to open general data"

KILL dest$ + SLASH + game + ".gen"

passokay = -1

IF gen(genPW2Length) > -1 THEN
 passokay = 0
 '----load password-----
 'Note that this is still using the old 2nd-style password format, not the
 'newer simpler 3rd-style password format. This is okay for now, since
 'CUSTOM writes both 2nd and 3rd style passwords, but supporting 3rd-style
 'here also would be desireable
 readscatter rpas$, gen(genPW2Length), gen(), 200
 rpas$ = rotascii(rpas$, gen(genPW2Offset) * -1)
 'PRINT rpas$
 '-----get inputed password-----
 print "Password Required"
 pas$ = ""
 DO
  w$ = readkey$
  IF w$ = CHR$(13) THEN
   PRINT ""
   IF pas$ <> rpas$ THEN fatalerror "password mismatch"
   passokay = -1
   EXIT DO
  END IF
  LOCATE , 1: FOR i = 1 TO LEN(pas$): PRINT " "; : NEXT i
  pas$ = editstr(pas$, w$, cur, 17, 0)
  LOCATE , 1: FOR i = 1 TO LEN(pas$): PRINT "*"; : NEXT i
  sleep 80,1
 LOOP
END IF

IF passokay THEN
 unlump lumped$, dest$ + SLASH
END IF

CHDIR olddir$
PRINT "Done."
SYSTEM

REM $STATIC
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
   IF (ASC(key$) >= 32 AND ASC(key$) < 127 AND key$ <> "," AND key$ <> "~" AND number = 0) OR (ASC(key$) >= 48 AND ASC(key$) <= 57 AND number) THEN
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

 'RMDIR does not work unless isdir$ is called first. If I tried to figure out why, my brain would explode
 isdir$(dest$)
 IF createddir THEN killdir dest$
 SYSTEM
END SUB

SUB debug (e$)
 PRINT e$
END SUB

SUB debuginfo (e$)
 PRINT e$
END SUB

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
result$ = ""

FOR i = LEN(s$) TO 1 STEP -1
 IF MID$(s$, i, 1) = d$ THEN
  rightafter$ = result$
  EXIT FOR
 END IF
 result$ = MID$(s$, i, 1) + result$
NEXT i

END FUNCTION

'FUNCTION readpassword$
'
''--read a 17-byte string from GEN at word offset 7
''--(Note that array2str uses the byte offset not the word offset)
's$ = STRING$(17, 0)
'array2str general(), 14, s$
'
''--reverse ascii rotation / weak obfuscation
's$ = rotascii(s$, general(6) * -1)
'
''-- discard ascii chars lower than 32
'p$ = ""
'FOR i = 1 TO 17
' c$ = MID$(s$, i, 1)
' IF ASC(c$) >= 32 THEN p$ = p$ + c$
'NEXT i
'
'readpassword$ = p$
'
'END FUNCTION
