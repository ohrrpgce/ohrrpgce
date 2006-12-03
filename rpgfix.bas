'OHRRPGCE RPGFIX - RPG File format upgrade utility
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
DECLARE SUB writepassword (p$, gen%())
'$DYNAMIC
DEFINT A-Z
DECLARE SUB readscatter (s$, lhold%, array%(), start%)
DECLARE SUB xbload (file$, array%(), e$)
DECLARE SUB writescatter (s$, lhold%, array%(), start%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB savetanim (n%, tastuf%(), game$)
DECLARE FUNCTION f$ (n%)
DECLARE FUNCTION upgrade% (buffer%(), game$, nowv%)
'assembly subs and functions
DECLARE SUB setpicstuf (buf(), BYVAL b, BYVAL p)
DECLARE SUB loadset (fil$, BYVAL i, BYVAL l)
DECLARE SUB storeset (fil$, BYVAL i, BYVAL l)
'DECLARE SUB setdiskpages (buf(), BYVAL h, BYVAL l)
'DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
'DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
DECLARE SUB setmapdata (array(), pas(), BYVAL t, BYVAL b)
DECLARE SUB setmapblock (BYVAL x, BYVAL y, BYVAL v)
DECLARE FUNCTION readmapblock (BYVAL x, BYVAL y)
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
DECLARE SUB copyfile (s$, d$, buf())
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
DECLARE SUB lumpfiles (listf$, lump$, path$, buffer())
DECLARE SUB unlump (lump$, ulpath$, buffer())
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

#include "const.bi"

DIM buffer(16383), lumpbuf(16383)

nowversion = 5

game$ = LCASE$(COMMAND$)

IF game$ = "" OR RIGHT$(game$, 1) = "?" THEN
 PRINT "RPGFIX.EXE v." + LTRIM$(STR$(nowversion)) + "                     O.H.R.RPG.C.E. game updating utility"
 PRINT ""
 PRINT "this program allows you to update an obsolete RPG file to the latest"
 PRINT "file-format version. This copy of RPGFIX upgrades RPG files from versions"
 PRINT "0-" + LTRIM$(STR$(nowversion - 1)) + " to version " + LTRIM$(STR$(nowversion)) + ". If your copy of GAME.EXE requires a newer version,"
 PRINT "you must download the latest RPGFIX from http://HamsterRepublic.com"
 PRINT ""
 PRINT "To use, type RPGFIX MYGAME.RPG at the dos prompt, or in Windows,"
 PRINT "drag and drop your RPG file onto the RPGFIX.EXE program file."
 SYSTEM
END IF

progdir$ = STRING$(rpathlength, 0): getstring progdir$
IF RIGHT$(progdir$, 1) = "\" THEN progdir$ = LEFT$(progdir$, LEN(progdir$) - 1)
CHDIR progdir$

IF RIGHT$(game$, 4) = ".rpg" THEN game$ = LEFT$(game$, LEN(game$) - 4)

IF NOT isfile(game$ + ".rpg" + CHR$(0)) THEN PRINT game$ + ".rpg was not found": SYSTEM

IF isdir("rpgfix.tmp" + CHR$(0)) THEN
 KILL "rpgfix.tmp\*.*"
ELSE
 MKDIR "rpgfix.tmp"
END IF

PRINT "upgrading " + game$ + ".rpg"

PRINT "unlumping"
unlump game$ + ".rpg" + CHR$(0), "rpgfix.tmp\", lumpbuf()

temp$ = game$
rpg$ = ""
DO
 IF LEFT$(rpg$, 1) = "\" OR LEFT$(rpg$, 1) = ":" THEN rpg$ = RIGHT$(rpg$, LEN(rpg$) - 1): EXIT DO
 IF LEN(rpg$) >= LEN(game$) THEN EXIT DO
 rpg$ = RIGHT$(temp$, 1) + rpg$
 temp$ = LEFT$(temp$, LEN(temp$) - 1)
LOOP

IF upgrade(buffer(), "rpgfix.tmp\" + rpg$, nowversion) THEN
 PRINT "making a backup copy"
 copyfile game$ + ".rpg" + CHR$(0), game$ + ".old" + CHR$(0), buffer()
 PRINT "relumping"
 findfiles "rpgfix.tmp\" + "*.*" + CHR$(0), 0, "rpgfix.tmp\____lump.tmp" + CHR$(0), buffer()
 lumpfiles "rpgfix.tmp\____lump.tmp" + CHR$(0), game$ + ".rpg" + CHR$(0), "rpgfix.tmp\", lumpbuf()
END IF

KILL "rpgfix.tmp\*.*"
RMDIR "rpgfix.tmp"

PRINT "done"

REM $STATIC
FUNCTION f$ (n)
s$ = LTRIM$(STR$(n))
WHILE LEN(s$) < 2: s$ = "0" + s$: WEND
s$ = RIGHT$(s$, 2)
f$ = s$
END FUNCTION

FUNCTION loopvar (var, min, max, inc)
a = var + inc
IF a > max THEN a = a - ((max - min) + 1): loopvar = a: EXIT FUNCTION
IF a < min THEN a = a + ((max - min) + 1): loopvar = a: EXIT FUNCTION
loopvar = a
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

FUNCTION rotascii$ (s$, o)

temp$ = ""

FOR i = 1 TO LEN(s$)
 temp$ = temp$ + CHR$(loopvar(ASC(MID$(s$, i, 1)), 0, 255, o))
NEXT i

rotascii$ = temp$

END FUNCTION

SUB savetanim (n%, tastuf%(), game$)
setpicstuf tastuf(), 80, -1
storeset game$ + ".tap" + CHR$(0), n, 0
END SUB

FUNCTION upgrade (buffer(), game$, nowv)

DIM pal16(8)

upgrade = 0 'false

DIM general(500), font(1024)

IF NOT isfile("rpgfix.tmp\archinym.lmp" + CHR$(0)) THEN
 PRINT "inserting archinym"
 '--create archinym information lump
 fh = FREEFILE
 OPEN "rpgfix.tmp\archinym.lmp" FOR OUTPUT AS #fh
 PRINT #fh, RIGHT$(game$, LEN(game$) - LEN("rpgfix.tmp\"))
 PRINT #fh, "upgraded with RPGFIX.EXE 20000412"
 CLOSE #fh
 upgrade = -1
ELSE
 '--set game$ according to the archinym
 IF isfile("rpgfix.tmp\archinym.lmp" + CHR$(0)) THEN
  fh = FREEFILE
  OPEN "rpgfix.tmp\archinym.lmp" FOR INPUT AS #fh
  LINE INPUT #fh, a$
  CLOSE #fh
  IF LEN(a$) <= 8 THEN game$ = "rpgfix.tmp\" + a$
 END IF
END IF

xbload game$ + ".fnt", font(), "missing font file!"
xbload game$ + ".gen", general(), "missing general game data!"

PRINT "checking current version"
PRINT "RPG file version is" + STR$(general(95))

IF general(95) > nowv THEN
 PRINT "this copy of RPGFIX.EXE only supports up to version" + STR$(nowv)
 EXIT FUNCTION
END IF

IF general(95) = nowv THEN
 IF general(5) < 256 THEN
  PRINT "updating password from scattertable to slightly saner format"
 ELSE
  PRINT "there is no need to upgrade this RPG file"
  EXIT FUNCTION
 END IF
END IF

IF general(95) = 0 THEN
 general(95) = 1
 PRINT "Flushing New Text Data..."
 setpicstuf buffer(), 400, -1
 FOR o = 0 TO 999
  loadset game$ + ".say" + CHR$(0), o, 0
  temp$ = STRING$(68, 0)
  str2array temp$, buffer(), 331
  storeset game$ + ".say" + CHR$(0), o, 0
 NEXT o
END IF
IF general(95) = 1 THEN
 general(95) = 2
 PRINT "Updating Door Format..."
 FOR o = 0 TO 19
  xbload game$ + ".dor", buffer(), "map" + STR$(o) + " missing door data!"
  FOR i = 0 TO 299
   buffer(i) = buffer(o * 300 + i)
  NEXT i
  setpicstuf buffer(), 600, -1
  storeset game$ + ".dox" + CHR$(0), o, 0
 NEXT o
 PRINT "Enforcing default font"
 IF NOT isfile("ohrrpgce.fnt" + CHR$(0)) THEN PRINT "unable to force default font: cannot find default font OHRRPGCE.FNT"
 copyfile "ohrrpgce.fnt" + CHR$(0), game$ + ".fnt" + CHR$(0), buffer()
 xbload game$ + ".fnt", font(), "missing font file"
 PRINT "Making AniMaptiles Backward Compatable"
 FOR i = 0 TO 39
  buffer(i) = 0
 NEXT i
 FOR i = 0 TO 1
  o = i * 20
  buffer(0 + o) = 112
  buffer(1 + o) = 0
  '--wait 3--
  buffer(2 + o + 0) = 5
  buffer(11 + o + 0) = 3
  '--right 1--
  buffer(2 + o + 1) = 3
  buffer(11 + o + 1) = 1
  '--wait 3--
  buffer(2 + o + 2) = 5
  buffer(11 + o + 2) = 3
  '--left 1--
  buffer(2 + o + 3) = 4
  buffer(11 + o + 3) = 1
 NEXT i
 FOR i = 0 TO 14
  savetanim i, buffer(), game$
 NEXT i
 FOR i = 0 TO general(0)
  PRINT " map" + STR$(i)
  xbload game$ + ".t" + f$(i), buffer(), "map" + STR$(i) + " missing tilemap!"
  setmapdata buffer(), buffer(), 0, 0
  FOR tx = 0 TO buffer(0)
   FOR ty = 0 TO buffer(1)
    IF readmapblock(tx, ty) = 158 THEN setmapblock tx, ty, 206
   NEXT ty
  NEXT tx
  DEF SEG = VARSEG(buffer(0)): BSAVE game$ + ".t" + f$(i), VARPTR(buffer(0)), buffer(0) * buffer(1) + 4
 NEXT i
END IF
'---VERSION 3---
IF general(95) = 2 THEN
 general(95) = 3
 '-GET OLD PASSWORD
 rpas$ = ""
 FOR i = 1 TO general(99)
  IF general(4 + i) >= 0 AND general(4 + i) <= 255 THEN rpas$ = rpas$ + CHR$(loopvar(general(4 + i), 0, 255, general(98) * -1))
 NEXT i
 '-SET SCATTERTABLE BASE
 general(199) = INT(RND * 15) + 1
 '-WRITE PASSWORD INTO SCATTERTABLE
 general(93) = INT(RND * 250) + 1
 rpas$ = rotascii(rpas$, general(93))
 writescatter rpas$, general(94), general(), 200
 '-REPLACE OLD PASSWORD
 pas$ = rotascii("ufxx|twi%|fx%rt{ji", -5)
 general(99) = LEN(pas$)
 general(98) = INT(RND * 250) + 1
 FOR i = 1 TO general(99)
  temp = ASC(MID$(pas$, i, 1))
  general(4 + i) = loopvar(temp, 0, 255, general(98))
 NEXT i
 PRINT "Data Scaling Shtuff..."
 general(26) = 40
 general(27) = 149
 general(28) = 79
 general(29) = 29
 general(30) = 119
 general(31) = 149
 general(32) = 99
 general(33) = 14
 general(34) = 200
 general(35) = 59
 general(36) = 500
 general(37) = 1000
 general(38) = 99
 general(39) = 999
END IF
'--VERSION 4--
IF general(95) = 3 THEN
 general(95) = 4
 PRINT "Clearing New Attack Bitsets..."
 setpicstuf buffer(), 80, -1
 FOR o = 0 TO general(34)
  loadset game$ + ".dt6" + CHR$(0), o, 0
  buffer(18) = 0
  IF readbit(buffer(), 20, 60) THEN buffer(18) = 1
  setbit buffer(), 20, 2, 0
  FOR i = 21 TO 58
   setbit buffer(), 20, i, 0
  NEXT i
  FOR i = 60 TO 63
   setbit buffer(), 20, i, 0
  NEXT i
  storeset game$ + ".dt6" + CHR$(0), o, 0
 NEXT o
 setbit general(), 101, 6, 0 'no hide readymeter
 setbit general(), 101, 7, 0 'no hide health meter
END IF
'--VERSION 5--
IF general(95) = 4 THEN
 general(95) = 5
 PRINT "Upgrading 16-color Palette Format..."
 setpicstuf pal16(), 16, -1
 xbload game$ + ".pal", buffer(), "16-color palletes missing from " + game$
 KILL game$ + ".pal"
 '--find last used palette
 last = 99
 foundpal = 0
 FOR j = 99 TO 0 STEP -1
  FOR i = 0 TO 7
   IF buffer(j * 8 + i) <> 0 THEN
    last = j
    foundpal = 1
    EXIT FOR
   END IF
  NEXT i
  IF foundpal THEN EXIT FOR
 NEXT j
 PRINT "Last used palette is" + STR$(last)
 '--write header
 pal16(0) = 4444
 pal16(1) = last
 FOR i = 2 TO 7
  pal16(i) = 0
 NEXT i
 storeset game$ + ".pal" + CHR$(0), 0, 0
 '--convert palettes
 FOR j = 0 TO last
  FOR i = 0 TO 7
   pal16(i) = buffer(j * 8 + i)
  NEXT i
  storeset game$ + ".pal" + CHR$(0), 1 + j, 0
 NEXT j
END IF

'--update to new (3rd) password format
IF general(5) < 256 THEN
 general(5) = 256
 IF general(94) = -1 THEN
  '--no password, write a blank one
  pas$ = ""
 ELSE
  '--read the old scattertable
  readscatter pas$, general(94), general(), 200
  pas$ = rotascii(pas$, general(93) * -1)
 END IF
 writepassword pas$, general()
END IF

DEF SEG = VARSEG(font(0)): BSAVE game$ + ".fnt", VARPTR(font(0)), 2048
DEF SEG = VARSEG(general(0)): BSAVE game$ + ".gen", VARPTR(general(0)), 1000

upgrade = -1 'true

'wow! this is quite a big and ugly routine!
END FUNCTION

SUB writepassword (p$, gen())

'-- set password version number (only if needed)
IF gen(5) < 256 THEN gen(5) = 256

'--pad the password with some silly obfuscating low-ascii chars
FOR i = 1 TO 17 - LEN(p$)
 IF INT(RND * 10) < 5 THEN
  p$ = p$ + CHR$(INT(RND * 30))
 ELSE
  p$ = CHR$(INT(RND * 30)) + p$
 END IF
NEXT i

'--apply a new ascii rotation / weak obfuscation number
gen(6) = INT(RND * 253) + 1
p$ = rotascii(p$, gen(6))

'--write the password into GEN
str2array p$, gen(), 14

END SUB

SUB writescatter (s$, lhold, array(), start)
DIM stray(10)

s$ = LEFT$(s$, 20)
lhold = LEN(s$) * 8 - 1
str2array s$, stray(), 0

FOR i = 0 TO lhold
 trueb = readbit(stray(), 0, i)
 DO
  scatb = INT(RND * (16 + (i * 16)))
 LOOP UNTIL readbit(array(), start - 1, scatb) = trueb
 array(start + i) = scatb
NEXT i

FOR i = lhold + 1 TO 159
 array(start + i) = INT(RND * 4444)
NEXT i

END SUB

SUB xbload (file$, array(), e$)

IF isfile(file$ + CHR$(0)) THEN
 handle = FREEFILE
 OPEN file$ FOR BINARY AS #handle
 bytes = LOF(handle)
 CLOSE #handle
 IF bytes THEN
  OPEN file$ FOR BINARY AS #handle
  a$ = " "
  GET #handle, 1, a$
  CLOSE #handle
  IF a$ = CHR$(253) THEN
   DEF SEG = VARSEG(array(0)): BLOAD file$, VARPTR(array(0))
  ELSE
   PRINT e$ + "(unbloadable)"
   SYSTEM
  END IF
 ELSE
  PRINT e$ + "(zero byte)"
  SYSTEM
 END IF
ELSE
 PRINT e$
 SYSTEM
END IF

END SUB

