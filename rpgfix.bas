'$DYNAMIC
DEFINT A-Z
DECLARE SUB writescatter (s$, lhold%, array%(), start%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB savetanim (n%, tastuf%(), game$)
DECLARE FUNCTION f$ (n%)
DECLARE FUNCTION upgrade% (buffer%(), game$)
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

DIM buffer(16383), lumpbuf(32767)

game$ = LCASE$(COMMAND$)

IF game$ = "" OR RIGHT$(game$, 1) = "?" THEN
 PRINT "RPGFIX.EXE v.3                     O.H.R.RPG.C.E. game updating utility"
 PRINT ""
 PRINT "this program allows you to update an obsolete RPG file to the latest"
 PRINT "file-format version. This copy of RPGFIX upgrades RPG files from versions"
 PRINT "0,1,2 to version 3. If your copy of GAME.EXE requires a newer version,"
 PRINT "you must download the latest RPGFIX from http://HamsterRepublic.com"
 PRINT ""
 PRINT "To use, type RPGFIX MYGAME.RPG at the dos prompt, or in Windows,"
 PRINT "drag and drop your RPG file onto the RPGFIX.EXE program file."
 SYSTEM
END IF

progdir$ = STRING$(rpathlength, 0): getstring progdir$
IF RIGHT$(progdir$, 1) <> "\" THEN progdir$ = progdir$ + "\"
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

IF upgrade(buffer(), "rpgfix.tmp\" + rpg$) THEN
 PRINT "making a backup copy"
 copyfile game$ + ".rpg" + CHR$(0), game$ + ".old" + CHR$(0), buffer()
 PRINT "relumping"
 findfiles "rpgfix.tmp\" + rpg$ + ".*" + CHR$(0), 32, "rpgfix.tmp\____lump.tmp" + CHR$(0), buffer()
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

FUNCTION upgrade (buffer(), game$)

upgrade = 0 'false

'current RPG format version is 3
nowv = 3

DIM general(500), font(1024)

DEF SEG = VARSEG(font(0)): BLOAD game$ + ".fnt", VARPTR(font(0))
DEF SEG = VARSEG(general(0)): BLOAD game$ + ".gen", VARPTR(general(0))

PRINT "checking current version"

IF general(95) > nowv THEN
 PRINT "RPG file version is" + STR$(general(95))
 PRINT "this copy of RPGFIX.EXE only supports up to version" + STR$(nowv)
 EXIT FUNCTION
END IF

IF general(95) = nowv THEN
 PRINT "there is no need to upgrade this RPG file"
 EXIT FUNCTION
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
  DEF SEG = VARSEG(buffer(0)): BLOAD game$ + ".dor", VARPTR(buffer(0))
  FOR i = 0 TO 299
   buffer(i) = buffer(o * 300 + i)
  NEXT i
  setpicstuf buffer(), 600, -1
  storeset game$ + ".dox" + CHR$(0), o, 0
 NEXT o
 PRINT "Enforcing default font"
 IF NOT isfile("ohrrpgce.fnt" + CHR$(0)) THEN PRINT "unable to force default font: cannot find default font OHRRPGCE.FNT"
 copyfile "ohrrpgce.fnt" + CHR$(0), game$ + ".fnt" + CHR$(0), buffer()
 DEF SEG = VARSEG(font(0)): BLOAD game$ + ".fnt", VARPTR(font(0))
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
  DEF SEG = VARSEG(buffer(0)): BLOAD game$ + ".t" + f$(i), VARPTR(buffer(0))
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


DEF SEG = VARSEG(font(0)): BSAVE game$ + ".fnt", VARPTR(font(0)), 2048
DEF SEG = VARSEG(general(0)): BSAVE game$ + ".gen", VARPTR(general(0)), 1000

upgrade = -1 'true

'wow! this is quite a big and ugly routine!
END FUNCTION

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

