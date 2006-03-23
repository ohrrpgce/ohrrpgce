'OHRRPGCE CUSTOM - More misc unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB stredit (s$, maxl%)
DECLARE FUNCTION str2lng& (stri$)
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION readshopname$ (shopnum%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB writeconstant (filehandle%, num%, names$, unique$(), prefix$)
DECLARE SUB safekill (f$)
DECLARE SUB touchfile (f$)
DECLARE SUB standardmenu (menu$(), size%, vis%, pt%, top%, x%, y%, page%, edge%)
DECLARE FUNCTION readitemname$ (index%)
DECLARE FUNCTION readattackname$ (index%)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION getShortName$ (filename$)
DECLARE FUNCTION getLongName$ (filename$)
DECLARE SUB textfatalerror (e$)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION scriptname$ (num%, f$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE FUNCTION getmapname$ (m%)
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION isunique% (s$, u$(), r%)
DECLARE FUNCTION loadname$ (length%, offset%)
DECLARE SUB exportnames (gamedir$)
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE FUNCTION needaddset (pt%, check%, what$)
DECLARE FUNCTION browse$ (special, default$, fmask$, tmp$)
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION usemenu (pt%, top%, first%, last%, size%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%())
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION intstr$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB debug (s$)
DECLARE SUB editbitset (array%(), wof%, last%, names$())
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage ()
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (master%(), font%())
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE FUNCTION zintgrabber% (n%, min%, max%, less%, more%)
DECLARE FUNCTION xintgrabber% (n%, pmin%, pmax%, nmin%, nmax%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE FUNCTION maplumpname$ (map, oldext$)
DECLARE FUNCTION itemstr$ (it%, hiden%, offbyone%)
DECLARE FUNCTION validmusicfile(file$)
DECLARE FUNCTION getsongname$ (num%)

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'cglobals.bi'

'$INCLUDE: 'const.bi'

REM $STATIC
FUNCTION browse$ (special, default$, fmask$, tmp$)
browse$ = ""

'special=0   no preview
'special=1   just BAM
'special=2   16 color BMP
'special=3   background
'special=4   master palette
'special=5   any supported music (currently *.bam and *.mid)  (fmask$ is ignored)
mashead$ = CHR$(253) + CHR$(13) + CHR$(158) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(6)
paledithead$ = CHR$(253) + CHR$(217) + CHR$(158) + CHR$(0) + CHR$(0) + CHR$(7) + CHR$(6)

DIM drive(26), drive$(26), tree$(255), display$(255), treec(255), catfg(6), catbg(6), bmpd(40)

catfg(0) = 9: catbg(0) = 8    'drives
catfg(1) = 9: catbg(1) = 8    'directories
catfg(2) = 9: catbg(2) = 0    'subdirectories
catfg(3) = 7: catbg(3) = 0    'files
catfg(4) = 11: catbg(4) = 8   'root
catfg(5) = 10: catbg(5) = 8   'special
catfg(6) = 8: catbg(5) = 0    'disabled

drivetotal = drivelist(drive())
drive(26) = 15
'---FOR NOW, IGNORE FLOPPIES---
FOR i = 0 TO 25
 WHILE drive(i) = 1 OR drive(i) = 2
  FOR j = i TO 25
   drive(j) = drive(j + 1)
  NEXT j
  drivetotal = drivetotal - 1
 WEND
NEXT i

remember$ = STRING$(pathlength, 0): getstring remember$
IF RIGHT$(remember$, 1) <> SLASH THEN remember$ = remember$ + SLASH
IF default$ = "" THEN
 nowdir$ = remember$
ELSE
 nowdir$ = default$
END IF

GOSUB context

treeptr = 0
treetop = 0

setkeys
DO
 setwait timing(), 80
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF usemenu(treeptr, treetop, 0, treesize, 17) OR changed THEN
  alert$ = ""
  changed = 0
  SELECT CASE special
   CASE 1
    stopsong
    IF treec(treeptr) = 3 OR treec(treeptr) = 6 THEN
     IF validmusicfile(nowdir$ + tree$(treeptr)) THEN
      loadsong nowdir$ + tree$(treeptr) + CHR$(0)
     ELSE
      alert$ = tree$(treeptr) + " is not a valid BAM file"
     END IF
    END IF
   CASE 2, 3
    IF bmpinfo(nowdir$ + tree$(treeptr) + CHR$(0), bmpd()) THEN
     alert$ = intstr$(bmpd(1)) + "*" + intstr$(bmpd(2)) + " pixels, " + intstr$(bmpd(0)) + "-bit color"
    END IF
   CASE 4
    IF treec(treeptr) = 3 OR treec(treeptr) = 6 THEN
     masfh = FREEFILE
     OPEN nowdir$ + tree$(treeptr) FOR BINARY AS #masfh
     a$ = "       "
     GET #masfh, 1, a$
     CLOSE #masfh
     SELECT CASE a$
      CASE mashead$
       alert$ = "MAS format"
      CASE paledithead$
       alert$ = "MAS format (PalEdit)"
      CASE ELSE
       alert$ = "Not a valid MAS file"
     END SELECT
    END IF
   CASE 5
    stopsong
    IF treec(treeptr) = 3 OR treec(treeptr) = 6 THEN
     IF validmusicfile(nowdir$ + tree$(treeptr)) THEN
      '-- NOTE TO SIMON: when you get to this, wrap this in a compat file, QB: as it is, FB: without the IF
      IF canplay(tree$(treeptr)) THEN
       loadsong nowdir$ + tree$(treeptr) + CHR$(0)
      END IF
     ELSE
      alert$ = tree$(treeptr) + " is not a valid music file"
     END IF
    END IF
  END SELECT
 END IF
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  alert$ = ""
  changed = 1
  IF special = 1 OR special = 5 THEN stopsong
  SELECT CASE treec(treeptr)
   CASE 0, 1
    nowdir$ = LEFT$(tree$(0), 3)
    FOR i = 1 TO treeptr
     nowdir$ = nowdir$ + tree$(i)
    NEXT i
    GOSUB context
   CASE 2
    nowdir$ = nowdir$ + tree$(treeptr) + SLASH
    GOSUB context
   CASE 3
    browse$ = nowdir$ + tree$(treeptr)
    EXIT DO
   CASE 4
    nowdir$ = ""
    GOSUB context
    FOR i = 0 TO drivetotal - 1
     IF drive(i) = 3 THEN treeptr = i
    NEXT i
  END SELECT
 END IF
 rectangle 4, 4, 312, 14, 1, dpage
 edgeprint nowdir$, 8, 6, 15, dpage
 rectangle 4, 184, 312, 14, 1, dpage
 edgeprint alert$, 8, 186, 15, dpage
 textcolor 15, 0
 printstr ">", 0, 20 + (treeptr - treetop) * 9, dpage
 FOR i = treetop TO small(treetop + 17, treesize)
  textcolor catfg(treec(i)), catbg(treec(i))
  a$ = display$(i)
  DO WHILE LEN(a$) < 38 AND catbg(treec(i)) > 0
   a$ = a$ + " "
  LOOP
  printstr a$, 10, 20 + (i - treetop) * 9, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
default$ = nowdir$
EXIT FUNCTION

context:
treesize = 0
IF nowdir$ = "" THEN
ELSE
 a = ASC(LEFT$(nowdir$, 1)) - 64
 a$ = nowdir$
 IF LINUX THEN
  treesize = -1
 ELSE
  FOR i = 0 TO drivetotal - 1
   'IF a = drive(i) THEN tree$(treesize) = drive$(i)
   IF a = drive(i) THEN tree$(treesize) = CHR$(64 + drive(i)) + ":" + SLASH
  NEXT i
  treec(treesize) = 0
  a$ = RIGHT$(a$, LEN(a$) - 3)
 END IF 
 b$ = ""
 DO UNTIL a$ = "" OR treesize >= 255
  b$ = b$ + LEFT$(a$, 1)
  a$ = RIGHT$(a$, LEN(a$) - 1)
  IF RIGHT$(b$, 1) = SLASH THEN
   treesize = treesize + 1
   tree$(treesize) = b$
   treec(treesize) = 1
   b$ = ""
  END IF
 LOOP
 '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
 findfiles nowdir$ + ALLFILES + CHR$(0), 16, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
 fh = FREEFILE
 OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh) OR treesize >= 255
  treesize = treesize + 1
  treec(treesize) = 2
  LINE INPUT #fh, tree$(treesize)
  IF tree$(treesize) = "." OR tree$(treesize) = ".." OR RIGHT$(tree$(treesize), 4) = ".tmp" THEN treesize = treesize - 1
 LOOP
 CLOSE #fh
 safekill tmp$ + "hrbrowse.tmp"
 '---FIND ALL FILES IN FILEMASK---
 IF special = 5 THEN
  '--disregard fmask$. one call per extension
  findfiles nowdir$ + "*.bam" + CHR$(0), 0, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
  GOSUB addmatchs
  findfiles nowdir$ + "*.mid" + CHR$(0), 0, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
  GOSUB addmatchs
 ELSE
  findfiles nowdir$ + fmask$ + CHR$(0), 0, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
  GOSUB addmatchs
 END IF
END IF

'--get longnames for display
FOR i = 0 TO treesize
 SELECT CASE treec(i)
  CASE 2, 3, 6
   display$(i) = getLongName$(nowdir$ + tree$(i))
  CASE ELSE
   display$(i) = tree$(i)
 END SELECT
NEXT i

'--alphabetize
FOR o = treesize TO 2 STEP -1
 FOR i = 1 TO o
  IF (treec(i) = 2 OR treec(i) = 3 OR treec(i) = 6) AND (treec(i - 1) = 2 OR treec(i - 1) = 3 OR treec(i - 1) = 6) THEN
   IF ASC(LCASE$(LEFT$(display$(i), 1))) < ASC(LCASE$(LEFT$(display$(i - 1), 1))) THEN
    SWAP display$(i), display$(i - 1)
    SWAP tree$(i), tree$(i - 1)
    SWAP treec(i), treec(i - 1)
   END IF
  END IF
 NEXT i
NEXT o

'--sort by type
FOR o = treesize TO 2 STEP -1
 FOR i = 1 TO o
  IF (treec(i) = 2 OR treec(i) = 3 OR treec(i) = 6) AND (treec(i - 1) = 2 OR treec(i - 1) = 3 OR treec(i - 1) = 6) THEN
   IF treec(i) < treec(i - 1) THEN
    SWAP display$(i), display$(i - 1)
    SWAP tree$(i), tree$(i - 1)
    SWAP treec(i), treec(i - 1)
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

addmatchs:
fh = FREEFILE
OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #fh
DO UNTIL EOF(fh) OR treesize >= 255
 treesize = treesize + 1
 treec(treesize) = 3
 LINE INPUT #fh, tree$(treesize)
 '---music files
 IF special = 1 OR special = 5 THEN
  IF validmusicfile(nowdir$ + tree$(treesize)) = 0 THEN
   treec(treesize) = 6
  END IF
 END IF
 '---4-bit BMP browsing
 IF special = 2 THEN
  IF bmpinfo(nowdir$ + tree$(treesize) + CHR$(0), bmpd()) THEN
   IF bmpd(0) <> 4 OR bmpd(1) > 320 OR bmpd(2) > 200 THEN
    treec(treesize) = 6
   END IF
  ELSE
   treesize = treesize - 1
  END IF
 END IF
 '---320x200x24/8bit BMP files
 IF special = 3 THEN
  IF bmpinfo(nowdir$ + tree$(treesize) + CHR$(0), bmpd()) THEN
   IF ISDOS = 1 THEN
    IF bmpd(0) <> 24 OR bmpd(1) <> 320 OR bmpd(2) <> 200 then
     treec(treesize) = 6
    END IF
   ELSE
    IF (bmpd(0) <> 24 AND bmpd(0) <> 8) OR bmpd(1) <> 320 OR bmpd(2) <> 200 THEN
    treec(treesize) = 6
    END IF
   END IF
  ELSE
   treesize = treesize - 1
  END IF
 END IF
 '--master palettes  (why isn't this up there?)
 IF special = 4 THEN
  masfh = FREEFILE
  OPEN nowdir$ + tree$(treesize) FOR BINARY AS #masfh
  a$ = "       "
  GET #masfh, 1, a$
  CLOSE #masfh
  IF a$ <> mashead$ AND a$ <> paledithead$ THEN
   treec(treesize) = 6
  END IF
 END IF
LOOP
CLOSE #fh
safekill tmp$ + "hrbrowse.tmp"

RETURN

END FUNCTION

SUB cropafter (index, limit, flushafter, lump$, bytes, prompt)

'if bytes is negative, then pages are used. flushafter becomes the working page number

'flushafter -1 = no flush
'flushafter 0 = record flush

DIM menu$(1)

IF prompt THEN
 menu$(0) = "No do not delete anything"
 menu$(1) = "Yes, delete all records after this one"
 IF sublist(1, menu$()) < 1 THEN
  setkeys
  EXIT SUB
 ELSE
  setkeys
 END IF
END IF

IF bytes >= 0 THEN
 setpicstuf buffer(), bytes, -1
 FOR i = 0 TO index
  loadset lump$ + CHR$(0), i, 0
  storeset workingdir$ + SLASH + "_cropped.tmp" + CHR$(0), i, 0
 NEXT i
 IF flushafter THEN
  'FOR i = 0 TO INT(bytes / 2) + 1
  '  buffer(i) = 0
  'NEXT i
  flusharray buffer(), INT(bytes / 2) + 1, 0
  FOR i = index + 1 TO limit
   storeset workingdir$ + SLASH + "_cropped.tmp" + CHR$(0), i, 0
  NEXT i
 ELSE
  limit = index
 END IF
 
ELSE '--use pages instead of sets
 setdiskpages buffer(), 200, 0
 FOR i = 0 TO index
  loadpage lump$ + CHR$(0), i, flushafter
  storepage workingdir$ + SLASH + "_cropped.tmp" + CHR$(0), i, flushafter
 NEXT i
 limit = index
 
END IF'--separate setpicstuf and setdiskpages

copyfile workingdir$ + SLASH + "_cropped.tmp" + CHR$(0), lump$ + CHR$(0), buffer()
safekill workingdir$ + SLASH + "_cropped.tmp"

END SUB

SUB cycletile (cycle(), tastuf(), pt(), skip())

FOR i = 0 TO 1
 skip(i) = large(skip(i) - 1, 0)
 IF skip(i) = 0 THEN
  notstuck = 10
  DO
   SELECT CASE tastuf(2 + 20 * i + pt(i))
    CASE 0
     pt(i) = 0
     cycle(i) = 0
    CASE 1
     cycle(i) = cycle(i) - tastuf(11 + 20 * i + pt(i)) * 16
     pt(i) = loopvar(pt(i), 0, 8, 1)
    CASE 2
     cycle(i) = cycle(i) + tastuf(11 + 20 * i + pt(i)) * 16
     pt(i) = loopvar(pt(i), 0, 8, 1)
    CASE 3
     cycle(i) = cycle(i) + tastuf(11 + 20 * i + pt(i))
     pt(i) = loopvar(pt(i), 0, 8, 1)
    CASE 4
     cycle(i) = cycle(i) - tastuf(11 + 20 * i + pt(i))
     pt(i) = loopvar(pt(i), 0, 8, 1)
    CASE 5
     skip(i) = tastuf(11 + 20 * i + pt(i))
     pt(i) = loopvar(pt(i), 0, 8, 1)
    CASE ELSE
     pt(i) = loopvar(pt(i), 0, 8, 1)
   END SELECT
   notstuck = large(notstuck - 1, 0)
  LOOP WHILE notstuck AND skip(i) = 0
 END IF
NEXT i

END SUB

SUB exportnames (gamedir$)

DIM u$(1024), names$(32), stat$(11)
max = 32

getnames names$(), max
stat$(0) = names$(0)
stat$(1) = names$(1)
stat$(2) = names$(2)
stat$(3) = names$(3)
stat$(4) = names$(5)
stat$(5) = names$(6)
stat$(6) = names$(29)
stat$(7) = names$(30)
stat$(8) = names$(8)
stat$(9) = names$(7)
stat$(10) = names$(31)
stat$(11) = names$(4)

outf$ = gamedir$ + SLASH + RIGHT$(game$, LEN(game$) - 12) + ".hsi"

clearpage 0
clearpage 1
setvispage 0
textcolor 15, 0
pl = 0
printstr "exporting HamsterSpeak Definitions to:", 0, pl * 8, 0: pl = pl + 1
printstr outf$, 0, pl * 8, 0: pl = pl + 1
'Need to call this quite a lot to refresh the screen for FB. Bit of a
'compromise between showing the process and slowing things down, since
'it will copy the page data every time.
setvispage 0

fh = FREEFILE
OPEN outf$ FOR OUTPUT AS #fh
PRINT #fh, "# HamsterSpeak constant definitions for " + RIGHT$(game$, LEN(game$) - 12)
PRINT #fh, ""
PRINT #fh, "define constant, begin"

printstr "tag names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
FOR i = 2 TO 999
 writeconstant fh, i, lmnemonic(i), u$(), "tag"
NEXT i

printstr "song names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
FOR i = 0 TO general(genMaxSong)
 writeconstant fh, i, getsongname$(i), u$(), "song"
NEXT i
setvispage 0

printstr "hero names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
FOR i = 0 TO general(35)
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), i, 0
 writeconstant fh, i, loadname(0, 1), u$(), "hero"
NEXT i

printstr "item names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
FOR i = 0 TO 255
 writeconstant fh, i, readitemname$(i), u$(), "item"
NEXT i
setvispage 0

printstr "stat names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
FOR i = 0 TO 11
 writeconstant fh, i, stat$(i), u$(), "stat"
NEXT i

printstr "slot names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
writeconstant fh, 1, "Weapon", u$(), "slot"
FOR i = 0 TO 3
 writeconstant fh, i + 2, names$(25 + i), u$(), "slot"
NEXT i
setvispage 0

printstr "map names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
FOR i = 0 TO general(0)
 writeconstant fh, i, getmapname$(i), u$(), "map"
NEXT i

printstr "attack names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
FOR i = 0 TO general(34)
 writeconstant fh, i + 1, readattackname$(i), u$(), "atk"
NEXT i
setvispage 0

printstr "shop names", 0, pl * 8, 0: pl = pl + 1
a = isunique("", u$(), 1)
FOR i = 0 TO general(97)
 writeconstant fh, i, readshopname$(i), u$(), "shop"
NEXT i

PRINT #fh, "end"
CLOSE #fh

printstr "done", 0, pl * 8, 0: pl = pl + 1
setvispage 0
w = getkey

END SUB

SUB fatalerror (e$)

debug "fatal error:" + e$
textcolor 15, 0
FOR i = 0 TO 1
 clearpage i
 printstr e$, 0, 0, i
 printstr "an error has occured. Press ESC to", 0, 16, i
 printstr "close " + CUSTOMEXE + " or press any other", 0, 24, i
 printstr "key to try to continue. If the", 0, 32, i
 printstr "error keeps happening, send e-mail to", 0, 40, i
 printstr "ohrrpgce-crash@HamsterRepublic.com", 0, 48, i
NEXT i
setvispage vpage 'refresh

w = getkey

IF w = 1 THEN
 restoremode
 
 touchfile workingdir$ + SLASH + "__danger.tmp"
 
 PRINT "fatal error:"
 PRINT e$
 
 'borrowed this code from game.bas cos wildcard didn't work in FB
 findfiles workingdir$ + SLASH + ALLFILES + chr$(0), 0, "filelist.tmp" + CHR$(0), buffer()
 fh = FREEFILE
 OPEN "filelist.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  KILL workingdir$ + SLASH + filename$
 LOOP
 CLOSE #fh
 KILL "filelist.tmp"
 RMDIR workingdir$
 
 SYSTEM
END IF

END SUB

FUNCTION getmapname$ (m)
setpicstuf buffer(), 80, -1
loadset game$ + ".mn" + CHR$(0), m, 0
a$ = STRING$(small((buffer(0) AND 255), 39), " ")
array2str buffer(), 1, a$
getmapname$ = a$
END FUNCTION

SUB getnames (stat$(), max)

fh = FREEFILE

OPEN game$ + ".stt" FOR BINARY AS #fh

FOR i = 0 TO max
 temp$ = CHR$(0)
 GET #fh, 1 + (11 * i), temp$
 temp = 0: IF temp$ <> "" THEN temp = ASC(temp$)
 stat$(i) = ""
 FOR o = 1 TO small(temp, 20)
  temp$ = " "
  GET #fh, 1 + (11 * i) + o, temp$
  stat$(i) = stat$(i) + temp$
 NEXT o
NEXT i

CLOSE #fh

END SUB

SUB getpal16 (array(), aoffset, foffset)


setpicstuf buffer(), 16, -1
loadset game$ + ".pal" + CHR$(0), 0, 0

IF buffer(0) <> 4444 THEN
 fatalerror "16-color palette file may be corrupt."
 buffer(0) = 4444
END IF

IF buffer(1) >= foffset AND foffset >= 0 THEN '--check in-range
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

END SUB

FUNCTION getShortName$ (filename$)
'--given a long filename, returns its short name.
'  it will always return the filename only, without the path
'  even though it can accept a fully qualified filename as input

result$ = ""
'length = ShortNameLength(filename$ + CHR$(0))
length = -1
IF length = -1 THEN
 '--failed, return input (minus path)
 FOR i = LEN(filename$) TO 1 STEP -1
  IF MID$(filename$, i, 1) = SLASH OR MID$(filename$, i, 1) = ":" THEN EXIT FOR
  result$ = MID$(filename$, i, 1) + result$
 NEXT i
ELSE
 a$ = STRING$(length, 0)
 getstring a$
 FOR i = LEN(a$) TO 1 STEP -1
  IF MID$(a$, i, 1) = SLASH OR MID$(a$, i, 1) = ":" THEN EXIT FOR
  result$ = MID$(a$, i, 1) + result$
 NEXT i
END IF
getShortName$ = result$
END FUNCTION

FUNCTION heroname$ (num, cond(), a())
h$ = ""
IF cond(num) THEN
 setpicstuf a(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), ABS(cond(num)) - 1, 0
 FOR i = 1 TO 0 + a(0)
  h$ = h$ + CHR$(a(i))
 NEXT i
END IF
heroname$ = h$
END FUNCTION

FUNCTION isunique (s$, u$(), r)
STATIC uptr

IF r THEN '--reset
 FOR i = 0 TO uptr
  u$(i) = s$
 NEXT i
 uptr = -1
 EXIT FUNCTION
END IF

IF s$ = "" THEN isunique = -1: EXIT FUNCTION

FOR i = 0 TO uptr
 IF LCASE$(s$) = u$(i) THEN isunique = 0: EXIT FUNCTION
NEXT i

uptr = small(uptr + 1, 1024)'--gives up trying after 1024 records
u$(uptr) = LCASE$(s$)
isunique = -1

END FUNCTION

FUNCTION lmnemonic$ (index)
DIM buf(20)
setpicstuf buf(), 42, -1

IF index = 0 THEN lmenmonic$ = "NULL": EXIT FUNCTION
IF index = 1 THEN lmenmonic$ = "CONSTANT": EXIT FUNCTION

loadset game$ + ".tmn" + CHR$(0), index, 0

temp$ = ""
FOR i = 1 TO small(buf(0), 20)
 temp$ = temp$ + CHR$(bound(buf(i), 0, 255))
NEXT i

lmnemonic$ = temp$

END FUNCTION

FUNCTION loadname$ (length, offset)
a$ = ""
FOR i = 0 TO buffer(length) - 1
 j = buffer(offset + i)
 IF j > 255 OR j < 0 THEN j = 0
 a$ = a$ + CHR$(j)
NEXT i
loadname$ = a$
END FUNCTION

FUNCTION onoroff$ (n)
IF SGN(n) + 1 THEN
 onoroff$ = "ON"
ELSE
 onoroff$ = "OFF"
END IF
END FUNCTION

SUB scriptman (gamedir$)
STATIC defaultdir$
DIM menu$(5)

menumax = 2
menu$(0) = "Previous Menu"
menu$(1) = "export names for scripts (.hsi)"
menu$(2) = "import compiled plotscripts (.hs)"

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(pt, 0, 0, menumax, 24)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  SELECT CASE pt
   CASE 0
    EXIT DO
   CASE 1
    exportnames gamedir$
   CASE 2
    f$ = browse(0, defaultdir$, "*.hs", "")
    IF f$ <> "" THEN
     setpicstuf buffer(), 7, -1
     loadset f$ + CHR$(0), 0, 0
     clearpage vpage
     IF buffer(0) = 21320 AND buffer(1) = 0 THEN
      copyfile f$ + CHR$(0), game$ + ".hsp" + CHR$(0), buffer()
      GOSUB maknamlst
      edgeprint "imported" + STR$(viscount) + " scripts", 0, 180, 15, vpage
     ELSE
      texty = 0
      printstr f$, 0, texty * 8, vpage: texty = texty + 1
      printstr "is not really a compiled .hs file.", 0, texty * 8, vpage: texty = texty + 1
      printstr "Did you create it by compiling a", 0, texty * 8, vpage: texty = texty + 1
      printstr "script file with hspeak.exe, or did", 0, texty * 8, vpage: texty = texty + 1
      printstr "you just give your script a name that", 0, texty * 8, vpage: texty = texty + 1
      printstr "ends in .hs and hoped it would work?", 0, texty * 8, vpage: texty = texty + 1
      printstr "Use hspeak.exe to create real .hs files", 0, texty * 8, vpage: texty = texty + 1
     END IF
     setvispage vpage 'force refresh for FB
     w = getkey
    END IF
  END SELECT
 END IF
 
 standardmenu menu$(), menumax, 22, pt, 0, 0, 0, dpage, 0
 
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
EXIT SUB

maknamlst:
textcolor 7, 0
textx = 0: texty = 0
dummy = unlumpone(game$ + ".hsp", "scripts.txt", workingdir$ + SLASH + "scripts.txt")
fptr = FREEFILE
OPEN workingdir$ + SLASH + "scripts.txt" FOR INPUT AS #fptr
setpicstuf buffer(), 40, -1
general(40) = 0
general(43) = 0
viscount = 0
DO
 IF EOF(fptr) THEN EXIT DO
 LINE INPUT #fptr, names$
 LINE INPUT #fptr, num$
 LINE INPUT #fptr, argc$
 FOR i = 1 TO str2int(argc$)
  LINE INPUT #fptr, dummy$
 NEXT i
 names$ = LEFT$(names$, 36)
 buffer(0) = str2int(num$)
 buffer(1) = LEN(names$)
 str2array names$, buffer(), 4
 storeset workingdir$ + SLASH + "plotscr.lst" + CHR$(0), general(40), 0
 general(40) = general(40) + 1
 IF buffer(0) > general(43) AND buffer(0) < 16384 THEN general(43) = buffer(0)
 IF textx + LEN(names$) + 1 >= 40 THEN
  textx = 0
  texty = texty + 1
  IF texty > 23 THEN
   setvispage vpage 'force refresh
   clearpage vpage
   texty = 0
  END IF
 END IF
 IF buffer(0) < 16384 THEN
  viscount = viscount + 1
  printstr names$ + ",", textx * 8, texty * 8, vpage
  textx = textx + LEN(names$) + 2
 END IF
LOOP
CLOSE #fptr
safekill workingdir$ + SLASH + "scripts.txt"
RETURN

END SUB

FUNCTION scriptname$ (num, f$)
a$ = LTRIM$(STR$(num))
IF num THEN
 setpicstuf buffer(), 40, -1
 FOR i = 0 TO general(40) - 1
  loadset workingdir$ + SLASH + f$ + CHR$(0), i, 0
  IF buffer(0) = num THEN
   a$ = STRING$(small(large(buffer(1), 0), 38), " ")
   array2str buffer(), 4, a$
   EXIT FOR
  END IF
 NEXT i
ELSE
 a$ = "[none]"
END IF
scriptname$ = a$
END FUNCTION

SUB standardmenu (menu$(), size, vis, pt, top, x, y, page, edge)
STATIC tog

tog = tog XOR 1

FOR i = top TO top + vis
 IF i <= size THEN
  IF edge THEN
   col = 7
   IF pt = i THEN col = 14 + tog
   edgeprint menu$(i), x + 0, y + (i - top) * 8, col, page
  ELSE
   textcolor 7, 0
   IF pt = i THEN textcolor 14 + tog, 0
   printstr menu$(i), x + 0, y + (i - top) * 8, page
  END IF
 END IF
NEXT i

END SUB

SUB statname
DIM stat$(116), names$(116), maxlen(116)
max = 116
clearpage 0
clearpage 1

'--load current names
getnames stat$(), 32 '--undefaulted

FOR i = 0 TO max
 SELECT CASE i
  CASE 55, 74 TO 76, 78, 80 TO 86, 88 TO 92, 97 TO 98, 106 TO 115
   maxlen(i) = 20
  CASE 39, 40
   maxlen(i) = 8
  CASE 94 TO 96, 105
   maxlen(i) = 30
  CASE ELSE
   maxlen(i) = 10
 END SELECT
NEXT i

names$(0) = "Health Points"
names$(1) = "Spell Points"
names$(2) = "Attack Power"
names$(3) = "Accuracy"
names$(4) = "Extra Hits"
names$(5) = "Blocking Power"
names$(6) = "Dodge Rate"
names$(7) = "Counter Rate"
names$(8) = "Speed"
FOR i = 1 TO 8
 names$(8 + i) = "Enemy Type" + STR$(i)
 names$(16 + i) = "Elemental" + STR$(i)
NEXT i
FOR i = 1 TO 4
 names$(24 + i) = "Armor" + STR$(i)
NEXT i
names$(29) = "Spell Skill"
names$(30) = "Spell Block"
names$(31) = "Spell cost %"
names$(32) = "Money"
names$(33) = "Experience":              stat$(33) = readglobalstring$(33, "Experience", 10)
names$(34) = "Battle Item Menu":        stat$(34) = readglobalstring$(34, "Item", 10)
names$(35) = "Exit Item Menu":          stat$(35) = readglobalstring$(35, "DONE", 10)
names$(36) = "Sort Item Menu":          stat$(36) = readglobalstring$(36, "AUTOSORT", 10)
names$(37) = "Drop Item":               stat$(37) = readglobalstring$(37, "TRASH", 10)
names$(38) = "Weapon":                  stat$(38) = readglobalstring$(38, "Weapon", 10)
names$(39) = "Unequip All":             stat$(39) = readglobalstring$(39, "-REMOVE-", 10)
names$(40) = "Exit Equip":              stat$(40) = readglobalstring$(40, "-EXIT-", 10)
names$(41) = "Drop Prompt":             stat$(41) = readglobalstring$(41, "Discard", 10)
names$(42) = "Negative Drop Prefix":    stat$(42) = readglobalstring$(42, "Cannot", 10)
names$(43) = "Level":                   stat$(43) = readglobalstring$(43, "Level", 10)
names$(44) = "Overwrite Save Yes":      stat$(44) = readglobalstring$(44, "Yes", 10)
names$(45) = "Overwrite Save No":       stat$(45) = readglobalstring$(45, "No", 10)
names$(46) = "Exit Spell List Menu":    stat$(46) = readglobalstring$(46, "EXIT", 10)
names$(47) = "(exp) for next (level)":  stat$(47) = readglobalstring$(47, "for next", 10)
names$(48) = "Remove Hero from Team":   stat$(48) = readglobalstring$(48, "REMOVE", 10)
names$(49) = "Pay at Inn":              stat$(49) = readglobalstring$(49, "Pay", 10)
names$(50) = "Cancel Inn":              stat$(50) = readglobalstring$(50, "Cancel", 10)
names$(51) = "Cancel Spell Menu":       stat$(51) = readglobalstring$(51, "(CANCEL)", 10)
names$(52) = "New Game":                stat$(52) = readglobalstring$(52, "NEW GAME", 10)
names$(53) = "Exit Game":               stat$(53) = readglobalstring$(53, "EXIT", 10)
names$(54) = "Pause":                   stat$(54) = readglobalstring$(54, "PAUSE", 10)
names$(55) = "Quit Playing Prompt":     stat$(55) = readglobalstring$(55, "Quit Playing?", 20)
names$(56) = "Quit Playing Yes":        stat$(56) = readglobalstring$(57, "Yes", 10)
names$(57) = "Quit Playing No":         stat$(57) = readglobalstring$(58, "No", 10)
names$(58) = "Cancel Save":             stat$(58) = readglobalstring$(59, "CANCEL", 10)
names$(59) = "Menu: Items":             stat$(59) = readglobalstring$(60, "Items", 10)
names$(60) = "Menu: Spells":            stat$(60) = readglobalstring$(61, "Spells", 10)
names$(61) = "Menu: Status":            stat$(61) = readglobalstring$(62, "Status", 10)
names$(62) = "Menu: Equip":             stat$(62) = readglobalstring$(63, "Equip", 10)
names$(63) = "Menu: Order":             stat$(63) = readglobalstring$(64, "Order", 10)
names$(64) = "Menu: Team":              stat$(64) = readglobalstring$(65, "Team", 10)
names$(65) = "Menu: Save":              stat$(65) = readglobalstring$(66, "Save", 10)
names$(66) = "Menu: Quit":              stat$(66) = readglobalstring$(67, "Quit", 10)
names$(67) = "Menu: Minimap":           stat$(67) = readglobalstring$(68, "Map", 10)
names$(68) = "Volume Control":          stat$(68) = readglobalstring$(69, "Volume", 10)
names$(69) = "Shop Menu: Buy":          stat$(69) = readglobalstring$(70, "Buy", 10)
names$(70) = "Shop Menu: Sell":         stat$(70) = readglobalstring$(71, "Sell", 10)
names$(71) = "Shop Menu: Inn":          stat$(71) = readglobalstring$(72, "Inn", 10)
names$(72) = "Shop Menu: Hire":         stat$(72) = readglobalstring$(73, "Hire", 10)
names$(73) = "Shop Menu: Exit":         stat$(73) = readglobalstring$(74, "Exit", 10)
names$(74) = "Unsellable item warning": stat$(74) = readglobalstring$(75, "CANNOT SELL", 20)
names$(75) = "Sell value prefix":       stat$(75) = readglobalstring$(77, "Worth", 20)
names$(76) = "Sell trade prefix":       stat$(76) = readglobalstring$(79, "Trade for", 20)
names$(77) = "($) and a (item)":        stat$(77) = readglobalstring$(81, "and a", 10)
names$(78) = "Worthless item warning":  stat$(78) = readglobalstring$(82, "Worth Nothing", 20)
names$(79) = "Sell alert":              stat$(79) = readglobalstring$(84, "Sold", 10)
names$(80) = "Buy trade prefix":        stat$(80) = readglobalstring$(85, "Trade for", 20)
names$(81) = "Hire price prefix":       stat$(81) = readglobalstring$(87, "Joins for", 20)
names$(82) = "Cannot buy prefix":       stat$(82) = readglobalstring$(89, "Cannot Afford", 20)
names$(83) = "Cannot hire prefix":      stat$(83) = readglobalstring$(91, "Cannot Hire", 20)
names$(84) = "Buy alert":               stat$(84) = readglobalstring$(93, "Purchased", 20)
names$(85) = "Hire alert (suffix)":     stat$(85) = readglobalstring$(95, "Joined!", 20)
names$(86) = "(#) in stock":            stat$(86) = readglobalstring$(97, "in stock", 20)
names$(87) = "Equipability prefix":     stat$(87) = readglobalstring$(99, "Equip:", 10)
names$(88) = "Party full warning":      stat$(88) = readglobalstring$(100, "No Room In Party", 20)
names$(89) = "Replace Save Prompt":     stat$(89) = readglobalstring$(102, "Replace Old Data?", 20)
names$(90) = "Status Prompt":           stat$(90) = readglobalstring$(104, "Who's Status?", 20)
names$(91) = "Spells Prompt":           stat$(91) = readglobalstring$(106, "Who's Spells?", 20)
names$(92) = "Equip Prompt":            stat$(92) = readglobalstring$(108, "Equip Who?", 20)
names$(93) = "Equip Nothing (unequip)": stat$(93) = readglobalstring$(110, "Nothing", 10)
names$(94) = "Nothing to Steal":        stat$(94) = readglobalstring$(111, "Has Nothing", 30)
names$(95) = "Steal Failure":           stat$(95) = readglobalstring$(114, "Cannot Steal", 30)
names$(96) = "Stole (itemname)":        stat$(96) = readglobalstring$(117, "Stole", 30)
names$(97) = "When an Attack Misses":   stat$(97) = readglobalstring$(120, "miss", 20)
names$(98) = "When a Spell Fails":      stat$(98) = readglobalstring$(122, "fail", 20)
names$(99) = "(hero) learned (spell)":  stat$(99) = readglobalstring$(124, "learned", 10)
names$(100) = "Found (gold)":           stat$(100) = readglobalstring$(125, "Found", 10)
names$(101) = "Gained (experience)":    stat$(101) = readglobalstring$(126, "Gained", 10)
names$(102) = "Weak to (elemental)":    stat$(102) = readglobalstring$(127, "Weak to", 10)
names$(103) = "Strong to (elemental)":  stat$(103) = readglobalstring$(128, "Strong to", 10)
names$(104) = "Absorbs (elemental)":    stat$(104) = readglobalstring$(129, "Absorbs", 10)
names$(105) = "No Elemental Effects":   stat$(105) = readglobalstring$(130, "No Elemental Effects", 30)
names$(106) = "(hero) has no spells":   stat$(106) = readglobalstring$(133, "has no spells", 20)
names$(107) = "Plotscript: pick hero":  stat$(107) = readglobalstring$(135, "Which Hero?", 20)
names$(108) = "Hero name prompt":       stat$(108) = readglobalstring$(137, "Name the Hero", 20)
names$(109) = "Found a (item)":         stat$(109) = readglobalstring$(139, "Found a", 20)
names$(110) = "Found (number) (items)": stat$(110) = readglobalstring$(141, "Found", 20)
names$(111) = "THE INN COSTS (# gold)": stat$(111) = readglobalstring$(143, "THE INN COSTS", 20)
names$(112) = "You have (# gold)":      stat$(112) = readglobalstring$(145, "You have", 20)
names$(113) = "CANNOT RUN!":            stat$(113) = readglobalstring$(147, "CANNOT RUN!", 20)
names$(114) = "Level up for (hero)":    stat$(114) = readglobalstring$(149, "Level up for", 20)
names$(115) = "(#) levels for (hero)":  stat$(115) = readglobalstring$(151, "levels for", 20)
names$(116) = "($) and (number) (item)":stat$(116) = readglobalstring$(153, "and", 10)

'names$() = "":      stat$() = readglobalstring$(, "", 10)

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(pt, top, 0, max, 22)
 strgrabber stat$(pt), maxlen(pt)
 IF keyval(28) > 1 THEN GOSUB typestat
 
 standardmenu names$(), max, 22, pt, top, 0, 0, dpage, 0
 standardmenu stat$(), max, 22, pt, top, 232, 0, dpage, 0
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
j = 0
FOR i = 0 TO max
 writeglobalstring j, stat$(i), maxlen(i)
 j = j + 1 + (maxlen(i) \ 11)
NEXT i
clearpage 0
clearpage 1
EXIT SUB

typestat:
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 OR keyval(28) > 1 OR keyval(72) > 1 OR keyval(80) > 1 THEN RETURN
 strgrabber stat$(pt), maxlen(pt)
 
 FOR i = top TO top + 22
  textcolor 7, 0
  IF i = pt THEN textcolor 14 + tog, 0
  printstr names$(i), 0, (i - top) * 8, dpage
  xpos = 232
  IF i = pt THEN
   textcolor 15, 1
   xpos = 312 - (8 * LEN(stat$(i)))
  END IF
  printstr stat$(i), xpos, (i - top) * 8, dpage
 NEXT i
 
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

END SUB

SUB storepal16 (array(), aoffset, foffset)

setpicstuf buffer(), 16, -1
loadset game$ + ".pal" + CHR$(0), 0, 0

IF buffer(0) <> 4444 THEN
 fatalerror "16-color palette file may be corrupt."
 buffer(0) = 4444
 storeset game$ + ".pal" + CHR$(0), 0, 0
END IF

last = buffer(1)

IF foffset > last THEN
 '--blank out palettes before extending file
 FOR i = last + 1 TO foffset
  FOR j = 0 TO 7
   buffer(j) = 0
  NEXT j
  storeset game$ + ".pal" + CHR$(0), 1 + i, 0
 NEXT i
 '--update header
 buffer(0) = 4444
 buffer(1) = foffset
 storeset game$ + ".pal" + CHR$(0), 0, 0
END IF

IF foffset >= 0 THEN '--never write a negative file offset
 'copy palette to buffer
 FOR i = 0 TO 7
  buffer(i) = array(aoffset * 8 + i)
 NEXT i
 'write palette
 storeset game$ + ".pal" + CHR$(0), 1 + foffset, 0
END IF

END SUB

FUNCTION sublist (num, s$())
clearpage 0
clearpage 1

setkeys
DO
 setwait timing(), 90
 setkeys
 dummy = usemenu(pt, 0, 0, num, 22)
 IF keyval(1) > 1 THEN
  sublist = -1
  EXIT DO
 END IF
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  sublist = pt
  EXIT DO
 END IF
 tog = tog XOR 1
 standardmenu s$(), num, 22, pt, top, 0, 0, dpage, 0
 SWAP dpage, vpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

clearpage 0
clearpage 1

END FUNCTION

SUB textage
DIM m$(10), x$(8), cond(21), ct(-1 TO 21), menu$(21), a(318), order(21), grey(21), choice$(1), max(8), min(8), buf(16384), h$(2), tagmn$
pt = 1

order(0) = 0:      grey(0) = -1
order(1) = 1:      grey(1) = 0
order(2) = 2:      grey(2) = -1
order(3) = 3:      grey(3) = 2
order(4) = 4:      grey(4) = 2
order(5) = 13:     grey(5) = -1
order(6) = 14:     grey(6) = 5
order(7) = 5:      grey(7) = -1
order(8) = 6:      grey(8) = 7
order(9) = 17:     grey(9) = -1
order(10) = 18:    grey(10) = 9
order(11) = 7:     grey(11) = -1
order(12) = 8:     grey(12) = 11
order(13) = 9:     grey(13) = -1
order(14) = 10:    grey(14) = 13
order(15) = 19:    grey(15) = 13
order(16) = 20:    grey(16) = 13
order(17) = 15:    grey(17) = -1
order(18) = 16:    grey(18) = 17
order(19) = 11:    grey(19) = -1
order(20) = 12:    grey(20) = 19
ct(-1) = -1
ct(0) = 0
ct(1) = 7
ct(2) = 0
ct(3) = 0
ct(4) = 0
ct(5) = 0
ct(6) = 1
ct(7) = 0
ct(8) = 2
ct(9) = 0
ct(10) = 3
ct(11) = 0
ct(12) = 7
ct(13) = 0
ct(14) = 4
ct(15) = 0
ct(16) = 5
ct(17) = 0
ct(18) = 6
m$(0) = "Return to Main Menu"
m$(1) = "Text Box"
m$(2) = "Edit Text"
m$(3) = "Edit Conditionals"
m$(4) = "Edit Choice"
m$(5) = "Box Appearance"
m$(6) = "Next:"
m$(7) = "Text Search:"
search$ = ""
GOSUB loadlines
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(29) > 0 AND keyval(14) THEN
  GOSUB savelines
  cropafter pt, general(39), 0, game$ + ".say", 400, 1
  GOSUB loadlines
 END IF
 dummy = usemenu(csr, 0, 0, 7, 24)
 remptr = pt
 SELECT CASE csr
  CASE 7'textsearch
   strgrabber search$, 36
  CASE 6'quickchainer
   IF intgrabber(cond(12), general(43) * -1, general(39), 75, 77) THEN
    IF cond(12) = 0 THEN
     cond(11) = 0
    ELSE
     IF cond(11) = 0 THEN cond(11) = -1
    END IF
    GOSUB nextboxline
   END IF'--modify next
  CASE ELSE '--not using the quick textbox chainer
   IF intgrabber(pt, 0, general(39), 51, 52) THEN
    SWAP pt, remptr
    GOSUB savelines
    SWAP pt, remptr
    GOSUB loadlines
   END IF
   IF keyval(75) > 1 AND pt > 0 THEN GOSUB savelines: pt = pt - 1: GOSUB loadlines
   IF keyval(77) > 1 AND pt < 32767 THEN
    GOSUB savelines
    pt = pt + 1
    IF needaddset(pt, general(39), "text box") THEN GOSUB clearlines
    GOSUB loadlines
   END IF'--next/add text box
 END SELECT
 IF (keyval(28) > 1 OR keyval(57) > 1) THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 2 THEN GOSUB picktext
  IF csr = 3 THEN GOSUB conditions: GOSUB nextboxline
  IF csr = 4 THEN GOSUB tchoice
  IF csr = 5 THEN GOSUB groovybox
  IF csr = 6 AND cond(12) > 0 THEN
   GOSUB savelines
   pt = cond(12)
   GOSUB loadlines
  END IF
  IF csr = 7 AND keyval(28) > 1 THEN
   GOSUB savelines
   GOSUB seektextbox
   GOSUB loadlines
  END IF
 END IF
 textcolor 7, 0
 IF csr = 1 THEN textcolor 14 + tog, 0
 printstr STR$(pt), 64, 8, dpage
 m$(7) = "Text Search:" + search$
 
 standardmenu m$(), 7, 7, csr, 0, 0, 0, dpage, 0
 
 textcolor 15, 0
 FOR i = 0 TO 7
  printstr x$(i), 8, 100 + i * 10, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
clearpage 0
clearpage 1
clearpage 2
clearpage 3
GOSUB savelines
EXIT SUB

nextboxline:
SELECT CASE cond(11)
 CASE 0
  m$(6) = "Next: None Selected"
 CASE -1
  IF cond(12) >= 0 THEN
   m$(6) = "Next: Box" + STR$(cond(12))
  ELSE
   m$(6) = "Next: script " + scriptname$(ABS(cond(12)), "plotscr.lst")
  END IF
 CASE ELSE
  IF cond(12) >= 0 THEN
   m$(6) = "Next: Box" + STR$(cond(12)) + " (conditional)"
  ELSE
   m$(6) = "Next: script " + scriptname$(ABS(cond(12)), "plotscr.lst") + " (conditional)"
  END IF
END SELECT
RETURN

conditions:
cur = 0
GOSUB heroar
GOSUB shopar
GOSUB itemar
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 IF (keyval(57) > 1 OR keyval(28) > 1) AND cur = -1 THEN RETURN
 dummy = usemenu(cur, 0, -1, 20, 24)
 IF keyval(83) > 1 AND cur > -1 THEN cond(order(cur)) = 0
 IF cur >= 0 THEN
  temp = cond(order(cur))
  IF ct(order(cur)) = 0 THEN dummy = intgrabber(cond(order(cur)), -999, 999, 75, 77)
  IF ct(order(cur)) = 1 THEN dummy = intgrabber(cond(order(cur)), 0, general(37), 75, 77)
  IF ct(order(cur)) = 2 THEN dummy = intgrabber(cond(order(cur)), -32000, general(97) + 1, 75, 77)
  IF ct(order(cur)) = 3 THEN dummy = intgrabber(cond(order(cur)), -99, 99, 75, 77)
  IF ct(order(cur)) = 4 THEN dummy = intgrabber(cond(order(cur)), -32000, 32000, 75, 77)
  IF ct(order(cur)) = 5 THEN dummy = intgrabber(cond(order(cur)), 0, 199, 75, 77)
  IF ct(order(cur)) = 6 THEN dummy = xintgrabber(cond(order(cur)), 0, 255, 0, -255, 75, 77)
  IF ct(order(cur)) = 7 THEN dummy = intgrabber(cond(order(cur)), general(43) * -1, general(39), 75, 77)
  IF order(cur) = 10 OR order(cur) = 19 OR order(cur) = 20 THEN IF temp <> cond(order(cur)) THEN GOSUB heroar
  IF order(cur) = 8 THEN IF temp <> cond(order(cur)) THEN GOSUB shopar
  IF order(cur) = 18 THEN IF temp <> cond(order(cur)) THEN GOSUB itemar
 END IF
 GOSUB textcmenu
 textcolor 7, 0
 IF cur = -1 THEN textcolor 14 + tog, 0
 printstr "Go Back", 0, 0, dpage
 FOR i = 0 TO 20
  textcolor 7, 0
  IF grey(i) < 0 THEN
   c = 6
   IF cond(order(i)) = 0 THEN c = 8
   IF cond(order(i)) = -1 THEN c = 1
   rectangle 0, 9 + (i * 9), 320, 8, c, dpage
  ELSE
   IF cond(order(grey(i))) = 0 THEN textcolor 8, 0
  END IF
  IF i = cur THEN textcolor 14 + tog, 0
  printstr menu$(order(i)), 0, 9 + (i * 9), dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

textcmenu:
n = 0: GOSUB txttag
SELECT CASE cond(1)
 CASE 0
  menu$(1) = " use [text box or script] instead"
 CASE IS < 0
  menu$(1) = " run " + scriptname$(cond(1) * -1, "plotscr.lst") + " instead"
 CASE IS > 0
  menu$(1) = " jump to text box" + STR$(cond(1)) + " instead"
END SELECT
n = 2: GOSUB txttag
FOR i = 3 TO 4
 menu$(i) = " set tag" + STR$(ABS(cond(i))) + " = " + onoroff$(cond(i)) + " (" + lmnemonic$(ABS(cond(i))) + ")"
 IF ABS(cond(i)) <= 1 THEN menu$(i) = LEFT$(menu$(i), LEN(menu$(i)) - 2) + "[unchangeable]"
NEXT i
n = 5: GOSUB txttag
menu$(6) = " fight enemy formation" + STR$(cond(6))
n = 7: GOSUB txttag
IF cond(8) > 0 THEN menu$(8) = " go to shop" + STR$(cond(8)) + " " + shop$
IF cond(8) < 0 THEN menu$(8) = " go to an Inn that costs" + STR$(ABS(cond(8))) + "$"
IF cond(8) = 0 THEN menu$(8) = " restore Hp and Mp [select shop here]"
n = 9: GOSUB txttag
IF cond(10) = 0 THEN menu$(10) = " do not add/remove heros"
IF cond(10) > 0 THEN menu$(10) = " add " + h$(0) + " to party"
IF cond(10) < 0 THEN menu$(10) = " remove " + h$(0) + " from party"
IF cond(19) = 0 THEN menu$(19) = " do not swap in/out heros"
IF cond(19) > 0 THEN menu$(19) = " swap in " + h$(1)
IF cond(19) < 0 THEN menu$(19) = " swap out " + h$(1)
IF cond(20) = 0 THEN menu$(20) = " do not unlock/lock heros"
IF cond(20) > 0 THEN menu$(20) = " unlock " + h$(2)
IF cond(20) < 0 THEN menu$(20) = " lock " + h$(2)
n = 11: GOSUB txttag
SELECT CASE cond(12)
 CASE 0
  menu$(12) = " use [text box or script] next"
 CASE IS < 0
  menu$(12) = " run " + scriptname$(cond(12) * -1, "plotscr.lst") + " next"
 CASE IS > 0
  menu$(12) = " jump to text box" + STR$(cond(12)) + " next"
END SELECT
n = 13: GOSUB txttag
IF cond(14) < 0 THEN
 menu$(14) = " lose" + STR$(ABS(cond(14))) + "$"
ELSE
 menu$(14) = " gain" + STR$(ABS(cond(14))) + "$"
END IF
n = 15: GOSUB txttag
menu$(16) = " instantly use door" + STR$(cond(16))
n = 17: GOSUB txttag
IF cond(18) = 0 THEN menu$(18) = " do not add/remove items"
IF cond(18) > 0 THEN menu$(18) = " add one" + item$
IF cond(18) < 0 THEN menu$(18) = " remove one" + item$
RETURN

txttag:
menu$(n) = "If tag" + STR$(ABS(cond(n))) + " = " + onoroff$(cond(n)) + " (" + lmnemonic$(ABS(cond(n))) + ")"
IF cond(n) = 0 THEN menu$(n) = "Never do the following"
IF cond(n) = 1 THEN menu$(n) = LEFT$(menu$(n), LEN(menu$(n)) - 2) + "[Never]"
IF cond(n) = -1 THEN menu$(n) = "Always do the following"
RETURN

tchoice:
menu$(0) = "Go Back"
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 dummy = usemenu(tcur, 0, 0, 5, 24)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF tcur = 0 THEN RETURN
  IF tcur = 1 THEN setbit buffer(), 174, 0, (readbit(buffer(), 174, 0) XOR 1)
 END IF
 IF tcur = 1 THEN
  IF keyval(75) > 1 OR keyval(77) > 1 THEN setbit buffer(), 174, 0, (readbit(buffer(), 174, 0) XOR 1)
 END IF
 FOR i = 0 TO 1
  IF tcur = 2 + (i * 2) THEN strgrabber choice$(i), 15
  IF tcur = 3 + (i * 2) THEN dummy = intgrabber(buffer(182 + (i * 9)), -999, 999, 75, 77)
 NEXT i
 GOSUB tcmenu
 
 standardmenu menu$(), 5, 5, tcur, 0, 0, 8, dpage, 0
 
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

tcmenu:
IF readbit(buffer(), 174, 0) THEN menu$(1) = "Choice = Enabled" ELSE menu$(1) = "Choice = Disabled"
FOR i = 0 TO 1
 menu$(2 + (i * 2)) = "Option" + STR$(i) + " text:" + choice$(i)
 IF buffer(182 + (i * 9)) THEN
  menu$(3 + (i * 2)) = "Set tag" + STR$(ABS(buffer(182 + (i * 9)))) + " = " + onoroff$(buffer(182 + (i * 9))) + " (" + lmnemonic$(ABS(buffer(182 + (i * 9)))) + ")"
 ELSE
  menu$(3 + (i * 2)) = "Set tag 0 (none)"
 END IF
NEXT i
RETURN

heroar:
h$(0) = heroname$(10, cond(), a())
h$(1) = heroname$(19, cond(), a())
h$(2) = heroname$(20, cond(), a())
RETURN

shopar:
shop$ = ""
IF cond(8) > 0 THEN
 setpicstuf a(), 40, -1
 loadset game$ + ".sho" + CHR$(0), cond(8) - 1, 0
 FOR i = 1 TO small(a(0), 15)
  shop$ = shop$ + CHR$(a(i))
 NEXT i
END IF
RETURN

itemar:
item$ = ""
IF cond(18) <> 0 THEN item$ = itemstr$(ABS(cond(18)), 0, 0)
RETURN

picktext:
y = 0
insert = -1
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 IF keyval(28) > 1 AND y < 7 THEN y = y + 1
 IF usemenu(y, 0, 0, 7, 24) THEN insert = -1
 IF y <= 7 AND y >= 0 THEN
  stredit x$(y), 38
 END IF
 rectangle 4, 4, 312, 88, 15, dpage
 rectangle 5, 5, 310, 86, 243, dpage
 FOR i = 0 TO 7
  textcolor 7, 0
  IF y = i THEN
   textcolor 15, 2 + tog
   printstr " ", 8 + insert * 8, 8 + i * 10, dpage
   textcolor 15 - tog, 0
  END IF
  printstr x$(i), 8, 8 + i * 10, dpage
 NEXT i
 textcolor 10, 0
 printstr "-", 0, 8 + y * 10, dpage
 textcolor 15, 0
 printstr "Text Box" + STR$(pt), 0, 100, dpage
 printstr "${C0} = Leader's name", 0, 120, dpage
 printstr "${C#} = Hero name at caterpillar slot #", 0, 128, dpage
 printstr "${P#} = Hero name at party slot #", 0, 136, dpage
 printstr "${H#} = Name of hero ID #", 0, 144, dpage
 printstr "${V#} = Global Plotscript Variable ID #", 0, 152, dpage
 printstr "${S#} = Insert String Variable with ID #", 0, 160, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
LOOP

groovybox:
menu$(0) = "Go Back"
menu$(1) = "Position:"
menu$(2) = "Shrink:"
menu$(3) = "Textcolor:"
menu$(4) = "Bordercolor:"
menu$(5) = "Backdrop:"
menu$(6) = "Music:"
menu$(7) = "Show Box:"
menu$(8) = "Translucent:"
menu$(9) = "Restore Music:"
GOSUB refresh
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 dummy = usemenu(gcsr, 0, 0, 9, 24)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF gcsr = 0 THEN RETURN
  FOR i = 0 TO 2
   IF gcsr = 7 + i THEN setbit buffer(), 174, 1 + i, (readbit(buffer(), 174, 1 + i) XOR 1)
  NEXT i
 END IF
 FOR i = 0 TO 2
  IF gcsr = 7 + i THEN
   IF keyval(75) > 1 OR keyval(77) > 1 THEN setbit buffer(), 174, 1 + i, (readbit(buffer(), 174, 1 + i) XOR 1)
  END IF
 NEXT i
 FOR i = 0 TO 3
  IF gcsr = 1 + i THEN
   IF intgrabber(buffer(193 + i), min(i), max(i), 75, 77) THEN GOSUB refresh
  END IF
 NEXT i
 FOR i = 4 TO 5
  IF gcsr = 1 + i THEN
   IF zintgrabber(buffer(193 + i), min(i), max(i), 75, 77) THEN GOSUB refresh
  END IF
 NEXT i
 GOSUB previewbox
 FOR i = 0 TO 9
  col = 7: IF i = gcsr THEN col = 14 + tog
  temp$ = menu$(i)
  IF i > 0 AND i <= 4 THEN
   temp$ = temp$ + STR$(buffer(192 + i))
  END IF
  IF i >= 5 AND i <= 6 THEN '-- backdrop and songs have "none" as an option
   IF buffer(192 + i) THEN
    temp$ = menu$(i) + STR$(buffer(192 + i) - 1)
    IF i = 6 THEN
     temp$ = temp$ + " " + getsongname$(buffer(192 + i) - 1)
    END IF
   ELSE
    temp$ = menu$(i) + " NONE"
   END IF
  END IF
  IF i > 6 AND i < 9 THEN
   IF readbit(buffer(), 174, -6 + i) THEN temp$ = temp$ + " NO" ELSE temp$ = temp$ + " YES"
  END IF
  IF i = 9 THEN
   IF readbit(buffer(), 174, -6 + i) THEN temp$ = temp$ + " YES" ELSE temp$ = temp$ + " NO"
  END IF
  edgeprint temp$, 0, i * 10, col, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
LOOP

refresh:
clearpage 2
min(0) = 0
max(0) = 27 + buffer(194)
min(1) = 0
max(1) = 21
min(2) = 0
max(2) = 255
min(3) = 0
max(3) = 14
min(4) = -1
max(4) = general(genMaxBackdrop) - 1
min(5) = -1
max(5) = general(genMaxSong)
IF buffer(197) > 0 THEN
 setdiskpages buf(), 200, 0
 loadpage game$ + ".mxs" + CHR$(0), buffer(197) - 1, 2
END IF
RETURN

previewbox:
IF readbit(buffer(), 174, 1) = 0 THEN
 rectangle 4, 4 + (buffer(193) * 4), 312, 88 - (buffer(194) * 4), ((buffer(196) + 1) * 16) + 12, dpage
 rectangle 5, 5 + (buffer(193) * 4), 310, 86 - (buffer(194) * 4), ((buffer(196) + 1) * 16) + 2, dpage
END IF
FOR i = 0 TO 7
 col = 15: IF buffer(195) > 0 THEN col = buffer(195)
 edgeprint x$(i), 8, 8 + (buffer(193) * 4) + i * 10, col, dpage
NEXT i
RETURN

loadlines:
setpicstuf buffer(), 400, -1
loadset game$ + ".say" + CHR$(0), pt, 0
temp$ = STRING$(42, 0)
array2str buffer(), 305, temp$
str2array temp$, cond(), 0
FOR i = 0 TO 21
 IF ct(i) = 0 THEN cond(i) = bound(cond(i), -999, 999)
 IF ct(i) = 1 THEN cond(i) = bound(cond(i), 0, general(37))
 IF ct(i) = 2 THEN cond(i) = bound(cond(i), -32000, 99)
 IF ct(i) = 3 THEN cond(i) = bound(cond(i), -99, 99)
 IF ct(i) = 4 THEN cond(i) = bound(cond(i), -32000, 32000)
 IF ct(i) = 5 THEN cond(i) = bound(cond(i), 0, 199)
 IF ct(i) = 6 THEN cond(i) = bound(cond(i), -255, 255)
 IF ct(i) = 7 THEN cond(i) = bound(cond(i), general(43) * -1, general(39))
NEXT i
FOR i = 0 TO 7
 x$(i) = STRING$(38, 0)
 array2str buffer(), i * 38, x$(i)
 WHILE RIGHT$(x$(i), 1) = CHR$(0): x$(i) = LEFT$(x$(i), LEN(x$(i)) - 1): WEND
NEXT i
FOR i = 0 TO 1
 choice$(i) = STRING$(15, 0)
 array2str buffer(), 349 + (i * 18), choice$(i)
 WHILE RIGHT$(choice$(i), 1) = CHR$(0): choice$(i) = LEFT$(choice$(i), LEN(choice$(i)) - 1): WEND
NEXT i
GOSUB nextboxline
search$ = ""
RETURN

savelines:
setpicstuf buffer(), 400, -1
FOR i = 0 TO 7
 WHILE LEN(x$(i)) < 38: x$(i) = x$(i) + CHR$(0): WEND
 str2array x$(i), buffer(), i * 38
NEXT i
temp$ = STRING$(42, 0)
array2str cond(), 0, temp$
str2array temp$, buffer(), 305
FOR i = 0 TO 1
 WHILE LEN(choice$(i)) < 15: choice$(i) = choice$(i) + CHR$(0): WEND
 str2array choice$(i), buffer(), 349 + (i * 18)
NEXT i
storeset game$ + ".say" + CHR$(0), pt, 0
RETURN

clearlines:
'--this inits a new text box, and copies in values from text box 0 for defaults
setpicstuf buffer(), 400, -1
loadset game$ + ".say" + CHR$(0), 0, 0
FOR i = 0 TO 199
 SELECT CASE i
  CASE 174, 193, 195, 196
   '--these are the ones we preserve
  CASE ELSE
   buffer(i) = 0
 END SELECT
NEXT i
storeset game$ + ".say" + CHR$(0), pt, 0
RETURN

seektextbox:
setpicstuf buffer(), 400, -1
remptr = pt
pt = pt + 1
DO
 IF pt > general(39) THEN pt = 0
 IF pt = remptr THEN
  rectangle 115, 90, 100, 20, 1, vpage
  edgeprint "Not found.", 120, 95, 15, vpage
  w = getkey
  EXIT DO
 END IF
 loadset game$ + ".say" + CHR$(0), pt, 0
 foundstr = 0
 FOR i = 0 TO 7
  tmp$ = STRING$(38, 0)
  array2str buffer(), i * 38, tmp$
  WHILE RIGHT$(tmp$, 1) = CHR$(0): tmp$ = LEFT$(tmp$, LEN(tmp$) - 1): WEND
  IF INSTR(UCASE$(tmp$), UCASE$(search$)) > 0 THEN foundstr = 1
 NEXT i
 IF foundstr = 1 THEN EXIT DO
 pt = pt + 1
LOOP

RETURN

'--text box record (byte offsets! not words!)
'0-303   lines
'304     unused?
'305-347 conditionals
'349-384 choice text

END SUB

SUB textxbload (f$, array(), e$)

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
   xBLOAD f$, array(), "Load failed" 'not sure about this
  ELSE
   textfatalerror e$ + "(unbloadable)"
  END IF
 ELSE
  textfatalerror e$ + "(zero byte)"
 END IF
ELSE
 textfatalerror e$
END IF

END SUB

FUNCTION usemenu (pt, top, first, last, size)

oldptr = pt
oldtop = top

IF keyval(72) > 1 THEN pt = loopvar(pt, first, last, -1) 'UP
IF keyval(80) > 1 THEN pt = loopvar(pt, first, last, 1)  'DOWN
IF keyval(73) > 1 THEN pt = large(pt - size, first)      'PGUP
IF keyval(81) > 1 THEN pt = small(pt + size, last)       'PGDN
IF keyval(71) > 1 THEN pt = first                         'HOME
IF keyval(79) > 1 THEN pt = last                          'END
top = bound(top, pt - size, pt)

IF oldptr = pt AND oldtop = top THEN
 usemenu = 0
ELSE
 usemenu = 1
END IF

END FUNCTION

FUNCTION validmusicfile (file$)
'-- actually, doesn't need to be a music file, but only multi-filetype imported data right now
ext$ = LCASE$(RIGHT$(file$, 4))
SELECT CASE ext$
 CASE ".bam"
  a$ = "    "
  realhd$ = "CBMF"
 CASE ".mid"
  a$ = "    "
  realhd$ = "MThd"
END SELECT
musfh = FREEFILE
OPEN file$ FOR BINARY AS #musfh
GET #musfh, 1, a$
CLOSE #musfh
IF a$ = realhd$ THEN validmusicfile = 1 ELSE validmusicfile = 0
END FUNCTION

SUB verifyrpg

xbload game$ + ".gen", buffer(), "General data is missing!"

FOR i = 0 TO buffer(0)
 IF NOT isfile(maplumpname$(i, "t") + CHR$(0)) THEN fatalerror "map" + filenum$(i) + " tilemap is missing!"
 IF NOT isfile(maplumpname$(i, "p") + CHR$(0)) THEN fatalerror "map" + filenum$(i) + " passmap is missing!"
 IF NOT isfile(maplumpname$(i, "e") + CHR$(0)) THEN fatalerror "map" + filenum$(i) + " foemap is missing!"
 IF NOT isfile(maplumpname$(i, "l") + CHR$(0)) THEN fatalerror "map" + filenum$(i) + " NPClocations are missing!"
 IF NOT isfile(maplumpname$(i, "n") + CHR$(0)) THEN fatalerror "map" + filenum$(i) + " NPCdefinitions are missing!"
 IF NOT isfile(maplumpname$(i, "d") + CHR$(0)) THEN fatalerror "map" + filenum$(i) + " doorlinks are missing!"
NEXT
END SUB

SUB writeconstant (filehandle, num, names$, unique$(), prefix$)
'prints to already-open filehandle 1
a$ = exclusive(names$, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 _'~")
WHILE NOT isunique(a$, unique$(), 0): a$ = numbertail(a$): WEND
IF a$ <> "" THEN
 a$ = LTRIM$(STR$(num)) + "," + prefix$ + ":" + a$
 PRINT #filehandle, a$
END IF
END SUB

