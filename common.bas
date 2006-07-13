'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'

'$INCLUDE: 'uiconst.bi'
'$INCLUDE: 'common.bi'

FUNCTION bound (n, lowest, highest)
bound = n
IF n < lowest THEN bound = lowest
IF n > highest THEN bound = highest
END FUNCTION

FUNCTION browse$ (special, default$, fmask$, tmp$, needf)
browse$ = ""

'special=0   no preview
'special=1   just BAM
'special=2   16 color BMP
'special=3   background
'special=4   master palette
'special=5   any supported music (currently *.bam and *.mid)  (fmask$ is ignored)
'special=6   any supported SFX (currently *.wav) (fmask$ is ignored)
'special=7   RPG files
mashead$ = CHR$(253) + CHR$(13) + CHR$(158) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(6)
paledithead$ = CHR$(253) + CHR$(217) + CHR$(158) + CHR$(0) + CHR$(0) + CHR$(7) + CHR$(6)

limit = 255
DIM drive(26), drive$(26), tree$(limit), display$(limit), about$(limit), treec(limit), catfg(6), catbg(6), bmpd(40)
'about$() is only used for special 7

catfg(0) = 9: catbg(0) = 8    'drives
catfg(1) = 9: catbg(1) = 8    'directories
catfg(2) = 9: catbg(2) = 0    'subdirectories
catfg(3) = 7: catbg(3) = 0    'files
catfg(4) = 11: catbg(4) = 8   'root
catfg(5) = 10: catbg(5) = 8   'special
catfg(6) = 8: catbg(5) = 0    'disabled

IF needf = 1 THEN
 FOR i = 0 TO 767
  buffer(i) = 0
 NEXT i
 buffer(24) = 5
 buffer(25) = 5
 buffer(26) = 5
 setpal buffer()
END IF

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

remember$ = curdir$ + SLASH
IF default$ = "" THEN
 nowdir$ = remember$
ELSE
 nowdir$ = default$
END IF

If special = 7 THEN viewsize = 16 ELSE viewsize = 17

GOSUB context

treeptr = 0
treetop = 0

changed = 1

setkeys
DO
 setwait 80
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF usemenu(treeptr, treetop, 0, treesize, viewsize) OR changed THEN
  alert$ = ""
  changed = 0
  GOSUB hover
 END IF
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  alert$ = ""
  changed = 1
  IF special = 1 OR special = 5 THEN stopsong
  SELECT CASE treec(treeptr)
'   CASE 0
'    IF hasmedia(ASC(LEFT$(tree$(treeptr), 1)) - 64) THEN
'     nowdir$ = LEFT$(tree$(treeptr), 3)
'     GOSUB context
'    END IF
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
 rectangle 5, 4, 310, 12, 1, dpage
 drawbox 4, 3, 312, 14, 9, dpage
 edgeprint nowdir$, 8, 6, uilook(uiText), dpage
 rectangle 5, 32 + viewsize * 9, 310, 12, 1, dpage
 drawbox 4, 31 + viewsize * 9, 312, 14, 9, dpage
 edgeprint alert$, 8, 34 + viewsize * 9, uilook(uiText), dpage
 IF special = 7 THEN
  rectangle 0, 190, 320, 10, 8, dpage
  edgeprint version$, 8, 190, uilook(uiMenuItem), dpage
  textcolor uilook(uiText), 0
 END IF
 textcolor uilook(uiText), 0
 printstr ">", 0, 20 + (treeptr - treetop) * 9, dpage
 FOR i = treetop TO small(treetop + viewsize, treesize)
  textcolor catfg(treec(i)), catbg(treec(i))
  a$ = display$(i)
  IF LEN(a$) < 38 AND catbg(treec(i)) > 0 THEN a$ = a$ + STRING$(38 - LEN(a$), " ")
  printstr a$, 10, 20 + (i - treetop) * 9, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 IF needf = 1 THEN fadein -1: setkeys
 IF needf THEN needf = needf - 1
 dowait
LOOP
default$ = nowdir$
EXIT FUNCTION

hover:
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
    loadsong nowdir$ + tree$(treeptr) + CHR$(0)
   ELSE
    alert$ = tree$(treeptr) + " is not a valid music file"
   END IF
  END IF
 CASE 6
  stopsfx 0
  IF treec(treeptr) = 3 OR treec(treeptr) = 6 THEN
   IF isawav(nowdir$ + tree$(treeptr)) THEN
    loadsfx 0, nowdir$ + tree$(treeptr)
    playsfx 0,0
   ELSE
    alert$ = tree$(treeptr) + " is not a valid sound effect"
   END IF
  END IF
 CASE 7
  alert$ = about$(treeptr)
END SELECT
IF treec(treeptr) = 0 THEN alert$ = "Drive"
IF treec(treeptr) = 1 THEN alert$ = "Directory"
IF treec(treeptr) = 2 THEN alert$ = "Subdirectory"
RETURN

context:
'for progress meter
IF ranalready THEN rectangle 5, 32 + viewsize * 9, 310, 12, 1, vpage
meter = 0
treesize = 0
IF nowdir$ = "" THEN
ELSE
 GOSUB drawmeter
 a$ = nowdir$
 IF LINUX THEN
  treesize = -1
 ELSE
  a = ASC(LEFT$(nowdir$, 1)) - 64
  FOR i = 0 TO drivetotal - 1
   'IF a = drive(i) THEN tree$(treesize) = drive$(i)
   IF a = drive(i) THEN
    tree$(treesize) = CHR$(64 + drive(i)) + ":" + SLASH
    GOSUB drawmeter
   END IF
  NEXT i
  treec(treesize) = 0
  a$ = RIGHT$(a$, LEN(a$) - 3)
 END IF 
 b$ = ""
 DO UNTIL a$ = "" OR treesize >= limit
  b$ = b$ + LEFT$(a$, 1)
  a$ = RIGHT$(a$, LEN(a$) - 1)
  IF RIGHT$(b$, 1) = SLASH THEN
   treesize = treesize + 1
   tree$(treesize) = b$
   treec(treesize) = 1
   b$ = ""
   GOSUB drawmeter
  END IF
 LOOP
 '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
 findfiles nowdir$ + ALLFILES + CHR$(0), 16, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
 fh = FREEFILE
 OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh) OR treesize >= limit
  treesize = treesize + 1
  treec(treesize) = 2
  LINE INPUT #fh, tree$(treesize)
  IF tree$(treesize) = "." OR tree$(treesize) = ".." OR RIGHT$(tree$(treesize), 4) = ".tmp" THEN treesize = treesize - 1
  GOSUB drawmeter
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
 ELSEIF special = 6 THEN
  '--disregard fmask$. one call per extension
  findfiles nowdir$ + "*.wav" + CHR$(0), 0, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
  GOSUB addmatchs
 ELSE
  findfiles nowdir$ + fmask$ + CHR$(0), 0, tmp$ + "hrbrowse.tmp" + CHR$(0), buffer()
  GOSUB addmatchs
 END IF
END IF

'--set display
FOR i = 0 TO treesize
 IF NOT (special = 7 AND (treec(i) = 3 OR treec(i) = 6)) THEN
  display$(i) = tree$(i)
 END IF
NEXT

meter = 0

'--alphabetize
FOR o = treesize TO 2 STEP -1
 FOR i = 1 TO o
  IF (treec(i) = 2 OR treec(i) = 3 OR treec(i) = 6) AND (treec(i - 1) = 2 OR treec(i - 1) = 3 OR treec(i - 1) = 6) THEN
   IF ASC(LCASE$(LEFT$(display$(i), 1))) < ASC(LCASE$(LEFT$(display$(i - 1), 1))) THEN
    SWAP display$(i), display$(i - 1)
    SWAP about$(i), about$(i - 1)
    SWAP tree$(i), tree$(i - 1)
    SWAP treec(i), treec(i - 1)
   END IF
  END IF
 NEXT i
 GOSUB drawmeter2
NEXT o

'--sort by type
FOR o = treesize TO 2 STEP -1
 FOR i = 1 TO o
  IF (treec(i) = 2 OR treec(i) = 3 OR treec(i) = 6) AND (treec(i - 1) = 2 OR treec(i - 1) = 3 OR treec(i - 1) = 6) THEN
   IF treec(i) < treec(i - 1) THEN
    SWAP display$(i), display$(i - 1)
    SWAP about$(i), about$(i - 1)
    SWAP tree$(i), tree$(i - 1)
    SWAP treec(i), treec(i - 1)
   END IF
  END IF
 NEXT i
 GOSUB drawmeter2
NEXT o

'--set cursor
IF treeptr > treesize THEN treeptr = 0: treetop = 0
FOR i = 1 TO treesize
 IF treec(i) = 1 OR treec(i) = 0 THEN treeptr = i
NEXT i
FOR i = treesize TO 2 STEP -1
 IF treec(i) = 3 THEN treeptr = i
NEXT i
treetop = bound(treetop, treeptr - (viewsize + 2), treeptr)

'--don't display progress bar overtop of previous menu
ranalready = 1

RETURN

addmatchs:
fh = FREEFILE
OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #fh
DO UNTIL EOF(fh) OR treesize >= limit
 treesize = treesize + 1
 treec(treesize) = 3
 LINE INPUT #fh, tree$(treesize)
 '---music files
 IF special = 1 OR special = 5 THEN
  IF validmusicfile(nowdir$ + tree$(treesize)) = 0 THEN
   treec(treesize) = 6
  END IF
 END IF
 IF special = 6 THEN
  IF isawav(nowdir$ + tree$(treesize)) = 0 THEN
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
 '--RPG files
 IF special = 7 THEN
  unlumpfile nowdir$ + tree$(treesize), "browse.txt", tmp$, buffer()
  IF isfile(tmp$ + "browse.txt") THEN
   setpicstuf buffer(), 40, -1
   loadset tmp$ + "browse.txt", 0, 0
   display$(treesize) = STRING$(bound(buffer(0), 0, 38), " ")
   array2str buffer(), 2, display$(treesize)
   loadset tmp$ + "browse.txt", 1, 0
   about$(treesize) = STRING$(bound(buffer(0), 0, 38), " ")
   array2str buffer(), 2, about$(treesize)
   safekill tmp$ + "browse.txt"
   IF LEN(display$(treesize)) = 0 THEN display$(treesize) = tree$(treesize)
  ELSE
   about$(treesize) = ""
   display$(treesize) = tree$(treesize)
  END IF
 END IF

 GOSUB drawmeter
LOOP
CLOSE #fh
safekill tmp$ + "hrbrowse.tmp"

RETURN

drawmeter:
IF ranalready THEN
 meter = small(meter + 1, 308): rectangle 5 + meter, 33 + viewsize * 9, 2, 5, 9, vpage
 setvispage vpage 'refresh
END IF
RETURN

drawmeter2:
IF ranalready THEN
 meter = small(meter + 1, 308): rectangle 5 + meter, 41 + viewsize * 9, 2, 2, 9, vpage
 setvispage vpage 'refresh
END IF
RETURN

'this block is never called
vlabels:
FOR i = 0 TO drivetotal - 1
 IF isremovable(drive(i)) = 0 THEN
  drive$(i) = CHR$(64 + drive(i)) + ":" + SLASH + " (removable)"
 ELSE '--not removable--
  IF hasmedia(drive(i)) THEN
   findfiles CHR$(64 + drive(i)) + ":" + SLASH + ALLFILES + CHR$(0), 8, tmpdir$ + "hrbrowse.tmp" + CHR$(0), buffer()
   fh = FREEFILE
   OPEN tmpdir$ + "hrbrowse.tmp" FOR INPUT AS #fh
   IF LOF(fh) THEN
    LINE INPUT #fh, a$
    b$ = ""
    FOR j = 1 TO LEN(a$)
     IF MID$(a$, j, 1) <> "." THEN b$ = b$ + MID$(a$, j, 1)
    NEXT j
    drive$(i) = CHR$(64 + drive(i)) + ":" + SLASH + " (" + b$ + ")"
   END IF
   CLOSE #fh
   safekill tmpdir$ + "hrbrowse.tmp"
  ELSE '--no media--
   drive$(i) = CHR$(64 + drive(i)) + ":" + SLASH + " (not ready)"
  END IF'--check media--
 END IF'--check removable--
NEXT i
RETURN

END FUNCTION

SUB edgeprint (s$, x, y, c, p)
textcolor uilook(uiOutline), 0
printstr s$, x, y + 1, p
printstr s$, x + 1, y, p
printstr s$, x + 2, y + 1, p
printstr s$, x + 1, y + 2, p
textcolor c, 0
printstr s$, x + 1, y + 1, p
END SUB

'fade in and out not actually used in custom
SUB fadein (force)
fadestate = 1
fadetopal master(), buffer()
END SUB

SUB fadeout (red, green, blue, force)
fadestate = 0
fadeto buffer(), red, green, blue
END SUB

SUB getui (f$)
'load ui colors from data lump
'(lump not finalised, just set defaults for now)

RESTORE defaultui
FOR i=0 TO uiColors
 READ col%
 uilook(i) = col%
NEXT

'The QB editor moves this data to the top, but QB still compiles fine
'with it here.
defaultui:
DATA 0,7,8,14,15,6,7,1,2,18,21,35,37,15,240,10,14
DATA 18,28,34,44,50,60,66,76,82,92,98,108,114,124,130,140
DATA 146,156,162,172,178,188,194,204,210,220,226,236,242,252 

END SUB

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

SUB safekill (f$)
IF isfile(f$ + CHR$(0)) THEN KILL f$
END SUB

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION trimpath$ (filename$)
'return the filename without path
IF NOT INSTR(filename$,SLASH) THEN RETURN filename$
FOR i = LEN(filename$) TO 1 STEP -1
 IF MID$(filename$, i, 1) = SLASH THEN i += 1 : EXIT FOR
NEXT
RETURN MID$(filename$, i)
END FUNCTION

FUNCTION trimextension$ (filename$)
'return the filename without extension
IF NOT INSTR(filename$,".") THEN RETURN filename$
FOR i = LEN(filename$) TO 1 STEP -1
 IF MID$(filename$, i, 1) = "." THEN i -= 1 : EXIT FOR
NEXT
RETURN MID$(filename$, 1, i)
END FUNCTION

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

FUNCTION soundfile$ (sfxnum%)
	DIM as string sfxbase
	
	sfxbase = workingdir$ + SLASH + "sfx" + LTRIM$(STR$(sfxnum%))
	soundfile = ""
	if isfile(sfxbase + ".wav") then
		'is there a wave?
		soundfile = sfxbase + ".wav"
	else
		'other formats? not right now
	end if
END FUNCTION

SUB debug (s$)
 DIM filename$
 #IFDEF IS_GAME
   filename$ = "g_debug.txt"
 #ELSE
   filename$ = "c_debug.txt"
 #ENDIF
 fh = FREEFILE
 OPEN filename$ FOR APPEND AS #fh
 PRINT #fh, s$
 CLOSE #fh
END SUB
