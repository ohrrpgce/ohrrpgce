'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

'$INCLUDE: 'const.bi'
'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'

'$INCLUDE: 'uiconst.bi'
'$INCLUDE: 'common.bi'

'$INCLUDE: 'music.bi'

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
DIM drive$(26), tree$(limit), display$(limit), about$(limit), treec(limit), catfg(6), catbg(6), bmpd(40), f = -1
'about$() is only used for special 7

showHidden = 0

catfg(0) = 7: catbg(0) = 1    'selectable drives (none on unix systems)
catfg(1) = 9: catbg(1) = 8    'directories
catfg(2) = 9: catbg(2) = 0    'subdirectories
catfg(3) = 7: catbg(3) = 0    'files
catfg(4) = 9: catbg(4) = 8   'root of current drive
catfg(5) = 10: catbg(5) = 8   'special
catfg(6) = 8: catbg(6) = 0    'disabled

IF needf = 1 THEN
 FOR i = 0 TO 767
  buffer(i) = 0
 NEXT i
 buffer(24) = 5
 buffer(25) = 5
 buffer(26) = 5
 setpal buffer()
END IF

drivetotal = drivelist(drive$())

remember$ = curdir$ + SLASH
IF default$ = "" THEN
 nowdir$ = remember$
ELSE
 nowdir$ = default$
END IF

If special = 7 THEN viewsize = 16 ELSE viewsize = 17

treeptr = 0
treetop = 0
treesize = 0

ranalready = 0
GOSUB context

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
   CASE 0
    'this could take a while...
    rectangle 5, 32 + viewsize * 9, 310, 12, 1, vpage
    edgeprint "Reading...", 8, 34 + viewsize * 9, uilook(uiText), vpage
    setvispage vpage
    IF hasmedia(tree$(treeptr)) THEN
     nowdir$ = tree$(treeptr)
     'display$(treeptr) = tree$(treeptr) + " <" + drivelabel$(tree$(treeptr)) + ">"
     GOSUB context
    ELSE
     alert$ = "No media"
     changed = 0
    END IF
   CASE 1, 4
    nowdir$ = ""
    FOR i = drivetotal TO treeptr
     nowdir$ = nowdir$ + tree$(i)
    NEXT i
    GOSUB context
   CASE 2
    nowdir$ = nowdir$ + tree$(treeptr) + SLASH
    GOSUB context
   CASE 3
    browse$ = nowdir$ + tree$(treeptr)
    EXIT DO
  END SELECT
 END IF
 IF keyval(29) THEN
  'Ctrl + H for hidden
  IF keyval(35) > 1 THEN
   showHidden = showHidden XOR attribHidden
   GOSUB context
  END IF
 ELSE
  'find by letter
  FOR i = 2 TO 53
   IF keyval(i) > 1 AND keyv(i, 0) > 0 THEN
    FOR j = 1 TO treesize
     mappedj = (j + treeptr) MOD (treesize + 1)
     tempstr$ = LCASE$(display$(mappedj))
     IF (treec(mappedj) = 1 OR treec(mappedj) = 2 OR treec(mappedj) = 3) AND tempstr$[0] = keyv(i, 0) THEN treeptr = mappedj: EXIT FOR
    NEXT
    EXIT FOR
   END IF
  NEXT i
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
stopsong:if f >= 0 then sound_stop(f, -1): UnloadSound(f)
EXIT FUNCTION

hover:
SELECT CASE special
 CASE 1
  stopsong
  IF treec(treeptr) = 3 OR treec(treeptr) = 6 THEN
   IF validmusicfile(nowdir$ + tree$(treeptr), FORMAT_BAM) THEN
    loadsong nowdir$ + tree$(treeptr)
   ELSE
    alert$ = tree$(treeptr) + " is not a valid BAM file"
   END IF
  END IF
 CASE 2, 3
  IF bmpinfo(nowdir$ + tree$(treeptr), bmpd()) THEN
   alert$ = STR$(bmpd(1)) + "*" + STR$(bmpd(2)) + " pixels, " + STR$(bmpd(0)) + "-bit color"
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
   IF validmusicfile(nowdir$ + tree$(treeptr), VALID_MUSIC_FORMAT) THEN
    loadsong nowdir$ + tree$(treeptr)
   ELSE
    alert$ = tree$(treeptr) + " is not a valid music file"
   END IF
  END IF
 CASE 6
  if f > -1 then
    sound_stop(f,-1)
    UnloadSound(f)
    f = -1
  end if
  IF treec(treeptr) = 3 OR treec(treeptr) = 6 THEN
   IF validmusicfile(nowdir$ + tree$(treeptr), VALID_FX_FORMAT) THEN
    f = LoadSound(nowdir$ + tree$(treeptr))
    sound_play(f, 0, -1)
   ELSE
    alert$ = left(tree$(treeptr), 20) + " is not a valid sound effect"
   END IF
  END IF
 CASE 7
  alert$ = about$(treeptr)
END SELECT
IF treec(treeptr) = 0 THEN alert$ = "Drive"
IF treec(treeptr) = 1 THEN alert$ = "Directory"
IF treec(treeptr) = 2 THEN alert$ = "Subdirectory"
IF treec(treeptr) = 4 THEN alert$ = "Root"
RETRACE

context:
'erase old list
FOR i = 0 TO limit
 tree$(i) = ""
 display$(i) = ""
 about$(i) = ""
 treec(i) = 0
NEXT i
'for progress meter
IF ranalready THEN rectangle 5, 32 + viewsize * 9, 310, 12, 1, vpage
meter = 0
treesize = 0
IF nowdir$ = "" THEN
ELSE
 GOSUB drawmeter
 a$ = nowdir$
 '--Drive list
 IF LINUX THEN
  treesize = 0
 ELSE
  FOR i = 0 TO drivetotal - 1
   tree$(treesize) = drive$(i)
   treec(treesize) = 0
   IF isremovable(drive$(i)) THEN
    display$(treesize) = drive$(i) + " (removable)"
   ELSE
    IF hasmedia(drive$(i)) THEN
     display$(treesize) = drive$(i) + " <" + drivelabel$(drive$(i)) + ">"
    ELSE
     display$(treesize) = drive$(i) + " (not ready)"
    END IF
    GOSUB drawmeter
   END IF
   treesize += 1
  NEXT i
  'could add My Documents to drives list here
 END IF
 '--Current drive
 tree$(treesize) = MID$(a$, 1, INSTR(a$, SLASH))
#IFNDEF __FB_LINUX__
 IF hasmedia(tree$(treesize)) = 0 THEN
  'Somebody pulled out the disk
  changed = 0
  alert$ = "Disk not readable"
  treesize -= 1
  treeptr = 0
  treetop = 0
  nowdir$ = ""
  RETRACE
 END IF
#ENDIF
 a$ = MID$(a$, INSTR$(a$, SLASH) + 1)
 treec(treesize) = 4
 tmpname$ = drivelabel$(tree$(treesize))
 IF LEN(tmpname$) THEN display$(treesize) = tree$(treesize) + " <" + tmpname$ + ">"
 '--Directories
 b$ = ""
 DO UNTIL a$ = "" OR treesize >= limit
  b$ = b$ + LEFT$(a$, 1)
  a$ = RIGHT$(a$, LEN(a$) - 1)
  IF RIGHT$(b$, 1) = SLASH THEN
#IFNDEF __FB_LINUX__
   'Special handling of My Documents in Windows
   IF b$ = "My Documents\" OR b$ = "MYDOCU~1\" THEN
    FOR i = treesize to drivetotal STEP -1
     b$ = tree$(i) + b$
    NEXT i
    treesize = drivetotal - 1
    display$(treesize + 1) = "My Documents\"
   END IF
#ENDIF
   treesize = treesize + 1
   tree$(treesize) = b$
   treec(treesize) = 1
   b$ = ""
   GOSUB drawmeter
  END IF
 LOOP
 '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
 findfiles nowdir$ + ALLFILES, 16, tmp$ + "hrbrowse.tmp"
 fh = FREEFILE
 OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh) OR treesize >= limit
  treesize = treesize + 1
  treec(treesize) = 2
  LINE INPUT #fh, tree$(treesize)
  IF tree$(treesize) = "." OR tree$(treesize) = ".." OR RIGHT$(tree$(treesize), 4) = ".tmp" THEN treesize = treesize - 1
  IF special = 7 THEN ' Special handling in RPG mode
   IF right$(tree$(treesize),7) = ".rpgdir" THEN treesize = treesize -1
  END IF
  GOSUB drawmeter
 LOOP
 CLOSE #fh
 safekill tmp$ + "hrbrowse.tmp"
 '---FIND ALL FILES IN FILEMASK---
 attrib = attribAlmostAll OR showHidden
 IF special = 5 THEN
  '--disregard fmask$. one call per extension
  findfiles nowdir$ + anycase$("*.bam"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.mid"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.xm"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.it"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.mod"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.s3m"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.ogg"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.mp3"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
 ELSEIF special = 6 THEN
  '--disregard fmask$. one call per extension
  findfiles nowdir$ + anycase$("*.wav"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.s3m"), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
'   findfiles nowdir$ + anycase$("*.xm"), attrib, tmp$ + "hrbrowse.tmp"
'   GOSUB addmatchs
'   findfiles nowdir$ + anycase$("*.it"), attrib, tmp$ + "hrbrowse.tmp"
'   GOSUB addmatchs
'   findfiles nowdir$ + anycase$("*.mod"), attrib, tmp$ + "hrbrowse.tmp"
'   GOSUB addmatchs
   findfiles nowdir$ + anycase$("*.ogg"), attrib, tmp$ + "hrbrowse.tmp"
   GOSUB addmatchs
   findfiles nowdir$ + anycase$("*.mp3"), attrib, tmp$ + "hrbrowse.tmp"
   GOSUB addmatchs
 ELSEIF special = 7 THEN
  'Call once for RPG files once for rpgdirs
  findfiles nowdir$ + anycase$(fmask$), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
  findfiles nowdir$ + anycase$("*.rpgdir"), 16, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
 ELSE
  findfiles nowdir$ + anycase$(fmask$), attrib, tmp$ + "hrbrowse.tmp"
  GOSUB addmatchs
 END IF
END IF

'--set display
FOR i = 0 TO treesize
 IF LEN(display$(i)) = 0 THEN
  display$(i) = tree$(i)
 END IF
NEXT

sortstart = 0
FOR k = 0 TO treesize
 IF treec(k) = 2 OR treec(k) = 3 OR treec(k) = 6 THEN sortstart = k: EXIT FOR
NEXT

'--alphabetize
FOR i = sortstart TO treesize - 1
 FOR j = treesize TO i + 1 STEP -1
  FOR k = 0 TO small(LEN(display$(i)), LEN(display$(j)))
   chara = ASC(LCASE$(CHR$(display$(i)[k])))
   charb = ASC(LCASE$(CHR$(display$(j)[k])))
   IF chara < charb THEN
    EXIT FOR
   ELSEIF chara > charb THEN
    SWAP display$(i), display$(j)
    SWAP about$(i), about$(j)
    SWAP tree$(i), tree$(j)
    SWAP treec(i), treec(j)
    EXIT FOR
   END IF
  NEXT
 NEXT i
NEXT o

'--sort by type
FOR o = treesize TO sortstart + 2 STEP -1
 FOR i = sortstart + 1 TO o
  IF treec(i) < treec(i - 1) THEN
   SWAP display$(i), display$(i - 1)
   SWAP about$(i), about$(i - 1)
   SWAP tree$(i), tree$(i - 1)
   SWAP treec(i), treec(i - 1)
  END IF
 NEXT i
NEXT o

'--set cursor
treeptr = 0
treetop = 0
FOR i = drivetotal TO treesize
 IF treec(i) = 1 OR treec(i) = 4 THEN treeptr = i
NEXT i
FOR i = treesize TO 1 STEP -1
 IF treec(i) = 3 THEN treeptr = i
NEXT i
treetop = bound(treetop, treeptr - (viewsize + 2), treeptr)

'--don't display progress bar overtop of previous menu
ranalready = 1

RETRACE

addmatchs:
fh = FREEFILE
OPEN tmp$ + "hrbrowse.tmp" FOR INPUT AS #fh
DO UNTIL EOF(fh) OR treesize >= limit
 treesize = treesize + 1
 treec(treesize) = 3
 LINE INPUT #fh, tree$(treesize)
 '---music files
 IF special = 1 OR special = 5 THEN
  IF validmusicfile(nowdir$ + tree$(treesize), VALID_MUSIC_FORMAT) = 0 THEN
   treec(treesize) = 6
  END IF
 END IF
 IF special = 6 THEN
  IF validmusicfile(nowdir$ + tree$(treesize), VALID_FX_FORMAT) = 0 THEN
   treec(treesize) = 6
  END IF
 END IF
 '---4-bit BMP browsing
 IF special = 2 THEN
  IF bmpinfo(nowdir$ + tree$(treesize), bmpd()) THEN
   IF bmpd(0) <> 4 OR bmpd(1) > 320 OR bmpd(2) > 200 THEN
    treec(treesize) = 6
   END IF
  ELSE
   treesize = treesize - 1
  END IF
 END IF
 '---320x200x24/8bit BMP files
 IF special = 3 THEN
  IF bmpinfo(nowdir$ + tree$(treesize), bmpd()) THEN
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
  IF isdir(nowdir$ + tree$(treesize)) THEN
   'unlumped RPGDIR folders
   copyfile nowdir$ + tree$(treesize) + SLASH + "browse.txt", tmp$ + "browse.txt", buffer()
  ELSE
   'lumped RPG files
   unlumpfile nowdir$ + tree$(treesize), "browse.txt", tmp$
  END IF
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

RETRACE

drawmeter:
IF ranalready THEN
 meter = small(meter + 1, 308): rectangle 5 + meter, 33 + viewsize * 9, 2, 5, 9, vpage
 setvispage vpage 'refresh
END IF
RETRACE

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
DATA 0,7,8,14,15,6,7,1,2,18,21,35,37,15,240,10,14,240
DATA 18,28,34,44,50,60,66,76,82,92,98,108,114,124,130,140
DATA 146,156,162,172,178,188,194,204,210,220,226,236,242,252

END SUB

SUB safekill (f$)
IF isfile(f$) THEN KILL f$
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

FUNCTION soundfile$ (sfxnum%)
	DIM as string sfxbase, f

	sfxbase = workingdir$ + SLASH + "sfx" + STR$(sfxnum%)
	f = dir(sfxbase & ".*")
	if f <> "" then
		return workingdir$ & SLASH & f
	else
		return ""
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

FUNCTION getfixbit(bitnum AS INTEGER) AS INTEGER
 DIM f$
 f$ = workingdir$ + SLASH + "fixbits.bin"
 IF NOT isfile(f$) THEN RETURN 0
 DIM bits(1) as INTEGER
 setpicstuf bits(), 2, -1
 loadset f$, 0, 0
 RETURN readbit(bits(), 0, bitnum)
END FUNCTION

SUB setfixbit(bitnum AS INTEGER, bitval AS INTEGER)
 DIM f$
 f$ = workingdir$ + SLASH + "fixbits.bin"
 DIM bits(1) as INTEGER
 setpicstuf bits(), 2, -1
 IF isfile(f$) THEN
  loadset f$, 0, 0
 END IF
 setbit bits(), 0, bitnum, bitval
 storeset f$, 0, 0
END SUB

FUNCTION aquiretempdir$ ()
#IFNDEF __FB_LINUX__
'Windows only behavior
t$ = environ$("TEMP")
IF NOT isdir(t$) THEN t$ = environ("TMP")
IF NOT isdir(t$) THEN
 '--fall back to working dir if all else fails
 t$ = exepath$
END IF
IF RIGHT$(t$, 1) <> SLASH THEN t$ = t$ + SLASH
RETURN t$
#ELSE
'Linux only behavior
#IFDEF IS_CUSTOM
RETURN ""
#ELSE
h$ = environ$("HOME")
o$ = h$ + "/.ohrrpgce"
IF NOT isdir(o$) THEN makedir(o$)
d$ = DATE
t$ = TIME
tmp$ = o$ + "/" + MID$(d$,7,4) + MID$(d$,1,2) + MID$(d$,4,2) + MID$(t$,1,2) + MID$(t$,4,2) + MID$(t$,7,2) + "." + STR$(INT(RND * 1000)) + ".tmp"
tmp$ = tmp$ + "/"
RETURN tmp$
#ENDIF
#ENDIF
END FUNCTION

SUB copylump(package$, lump$, dest$)
IF isdir(package$) THEN
 'unlumped folder
 copyfile package$ + SLASH + lump$, dest$ + SLASH + lump$, buffer()
ELSE
 'lumpfile
 unlumpfile package$, lump$, dest$ + SLASH
END IF
END SUB

SUB centerbox (x, y, w, h, c, p)
tbc = uiTextBox + (2 * (c - 1))
rectangle x - INT(w * .5), y - INT(h * .5), w, h, uilook(tbc), p
rectangle x - INT(w * .5), y - INT(h * .5), w, 1, uilook(tbc + 1), p
rectangle x - INT(w * .5), y + (h - INT(h * .5)), w, 1, uilook(tbc + 1), p
rectangle x - INT(w * .5), y - INT(h * .5), 1, h, uilook(tbc + 1), p
rectangle x + (w - INT(w * .5)), y - INT(h * .5), 1, h + 1, uilook(tbc + 1), p
END SUB

SUB centerfuz (x, y, w, h, c, p)
tbc = uiTextBox + (2 * (c - 1))
fuzzyrect x - INT(w * .5), y - INT(h * .5), w, h, uilook(tbc), p
rectangle x - INT(w * .5), y - INT(h * .5), w, 1, uilook(tbc + 1), p
rectangle x - INT(w * .5), y + (h - INT(h * .5)), w, 1, uilook(tbc + 1), p
rectangle x - INT(w * .5), y - INT(h * .5), 1, h, uilook(tbc + 1), p
rectangle x + (w - INT(w * .5)), y - INT(h * .5), 1, h + 1, uilook(tbc + 1), p
END SUB

FUNCTION readbinstring$ (array(), offset, maxlen)

result$ = ""
strlen = bound(array(offset), 0, maxlen)

i = 1
DO WHILE LEN(result$) < strlen
 '--get an int
 n = array(offset + i)
 i = i + 1

 '--append the lowbyte as a char
 result$ = result$ + CHR$(n AND &HFF)

 '--if we still care about the highbyte, append it as a char too
 IF LEN(result$) < strlen THEN
  result$ = result$ + CHR$((n SHR 8) AND &HFF)
 END IF

LOOP

readbinstring$ = result$
END FUNCTION

SUB writebinstring (savestr$, array(), offset, maxlen)
s$ = savestr$

'--pad s$ to the right length
DO WHILE LEN(s$) < maxlen
 s$ = s$ + CHR$(0)
LOOP

'--if it is an odd number
IF (LEN(s$) AND 1) THEN
 s$ = s$ + CHR$(0)
END IF

'--write length (current not max)
array(offset) = LEN(savestr$)

FOR i = 1 TO LEN(s$) \ 2
 array(offset + i) = s$[2 * i - 2] OR (s$[2 * i - 1] SHL 8)
NEXT

END SUB

FUNCTION readbadbinstring$ (array(), offset, maxlen, skipword)
result$ = ""
strlen = bound(array(offset), 0, maxlen)

FOR i = 1 TO strlen
 '--read and int
 n = array(offset + skipword + i)
 '--if the int is a char use it.
 IF n >= 0 AND n <= 255 THEN
  '--take the low byte
  n = (n AND &HFF)
  '--use it
  result$ = result$ + CHR$(n)
 END IF
NEXT i

readbadbinstring$ = result$
END FUNCTION

SUB writebadbinstring (savestr$, array(), offset, maxlen, skipword)

'--write current length
array(offset) = LEN(savestr$)

FOR i = 1 TO LEN(savestr$)
 array(offset + skipword + i) = savestr$[i - 1]
NEXT i

FOR i = LEN(savestr$) + 1 TO maxlen
 array(offset + skipword + i) = 0
NEXT i

END SUB

FUNCTION read32bitstring$ (array(), offset)
result$ = ""
word = array(offset + 1)
FOR i = 1 TO array(offset)
 result$ += CHR$(word AND 255)
 IF i MOD 4 = 0 THEN word = array(offset + i \ 4 + 1) ELSE word = word SHR 8
NEXT
read32bitstring$ = result$
END FUNCTION

FUNCTION readbadgenericname$ (index, filename$, recsize, offset, size, skip)

'--clobbers buffer!

result$ = ""

IF index >= 0 THEN
 setpicstuf buffer(), recsize, -1
 loadset filename$, index, 0
 result$ = readbadbinstring$(buffer(), offset, size, skip)
END IF

readbadgenericname = result$

END FUNCTION

FUNCTION isbit (bb() as INTEGER, BYVAL w as INTEGER, BYVAL b as INTEGER) as INTEGER
 IF readbit (bb(), w, b) THEN
  RETURN -1
 ELSE
  RETURN 0
 END IF
END FUNCTION

FUNCTION scriptname$ (num, trigger = 0)
#ifdef IS_GAME
 'remember script names!
 STATIC cachenum, cacheids(9), cachenames$(9)
 FOR i = 0 TO cachenum - 1
  IF cacheids(i) = num THEN RETURN cachenames$(i)
 NEXT
#endif

DIM buf(19)
IF num >= 16384 AND trigger > 0 THEN
 fh = FREEFILE
 OPEN workingdir$ + SLASH + "lookup" + STR$(trigger) + ".bin" FOR BINARY AS #fh
 IF (num - 16384) * 40 <= LOF(fh) THEN
  loadrecord buf(), fh, 40, num - 16384
  sname$ = readbinstring(buf(), 1, 36)
  IF buf(0) THEN
   a$ = sname$
  ELSE
   a$ = "[" + sname$ + "]"
  END IF
  CLOSE fh
  GOTO theend
 END IF
 CLOSE fh
END IF

a$ = "[" + STR$(num) + "]"
IF num THEN
 fh = FREEFILE
 OPEN workingdir$ + SLASH + "plotscr.lst" FOR BINARY AS #fh
 numscripts = LOF(fh) \ 40
 CLOSE fh
 'numscripts = FILELEN(workingdir$ + SLASH + f$) \ 40
 setpicstuf buf(), 40, -1
 FOR i = 0 TO numscripts - 1
  loadset workingdir$ + SLASH + "plotscr.lst", i, 0
  IF buf(0) = num THEN
   a$ = STRING$(small(large(buf(1), 0), 38), " ")
   array2str buf(), 4, a$
   EXIT FOR
  END IF
 NEXT i
ELSE
 a$ = "[none]"
END IF

theend:
scriptname$ = a$
#ifdef IS_GAME
 IF cachenum = 10 THEN cachenum = 0
 cacheids(cachenum) = num
 cachenames$(cachenum) = a$
 cachenum += 1
#endif
END FUNCTION

SUB loaddefaultpals(fileset, poffset(), sets)
 DIM v AS SHORT
 f$ = workingdir$ & SLASH & "defpal" & fileset & ".bin"
 IF isfile(f$) THEN
   fh = FREEFILE
   OPEN f$ FOR BINARY AS #fh
   FOR i = 0 to sets
    GET #fh, 1 + i * 2, v
    poffset(i) = v
   NEXT i
   CLOSE #fh
 ELSE
   guessdefaultpals fileset, poffset(), sets
 END IF
END SUB

SUB savedefaultpals(fileset, poffset(), sets)
 DIM v AS SHORT
 f$ = workingdir$ & SLASH & "defpal" & fileset & ".bin"
 fh = FREEFILE
 OPEN f$ FOR BINARY AS #fh
 FOR i = 0 to sets
  v = poffset(i)
  PUT #fh, 1 + i * 2, v
 NEXT i
 CLOSE #fh
END SUB

SUB guessdefaultpals(fileset, poffset(), sets)
 flusharray poffset(), sets, 0
 SELECT CASE fileset
 CASE 0 'Heroes
  REDIM buf(318)
  FOR i = 0 TO sets
   FOR j = 0 TO gen(genMaxHero)
    loadherodata buf(), j
    IF buf(17) = i THEN
     poffset(i) = buf(18)
     EXIT FOR
    END IF
   NEXT j
  NEXT i
 CASE 1 TO 3 'Enemies
  REDIM buf(160)
  FOR i = 0 TO sets
   FOR j = 0 TO gen(genMaxEnemy)
    loadenemydata buf(), j
    IF buf(53) = i AND buf(55) + 1 = fileset THEN
     poffset(i) = buf(54)
     EXIT FOR
    END IF
   NEXT j
  NEXT i
 CASE 4 'NPCs
 CASE 5 'Weapons
 CASE 6 'Attacks
 CASE ELSE
   debug "Unknown sprite type: " & fileset
 END SELECT
END SUB

SUB flusharray (array(), size, value)
FOR i = 0 TO size
 array(i) = value
NEXT i
END SUB

SUB loadbinrecord (filename$, recsize, array(), index)

setpicstuf array(), recsize * 2, -1
loadset filename$, index, 0
for i = 0 to recsize
 s$ = s$ & "," & array(i)
next i
END SUB

SUB savebinrecord (filename$, recsize, array(), index)
setpicstuf array(), recsize * 2, -1
storeset filename$, index, 0
END SUB

SUB loadherodata (array(), index)
loadbinrecord (game$ & ".dt0", 318, array(), index)
END SUB

SUB saveherodata (array(), index)
savebinrecord (game$ & ".dt0", 318, array(), index)
END SUB

SUB loadenemydata (array(), index, altfile = 0)
IF altfile THEN
 file$ = workingdir$ + SLASH + "dt1.tmp"
ELSE
 file$ = game$ & ".dt1"
END IF
loadbinrecord (file$, 160, array(), index)
END SUB

SUB saveenemydata (array(), index, altfile = 0)
IF altfile THEN
 file$ = workingdir$ + SLASH + "dt1.tmp"
ELSE
 file$ = game$ & ".dt1"
END IF
savebinrecord (file$, 160, array(), index)
END SUB
