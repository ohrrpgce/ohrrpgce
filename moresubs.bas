'OHRRPGCE GAME - Various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
DECLARE FUNCTION rangel% (n&, r%)
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION str2lng& (stri$)
DECLARE SUB innRestore (stat%())
DECLARE SUB renamehero (who%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE SUB fadein (force%)
DECLARE SUB fadeout (red%, green%, blue%, force%)
DECLARE SUB safekill (f$)
DECLARE SUB loadtemppage (page%)
DECLARE SUB savetemppage (page%)
DECLARE SUB calibrate ()
DECLARE FUNCTION settingstring% (searchee$, setting$, result$)
DECLARE SUB writejoysettings ()
DECLARE SUB writescriptvar (id%, newval%)
DECLARE FUNCTION readscriptvar% (id%)
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION gethighbyte% (n%)
DECLARE FUNCTION readbadbinstring$ (array%(), offset%, maxlen%, skipword%)
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB getLongName (filename$, outfile$)
DECLARE SUB vishero (stat%())
DECLARE SUB sellstuff (id%, storebuf%(), stock%(), stat%())
DECLARE SUB buystuff (id%, shoptype%, storebuf%(), stock%(), stat%())
DECLARE SUB textfatalerror (e$)
DECLARE SUB playtimer ()
DECLARE FUNCTION averagelev% (stat%())
DECLARE FUNCTION countitem% (it%)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION movdivis% (xygo%)
DECLARE FUNCTION onwho% (w$, alone)
DECLARE SUB minimap (mx%, my%, x%, y%, tastuf%())
DECLARE SUB heroswap (iAll%, stat%())
DECLARE FUNCTION shoption (inn%, price%, needf%, stat%())
DECLARE SUB savegame (slot%, map%, foep%, stat%(), stock%())
DECLARE FUNCTION runscript% (n%, index%, newcall%, er$)
DECLARE SUB scripterr (e$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE SUB itstr (i%)
DECLARE FUNCTION findhero% (who%, f%, l%, d%)
DECLARE FUNCTION howmanyh% (f%, l%)
DECLARE FUNCTION consumeitem% (index%)
DECLARE FUNCTION istag% (num%, zero%)
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION usemenu% (pt%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
DECLARE FUNCTION browse$ (fmask$, needf%, bpage%)
DECLARE SUB doswap (s%, d%, stat%())
DECLARE SUB control ()
DECLARE FUNCTION picksave% (load%)
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB getitem (getit%, num%)
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%())
DECLARE SUB spells (pt%, stat%())
DECLARE SUB status (pt%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
DECLARE SUB resetlmp (slot%, lev%)
DECLARE SUB loadfoe (i%, formdata%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), stat%(), ebits%(), batname$())
DECLARE FUNCTION inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%())
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE SUB addhero (who%, slot%, stat%())
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION xstring% (s$, x%)
DECLARE SUB snapshot ()
DECLARE FUNCTION maplumpname$ (map, oldext$)
DECLARE FUNCTION exptolevel& (level%)

'--CD playing (not compiled in yet)
'DECLARE FUNCTION drivelist (l())
'DECLARE FUNCTION getupc (BYVAL dnum, upc$)
'DECLARE FUNCTION audioinfo (BYVAL dnum, track&())
'DECLARE SUB playaudio (BYVAL dnum, BYVAL ad&, BYVAL ln&)
'DECLARE SUB stopcd (BYVAL dnum)
'DECLARE FUNCTION playinfo (BYVAL dnum, cd())
'DECLARE FUNCTION getCDvol ()
'DECLARE SUB setcdvol (BYVAL v)

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'gglobals.bi'
'$INCLUDE: 'const.bi'
'$INCLUDE: 'scrconst.bi'
'$INCLUDE: 'uigame.bi'

REM $STATIC
SUB addhero (who, slot, stat())
DIM wbuf(100), thishbits(4)

'--load hero's data
setpicstuf buffer(), 636, -1
loadset game$ + ".dt0" + CHR$(0), who - 1, 0

'--load data of hero's default weapon
setpicstuf wbuf(), 200, -1
loadset game$ + ".itm" + CHR$(0), buffer(22), 0

'--do average level enforcement
IF buffer(21) < 0 THEN buffer(21) = averagelev(stat())

'--formally add hero
hero(slot) = who

'---MUST SET DEFAULT EQUIP---
wep = large(wbuf(48), 1)
FOR i = 0 TO 4
 eqstuf(slot, i) = 0
NEXT i
eqstuf(slot, 0) = buffer(22) + 1

'--fill in stats
FOR i = 0 TO 11
 stat(slot, 0, i) = atlevel(buffer(21), buffer(23 + i * 2), buffer(24 + i * 2)) + wbuf(54 + i)
 stat(slot, 1, i) = stat(slot, 0, i)
NEXT i
'--weapon picture and palette
stat(slot, 0, 13) = wbuf(52)
stat(slot, 1, 13) = wbuf(53)

'--weapon attack
bmenu(slot, 0) = wep

'--clear spell lists
FOR i = 1 TO 5
 bmenu(slot, i) = 0
NEXT i

'--include spell lists that have names
o = 1
FOR i = 0 TO 3
 IF buffer(243 + i * 11) > 0 THEN bmenu(slot, o) = (i + 1) * -1: o = o + 1
NEXT i

'--add item list to the end
bmenu(slot, o) = -10

'--put spells in spell list
FOR i = 0 TO 3
 FOR o = 0 TO 23
  spell(slot, i, o) = 0
  IF buffer(47 + (i * 48) + (o * 2)) > 0 AND buffer(48 + (i * 48) + (o * 2)) - 1 <= buffer(21) AND buffer(48 + (i * 48) + (o * 2)) > 0 THEN spell(slot, i, o) = buffer(47 + (i * 48) + (o * 2))
 NEXT o
NEXT i

'--elemental bitsets
FOR i = 0 TO 2
 thishbits(i) = buffer(240 + i)
 nativehbits(slot, i) = buffer(240 + i)
NEXT i

'--reset levelmp
resetlmp slot, buffer(21)

'--setup experience
stat(slot, 0, 12) = buffer(21)
stat(slot, 1, 12) = 0
exlev&(slot, 0) = 0
exlev&(slot, 1) = exptolevel(buffer(21))

'--heros are added unlocked
setbit hmask(), 0, who - 1, 0

'--appearance settings
stat(slot, 0, 14) = buffer(17)'bat pic
stat(slot, 0, 15) = buffer(18)'bat pal
stat(slot, 1, 14) = buffer(19)'walk pic
stat(slot, 1, 15) = buffer(20)'walk pal
stat(slot, 0, 16) = buffer(22) + 1'default weapon

'--read hero's name (doing this last because it clobbers the buffer)
names$(slot) = readbadbinstring$(buffer(), 0, 16, 0)
'--if renaming is permitted, do it
IF readbit(thishbits(), 0, 24) THEN
 '--add-hero rename is allowed
 renamehero slot
END IF

END SUB

SUB aquiretempdir

'--use program dir for temp dir
tmpdir$ = STRING$(envlength("TEMP"), 0): getstring tmpdir$
'DEBUG debug "aquired temp dir "+tmpdir$
IF NOT isdir(tmpdir$ + CHR$(0)) THEN
 '--fall back to working dir if all else fails
 tmpdir$ = STRING$(rpathlength, 0): getstring tmpdir$
 'DEBUG debug "Invalid temp dir. fall back to " + tmpdir$
END IF

IF RIGHT$(tmpdir$, 1) <> SLASH THEN tmpdir$ = tmpdir$ + SLASH

END SUB

FUNCTION atlevel (now, a0, a99)

'CLS : a = 80: b = 8500: PRINT : FOR i = 0 TO 99 STEP 5: PRINT i; " "; atlevel(i, a, b): LINE (640, i)-(640 - atlevel(i, a, b) / 100, i), 4: NEXT i

'atlevel = (.8 + now / 50) * now * ((a99 - a0) / 100) + a0 + .1
IF now < 0 THEN atlevel = 0: EXIT FUNCTION
atlevel = (.8 + now / 50) * now * ((a99 - a0) / 275.222) + a0 + .1

END FUNCTION

FUNCTION averagelev (stat())
average = 0
count = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN average = average + stat(i, 0, 12): count = count + 1
NEXT i
IF count > 0 THEN average = average / count
averagelev = average
END FUNCTION

FUNCTION browse$ (fmask$, needf, bpage%)
browse$ = ""

DIM drive(26), drive$(26), tree$(255), treec(255), true$(255), about$(255), catfg(6), catbg(6)

IF needf = 1 THEN
 FOR i = 0 TO 767
  buffer(i) = 0
 NEXT i
 buffer(24) = 5
 buffer(25) = 5
 buffer(26) = 5
 setpal buffer()
END IF

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
IF RIGHT$(remember$, 1) <> SLASH THEN remember$ = remember$ + SLASH
nowdir$ = remember$

GOSUB context

treeptr = 0
treetop = 0

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(treeptr, treetop, 0, treesize, 16)
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
    nowdir$ = nowdir$ + tree$(treeptr) + SLASH
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
 edgeprint nowdir$, 8, 6, uilook(uiText), dpage
 rectangle 0, 190, 320, 10, 8, dpage
 rectangle 4, 175, 312, 14, 9, dpage
 rectangle 5, 176, 310, 12, 1, dpage
 edgeprint about$(treeptr), 8, 178, uilook(uiText), dpage
 edgeprint version$, 8, 190, uilook(uiMenuItem), dpage
 textcolor uilook(uiText), 0
 printstr ">", 0, 20 + (treeptr - treetop) * 9, dpage
 FOR i = treetop TO small(treetop + 16, treesize)
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
 IF needf = 1 THEN fadein -1: needf = 0: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
EXIT FUNCTION

context:
DIM timeout AS DOUBLE
timeout = TIMER
rectangle 5, 176, 310, 12, 1, vpage
meter = 0
treesize = 0
IF nowdir$ = "" THEN
 ' FOR i = 0 TO drivetotal - 1
 '  tree$(i) = drive$(i)
 '  treec(i) = 0
 ' NEXT i
 ' treesize = drivetotal - 1
ELSE
 GOSUB drawmeter
 'tree$(treesize) = "[ROOT]"
 'treec(treesize) = 4
 a$ = nowdir$
 IF LINUX THEN
  treesize = -1
 ELSE ' Windows and DOS
  a = ASC(LEFT$(nowdir$, 1)) - 64
  FOR i = 0 TO drivetotal - 1
   'IF a = drive(i) THEN tree$(treesize) = drive$(i)
   IF a = drive(i) THEN
    tree$(treesize) = CHR$(64 + drive(i)) + ":" + SLASH
    GOSUB drawmeter
   END IF
  NEXT i
  treec(treesize) = 0
  about$(treesize) = "Drive"
  a$ = RIGHT$(a$, LEN(a$) - 3)
 END IF
 b$ = ""
 DO UNTIL a$ = ""
  b$ = b$ + LEFT$(a$, 1)
  a$ = RIGHT$(a$, LEN(a$) - 1)
  IF RIGHT$(b$, 1) = SLASH THEN
   treesize = small(treesize + 1, limit)
   tree$(treesize) = b$
   treec(treesize) = 1
   about$(treesize) = "Directory"
   b$ = ""
   GOSUB drawmeter
  END IF
 LOOP
 '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
 findfiles nowdir$ + ALLFILES + CHR$(0), 16, tmpdir$ + "hrbrowse.tmp" + CHR$(0), buffer()
 fh = FREEFILE
 OPEN tmpdir$ + "hrbrowse.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  treesize = small(treesize + 1, limit)
  treec(treesize) = 2
  about$(treesize) = "Subdirectory"
  LINE INPUT #fh, tree$(treesize)
  IF tree$(treesize) = "." OR tree$(treesize) = ".." OR RIGHT$(tree$(treesize), 4) = ".tmp" THEN treesize = treesize - 1
  IF tree$(treesize) = "" THEN treesize = treesize - 1
  GOSUB drawmeter
 LOOP
 CLOSE #fh
 safekill tmpdir$ + "hrbrowse.tmp"
 '---FIND ALL FILES IN FILEMASK---
 findfiles nowdir$ + fmask$ + CHR$(0), 0, tmpdir$ + "hrbrowse.tmp" + CHR$(0), buffer()
 fh = FREEFILE
 OPEN tmpdir$ + "hrbrowse.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  treesize = small(treesize + 1, limit)
  treec(treesize) = 3
  LINE INPUT #fh, true$(treesize)
  true$(treesize) = true$(treesize)
  IF true$(treesize) = "" THEN
   treesize = treesize - 1
  ELSE
   IF timeout + 15 > TIMER THEN
    unlumpfile nowdir$ + true$(treesize) + CHR$(0), "browse.txt", tmpdir$, buffer()
    IF isfile(tmpdir$ + "browse.txt" + CHR$(0)) THEN
     setpicstuf buffer(), 40, -1
     loadset tmpdir$ + "browse.txt" + CHR$(0), 0, 0
     tree$(treesize) = STRING$(bound(buffer(0), 0, 38), " ")
     array2str buffer(), 2, tree$(treesize)
     loadset tmpdir$ + "browse.txt" + CHR$(0), 1, 0
     about$(treesize) = STRING$(bound(buffer(0), 0, 38), " ")
     array2str buffer(), 2, about$(treesize)
     safekill tmpdir$ + "browse.txt"
     IF LEN(tree$(treesize)) = 0 THEN tree$(treesize) = true$(treesize)
    ELSE
     tree$(treesize) = true$(treesize)
     about$(treesize) = ""
    END IF
   ELSE
    tree$(treesize) = true$(treesize)
    about$(treesize) = ""
   END IF
  END IF
  GOSUB drawmeter
 LOOP
 CLOSE #fh
 safekill tmpdir$ + "hrbrowse.tmp"
END IF

'--get longnames for display
FOR i = 0 TO treesize
 SELECT CASE treec(i)
  CASE 2, 3, 6
   IF tree$(i) = true$(i) THEN
    getLongName nowdir$ + tree$(i), tree$(i)
   END IF
 END SELECT
NEXT i

'--alphabetize
meter = 0
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
 GOSUB drawmeter2
NEXT o

'--sort by type
meter = 0
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
treetop = bound(treetop, treeptr - 19, treeptr)

widest = 0
FOR i = 0 TO treesize
 IF LEN(tree$(i)) > widest THEN widest = LEN(tree$(i))
NEXT i

RETURN

drawmeter:
meter = meter + 1: rectangle 5 + meter, 177, 2, 5, 9, vpage
setvispage vpage 'refresh
RETURN

drawmeter2:
meter = meter + 1: rectangle 5 + meter, 186, 2, 2, 9, vpage
setvispage vpage 'refresh
RETURN

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

SUB calibrate

state = 0
state$ = "Center Joystick and Press Button"
midx = 400
midy = 400
button = 0
disabled = 10

setkeys
DO
 setwait timing(), speedcontrol
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
   IF NOT disabled THEN
    writejoysettings
    EXIT DO
   END IF
 END SELECT
 centerbox 160, 100, 100, 80, 1, dpage
 centerbox 160, 100, 20, 20, 3, dpage
 IF state > 0 THEN
  centerbox 160 + (joy(0) - midx) * .1, 100 + (joy(1) - midy) * .1, 10, 10, 15, dpage
 END IF
 IF state > 0 AND state < 5 THEN
  edgeprint "This way!", tx - 36, ty - 5, uilook(uiDescription), dpage
 END IF
 edgeprint "Calibrate Joystick", 88, 8, uilook(uiText), dpage
 edgeprint state$, 160 - 4 * LEN(state$), 174, uilook(uiSelectedItem + tog), dpage
 jpos$ = "X=" + LTRIM$(STR$(joy(0))) + " Y=" + LTRIM$(STR$(joy(1)))
 edgeprint jpos$, 160 - 4 * LEN(jpos$), 184, uilook(uiSelectedItem + tog), dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

END SUB

FUNCTION consumeitem (index)
'--subtracts one of an item at a location. If the item is depleted, returns true. If there are some of the item left, it returns false
consumeitem = 0
lb = (item(index) AND 255)
hb = INT(item(index) / 256) - 1
item(index) = lb + (hb * 256)
item$(index) = LEFT$(item$(index), 9) + RIGHT$(STR$(hb), 2)
IF hb = 0 THEN item(index) = 0: item$(index) = "           ": consumeitem = -1
END FUNCTION

FUNCTION countitem (it)
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

SUB cycletile (cycle(), tastuf(), pt(), skip())

FOR i = 0 TO 1
 IF NOT istag(tastuf(1 + 20 * i), 0) THEN
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
     CASE 6
      IF istag(tastuf(11 + 20 * i + pt(i)), 0) THEN
       pt(i) = loopvar(pt(i), 0, 8, 1)
      ELSE
       pt(i) = 0
       cycle(i) = 0
      END IF
     CASE ELSE
      pt(i) = loopvar(pt(i), 0, 8, 1)
    END SELECT
    notstuck = large(notstuck - 1, 0)
   LOOP WHILE notstuck AND skip(i) = 0
  END IF
 END IF
NEXT i

END SUB

SUB debug (s$)
fh = FREEFILE
OPEN "g_debug.txt" FOR APPEND AS #fh
PRINT #fh, s$
CLOSE #fh
END SUB

SUB delitem (it, num)
FOR o = 0 TO 199
 lb = (item(o) AND 255)
 hb = INT(item(o) / 256)
 IF it = lb AND hb > 0 THEN
  IF hb <= num THEN
   num = num - hb
   hb = 0
   lb = 0
  ELSE
   hb = hb - num
   num = 0
  END IF
  item(o) = lb + (hb * 256)
  itstr o
  IF num = 0 THEN EXIT FOR
 END IF
NEXT o
END SUB

SUB doswap (s, d, stat())

'---swap hmask (bitsets which tell which heros are locked)
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
 FOR o = 0 TO 23
  SWAP spell(s, i, o), spell(d, i, o)
 NEXT o
NEXT i

'---hero stats
FOR i = 0 TO 1
 FOR o = 0 TO 16
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
SWAP names$(s), names$(d)

'---Equipment
FOR i = 0 TO 4
 SWAP eqstuf(s, i), eqstuf(d, i)
NEXT i

'---elemental bitsets
FOR i = 0 TO 4
 SWAP nativehbits(s, i), nativehbits(d, i)
NEXT i

'---reload hero pictures and palettes
vishero stat()

'hero(40), bmenu(40,5), spell(40,3,23), stat(40,1,13), lmp(40,7), exlev&(40,1), names$(40), eqstuf(40,4)
END SUB

SUB drawsay (saybit(), sayenh(), say$(), showsay, choose$(), choosep)
IF readbit(saybit(), 0, 1) = 0 THEN
 IF readbit(saybit(), 0, 2) = 0 THEN
  centerfuz 160, 48 + (sayenh(0) * 4) - (sayenh(1) * 2), 312, 88 - (sayenh(1) * 4), sayenh(3) + 1, dpage
 ELSE
  centerbox 160, 48 + (sayenh(0) * 4) - (sayenh(1) * 2), 312, 88 - (sayenh(1) * 4), sayenh(3) + 1, dpage
 END IF '---TO FUZZ OR NOT TO FUZZ?-----
END IF
col = uilook(uiText): IF sayenh(2) > 0 THEN col = sayenh(2)
FOR i = 0 TO 8 - showsay
 edgeprint say$(i), 7, (8 + i * 10) + (sayenh(0) * 4), col, dpage
NEXT i
IF showsay > 1 THEN showsay = showsay - 1
IF readbit(saybit(), 0, 0) THEN
 tempy = 100 + (sayenh(0) * 4) - (sayenh(1) * 4)
 IF tempy > 160 THEN tempy = 20
 centerbox 160, tempy + 12, 10 + large(LEN(choose$(0)) * 8, LEN(choose$(1)) * 8), 24, sayenh(3) + 1, dpage
 FOR i = 0 TO 1
  col = uilook(uiMenuItem): IF choosep = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint choose$(i), xstring(choose$(i), 160), tempy + 2 + (i * 10), col, dpage
 NEXT i
END IF
END SUB

SUB edgeprint (s$, x, y, c, p)
textcolor uilook(uiOutline), 0
printstr s$, x, y + 1, p
printstr s$, x + 1, y, p
printstr s$, x + 2, y + 1, p
printstr s$, x + 1, y + 2, p
textcolor c, 0
printstr s$, x + 1, y + 1, p
END SUB

SUB evalherotag (stat())

leader = -1
FOR i = 3 TO 0 STEP -1
 IF hero(i) > 0 THEN leader = hero(i) - 1
NEXT i

FOR i = 0 TO large(gen(35), 59) '--for each available hero
 FOR j = 0 TO 3
  IF herobits(i, j) > 1 THEN setbit tag(), 0, herobits(i, j), 0
 NEXT j
 FOR j = 0 TO 40
  IF hero(j) - 1 = i THEN
   IF herobits(i, 0) > 1 THEN setbit tag(), 0, herobits(i, 0), 1 '---HAVE HERO
   IF herobits(i, 1) > 1 AND stat(j, 0, 0) THEN setbit tag(), 0, herobits(i, 1), 1 '---IS ALIVE
   IF herobits(i, 2) > 1 AND i = leader THEN setbit tag(), 0, herobits(i, 2), 1 '---IS LEADER
   IF herobits(i, 3) > 1 AND j < 4 THEN setbit tag(), 0, herobits(i, 3), 1 '---IN PARTY
  END IF
 NEXT j
NEXT i

'--this may not be the best place to do this, but here we make sure that you do not have an all-dead party
'falive = -1
'fhave = -1
'FOR i = 3 TO 0 STEP -1
' IF hero(i) > 0 THEN
'  fhave = i
'  IF stat(i, 1, 0) > 0 THEN falive = i
' END IF
'NEXT i
'IF falive = -1 THEN
' stat(fhave, 1, 0) = 1
'END IF

'292     have hero tag
'293     is alive tag
'294     is leader tag
'295     is in active party tag
END SUB

SUB evalitemtag

FOR i = 0 TO 255
 'clear all four bits
 FOR j = 0 TO 3
  IF itembits(i, j) > 1 THEN setbit tag(), 0, itembits(i, j), 0
 NEXT j
NEXT i

'search inventory slots
FOR j = 0 TO 199
 'get item ID
 id = (item(j) AND 255) - 1
 IF id >= 0 THEN 'there is an item in this slot
  IF itembits(id, 0) > 1 THEN setbit tag(), 0, itembits(id, 0), 1 'you have it
  IF itembits(id, 1) > 1 THEN setbit tag(), 0, itembits(id, 1), 1 'it is in your inventory
 END IF
NEXT j
 
FOR j = 0 TO 40 'search hero list
 FOR k = 0 TO 4 'search equipment slots
  id = eqstuf(j, k) - 1
  IF id >= 0 THEN ' there is an item equipped in this slot
   IF itembits(id, 0) > 1 THEN setbit tag(), 0, itembits(id, 0), 1 'you have it
   IF itembits(id, 2) > 1 THEN setbit tag(), 0, itembits(id, 2), 1 'it is equipped
   IF j < 4 AND itembits(id, 3) > 1 THEN setbit tag(), 0, itembits(id, 3), 1   'it is equipped by an active hero
  END IF
 NEXT k
NEXT j

'itembits(n,0)      when have tag
'itembits(n,1)      is in inventory
'itembits(n,2)      is equiped tag
'itembits(n,3)      is equiped by hero in active party

END SUB

SUB fadein (force)
fadestate = 1
fadetopal master(), buffer()
END SUB

SUB fadeout (red, green, blue, force)
fadestate = 0
fadeto buffer(), red, green, blue
END SUB

SUB fatalerror (e$)

setvispage 0
centerbox 160, 100, 300, 180, 3, 0
edgeprint e$, xstring(e$, 160), 20, uilook(uiText), 0
edgeprint "Press ESC to cleanly close the program", 15, 40, uilook(uiMenuItem), 0
edgeprint "or any other key to ignore the", 15, 50, uilook(uiMenuItem), 0
edgeprint "error and try to continue playing.", 15, 60, uilook(uiMenuItem), 0

w = getkey

IF w = 1 THEN
 '--close digital audio file
 'closefile
 '--close current BAM file
 closemusic
 '--reset FM synth chip
 'dummy = resetfm
 '--replace Mode-X with previous screen mode
 restoremode
 '--display error message
 PRINT e$
 '--crash out
 SYSTEM
END IF

END SUB

FUNCTION findhero (who, f, l, d)
result = -1
FOR i = f TO l STEP d
 IF hero(i) = who OR (who = -1 AND hero(i)) THEN result = i: EXIT FOR
NEXT i
findhero = result
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
 fh = FREEFILE
 OPEN game$ + ".stt" FOR BINARY AS #fh
 max = 32
 FOR i = 0 TO max
  temp$ = CHR$(0)
  GET #fh, 1 + (11 * i), temp$
  temp = 0: IF temp$ <> "" THEN temp = small(ASC(temp$), 10)
  stat$(i) = STRING$(temp, CHR$(0))
  GET #fh, 2 + (11 * i), stat$(i)
 NEXT i
 CLOSE #fh
END IF
END SUB

SUB heroswap (iAll%, stat())

'Page 2 has the npcs, which don't need to be reloaded afterward
'Page 3 holds a copy of vpage.
savetemppage 3
copypage dpage, 3

DIM swindex(40), swname$(40)

swapme = -1
ecsr = -1

GOSUB resetswap

IF hero(acsr) THEN info$ = names$(acsr) ELSE info$ = ""

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(5) > 1 THEN
  IF swapme >= 0 THEN
   swapme = -1
  ELSE
   loadtemppage 3
   FOR t = 4 TO 5: carray(t) = 0: NEXT t
   EXIT SUB
  END IF
 END IF
 IF iAll THEN
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
  IF hero(acsr) AND ecsr < 0 THEN info$ = names$(acsr) ELSE info$ = ""
 END IF
 IF carray(3) > 1 AND ecsr < 0 THEN
  acsr = loopvar(acsr, 0, 3, 1)
  IF hero(acsr) AND ecsr < 0 THEN info$ = names$(acsr) ELSE info$ = ""
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
    doswap temp, temp2, stat()
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
IF hero(acsr) AND ecsr < 0 THEN info$ = names$(acsr) ELSE info$ = ""
RETURN

'---DRAWS SWAP MENU AND CURRENT SELECTION----
showswapmenu:
centerbox 160, 66, 130, 38, 1, dpage
o = 0
FOR i = 0 TO 3
 IF i = swapme OR hero(i) > 0 THEN rectangle 105 + (30 * i), 60, 20, 20, uilook(uiTextBox), dpage
 IF hero(i) THEN
  loadsprite buffer(), 0, 200 * 4, o * 5, 20, 20, 2
  drawsprite buffer(), 0, pal16(), o * 16, 105 + (30 * i), 60 + (i = swapme) * 6, dpage
  o = o + 1
 END IF
NEXT i
IF ecsr < 0 THEN edgeprint CHR$(24), 111 + 30 * acsr, 52, uilook(uiSelectedItem + tog), dpage
IF iAll THEN
 centerbox 160, 100 + small(high, 8) * 5, wide * 8 + 16, small(high, 8) * 10 + 10, 1, dpage
 FOR i = top TO small(top + 7, la)
  'Some of the colours are a bit bizarre, here, especially the time bar stuff below
  c = uilook(uiMenuItem)
  IF swapme = i + 4 THEN c = uilook(uiDisabledSelection) '6
  IF ecsr = i THEN
   c = uilook(uiSelectedItem + tog)
   IF swapme = i + 4 THEN c = uilook(uiDisabledSelection + tog) '6 + 8 * tog
  END IF
  IF swapme > -1 AND swapme < 4 THEN
   IF (numhero < 2 AND i = la) OR readbit(hmask(), 0, acsr) THEN c = uilook(uiTimeBar + ((ecsr = i) * tog)) '8 + ((ecsr = i) * tog)
  END IF
  edgeprint swname$(i), xstring(swname$(i), 160), 100 + (i - top) * 10, c, dpage
 NEXT i
END IF
IF LEN(info$) THEN
 centerbox 160, 44, (LEN(info$) + 2) * 8, 14, 1, dpage
 edgeprint info$, xstring(info$, 160), 39, uilook(uiText), dpage
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
  swname$(la) = names$(i)
  wide = large(wide, LEN(swname$(la)))
 END IF
NEXT i
la = la + 1
FOR i = 40 TO 4 STEP -1
 IF hero(i) = 0 THEN
  swindex(la) = i
  swname$(la) = readglobalstring$(48, "-REMOVE-", 10)
  wide = large(wide, 7)
 END IF
NEXT i
high = small(8, la + 1)
numhero = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  numhero = numhero + 1
 END IF
NEXT i
IF hero(acsr) AND ecsr < 0 THEN info$ = names$(acsr) ELSE info$ = ""
RETURN
END SUB

FUNCTION howmanyh (f, l)
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
capper& = str2lng&(n$)
IF capper& > 32767 THEN capper& = 32767
n = capper&
IF s THEN n = n * s
'CLIPBOARD
IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip = n
IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN n = clip
n = large(min, n)
n = small(max, n)
END SUB

FUNCTION istag (num, zero)
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
'--clobbers buffer!
IF gen(95) < 2 THEN
 '--obsolete doors
ELSE
 '--THE RIGHT WAY--
 setpicstuf buffer(), 600, -1
 loadset game$ + ".dox" + CHR$(0), map, 0
 FOR i = 0 TO 99
  door(i) = buffer(i)
  door(100 + i) = buffer(100 + i)
  setbit door(), 200, i, buffer(200 + i)
 NEXT i
END IF
END SUB

SUB loadgame (slot, map, foep, stat(), stock())

'--return gen to defaults
xbload game$ + ".gen", gen(), "General data is missing from " + game$

sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
setpicstuf buffer(), 30000, -1
loadset sg$ + CHR$(0), slot * 2, 0

version = buffer(0)
IF version < 2 OR version > 3 THEN EXIT SUB
map = buffer(1)
catx(0) = buffer(2)
caty(0) = buffer(3)
catd(0) = buffer(4)
foep = buffer(5)
'leader = buffer(6)
mapx = buffer(7)
mapy = buffer(8)

temp$ = ""
FOR i = 0 TO 24
 IF buffer(i + 9) < 0 OR buffer(i + 9) > 255 THEN buffer(i + 9) = 0
 IF buffer(i + 9) > 0 THEN temp$ = temp$ + CHR$(buffer(i + 9))
NEXT i
gold& = str2lng&(temp$)

z = 34
FOR i = 0 TO 500
 SELECT CASE i
  CASE 42, 44 TO 54, 56 TO 92
   gen(i) = buffer(z)
 END SELECT
 z = z + 1
NEXT i

DeserNPCL npc(),z,buffer(),300

FOR i = 0 TO 126
 tag(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 40
 hero(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 500
 '--used to be the useless a() buffer
 dummy = buffer(z): z = z + 1
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
  FOR j = 0 TO 23
   spell(i, o, j) = buffer(z): z = z + 1
  NEXT j
  z = z + 1'--skip extra data
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
  exlev&(i, o) = str2lng&(temp$)
 NEXT o
NEXT i
FOR i = 0 TO 40
 temp$ = ""
 FOR j = 0 TO 16
  IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
  IF buffer(z) > 0 THEN temp$ = temp$ + CHR$(buffer(z))
  z = z + 1
 NEXT j
 names$(i) = temp$
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
 catx(i * 5) = buffer(z): z = z + 1
 caty(i * 5) = buffer(z): z = z + 1
 catd(i * 5) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 1024
 global(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 21
 veh(i) = buffer(z): z = z + 1
NEXT i
'--picture and palette
picpalmagicnum = buffer(z): z = z + 1
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 14 TO 16
   IF picpalmagicnum = 4444 THEN stat(i, o, j) = buffer(z)
   z = z + 1
  NEXT j
 NEXT o
NEXT i
'native hero bitsets
nativebitmagicnum = buffer(z): z = z + 1
FOR i = 0 TO 40
 FOR o = 0 TO 4
  IF nativebitmagicnum = 4444 THEN nativehbits(i, o) = buffer(z)
  z = z + 1
 NEXT o
NEXT i

'---BLODDY BACKWARD COMPATABILITY---
'fix doors...
IF version = 2 THEN gen(95) = 3

IF picpalmagicnum <> 4444 THEN
 '--fix appearance settings
 FOR sl = 0 TO 40
  IF hero(sl) > 0 THEN
   setpicstuf buffer(), 636, -1
   loadset game$ + ".dt0" + CHR$(0), hero(sl) - 1, 0
   stat(sl, 0, 14) = buffer(17)'bat pic
   stat(sl, 0, 15) = buffer(18)'bat pal
   stat(sl, 1, 14) = buffer(19)'walk pic
   stat(sl, 1, 15) = buffer(20)'walk pal
   stat(sl, 0, 16) = buffer(22) + 1'default weapon
  END IF
 NEXT sl
END IF

IF nativebitmagicnum <> 4444 THEN
 '--fix native hero bits
 FOR sl = 0 TO 40
  IF hero(sl) > 0 THEN
   setpicstuf buffer(), 636, -1
   loadset game$ + ".dt0" + CHR$(0), hero(sl) - 1, 0
   FOR i = 0 TO 4
    nativehbits(sl, i) = buffer(240 + i)
   NEXT i
  END IF
 NEXT sl
END IF

'ALL THE STUFF THAT MUST BE SAVED
'map,x,y,d,foep,gold&,gen(500),npcl(2100),tag(126),hero(40),stat(40,1,13),bmenu(40,5),spell(40,3,23),lmp(40,7),exlev&(40,1),names$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4)
'ALL THE STUFF THAT MUST BE PASSED
'slot,map,x,y,d,foep,gold&,stat(),bmenu(),spell(),lmp(),exlev&(),item(),item$()
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

SUB minimap (mx, my, x, y, tastuf())

'loadpage game$ + ".til" + CHR$(0), gmap(0), 3
centerfuz 160, 100, 304, 184, 1, vpage
centerbox 159, 99, scroll(0) + 3, scroll(1) + 3, 15, vpage
setmapdata scroll(), buffer(), 0, 0
abort = 0
setkeys
FOR i = 0 TO scroll(1) - 1
 setkeys
 playtimer
 control
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
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 i = 1: DO
 IF keyval(i) > 1 OR carray(4) > 1 OR carray(5) > 1 THEN
  setkeys
  FOR i = 0 TO 7: carray(i) = 0: NEXT i
  EXIT SUB
 END IF
 i = i + 1: LOOP UNTIL i > 88
 rectangle 160 - (scroll(0) * .5) + (x / 20), 100 - (scroll(1) * .5) + (y / 20), 1, 1, uilook(uiSelectedItem + tog), dpage '15 + (tog * 5), dpage
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

FUNCTION onwho (w$, alone)

'-- pre-select the first hero
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  w = i
  EXIT FOR
 END IF
NEXT i

'-- if there is only one hero, return immediately
'--unless we are in alone-mode
IF alone = 0 AND howmanyh(0, 3) <= 1 THEN onwho = w: setkeys: EXIT FUNCTION

copypage dpage, vpage
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 wtg = loopvar(wtg, 0, 3, 1)
 IF carray(5) > 1 THEN
  onwho = -1: EXIT DO
 END IF
 IF carray(2) > 1 THEN DO: w = loopvar(w, 0, 3, -1): LOOP UNTIL hero(w) > 0
 IF carray(3) > 1 THEN DO: w = loopvar(w, 0, 3, 1): LOOP UNTIL hero(w) > 0
 IF carray(4) > 1 THEN onwho = w: EXIT DO
 centerbox 160, 100, 140, 52, 1, dpage
 o = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   wt = 0: IF w = i THEN wt = INT(wtg / 2)
   loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
   drawsprite buffer(), 0, pal16(), o * 16, 100 + i * 30, 100, dpage
   o = o + 1
  END IF
 NEXT i
 edgeprint CHR$(25), 106 + w * 30, 90, uilook(uiSelectedItem + tog), dpage
 edgeprint w$, xstring(w$, 160), 80, uilook(uiText), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

setkeys
flusharray carray(), 7, 0

END FUNCTION

FUNCTION range (n, r)
a = (n / 100) * r
range = n + INT(RND * (a * 2)) - a
END FUNCTION

FUNCTION rangel (n&, r)
a = (n& / 100) * r
rangel = n& + INT(RND * (a * 2)) - a
END FUNCTION

SUB readjoysettings

IF isfile(progdir$ + "joyset.ini" + CHR$(0)) THEN
 '--use joyset.ini
 fh = FREEFILE
 OPEN progdir$ + "joyset.ini" FOR INPUT AS #fh
 safety = 0
 DO WHILE NOT EOF(fh) AND safety < 100
  LINE INPUT #fh, a$
  IF settingstring(a$, "UPTHRESH", n$) THEN
   joy(9) = str2int(n$)
  END IF
  IF settingstring(a$, "DOWNTHRESH", n$) THEN
   joy(10) = str2int(n$)
  END IF
  IF settingstring(a$, "LEFTTHRESH", n$) THEN
   joy(11) = str2int(n$)
  END IF
  IF settingstring(a$, "RIGHTTHRESH", n$) THEN
   joy(12) = str2int(n$)
  END IF
  IF settingstring(a$, "USEBUTTON", n$) THEN
   joy(13) = bound(str2int(n$) + 2, 2, 3)
  END IF
  IF settingstring(a$, "MENUBUTTON", n$) THEN
   joy(14) = bound(str2int(n$) + 2, 2, 3)
  END IF
  safety = safety + 1
 LOOP
 CLOSE #fh
 '--wait a little to make sure the buttons clear
 setwait timing(), speedcontrol
 dowait
ELSE
 '--no joyset.ini file, must recalibrate
 calibrate
END IF

END SUB

FUNCTION readscriptvar (id)

SELECT CASE id
 CASE IS < 0 'local variable
  readscriptvar = heap(scrat(nowscript, scrheap) + ABS(id) - 1)
 CASE 0 TO 1024 'global variable
  readscriptvar = global(id)
 CASE ELSE
  scripterr "Cannot read global" + STR$(id) + ". out of range"
END SELECT

END FUNCTION

SUB reinitnpc (remember, map)
IF remember THEN
'  FOR i = 0 TO 299
'   buffer(i + 0) = npc(i + 0).x
'   buffer(i + 300) = npc(i + 300).y
'   buffer(i + 600) = npc(i + 600).id
'   buffer(i + 900) = npc(i + 900).dir
'   buffer(i + 1500) = npc(i + 1500)
'   buffer(i + 1800) = npc(i + 1800)
'  NEXT i
 
 SerNPCL npc(),0,buffer(),300
END IF
'xbload maplumpname$(map, "l"), npcl(), "Oh No! Map" + LTRIM$(STR$(map)) + " NPC locations are missing"
LoadNPCL maplumpname$(map, "l"), npc(), 300
IF remember THEN
'  FOR i = 0 TO 299
'   npcl(i + 0) = buffer(i + 0)
'   npcl(i + 300) = buffer(i + 300)
'   npcl(i + 600) = buffer(i + 600)
'   npcl(i + 900) = buffer(i + 900)
'   npcl(i + 1500) = buffer(i + 1500)
'   npcl(i + 1800) = buffer(i + 1800)
'  NEXT i
 DeserNPCL npc(),0,buffer(), 300
END IF
END SUB

SUB renamehero (who)

setpicstuf buffer(), 636, -1
loadset game$ + ".dt0" + CHR$(0), hero(who) - 1, 0
limit = buffer(296)
IF limit = 0 THEN limit = 16

prompt$ = readglobalstring$(137, "Name the Hero", 20)
spacer$ = STRING$(large(limit, LEN(names$(who))), " ")
remember$ = names$(who)
rememberjoycal = gen(60)
gen(60) = 1'--disable joystick calibration

copypage dpage, vpage
IF fadestate = 0 THEN
 fadein -1
 needfadeout = 1
END IF

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 playtimer
 control
 centerbox 160, 100, 168, 32, 1, dpage
 IF carray(4) > 1 AND keyval(57) = 0 THEN EXIT DO
 IF carray(5) > 1 THEN names$(who) = remember$
 strgrabber names$(who), limit
 edgeprint prompt$, xstring(prompt$, 160), 90, uilook(uiText), dpage
 textcolor uilook(uiHighlight), uiLook(uiHighlight)
 printstr spacer$, xstring(spacer$, 161), 101, dpage
 edgeprint names$(who), xstring(names$(who), 160), 100, uilook(uiMenuItem), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

gen(60) = rememberjoycal '-- restore joystick calibration setting

IF needfadeout = 1 THEN
 fadeout 0, 0, 0, -1
END IF

END SUB

SUB resetgame (map, foep, stat(), stock(), showsay, scriptout$, sayenh())
map = 0
catx(0) = 0
caty(y) = 0
catd(d) = 0
foep = 0
'leader = 0
mapx = 0
mapy = 0
gold& = 0
showsay = 0
scriptout$ = ""
'--return gen to defaults
xbload game$ + ".gen", gen(), "General data is missing from " + game$

'flusharray npcl(), 2100, 0
CleanNPCL npc(),300
flusharray tag(), 126, 0
flusharray hero(), 40, 0
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 0 TO 16
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
  FOR j = 0 TO 23
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
 names$(i) = ""
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
flusharray hmask(), 3, 0
flusharray global(), 1024, 0
FOR i = 0 TO 128
 FOR o = 0 TO 13
  scrat(i, o) = 0
 NEXT o
NEXT i
flusharray veh(), 21, 0
flusharray sayenh(), 6, 0

FOR i = 0 TO 31
 plotstring$(i) = ""
 plotstrCol(i) = 15
 plotstrBGCol(i) = 0
 plotstrX(i) = 0
 plotstrY(i) = 0
 plotstrBits(i) = 0
NEXT i

xbload game$ + ".mas", master(), "master palette missing from " + game$

'ALL THE STUFF THAT MUST BE RESET
'map,foep,gold&,gen(500),npcl(2100),tag(126),hero(40),stat(40,1,13),bmenu(40,5),spell(40,3,23),lmp(40,7),exlev&(40,1),names$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4)
'30000
END SUB

SUB resetlmp (slot, lev)
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
current = 6
'last added midi music, change shop stuff and song name formats

IF v >= 5 AND v <= current THEN EXIT SUB
needf = 1
clearpage 0
clearpage 1
setvispage 0
centerbox 160, 100, 240, 100, 3, 0
IF v < 5 THEN
 ' Versions older than 5 do not support graceful backwards compatability
 edgeprint "Obsolete RPG File", 52, 70, uilook(uiSelectedItem), 0
 textcolor uilook(uiMenuItem), 0
 printstr "this game was created with", 52, 82, 0
 printstr "an obsolete version of the", 52, 90, 0
 printstr "OHRRPGCE. It may not run", 52, 98, 0
 printstr "as intended.", 52, 106, 0
END IF
IF v > current THEN
 'Versions newer than current cannot support graceful forward compatability
 edgeprint "Unsupported RPG File", 52, 70, uilook(uiText), 0
 textcolor uilook(uiMenuItem), 0
 printstr "this game has features", 52, 82, 0
 printstr "that are not supported in", 52, 90, 0
 printstr "this version of the", 52, 98, 0
 printstr "OHRRPGCE. Download the", 52, 106, 0
 printstr "latest version at", 52, 114, 0
 printstr "http://HamsterRepublic.com", 52, 122, 0
END IF
fadein -1
setvispage 0
w = getkey
fadeout 0, 0, 0, -1
END SUB

FUNCTION runscript (n, index, newcall, er$)
runscript = 1 ' --sucess by default...

IF index > 127 THEN
 scripterr "interpreter overloaded"
 runscript = 0 '--error
 scripterr "failed to load " + er$ + " script" + STR$(n)
 EXIT FUNCTION
END IF

IF newcall AND index > 0 THEN
 IF n = scrat(index - 1, scrid) AND readbit(gen(), 101, 10) = 0 THEN
  'fail quietly
  '--scripterr "script" + STR$(n) + " is already running"
  runscript = 2 '--quiet failure
  EXIT FUNCTION
 END IF
END IF

loadinstead = -1

'-- If we are loading a script that is already running
'-- we can re-use it.
FOR i = 0 TO nowscript
 IF scrat(i, scrid) = n THEN loadinstead = i: EXIT FOR
NEXT i

'-- if the script was the last terminated it can also be reused
IF loadinstead = -1 THEN
 IF scrat(index, scrid) = n AND scrat(index, scroff) = nextscroff AND scrat(index, scrsize) <> 0 THEN loadinstead = index
END IF

'erase state, pointer, return value and depth, set id
scrat(index, scrstate) = 0
scrat(index, scrptr) = 0
scrat(index, scrret) = 0
scrat(index, scrdepth) = 0
scrat(index, scrid) = n

IF loadinstead <> -1 THEN
 '--reuse the script from memory
 IF loadinstead = index THEN
  '--because the reloaded script will be on top of the script stack, we need to recalculate nextscroff
  nextscroff = scrat(index, scroff) + scrat(index, scrsize)
 ELSE
  scrat(index, scrsize) = 0
  scrat(index, scroff) = scrat(loadinstead, scroff)
  scrat(index, scrvars) = scrat(loadinstead, scrvars)
  scrat(index, scrargs) = scrat(loadinstead, scrargs)
 END IF
ELSE
 '--load the script from file
 IF isfile(workingdir$ + SLASH + LTRIM$(STR$(n)) + ".hsx" + CHR$(0)) THEN
  fbdim temp
	
  f = FREEFILE
  OPEN workingdir$ + SLASH + LTRIM$(STR$(n)) + ".hsx" FOR BINARY AS #f
  GET #f, 1, temp
  skip = temp
  GET #f, 3, temp
  scrat(index, scrvars) = temp
  IF skip >= 6 THEN
   GET #f, 5, temp
   scrat(index, scrargs) = temp
  ELSE
   scrat(index, scrargs) = 999
  END IF
  
  IF nextscroff + (LOF(f) - skip) / 2 > 4096 THEN
   scripterr "Script buffer overflow"
   CLOSE #f
   runscript = 0'--error
   scripterr "failed to load " + er$ + " script" + STR$(n)
   EXIT FUNCTION
  END IF
  scrat(index, scroff) = nextscroff
  scrat(index, scrsize) = (LOF(f) - skip) / 2
  nextscroff = nextscroff + scrat(index, scrsize)

  '--mysterious. why can't I do this?
  'bigstring$ = STRING$(LOF(f) - skip, 0)
  'GET #f, 1 + skip, bigstring$
  'str2array bigstring$, script(), scrat(index, scroff)
  FOR i = skip TO LOF(f) - 2 STEP 2
   GET #f, 1 + i, temp
   script(scrat(index, scroff) + ((i - skip) / 2)) = temp
  NEXT i
  CLOSE #f

  '--if any higher scripts have been overwritten, invalidate them
  FOR i = index + 1 TO 127
   IF scrat(i, scrid) = 0 THEN EXIT FOR
   IF nextscroff > scrat(i, scroff) THEN scrat(i, scrid) = 0 ELSE EXIT FOR
  NEXT i
 ELSE
  scripterr "failed to unlump " + LTRIM$(STR$(n)) + ".hsx"
 END IF
END IF

scrat(index + 1, scrheap) = scrat(index, scrheap) + (scrat(index, scrvars) + 1)

IF scrat(index + 1, scrheap) > 2048 THEN
 scripterr "Script heap overflow"
 runscript = 0'--error
 scripterr "failed to load " + er$ + " script" + STR$(n)
 EXIT FUNCTION
END IF

FOR i = 1 TO scrat(index, scrvars)
 heap(scrat(index, scrheap) + (i - 1)) = 0
NEXT i

scrat(index, scrstate) = stread

'--suspend the previous script...Why was I doing this?
IF newcall AND index > 0 THEN
 scrat(index - 1, scrstate) = scrat(index - 1, scrstate) * -1
END IF

'--we are sucessful, so now tis safe to increment this
nowscript = nowscript + 1

END FUNCTION

SUB savegame (slot, map, foep, stat(), stock())

'--FLUSH BUFFER---
FOR i = 0 TO 16000
 buffer(i) = 0
NEXT i

buffer(0) = 3        'SAVEGAME VERSION NUMBER
buffer(1) = map
buffer(2) = catx(0)
buffer(3) = caty(0)
buffer(4) = catd(0)
buffer(5) = foep
buffer(6) = 0    'was leader
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
 IF i <= 104 THEN
  buffer(z) = gen(i)
 ELSE
  buffer(z) = 0
 END IF
 z = z + 1
NEXT i
' FOR i = 0 TO 2100
'  buffer(z) = npcl(i): z = z + 1
' NEXT i
SerNPCL npc(), z, buffer(), 300
FOR i = 0 TO 126
 buffer(z) = tag(i): z = z + 1
NEXT i
FOR i = 0 TO 40
 buffer(z) = hero(i): z = z + 1
NEXT i
FOR i = 0 TO 500
 '--placeholder for old useless a() buffer
 buffer(z) = 0: z = z + 1
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
  FOR j = 0 TO 23
   buffer(z) = spell(i, o, j): z = z + 1
  NEXT j
  z = z + 1
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
 temp$ = names$(i)
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
 buffer(z) = catx(i * 5): z = z + 1
 buffer(z) = caty(i * 5): z = z + 1
 buffer(z) = catd(i * 5): z = z + 1
NEXT i
FOR i = 0 TO 1024
 buffer(z) = global(i): z = z + 1
NEXT i
FOR i = 0 TO 21
 buffer(z) = veh(i): z = z + 1
NEXT i
'--picture and palette
buffer(z) = 4444: z = z + 1 'magic number
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 14 TO 16
   buffer(z) = stat(i, o, j): z = z + 1
  NEXT j
 NEXT o
NEXT i
'--native hero bitsets
buffer(z) = 4444: z = z + 1 'magic number
FOR i = 0 TO 40
 FOR o = 0 TO 4
  buffer(z) = nativehbits(i, o): z = z + 1
 NEXT o
NEXT i

' z = 6513 here

setpicstuf buffer(), 30000, -1
sg$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4) + ".sav"
storeset sg$ + CHR$(0), slot * 2 + 1, 0


'ALL THE STUFF THAT MUST BE SAVED
'map,x,y,d,foep,gold&,gen(500),npcl(2100),tag(126),hero(40),stat(40,1,13),bmenu(40,5),spell(40,3,23),lmp(40,7),exlev&(40,1),names$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4)
'ALL THE STUFF THAT MUST BE PASSED
'slot,map,x,y,d,foep,gold&,stat(),bmenu(),spell(),lmp(),exlev&(),item(),item$()
'30000
END SUB

SUB scripterr (e$)

errormode = 1

SELECT CASE errormode
 CASE 1'--show error on screen
  textcolor uilook(uiText), 0
  clearpage vpage
  setpal master()
  centerbox 160, 20, 310, 30, 3, vpage
  printstr "Script Error!", 108, 10, vpage
  printstr e$, 160 - 4 * LEN(e$), 20, vpage
  setvispage vpage
  w = getkey
 CASE 2'--write error to file
  debug e$
END SELECT

END SUB

SUB scriptmath
SELECT CASE scrat(nowscript, curvalue)
 CASE 0' random
  lowest& = retvals(0)
  highest& = retvals(1)
  scriptret = retvals(0) + INT(RND * (highest& - lowest& + 1))
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
  writescriptvar retvals(0), retvals(1)
 CASE 17'increment
  writescriptvar retvals(0), readscriptvar(retvals(0)) + retvals(1)
 CASE 18'decrement
  writescriptvar retvals(0), readscriptvar(retvals(0)) - retvals(1)
 CASE ELSE
  scripterr "unsupported math"
END SELECT
END SUB

FUNCTION settingstring (searchee$, setting$, result$)

' checks to see if searchee$ begins with setting$=
' if so, sets result$ to the uppercased space-trimmed value that
' follows the = sign and returns true. If not found, returns false

settingstring = 0

IF UCASE$(LEFT$(searchee$, LEN(setting$) + 1)) = setting$ + "=" THEN
 result$ = UCASE$(LTRIM$(RTRIM$(MID$(searchee$, LEN(setting$) + 2, 32))))
 settingstring = -1
END IF

END FUNCTION

SUB shop (id, needf, stock(), stat(), map, foep, mx, my, tastuf())

DIM storebuf(40), menu$(10), menuid(10)

FOR i = 0 TO 7
 menuid(i) = i
NEXT i

menu$(0) = readglobalstring$(70, "Buy", 10)
menu$(1) = readglobalstring$(71, "Sell", 10)
menu$(2) = readglobalstring$(73, "Hire", 10)
menu$(3) = readglobalstring$(72, "Inn", 10)
menu$(4) = readglobalstring$(63, "Equip", 10)
menu$(5) = readglobalstring$(66, "Save", 10)
menu$(6) = readglobalstring$(68, "Map", 10)
menu$(7) = readglobalstring$(65, "Team", 10)

GOSUB initshop
IF last = -1 THEN EXIT SUB
IF last = 0 THEN autopick = 1
last = last + 1: menu$(last) = readglobalstring$(74, "Exit", 10)

GOSUB repaintback

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(0) > 1 THEN pt = large(pt - 1, 0)
 IF carray(1) > 1 THEN pt = small(pt + 1, last)
 IF carray(5) > 1 THEN EXIT DO
 IF carray(4) > 1 OR autopick THEN
  IF pt = last THEN EXIT DO
  IF menuid(pt) = 0 THEN '--BUY
   buystuff id, 0, storebuf(), stock(), stat()
  END IF
  IF menuid(pt) = 1 THEN '--SELL
   sellstuff id, storebuf(), stock(), stat()
  END IF
  IF menuid(pt) = 2 THEN '--HIRE
   buystuff id, 1, storebuf(), stock(), stat()
  END IF
  IF menuid(pt) = 6 THEN '--MAP
   loadpage game$ + ".til" + CHR$(0), gmap(0), 3
   minimap mx, my, catx(0), caty(0), tastuf()
  END IF
  IF menuid(pt) = 7 THEN '--TEAM
   heroswap 1, stat()
  END IF
  IF menuid(pt) = 4 THEN '--EQUIP
   w = onwho(readglobalstring$(108, "Equip Who?", 20), 0)
   IF w >= 0 THEN
    equip w, stat()
   END IF
  END IF
  IF menuid(pt) = 5 THEN '--SAVE
   temp = picksave(0)
   IF temp >= 0 THEN savegame temp, map, foep, stat(), stock()
   vishero stat()
  END IF
  IF menuid(pt) = 3 THEN '--INN
   inn = 0
   IF shoption(inn, storebuf(18), needf, stat()) THEN
    IF inn = 0 THEN
     innRestore stat()
    END IF
    IF storebuf(19) > 0 THEN
     '--Run animation for Inn
     rsr = runscript(storebuf(19), nowscript + 1, -1, "inn")
     IF rsr = 1 THEN
      EXIT DO
     END IF
    ELSE
     '--Inn has no script, do simple fade
     fadeout 0, 0, 20, 0
     needf = 1
    END IF
   END IF
  END IF
  IF autopick THEN EXIT SUB
  GOSUB repaintback
 END IF
 h = (last + 2) * 10
 centerbox 160, 104 + (h * .5), 96, h, 1, dpage
 centerbox 160, 90, LEN(sn$) * 8 + 8, 16, 1, dpage
 edgeprint sn$, xstring(sn$, 160), 85, uilook(uiText), dpage
 FOR i = 0 TO last
  c = uilook(uiMenuItem): IF pt = i THEN c = uilook(uiSelectedItem + tog)
  edgeprint menu$(i), xstring(menu$(i), 160), 109 + i * 10, c, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 IF needf = 1 THEN needf = 0: fadein 0: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
FOR t = 4 TO 5: carray(t) = 0: NEXT t
EXIT SUB

repaintback:
loadpage game$ + ".til" + CHR$(0), gmap(0), 3
setmapdata scroll(), buffer(), 0, 0
drawmap mapx, mapy, 0, dpage
copypage dpage, 3
RETURN

initshop:
setpicstuf storebuf(), 40, -1
loadset game$ + ".sho" + CHR$(0), id, 0
sn$ = readbadbinstring$(storebuf(), 0, 15, 0)
o = 0: last = -1
FOR i = 0 TO 7
 IF readbit(storebuf(), 17, i) THEN
  SWAP menu$(i), menu$(o)
  SWAP menuid(i), menuid(o)
  last = o
  o = o + 1
 END IF
NEXT i
RETURN
END SUB

FUNCTION shoption (inn, price, needf, stat())
DIM menu$(1), sname$(40)

savetemppage 3
copypage dpage, 3

shoption = 0

getnames sname$()

menu$(0) = readglobalstring$(49, "Pay", 10)
menu$(1) = readglobalstring$(50, "Cancel", 10)
inncost$ = readglobalstring$(143, "THE INN COSTS", 20)
youhave$ = readglobalstring$(145, "You have", 20)
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(5) > 1 THEN inn = 1: EXIT DO
 IF carray(0) > 1 OR carray(1) > 1 OR carray(2) > 1 OR carray(3) > 1 THEN inn = inn XOR 1
 IF carray(4) > 1 THEN
  IF inn = 0 AND gold& >= price THEN
   gold& = gold& - price
   shoption = -1
   EXIT DO
  END IF
  IF inn = 1 THEN EXIT DO
 END IF
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   col = uilook(uiText)
   edgeprint names$(i), 128 - LEN(names$(i)) * 8, 5 + i * 10, col, dpage
   edgeprint RIGHT$(STR$(stat(i, 0, 0)), LEN(STR$(stat(i, 0, 0))) - 1) + "/" + RIGHT$(STR$(stat(i, 1, 0)), LEN(STR$(stat(i, 1, 0))) - 1), 136, 5 + i * 10, col, dpage
  END IF
 NEXT i
 centerfuz 160, 90, 200, 60, 1, dpage
 rectangle 130, 92, 60, 22, uilook(uiHighlight), dpage 'orig colour 20
 edgeprint inncost$ + STR$(price) + " " + sname$(32), 160 - LEN(inncost$ + STR$(price) + " " + sname$(32)) * 4, 70, uilook(uiText), dpage
 edgeprint youhave$ + STR$(gold&) + " " + sname$(32), 160 - LEN(youhave$ + STR$(gold&) + " " + sname$(32)) * 4, 80, uilook(uiText), dpage
 FOR i = 0 TO 1
  col = uilook(uiMenuItem): IF inn = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint menu$(i), 160 - LEN(menu$(i)) * 4, 94 + i * 8, col, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 IF needf = 1 THEN needf = 0: fadein 0: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
loadtemppage 3

END FUNCTION

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

SUB snapshot
pre$ = LEFT$(sourcerpg$, LEN(sourcerpg$) - 4)

a$ = pre$
WHILE LEN(a$) > 0 AND RIGHT$(a$, 1) <> SLASH
 a$ = LEFT$(a$, LEN(a$) - 1)
WEND

pre$ = LEFT$(pre$, LEN(a$) + 6)

n = 0

DO
 n$ = RIGHT$(STR$(n), LEN(STR$(n)) - 1)
 shot$ = pre$ + n$ + ".bmp"
 IF isfile(shot$ + CHR$(0)) = 0 THEN EXIT DO
 n = n + 1
LOOP UNTIL n > 99

screenshot shot$ + CHR$(0), vpage, master(), buffer()

IF isfile(shot$ + CHR$(0)) THEN
 fh = FREEFILE
 OPEN shot$ FOR BINARY AS #fh
 a$ = CHR$(0)
 PUT #fh, 48, a$
 PUT #fh, 52, a$
 CLOSE #fh
END IF

END SUB

SUB tagdisplay
STATIC pt, top
DIM buf(20)

pt = large(pt, 0)

IF keyval(74) > 1 OR keyval(12) > 1 THEN
 '--minus
 IF keyval(29) > 0 THEN
  setbit tag(), 0, pt, 0
 ELSE
  pt = large(pt - 1, 0)
 END IF
END IF
IF keyval(78) > 1 OR keyval(13) > 1 THEN
 '--plus
 IF keyval(29) > 0 THEN
  setbit tag(), 0, pt, 1
 ELSE
  pt = small(pt + 1, 1999)
 END IF
END IF

top = bound(top, pt - 4, pt)

setpicstuf buf(), 42, -1
fuzzyrect 0, 0, 208, 50, uilook(uiOutline), dpage
FOR i = top TO top + 4
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
 c = uilook(uiDisabledItem)
 IF istag(i, 0) THEN c = uilook(uiHighlight) 'hmm
 'c = 8 + (-7 * istag(i, 0))
 edgeprint temp$, 8, (i - top) * 10, c, dpage
 IF i = pt THEN edgeprint "->", 0, (i - top) * 10, uilook(uiText), dpage
NEXT i

END SUB

SUB textfatalerror (e$)

'--show error
PRINT e$

'--crash out
SYSTEM

END SUB

FUNCTION unlumpone (lumpfile$, onelump$, asfile$)
unlumpone = 0

unlumpfile lumpfile$ + CHR$(0), onelump$, tmpdir$, buffer()

IF isfile(tmpdir$ + onelump$ + CHR$(0)) THEN
 copyfile tmpdir$ + onelump$ + CHR$(0), asfile$ + CHR$(0), buffer()
 KILL tmpdir$ + onelump$
 unlumpone = -1
END IF

f = FREEFILE

END FUNCTION

SUB writejoysettings
fh = FREEFILE
OPEN progdir$ + "joyset.ini" FOR OUTPUT AS #fh
PRINT #fh, "#Joystick/gamepad configuration"
PRINT #fh, "UPTHRESH=" + LTRIM$(STR$(joy(9)))
PRINT #fh, "DOWNTHRESH=" + LTRIM$(STR$(joy(10)))
PRINT #fh, "LEFTTHRESH=" + LTRIM$(STR$(joy(11)))
PRINT #fh, "RIGHTTHRESH=" + LTRIM$(STR$(joy(12)))
PRINT #fh, "USEBUTTON=" + LTRIM$(STR$(joy(13) - 2))
PRINT #fh, "MENUBUTTON=" + LTRIM$(STR$(joy(14 - 2)))
CLOSE #fh
END SUB

SUB writescriptvar (id, newval)

SELECT CASE id
 CASE IS < 0 'local variable
  heap(scrat(nowscript, scrheap) + ABS(id) - 1) = newval
 CASE 0 TO 1024 'global variable
  global(id) = newval
 CASE ELSE
  scripterr "Cannot write global" + STR$(id) + ". out of range"
END SELECT

END SUB

FUNCTION xstring (s$, x)
xstring = small(large(x - LEN(s$) * 4, 0), 319 - LEN(s$) * 8)
END FUNCTION

FUNCTION getdisplayname$ (default$)
 '--Get game's display name
 f$ = workingdir$ + SLASH + "browse.txt"
 IF isfile(f$ + CHR$(0)) THEN
  setpicstuf buffer(), 40, -1
  loadset f$ + CHR$(0), 0, 0
  s$ = STRING$(bound(buffer(0), 0, 38), " ")
  array2str buffer(), 2, s$
  IF LEN(s$) > 0 THEN
  	getdisplayname$ = s$
  	EXIT FUNCTION
  END IF
 END IF
 getdisplayname$ = default$
END FUNCTION
