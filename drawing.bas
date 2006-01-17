'OHRRPGCE CUSTOM - Mostly drawing-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION charpicker$ ()
DECLARE SUB writepassword (p$)
DECLARE FUNCTION readpassword$ ()
DECLARE SUB fixfilename (s$)
DECLARE FUNCTION filenum$ (n%)
DECLARE FUNCTION readattackname$ (index%)
DECLARE SUB copymapblock (buf%(), sx%, sy%, sp%, dx%, dy%, dp%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION getLongName$ (filename$)
DECLARE FUNCTION pal16browse% (curpal%, usepic%, picx%, picy%, picw%, pich%, picpage%)
DECLARE FUNCTION changepal% (oldval%, newval%, workpal%(), aindex%)
DECLARE SUB storepal16 (array%(), aoffset%, foffset%)
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE SUB smnemonic (tagname$, index%)
DECLARE SUB loadpasdefaults (array%(), tilesetnum%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE SUB standardmenu (menu$(), size%, vis%, ptr%, top%, x%, y%, dpage%, edge%)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE FUNCTION scriptname$ (num%, f$)
DECLARE SUB airbrush (x%, y%, d%, m%, c%, p%)
DECLARE SUB ellipse (x%, y%, radius%, c%, p%, squish1%, squish2%)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION needaddset (ptr%, check%, what$)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB writescatter (s$, lhold%, start%)
DECLARE SUB readscatter (s$, lhold%, start%)
DECLARE FUNCTION browse$ (special, default$, fmask$, tmp$)
DECLARE SUB cycletile (cycle%(), tastuf%(), ptr%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE SUB debug (s$)
DECLARE SUB setanimpattern (tastuf%(), taset%)
DECLARE FUNCTION intstr$ (n%)
DECLARE SUB loadtanim (n%, tastuf%())
DECLARE SUB savetanim (n%, tastuf%())
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%())
DECLARE SUB bitset (array%(), wof%, last%, name$())
DECLARE FUNCTION mouseover% (mouse%(), zox%, zoy%, zcsr%, area%())
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB formation (song$())
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata (atkdat$(), atklim%())
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage (song$())
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (master%(), font())
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)

'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'cglobals.bi'

'$INCLUDE: 'const.bi'

REM $STATIC
SUB airbrush (x, y, d, m, c, p)
'airbrush thanks to Ironhoof (Russel Hamrick)

'AirBrush this rutine works VERY well parameters as fallows:
' AIRBRUSH x , y , diameter , mist_amount , color , page
' diameter sets the width & hight by square radius
' mist_amount sets how many pixels to place i put 100 and it ran fast so
' it works EXCELLENTLY with a mouse on the DTE =)

DO WHILE count < m
 x2 = RND * d
 y2 = RND * d
 r = d \ 2
 x3 = x - r
 y3 = y - r
 IF ABS((x3 + x2) - x) ^ 2 + ABS((y3 + y2) - y) ^ 2 < r ^ 2 THEN
  putpixel x3 + x2, y3 + y2, c, p
 END IF
count = count + 1: LOOP

END SUB

FUNCTION changepal (oldval, newval, workpal(), aindex)

storepal16 workpal(), aindex, oldval
result = bound(newval, 0, 32767)
getpal16 workpal(), aindex, result

changepal = result

END FUNCTION

SUB copymapblock (buf(), sx, sy, sp, dx, dy, dp)

'buf() is a 20-byte array

FOR i = 0 TO 19
 loadsprite buf(), 0, sx, sy + i, 40, 1, sp
 stosprite buf(), 0, dx, dy + i, dp
NEXT i

END SUB

SUB ellipse (x, y, radius, c, p, squish1, squish2)
'ellipse thankss to Ironhoof (Russel Hamrick)

'works mostly like the normal QB circle but with
'more usefull features
' ELLIPSE x , y , radius , color , page , vertical pull , horizontal pull
'the vertical pull & horizontal pull should be in decimals or whole
'numbers. when both numbers are large it shrinks the circle to fit
'the screen like so ellipse 10,10,25,7,0,25,40 will make the ellispe
'smaller. but if its smaller number is 1 or 0 (same) and the other large 0, 25 it will only bend not shrink the ellipse.

r = radius
b = squish1
b2 = squish2

IF b = 0 THEN b = 1
IF b2 = 0 THEN b2 = 1
'IF b > b2 THEN r = r * b
'IF b < b2 THEN r = r * b2
t = -45
DO
 a# = (3.141593 * t) / 180
 xi# = COS(a#)
 yi# = SIN(a#)
 x2# = x - xi# * r / b
 y2# = y - yi# * r / b2
 IF x2# < 0 THEN x2# = 0
 IF x2# > 319 THEN x2# = 319
 IF y2# < 0 THEN y2# = 0
 IF y2# > 199 THEN y2# = 199
 IF lx# = 0 AND ly# = 0 THEN lx# = x2#: ly# = y2#
 drawline x2#, y2#, lx#, ly#, c, p
 lx# = x2#
 ly# = y2#
 t = t + 4:
 IF t > 360 THEN EXIT DO
LOOP

END SUB

SUB gendata (song$(), master())
STATIC default$
DIM m$(18), max(18), bit$(15), subm$(4), scriptgenof(4)
IF general(61) <= 0 THEN general(61) = 161
IF general(62) <= 0 THEN general(62) = 159
last = 17
m$(0) = "Return to Main Menu"
m$(1) = "Preference Bitsets..."
m$(8) = "Password For Editing..."
m$(9) = "Pick Title Screen..."
m$(10) = "Rename Game..."
m$(12) = "Special PlotScripts..."
m$(15) = "Import New Master Palette..."
max(1) = 1
max(2) = 320
max(3) = 200
max(4) = general(0)
max(5) = 100
max(6) = 100
max(7) = 100
max(8) = 0
max(11) = 32000
max(16) = 255
max(17) = 255
GOSUB loadpass
GOSUB genstr
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(csr, 0, 0, last, 24)
 IF (keyval(28) > 1 OR keyval(57) > 1) THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 1 THEN
   bit$(0) = "Pause on Battle Sub-menus"
   bit$(1) = "Enable Caterpillar Party"
   bit$(2) = "Don't Restore HP on Levelup"
   bit$(3) = "Don't Restore MP on Levelup"
   bit$(4) = "Inns Don't Revive Dead Heroes"
   bit$(5) = "Hero Swapping Always Available"
   bit$(6) = "Hide Ready-meter in Battle"
   bit$(7) = "Hide Health-meter in Battle"
   bit$(8) = "Disable Debugging Keys"
   bit$(9) = "Simulate Old Levelup Bug"
   bit$(10) = "Permit double-triggering of scripts"
   bit$(11) = "Skip title screen"
   bit$(12) = "Skip load screen"
   bit$(13) = "Pause on All Battle Menus"
   bit$(14) = "Disable Hero's Battle Cursor"
   bit$(15) = "Damage is capped at 9999"
   bitset general(), 101, 15, bit$()
  END IF
  IF csr = 9 THEN GOSUB ttlbrowse
  IF csr = 10 THEN GOSUB renrpg
  IF csr = 12 THEN GOSUB specialplot
  IF csr = 15 THEN GOSUB importmaspal
  IF csr = 8 THEN GOSUB inputpasw
 IF csr = 16 THEN
  d$ = charpicker$
  IF d$ <> "" THEN
  general(61) = ASC(d$)
   GOSUB genstr
  END IF
 END IF
 IF csr = 17 THEN
  d$ = charpicker$
  IF d$ <> "" THEN
  general(62) = ASC(d$)
   GOSUB genstr
  END IF
 END IF

 END IF
 IF csr > 1 AND csr <= 4 THEN
  IF intgrabber(general(100 + csr), 0, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr > 4 AND csr < 8 THEN
  IF intgrabber(general(csr - 3), 0, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr = 11 THEN
  IF intgrabber(general(96), 0, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr = 13 THEN
  strgrabber longname$, 38
  GOSUB genstr
 END IF
 IF csr = 14 THEN
  strgrabber aboutline$, 38
  GOSUB genstr
 END IF
 IF csr = 16 THEN
  IF intgrabber(general(61), 32, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr = 17 THEN
  IF intgrabber(general(62), 32, max(csr), 75, 77) THEN GOSUB genstr
 END IF

 standardmenu m$(), last, 22, csr, 0, 0, 0, dpage, 0
 
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
GOSUB savepass
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

genstr:
'IF general(101) = 0 THEN m$(1) = "Active Menu Mode" ELSE m$(1) = "Wait Menu Mode"
m$(2) = "Starting X" + STR$(general(102))
m$(3) = "Starting Y" + STR$(general(103))
m$(4) = "Starting Map" + STR$(general(104))
m$(5) = "Title Music:"
IF general(2) = 0 THEN m$(5) = m$(5) + " -none-" ELSE m$(5) = m$(5) + STR$(general(2) - 1) + " " + song$(general(2) - 1)
m$(6) = "Battle Victory Music:"
IF general(3) = 0 THEN m$(6) = m$(6) + " -none-" ELSE m$(6) = m$(6) + STR$(general(3) - 1) + " " + song$(general(3) - 1)
m$(7) = "Default Battle Music:"
IF general(4) = 0 THEN m$(7) = m$(7) + " -none-" ELSE m$(7) = m$(7) + STR$(general(4) - 1) + " " + song$(general(4) - 1)
m$(11) = "Starting Money:" + STR$(general(96))
m$(13) = "Long Name:" + longname$
m$(14) = "About Line:" + aboutline$
m$(16) = "Poison Indicator " + STR$(general(61)) + " " + CHR$(general(61))
m$(17) = "Stun Indicator " + STR$(general(62)) + " " + CHR$(general(62))
RETURN

ttlbrowse:
setdiskpages buffer(), 200, 0
GOSUB gshowpage
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 IF keyval(72) > 1 AND gcsr = 1 THEN gcsr = 0
 IF keyval(80) > 1 AND gcsr = 0 THEN gcsr = 1
 IF gcsr = 1 THEN
  IF intgrabber(general(1), 0, general(100) - 1, 75, 77) THEN GOSUB gshowpage
 END IF
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF gcsr = 0 THEN RETURN
 END IF
 col = 7: IF gcsr = 0 THEN col = 14 + tog
 edgeprint "Go Back", 1, 1, col, dpage
 col = 7: IF gcsr = 1 THEN col = 14 + tog
 edgeprint CHR$(27) + "Browse" + CHR$(26), 1, 11, col, dpage
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

gshowpage:
loadpage game$ + ".mxs" + CHR$(0), general(1), 2
RETURN

loadpass:
IF general(5) >= 256 THEN
 '--new simple format
 pas$ = readpassword$
ELSE
 '--old scattertable format
 readscatter pas$, general(94), 200
 pas$ = rotascii(pas$, general(93) * -1)
END IF
IF isfile(workingdir$ + "\browse.txt" + CHR$(0)) THEN
 setpicstuf buffer(), 40, -1
 loadset workingdir$ + "\browse.txt" + CHR$(0), 0, 0
 longname$ = STRING$(bound(buffer(0), 0, 38), " ")
 array2str buffer(), 2, longname$
 loadset workingdir$ + "\browse.txt" + CHR$(0), 1, 0
 aboutline$ = STRING$(bound(buffer(0), 0, 38), " ")
 array2str buffer(), 2, aboutline$
END IF
RETURN

savepass:

newpas$ = pas$
writepassword newpas$

'--also write old scattertable format, for backwards
'-- compatability with older versions of game.exe
general(93) = INT(RND * 250) + 1
oldpas$ = rotascii(pas$, general(93))
writescatter oldpas$, general(94), 200

'--write long name and about line
setpicstuf buffer(), 40, -1
buffer(0) = bound(LEN(longname$), 0, 38)
str2array longname$, buffer(), 2
storeset workingdir$ + "\browse.txt" + CHR$(0), 0, 0
buffer(0) = bound(LEN(aboutline$), 0, 38)
str2array aboutline$, buffer(), 2
storeset workingdir$ + "\browse.txt" + CHR$(0), 1, 0
RETURN

renrpg:
oldgame$ = RIGHT$(game$, LEN(game$) - 12)
newgame$ = oldgame$
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 strgrabber newgame$, 8
 fixfilename newgame$
 IF keyval(28) > 1 THEN
  IF oldgame$ <> newgame$ AND newgame$ <> "" THEN
   IF NOT isfile(newgame$ + ".rpg" + CHR$(0)) THEN
    textcolor 10, 0
    printstr "Finding files...", 0, 30, vpage
    findfiles workingdir$ + "\" + oldgame$ + ".*" + CHR$(0), 0, "temp.lst" + CHR$(0), buffer()
    fh = FREEFILE
    OPEN "temp.lst" FOR APPEND AS #fh
    WRITE #fh, "-END OF LIST-"
    CLOSE #fh
    fh = FREEFILE
    OPEN "temp.lst" FOR INPUT AS #fh
    textcolor 10, 0
    printstr "Renaming Lumps...", 0, 40, vpage
    textcolor 15, 2
    DO
     INPUT #fh, temp$
     IF temp$ = "-END OF LIST-" THEN EXIT DO
     printstr " " + RIGHT$(temp$, LEN(temp$) - LEN(oldgame$)) + " ", 0, 50, vpage
     copyfile workingdir$ + "\" + temp$ + CHR$(0), workingdir$ + "\" + newgame$ + RIGHT$(temp$, LEN(temp$) - LEN(oldgame$)) + CHR$(0), buffer()
     KILL workingdir$ + "\" + temp$
    LOOP
    CLOSE #fh
    KILL "temp.lst"
    '--update archinym information lump
    fh = FREEFILE
    IF isfile(workingdir$ + "\archinym.lmp" + CHR$(0)) THEN
     OPEN workingdir$ + "\archinym.lmp" FOR INPUT AS #fh
     LINE INPUT #fh, oldversion$
     LINE INPUT #fh, oldversion$
     CLOSE #fh
    END IF
    OPEN workingdir$ + "\archinym.lmp" FOR OUTPUT AS #fh
    PRINT #fh, newgame$
    PRINT #fh, oldversion$
    CLOSE #fh
    game$ = workingdir$ + "\" + newgame$
   ELSE '---IN CASE FILE EXISTS
    edgeprint newgame$ + " Already Exists. Cannot Rename.", 0, 30, 15, vpage
    w = getkey
   END IF '---END IF OKAY TO COPY
  END IF '---END IF VALID NEW ENTRY
  RETURN
 END IF
 textcolor 7, 0
 printstr "Current Name: " + oldgame$, 0, 0, dpage
 printstr "Type New Name and press ENTER", 0, 10, dpage
 textcolor 14 + tog, 2
 printstr newgame$, 0, 20, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

specialplot:
subcsr = 0
subm$(0) = "Previous Menu"
scriptgenof(1) = 41
scriptgenof(2) = 42
scriptgenof(3) = 57
GOSUB setspecialplotstr
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 dummy = usemenu(subcsr, 0, 0, 3, 24)
 SELECT CASE subcsr
  CASE 0
   IF keyval(57) > 1 OR keyval(28) > 1 THEN EXIT DO
  CASE 1 TO 3
   IF intgrabber(general(scriptgenof(subcsr)), 0, general(43), 75, 77) THEN
    GOSUB setspecialplotstr
   END IF
 END SELECT
 FOR i = 0 TO 3
  col = 7: IF subcsr = i THEN col = 14 + tog
  textcolor col, 0
  printstr subm$(i), 0, i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
RETURN

inputpasw:
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 OR keyval(28) > 1 THEN EXIT DO
 strgrabber pas$, 17
 textcolor 7, 0
 printstr "You can require a password for this", 0, 0, dpage
 printstr "game to be opened in CUSTOM.EXE", 0, 8, dpage
 printstr "This does not encrypt your file, and", 0, 16, dpage
 printstr "should only be considered weak security", 0, 24, dpage
 printstr "PASSWORD", 30, 64, dpage
 IF LEN(pas$) THEN
  textcolor 14 + tog, 1
  printstr pas$, 30, 74, dpage
 ELSE
  printstr "(NONE SET)", 30, 74, dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
RETURN

setspecialplotstr:
subm$(1) = "new-game script: " + scriptname$(general(41), "plotscr.lst")
subm$(2) = "game-over script: " + scriptname$(general(42), "plotscr.lst")
subm$(3) = "load-game script: " + scriptname$(general(57), "plotscr.lst")
RETURN

importmaspal:

clearpage vpage
textcolor 15, 0
printstr "WARNING: Importing a new master palette", 0, 0, vpage
printstr "will change ALL of your graphics!", 48, 8, vpage
w = getkey

f$ = browse$(4, default$, "*.mas", "")
IF f$ <> "" THEN
 '--copy the new palette in
 copyfile f$ + CHR$(0), game$ + ".mas" + CHR$(0), buffer()
 '--patch the header in case it is a corrupt PalEdit header.
 masfh = FREEFILE
 OPEN game$ + ".mas" FOR BINARY AS #masfh
 a$ = CHR$(13)
 PUT #masfh, 2, a$
 a$ = CHR$(0)
 PUT #masfh, 6, a$
 CLOSE #masfh
 '--load the new palette!
 xbload game$ + ".mas", master(), "MAS load error"
 setpal master()
END IF

RETURN

END SUB

SUB importbmp (f$, cap$, count, master())
STATIC default$
DIM menu$(10), pmask(767)

IF count = 0 THEN count = 1
clearpage 0
clearpage 1
clearpage 2
clearpage 3
menu$(0) = "Return to Main Menu"
menu$(1) = CHR$(27) + "Browse" + STR$(ptr) + CHR$(26)
menu$(2) = "Replace current " + cap$
menu$(3) = "Append a new " + cap$
menu$(4) = "Disable palette colors"
GOSUB resetpal
GOSUB showpage

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(29) > 0 AND keyval(14) > 1 THEN
  this = count - 1
  cropafter ptr, this, 3, game$ + f$, -1, 1
  count = this + 1
 END IF
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(csr, 0, 0, 4, 24)
 IF csr = 1 THEN
  IF intgrabber(ptr, 0, count - 1, 75, 77) THEN
   menu$(1) = CHR$(27) + "Browse" + STR$(ptr) + CHR$(26)
   GOSUB showpage
  END IF
 END IF
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 2 THEN
   at = ptr
   srcbmp$ = browse$(3, default$, "*.bmp", "")
   IF srcbmp$ <> "" THEN
    GOSUB bimport
   END IF
   GOSUB showpage
  END IF
  IF csr = 3 AND count < 32767 THEN
   at = count
   srcbmp$ = browse$(3, default$, "*.bmp", "")
   IF srcbmp$ <> "" THEN
    GOSUB bimport
    count = count + 1
   END IF
   GOSUB showpage
  END IF
  IF csr = 4 THEN GOSUB disable
 END IF
 FOR i = 0 TO 4
  col = 7: IF i = csr THEN col = 14 + tog
  edgeprint menu$(i), 1, 1 + 10 * i, col, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

disable:
csr2 = 0
setpal pmask()
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN setpal master(): RETURN
 IF csr2 = 0 THEN
  IF keyval(57) > 1 OR keyval(28) > 1 THEN setpal master(): RETURN
  IF keyval(80) > 1 THEN csr2 = 1: cy = -1
 END IF
 IF csr2 = 1 THEN
  IF keyval(75) > 1 THEN cx = large(cx - 1, 0)
  IF keyval(77) > 1 THEN cx = small(cx + 1, 15)
  IF keyval(80) > 1 THEN cy = small(cy + 1, 15)
  IF keyval(72) > 1 THEN cy = cy - 1: IF cy < 0 THEN cy = 0: csr2 = 0
  IF keyval(57) > 1 OR keyval(28) > 1 THEN
   FOR i = 0 TO 2
    pmask((cy * 16 + cx) * 3 + i) = pmask((cy * 16 + cx) * 3 + i) XOR master((cy * 16 + cx) * 3 + i)
   NEXT i
   setpal pmask()
  END IF
 END IF
 textcolor 7, 0: IF csr2 = 0 THEN textcolor 15 + 225 * tog, 0
 printstr "Previous Menu", 0, 0, dpage
 IF csr2 = 1 THEN rectangle 0 + cx * 10, 8 + cy * 10, 10, 10, 15 + 225 * tog, dpage
 FOR i = 0 TO 15
  FOR o = 0 TO 15
   rectangle 1 + o * 10, 9 + i * 10, 8, 8, i * 16 + o, dpage
  NEXT o
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

resetpal:
xbload game$ + ".mas", pmask(), "internal master palette missing!"
RETURN

bimport:
clearpage 3
setvispage 3
IF at < count THEN loadpage game$ + f$ + CHR$(0), at, 3
bitmap2page pmask(), srcbmp$ + CHR$(0), 3
storepage game$ + f$ + CHR$(0), at, 3
GOSUB resetpal
RETURN

showpage:
setdiskpages buffer(), 200, 0
loadpage game$ + f$ + CHR$(0), ptr, 2
RETURN

END SUB

SUB importsong (song$(), master())
STATIC default$
DIM music(16384)
setupmusic music()
setfmvol getfmvol
clearpage 0
clearpage 1
clearpage 2
clearpage 3
top1 = -1: csr = -1

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF usemenu(csr, top1, -1, 99, 20) THEN
  stopsong
  IF csr > -1 AND song$(csr) <> "" THEN loadsong game$ + "." + intstr$(csr) + CHR$(0)
 END IF
 IF csr > -1 THEN
  strgrabber song$(csr), 30
 END IF
 IF keyval(28) > 1 THEN
  IF csr = -1 THEN EXIT DO
  IF csr > -1 THEN
   stopsong
   sourcesong$ = browse$(1, default$, "*.bam", "")
   IF sourcesong$ <> "" THEN
    copyfile sourcesong$ + CHR$(0), game$ + "." + intstr$(csr) + CHR$(0), buffer()
    'a$ = ""
    'i = 0
    'WHILE LEFT$(a$, 1) <> "\"
    ' i = i + 1
    ' a$ = RIGHT$(sourcesong$, i)
    'WEND
    a$ = getLongName$(sourcesong$)
    a$ = LEFT$(a$, LEN(a$) - 4)
    song$(csr) = a$
   END IF
   IF csr > -1 AND song$(csr) <> "" THEN loadsong game$ + "." + intstr$(csr) + CHR$(0)
  END IF
 END IF
 FOR i = top1 TO top1 + 20
  textcolor 7, 0: IF i = csr THEN textcolor 14 + tog, 0
  temp$ = song$(i)
  IF i > -1 THEN
   IF temp$ = "" THEN temp$ = "-EMPTY SLOT-"
   temp$ = intstr$(i) + ":" + temp$
  END IF
  printstr temp$, 1, 1 + 8 * (i - top1), dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP
clearpage 0
clearpage 1
clearpage 2
clearpage 3
stopsong
closemusic

END SUB

SUB loadpasdefaults (array(), tilesetnum)
'--get file size
'fh = FREEFILE
'OPEN workingdir$ + "\defpass.BIN" FOR BINARY AS #fh
'  size& = LOF(fh)
'CLOSE #fh
flusharray array(), 160, 0
'--load defaults from tile set defaults file
setpicstuf array(), 322, -1
loadset workingdir$ + "\defpass.BIN" + CHR$(0), tilesetnum, 0
'--enforce magic number and filesize
IF array(160) <> 4444 THEN
 flusharray array(), 160, 0
END IF
END SUB

SUB loadtanim (n, tastuf())
setpicstuf tastuf(), 80, -1
loadset game$ + ".tap" + CHR$(0), n, 0
END SUB

SUB maptile (master(), font())
DIM mover(10), cutnpaste(19, 19), mouse(4), area(20, 4), tool$(5), menu$(10), tastuf(40), icon$(5), shortk(5), cursor(5), defaults(160), bitmenu$(10), pastogkey(7)

bitmenu$(0) = "Impassable to the North"
bitmenu$(1) = "Impassable to the East"
bitmenu$(2) = "Impassable to the South"
bitmenu$(3) = "Impassable to the West"
bitmenu$(4) = "A-type vehicle Tile"
bitmenu$(5) = "B-type vehicle Tile"
bitmenu$(6) = "Harm Tile"
bitmenu$(7) = "Overhead Tile"

pastogkey(0) = 72
pastogkey(1) = 77
pastogkey(2) = 80
pastogkey(3) = 75
pastogkey(4) = 30
pastogkey(5) = 48
pastogkey(6) = 35
pastogkey(7) = 24

setdiskpages buffer(), 200, 0

gotm = setmouse(mouse())

mapfile$ = game$ + ".til" + CHR$(0)
canpaste = 0
dcsr = 1
airsize = 5
mist = 10
tool$(0) = "Draw": icon$(0) = CHR$(3): shortk(0) = 32: cursor(0) = 0
tool$(1) = "Box ": icon$(1) = CHR$(4): shortk(1) = 48: cursor(1) = 1
tool$(2) = "Line": icon$(2) = CHR$(5): shortk(2) = 38: cursor(2) = 2
tool$(3) = "Fill": icon$(3) = "F":     shortk(3) = 33: cursor(3) = 3
tool$(4) = "Oval": icon$(4) = "O":     shortk(4) = 24: cursor(4) = 2
tool$(5) = "Air ": icon$(5) = "A":     shortk(5) = 30: cursor(5) = 3
area(0, 0) = 60
area(0, 1) = 0
area(0, 2) = 200
area(0, 3) = 160
area(0, 4) = -1
area(1, 0) = 0
area(1, 1) = 160
area(1, 2) = 320
area(1, 3) = 32
FOR i = 0 TO 5
 area(2 + i, 0) = 4 + i * 9
 area(2 + i, 1) = 32
 area(2 + i, 2) = 8
 area(2 + i, 3) = 8
NEXT i
FOR i = 0 TO 3
 area(12 + i, 0) = 4 + i * 9
 area(12 + i, 1) = 42
 area(12 + i, 2) = 8
 area(12 + i, 3) = 8
NEXT i
area(10, 0) = 8
area(10, 1) = 190
area(10, 2) = 32
area(10, 3) = 10
area(11, 0) = 280
area(11, 1) = 190
area(11, 2) = 32
area(11, 3) = 10
'LESS AIRBRUSH AREA
area(16, 0) = 12
area(16, 1) = 60
area(16, 2) = 8
area(16, 3) = 8
area(16, 4) = 0
'LESS AIRBRUSH MIST
area(17, 0) = 12
area(17, 1) = 76
area(17, 2) = 8
area(17, 3) = 8
area(17, 4) = 0
'MORE AIRBRUSH AREA
area(18, 0) = 36
area(18, 1) = 60
area(18, 2) = 8
area(18, 3) = 8
area(18, 4) = 0
'MORE AIRBRUSH MIST
area(19, 0) = 36
area(19, 1) = 76
area(19, 2) = 8
area(19, 3) = 8
area(19, 4) = 0

bnum = 0: c = 17
tmode = 0: cutfrom = 0
pagenum = -1
top = -1

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(29) > 0 AND keyval(14) > 1 AND pagenum > -1 THEN
  cropafter pagenum, general(33), 3, game$ + ".til", -1, 1
 END IF
 IF keyval(80) > 1 AND pagenum = general(33) AND general(33) < 32767 THEN
  pagenum = pagenum + 1
  IF needaddset(pagenum, general(33), "tile set") THEN
   WHILE pagenum > top + 20: top = top + 1: WEND
   clearpage 3
   storepage mapfile$, pagenum, 3
  END IF
 END IF
 IF usemenu(pagenum, top, -1, general(33), 20) THEN
  IF pagenum = -1 THEN clearpage 3 ELSE loadpage mapfile$, pagenum, 3
 END IF
 IF (keyval(57) > 1 OR keyval(28) > 1) AND pagenum = -1 THEN EXIT DO
 IF (keyval(57) > 1 OR keyval(28) > 1) AND pagenum > -1 THEN GOSUB tilemode
 FOR i = top TO small(top + 20, general(33))
  textcolor 7, 240
  IF pagenum = i THEN textcolor 14 + tog, 240
  IF i < 0 THEN
   printstr "Return to Main Menu", 10, 8 + (i - top) * 8, dpage
  ELSE
   printstr "Tile Set" + STR$(i), 10, 8 + (i - top) * 8, dpage
  END IF
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
clearpage 3
clearpage 2
clearpage 1
clearpage 0
EXIT SUB

savepasdefaults:
'--set magic number
defaults(160) = 4444
'--write defaults into tile set defaults file
setpicstuf defaults(), 322, -1
storeset workingdir$ + "\defpass.BIN" + CHR$(0), pagenum, 0
RETURN

tilemode:
loadpasdefaults defaults(), pagenum
GOSUB tilemodemenu
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN tmode = 0: GOSUB savepasdefaults: RETURN
 'IF keyval(72) > 1 THEN tmode = loopvar(tmode, 0, 4, -1)
 'IF keyval(80) > 1 THEN tmode = loopvar(tmode, 0, 4, 1)
 dummy = usemenu(tmode, 0, 0, 4, 24)
 IF keyval(57) OR keyval(28) > 1 THEN
  SELECT CASE tmode
   CASE 0, 1, 2
    GOSUB tiling
   CASE 3
    GOSUB tileanim
    setkeys
    GOSUB tilemodemenu
   CASE 4
    tmode = 0
    GOSUB savepasdefaults
    RETURN
  END SELECT
 END IF
 FOR i = 0 TO 4
  textcolor 7, 240
  IF tmode = i THEN textcolor 14 + tog, 240
  printstr menu$(i), 10, 8 * (i + 1), dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
tilemodemenu:
menu$(0) = "Draw Tiles"
menu$(1) = "Cut Tiles"
menu$(2) = "Set Default Passability"
menu$(3) = "Define Tile Animation"
menu$(4) = "Cancel"
RETURN

tileanim:
taset = 0
loadtanim pagenum, tastuf()
GOSUB utamenu
menu$(0) = "Previous Menu"
menu$(2) = "Set Animation Range"
menu$(3) = "Set Animation Pattern"
menu$(5) = "Test Animations"
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN savetanim pagenum, tastuf(): RETURN
 IF usemenu(taptr, dummy, 0, 5, 5) THEN GOSUB utamenu
 IF taptr = 1 THEN
  IF intgrabber(taset, 0, 1, 75, 77) THEN GOSUB utamenu
 END IF
 IF taptr = 4 THEN
  IF intgrabber(tastuf(1 + 20 * taset), -999, 999, 75, 77) THEN GOSUB utamenu
 END IF
 IF keyval(57) OR keyval(28) > 1 THEN
  IF taptr = 0 THEN savetanim pagenum, tastuf(): RETURN
  IF taptr = 2 THEN GOSUB setanimrange
  IF taptr = 3 THEN setanimpattern tastuf(), taset
  IF taptr = 5 THEN testanimpattern tastuf(), taset
  
 END IF
 FOR i = 0 TO 5
  textcolor 7, 240
  IF taptr = i THEN textcolor 14 + tog, 240
  printstr menu$(i), 10, 8 * (i + 1), dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
utamenu:
menu$(1) = CHR$(27) + "Animation set" + STR$(taset) + CHR$(26)
menu$(4) = "Disable if Tag#" + intstr$(ABS(tastuf(1 + 20 * taset))) + "=" + onoroff$(tastuf(1 + 20 * taset)) + " (" + lmnemonic(ABS(tastuf(1 + 20 * taset))) + ")"
RETURN

setanimrange:
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 OR keyval(28) > 1 OR keyval(57) > 1 THEN RETURN
 IF keyval(72) > 1 THEN tastuf(0 + 20 * taset) = large(tastuf(0 + 20 * taset) - 16, 0)
 IF keyval(80) > 1 THEN tastuf(0 + 20 * taset) = small(tastuf(0 + 20 * taset) + 16, 112)
 IF keyval(75) > 1 THEN tastuf(0 + 20 * taset) = large(tastuf(0 + 20 * taset) - 1, 0)
 IF keyval(77) > 1 THEN tastuf(0 + 20 * taset) = small(tastuf(0 + 20 * taset) + 1, 112)
 GOSUB drawanimrange
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

drawanimrange:
x = 0: y = 0
FOR i = 0 TO 159
 IF i < tastuf(0 + 20 * taset) OR i > tastuf(0 + 20 * taset) + 47 THEN
  fuzzyrect x * 20, y * 20, 20, 20, 15, dpage
 END IF
 x = x + 1: IF x > 15 THEN x = 0: y = y + 1
NEXT i
RETURN

tiling:
loadpage mapfile$, pagenum, 3
'pick block to draw/import/default
setkeys
DO
 setwait timing(), 120
 setkeys
 copypage 3, dpage
 IF gotm THEN
  readmouse mouse()
 END IF
 IF keyval(1) > 1 THEN storepage mapfile$, pagenum, 3: RETURN
 IF tmode <> 2 OR keyval(29) = 0 THEN
  IF keyval(75) > 0 THEN IF bnum > 0 THEN bnum = bnum - 1: IF gotm THEN mouse(0) = mouse(0) - 20: movemouse mouse(0), mouse(1)
  IF keyval(77) > 0 THEN IF bnum < 159 THEN bnum = bnum + 1: IF gotm THEN mouse(0) = mouse(0) + 20: movemouse mouse(0), mouse(1)
  IF keyval(72) > 0 THEN IF bnum > 15 THEN bnum = bnum - 16: IF gotm THEN mouse(1) = mouse(1) - 20: movemouse mouse(0), mouse(1)
  IF keyval(80) > 0 THEN IF bnum < 144 THEN bnum = bnum + 16: IF gotm THEN mouse(1) = mouse(1) + 20: movemouse mouse(0), mouse(1)
 END IF
 IF gotm THEN
  bnum = INT(mouse(1) / 20) * 16 + INT(mouse(0) / 20)
 END IF
 IF tmode = 2 THEN
  '--pass mode shortcuts
  FOR i = 0 TO 7
   IF keyval(29) > 0 OR i > 3 THEN
    IF keyval(pastogkey(i)) > 1 THEN
     setbit defaults(), bnum, i, readbit(defaults(), bnum, i) XOR 1
    END IF
   END IF
  NEXT i
 END IF
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN GOSUB copy
 IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN GOSUB paste
 IF (keyval(29) > 0 AND keyval(20) > 1) THEN GOSUB transpaste
 bx = bnum AND 15
 by = INT(bnum / 16)
 IF keyval(28) > 1 OR keyval(57) OR mouse(3) > 0 THEN
  setkeys
  IF tmode = 0 THEN GOSUB drawit
  IF tmode = 1 THEN GOSUB tilecut
  IF tmode = 2 THEN
   bitset defaults(), bnum, 7, bitmenu$()
  END IF
 END IF
 IF c < 30 THEN c = c + 1 ELSE c = 17
 tog = tog XOR 1
 IF tmode = 2 THEN
  FOR o = 0 TO 9
   FOR i = 0 TO 15
    IF (defaults(i + o * 16) AND 1) THEN rectangle i * 20, o * 20, 20, 3, 7 + tog, dpage
    IF (defaults(i + o * 16) AND 2) THEN rectangle i * 20 + 17, o * 20, 3, 20, 7 + tog, dpage
    IF (defaults(i + o * 16) AND 4) THEN rectangle i * 20, o * 20 + 17, 20, 3, 7 + tog, dpage
    IF (defaults(i + o * 16) AND 8) THEN rectangle i * 20, o * 20, 3, 20, 7 + tog, dpage
    textcolor 14 + tog, 0
    IF (defaults(i + o * 16) AND 16) THEN printstr "A", i * 20, o * 20, dpage
    IF (defaults(i + o * 16) AND 32) THEN printstr "B", i * 20 + 10, o * 20, dpage
    IF (defaults(i + o * 16) AND 64) THEN printstr "H", i * 20, o * 20 + 10, dpage
    IF (defaults(i + o * 16) AND 128) THEN printstr "O", i * 20 + 10, o * 20 + 10, dpage
   NEXT i
  NEXT o
 END IF
 rectangle bx * 20 + 7, by * 20 + 7, 6, 6, c, dpage
 IF gotm THEN
  textcolor 10 + tog * 5, 0
  printstr CHR$(2), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 dowait
LOOP

'draw large block
drawit:
justpainted = 0
undoptr = 0
allowundo = 0
clearpage 2
FOR i = 0 TO 5
 rectangle 279, 9 + (i * 21), 22, 22, 7, 2
 rectangle 280, 10 + (i * 21), 20, 20, 0, 2
NEXT i
GOSUB refreshbig
textcolor 7, 0
printstr ">", 270, 16 + (undoptr * 21), 2
FOR i = 0 TO 31
 FOR j = 0 TO 7
  rectangle i * 10, j * 4 + 160, 10, 4, j * 32 + i, 2
 NEXT j
NEXT i
'---EDIT BLOCK---
edit:
IF gotm THEN
 omx = mouse(0): omy = mouse(1)
 movemouse remx, remy
END IF
delay = 3
setkeys
DO
 setwait timing(), 120
 setkeys
 IF gotm THEN
  readmouse mouse()
  zcsr = 0
  zone = mouseover(mouse(), zox, zoy, zcsr, area())
 END IF
 tog = tog XOR 1
 delay = large(delay - 1, 0)
 justpainted = large(justpainted - 1, 0)
 IF keyval(1) > 1 THEN
  IF hold THEN
   hold = 0
  ELSE
   IF gotm THEN
    remx = mouse(0): remy = mouse(1)
    movemouse omx, omy
   END IF
   RETURN
  END IF
 END IF
 IF keyval(75) > 0 AND keyval(56) = 0 THEN IF x > 0 THEN x = x - 1: IF zone = 1 THEN mouse(0) = mouse(0) - 10: movemouse mouse(0), mouse(1)
 IF keyval(77) > 0 AND keyval(56) = 0 THEN IF x < 19 THEN x = x + 1: IF zone = 1 THEN mouse(0) = mouse(0) + 10: movemouse mouse(0), mouse(1)
 IF keyval(72) > 0 AND keyval(56) = 0 THEN IF y > 0 THEN y = y - 1: IF zone = 1 THEN mouse(1) = mouse(1) - 8: movemouse mouse(0), mouse(1)
 IF keyval(80) > 0 AND keyval(56) = 0 THEN IF y < 19 THEN y = y + 1: IF zone = 1 THEN mouse(1) = mouse(1) + 8: movemouse mouse(0), mouse(1)
 '---KEYBOARD SHORTCUTS FOR TOOLS------------
 FOR i = 0 TO 5
  IF keyval(shortk(i)) > 1 THEN tool = i: hold = 0: dcsr = cursor(i) + 1
 NEXT i
 '----------
 IF keyval(51) > 1 OR (keyval(56) > 0 AND keyval(75) > 0) THEN IF cc > 0 THEN cc = cc - 1
 IF keyval(52) > 1 OR (keyval(56) > 0 AND keyval(77) > 0) THEN IF cc < 255 THEN cc = cc + 1
 IF keyval(56) > 0 AND keyval(72) > 0 THEN IF cc > 31 THEN cc = cc - 32
 IF keyval(56) > 0 AND keyval(80) > 0 THEN IF cc < 224 THEN cc = cc + 32
 IF keyval(41) > 1 THEN hideptr = hideptr XOR 1
 IF keyval(29) > 0 AND keyval(44) > 1 AND allowundo THEN
  undoptr = loopvar(undoptr, 0, 5, -1)
  GOSUB readundoblock
 END IF
 IF keyval(57) > 0 THEN GOSUB clicktile
 IF keyval(28) > 1 THEN cc = readpixel(bx * 20 + x, by * 20 + y, 3)
 IF keyval(58) > 0 THEN GOSUB scrolltile
 IF gotm THEN
  IF zone = 1 THEN
   x = INT(zox / 10)
   y = INT(zoy / 8)
   IF mouse(2) = 2 THEN cc = readpixel(bx * 20 + x, by * 20 + y, 3)
   IF mouse(2) = 1 THEN GOSUB clicktile
  END IF
  IF zone = 2 THEN
   IF mouse(2) > 0 AND mouse(3) = 1 THEN cc = (INT(zoy / 4) * 32) + INT(zox / 10)
  END IF
  IF zone >= 3 AND zone <= 8 AND mouse(3) = 1 THEN
   tool = zone - 3
   dcsr = cursor(tool) + 1
   hold = 0
  END IF
  IF zone >= 13 AND zone <= 16 AND mouse(3) = 1 THEN GOSUB fliptile
  '--mouse over undo
  IF mouse(0) >= 280 AND mouse(0) < 300 THEN
   FOR i = 0 TO 5
    IF mouse(1) >= (10 + (i * 21)) AND mouse(1) < (30 + (i * 21)) THEN
     IF mouse(3) = 1 AND allowundo THEN
      undoptr = i
      GOSUB readundoblock
     END IF
    END IF
   NEXT i
  END IF
 END IF
 IF tool = 5 THEN '--adjust airbrush
  IF mouse(3) = 1 OR mouse(2) = 1 THEN
   IF zone = 17 THEN airsize = large(airsize - 1, 1)
   IF zone = 19 THEN airsize = small(airsize + 1, 30)
   IF zone = 18 THEN mist = large(mist - 1, 1)
   IF zone = 20 THEN mist = small(mist + 1, 99)
  END IF
  IF keyval(12) > 1 OR keyval(74) > 1 THEN
   IF keyval(29) > 0 THEN
    mist = large(mist - 1, 1)
   ELSE
    airsize = large(airsize - 1, 1)
   END IF
  END IF
  IF keyval(13) > 1 OR keyval(78) > 1 THEN
   IF keyval(29) > 0 THEN
    mist = small(mist + 1, 99)
   ELSE
    airsize = small(airsize + 1, 80)
   END IF
  END IF
 END IF
 IF keyval(14) > 1 OR keyval(26) > 1 OR keyval(27) > 1 THEN GOSUB fliptile
 cy = INT(cc / 32)
 cx = cc AND 31
 IF c < 15 THEN c = c + 1 ELSE c = 1
 rectangle cx * 10 + 4, cy * 4 + 162, 3, 1, c, dpage
 rectangle 60 + x * 10, y * 8, 10, 8, readpixel(bx * 20 + x, by * 20 + y, 3), dpage
 rectangle x * 10 + 64, y * 8 + 3, 3, 2, c, dpage
 IF tool = 5 THEN
  ellipse 64 + x * 10, 3 + y * 8, (airsize * 9) / 2, cc, dpage, 0, 0
 END IF
 SELECT CASE hold
  CASE 1
   rectangle 60 + small(x, hox) * 10, small(y, hoy) * 8, (ABS(x - hox) + 1) * 10, (ABS(y - hoy) + 1) * 8, cc, dpage
  CASE 2
   drawline 65 + x * 10, 4 + y * 8, 65 + hox * 10, 4 + hoy * 8, cc, dpage
  CASE 3
   radius = large(ABS(hox - x), ABS(hoy - y)) * 9
   ellipse 65 + hox * 10, 4 + hoy * 8, radius, cc, dpage, 0, 0
 END SELECT
 textcolor 15, 1
 printstr tool$(tool), 8, 8, dpage
 printstr "Tool", 8, 16, dpage
 printstr "Undo", 274, 1, dpage
 FOR i = 0 TO 5
  t1 = 7: t2 = 8
  IF tool = i THEN t1 = 15: t2 = 7
  IF zone - 3 = i THEN t2 = 6
  textcolor t1, t2
  printstr icon$(i), 4 + i * 9, 32, dpage
 NEXT i
 FOR i = 0 TO 3
  textcolor 7, 8: IF zone = 13 + i THEN textcolor 15, 6
  printstr CHR$(7 + i), 4 + i * 9, 42, dpage
 NEXT i
 IF tool = 5 THEN
  textcolor 7, 0
  printstr "SIZE", 12, 52, dpage
  printstr STR$(airsize), 12, 60, dpage
  printstr "MIST", 12, 68, dpage
  printstr STR$(mist), 12, 76, dpage
  textcolor 7, 8: IF zone = 17 THEN textcolor 15, 6
  printstr CHR$(27), 12, 60, dpage
  textcolor 7, 8: IF zone = 18 THEN textcolor 15, 6
  printstr CHR$(27), 12, 76, dpage
  textcolor 7, 8: IF zone = 19 THEN textcolor 15, 6
  printstr CHR$(26), 36, 60, dpage
  textcolor 7, 8: IF zone = 20 THEN textcolor 15, 6
  printstr CHR$(26), 36, 76, dpage
 END IF
 IF gotm THEN
  c = zcsr
  IF c = -1 THEN
   c = dcsr
   IF hideptr THEN c = -2
  END IF
  textcolor 10 + tog * 5, 0
  printstr CHR$(2 + c), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

clicktile:
IF delay > 0 THEN RETURN
SELECT CASE tool
 CASE 0'---DRAW
  IF justpainted = 0 THEN GOSUB writeundoblock
  justpainted = 3
  putpixel 280 + x, 10 + (undoptr * 21) + y, cc, 2
  rectangle bx * 20 + x, by * 20 + y, 1, 1, cc, 3: rectangle 60 + x * 10, y * 8, 10, 8, cc, 2
 CASE 1'---BOX
  IF mouse(3) > 0 OR keyval(57) > 1 THEN
   IF hold = 1 THEN
    GOSUB writeundoblock
    rectangle small(bx * 20 + x, bx * 20 + hox), small(by * 20 + y, by * 20 + hoy), ABS(x - hox) + 1, ABS(y - hoy) + 1, cc, 3
    GOSUB refreshbig
    hold = 0
   ELSE
    hold = 1
    hox = x
    hoy = y
   END IF
  END IF
 CASE 2'---LINE
  IF mouse(3) > 0 OR keyval(57) > 1 THEN
   IF hold = 2 THEN
    GOSUB writeundoblock
    drawline bx * 20 + x, by * 20 + y, bx * 20 + hox, by * 20 + hoy, cc, 3
    GOSUB refreshbig
    hold = 0
   ELSE
    hold = 2
    hox = x
    hoy = y
   END IF
  END IF
 CASE 3'---FILL
  IF mouse(3) > 0 OR keyval(57) > 1 THEN
   GOSUB writeundoblock
   rectangle 0, 0, 22, 22, 15, dpage
   FOR i = 0 TO 19
    FOR j = 0 TO 19
     rectangle 1 + i, 1 + j, 1, 1, readpixel(bx * 20 + i, by * 20 + j, 3), dpage
    NEXT j
   NEXT i
   paintat 1 + x, 1 + y, cc, dpage, buffer(), 16384
   FOR i = 0 TO 19
    FOR j = 0 TO 19
     rectangle bx * 20 + i, by * 20 + j, 1, 1, readpixel(1 + i, 1 + j, dpage), 3
    NEXT j
   NEXT i
   GOSUB refreshbig
   rectangle 0, 0, 22, 22, 0, dpage
  END IF
 CASE 4'---OVAL
  IF mouse(3) > 0 OR keyval(57) > 1 THEN
   IF hold = 3 THEN
    GOSUB writeundoblock
    radius = large(ABS(hox - x), ABS(hoy - y))
    rectangle 0, 0, 22, 22, 15, dpage
    FOR i = 0 TO 19
     FOR j = 0 TO 19
      rectangle 1 + i, 1 + j, 1, 1, readpixel(bx * 20 + i, by * 20 + j, 3), dpage
     NEXT j
    NEXT i
    ellipse 1 + hox, 1 + hoy, radius, cc, dpage, 0, 0
    FOR i = 0 TO 19
     FOR j = 0 TO 19
      rectangle bx * 20 + i, by * 20 + j, 1, 1, readpixel(1 + i, 1 + j, dpage), 3
     NEXT j
    NEXT i
    GOSUB refreshbig
    hold = 0
   ELSE
    hold = 3
    hox = x
    hoy = y
   END IF
  END IF
 CASE 5'---AIR
  IF justpainted = 0 THEN GOSUB writeundoblock
  justpainted = 3
  rectangle 19, 119, 22, 22, 15, dpage
  FOR i = 0 TO 19
   FOR j = 0 TO 19
    rectangle 20 + i, 120 + j, 1, 1, readpixel(bx * 20 + i, by * 20 + j, 3), dpage
   NEXT j
  NEXT i
  airbrush 20 + x, 120 + y, airsize, mist, cc, dpage
  FOR i = 0 TO 19
   FOR j = 0 TO 19
    rectangle bx * 20 + i, by * 20 + j, 1, 1, readpixel(20 + i, 120 + j, dpage), 3
   NEXT j
  NEXT i
  GOSUB refreshbig
END SELECT
RETURN

scrolltile:
rectangle 0, 0, 20, 20, 0, dpage
shiftx = 0: shifty = 0
IF keyval(72) > 0 THEN shifty = -1
IF keyval(80) > 0 THEN shifty = 1
IF keyval(75) > 0 THEN shiftx = -1
IF keyval(77) > 0 THEN shiftx = 1
FOR i = 0 TO 19
 FOR j = 0 TO 19
  tempx = (i + shiftx + 20) MOD 20
  tempy = (j + shifty + 20) MOD 20
  rectangle tempx, tempy, 1, 1, readpixel(bx * 20 + i, by * 20 + j, 3), dpage
 NEXT j
NEXT i
FOR i = 0 TO 19
 FOR j = 0 TO 19
  rectangle bx * 20 + i, by * 20 + j, 1, 1, readpixel(i, j, dpage), 3
 NEXT j
NEXT i
GOSUB refreshbig
rectangle 0, 0, 20, 20, 0, dpage
RETURN

refreshbig:
copymapblock mover(), bx * 20, by * 20, 3, 280, 10 + (undoptr * 21), 2
rectangle 59, 0, 202, 161, 15, 2
FOR i = 0 TO 19
 FOR j = 0 TO 19
  rectangle 60 + i * 10, j * 8, 10, 8, readpixel(bx * 20 + i, by * 20 + j, 3), 2
 NEXT j
NEXT i
RETURN

writeundoblock:
rectangle 270, 16 + (undoptr * 21), 8, 8, 0, 2
undoptr = loopvar(undoptr, 0, 5, 1)
copymapblock mover(), bx * 20, by * 20, 3, 280, 10 + (undoptr * 21), 2
textcolor 7, 0
printstr ">", 270, 16 + (undoptr * 21), 2
allowundo = 1
RETURN

readundoblock:
FOR j = 0 TO 5
 rectangle 270, 16 + (j * 21), 8, 8, 0, 2
NEXT j
copymapblock mover(), 280, 10 + (undoptr * 21), 2, bx * 20, by * 20, 3
textcolor 7, 0
printstr ">", 270, 16 + (undoptr * 21), 2
GOSUB refreshbig
RETURN

fliptile:
GOSUB writeundoblock
rectangle 0, 0, 20, 20, 0, dpage
flipx = 0: flipy = 0
IF (zone = 13 OR zone = 16) OR keyval(26) > 1 OR (keyval(14) > 1 AND keyval(29) = 0) THEN flipx = 19
IF zone = 14 OR zone = 15 OR keyval(27) > 1 OR (keyval(14) > 1 AND keyval(29) > 0) THEN flipy = 19
FOR i = 0 TO 19
 FOR j = 0 TO 19
  tempx = ABS(i - flipx)
  tempy = ABS(j - flipy)
  IF (zone = 15 OR zone = 16) OR (keyval(26) > 1 OR keyval(27) > 1) THEN SWAP tempx, tempy
  rectangle tempx, tempy, 1, 1, readpixel(bx * 20 + i, by * 20 + j, 3), dpage
 NEXT j
NEXT i
FOR i = 0 TO 19
 FOR j = 0 TO 19
  rectangle bx * 20 + i, by * 20 + j, 1, 1, readpixel(i, j, dpage), 3
 NEXT j
NEXT i
GOSUB refreshbig
rectangle 0, 0, 20, 20, 0, dpage
RETURN

tilecut:
IF gotm THEN
 omx = mouse(0): omy = mouse(1)
 movemouse remx, remy
END IF
delay = 3
clearpage 2
loadpage game$ + ".mxs" + CHR$(0), cutfrom, 2
setkeys
DO
 setwait timing(), 120
 setkeys
 tog = tog XOR 1
 delay = large(delay - 1, 0)
 IF gotm THEN
  readmouse mouse()
  zcsr = 0
  zone = mouseover(mouse(), zox, zoy, zcsr, area())
  cutx = small(mouse(0), 300)
  cuty = small(mouse(1), 180)
 END IF
 IF keyval(1) > 1 THEN
  IF gotm THEN
   remx = mouse(0): remy = mouse(1)
   movemouse omx, omy
  END IF
  RETURN
 END IF
 inc = 1: IF keyval(56) > 0 THEN inc = 20
 IF keyval(72) > 0 THEN cuty = large(cuty - inc, 0): IF gotm THEN movemouse cutx, cuty
 IF keyval(80) > 0 THEN cuty = small(cuty + inc, 180): IF gotm THEN movemouse cutx, cuty
 IF keyval(75) > 0 THEN cutx = large(cutx - inc, 0): IF gotm THEN movemouse cutx, cuty
 IF keyval(77) > 0 THEN cutx = small(cutx + inc, 300): IF gotm THEN movemouse cutx, cuty
 IF keyval(57) > 1 OR keyval(28) > 0 OR (mouse(3) > 0 AND zone < 11) THEN
  IF delay = 0 THEN
   setkeys
   FOR i = 0 TO 19
    FOR j = 0 TO 19
     rectangle bx * 20 + i, by * 20 + j, 1, 1, readpixel(cutx + i, cuty + j, 2), 3
    NEXT j
   NEXT i
   IF gotm THEN
    remx = mouse(0): remy = mouse(1)
    movemouse omx, omy
   END IF
   RETURN
  END IF
 END IF
 '---PICK BACKGROUND PAGE------
 temp = cutfrom
 dummy = intgrabber(cutfrom, 0, general(100) - 1, 51, 52)
 IF zone = 11 AND mouse(3) > 0 THEN cutfrom = loopvar(cutfrom, 0, general(100) - 1, -1)
 IF zone = 12 AND mouse(3) > 0 THEN cutfrom = loopvar(cutfrom, 0, general(100) - 1, 1)
 IF temp <> cutfrom THEN loadpage game$ + ".mxs" + CHR$(0), cutfrom, 2
 '----
 drawline cutx, cuty, cutx + 19, cuty, 10 + tog * 5, dpage
 drawline cutx, cuty, cutx, cuty + 19, 10 + tog * 5, dpage
 drawline cutx + 19, cuty + 19, cutx + 19, cuty, 10 + tog * 5, dpage
 drawline cutx + 19, cuty + 19, cutx, cuty + 19, 10 + tog * 5, dpage
 textcolor 7 + tog, 1
 IF zone = 11 THEN textcolor 14 + tog, 3
 printstr "Prev", 8, 190, dpage
 textcolor 7 + tog, 1
 IF zone = 12 THEN textcolor 14 + tog, 3
 printstr "Next", 280, 190, dpage
 textcolor 15, 1
 printstr STR$(cutfrom) + " ", 160 - LEN(STR$(cutfrom) + " ") * 4, 190, dpage
 IF gotm THEN
  textcolor 10 + tog * 5, 0
  printstr CHR$(2), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

copy:
FOR i = 0 TO 19
 FOR j = 0 TO 19
  cutnpaste(i, j) = readpixel(bx * 20 + i, by * 20 + j, 3)
 NEXT j
NEXT i
canpaste = 1
RETURN

paste:
IF canpaste = 0 THEN RETURN
FOR i = 0 TO 19
 FOR j = 0 TO 19
  rectangle bx * 20 + i, by * 20 + j, 1, 1, cutnpaste(i, j), 3
NEXT j: NEXT i
RETURN

transpaste:
IF canpaste = 0 THEN RETURN
FOR i = 0 TO 19
 FOR j = 0 TO 19
  IF cutnpaste(i, j) THEN rectangle bx * 20 + i, by * 20 + j, 1, 1, cutnpaste(i, j), 3
 NEXT j
NEXT i
RETURN

END SUB

FUNCTION mouseover (mouse(), zox, zoy, zcsr, area())

FOR i = 20 TO 0 STEP -1
 IF area(i, 2) <> 0 AND area(i, 3) <> 0 THEN
  IF mouse(0) >= area(i, 0) AND mouse(0) < area(i, 0) + area(i, 2) THEN
   IF mouse(1) >= area(i, 1) AND mouse(1) < area(i, 1) + area(i, 3) THEN
    zox = mouse(0) - area(i, 0)
    zoy = mouse(1) - area(i, 1)
    zcsr = area(i, 4)
    mouseover = i + 1
    EXIT FUNCTION
   END IF 'Y OKAY---
  END IF 'X OKAY---
 END IF 'VALID ZONE---
NEXT i

END FUNCTION

FUNCTION pal16browse (curpal, usepic, picx, picy, picw, pich, picpage)

DIM pal16(80)

result = curpal
clearpage vpage
setvispage vpage
top = curpal - 1

'--get last pal
setpicstuf buffer(), 16, -1
loadset game$ + ".pal" + CHR$(0), 0, 0
lastpal = buffer(1)
o = 0
FOR i = lastpal TO 0 STEP -1
 loadset game$ + ".pal" + CHR$(0), 1 + i, 0
 FOR j = 0 TO 7
  IF buffer(j) <> 0 THEN o = 1: EXIT FOR
 NEXT j
 IF o = 1 THEN EXIT FOR
 lastpal = i
NEXT i

GOSUB onscreenpals

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF usemenu(curpal, top, -1, 32767, 9) THEN GOSUB onscreenpals
 IF intgrabber(curpal, 0, 32767, 51, 52) THEN
  top = bound(top, curpal - 8, curpal - 1)
  GOSUB onscreenpals
 END IF
 IF keyval(71) > 1 THEN curpal = -1: top = -1: GOSUB onscreenpals
 IF keyval(79) > 1 THEN curpal = lastpal: top = large(-1, lastpal - 8): GOSUB onscreenpals
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF curpal >= 0 THEN result = curpal
  EXIT DO
 END IF
 
 FOR i = 0 TO 9
  textcolor 7, 0
  IF top + i = curpal THEN textcolor 14 + tog, 0
  SELECT CASE top + i
   CASE IS >= 0
    printstr LTRIM$(STR$(top + i)), 4, 5 + i * 20, dpage
    o = LEN(STR$(top + i)) * 8
    IF top + i = curpal THEN
     rectangle o - 1, 1 + i * 20, 114, 18, 7, dpage
     rectangle o, 2 + i * 20, 112, 16, 0, dpage
    END IF
    FOR j = 0 TO 15
     c = pal16(i * 8 + j \ 2)
     IF (j AND 1) = 1 THEN
      c = (c \ 256)
     ELSE
      c = (c AND &HFF)
     END IF
     rectangle o + j * 7, 2 + i * 20, 5, 16, c, dpage
    NEXT j
    IF top + i <> curpal THEN
     FOR k = 0 TO usepic - 1
      loadsprite buffer(), 0, picx + k * (picw * pich \ 2), picy, picw, pich, picpage
      drawsprite buffer(), 0, pal16(), i * 16, o + 140 + (k * picw), i * 20 - (pich \ 2 - 10), dpage
     NEXT k
    END IF
   CASE 0
    printstr "Cancel", 4, 5 + i * 20, dpage
  END SELECT
 NEXT i
 IF curpal >= 0 THEN '--write current pic on top
  o = LEN(STR$(curpal)) * 8
  FOR k = 0 TO usepic - 1
   loadsprite buffer(), 0, picx + k * (picw * pich \ 2), picy, picw, pich, picpage
   drawsprite buffer(), 0, pal16(), (curpal - top) * 16, o + 120 + (k * picw), (curpal - top) * 20 - (pich \ 2 - 10), dpage
  NEXT k
 END IF
 
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
pal16browse = result
EXIT FUNCTION

onscreenpals:
FOR i = 0 TO 9
 getpal16 pal16(), i, top + i
NEXT i
RETURN

END FUNCTION

SUB savetanim (n, tastuf())
setpicstuf tastuf(), 80, -1
storeset game$ + ".tap" + CHR$(0), n, 0
END SUB

SUB setanimpattern (tastuf(), taset)
DIM menu$(12), stuff$(7), llim(7), ulim(7)
menu$(0) = "Previous Menu"
stuff$(0) = "end of animation"
stuff$(1) = "up"
stuff$(2) = "down"
stuff$(3) = "right"
stuff$(4) = "left"
stuff$(5) = "wait"
stuff$(6) = "if tag do rest"
stuff$(7) = "unknown command"
FOR i = 1 TO 2
 llim(i) = 0
 ulim(i) = 9
NEXT i
FOR i = 3 TO 4
 llim(i) = 0
 ulim(i) = 159
NEXT i
llim(5) = 0
ulim(5) = 32767
llim(6) = -999
ulim(6) = 999

GOSUB refreshmenu

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 SELECT CASE context
  CASE 0 '---PICK A STATEMENT---
   IF keyval(1) > 1 THEN EXIT DO
   IF usemenu(ptr, dummy, 0, 9, 9) THEN GOSUB refreshmenu
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    IF ptr = 0 THEN
     EXIT DO
    ELSE
     context = 1
    END IF
   END IF
  CASE 1 '---EDIT THAT STATEMENT---
   IF keyval(1) > 1 OR keyval(28) > 1 OR keyval(57) > 1 THEN context = 0
   dummy = usemenu(ptr2, dummy, 0, 1, 1)
   IF ptr2 = 0 THEN IF intgrabber(tastuf(2 + bound(ptr - 1, 0, 8) + 20 * taset), 0, 6, 75, 77) THEN GOSUB refreshmenu
   IF ptr2 = 1 THEN IF intgrabber(tastuf(11 + bound(ptr - 1, 0, 8) + 20 * taset), llim(tastuf(2 + bound(ptr - 1, 0, 8) + 20 * taset)), ulim(tastuf(2 + bound(ptr - 1, 0, 8) + 20 * taset)), 75, 77) THEN GOSUB refreshmenu
 END SELECT
 FOR i = 0 TO 9
  textcolor 7, 0
  IF i = ptr THEN
   textcolor 14 + tog, 0
  END IF
  IF context = 1 THEN textcolor 8, 0
  printstr menu$(i), 0, i * 8, dpage
 NEXT i
 IF ptr > 0 THEN
  FOR i = 0 TO 1
   textcolor 7, 0
   IF context = 1 AND i = ptr2 THEN
    textcolor 14 + tog, 0
   END IF
   IF context = 0 THEN textcolor 8, 0
   printstr menu$(10 + i), 0, 100 + i * 8, dpage
  NEXT i
 END IF 'ptr > 1
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
GOSUB forcebounds
EXIT SUB

refreshmenu:
GOSUB forcebounds
FOR i = 1 TO 9
 menu$(i) = "-"
NEXT i
menu$(10) = ""
FOR i = 0 TO 8
 a = bound(tastuf((2 + i) + 20 * taset), 0, 7)
 b = tastuf((11 + i) + 20 * taset)
 menu$(i + 1) = stuff$(a)
 IF a = 0 THEN EXIT FOR
 IF a > 0 AND a < 6 THEN menu$(i + 1) = menu$(i + 1) + STR$(b)
 IF a = 6 THEN menu$(i + 1) = menu$(i + 1) + lmnemonic(b)
NEXT i
IF i = 8 THEN menu$(10) = "end of animation"
menu$(10) = "Action=" + stuff$(bound(tastuf(2 + bound(ptr - 1, 0, 8) + 20 * taset), 0, 7))
menu$(11) = "Value="
this = tastuf(11 + bound(ptr - 1, 0, 8) + 20 * taset)
SELECT CASE tastuf(2 + bound(ptr - 1, 0, 8) + 20 * taset)
 CASE 1 TO 4
  menu$(11) = menu$(11) + intstr$(this) + " Tiles"
 CASE 5
  menu$(11) = menu$(11) + intstr$(this) + " Ticks"
 CASE 6
  menu$(11) = menu$(11) + "Tag#" + intstr$(ABS(this)) + "=" + onoroff$(this) + " " + lmnemonic(ABS(this))
 CASE ELSE
  menu$(11) = menu$(11) + "N/A"
END SELECT
RETURN

forcebounds:
FOR i = 0 TO 8
 j = bound(i, 0, 8) + 20 * taset
 tastuf(2 + j) = bound(tastuf(2 + j), 0, 7)
 tastuf(11 + j) = bound(tastuf(11 + j), llim(tastuf(2 + j)), ulim(tastuf(2 + j)))
NEXT i
RETURN

END SUB

SUB sizemar (array(), wide, high, tempx, tempy, tempw, temph, yout, page)
'---FLUSH BUFFER---
edgeprint "Flushing buffer...", 0, yout * 10, 15, page: yout = yout + 1
buffer(0) = tempw
buffer(1) = temph
FOR i = 2 TO 16002
 buffer(i) = 0
NEXT i
'---WRITE RESIZED MAP INTO BUFFER
edgeprint "Resizing Map...", 0, yout * 10, 15, page: yout = yout + 1
FOR i = tempy TO tempy + temph
 FOR o = tempx TO tempx + tempw
  setmapdata array(), array(), 20, 0
  IF o < wide AND i < high THEN
   j = readmapblock(o, i)
  ELSE
   j = 0
  END IF
  setmapdata buffer(), buffer(), 20, 0
  setmapblock o - tempx, i - tempy, j
 NEXT o
NEXT i
'---MOVE BUFFER TO ARRAY---
edgeprint "Refreshing from buffer...", 0, yout * 10, 15, page: yout = yout + 1
FOR i = 0 TO 16002
 array(i) = buffer(i)
NEXT i

END SUB

SUB sprite (xw, yw, sets, perset, soff, foff, atatime, info$(), size, zoom, file$, master(), font())
STATIC default$, clippedpal, clippedw, clippedh, paste
DIM nulpal(8), placer(1600), pclip(8), menu$(255), pmenu$(3), bmpd(40), mouse(4), area(20, 4), tool$(5), icon$(5), shortk(5), cursor(5), workpal(8)

gotm = setmouse(mouse())
GOSUB initmarea
airsize = 5
mist = 10
icsr = 0
itop = 0
dcsr = 1
pmenu$(0) = "Overwrite Current Palette"
pmenu$(1) = "Import Without Palette"
pmenu$(2) = "Cancel Import"
tool$(0) = "Draw": icon$(0) = CHR$(3): shortk(0) = 32: cursor(0) = 0
tool$(1) = "Box ": icon$(1) = CHR$(4): shortk(1) = 48: cursor(1) = 1
tool$(2) = "Line": icon$(2) = CHR$(5): shortk(2) = 38: cursor(2) = 2
tool$(3) = "Fill": icon$(3) = "F":     shortk(3) = 33: cursor(3) = 3
tool$(4) = "Oval": icon$(4) = "O":     shortk(4) = 24: cursor(4) = 2
tool$(5) = "Air ": icon$(5) = "A":     shortk(5) = 30: cursor(5) = 3

DEF SEG = VARSEG(nulpal(0))
FOR i = 0 TO 15
 POKE i, i
NEXT i
offset = 0
getpal16 workpal(), 0, offset

FOR j = 0 TO 0 + atatime
 GOSUB loadwuc
NEXT j

setkeys
DO
 setwait timing(), 120
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(29) > 0 AND keyval(14) > 1 THEN
  GOSUB savealluc
  cropafter ptr, sets, 0, game$ + file$, size * perset, 1
  clearpage 3
  GOSUB loadalluc
 END IF
 IF keyval(57) > 1 OR keyval(28) > 1 THEN GOSUB spriteage
 IF keyval(73) > 1 THEN
  GOSUB savealluc
  top = large(top - atatime, 0)
  ptr = large(ptr - atatime, 0)
  GOSUB loadalluc
 END IF
 IF keyval(81) > 1 THEN
  GOSUB savealluc
  top = large(small(top + atatime, sets - atatime), 0)
  ptr = large(small(ptr + atatime, sets - atatime), 0)
  GOSUB loadalluc
 END IF
 IF keyval(72) > 1 THEN
  ptr = large(ptr - 1, 0)
  IF ptr < top THEN
   j = top + atatime: GOSUB savewuc
   top = ptr
   FOR i = atatime TO 1 STEP -1
    FOR o = 0 TO perset - 1
     loadsprite placer(), 0, size * o, soff * (i - 1), xw, yw, 3
     stosprite placer(), 0, size * o, soff * i, 3
    NEXT o
   NEXT i
   j = ptr: GOSUB loadwuc
  END IF
 END IF
 IF keyval(80) > 1 AND ptr < 32767 THEN
  ptr = ptr + 1
  IF needaddset(ptr, sets, "graphics") THEN
   setpicstuf buffer(), size * perset, -1
   FOR i = 0 TO (size * perset) / 2
    buffer(i) = 0
   NEXT i
   storeset game$ + file$ + CHR$(0), ptr, 0
  END IF
  IF ptr > top + atatime THEN
   j = top: GOSUB savewuc
   top = top + 1
   FOR i = 0 TO atatime - 1
    FOR o = 0 TO perset - 1
     loadsprite placer(), 0, size * o, soff * (i + 1), xw, yw, 3
     stosprite placer(), 0, size * o, soff * i, 3
    NEXT o
   NEXT i
   j = ptr: GOSUB loadwuc
  END IF
 END IF
 IF keyval(75) > 1 THEN num = large(num - 1, 0)
 IF keyval(77) > 1 THEN num = small(num + 1, perset - 1)
 IF keyval(26) > 1 THEN
  offset = changepal(offset, offset - 1, workpal(), 0)
 END IF
 IF keyval(27) > 1 THEN
  offset = changepal(offset, offset + 1, workpal(), 0)
 END IF
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN 
  loadsprite spriteclip(), 0, num * size, soff * (ptr - top), xw, yw, 3
  paste = 1
  clippedw = xw
  clippedh = yw
 END IF
 IF (((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1)) AND paste = 1 THEN
  loadsprite placer(), 0, num * size, soff * (ptr - top), xw, yw, 3
  rectangle 0, 0, xw, yw, 0, dpage
  drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
  rectangle 0, 0, clippedw, clippedh, 0, dpage
  drawsprite spriteclip(), 0, nulpal(), 0, 0, 0, dpage
  getsprite placer(), 0, 0, 0, xw, yw, dpage
  stosprite placer(), 0, num * size, soff * (ptr - top), 3
 END IF
 IF (keyval(29) > 0 AND keyval(20) > 1) AND paste = 1 THEN     'transparent pasting
  loadsprite placer(), 0, num * size, soff * (ptr - top), xw, yw, 3
  rectangle 0, 0, xw, yw, 0, dpage
  drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
  drawsprite spriteclip(), 0, nulpal(), 0, 0, 0, dpage
  getsprite placer(), 0, 0, 0, xw, yw, dpage
  stosprite placer(), 0, num * size, soff * (ptr - top), 3
 END IF
 GOSUB choose
 textcolor 7, 0
 printstr "Palette" + STR$(offset), 320 - (LEN("Palette" + STR$(offset)) * 8), 0, dpage
 printstr "Set" + STR$(ptr), 320 - (LEN("Set" + STR$(ptr)) * 8), 8, dpage
 printstr info$(num), 320 - (LEN(info$(num)) * 8), 16, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
offset = changepal(offset, offset, workpal(), 0)
GOSUB savealluc
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

choose:
rectangle 0, 0, 320, 200, 244, dpage
rectangle 4 + (num * (xw + 1)), (ptr - top) * (yw + 5), xw + 2, yw + 2, 15, dpage
FOR i = top TO small(top + atatime, sets)
 FOR o = 0 TO perset - 1
  rectangle 5 + (o * (xw + 1)), 1 + ((i - top) * (yw + 5)), xw, yw, 0, dpage
  loadsprite placer(), 0, size * o, soff * (i - top), xw, yw, 3
  drawsprite placer(), 0, workpal(), 0, 5 + (o * (xw + 1)), 1 + ((i - top) * (yw + 5)), dpage
 NEXT o
NEXT i
RETURN

spriteage:
undodepth = 0
undoptr = 0
undomax = (32000 \ size) - 1
GOSUB spedbak
loadsprite placer(), 0, num * size, soff * (ptr - top), xw, yw, 3
setkeys
DO
 setwait timing(), 110
 setkeys
 IF gotm THEN
  readmouse mouse()
  zcsr = 0
  zone = mouseover(mouse(), zox, zoy, zcsr, area())
 END IF
 IF keyval(1) > 1 THEN
  IF box OR drl OR ovalstep THEN
   GOSUB resettool
  ELSE
   stosprite placer(), 0, num * size, soff * (ptr - top), 3: GOSUB resettool: RETURN
  END IF
 END IF
 GOSUB sprctrl
 tog = tog XOR 1
 copypage 2, dpage  'moved this here to cover up residue on dpage (which was there before I got here!)
 GOSUB spritescreen
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

resettool:
box = 0
drl = 0
ovalstep = 0
RETURN

sprctrl:
IF mouse(2) = 0 THEN
 oldx = -1
 oldy = -1
END IF
IF keyval(4) > 1 THEN setvispage 3: w = getkey
IF keyval(41) > 1 THEN hideptr = hideptr XOR 1
IF keyval(51) > 1 AND col > 0 THEN col = col - 1
IF keyval(52) > 1 AND col < 15 THEN col = col + 1
IF zone = 2 THEN
 IF mouse(3) > 0 THEN col = small(INT(zox / 4), 15)
END IF
IF keyval(26) > 1 OR (zone = 5 AND mouse(3) > 0) THEN
 offset = changepal(offset, offset - 1, workpal(), 0)
END IF
IF keyval(27) > 1 OR (zone = 6 AND mouse(3) > 0) THEN
 offset = changepal(offset, offset + 1, workpal(), 0)
END IF
IF keyval(25) > 1 OR (zone = 19 AND mouse(3) > 0) THEN '--call palette browser
 '--write changes so far
 stosprite placer(), 0, num * size, soff * (ptr - top), 3
 '--save current palette
 storepal16 workpal(), 0, offset
 offset = pal16browse(offset, 1, num * size, soff * (ptr - top), xw, yw, 3)
 getpal16 workpal(), 0, offset
END IF
'--UNDO
IF (keyval(29) > 0 AND keyval(44) > 1) OR (zone = 20 AND mouse(3) > 0) THEN GOSUB readundospr
'--COPY (CTRL+INS,SHIFT+DEL,CTRL+C)
IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN
 clippedw = xw
 clippedh = yw
 stosprite placer(), 0, num * size, soff * (ptr - top), 3
 loadsprite spriteclip(), 0, num * size, soff * (ptr - top), xw, yw, 3
 paste = 1
END IF
'--PASTE (SHIFT+INS,CTRL+V)
IF (((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1)) AND paste = 1 THEN
 rectangle 0, 0, xw, yw, 0, dpage
 drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
 rectangle x, y, clippedw, clippedh, 0, dpage
 drawsprite spriteclip(), 0, nulpal(), 0, x, y, dpage
 getsprite placer(), 0, 0, 0, xw, yw, dpage
END IF
'--TRANSPARENT PASTE (CTRL+T)
IF (keyval(29) > 0 AND keyval(20) > 1) AND paste = 1 THEN
 rectangle 0, 0, xw, yw, 0, dpage
 drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
 drawsprite spriteclip(), 0, nulpal(), 0, x, y, dpage
 getsprite placer(), 0, 0, 0, xw, yw, dpage
END IF
'--COPY PALETTE (ALT+C)
IF keyval(56) > 0 AND keyval(46) > 1 THEN
 FOR i = 0 TO 7
  pclip(i) = workpal(i)
 NEXT
 clippedpal = 1
END IF
'--PASTE PALETTE (ALT+V)
IF keyval(56) > 0 AND keyval(47) > 1 THEN
 IF clippedpal THEN
  FOR i = 0 TO 8
   workpal(i) = pclip(i)
  NEXT
 END IF
END IF
IF keyval(56) > 0 AND col > 0 THEN
 DEF SEG = VARSEG(workpal(0))
 IF keyval(72) > 0 AND PEEK(col) > 15 THEN POKE col, PEEK(col) - 16
 IF keyval(80) > 0 AND PEEK(col) < 240 THEN POKE col, PEEK(col) + 16
 IF keyval(75) > 0 AND PEEK(col) > 0 THEN POKE col, PEEK(col) - 1
 IF keyval(77) > 0 AND PEEK(col) < 255 THEN POKE col, PEEK(col) + 1
END IF
IF mouse(3) = 1 AND zone = 3 AND col > 0 THEN
 POKE col, INT(INT(zoy / 6) * 16) + INT(zox / 4)
END IF
IF keyval(56) = 0 THEN
 IF keyval(72) > 0 THEN
  y = large(0, y - 1)
  IF zone = 1 THEN mouse(1) = mouse(1) - zoom: movemouse mouse(0), mouse(1)
  IF zone = 14 THEN mouse(1) = mouse(1) - 1: movemouse mouse(0), mouse(1)
 END IF
 IF keyval(80) > 0 THEN
  y = small(yw - 1, y + 1)
  IF zone = 1 THEN mouse(1) = mouse(1) + zoom: movemouse mouse(0), mouse(1)
  IF zone = 14 THEN mouse(1) = mouse(1) + 1: movemouse mouse(0), mouse(1)
 END IF
 IF keyval(75) > 0 THEN
  x = large(0, x - 1)
  IF zone = 1 THEN mouse(0) = mouse(0) - zoom: movemouse mouse(0), mouse(1)
  IF zone = 14 THEN mouse(0) = mouse(0) - 1: movemouse mouse(0), mouse(1)
 END IF
 IF keyval(77) > 0 THEN
  x = small(xw - 1, x + 1)
  IF zone = 1 THEN mouse(0) = mouse(0) + zoom: movemouse mouse(0), mouse(1)
  IF zone = 14 THEN mouse(0) = mouse(0) + 1: movemouse mouse(0), mouse(1)
 END IF
END IF
IF zone = 1 THEN
 x = INT(zox / zoom)
 y = INT(zoy / zoom)
END IF
IF tool = 5 THEN '--adjust airbrush
 IF mouse(3) = 1 OR mouse(2) = 1 THEN
  IF zone = 15 THEN airsize = large(airsize - 1, 1)
  IF zone = 17 THEN airsize = small(airsize + 1, 80)
  IF zone = 16 THEN mist = large(mist - 1, 1)
  IF zone = 18 THEN mist = small(mist + 1, 99)
 END IF
 IF keyval(12) > 1 OR keyval(74) > 1 THEN
  IF keyval(29) > 0 THEN
   mist = large(mist - 1, 1)
  ELSE
   airsize = large(airsize - 1, 1)
  END IF
 END IF
 IF keyval(13) > 1 OR keyval(78) > 1 THEN
  IF keyval(29) > 0 THEN
   mist = small(mist + 1, 99)
  ELSE
   airsize = small(airsize + 1, 80)
  END IF
 END IF
END IF
IF zone = 14 THEN
 x = zox
 y = zoy
END IF
IF ((zone = 1 OR zone = 14) AND (mouse(3) = 1 OR mouse(2) = 1)) OR keyval(57) > 0 THEN
 SELECT CASE tool
  CASE 0'---Draw
   GOSUB putdot
  CASE 1'---Box
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF box THEN
     box = 0: GOSUB drawsquare
    ELSE
     box = 1: bx = x: by = y
    END IF
   END IF
  CASE 2'---Line
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF drl THEN
     drl = 0: GOSUB straitline
    ELSE
     drl = 1: bx = x: by = y
    END IF
   END IF
  CASE 3'---Fill
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    GOSUB floodfill
   END IF
  CASE 4'---Oval
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    SELECT CASE ovalstep
     CASE 0'--start oval
      bx = x: by = y
      squishx = 0: squishy = 0
      radius = 0
      ovalstep = 1
     CASE 1'--draw the oval
      GOSUB drawoval
      ovalstep = 0
    END SELECT
   END IF
  CASE 5'---Spray
   GOSUB sprayspot
 END SELECT
END IF
IF ovalstep = 1 THEN
 radius = large(ABS(x - bx), ABS(y - by))
END IF
FOR i = 0 TO 5
 IF (mouse(3) > 0 AND zone = 7 + i) OR keyval(shortk(i)) > 1 THEN
  tool = i
  GOSUB resettool
  dcsr = cursor(i) + 1
 END IF
NEXT i
IF keyval(28) > 1 OR (zone = 1 AND mouse(2) = 2) THEN
 drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage: col = readpixel(239 + x, 119 + y, dpage)
END IF
IF keyval(14) > 1 OR (zone = 4 AND mouse(3) > 0) THEN wardsprite placer(), 0, nulpal(), 0, 239, 119, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
IF keyval(58) > 0 THEN
 IF keyval(72) > 0 THEN rectangle 239, 119, xw, yw, 0, dpage: drawsprite placer(), 0, nulpal(), 0, 239, 118, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
 IF keyval(80) > 0 THEN rectangle 239, 119, xw, yw, 0, dpage: drawsprite placer(), 0, nulpal(), 0, 239, 120, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
 IF keyval(75) > 0 THEN rectangle 239, 119, xw, yw, 0, dpage: drawsprite placer(), 0, nulpal(), 0, 238, 119, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
 IF keyval(77) > 0 THEN rectangle 239, 119, xw, yw, 0, dpage: drawsprite placer(), 0, nulpal(), 0, 240, 119, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
END IF
IF keyval(23) > 1 OR (zone = 13 AND mouse(3) > 0) THEN GOSUB import16
RETURN

spedbak:
clearpage 2
rectangle 3, 0, xw * zoom + 2, yw * zoom + 2, 15, 2
rectangle 4, 1, xw * zoom, yw * zoom, 0, 2
rectangle 245, 109, 67, 8, 15, 2
rectangle 246, 110, 65, 6, 0, 2
rectangle 238, 118, xw + 2, yw + 2, 15, 2
rectangle 239, 119, xw, yw, 0, 2
area(0, 2) = xw * zoom
area(0, 3) = yw * zoom
area(13, 2) = xw
area(13, 3) = yw
RETURN

import16:
srcbmp$ = browse$(2, default$, "*.bmp", "")
IF srcbmp$ = "" THEN RETURN
'--------------------
'DECIDE ABOUT PALETTE
pcsr = 0
setkeys
DO
 setwait timing(), 110
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 IF keyval(75) > 1 OR keyval(26) > 1 THEN
  offset = changepal(offset, offset - 1, workpal(), 0)
 END IF
 IF keyval(77) > 1 OR keyval(27) > 1 THEN
  offset = changepal(offset, offset + 1, workpal(), 0)
 END IF
 dummy = usemenu(pcsr, 0, 0, 2, 24)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF pcsr = 2 THEN RETURN
  EXIT DO
 END IF
 GOSUB spritescreen
 rectangle 4, 156, 208, 32, 8, dpage
 FOR i = 0 TO 2
  c = 7: IF i = pcsr THEN c = 14 + tog
  edgeprint pmenu$(i), 8, 160 + (i * 8), c, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

clearpage dpage
clearpage 2

loadbmp srcbmp$ + CHR$(0), 1, 1, buffer(), 2

'---------------------
'PICK BACKGROUND COLOR
gx = 1
gy = 1
temp = bmpinfo(srcbmp$ + CHR$(0), bmpd())
edjx = small(320, bmpd(1))
edjy = small(200, bmpd(2))
setkeys
DO
 setwait timing(), 110
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN GOSUB spedbak: RETURN '--Cancel
 IF keyval(72) > 0 THEN gy = large(gy - (1 + (keyval(56) * 8)), 1)
 IF keyval(80) > 0 THEN gy = small(gy + (1 + (keyval(56) * 8)), edjy)
 IF keyval(75) > 0 THEN gx = large(gx - (1 + (keyval(56) * 8)), 1)
 IF keyval(77) > 0 THEN gx = small(gx + (1 + (keyval(56) * 8)), edjx)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN EXIT DO
 rectangle gx, gy, 1, 1, 15 + tog, dpage
 edgeprint "Pick Background Color", 0, 190, 7, dpage
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

'--picked a transparent pixel
temp = readpixel(gx, gy, 2)
'--swap the transparent pixels to 0
FOR i = 1 TO edjx
 FOR o = 1 TO edjy
  IF readpixel(i, o, 2) = temp THEN
   rectangle i, o, 1, 1, 0, 2
  ELSE
   IF readpixel(i, o, 2) = 0 THEN
    rectangle i, o, 1, 1, temp, 2
   END IF
  END IF
 NEXT o
NEXT i
'--swap the transparent palette entry to 0
IF pcsr = 0 THEN
 getbmppal srcbmp$ + CHR$(0), master(), workpal(), 0
 DEF SEG = VARSEG(workpal(0))
 'swap black with the transparent color
 POKE temp, PEEK(0)
 POKE 0, 0
END IF
'--read the sprite
getsprite placer(), 0, 1, 1, xw, yw, 2
GOSUB spedbak
RETURN

floodfill:
GOSUB writeundospr
rectangle 238, 118, xw + 2, yw + 2, 17, dpage
rectangle 239, 119, xw, yw, 0, dpage
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
paintat 239 + x, 119 + y, col, dpage, buffer(), 16384
getsprite placer(), 0, 239, 119, xw, yw, dpage
RETURN

sprayspot:
IF oldx = -1 AND oldy = -1 THEN GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
airbrush 239 + x, 119 + y, airsize, mist, col, dpage
getsprite placer(), 0, 239, 119, xw, yw, dpage
oldx = x
oldy = y
RETURN

writeundospr:
stosprite placer(), 0, undoptr * size, 100, 3
undoptr = loopvar(undoptr, 0, undomax, 1)
undodepth = small(undodepth + 1, undomax + 1)
RETURN

readundospr:
IF undodepth > 0 THEN
 undodepth = undodepth - 1
 undoptr = loopvar(undoptr, 0, undomax, -1)
 loadsprite placer(), 0, undoptr * size, 100, xw, yw, 3
END IF
RETURN

putdot:
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
IF oldx = -1 AND oldy = -1 THEN
 GOSUB writeundospr
 rectangle 239 + x, 119 + y, 1, 1, col, dpage
ELSE
 drawline 239 + x, 119 + y, 239 + oldx, 119 + oldy, col, dpage
END IF
getsprite placer(), 0, 239, 119, xw, yw, dpage
oldx = x
oldy = y
RETURN

drawoval:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
ellipse 239 + bx, 119 + by, radius, col, dpage, squishx, squishy
getsprite placer(), 0, 239, 119, xw, yw, dpage
RETURN

drawsquare:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
rectangle 239 + small(x, bx), 119 + small(y, by), ABS(x - bx) + 1, ABS(y - by) + 1, col, dpage
getsprite placer(), 0, 239, 119, xw, yw, dpage
RETURN

straitline:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
drawline 239 + x, 119 + y, 239 + bx, 119 + by, col, dpage
getsprite placer(), 0, 239, 119, xw, yw, dpage
RETURN

spritescreen:
DEF SEG = VARSEG(workpal(0))
rectangle 247 + ((PEEK(col) - (INT(PEEK(col) / 16) * 16)) * 4), 0 + (INT(PEEK(col) / 16) * 6), 5, 7, 15, dpage
FOR i = 0 TO 15
 FOR o = 0 TO 15
  rectangle 248 + (i * 4), 1 + (o * 6), 3, 5, o * 16 + i, dpage
 NEXT o
NEXT i
textcolor 15, 8: IF zone = 5 THEN textcolor 15, 6
printstr CHR$(27), 248, 100, dpage
textcolor 15, 8: IF zone = 6 THEN textcolor 15, 6
printstr CHR$(26), 304, 100, dpage
textcolor 15, 0
printstr LEFT$(" Pal", 4 - (LEN(STR$(offset)) - 3)) + STR$(offset), 248, 100, dpage
rectangle 247 + (col * 4), 110, 5, 7, 15, dpage
FOR i = 0 TO 15
 rectangle 248 + (i * 4), 111, 3, 5, PEEK(i), dpage
NEXT
IF zoom = 4 THEN hugesprite placer(), workpal(), 0, 4, 1, dpage
IF zoom = 2 THEN bigsprite placer(), workpal(), 0, 4, 1, dpage
IF box = 1 THEN
 DEF SEG = VARSEG(workpal(0))
 rectangle 4 + small(x, bx) * zoom, 1 + small(y, by) * zoom, (ABS(x - bx) + 1) * zoom, (ABS(y - by) + 1) * zoom, PEEK(col), dpage
 rectangle 4 + bx * zoom, 1 + by * zoom, zoom, zoom, tog * 15, dpage
END IF
rectangle 4 + (x * zoom), 1 + (y * zoom), zoom, zoom, tog * 15, dpage
drawsprite placer(), 0, workpal(), 0, 239, 119, dpage
IF box = 1 THEN
 DEF SEG = VARSEG(workpal(0))
 rectangle 239 + small(x, bx), 119 + small(y, by), ABS(x - bx) + 1, ABS(y - by) + 1, PEEK(col), dpage
 rectangle 239 + bx, 119 + by, 1, 1, tog * 15, dpage
END IF
IF drl = 1 THEN
 DEF SEG = VARSEG(workpal(0))
 drawline 239 + x, 119 + y, 239 + bx, 119 + by, PEEK(col), dpage
 drawline 5 + (x * zoom), 2 + (y * zoom), 5 + (bx * zoom), 2 + (by * zoom), PEEK(col), dpage
END IF
IF ovalstep > 0 THEN
 DEF SEG = VARSEG(workpal(0))
 ellipse 239 + bx, 119 + by, radius, PEEK(col), dpage, squishx, squishy
 ellipse 5 + (bx * zoom), 2 + (by * zoom), radius * zoom, PEEK(col), dpage, squishx, squishy
END IF
IF tool = 5 THEN
 DEF SEG = VARSEG(workpal(0))
 ellipse 239 + x, 119 + y, airsize / 2, PEEK(col), dpage, 0, 0
 ellipse 5 + (x * zoom), 2 + (y * zoom), (airsize / 2) * zoom, PEEK(col), dpage, 0, 0
END IF
rectangle 239 + x, 119 + y, 1, 1, tog * 15, dpage
textcolor 7, 0
printstr info$(num), 0, 182, dpage
printstr "Tool:" + tool$(tool), 0, 190, dpage
FOR i = 0 TO 5
 t1 = 7: t2 = 8
 IF tool = i THEN t1 = 15: t2 = 7
 IF zone - 7 = i THEN t2 = 6
 textcolor t1, t2
 printstr icon$(i), 90 + i * 12, 190, dpage
NEXT i
textcolor 7, 8: IF zone = 4 THEN textcolor 15, 6
printstr CHR$(7), 182, 190, dpage
textcolor 7, 8: IF zone = 13 THEN textcolor 15, 6
printstr "I", 194, 190, dpage
textcolor 0 + (7 * SGN(undodepth)), 8: IF zone = 20 AND undodepth > 0 THEN textcolor 15, 6
printstr "UNDO", 170, 182, dpage
IF tool = 5 THEN
 textcolor 7, 0
 printstr "SIZE" + LTRIM$(STR$(airsize)), 218, 182, dpage
 printstr "MIST" + LTRIM$(STR$(mist)), 218, 190, dpage
 textcolor 7, 8: IF zone = 15 THEN textcolor 15, 6
 printstr CHR$(27), 210, 182, dpage
 textcolor 7, 8: IF zone = 16 THEN textcolor 15, 6
 printstr CHR$(27), 210, 190, dpage
 textcolor 7, 8: IF zone = 17 THEN textcolor 15, 6
 printstr CHR$(26), 266, 182, dpage
 textcolor 7, 8: IF zone = 18 THEN textcolor 15, 6
 printstr CHR$(26), 266, 190, dpage
END IF
IF gotm THEN
 c = zcsr
 IF c = -1 THEN
  IF hideptr THEN c = -2 ELSE c = dcsr
 END IF
 textcolor 10 + tog * 5, 0
 printstr CHR$(2 + c), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
END IF
RETURN

initmarea:
'0 x
'1 y
'2 width
'3 height
'4 cursor
'DRAWING ZONE
area(0, 0) = 4
area(0, 1) = 1
area(0, 4) = -1
'PALETTE ZONE
area(1, 0) = 248
area(1, 1) = 111
area(1, 2) = 64
area(1, 3) = 6
area(1, 4) = 0
'MASTER PAL ZONE
area(2, 0) = 248
area(2, 1) = 1
area(2, 2) = 64
area(2, 3) = 96
area(2, 4) = 0
'FLIP BUTTON
area(3, 0) = 182
area(3, 1) = 190
area(3, 2) = 8
area(3, 3) = 10
area(3, 4) = 0
'PREV PAL BUTTON
area(4, 0) = 248
area(4, 1) = 100
area(4, 2) = 8
area(4, 3) = 8
area(4, 4) = 0
'NEXT PAL BUTTON
area(5, 0) = 304
area(5, 1) = 100
area(5, 2) = 8
area(5, 3) = 8
area(5, 4) = 0
'TOOL BUTTONS
FOR i = 0 TO 5
 area(6 + i, 0) = 90 + i * 12
 area(6 + i, 1) = 190
 area(6 + i, 2) = 8
 area(6 + i, 3) = 10
 area(6 + i, 4) = 0
NEXT i
'IMPORT BUTTON
area(12, 0) = 194
area(12, 1) = 190
area(12, 2) = 8
area(12, 3) = 10
area(12, 4) = 0
'SMALL DRAWING AREA
area(13, 0) = 239
area(13, 1) = 119
area(13, 4) = -1
'LESS AIRBRUSH AREA
area(14, 0) = 210
area(14, 1) = 182
area(14, 2) = 8
area(14, 3) = 8
area(14, 4) = 0
'LESS AIRBRUSH MIST
area(15, 0) = 210
area(15, 1) = 190
area(15, 2) = 8
area(15, 3) = 8
area(15, 4) = 0
'MORE AIRBRUSH AREA
area(16, 0) = 266
area(16, 1) = 182
area(16, 2) = 8
area(16, 3) = 8
area(16, 4) = 0
'MORE AIRBRUSH MIST
area(17, 0) = 266
area(17, 1) = 190
area(17, 2) = 8
area(17, 3) = 8
area(17, 4) = 0
'PALETTE NUMBER
area(18, 0) = 256
area(18, 1) = 100
area(18, 2) = 48
area(18, 3) = 8
area(18, 4) = 0
'UNDO BUTTON
area(19, 0) = 170
area(19, 1) = 182
area(19, 2) = 32
area(19, 3) = 8
area(19, 4) = 0
RETURN

savealluc:
FOR j = top TO top + atatime: GOSUB savewuc: NEXT j
RETURN

savewuc:
IF j <= sets THEN
 setpicstuf buffer(), size * perset, 2
 FOR o = 0 TO (perset - 1)
  loadsprite placer(), 0, size * o, soff * (j - top), xw, yw, 3
  stosprite placer(), 0, size * o, 0, 2
 NEXT o
 storeset game$ + file$ + CHR$(0), large(j, 0), 0
END IF
RETURN

loadalluc:
FOR j = top TO top + atatime: GOSUB loadwuc: NEXT j
RETURN

loadwuc:
IF j <= sets THEN
 setpicstuf buffer(), size * perset, 2
 loadset game$ + file$ + CHR$(0), large(j, 0), 0
 FOR o = 0 TO (perset - 1)
  loadsprite placer(), 0, size * o, 0, xw, yw, 2
  stosprite placer(), 0, size * o, soff * (j - top), 3
 NEXT o
END IF
RETURN

END SUB

SUB testanimpattern (tastuf(), taset)

DIM cycle(1), sample(7), cycptr(1), cycskip(1)

clearpage vpage
clearpage dpage

sample(0) = 3
sample(1) = 3
buffer(0) = 16
buffer(1) = 3
setmapdata buffer(), buffer(), 10, 130
FOR i = 0 TO 47
 y = INT(i / 16)
 x = i - y * 16
 setmapblock x, y, tastuf(20 * taset) + i
NEXT i

GOSUB setupsample

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(72) > 1 THEN csr = loopvar(csr, 0, 47, -16): GOSUB setupsample
 IF keyval(80) > 1 THEN csr = loopvar(csr, 0, 47, 16): GOSUB setupsample
 IF keyval(75) > 1 THEN csr = loopvar(csr, 0, 47, -1): GOSUB setupsample
 IF keyval(77) > 1 THEN csr = loopvar(csr, 0, 47, 1): GOSUB setupsample
 SWAP vpage, dpage
 setvispage vpage
 '--draw available animating tiles--
 setmapdata buffer(), buffer(), 10, 130
 drawmap 0, -10, 0, dpage
 '--draw sample--
 setmapdata sample(), sample(), 100, 40
 setanim tastuf(0) + cycle(0), tastuf(20) + cycle(1)
 cycletile cycle(), tastuf(), cycptr(), cycskip()
 drawmap -130, -100, 0, dpage
 '--Draw cursor--
 y = INT(csr / 16)
 x = csr - y * 16
 rectangle 20 * x, 10 + 20 * y, 20, 1, 14 + tog, dpage
 rectangle 20 * x, 10 + 20 * y, 1, 20, 14 + tog, dpage
 rectangle 20 * x, 29 + 20 * y, 20, 1, 14 + tog, dpage
 rectangle 20 * x + 19, 10 + 20 * y, 1, 20, 14 + tog, dpage
 
 dowait
LOOP
EXIT SUB

setupsample:
setmapdata sample(), sample(), 100, 70
FOR i = 0 TO 8
 y = INT(i / 3)
 x = i - y * 3
 setmapblock x, y, 160 + (taset * 48) + csr
NEXT i
RETURN

END SUB

