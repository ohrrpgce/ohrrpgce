'OHRRPGCE CUSTOM - Editor menu routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
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
DECLARE SUB strgrabber (s$, maxl%)
DECLARE FUNCTION maplumpname$ (map, oldext$)
DECLARE SUB editmenus ()
DECLARE SUB writepassword (p$)
DECLARE FUNCTION readpassword$ ()
DECLARE SUB writescatter (s$, lhold%, start%)
DECLARE SUB readscatter (s$, lhold%, start%)
DECLARE SUB fixfilename (s$)
DECLARE FUNCTION filesize$ (file$)
DECLARE FUNCTION inputfilename$ (query$, ext$)
DECLARE FUNCTION getsongname$ (num%)
DECLARE FUNCTION getsfxname$ (num%)
DECLARE SUB writebinstring (savestr$, array%(), offset%, maxlen%)

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'cglobals.bi'

'$INCLUDE: 'const.bi'

REM $STATIC
SUB editmenus

'DIM menu$(20), veh(39), min(39), max(39), offset(39), vehbit$(15), tiletype$(8)

CONST MenuDatSize = 187
DIM menu$(20),min(20),max(20), bitname$(-1 to 32), menuname$

DIM menudat(MenuDatSize - 1), pt, csr

pt = 0: csr = 0

bitname$(0) = "Background is Fuzzy"


' min(3) = 0: max(3) = 5: offset(3) = 8             'speed
' FOR i = 0 TO 3
'  min(5 + i) = 0: max(5 + i) = 8: offset(5 + i) = 17 + i
' NEXT i
' min(9) = -1: max(9) = 255: offset(9) = 11 'battles
' min(10) = -2: max(10) = general(43): offset(10) = 12 'use button
' min(11) = -2: max(11) = general(43): offset(11) = 13 'menu button
' min(12) = -999: max(12) = 999: offset(12) = 14 'tag
' min(13) = general(43) * -1: max(13) = general(39): offset(13) = 15'mount
' min(14) = general(43) * -1: max(14) = general(39): offset(14) = 16'dismount
' min(15) = 0: max(15) = 99: offset(15) = 21'dismount

GOSUB loaddat
GOSUB menu

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(csr, top, 0, 15, 22)
 SELECT CASE csr
  CASE 0
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    EXIT DO
   END IF
  CASE 1
   IF pt = general(genMaxMenu) AND keyval(77) > 1 THEN
    GOSUB savedat
    pt = bound(pt + 1, 0, 255)
    IF needaddset(pt, general(genMaxMenu), "menu") THEN
     FOR i = 0 TO MenuDatSize - 1
      menudat(i) = 0
     NEXT i
     menuname$ = ""
     GOSUB menu
    END IF
   END IF
   newptr = pt
   IF intgrabber(newptr, 0, general(genMaxMenu), 75, 77) THEN
    GOSUB savedat
    pt = newptr
    GOSUB loaddat
    GOSUB menu
   END IF
  CASE 2
   oldname$ = menuname$
   strgrabber menuname$, 15
   IF oldname$ <> menuname$ THEN GOSUB menu
'   CASE 3, 5 TO 15
'    IF intgrabber(veh(offset(csr)), min(csr), max(csr), 75, 77) THEN
'     GOSUB vehmenu
'    END IF
  CASE 4
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    editbitset menudat(), 13, 32, bitname$()
   END IF
 END SELECT
 standardmenu menu$(), 15, 15, csr, top, 0, 0, dpage, 0
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
' GOSUB saveveh
EXIT SUB

menu:
menu$(0) = "Previous Menu"
menu$(1) = "Menu" + STR$(pt)
menu$(2) = "Name: " + vehname$

' IF veh(offset(3)) = 3 THEN tmp$ = " 10" ELSE tmp$ = STR$(veh(8))
' menu$(3) = "Speed:" + tmp$

' menu$(4) = "Vehicle Bitsets..." '9,10

' menu$(5) = "Override walls: "
' menu$(6) = "Blocked by: "
' menu$(7) = "Mount from: "
' menu$(8) = "Dismount to: "
' FOR i = 0 TO 3
'  menu$(5 + i) = menu$(5 + i) + tiletype$(bound(veh(offset(5 + i)), 0, 8))
' NEXT i

' SELECT CASE veh(offset(9))
'  CASE -1
'   tmp$ = "disabled"
'  CASE 0
'   tmp$ = "enabled"
'  CASE ELSE
'   tmp$ = "formation set" + STR$(veh(offset(9)))
' END SELECT
' menu$(9) = "Random Battles: " + tmp$ '11

' FOR i = 0 TO 1
'  SELECT CASE veh(offset(10 + i))
'   CASE -2
'    tmp$ = "disabled"
'   CASE -1
'    tmp$ = "menu"
'   CASE 0
'    tmp$ = "dismount"
'   CASE ELSE
'    tmp$ = "script " + scriptname$(ABS(veh(offset(10 + i))), "plotscr.lst")
'  END SELECT
'  IF i = 0 THEN menu$(10 + i) = "Use button: " + tmp$'12
'  IF i = 1 THEN menu$(10 + i) = "Menu button: " + tmp$'13
' NEXT i

' SELECT CASE ABS(veh(offset(12)))
'  CASE 0
'   tmp$ = " (DISABLED)"
'  CASE 1
'   tmp$ = " (RESERVED TAG)"
'  CASE ELSE
'   tmp$ = " (" + lmnemonic$(ABS(veh(offset(12)))) + ")"  '14
' END SELECT
' menu$(12) = "If riding Tag" + STR$(ABS(veh(offset(12)))) + "=" + onoroff$(veh(offset(12))) + tmp$

' SELECT CASE veh(offset(13))
'  CASE 0
'   tmp$ = "[script/textbox]"
'  CASE IS < 0
'   tmp$ = "run script " + scriptname$(ABS(veh(offset(13))), "plotscr.lst")
'  CASE IS > 0
'   tmp$ = "text box" + STR$(veh(offset(13)))
' END SELECT
' menu$(13) = "On Mount: " + tmp$

' SELECT CASE veh(offset(14))
'  CASE 0
'   tmp$ = "[script/textbox]"
'  CASE IS < 0
'   tmp$ = "run script " + scriptname$(ABS(veh(offset(14))), "plotscr.lst")
'  CASE IS > 0
'   tmp$ = "text box" + STR$(veh(offset(14)))
' END SELECT
' menu$(14) = "On Dismount: " + tmp$

' menu$(15) = "Elevation:" + STR$(veh(offset(15))) + " pixels"
RETURN

loaddat:
setpicstuf menudat(), MenuDatSize*2, -1
loadset "menus.dat" + CHR$(0), pt, 0
menuname$ = STRING$(bound(menudat(0) AND 255, 0, 15), 0)
array2str menudat(), 1, menuname$
RETURN

savedat:
' veh(0) = bound(LEN(vehname$), 0, 5)
' str2array vehname$, veh(), 1
' setpicstuf veh(), 80, -1
' storeset "menus.dat" + CHR$(0), pt, 0
RETURN

END SUB

SUB vehicles

DIM menu$(20), veh(39), min(39), max(39), offset(39), vehbit$(15), tiletype$(8)

pt = 0: csr = 0

vehbit$(0) = "Pass through walls"
vehbit$(1) = "Pass through NPCs"
vehbit$(2) = "Enable NPC activation"
vehbit$(3) = "Enable door use"
vehbit$(4) = "Do not hide leader"
vehbit$(5) = "Do not hide party"
vehbit$(6) = "Dismount one space ahead"
vehbit$(7) = "Pass walls while dismounting"
vehbit$(8) = "Disable flying shadow"

tiletype$(0) = "default"
tiletype$(1) = "A"
tiletype$(2) = "B"
tiletype$(3) = "A and B"
tiletype$(4) = "A or B"
tiletype$(5) = "not A"
tiletype$(6) = "not B"
tiletype$(7) = "neither A nor B"
tiletype$(8) = "everywhere"

min(3) = 0: max(3) = 5: offset(3) = 8             'speed
FOR i = 0 TO 3
 min(5 + i) = 0: max(5 + i) = 8: offset(5 + i) = 17 + i
NEXT i
min(9) = -1: max(9) = 255: offset(9) = 11 'battles
min(10) = -2: max(10) = general(43): offset(10) = 12 'use button
min(11) = -2: max(11) = general(43): offset(11) = 13 'menu button
min(12) = -999: max(12) = 999: offset(12) = 14 'tag
min(13) = general(43) * -1: max(13) = general(39): offset(13) = 15'mount
min(14) = general(43) * -1: max(14) = general(39): offset(14) = 16'dismount
min(15) = 0: max(15) = 99: offset(15) = 21'dismount

GOSUB loadveh
GOSUB vehmenu

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(csr, top, 0, 15, 22)
 SELECT CASE csr
  CASE 0
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    EXIT DO
   END IF
  CASE 1
   IF pt = general(55) AND keyval(77) > 1 THEN
    GOSUB saveveh
    pt = bound(pt + 1, 0, 32767)
    IF needaddset(pt, general(55), "vehicle") THEN
     FOR i = 0 TO 39
      veh(i) = 0
     NEXT i
     vehname$ = ""
     GOSUB vehmenu
    END IF
   END IF
   newptr = pt
   IF intgrabber(newptr, 0, general(55), 75, 77) THEN
    GOSUB saveveh
    pt = newptr
    GOSUB loadveh
    GOSUB vehmenu
   END IF
  CASE 2
   oldname$ = vehname$
   strgrabber vehname$, 15
   IF oldname$ <> vehname$ THEN GOSUB vehmenu
  CASE 3, 5 TO 15
   IF intgrabber(veh(offset(csr)), min(csr), max(csr), 75, 77) THEN
    GOSUB vehmenu
   END IF
  CASE 4
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    editbitset veh(), 9, 8, vehbit$()
   END IF
 END SELECT
 standardmenu menu$(), 15, 15, csr, top, 0, 0, dpage, 0
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
GOSUB saveveh
EXIT SUB

vehmenu:
menu$(0) = "Previous Menu"
menu$(1) = "Vehicle" + STR$(pt)
menu$(2) = "Name: " + vehname$

IF veh(offset(3)) = 3 THEN tmp$ = " 10" ELSE tmp$ = STR$(veh(8))
menu$(3) = "Speed:" + tmp$

menu$(4) = "Vehicle Bitsets..." '9,10

menu$(5) = "Override walls: "
menu$(6) = "Blocked by: "
menu$(7) = "Mount from: "
menu$(8) = "Dismount to: "
FOR i = 0 TO 3
 menu$(5 + i) = menu$(5 + i) + tiletype$(bound(veh(offset(5 + i)), 0, 8))
NEXT i

SELECT CASE veh(offset(9))
 CASE -1
  tmp$ = "disabled"
 CASE 0
  tmp$ = "enabled"
 CASE ELSE
  tmp$ = "formation set" + STR$(veh(offset(9)))
END SELECT
menu$(9) = "Random Battles: " + tmp$ '11

FOR i = 0 TO 1
 SELECT CASE veh(offset(10 + i))
  CASE -2
   tmp$ = "disabled"
  CASE -1
   tmp$ = "menu"
  CASE 0
   tmp$ = "dismount"
  CASE ELSE
   tmp$ = "script " + scriptname$(ABS(veh(offset(10 + i))), "plotscr.lst")
 END SELECT
 IF i = 0 THEN menu$(10 + i) = "Use button: " + tmp$'12
 IF i = 1 THEN menu$(10 + i) = "Menu button: " + tmp$'13
NEXT i

SELECT CASE ABS(veh(offset(12)))
 CASE 0
  tmp$ = " (DISABLED)"
 CASE 1
  tmp$ = " (RESERVED TAG)"
 CASE ELSE
  tmp$ = " (" + lmnemonic$(ABS(veh(offset(12)))) + ")"  '14
END SELECT
menu$(12) = "If riding Tag" + STR$(ABS(veh(offset(12)))) + "=" + onoroff$(veh(offset(12))) + tmp$

SELECT CASE veh(offset(13))
 CASE 0
  tmp$ = "[script/textbox]"
 CASE IS < 0
  tmp$ = "run script " + scriptname$(ABS(veh(offset(13))), "plotscr.lst")
 CASE IS > 0
  tmp$ = "text box" + STR$(veh(offset(13)))
END SELECT
menu$(13) = "On Mount: " + tmp$

SELECT CASE veh(offset(14))
 CASE 0
  tmp$ = "[script/textbox]"
 CASE IS < 0
  tmp$ = "run script " + scriptname$(ABS(veh(offset(14))), "plotscr.lst")
 CASE IS > 0
  tmp$ = "text box" + STR$(veh(offset(14)))
END SELECT
menu$(14) = "On Dismount: " + tmp$

menu$(15) = "Elevation:" + STR$(veh(offset(15))) + " pixels"
RETURN

loadveh:
setpicstuf veh(), 80, -1
loadset game$ + ".veh" + CHR$(0), pt, 0
vehname$ = STRING$(bound(veh(0) AND 255, 0, 15), 0)
array2str veh(), 1, vehname$
RETURN

saveveh:
veh(0) = bound(LEN(vehname$), 0, 15)
str2array vehname$, veh(), 1
setpicstuf veh(), 80, -1
storeset game$ + ".veh" + CHR$(0), pt, 0
RETURN

END SUB

SUB gendata (master())
STATIC default$
CONST maxMenu = 31
DIM m$(maxMenu), max(maxMenu), bitname$(15), subm$(4), scriptgenof(4)
DIM names$(32), stat$(11), menutop
getnames names$(), 32
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

IF general(genPoison) <= 0 THEN general(genPoison) = 161
IF general(genStun) <= 0 THEN general(genStun) = 159
IF general(genMute) <= 0 THEN general(genMute) = 163
last = maxMenu
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
max(4) = general(genMaxMap)
max(5) = general(genMaxSong)
max(6) = general(genMaxSong)
max(7) = general(genMaxSong)
max(8) = 0
max(11) = 32000
max(16) = 255 'poison
max(17) = 255 'stun
max(18) = 255 'mute
max(19) = 32767
FOR i = 20 to 21 'shut up
 max(i) = 9999 'HP + MP
NEXT
FOR i = 22 to 29
 max(i) = 999 'Regular stats
NEXT
max(30) = 100 'MP~
max(31) = 20  'Extra Hits

GOSUB loadpass
GOSUB genstr
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(csr, menutop, 0, last, 22)
 IF (keyval(28) > 1 OR keyval(57) > 1) THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 1 THEN
   bitname$(0) = "Pause on Battle Sub-menus"
   bitname$(1) = "Enable Caterpillar Party"
   bitname$(2) = "Don't Restore HP on Levelup"
   bitname$(3) = "Don't Restore MP on Levelup"
   bitname$(4) = "Inns Don't Revive Dead Heroes"
   bitname$(5) = "Hero Swapping Always Available"
   bitname$(6) = "Hide Ready-meter in Battle"
   bitname$(7) = "Hide Health-meter in Battle"
   bitname$(8) = "Disable Debugging Keys"
   bitname$(9) = "Simulate Old Levelup Bug"
   bitname$(10) = "Permit double-triggering of scripts"
   bitname$(11) = "Skip title screen"
   bitname$(12) = "Skip load screen"
   bitname$(13) = "Pause on All Battle Menus"
   bitname$(14) = "Disable Hero's Battle Cursor"
   editbitset general(), 101, 15, bitname$()
  END IF
  IF csr = 9 THEN GOSUB ttlbrowse
  IF csr = 10 THEN GOSUB renrpg
  IF csr = 12 THEN GOSUB specialplot
  IF csr = 15 THEN GOSUB importmaspal
  IF csr = 8 THEN GOSUB inputpasw
 IF csr = 16 THEN
  d$ = charpicker$
  IF d$ <> "" THEN
  general(genPoison) = ASC(d$)
   GOSUB genstr
  END IF
 END IF
 IF csr = 17 THEN
  d$ = charpicker$
  IF d$ <> "" THEN
  general(genStun) = ASC(d$)
   GOSUB genstr
  END IF
 END IF
 IF csr = 18 THEN
  d$ = charpicker$
  IF d$ <> "" THEN
  general(genMute) = ASC(d$)
   GOSUB genstr
  END IF
 END IF

 END IF
 IF csr > 1 AND csr <= 4 THEN
  IF intgrabber(general(100 + csr), 0, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr > 4 AND csr < 8 THEN
  IF zintgrabber(general(csr - 3), -1, max(csr), 75, 77) THEN GOSUB genstr
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
  IF intgrabber(general(genPoison), 32, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr = 17 THEN
  IF intgrabber(general(genStun), 32, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr = 18 THEN
  IF intgrabber(general(genMute), 32, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr = 19 THEN
  IF intgrabber(general(genDamageCap), 0, max(csr), 75, 77) THEN GOSUB genstr
 END IF
 IF csr >= 20 AND csr <= 31 THEN
  IF intgrabber(general(genStatCap + (csr - 20)), 0, max(csr), 75, 77) THEN GOSUB genstr
 END IF

 standardmenu m$(), last, 22, csr, menutop, 0, 0, dpage, 0

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
IF general(2) = 0 THEN m$(5) = m$(5) + " -none-" ELSE m$(5) = m$(5) + STR$(general(2) - 1) + " " + getsongname$(general(2) - 1)
m$(6) = "Battle Victory Music:"
IF general(3) = 0 THEN m$(6) = m$(6) + " -none-" ELSE m$(6) = m$(6) + STR$(general(3) - 1) + " " + getsongname$(general(3) - 1)
m$(7) = "Default Battle Music:"
IF general(4) = 0 THEN m$(7) = m$(7) + " -none-" ELSE m$(7) = m$(7) + STR$(general(4) - 1) + " " + getsongname$(general(4) - 1)
m$(11) = "Starting Money:" + STR$(general(96))
m$(13) = "Long Name:" + longname$
m$(14) = "About Line:" + aboutline$
m$(16) = "Poison Indicator " + STR$(general(61)) + " " + CHR$(general(61))
m$(17) = "Stun Indicator " + STR$(general(62)) + " " + CHR$(general(62))
m$(18) = "Mute Indicator " + STR$(general(genMute)) + " " + CHR$(general(genMute))
m$(19) = "Damage Cap:"
if general(genDamageCap) = 0 THEN m$(19) = m$(19) + " None" ELSE m$(19) = m$(19) + STR$(general(genDamageCap))
FOR i = 0 to 11
 m$(20 + i) = stat$(i) + " Cap:"
 if general(genStatCap + i) = 0 THEN m$(20 + i) = m$(20 + i) + " None" ELSE m$(20 + i) = m$(20 + i) + STR$(general(genStatCap + i))
NEXT
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
IF isfile(workingdir$ + SLASH + "browse.txt" + CHR$(0)) THEN
 setpicstuf buffer(), 40, -1
 loadset workingdir$ + SLASH + "browse.txt" + CHR$(0), 0, 0
 longname$ = STRING$(bound(buffer(0), 0, 38), " ")
 array2str buffer(), 2, longname$
 loadset workingdir$ + SLASH + "browse.txt" + CHR$(0), 1, 0
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
storeset workingdir$ + SLASH + "browse.txt" + CHR$(0), 0, 0
buffer(0) = bound(LEN(aboutline$), 0, 38)
str2array aboutline$, buffer(), 2
storeset workingdir$ + SLASH + "browse.txt" + CHR$(0), 1, 0
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
    findfiles workingdir$ + SLASH + oldgame$ + ".*" + CHR$(0), 0, "temp.lst" + CHR$(0), buffer()
    fh = FREEFILE
    OPEN "temp.lst" FOR APPEND AS #fh
    PRINT #fh, "-END OF LIST-"
    CLOSE #fh
    fh = FREEFILE
    OPEN "temp.lst" FOR INPUT AS #fh
    textcolor 10, 0
    printstr "Renaming Lumps...", 0, 40, vpage
    textcolor 15, 2
    DO
     LINE INPUT #fh, temp$
     IF temp$ = "-END OF LIST-" THEN EXIT DO
     printstr " " + RIGHT$(temp$, LEN(temp$) - LEN(oldgame$)) + " ", 0, 50, vpage
     copyfile workingdir$ + SLASH + temp$ + CHR$(0), workingdir$ + SLASH + newgame$ + RIGHT$(temp$, LEN(temp$) - LEN(oldgame$)) + CHR$(0), buffer()
     KILL workingdir$ + SLASH + temp$
    LOOP
    CLOSE #fh
    safekill "temp.lst"
    '--update archinym information lump
    fh = FREEFILE
    IF isfile(workingdir$ + SLASH + "archinym.lmp" + CHR$(0)) THEN
     OPEN workingdir$ + SLASH + "archinym.lmp" FOR INPUT AS #fh
     LINE INPUT #fh, oldversion$
     LINE INPUT #fh, oldversion$
     CLOSE #fh
    END IF
    OPEN workingdir$ + SLASH + "archinym.lmp" FOR OUTPUT AS #fh
    PRINT #fh, newgame$
    PRINT #fh, oldversion$
    CLOSE #fh
    game$ = workingdir$ + SLASH + newgame$
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
 printstr "game to be opened in " + CUSTOMEXE, 0, 8, dpage
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

SUB importsong (master())
STATIC default$
DIM music(16384)
setupmusic music()
setfmvol getfmvol
clearpage 0
clearpage 1
clearpage 2
clearpage 3
DIM menu$(10), submenu$(2)
menu$(0) = "Previous Menu"
menu$(3) = "Import Song..."
menu$(4) = "Export Song..."
menu$(5) = "Delete Song"

csr = 1
snum = 0
GOSUB getinfo

setkeys
DO
 setwait timing(), 100
 setkeys
 IF keyval(1) > 1 THEN EXIT DO

 dummy = usemenu(csr, 0, 0, optionsbottom, 22)

 IF csr = 2 AND songfile$ <> "" THEN
  strgrabber sname$, 30
  menu$(2) = "Name: " + sname$
 ELSE
  '-- check for switching song
  newsong = snum
  IF intgrabber(newsong, 0, general(genMaxSong), 51, 52) THEN
   GOSUB ssongdata
   snum = newsong
   GOSUB getinfo
  END IF
  IF keyval(75) > 1 AND snum > 0 THEN
   GOSUB ssongdata
   snum = snum - 1
   GOSUB getinfo
  END IF
  IF keyval(77) > 1 AND snum < 32767 THEN
   GOSUB ssongdata
   snum = snum + 1
   IF needaddset(snum, general(genMaxSong), "song") THEN sname$ = ""
   GOSUB getinfo
  END IF
 END IF
 IF (keyval(28) > 1 OR keyval(57) > 1) THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 3 THEN GOSUB importsongfile
  IF csr = 4 AND songfile$ <> "" THEN GOSUB exportsong
  IF csr = 5 AND songfile$ <> "" THEN  'delete song
   safekill songfile$
   safekill bamfile$
   GOSUB getinfo
  END IF
  IF csr = 6 THEN  'delete BAM fallback
   safekill bamfile$
   GOSUB getinfo
   csr = 0
  END IF
 END IF

 standardmenu menu$(), 10, 22, csr, 0, 0, 0, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP
GOSUB ssongdata
clearpage 0
clearpage 1
clearpage 2
clearpage 3
stopsong
closemusic

EXIT SUB

getinfo:
stopsong

'-- first job: find the song's name
temp$ = workingdir$ + SLASH + "song" + intstr$(snum)
songfile$ = ""
songtype$ = "NO FILE"
'-- BAM special case and least desirable, so check first and override
IF snum > 99 THEN
 IF isfile(temp$ + ".bam" + CHR$(0)) THEN ext$ = ".bam" : songfile$ = temp$ + ext$ : songtype$ = "Bob's Adlib Music (BAM)"
ELSE
 IF isfile(game$ + "." + intstr$(snum) + CHR$(0)) THEN ext$ = ".bam" : songfile$ = game$ + "." + intstr$(snum) : songtype$ = "Bob's Adlib Music (BAM)"
END IF
bamfile$ = songfile$
IF isfile(temp$ + ".mid" + CHR$(0)) THEN ext$ = ".mid" : songfile$ = temp$ + ext$ : songtype$ = "MIDI Music (MID)"
'--add more formats here

sname$ = getsongname$(snum)

IF songfile$ <> "" THEN '--song exists
 IF canplay(songfile$) THEN
  loadsong songfile$ + CHR$(0)
 ELSE
  IF bamfile$ <> "" THEN loadsong bamfile$ + CHR$(0)
 END IF
ELSE
 sname$ = ""
END IF

menu$(1) = "<- Song " + intstr$(snum) + " of " + intstr$(general(genMaxSong)) + " ->"
IF songfile$ <> "" THEN menu$(2) = "Name: " + sname$ ELSE menu$(2) = "-Unused-"
menu$(7) = ""
menu$(8) = "Type: " + songtype$
menu$(9) = "Filesize: " + filesize$(songfile$)
IF bamfile$ <> songfile$ AND bamfile$ <> "" THEN
 menu$(10) = "BAM fallback exists. Filesize: " + filesize$(bamfile$)
 menu$(6) = "Delete BAM fallback"
 optionsbottom = 6
ELSE
 menu$(10) = ""
 menu$(6) = ""
 optionsbottom = 5
END IF
'-- add author, length, etc, info here
RETURN

importsongfile:
stopsong
'sourcesong$ = browse$(1, default$, "*.bam", "")
sourcesong$ = browse$(5, default$, "", "")
IF sourcesong$ = "" THEN
 GOSUB getinfo 'to play the song again
 RETURN
END IF
'--remove song file (except BAM, we can leave those as fallback for QB version)
IF songfile$ <> bamfile$ THEN safekill songfile$

IF sourcesong$ <> "" THEN
 IF LCASE$(RIGHT$(sourcesong$, 4)) = ".bam" AND snum <= 99 THEN
  songfile$ = game$ + "." + intstr$(snum)
 ELSE
  songfile$ = workingdir$ + SLASH + "song" + intstr$(snum) + MID$(sourcesong$, INSTR(sourcesong$, "."))
 END IF
 copyfile sourcesong$ + CHR$(0), songfile$ + CHR$(0), buffer()
 a$ = getLongName$(sourcesong$)
 a$ = MID$(a$, 1, INSTR(a$, ".") - 1)
 sname$ = a$
 GOSUB ssongdata
END IF
GOSUB getinfo
RETURN

exportsong:
query$ = "Name of file to export to?"
IF bamfile$ <> songfile$ AND bamfile$ <> "" THEN
 submenu$(0) = "Export " + ext$ + " file"
 submenu$(1) = "Export .bam fallback file"
 submenu$(2) = "Cancel"
 choice = sublist(2, submenu$())
 IF choice = 1 THEN ext$ = ".bam" : songfile$ = bamfile$
 IF choice = 2 THEN RETURN
END IF
outfile$ = inputfilename$(query$, ext$)
IF outfile$ = "" THEN RETURN
copyfile songfile$ + CHR$(0), outfile$ + ext$ + CHR$(0), buffer()
RETURN

ssongdata:
flusharray buffer(), curbinsize(2) / 2, 0
setpicstuf buffer(), curbinsize(2), -1
writebinstring sname$, buffer(), 0, 30
storeset workingdir$ + SLASH + "songdata.bin" + CHR$(0), snum, 0
RETURN

END SUB


SUB importsfx (master())
STATIC default$
setupsound
clearpage 0
clearpage 1
clearpage 2
clearpage 3
DIM menu$(10), submenu$(2), optionsbottom
optionsbottom = 6
menu$(0) = "Previous Menu"
menu$(3) = "Import Wave..."
menu$(4) = "Export Wave..."
menu$(5) = "Delete Wave"
menu$(6) = "Play Wave"

csr = 1
snum = 0
GOSUB getinfo

setkeys
DO
 setwait timing(), 100
 setkeys
 IF keyval(1) > 1 THEN EXIT DO

 dummy = usemenu(csr, 0, 0, optionsbottom, 22)

 IF csr = 2 AND sfxfile$ <> "" THEN
  strgrabber sname$, 30
  menu$(2) = "Name: " + sname$
 ELSE
  '-- check for switching sfx
  newsfx = snum
  IF intgrabber(newsfx, 0, general(genMaxSFX), 51, 52) THEN
   GOSUB ssfxdata
   snum = newsfx
   GOSUB getinfo
  END IF
  IF keyval(75) > 1 AND snum > 0 THEN
   GOSUB ssfxdata
   snum = snum - 1
   GOSUB getinfo
  END IF
  IF keyval(77) > 1 AND snum < 32767 THEN
   GOSUB ssfxdata
   snum = snum + 1
   IF needaddset(snum, general(genMaxSFX), "sfx") THEN sname$ = ""
   GOSUB getinfo
  END IF
 END IF
 IF (keyval(28) > 1 OR keyval(57) > 1) THEN
  SELECT CASE csr 
  CASE 0
    EXIT DO
  CASE 3
    GOSUB importsfxfile
  CASE 4
    IF sfxfile$ <> "" THEN GOSUB exportsfx
  CASE 5
    IF sfxfile$ <> "" THEN  'delete sfx
      safekill sfxfile$
      GOSUB getinfo
    END IF
  CASE ELSE
    IF sfxfile$ <> "" THEN 'play sfx
      playsfx 0,0
    END IF
  END SELECT
 END IF
 

  
  

 standardmenu menu$(), 10, 22, csr, 0, 0, 0, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP
GOSUB ssfxdata
clearpage 0
clearpage 1
clearpage 2
clearpage 3
closesound

EXIT SUB

getinfo:
stopsfx 0
'-- first job: find the sfx's name
temp$ = workingdir$ + SLASH + "sfx" + intstr$(snum)
sfxfile$ = ""
sfxtype$ = "NO FILE"

IF isfile(temp$ + ".wav" + CHR$(0)) THEN ext$ = ".wav" : sfxfile$ = temp$ + ext$ : sfxtype$ = "Waveform (WAV)"
'--add more formats here

sname$ = getsfxname$(snum)

loadsfx 0, sfxfile$

IF sfxfile$ = "" THEN '--sfx doesn't exist
 sname$ = ""
END IF

menu$(1) = "<- SFX " + intstr$(snum) + " of " + intstr$(general(genMaxSFX)) + " ->"
IF sfxfile$ <> "" THEN menu$(2) = "Name: " + sname$ ELSE menu$(2) = "-Unused-"
menu$(7) = ""
menu$(8) = "Type: " + sfxtype$
menu$(9) = "Filesize: " + filesize$(sfxfile$)

'-- add author, length, etc, info here
RETURN

importsfxfile:

sourcesfx$ = browse$(6, default$, "", "")
IF sourcesfx$ = "" THEN
 RETURN
END IF

safekill sfxfile$

IF sourcesfx$ <> "" THEN
 sfxfile$ = workingdir$ + SLASH + "sfx" + intstr$(snum) + MID$(sourcesfx$, INSTR(sourcesfx$, "."))
 copyfile sourcesfx$ + CHR$(0), sfxfile$ + CHR$(0), buffer()
 a$ = getLongName$(sourcesfx$)
 a$ = MID$(a$, 1, INSTR(a$, ".") - 1)
 sname$ = a$
 GOSUB ssfxdata
END IF
GOSUB getinfo
RETURN

exportsfx:
query$ = "Name of file to export to?"
outfile$ = inputfilename$(query$, ext$)
IF outfile$ = "" THEN RETURN
copyfile sfxfile$ + CHR$(0), outfile$ + ext$ + CHR$(0), buffer()
RETURN

ssfxdata:
flusharray buffer(), curbinsize(3) / 2, 0
setpicstuf buffer(), curbinsize(3), -1
writebinstring sname$, buffer(), 0, 30
storeset workingdir$ + SLASH + "sfxdata.bin" + CHR$(0), snum, 0
RETURN

END SUB