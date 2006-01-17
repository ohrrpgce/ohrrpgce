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
DECLARE SUB writeconstant (filehandle%, num%, name$, unique$(), prefix$)
DECLARE SUB safekill (f$)
DECLARE SUB touchfile (f$)
DECLARE SUB romfontchar (font%(), char%)
DECLARE SUB standardmenu (menu$(), size%, vis%, ptr%, top%, x%, y%, page%, edge%)
DECLARE FUNCTION readitemname$ (index%)
DECLARE FUNCTION readattackname$ (index%)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION getShortName$ (filename$)
DECLARE FUNCTION getLongName$ (filename$)
DECLARE SUB textfatalerror (e$)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION scriptname$ (num%, f$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE FUNCTION getmapname$ (m%)
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION isunique% (s$, u$(), r%)
DECLARE FUNCTION loadname$ (length%, offset%)
DECLARE SUB exportnames (gamedir$, song$())
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE FUNCTION needaddset (ptr%, check%, what$)
DECLARE FUNCTION browse$ (special, default$, fmask$, tmp$)
DECLARE SUB cycletile (cycle%(), tastuf%(), ptr%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%())
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION intstr$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB debug (s$)
DECLARE SUB bitset (array%(), wof%, last%, name$())
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB formation (song$())
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage (song$())
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (master%(), font%())
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE FUNCTION maplumpname$ (map, oldext$)
DECLARE SUB editmenus ()

'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'cglobals.bi'

'$INCLUDE: 'const.bi'

REM $STATIC
SUB editmenus

'DIM menu$(20), veh(39), min(39), max(39), offset(39), vehbit$(15), tiletype$(8)

CONST MenuDatSize = 187
DIM menu$(20),min(20),max(20), bit$(-1 to 32), menuname$

DIM menudat(MenuDatSize - 1), ptr, csr

ptr = 0: csr = 0

bit$(0) = "Background is Fuzzy"


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
   IF ptr = general(genMaxMenu) AND keyval(77) > 1 THEN
    GOSUB savedat
    ptr = bound(ptr + 1, 0, 255)
    IF needaddset(ptr, general(genMaxMenu), "menu") THEN
     FOR i = 0 TO MenuDatSize - 1
      menudat(i) = 0
     NEXT i
     menuname$ = ""
     GOSUB menu
    END IF
   END IF
   newptr = ptr
   IF intgrabber(newptr, 0, general(genMaxMenu), 75, 77) THEN
    GOSUB savedat
    ptr = newptr
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
   bitset menudat(), 13, 32, bit$()
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
menu$(1) = "Menu" + STR$(ptr)
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
' RETURN

loaddat:
setpicstuf menudat(), MenuDatSize*2, -1
loadset "menus.dat" + CHR$(0), ptr, 0
menuname$ = STRING$(bound(menudat(0) AND 255, 0, 15), 0)
array2str menudat(), 1, menuname$
RETURN

savedat:
' veh(0) = bound(LEN(vehname$), 0, 5)
' str2array vehname$, veh(), 1
' setpicstuf veh(), 80, -1
' storeset "menus.dat" + CHR$(0), ptr, 0
RETURN

END SUB

SUB vehicles

DIM menu$(20), veh(39), min(39), max(39), offset(39), vehbit$(15), tiletype$(8)

ptr = 0: csr = 0

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
   IF ptr = general(55) AND keyval(77) > 1 THEN
    GOSUB saveveh
    ptr = bound(ptr + 1, 0, 32767)
    IF needaddset(ptr, general(55), "vehicle") THEN
     FOR i = 0 TO 39
      veh(i) = 0
     NEXT i
     vehname$ = ""
     GOSUB vehmenu
    END IF
   END IF
   newptr = ptr
   IF intgrabber(newptr, 0, general(55), 75, 77) THEN
    GOSUB saveveh
    ptr = newptr
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
    bitset veh(), 9, 8, vehbit$()
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
menu$(1) = "Vehicle" + STR$(ptr)
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
loadset game$ + ".veh" + CHR$(0), ptr, 0
vehname$ = STRING$(bound(veh(0) AND 255, 0, 15), 0)
array2str veh(), 1, vehname$
RETURN

saveveh:
veh(0) = bound(LEN(vehname$), 0, 15)
str2array vehname$, veh(), 1
setpicstuf veh(), 80, -1
storeset game$ + ".veh" + CHR$(0), ptr, 0
RETURN

END SUB