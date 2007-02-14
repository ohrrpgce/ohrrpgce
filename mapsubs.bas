'OHRRPGCE CUSTOM - Miscellaneous unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION addmaphow% ()
DECLARE FUNCTION filenum$ (n%)
DECLARE FUNCTION animadjust% (tilenum%, tastuf%())
DECLARE FUNCTION readattackname$ (index%)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB importbmp (f$, cap$, count%)
DECLARE SUB upgrade (font%())
DECLARE SUB loadpasdefaults (array%(), tilesetnum%)
DECLARE SUB textxbload (f$, array%(), e$)
DECLARE SUB fixorder (f$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE SUB standardmenu (menu$(), size%, vis%, pt%, top%, x%, y%, page%, edge%)
DECLARE SUB vehicles ()
DECLARE SUB verifyrpg ()
DECLARE FUNCTION getmapname$ (m%)
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE SUB writescatter (s$, lhold%, array%(), start%)
DECLARE SUB readscatter (s$, lhold%, array%(), start%)
DECLARE SUB fontedit (font%(), gamedir$)
DECLARE SUB savetanim (n%, tastuf%())
DECLARE SUB loadtanim (n%, tastuf%())
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%())
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE SUB smnemonic (tagname$, index%)
DECLARE SUB tagnames ()
DECLARE SUB sizemar (array%(), wide%, high%, tempx%, tempy%, tempw%, temph%, yout%, page%, big%)
DECLARE SUB drawmini (high%, wide%, cursor%(), page%, tastuf%())
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB mapmaker (font%(), map%(), pass%(), emap%(), doors%(), link%(), npc%(), npcstat%())
DECLARE SUB npcdef (npc%(), pt%)
DECLARE SUB editbitset (array%(), wof%, last%, name$())
DECLARE SUB sprite (xw%, yw%, sets%, perset%, soff%, foff%, atatime%, info$(), size%, zoom%, fileset%, font%())
DECLARE FUNCTION needaddset (pt%, check%, what$)
DECLARE SUB shopdata ()
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE FUNCTION zintgrabber% (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE SUB importsong ()
DECLARE SUB gendata ()
DECLARE SUB itemdata ()
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata (atkdat$(), atklim%())
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage ()
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (font())
DECLARE FUNCTION getsongname$ (num%)
DECLARE FUNCTION scriptbrowse$ (trigger%, triggertype%, scrtype$)
DECLARE FUNCTION scrintgrabber (n%, BYVAL min%, BYVAL max%, BYVAL less%, BYVAL more%, scriptside%, triggertype%)

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi" 
#include "cglobals.bi"

#include "const.bi"
#include "scrconst.bi"
#include "scancodes.bi"

REM $STATIC

FUNCTION addmaphow
'--Return values
'  -2  =Cancel
'  -1  =New blank
'  >=0 =Copy


DIM temp$(2)

maptocopy = 0
pt = 0

GOSUB addmaphowmenu
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN
  '--return cancel
  addmaphow = -2
  EXIT DO
 END IF
 IF usemenu(pt, 0, 0, 2, 22) THEN
  GOSUB addmaphowmenu
 END IF
 IF pt = 2 THEN
  IF intgrabber(maptocopy, 0, gen(0), 75, 77) THEN
   GOSUB addmaphowmenu
  END IF
 END IF
 IF keyval(28) > 1 OR keyval(56) > 1 THEN
  SELECT CASE pt
   CASE 0 ' cancel
    addmaphow = -2
   CASE 1 ' blank
    addmaphow = -1
   CASE 2 ' copy
    addmaphow = maptocopy
  END SELECT
  EXIT DO
 END IF
 standardmenu temp$(), 2, 22, pt, 0, 0, 0, dpage, 0
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
EXIT FUNCTION

addmaphowmenu:
temp$(0) = "Cancel"
temp$(1) = "New Blank Map"
temp$(2) = "Copy of map" + XSTR$(maptocopy) + " " + getmapname$(maptocopy)
RETRACE

END FUNCTION

FUNCTION animadjust (tilenum, tastuf())
'given a tile number and the tile-animation data,
'adjusts to make sure the tile is non-animated
pic = tilenum
IF pic >= 208 THEN pic = (pic - 208) + tastuf(20)
IF pic >= 160 THEN pic = (pic - 160) + tastuf(0)
animadjust = pic
END FUNCTION

SUB mapmaker (font(), map(), pass(), emap(), doors(), link(), npc(), npcstat())
DIM menubar(82), cursor(600), mode$(12), list$(12), temp$(12), ulim(4), llim(4), menu$(-1 TO 20), topmenu$(24), gmap(20), gd$(-1 TO 20), gdmax(20), gdmin(20), destdoor(300), tastuf(40), cycle(1), cycptr(1), cycskip(1), sampmap(2), cursorpal(8),  _
defaults(160), pal16(288), gmapscr$(5), gmapscrof(5), npcnum(35)

DIM as integer visible(0 to 2) = {1,0,0}
DIM as integer usetile(0 to 2)
DIM as integer menubarstart(0 to 2)
DIM as integer layer

textcolor 15, 0

wide = 0: high = 0: nptr = 0
mapname$ = ""

xtemp$ = ""
FOR i = 0 TO 15: xtemp$ = xtemp$ + CHR$(i): NEXT i
str2array xtemp$, cursorpal(), 0

'--create cursor
clearpage 2
rectangle 0, 0, 20, 20, 15, 2
rectangle 1, 1, 18, 18, 0, 2
rectangle 2, 2, 16, 16, 7, 2
rectangle 3, 3, 14, 14, 0, 2
getsprite cursor(), 200, 0, 0, 20, 20, 2
clearpage 2
rectangle 0, 0, 20, 20, 15, 2
rectangle 1, 1, 18, 18, 0, 2
rectangle 3, 3, 14, 14, 7, 2
rectangle 4, 4, 12, 12, 0, 2
getsprite cursor(), 400, 0, 0, 20, 20, 2

mode$(0) = "Picture Mode"
mode$(1) = "Passability Mode"
mode$(2) = "Door Placement Mode"
mode$(3) = "NPC Placement Mode"
mode$(4) = "Foe Mapping Mode"
menubar(0) = 160: menubar(1) = 1
sampmap(0) = 1
sampmap(1) = 1
GOSUB loadmenu

maptop = 0
pt = 0
GOSUB maketopmenu
setkeys
DO
 setwait timing(), 120
 setkeys
 IF keyval(1) > 1 THEN EXIT DO
 oldtop = maptop
 dummy = usemenu(pt, maptop, 0, 2 + gen(0), 24)
 IF oldtop <> maptop THEN GOSUB maketopmenu
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF pt = 0 THEN EXIT DO
  IF pt > 0 AND pt <= gen(0) + 1 THEN
   '--silly backcompat pt adjustment
   pt = pt - 1
   GOSUB loadmap
   GOSUB whattodo
   pt = pt + 1
   GOSUB maketopmenu
  END IF
  IF pt = gen(0) + 2 THEN
   GOSUB addmap
   GOSUB maketopmenu
  END IF
 END IF
 tog = tog XOR 1
 FOR i = 0 TO 24
  textcolor 7, 0
  IF pt = maptop + i THEN textcolor 14 + tog, 0
  printstr topmenu$(i), 0, i * 8, dpage
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
EXIT SUB

maketopmenu:
FOR i = 0 TO 24
 SELECT CASE maptop + i
  CASE 0
   topmenu$(i) = "Return to Main Menu"
  CASE 1 TO gen(0) + 1
   topmenu$(i) = "Map " + filenum$((maptop + i) - 1) + ": " + getmapname$((maptop + i) - 1)
  CASE gen(0) + 2
   topmenu$(i) = "Add a New Map"
  CASE ELSE
   topmenu$(i) = ""
 END SELECT
NEXT i
RETRACE

whattodo:
x = 0: y = 0: mapx = 0: mapy = 0
csr = 0
list$(0) = "Return to Map Menu"
list$(1) = "Resize Map..."
list$(2) = "Edit NPCs..."
list$(3) = "Edit General Map Data..."
list$(4) = "Erase Map Data"
list$(5) = "Link Doors..."
list$(6) = "Edit Tilemap..."
list$(7) = "Edit Wallmap..."
list$(8) = "Place Doors..."
list$(9) = "Place NPCs..."
list$(10) = "Edit Foemap..."
list$(11) = "Re-load Default Passability"
list$(12) = "Map name:"
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN
  GOSUB savemap
  RETRACE
 END IF
 dummy = usemenu(csr, 0, 0, 12, 24)
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF csr = 0 THEN
   GOSUB savemap
   RETRACE
  END IF
  IF csr = 1 THEN GOSUB sizemap
  IF csr = 2 THEN
   npcdef npcstat(), pt
  END IF
  IF csr = 3 THEN
   GOSUB gmapdata
   loadpage game$ + ".til", gmap(0), 3
  END IF
  IF csr = 4 THEN GOSUB delmap
  IF csr = 5 THEN GOSUB linkdoor
  IF csr > 5 AND csr < 11 THEN editmode = csr - 6: GOSUB mapping
  IF csr = 11 THEN
   '--reload default passability
   temp$(0) = "No, Nevermind. No passability changes"
   temp$(1) = "Set default passability for whole map"
   IF sublist(1, temp$()) = 1 THEN
    FOR tx = 0 TO pass(0) - 1
     FOR ty = 0 TO pass(1) - 1
      setmapdata map(), pass(), 0, 0
      n = defaults(animadjust(readmapblock(tx, ty, 0), tastuf()))
      setmapdata pass(), pass(), 0, 0
      setmapblock tx, ty, 0, n
     NEXT ty
    NEXT tx
   END IF
  END IF
 END IF
 IF csr = 12 THEN strgrabber mapname$, 39
 list$(12) = "Map name:" + mapname$
 IF LEN(list$(12)) > 40 THEN list$(12) = mapname$
 
 standardmenu list$(), 12, 12, csr, 0, 0, 0, dpage, 0
 
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

gmapdata:
gmapmax = 18
gd = 0
gd$(-1) = "Previous Menu"
gd$(0) = "Map Tileset:"
gd$(1) = "Ambient Music:"
gd$(2) = "Minimap Available:"
gd$(3) = "Save Anywhere:"
gd$(4) = "Display Map Name:"
gd$(5) = "Map Edge Mode:"
gd$(6) = "Default Edge Tile:"
gd$(7) = "Autorun Script: "
gd$(8) = "Script Argument:"
gd$(9) = "Harm-Tile Damage:"
gd$(10) = "Harm-Tile Flash:"
gd$(11) = "Foot Offset:"
gd$(12) = "After-Battle Script:"
gd$(13) = "Instead-of-Battle Script:"
gd$(14) = "Each-Step Script:"
gd$(15) = "On-Keypress Script:"
gd$(16) = "Walkabout Layering:"
gd$(17) = "NPC Data:"
gd$(18) = "Tile Data:"
gdmax(0) = gen(genMaxTile):            gdmin(0) = 0
gdmax(1) = gen(genMaxSong) + 1:        gdmin(1) = -1
gdmax(2) = 1:                              gdmin(2) = 0
gdmax(3) = 1:                              gdmin(3) = 0
gdmax(4) = 255:                            gdmin(4) = 0
gdmax(5) = 2:                              gdmin(5) = 0
gdmax(6) = 255:                            gdmin(6) = 0
gdmax(7) = 32767:                          gdmin(7) = 0
gdmax(8) = 32767:                          gdmin(8) = -32767
gdmax(9) = 32767:                          gdmin(9) = -32767
gdmax(10) = 255:                           gdmin(10) = 0
gdmax(11) = 20:                            gdmin(11) = -20
gdmax(12) = 32767:                         gdmin(12) = 0
gdmax(13) = 32767:                         gdmin(13) = 0
gdmax(14) = 32767:                         gdmin(14) = 0
gdmax(15) = 32767:                         gdmin(15) = 0
gdmax(16) = 1:                             gdmin(16) = 0
gdmax(17) = 2:                             gdmin(16) = 0
gdmax(18) = 2:                             gdmin(16) = 0

gmapscrof(0) = 7
gmapscrof(1) = 12
gmapscrof(2) = 13
gmapscrof(3) = 14
gmapscrof(4) = 15

IF gmap(16) > 1 THEN gmap(16) = 0
FOR i = 0 TO gmapmax
 gmap(i) = bound(gmap(i), gdmin(i), gdmax(i))
NEXT i
FOR i = 0 TO 4
 gmapscr$(i) = scriptname$(gmap(gmapscrof(i)), plottrigger)
NEXT i
setkeys
DO
 setwait timing(), 120
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(gd, 0, -1, gmapmax, 24)
 SELECT CASE gd
  CASE -1
   IF keyval(57) > 1 OR keyval(28) > 1 THEN EXIT DO
  CASE 1
   zintgrabber(gmap(gd), gdmin(gd) - 1, gdmax(gd) - 1, 75, 77) 'song is optional
  CASE 7, 12 TO 15
   IF gd = 7 THEN idx = 0 ELSE idx = gd - 11
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    gmapscr$(idx) = scriptbrowse$(gmap(gd), plottrigger, "plotscript")
   ELSEIF scrintgrabber(gmap(gd), 0, 0, 75, 77, 1, plottrigger) THEN
    gmapscr$(idx) = scriptname$(gmap(gd), plottrigger)
   END IF
  CASE ELSE
   dummy = intgrabber(gmap(gd), gdmin(gd), gdmax(gd), 75, 77)
 END SELECT
 scri = 0
 FOR i = -1 TO gmapmax
  xtemp$ = ""
  SELECT CASE i
   CASE 0, 9
    xtemp$ = XSTR$(gmap(i))
   CASE 1
    IF gmap(1) = 0 THEN
     xtemp$ = " -silence-"
    ELSEIF gmap(1) = -1 THEN
     xtemp$ = " -none-"
    ELSE
     xtemp$ = XSTR$(gmap(1) - 1) + " " + getsongname$(gmap(1) - 1)
    END IF
   CASE 2, 3
    IF gmap(i) = 0 THEN xtemp$ = " NO" ELSE xtemp$ = " YES"
   CASE 4
    IF gmap(i) = 0 THEN xtemp$ = " NO" ELSE xtemp$ = XSTR$(gmap(i)) + " ticks"
   CASE 5
    SELECT CASE gmap(i)
     CASE 0
      xtemp$ = " Crop"
     CASE 1
      xtemp$ = " Wrap"
     CASE 2
      xtemp$ = " use default edge tile"
    END SELECT
   CASE 6
    IF gmap(5) = 2 THEN
     xtemp$ = XSTR$(gmap(i))
    ELSE
     xtemp$ = " N/A"
    END IF
   CASE 7, 12 TO 15
    xtemp$ = gmapscr$(scri)
    scri = scri + 1
   CASE 8
    IF gmap(7) = 0 THEN
     xtemp$ = " N/A"
    ELSE
     xtemp$ = XSTR$(gmap(i))
    END IF
   CASE 10
    IF gmap(i) = 0 THEN
     xtemp$ = " none"
    ELSE
     xtemp$ = XSTR$(gmap(i))
    END IF
   CASE 11
    SELECT CASE gmap(i)
     CASE 0
      xtemp$ = " none"
     CASE IS < 0
      xtemp$ = " up" + XSTR$(ABS(gmap(i))) + " pixels"
     CASE IS > 0
      xtemp$ = " down" + XSTR$(gmap(i)) + " pixels"
    END SELECT
   CASE 16
    IF gmap(i) = 1 THEN
     xtemp$ = " NPCs over Heroes"
    ELSE
     xtemp$ = " Heroes over NPCs"
    END IF
   CASE 17, 18
    SELECT CASE gmap(i)
     CASE 0
      xtemp$ = " Don't save state when leaving"
     CASE 1
      xtemp$ = " Remember state when leaving"
     CASE 2
      xtemp$ = " Always load afresh from RPG"
    END SELECT
  END SELECT
  textcolor 7, 0
  IF i = gd THEN textcolor 14 + tog, 0
  printstr gd$(i) + xtemp$, 0, 8 + (8 * i), dpage
  IF i = 10 THEN rectangle 4 + (8 * LEN(gd$(i) + xtemp$)), 8 + (8 * i), 8, 8, gmap(i), dpage
 NEXT i
 IF gmap(5) = 2 THEN
  '--show default edge tile
  setmapdata sampmap(), sampmap(), 180, 0
  setmapblock 0, 0, 0, gmap(6)
  drawmap 0, -180, 0, 0, dpage
  rectangle 20, 180, 300, 20, 240, dpage
 END IF
 
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

loadpasdefaults defaults(), gmap(0)
RETRACE

mapping:
DIM jiggle(0) AS INTEGER
clearpage 2
'--load NPC graphics--
FOR i = 0 TO 35
 setpicstuf buffer(), 1600, 2
 loadset game$ + ".pt4", npcstat(i * 15 + 0), 5 * i
 getpal16 pal16(), i, npcstat(i * 15 + 1), 4, npcstat(i * 15 + 0)
NEXT i
defpass = 1

setkeys
DO
 setwait timing(), 120
 setkeys
 if keyval(scCtrl) = 0 AND keyval(scAlt) = 0 then
	 IF keyval(59) > 1 THEN
	  editmode = 0
	 END IF
	 IF keyval(60) > 1 THEN
	  editmode = 1
	 END IF
	 IF keyval(61) > 1 THEN
	  editmode = 2
	 END IF
	 IF keyval(62) > 1 THEN
	  editmode = 3
	 END IF
	 IF keyval(63) > 1 THEN
	  editmode = 4
	 END IF
 else
 	for i = 0 to 2
 		if keyval(scAlt) AND keyval(scF1 + i) AND i > 0 then setbit(gmap(), 19, i-1, readbit(gmap(), 19, i-1) xor 1): if not readbit(gmap(), 19, i-1) then visible(i) = 0: layer = 0: visible(0) = 1
 		if keyval(scCtrl) AND keyval(scF1 + i) then
 			if i = 0 then
 				visible(i) = visible(i) xor 1
 			elseif readbit(gmap(), 19, i-1) then
 				visible(i) = visible(i) xor 1
 			end if
 		end if
 	next
 end if
 IF keyval(29) > 0 AND keyval(38) > 1 THEN gosub layermenu'ctrl-L
 IF keyval(1) > 1 THEN RETRACE
 IF keyval(15) > 1 THEN tiny = tiny XOR 1
 SELECT CASE editmode
  '---TILEMODE------
  CASE 0
   setmapdata map(), pass(), 20, 0
   IF keyval(33) > 1 AND keyval(29) > 0 THEN' Alt+F Fill screen
    FOR i = 0 TO 14
     FOR o = 0 TO 8
      setmapblock mapx \ 20 + i, mapy \ 20 + o, layer, usetile(layer)
      IF defpass THEN setpassblock mapx \ 20 + i, mapy \ 20 + o, defaults(usetile(layer))
     NEXT o
    NEXT i
    setmapdata map(), pass(), 20, 0
   END IF
   IF keyval(29) > 0 AND keyval(36) > 1 THEN
     setbit jiggle(), 0, layer, (readbit(jiggle(), 0, layer) XOR 1)
   END IF
   IF keyval(41) > 1 THEN GOSUB minimap
   IF keyval(28) > 1 THEN GOSUB pickblock
   IF keyval(57) > 0 THEN
    IF defpass THEN setpassblock x, y, defaults(usetile(layer))
    setmapblock x, y, layer, usetile(layer)
   END IF
   IF keyval(58) > 1 THEN 'grab tile
    usetile(layer) = animadjust(readmapblock(x, y, layer), tastuf())
	GOSUB updatetilepicker
   END IF
   IF keyval(29) > 0 AND keyval(32) > 1 THEN defpass = defpass XOR 1   
   FOR i = 0 TO 1
   	FOR l = 0 to 2
    IF keyval(2 + i) > 1 THEN
     old = readmapblock(x, y, l)
     IF old > 159 + (i * 48) THEN
      new = (old - (160 + (i * 48))) + tastuf(i * 20)
     ELSE
      IF old >= tastuf(i * 20) AND old < tastuf(i * 20) + 48 THEN
       new = 160 + (i * 48) + (old - tastuf(i * 20))
      END IF
     END IF
     IF keyval(29) = 0 THEN
      setmapblock x, y, l, new
     ELSE
      FOR tx = 0 TO map(0)
       FOR ty = 0 TO map(1)
				IF readmapblock(tx, ty, l) = old THEN setmapblock tx, ty, l, new
       NEXT ty
      NEXT tx
     END IF
    END IF
    Next l
   NEXT i
   IF keyval(51) > 0 AND usetile(layer) > 0 THEN
    usetile(layer) = usetile(layer) - 1
    GOSUB updatetilepicker
   END IF
   IF keyval(52) > 0 AND usetile(layer) < 159 THEN
    usetile(layer) = usetile(layer) + 1
    GOSUB updatetilepicker
   END IF
   '---PASSMODE-------
  CASE 1
   setmapdata pass(), pass(), 20, 0
   over = readmapblock(x, y, 0)
   IF keyval(57) > 1 AND (over AND 15) = 0 THEN setmapblock x, y, 0, 15
   IF keyval(57) > 1 AND (over AND 15) = 15 THEN setmapblock x, y, 0, 0
   IF keyval(57) > 1 AND (over AND 15) > 0 AND (over AND 15) < 15 THEN setmapblock x, y, 0, 0
   IF keyval(29) > 0 THEN
    IF keyval(72) > 1 THEN setmapblock x, y, 0, (over XOR 1)
    IF keyval(77) > 1 THEN setmapblock x, y, 0, (over XOR 2)
    IF keyval(80) > 1 THEN setmapblock x, y, 0, (over XOR 4)
    IF keyval(75) > 1 THEN setmapblock x, y, 0, (over XOR 8)
   END IF
   IF keyval(30) > 1 THEN setmapblock x, y, 0, (over XOR 16) 'vehicle A
   IF keyval(48) > 1 THEN setmapblock x, y, 0, (over XOR 32) 'vehicle B
   IF keyval(35) > 1 THEN setmapblock x, y, 0, (over XOR 64) 'harm tile
   IF keyval(24) > 1 THEN setmapblock x, y, 0, (over XOR 128)'overhead
   '---DOORMODE-----
  CASE 2
   IF keyval(57) > 1 THEN
    temp = 0
    FOR i = 0 TO 99
     IF doors(i) = x AND doors(i + 100) = y + 1 AND doors(i + 200) = 1 THEN temp = 1: doors(i + 200) = 0
    NEXT
    IF temp = 0 THEN
     temp = -1
     FOR i = 99 TO 0 STEP -1
      IF doors(i + 200) = 0 THEN temp = i
     NEXT
     IF temp >= 0 THEN doors(0 + temp) = x: doors(100 + temp) = y + 1: doors(200 + temp) = 1
    END IF
   END IF
   '---NPCMODE------
  CASE 3
   
   IF keyval(83) > 1 THEN
    FOR i = 0 TO 299
     IF npc(i + 600) > 0 THEN
      IF npc(i + 0) = x AND npc(i + 300) = y + 1 THEN npc(i + 600) = 0
     END IF
    NEXT i
   END IF
   nd = -1
   IF keyval(72) > 1 THEN nd = 0
   IF keyval(77) > 1 THEN nd = 1
   IF keyval(80) > 1 THEN nd = 2
   IF keyval(75) > 1 THEN nd = 3
   IF keyval(57) > 1 OR (keyval(29) > 0 AND nd > -1) THEN
    temp = 0
    IF nd = -1 THEN
     FOR i = 0 TO 299
      IF npc(i + 600) > 0 THEN
       IF npc(i + 0) = x AND npc(i + 300) = y + 1 THEN npc(i + 600) = 0: temp = 1
      END IF
     NEXT i
    END IF
    IF nd = -1 THEN nd = 2
    IF temp = 0 THEN
     temp = -1
     FOR i = 299 TO 0 STEP -1
      IF npc(i + 600) = 0 THEN temp = i
     NEXT i
     IF temp >= 0 THEN npc(temp + 0) = x: npc(temp + 300) = y + 1: npc(temp + 600) = nptr + 1: npc(temp + 900) = nd
    END IF
   END IF
   IF keyval(51) > 0 THEN nptr = nptr - 1: IF nptr < 0 THEN nptr = 35
   IF keyval(52) > 0 THEN nptr = nptr + 1: IF nptr > 35 THEN nptr = 0
   '---FOEMODE--------
  CASE 4
   IF keyval(51) > 0 THEN foe = loopvar(foe, 0, 255, -1)
   IF keyval(52) > 0 THEN foe = loopvar(foe, 0, 255, 1)
   IF keyval(57) > 0 THEN setmapdata emap(), pass(), 20, 0: setmapblock x, y, 0, foe: setmapdata map(), pass(), 20, 0
   IF keyval(33) > 1 AND keyval(29) > 0 THEN
    setmapdata emap(), pass(), 20, 0
    FOR i = 0 TO 14
     FOR o = 0 TO 8
      setmapblock INT(mapx / 20) + i, INT(mapy / 20) + o, 0, foe
     NEXT o
    NEXT i
    setmapdata map(), pass(), 20, 0
   END IF
   IF keyval(58) > 1 THEN foe = readmapblock(x, y, 0)
   '--done input-modes-------
 END SELECT
 
 setmapdata map(), pass(), 20, 0
 
 '--general purpose controls----
 IF keyval(56) = 0 AND keyval(29) = 0 THEN
  IF keyval(72) > 0 THEN y = large(y - 1, 0): IF y < INT(mapy / 20) AND mapy > 0 THEN mapy = mapy - 20
  IF keyval(80) > 0 THEN y = small(y + 1, high - 1): IF y > INT(mapy / 20) + 8 AND mapy < (high * 20) - 180 THEN mapy = mapy + 20
  IF keyval(75) > 0 THEN x = large(x - 1, 0): IF x < INT(mapx / 20) AND mapx > 0 THEN mapx = mapx - 20
  IF keyval(77) > 0 THEN x = small(x + 1, wide - 1): IF x > INT(mapx / 20) + 14 AND mapx < (wide * 20) - 300 THEN mapx = mapx + 20
 END IF
 IF keyval(56) > 0 AND keyval(29) = 0 THEN
  IF keyval(72) > 0 AND mapy > 0 THEN mapy = mapy - 20: y = y - 1
  IF keyval(80) > 0 AND mapy < (high * 20) - 180 THEN mapy = mapy + 20: y = y + 1
  IF keyval(75) > 0 AND mapx > 0 THEN mapx = mapx - 20: x = x - 1
  IF keyval(77) > 0 AND mapx < ((wide + 1) * 20) - 320 THEN mapx = mapx + 20: x = x + 1
 END IF
 
 IF keyval(scPageup) > 1 then
 	for i = layer+1 to 2
 		if readbit(gmap(), 19, i-1) then
 			layer = i
 			visible(i) = 1
			GOSUB updatetilepicker
 			exit for
 		end if
 	next
 end if
 	 
 IF keyval(scPageDown) > 1 then
 	for i = layer-1 to 0 step -1
 		if i > 0 THEN
 			if readbit(gmap(), 19, i-1) then
	 			layer = i
	 			visible(i) = 1
				GOSUB updatetilepicker
	 			exit for
	 		end if
	 	ELSE
	 		layer = i
	 		visible(i) = 1
			GOSUB updatetilepicker
	 		exit for
 		end if
 	next
 end if
 
 tog = tog XOR 1
 flash = loopvar(flash, 0, 3, 1)
 
 '--draw menubar
 IF editmode = 0 THEN
  setmapdata menubar(), pass(), 0, 180
  drawmap menubarstart(layer) * 20, 0, 0, 0, dpage
 ELSE
  rectangle 0, 0, 320, 20, 0, dpage
 END IF
 
 '--draw map
 setmapdata map(), pass(), 20, 0
 setanim tastuf(0) + cycle(0), tastuf(20) + cycle(1)
 cycletile cycle(), tastuf(), cycptr(), cycskip()
 rectangle 0, 20, 320, 180, 0, dpage
 for i = 0 to 2
 	if visible(i) then
		jigx = 0: jigy = 0
		if readbit(jiggle(), 0, i) and tog then
			if i = 0 then jigx = 1
			if i = 1 then jigy = 1
			if i = 2 then jigx = -1: jigy = -1
		end if
		drawmap mapx + jigx, mapy + jigy - 20, i, 0, dpage, i <> 0
	end if
 next

 
 '--show passmode overlay
 IF editmode = 1 THEN
  setmapdata pass(), pass(), 20, 0
  FOR o = 0 TO 8
   FOR i = 0 TO 15
    over = readmapblock(INT(mapx / 20) + i, INT(mapy / 20) + o, 0)
    IF (over AND 1) THEN rectangle i * 20, o * 20 + 20, 20, 3, 7 + tog, dpage
    IF (over AND 2) THEN rectangle i * 20 + 17, o * 20 + 20, 3, 20, 7 + tog, dpage
    IF (over AND 4) THEN rectangle i * 20, o * 20 + 37, 20, 3, 7 + tog, dpage
    IF (over AND 8) THEN rectangle i * 20, o * 20 + 20, 3, 20, 7 + tog, dpage
    textcolor 14 + tog, 0
    IF (over AND 16) THEN printstr "A", i * 20, o * 20 + 20, dpage
    IF (over AND 32) THEN printstr "B", i * 20 + 10, o * 20 + 20, dpage
    IF (over AND 64) THEN printstr "H", i * 20, o * 20 + 30, dpage
    IF (over AND 128) THEN printstr "O", i * 20 + 10, o * 20 + 30, dpage
   NEXT i
  NEXT o
 END IF
 
 '--door display--
 IF editmode = 2 THEN
  textcolor 240, 0
  FOR i = 0 TO 99
   IF doors(i) >= INT(mapx / 20) AND doors(i) < INT(mapx / 20) + 16 AND doors(i + 100) > INT(mapy / 20) AND doors(i + 100) <= INT(mapy / 20) + 9 AND doors(i + 200) = 1 THEN
    rectangle doors(i) * 20 - mapx, doors(i + 100) * 20 - mapy, 20, 20, 15 - tog, dpage
    printstr STR$(i), doors(i) * 20 - mapx + 10 - (4 * LEN(STR$(i))), doors(i + 100) * 20 - mapy + 6, dpage
   END IF
  NEXT
 END IF
 
 '--npc display--
 IF editmode = 3 THEN
  FOR i = 0 to 35
   npcnum(i) = 0
  NEXT
  walk = walk + 1: IF walk > 3 THEN walk = 0
  FOR i = 0 TO 299
   IF npc(i + 600) > 0 THEN
    IF npc(i + 0) >= INT(mapx / 20) AND npc(i + 0) < INT(mapx / 20) + 16 AND npc(i + 300) > INT(mapy / 20) AND npc(i + 300) <= INT(mapy / 20) + 9 THEN
     loadsprite cursor(), 0, 400 * npc(i + 900) + (200 * INT(walk / 2)), 5 * (npc(i + 600) - 1), 20, 20, 2
     drawsprite cursor(), 0, pal16(), 16 * (npc(i + 600) - 1), npc(i) * 20 - mapx, npc(i + 300) * 20 - mapy, dpage
     textcolor 14 + tog, 0
     xtemp$ = STR$(npc(i + 600) - 1)
     printstr xtemp$, npc(i) * 20 - mapx, npc(i + 300) * 20 - mapy + 3, dpage
     xtemp$ = STR$(npcnum(npc(i + 600)-1))
     printstr xtemp$, npc(i) * 20 - mapx, npc(i + 300) * 20 - mapy + 12, dpage
    END IF
    npcnum(npc(i + 600) - 1) = npcnum(npc(i + 600) - 1) + 1
   END IF
  NEXT
 END IF
 
 '--position finder--
 IF tiny = 1 THEN rectangle 0, 20, wide, high, 1, dpage: rectangle mapx / 20, (mapy / 20) + 20, 15, 9, 10, dpage
 
 '--normal cursor--
 IF editmode <> 3 THEN
  drawsprite cursor(), 200 * (1 + tog), cursorpal(), 0, (x * 20) - mapx, (y * 20) - mapy + 20, dpage
  IF editmode = 0 THEN drawsprite cursor(), 200 * (1 + tog), cursorpal(), 0, ((usetile(layer) - menubarstart(layer)) * 20), 0, dpage
 END IF
 
 '--npc placement cursor--
 IF editmode = 3 THEN
  loadsprite cursor(), 0, (walk * 400), nptr * 5, 20, 20, 2
  drawsprite cursor(), 0, pal16(), 16 * nptr, (x * 20) - mapx, (y * 20) - mapy + 20, dpage
  textcolor 14 + tog, 0
  xtemp$ = STR$(nptr)
  printstr xtemp$, (x * 20) - mapx, (y * 20) - mapy + 28, dpage
 END IF
 
 '--show foemap--
 IF editmode = 4 THEN
  setmapdata emap(), pass(), 20, 0
  textcolor 14 + tog, 0
  FOR i = 0 TO 14
   FOR o = 0 TO 8
    temp = readmapblock(INT(mapx / 20) + i, INT(mapy / 20) + o, 0)
    IF temp > 0 THEN printstr STR$(temp), i * 20 - ((temp < 10) * 5), o * 20 + 26, dpage
   NEXT o
  NEXT i
 END IF
 
 textcolor 14 + tog, 0
 printstr "Layer " & layer, 0, 180, dpage
 printstr "X " & x & "   Y " & y, 0, 192, dpage
 setmapdata map(), pass(), 20, 0
 rectangle 300, 0, 20, 200, 0, dpage
 rectangle 0, 19, 320, 1, 15, dpage
 IF editmode = 0 THEN
  status$ = "Default Passability "
  IF defpass THEN status$ = status$ + "On" ELSE status$ = status$ + "Off"
  printstr status$, 124, 192, dpage
 END IF
 textcolor 15, 0
 printstr mode$(editmode), 0, 24, dpage
 IF editmode = 4 THEN textcolor 15, 1: printstr "Formation Set:" + XSTR$(foe), 0, 16, dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

updatetilepicker:
	menubarstart(layer) = bound(menubarstart(layer), large(usetile(layer) - 14, 0), small(usetile(layer), 145))
	by = INT(usetile(layer) / 16)
	bx = usetile(layer) - (by * 16)
RETRACE

pickblock:
setkeys
DO
 setwait timing(), 120
 setkeys
 IF keyval(28) > 1 OR keyval(1) > 1 THEN menu = usetile(layer): EXIT DO
 IF keyval(72) > 0 AND by > 0 THEN by = by - 1: usetile(layer) = usetile(layer) - 16
 IF keyval(80) > 0 AND by < 9 THEN by = by + 1: usetile(layer) = usetile(layer) + 16
 IF keyval(75) > 0 AND bx > 0 THEN bx = bx - 1: usetile(layer) = usetile(layer) - 1
 IF keyval(77) > 0 AND bx < 15 THEN bx = bx + 1: usetile(layer) = usetile(layer) + 1
 IF keyval(51) > 0 AND usetile(layer) > 0 THEN usetile(layer) = usetile(layer) - 1: bx = bx - 1: IF bx < 0 THEN bx = 15: by = by - 1
 IF keyval(52) > 0 AND usetile(layer) < 159 THEN usetile(layer) = usetile(layer) + 1: bx = bx + 1: IF bx > 15 THEN bx = 0: by = by + 1
 tog = tog XOR 1
 loadsprite cursor(), 0, 0, 0, 20, 20, 2
 drawsprite cursor(), 200 * (1 + tog), cursorpal(), 0, bx * 20, by * 20, dpage
 copypage dpage, vpage
 copypage 3, dpage
 dowait
LOOP
GOSUB updatetilepicker
RETRACE

sizemap:
clearpage 2
tempx = 0: tempy = 0
tempw = wide: temph = high
setmapdata map(), pass(), 20, 0
drawmini high, wide, cursor(), 2, tastuf()
setkeys
DO
 setwait timing(), 100
 setkeys
 IF keyval(1) > 1 THEN RETRACE
 IF keyval(28) > 1 THEN
  GOSUB dosizemap
  RETRACE
 END IF
 IF keyval(29) THEN
  IF keyval(72) > 0 THEN tempy = tempy - (1 + (keyval(56) * 8)): tempy = large(tempy, 0)
  IF keyval(80) > 0 THEN tempy = tempy + (1 + (keyval(56) * 8)): tempy = small(tempy, high - temph)
  IF keyval(75) > 0 THEN tempx = tempx - (1 + (keyval(56) * 8)): tempx = large(tempx, 0)
  IF keyval(77) > 0 THEN tempx = tempx + (1 + (keyval(56) * 8)): tempx = small(tempx, wide - tempw)
  tempx = large(tempx, 0)
  tempy = large(tempy, 0)
 ELSE
  IF keyval(72) > 0 THEN temph = temph - (1 + (keyval(56) * 8)): temph = large(temph, 10)
  IF keyval(80) > 0 THEN temph = temph + (1 + (keyval(56) * 8)): temph = small(temph, 32000): WHILE temph * tempw > 32000 AND tempw > 16: tempw = tempw - 1: WEND
  IF keyval(75) > 0 THEN tempw = tempw - (1 + (keyval(56) * 8)): tempw = large(tempw, 16)
  IF keyval(77) > 0 THEN tempw = tempw + (1 + (keyval(56) * 8)): tempw = small(tempw, 32000): WHILE temph * tempw > 32000 AND temph > 10: temph = temph - 1: WEND
  th& = temph
  tw& = tempw
  WHILE th& * tw& >= 32000
   temph = large(temph - 1, 10)
   tempw = large(tempw - 1, 16)
   th& = temph
   tw& = tempw
  WEND
 END IF
 edgeprint "width" + XSTR$(wide) + CHR$(26) + STR$(tempw), 1, 1, 7, dpage
 edgeprint "height" + XSTR$(high) + CHR$(26) + STR$(temph), 1, 11, 7, dpage
 edgeprint "area" + XSTR$(wide * high) + CHR$(26) + STR$(temph * tempw), 1, 21, 7, dpage
 rectangle tempx, tempy, tempw, 1, 14 + tog, dpage
 rectangle tempx, tempy, 1, temph, 14 + tog, dpage
 rectangle tempx, tempy + temph, tempw, 1, 14 + tog, dpage
 rectangle tempx + tempw, tempy, 1, temph, 14 + tog, dpage
 copypage dpage, vpage
 copypage 2, dpage
 dowait
LOOP

dosizemap:
clearpage 0
clearpage 1
yout = 0
edgeprint "TILEMAP", 0, yout * 10, 15, vpage: yout = yout + 1
sizemar map(), wide, high, tempx, tempy, tempw, temph, yout, vpage, 1
edgeprint "PASSMAP", 0, yout * 10, 15, vpage: yout = yout + 1
sizemar pass(), wide, high, tempx, tempy, tempw, temph, yout, vpage, 0
edgeprint "FOEMAP", 0, yout * 10, 15, vpage: yout = yout + 1
sizemar emap(), wide, high, tempx, tempy, tempw, temph, yout, vpage, 0
setmapdata map(), pass(), 20, 0
wide = map(0): high = map(1)
'--reset map scroll position
x = 0: y = 0: mapx = 0: mapy = 0
edgeprint "Aligning and truncating doors", 0, yout * 10, 15, vpage: yout = yout + 1
FOR i = 0 TO 99
 doors(i) = doors(i) - tempx
 doors(i + 100) = doors(i + 100) - tempy
 IF doors(i) < 0 OR doors(i + 100) < 0 OR doors(i) > wide OR doors(i + 100) > high THEN
  doors(i + 200) = 0
 END IF
NEXT
edgeprint "Aligning and truncating NPCs", 0, yout * 10, 15, vpage: yout = yout + 1
FOR i = 0 TO 299
 npc(i + 0) = npc(i + 0) - tempx
 npc(i + 300) = npc(i + 300) - tempy
 IF npc(i + 0) < 0 OR npc(i + 300) < 0 OR npc(i + 0) > wide OR npc(i + 300) > high THEN
  npc(i + 600) = 0
 END IF
NEXT i
GOSUB verifymap
RETRACE

minimap:
clearpage vpage
setmapdata map(), pass(), 20, 0
drawmini high, wide, cursor(), vpage, tastuf()
printstr "Press Any Key", 0, 180, vpage
setvispage vpage
w = getkey
RETRACE

delmap:
setvispage vpage
temp$(0) = "Do Not Delete"
temp$(1) = "Delete Map"
yesno = sublist(1, temp$())
IF yesno = 1 THEN
 printstr "Please Wait...", 0, 40, vpage
 map(0) = 32: map(1) = 20
 pass(0) = 32: pass(1) = 20
 emap(0) = 32: emap(1) = 20
 FOR i = 2 TO ubound(map)
  rectangle INT(i * .02), 180, 2, 10, 15, vpage
  map(i) = 0
 NEXT i
 for i = 2 to 16002
 	rectangle INT(i * .02), 180, 2, 10, 0, vpage
 	pass(i) = 0
  emap(i) = 0
 next
 '---FLUSH DOOR LINKS---
 FOR i = 0 TO 1000
  link(i) = 0
 NEXT i
 '---FLUSH NPC LOCATIONS---
 FOR i = 0 TO 900
  npc(i) = 0
 NEXT i
 '---FLUSH DOOR LOCATIONS---
 FOR i = 0 TO 99
  doors(i) = 0
  doors(i + 100) = 0
  doors(i + 200) = 0
 NEXT
 xBSAVE maplumpname$(pt, "t"), map(), map(0) * map(1) * 3 + 4
 xBSAVE maplumpname$(pt, "p"), pass(), pass(0) * pass(1) + 4
 xBSAVE maplumpname$(pt, "e"), emap(), emap(0) * emap(1) + 4
 xBSAVE maplumpname$(pt, "d"), link(), 2000
 xBSAVE maplumpname$(pt, "l"), npc(), 3000
 setpicstuf doors(), 600, -1
 storeset game$ + ".dox", pt, 0
END IF
'--reset scroll position
wide = map(0): high = map(1)
x = 0: y = 0: mapx = 0: mapy = 0
layer = 0
visible(0) = 1: visible(1) = 0: visible(2) = 0
RETRACE

addmap:
how = addmaphow
'-- -2  =Cancel
'-- -1  =New blank
'-- >=0 =Copy
IF how = -1 THEN GOSUB newblankmap
IF how >= 0 THEN GOSUB copymap
RETRACE

copymap:
'--increment map count
gen(0) = gen(0) + 1
'--load the source map
pt = how
GOSUB loadmap
'-- save the new map
pt = gen(0)
GOSUB savemap
RETRACE

newblankmap:
'--increment map count
gen(0) = gen(0) + 1
'--flush map buffers
flusharray map(), 16002, 0
flusharray pass(), 16002, 0
flusharray emap(), 16002, 0
flusharray link(), 1000, 0
flusharray npc(), 900, 0
flusharray npcstat(), 1500, 0
flusharray doors(), 299, 0
'--setup default new map size
map(0) = 64: map(1) = 64
pass(0) = 64: pass(1) = 64
emap(0) = 64: emap(1) = 64
'--save map buffers
xBSAVE maplumpname$(gen(0), "t"), map(), map(0) * map(1) * 3 + 4
xBSAVE maplumpname$(gen(0), "p"), pass(), pass(0) * pass(1) + 4
xBSAVE maplumpname$(gen(0), "e"), emap(), emap(0) * emap(1) + 4
xBSAVE maplumpname$(gen(0), "d"), link(), 2000
xBSAVE maplumpname$(gen(0), "n"), npcstat(), 3000
xBSAVE maplumpname$(gen(0), "l"), npc(), 3000
setpicstuf doors(), 600, -1
storeset game$ + ".dox", gen(0), 0
'--setup map name
buffer(0) = 0
setpicstuf buffer(), 80, -1
storeset game$ + ".mn", gen(0), 0
RETRACE

savemap:
setpicstuf gmap(), 40, -1
storeset game$ + ".map", pt, 0
xBSAVE maplumpname$(pt, "t"), map(), map(0) * map(1) * 3 + 4
xBSAVE maplumpname$(pt, "p"), pass(), pass(0) * pass(1) + 4
xBSAVE maplumpname$(pt, "e"), emap(), emap(0) * emap(1) + 4
xBSAVE maplumpname$(pt, "l"), npc(), 3000
xBSAVE maplumpname$(pt, "d"), link(), 2000
xBSAVE maplumpname$(pt, "n"), npcstat(), 3000
setpicstuf doors(), 600, -1
storeset game$ + ".dox", pt, 0
'--save map name
buffer(0) = LEN(mapname$)
str2array LEFT$(mapname$, 39), buffer(), 1
setpicstuf buffer(), 80, -1
storeset game$ + ".mn", pt, 0
RETRACE

loadmap:
setpicstuf gmap(), 40, -1
loadset game$ + ".map", pt, 0
visible(0) = 1
for i = 0 to 1
	visible(i + 1) = readbit(gmap(), 19, i)
next
loadpage game$ + ".til", gmap(0), 3
loadtanim gmap(0), tastuf()
FOR i = 0 TO 1
 cycle(i) = 0
 cycptr(i) = 0
 cycskip(i) = 0
NEXT i
xbload maplumpname$(pt, "t"), map(), "tilemap lump is missing!"
xbload maplumpname$(pt, "p"), pass(), "passmap lump is missing!"
xbload maplumpname$(pt, "e"), emap(), "foemap lump is missing!"
xbload maplumpname$(pt, "l"), npc(), "npclocation lump is missing!"
xbload maplumpname$(pt, "n"), npcstat(), "npcstat lump is missing!"
xbload maplumpname$(pt, "d"), link(), "doorlink lump is missing!"
setpicstuf doors(), 600, -1
loadset game$ + ".dox", pt, 0
wide = map(0): high = map(1)
if ubound(map) < wide * high * 3 + 1 then redim preserve map(wide*high*3+1) 'three layers + 2 fields - 1 offset
mapname$ = getmapname$(pt)
loadpasdefaults defaults(), gmap(0)
GOSUB verifymap
RETRACE

verifymap:
IF map(0) <> pass(0) OR map(0) <> emap(0) OR map(1) <> pass(1) OR map(1) <> emap(1) THEN
 '--Map's X and Y do not match
 clearpage vpage
 j = 0
 textcolor 15, 0
 printstr "Map" + filenum$(pt) + ":" + mapname$, 0, j * 8, vpage: j = j + 1
 j = j + 1
 printstr "this map seems to be corrupted", 0, j * 8, vpage: j = j + 1
 j = j + 1
 printstr " TileMap" + XSTR$(map(0)) + "*" + STR$(map(1)) + " tiles", 0, j * 8, vpage: j = j + 1
 printstr " WallMap" + XSTR$(pass(0)) + "*" + STR$(pass(1)) + " tiles", 0, j * 8, vpage: j = j + 1
 printstr " FoeMap" + XSTR$(emap(0)) + "*" + STR$(emap(1)) + " tiles", 0, j * 8, vpage: j = j + 1
 j = j + 1
 printstr "What is the correct size?", 0, j * 8, vpage: j = j + 1
 DO
  textcolor 14, 240
  setkeys
  DO
   setwait timing(), 100
   setkeys
   IF keyval(1) > 1 OR keyval(28) > 1 THEN EXIT DO
   dummy = intgrabber(wide, 0, 9999, 75, 77)
   printstr "Width:" + XSTR$(wide) + "   ", 0, j * 8, vpage
   dowait
  LOOP
  j = j + 1
  setkeys
  DO
   setwait timing(), 100
   setkeys
   IF keyval(1) > 1 OR keyval(28) > 1 THEN EXIT DO
   dummy = intgrabber(high, 0, 9999, 75, 77)
   printstr "Height:" + XSTR$(high) + "    ", 0, j * 8, vpage
   dowait
  LOOP
  textcolor 15, 0
  IF wide * high < 32000 AND wide > 0 AND high > 0 THEN EXIT DO
  j = j - 2
  printstr "What is the correct size? (bad size!)", 0, j * 8, vpage: j = j + 1
 LOOP
 map(0) = wide: map(1) = high
 pass(0) = wide: pass(1) = high
 emap(0) = wide: emap(1) = high
 j = j + 2
 printstr "please report this error to", 0, j * 8, vpage: j = j + 1
 printstr "ohrrpgce@HamsterRepublic.com", 0, j * 8, vpage: j = j + 1
 w = getkey
END IF
RETRACE

loadmenu:
setmapdata menubar(), pass(), 180, 0
FOR i = 0 TO 159
 setmapblock i, 0, 0, i
NEXT
RETRACE

linkdoor:
GOSUB savemap
ulim(0) = 99: llim(0) = 0
ulim(1) = 99: llim(1) = 0
ulim(2) = gen(0): llim(2) = 0
ulim(3) = 999: llim(3) = -999
ulim(4) = 999: llim(4) = -999
ttop = 0: cur = 0
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN xBSAVE maplumpname$(pt, "d"), link(), 2000: RETRACE
 'IF keyval(72) > 1 AND cur > 0 THEN cur = cur - 1: IF cur < ttop THEN ttop = ttop - 1
 'IF keyval(80) > 1 AND cur < 199 THEN cur = cur + 1: IF cur > ttop + 10 THEN ttop = ttop + 1
 dummy = usemenu(cur, ttop, 0, 199, 10)
 IF keyval(28) > 1 OR keyval(57) > 1 THEN GOSUB seedoors
 FOR i = ttop TO ttop + 10
  textcolor 7, 0
  IF cur = i THEN textcolor 14 + tog, 0
  a$ = "Door" + XSTR$(link(i)) + " leads to door" + XSTR$(link(i + 200)) + " on map" + XSTR$(link(i + 400))
  printstr a$, 0, 2 + (i - ttop) * 16, dpage
  a$ = "  only if tag" + XSTR$(ABS(link(i + 600))) + " =" + XSTR$(SGN(SGN(link(i + 600)) + 1)) + " and tag" + XSTR$(ABS(link(i + 800))) + " =" + XSTR$(SGN(SGN(link(i + 800)) + 1))
  IF link(i + 600) = 0 AND link(i + 800) <> 0 THEN a$ = "  only if tag" + XSTR$(ABS(link(i + 800))) + " =" + XSTR$(SGN(SGN(link(i + 800)) + 1))
  IF link(i + 600) <> 0 AND link(i + 800) = 0 THEN a$ = "  only if tag" + XSTR$(ABS(link(i + 600))) + " =" + XSTR$(SGN(SGN(link(i + 600)) + 1))
  IF link(i + 600) = 0 AND link(i + 800) = 0 THEN a$ = "  all the time"
  printstr a$, 0, 10 + (i - ttop) * 16, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

seedoors:
xBSAVE maplumpname$(pt, "t"), map(), map(0) * map(1) + 4
menu$(-1) = "Go Back"
menu$(0) = "Entrance Door"
menu$(1) = "Exit Door"
menu$(2) = "Exit Map"
menu$(3) = "Require Tag"
menu$(4) = "Require Tag"
cur2 = -1
sdwait = 0
outmap$ = getmapname$(link(cur + 400))
GOSUB showldoor
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF sdwait > 0 THEN
  sdwait = sdwait - 1
  IF sdwait = 0 THEN GOSUB showldoor
 END IF
 IF keyval(1) > 1 THEN RETRACE
 'IF keyval(72) > 1 THEN cur2 = cur2 - 1: IF cur2 < -1 THEN cur2 = 4
 'IF keyval(80) > 1 THEN cur2 = cur2 + 1: IF cur2 > 4 THEN cur2 = -1
 dummy = usemenu(cur2, 0, -1, 4, 24)
 IF cur2 >= 0 THEN
  IF intgrabber(link(cur + (cur2 * 200)), llim(cur2), ulim(cur2), 75, 77) THEN sdwait = 3: outmap$ = getmapname$(link(cur + 400))
 ELSE
  IF keyval(28) > 1 OR keyval(57) > 1 THEN RETRACE
 END IF
 rectangle 0, 100, 320, 2, 1 + tog, dpage
 FOR i = -1 TO 4
  xtemp$ = ""
  IF i >= 0 AND i <= 2 THEN xtemp$ = XSTR$(link(cur + (i * 200)))
  IF i > 2 THEN
   IF link(cur + (i * 200)) THEN
    xtemp$ = XSTR$(ABS(link(cur + (i * 200)))) + " = " + onoroff$(link(cur + (i * 200))) + " (" + lmnemonic$(ABS(link(cur + (i * 200)))) + ")"
   ELSE
    xtemp$ = " 0 [N/A]"
   END IF
  END IF
  col = 7: IF cur2 = i THEN col = 14 + tog
  edgeprint menu$(i) + xtemp$, 1, 1 + (i + 1) * 10, col, dpage
 NEXT i
 edgeprint "ENTER", 275, 0, 15, dpage
 edgeprint "EXIT", 283, 190, 15, dpage
 edgeprint outmap$, 0, 190, 15, dpage
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

showldoor:
clearpage 2
setmapdata map(), pass(), 0, 101
IF doors(link(cur + (0 * 200)) + 200) = 1 THEN
 dmx = doors(link(cur + (0 * 200))) * 20 - 150
 dmy = doors(link(cur + (0 * 200)) + 100) * 20 - 65
 dmx = small(large(dmx, 0), map(0) * 20 - 320)
 dmy = small(large(dmy, 0), map(1) * 20 - 100)
 drawmap dmx, dmy, 0, 0, 2
 rectangle doors(link(cur + (0 * 200))) * 20 - dmx, doors(link(cur + (0 * 200)) + 100) * 20 - dmy - 20, 20, 20, 240, 2
 rectangle 1 + doors(link(cur + (0 * 200))) * 20 - dmx, 1 + doors(link(cur + (0 * 200)) + 100) * 20 - dmy - 20, 18, 18, 7, 2
 textcolor 240, 0
 xtemp$ = XSTR$(link(cur + (0 * 200)))
 printstr RIGHT$(xtemp$, LEN(xtemp$) - 1), doors(link(cur + (0 * 200))) * 20 - dmx + 10 - (4 * LEN(xtemp$)), doors(link(cur + (0 * 200)) + 100) * 20 - dmy - 14, 2
END IF
'-----------------EXIT DOOR
setpicstuf destdoor(), 600, -1
loadset game$ + ".dox", link(cur + (2 * 200)), 0
xbload maplumpname$(link(cur + (2 * 200)), "t"), map(), "Could not find map" + filenum$(link(cur + (2 * 200)))
setpicstuf buffer(), 40, -1
loadset game$ + ".map", link(cur + (2 * 200)), 0
loadpage game$ + ".til", buffer(0), 3
setmapdata map(), pass(), 101, 0
IF destdoor(link(cur + (1 * 200)) + 200) = 1 THEN
 dmx = destdoor(link(cur + (1 * 200))) * 20 - 150
 dmy = destdoor(link(cur + (1 * 200)) + 100) * 20 - 65
 dmx = small(large(dmx, 0), map(0) * 20 - 320)
 dmy = small(large(dmy, 0), map(1) * 20 - 100)
 drawmap dmx, dmy - 100, 0, 0, 2
 rectangle destdoor(link(cur + (1 * 200))) * 20 - dmx, destdoor(link(cur + (1 * 200)) + 100) * 20 - dmy + 80, 20, 20, 240, 2
 rectangle 1 + destdoor(link(cur + (1 * 200))) * 20 - dmx, 1 + destdoor(link(cur + (1 * 200)) + 100) * 20 - dmy + 80, 18, 18, 7, 2
 textcolor 240, 0
 xtemp$ = XSTR$(link(cur + (1 * 200)))
 printstr RIGHT$(xtemp$, LEN(xtemp$) - 1), destdoor(link(cur + (1 * 200))) * 20 - dmx + 10 - (4 * LEN(xtemp$)), destdoor(link(cur + (1 * 200)) + 100) * 20 - dmy + 86, 2
END IF
'-----------------RESET DATA
loadpage game$ + ".til", gmap(0), 3
xbload maplumpname$(pt, "t"), map(), "Tilemap lump disappeared!"
RETRACE


layermenu:

	gosub makelayermenu
	csr2 = 0
	
	DO 
		setwait timing(), 100
		setkeys
	 	tog = tog XOR 1

		IF keyval(1) > 1 THEN clearkey(1): EXIT DO
		usemenu(csr2, 0, 0, 4, 22)
		
		select case csr2
		case 0
			IF keyval(57) > 1 OR keyval(28) > 1 THEN
				clearkey(57) 'clear repeats
				clearkey(28)
    		EXIT DO
   		END IF
   	case 1 'layer 1 can't be disabled
   		IF keyval(75) > 1 OR keyval(77) > 1 THEN
   			visible(0) = visible(0) xor 1
   			gosub makelayermenu
   		end if
   	case 2 to 3
   		IF keyval(57) > 1 OR keyval(28) > 1 THEN
   			setbit(gmap(), 19, csr2-2, readbit(gmap(), 19, csr2-2) xor 1)
   			if not readbit(gmap(), 19, csr2-2) then visible(csr2-1) = 0
   			gosub makelayermenu
   		end if
   		IF readbit(gmap(), 19, csr2-2) AND (keyval(75) > 1 OR keyval(77) > 1) THEN
   			visible(csr2-1) = visible(csr2-1) xor 1
   			gosub makelayermenu
   		end if

		end select
		
		FOR i = 0 TO 3
		  if i = 1 then
		  	textcolor 7, 0
		  	IF csr2 = i THEN textcolor 14 + tog, 0
		  elseif i > 1 AND readbit(gmap(), 19, i-2) = 0 then
		  	textcolor 6, 0
		  	IF csr2 = i THEN textcolor 6 + tog, 0		 
		  else
		  	textcolor 7, 0
		  	IF csr2 = i THEN textcolor 14 + tog, 0
		  end if
		  
		  printstr menu$(i), 0, i * 8, dpage
		NEXT i
		
		SWAP vpage, dpage
 		setvispage vpage
 		clearpage dpage
 		dowait

	LOOP
	
RETRACE

makelayermenu:
menu$(0) = "Go back"
menu$(1) = "Bottom Layer "
menu$(2) = "Middle Layer "
menu$(3) = "Top Layer    "

for i = 1 to 3
	if visible(i-1) then
		menu$(i) = menu$(i) & "(Visible)"
	else
		menu$(i) = menu$(i) & "(Invisible)"
	end if
next
RETRACE

'----
'gmap(20)
'0=tileset
'1=ambient music
'2=minimap
'3=save anywhere
'4=show map name
'5=map edge mode
'6=default edge tile
'7=autorun script
'8=script argument
'9=harm tile damage
'10=harm tile flash
'11=default foot-offset
'12=afterbattle script
'13=instead-of-battle script
'14=eachstep script
'15=onkeypress script

'tiles
'1   north
'2   east
'4   south
'8   west
'16  vehicle A
'32  vehicle B
'64  harm tile
'128 overhead
END SUB

