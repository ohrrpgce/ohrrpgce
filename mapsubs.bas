'OHRRPGCE CUSTOM - Miscellaneous unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "udts.bi"

'basic subs and functions
DECLARE FUNCTION addmaphow% ()
DECLARE FUNCTION filenum$ (n%)
DECLARE FUNCTION animadjust% (tilenum%, tastuf%())
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB importbmp (f$, cap$, count%)
DECLARE SUB loadpasdefaults (array%(), tilesetnum%)
DECLARE SUB textxbload (f$, array%(), e$)
DECLARE SUB fixorder (f$)
DECLARE SUB standardmenu (menu$(), size%, vis%, pt%, top%, x%, y%, page%, edge%)
DECLARE SUB vehicles ()
DECLARE SUB verifyrpg ()
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE SUB fontedit (font%(), gamedir$)
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE SUB smnemonic (tagname$, index%)
DECLARE SUB tagnames ()
DECLARE SUB resizetiledata (array%(), xoff%, yoff%, neww%, newh%, yout%, page%, layer%)
DECLARE SUB drawmini (high%, wide%, cursor%(), page%, tastuf%())
DECLARE SUB mapmaker (font%(), npc%(), npcstat%())
DECLARE SUB npcdef (npc%(), pt%)
DECLARE SUB editbitset (array%(), wof%, last%, name$())
DECLARE SUB sprite (xw%, yw%, sets%, perset%, soff%, foff%, atatime%, info$(), size%, zoom%, fileset%, font%())
DECLARE FUNCTION needaddset (pt%, check%, what$)
DECLARE SUB shopdata ()
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
DECLARE FUNCTION scriptbrowse$ (trigger%, triggertype%, scrtype$)
DECLARE FUNCTION scrintgrabber (n%, BYVAL min%, BYVAL max%, BYVAL less%, BYVAL more%, scriptside%, triggertype%)

DECLARE Function LayerIsVisible(vis() as integer, byval l as integer) as integer
DECLARE Function LayerIsEnabled(gmap() as integer, byval l as integer) as integer
DECLARE Sub SetLayerVisible(vis() as integer, byval l as integer, byval v as integer)
DECLARE Sub SetLayerEnabled(gmap() as integer, byval l as integer, byval v as integer)
DECLARE Sub ToggleLayerVisible(vis() as integer, byval l as integer)
DECLARE Sub ToggleLayerEnabled(vis() as integer, byval l as integer)

DECLARE SUB DrawDoorPair(curmap as integer, cur as integer, map(), pass(), doors() as door, link() as doorlink, gmap())

DECLARE SUB calculatepassblock(x AS INTEGER, y AS INTEGER, map() AS INTEGER, pass() AS INTEGER, defaults() AS INTEGER, tastuf() AS INTEGER)
DECLARE SUB resizemapmenu (map(), tastuf(), byref newwide, byref newhigh, byref tempx, byref tempy)

DECLARE SUB make_top_map_menu(maptop, topmenu$())

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi" 
#include "cglobals.bi"

#include "const.bi"
#include "scrconst.bi"
#include "scancodes.bi"
#include "loading.bi"

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
  IF intgrabber(maptocopy, 0, gen(genMaxMap)) THEN
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
temp$(2) = "Copy of map " + STR$(maptocopy) + " " + getmapname$(maptocopy)
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

SUB mapmaker (font(), npc(), npcstat())
DIM menubar(82), cursor(600), mode$(12), list$(12), temp$(12), ulim(4), llim(4), menu$(-1 TO 20), topmenu$(24), gmap(dimbinsize(4)), gd$(-1 TO 20), gdmax(20), gdmin(20), tastuf(40), cycle(1), cycptr(1), cycskip(1), sampmap(2), cursorpal(8),  _
defaults(160), pal16(288), gmapscr$(5), gmapscrof(5), npcnum(35)
DIM her AS HeroDef

redim doors(99) as door, link(199) as doorlink

DIM as integer usetile(0 to 2)
DIM as integer menubarstart(0 to 2)
DIM as integer layer
DIM as integer jiggle(0), visible(0) = {&b111} 'used as bitsets

DIM heroimg(102), heropal(8)

REDIM map(2) ' dummy empty map data, will be resized later
REDIM pass(2)
REDIM emap(2)

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
make_top_map_menu maptop, topmenu$()
setkeys
DO
 setwait timing(), 120
 setkeys
 IF keyval(1) > 1 THEN EXIT DO
 oldtop = maptop
 usemenu pt, maptop, 0, 2 + gen(0), 24
 IF oldtop <> maptop THEN make_top_map_menu maptop, topmenu$()
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF pt = 0 THEN EXIT DO
  IF pt > 0 AND pt <= gen(0) + 1 THEN
   '--silly backcompat pt adjustment
   pt = pt - 1
   GOSUB loadmap
   GOSUB whattodo
   pt = pt + 1
   make_top_map_menu maptop, topmenu$()
  END IF
  IF pt = gen(0) + 2 THEN
   GOSUB addmap
   make_top_map_menu maptop, topmenu$()
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
 usemenu csr, 0, 0, 12, 24
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
      n = 0
      FOR tl = 0 TO 2
       tilenum = animadjust(readmapblock(tx, ty, tl), tastuf())
       IF tl = 0 OR tilenum > 0 THEN n = n OR defaults(tilenum)
      NEXT tl
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
 usemenu gd, 0, -1, gmapmax, 24
 SELECT CASE gd
  CASE -1
   IF keyval(57) > 1 OR keyval(28) > 1 THEN EXIT DO
  CASE 1
   zintgrabber(gmap(gd), gdmin(gd) - 1, gdmax(gd) - 1) 'song is optional
  CASE 7, 12 TO 15
   IF gd = 7 THEN idx = 0 ELSE idx = gd - 11
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    gmapscr$(idx) = scriptbrowse$(gmap(gd), plottrigger, "plotscript")
   ELSEIF scrintgrabber(gmap(gd), 0, 0, 75, 77, 1, plottrigger) THEN
    gmapscr$(idx) = scriptname$(gmap(gd), plottrigger)
   END IF
  CASE ELSE
   intgrabber gmap(gd), gdmin(gd), gdmax(gd)
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
     xtemp$ = " -same as previous map-"
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
clearpage 2
'--load hero graphics--
loadherodata @her, 0
loadrecord heroimg(), game$ + ".pt4", 100, her.walk_sprite * 8 + 4
fixspriterecord heroimg(), 20, 20
getpal16 heropal(), 0, her.walk_sprite_pal, 4, her.walk_sprite
'--load NPC graphics--
FOR i = 0 TO 35
 setpicstuf buffer(), 1600, 2
 loadset game$ + ".pt4", npcstat(i * 15 + 0), 5 * i
 getpal16 pal16(), i, npcstat(i * 15 + 1), 4, npcstat(i * 15 + 0)
NEXT i
defpass = 1
IF readbit(gen(), genBits, 15) THEN defpass = 0 ' option to default the defaults to OFF

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
  	if keyval(scAlt) AND keyval(sc1 + i) then
  		clearkey(sc1 + i)
  		togglelayerenabled(gmap(), i)
  		if not layerisenabled(gmap(), i) then
  			if layer = i then
  				do until layerisenabled(gmap(), layer): layer -= 1: loop
  			end if
  		end if
  	end if
  	#IFNDEF __FB_LINUX__
  	if keyval(scCtrl) AND keyval(scF1 + i) then
  		clearkey(scF1 + i)
	 		if layerisenabled(gmap(), i) then togglelayervisible(visible(), i)
  	end if
  	#ENDIF
 	next
 	
 	if keyval(scTilde) then
 		togglelayervisible(visible(), layer)
 		clearkey(scTilde)
 	end if
 end if
 
 IF keyval(29) > 0 AND keyval(38) > 1 THEN gosub layermenu'ctrl-L
 IF keyval(1) > 1 THEN RETRACE
 IF keyval(15) > 1 THEN tiny = tiny XOR 1
 IF keyval(14) > 1 THEN
   'delete tile
   setmapdata map(), pass(), 20, 0
   FOR i = 0 TO 2
    setmapblock x, y, i, 0
   NEXT i
   'delete passability
   setmapdata map(), pass(), 20, 0
   setpassblock x, y, 0
   'delete foemap
   setmapdata emap(), pass(), 20, 0
   setmapblock x, y, 0, 0
   'setmapdata sucks. we should find a better way...
   setmapdata map(), pass(), 20, 0
   'delete NPC
   FOR i = 0 TO 299
    IF npc(i + 600) > 0 THEN
     IF npc(i + 0) = x AND npc(i + 300) = y + 1 THEN npc(i + 600) = 0
    END IF
   NEXT i
   'delete door
   FOR i = 0 TO 99
    IF doors(i).x = x AND doors(i).y = y + 1 AND readbit(doors(i).bits(),0,0) = 1 THEN setbit(doors(i).bits(),0,0, 1)
   NEXT
 END IF
 IF keyval(29) > 0 AND keyval(35) > 1 THEN 'Ctrl+H for hero start position
  gen(genStartMap) = pt
  gen(genStartX) = x
  gen(genStartY) = y
 END IF
 SELECT CASE editmode
  '---TILEMODE------
  CASE 0
   setmapdata map(), pass(), 20, 0
   IF keyval(33) > 1 AND keyval(29) > 0 THEN' Ctrl+F Fill screen
    FOR tx = 0 TO 14
     FOR ty = 0 TO 8
      setmapblock mapx \ 20 + tx, mapy \ 20 + ty, layer, usetile(layer)
      IF defpass THEN calculatepassblock mapx \ 20 + tx, mapy \ 20 + ty, map(), pass(), defaults(), tastuf()
     NEXT ty
    NEXT tx
    setmapdata map(), pass(), 20, 0
   END IF
   IF keyval(19) > 1 AND keyval(29) > 0 THEN' Ctrl+R to replace-all
    old = readmapblock(x, y, layer)
    FOR ty = 0 to map(1)
     FOR tx = 0 to map(0)
      IF readmapblock(tx, ty, layer) = old THEN setmapblock tx, ty, layer, usetile(layer)
     NEXT tx
    NEXT ty
   END IF
   IF keyval(29) > 0 AND keyval(36) > 1 THEN
     setbit jiggle(), 0, layer, (readbit(jiggle(), 0, layer) XOR 1)
   END IF
   IF keyval(41) > 1 THEN GOSUB minimap
   IF keyval(28) > 1 THEN GOSUB pickblock
   IF keyval(57) > 0 THEN
    setmapblock x, y, layer, usetile(layer)
    IF defpass THEN calculatepassblock x, y, map(), pass(), defaults(), tastuf()
   END IF
   IF keyval(83) > 1 THEN 'delete
    setmapblock x, y, layer, 0
   END IF
   IF keyval(58) > 1 THEN 'grab tile
    usetile(layer) = animadjust(readmapblock(x, y, layer), tastuf())
   GOSUB updatetilepicker
   END IF
   IF keyval(29) > 0 AND keyval(32) > 1 THEN defpass = defpass XOR 1   
   FOR i = 0 TO 1
    IF keyval(2 + i) > 1 THEN
     newtile = -1
     old = readmapblock(x, y, layer)
     IF old > 159 + (i * 48) THEN
      newtile = (old - (160 + (i * 48))) + tastuf(i * 20)
     ELSE
      IF old >= tastuf(i * 20) AND old < tastuf(i * 20) + 48 THEN
       newtile = 160 + (i * 48) + (old - tastuf(i * 20))
      END IF
     END IF
     IF newtile >= 0 THEN
      IF keyval(29) = 0 THEN
       setmapblock x, y, layer, newtile
      ELSE
       FOR tx = 0 TO map(0)
        FOR ty = 0 TO map(1)
         IF readmapblock(tx, ty, layer) = old THEN setmapblock tx, ty, layer, newtile
        NEXT ty
       NEXT tx
      END IF
     END IF
    END IF
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
   IF keyval(83) > 1 THEN 'delete
    setmapblock x, y, 0, 0
   END IF
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
     IF doors(i).x = x AND doors(i).y = y + 1 AND readbit(doors(i).bits(),0,0) = 1 THEN temp = 1: setbit(doors(i).bits(),0,0,0)
    NEXT
    IF temp = 0 THEN
     temp = -1
     FOR i = 99 TO 0 STEP -1
      IF readbit(doors(i).bits(),0,0) = 0 THEN temp = i
     NEXT
     IF temp >= 0 THEN doors(temp).x = x: doors(temp).y = y + 1: setbit(doors(temp).bits(),0,0,1)
    END IF
   END IF
   IF keyval(83) > 1 THEN 'delete
    FOR i = 0 TO 99
     IF doors(i).x = x AND doors(i).y = y + 1 AND readbit(doors(i).bits(),0,0) = 1 THEN setbit(doors(i).bits(),0,0,0)
    NEXT
   END IF
   '---NPCMODE------
  CASE 3
   IF keyval(83) > 1 THEN 'delete
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
   IF keyval(57) > 0 THEN
    setmapdata emap(), pass(), 20, 0
    setmapblock x, y, 0, foe
    setmapdata map(), pass(), 20, 0
   END IF
   IF keyval(83) > 1 THEN 'delete
    setmapdata emap(), pass(), 20, 0
    setmapblock x, y, 0, 0
    setmapdata map(), pass(), 20, 0
   END IF
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
 IF keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0 THEN
  xrate = 8
  yrate = 5
 ELSE
  xrate = 1
  yrate = 1
 END IF
 IF keyval(56) = 0 AND keyval(29) = 0 THEN
  IF keyval(72) > 0 THEN y = large(y - yrate, 0): IF y < INT(mapy / 20) THEN mapy = y * 20
  IF keyval(80) > 0 THEN y = small(y + yrate, high - 1): IF y > INT(mapy / 20) + 8 THEN mapy = y * 20 - 160
  IF keyval(75) > 0 THEN x = large(x - xrate, 0): IF x < INT(mapx / 20) THEN mapx = x * 20
  IF keyval(77) > 0 THEN x = small(x + xrate, wide - 1): IF x > INT(mapx / 20) + 14 THEN mapx = x * 20 - 280
 END IF
 IF keyval(56) > 0 AND keyval(29) = 0 THEN
  oldrelx = x - mapx / 20
  oldrely = y - mapy / 20
  IF keyval(72) > 0 THEN mapy = large(mapy - 20 * yrate, 0)
  IF keyval(80) > 0 THEN mapy = small(mapy + 20 * yrate, high * 20 - 180)
  IF keyval(75) > 0 THEN mapx = large(mapx - 20 * xrate, 0)
  IF keyval(77) > 0 THEN mapx = small(mapx + 20 * xrate, wide * 20 - 300)
  x = mapx / 20 + oldrelx
  y = mapy / 20 + oldrely
 END IF
 
 if editmode = 0 then 'tilemode, uses layers
 IF keyval(scPageup) > 1 then
 	for i = layer+1 to 2
 		if layerisenabled(gmap(), i) then
 			layer = i
 			setlayervisible(visible(), layer, 1)
			GOSUB updatetilepicker
 			exit for
 		end if
 	next
 end if
 	 
 IF keyval(scPageDown) > 1 then
 	for i = layer-1 to 0 step -1
 		if layerisenabled(gmap(), i) then
 			layer = i
 			setlayervisible(visible(), layer, 1)
			GOSUB updatetilepicker
 			exit for
 		end if
 	next
 end if
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
 	if layerisvisible(visible(), i) AND layerisenabled(gmap(), i) then
		jigx = 0: jigy = 0
		if readbit(jiggle(), 0, i) and tog then
			if i = 0 then jigx = 1
			if i = 1 then jigy = 1
			if i = 2 then jigx = -1: jigy = -1
		end if
		if i = 0 then
			drawmap mapx + jigx, mapy + jigy - 20, 0, 1, dpage, 0
		elseif i = 1 then
			drawmap mapx + jigx, mapy + jigy - 20, 1, 0, dpage, 1
		elseif i = 2 then
			drawmap mapx + jigx, mapy + jigy - 20, 2, 0, dpage, 1
		end if
	end if
 next
 if layerisvisible(visible(), 0) AND layerisenabled(gmap(), 0) then
	if readbit(jiggle(), 0, 0) and tog then
		drawmap mapx + 1, mapy - 20, 0, 2, dpage, 0
	else
		drawmap mapx, mapy - 20, 0, 2, dpage, 0
	end if
 end if
 
 '--show passmode overlay
 IF editmode = 1 THEN
  setmapdata pass(), pass(), 20, 0
  FOR o = 0 TO 8
   FOR i = 0 TO 15
    over = readmapblock((mapx \ 20) + i, (mapy \ 20) + o, 0)
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
   IF doors(i).x >= INT(mapx / 20) AND doors(i).x < INT(mapx / 20) + 16 AND doors(i).y > INT(mapy / 20) AND doors(i).y <= INT(mapy / 20) + 9 AND readbit(doors(i).bits(),0,0) = 1 THEN
    rectangle doors(i).x * 20 - mapx, doors(i).y * 20 - mapy, 20, 20, 15 - tog, dpage
    printstr STR$(i), doors(i).x * 20 - mapx + 10 - (4 * LEN(STR$(i))), doors(i).y * 20 - mapy + 6, dpage
   END IF
  NEXT
 END IF

 '--hero start location display--
 IF gen(genStartMap) = pt THEN
  IF gen(genStartX) >= INT(mapx / 20) AND gen(genStartX) < INT(mapx / 20) + 16 AND gen(genStartY) > INT(mapy / 20) AND gen(genStartY) <= INT(mapy / 20) + 9 THEN
   drawsprite heroimg(), 0, heropal(), 0, gen(genStartX) * 20 - mapx, gen(genStartY) * 20 + 20 - mapy, dpage
   printstr "Hero", gen(genStartX) * 20 - mapx, gen(genStartY) * 20 + 30 - mapy, dpage
  END IF
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
 if editmode = 0 then
 printstr "Layer " & layer, 0, 180, dpage
 end if
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
 setvispage vpage
 dowait
LOOP
GOSUB updatetilepicker
RETRACE

sizemap:
tempw = 0
temph = 0
tempx = 0
tempy = 0
resizemapmenu map(), tastuf(), tempw, temph, tempx, tempy
IF tempw = -1 THEN RETRACE

clearpage 0
clearpage 1
yout = 0
edgeprint "TILEMAP", 0, yout * 10, 15, vpage: setvispage vpage: yout = yout + 1
resizetiledata map(), tempx, tempy, tempw, temph, yout, vpage, 3
edgeprint "PASSMAP", 0, yout * 10, 15, vpage: setvispage vpage: yout = yout + 1
resizetiledata pass(), tempx, tempy, tempw, temph, yout, vpage, 1
edgeprint "FOEMAP", 0, yout * 10, 15, vpage: setvispage vpage: yout = yout + 1
resizetiledata emap(), tempx, tempy, tempw, temph, yout, vpage, 1
' update SAV x/y offset in MAP lump
gmap(20) += tempx * - 1
gmap(21) += tempy * - 1
' update hero's starting position (if on current map)
IF gen(genStartMap) = pt THEN
 gen(genStartX) += tempx * -1
 gen(genStartY) += tempy * -1 
END IF
setmapdata map(), pass(), 20, 0
wide = map(0): high = map(1)
'--reset map scroll position
x = 0: y = 0: mapx = 0: mapy = 0
edgeprint "Aligning and truncating doors", 0, yout * 10, 15, vpage: yout = yout + 1
FOR i = 0 TO 99
 doors(i).x -= tempx
 doors(i).y -= tempy
 IF doors(i).x < 0 OR doors(i).y < 0 OR doors(i).x >= wide OR doors(i).y >= high THEN
  setbit(doors(i).bits(),0,0,0)
 END IF
NEXT
edgeprint "Aligning and truncating NPCs", 0, yout * 10, 15, vpage: setvispage vpage: yout = yout + 1
FOR i = 0 TO 299
 npc(i + 0) = npc(i + 0) - tempx
 npc(i + 300) = npc(i + 300) - tempy
 IF npc(i + 0) < 0 OR npc(i + 300) < 0 OR npc(i + 0) >= wide OR npc(i + 300) >= high THEN
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
 setvispage vpage

 cleantiledata map(), wide, high, 3
 cleantiledata pass(), wide, high
 cleantiledata emap(), wide, high
 flusharray npc(), 900, 0
 cleandoorlinks link()
 cleandoors doors()
 'flusharray doors(), 299, 0

 savetiledata maplumpname$(pt, "t"), map(), 3
 savetiledata maplumpname$(pt, "p"), pass()
 savetiledata maplumpname$(pt, "e"), emap()
 xBSAVE maplumpname$(pt, "l"), npc(), 3000
 serdoorlinks maplumpname$(pt, "d"), link()
 serdoors game$ + ".dox", doors(), pt
END IF
'--reset scroll position
x = 0: y = 0: mapx = 0: mapy = 0
layer = 0
'visible(0) = 1: visible(1) = 0: visible(2) = 0
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
gen(genMaxMap) += 1
'--flush map buffers
cleantiledata map(), 64, 64, 3
cleantiledata pass(), 64, 64
cleantiledata emap(), 64, 64
flusharray gmap(), 19, 0
flusharray npc(), 900, 0
flusharray npcstat(), 1500, 0
cleandoors doors()
cleandoorlinks link()
'--save map buffers
storerecord gmap(), game$ + ".map", getbinsize(4) / 2, gen(genMaxMap)
savetiledata maplumpname$(gen(genMaxMap), "t"), map(), 3
savetiledata maplumpname$(gen(genMaxMap), "p"), pass()
savetiledata maplumpname$(gen(genMaxMap), "e"), emap()
xBSAVE maplumpname$(gen(genMaxMap), "n"), npcstat(), 3000
xBSAVE maplumpname$(gen(genMaxMap), "l"), npc(), 3000
serdoors game$ + ".dox", doors(), gen(genMaxMap)
serdoorlinks maplumpname$(gen(genMaxMap), "d"), link()
'--setup map name
buffer(0) = 0
storerecord buffer(), game$ + ".mn", 40, gen(genMaxMap)
RETRACE

savemap:
storerecord gmap(), game$ + ".map", getbinsize(4) / 2, pt
savetiledata maplumpname$(pt, "t"), map(), 3
savetiledata maplumpname$(pt, "p"), pass()
savetiledata maplumpname$(pt, "e"), emap()
xBSAVE maplumpname$(pt, "l"), npc(), 3000
xBSAVE maplumpname$(pt, "n"), npcstat(), 3000
serdoors game$ + ".dox", doors(), pt
serdoorlinks maplumpname$(pt, "d"), link()
'--save map name
buffer(0) = LEN(mapname$)
str2array LEFT$(mapname$, 39), buffer(), 1
storerecord buffer(), game$ + ".mn", 40, pt
RETRACE

loadmap:
loadrecord gmap(), game$ + ".map", getbinsize(4) / 2, pt
visible(0) = &b111   'default all layers to visible, if they're enabled too, of course
loadpage game$ + ".til", gmap(0), 3
loadtanim gmap(0), tastuf()
FOR i = 0 TO 1
 cycle(i) = 0
 cycptr(i) = 0
 cycskip(i) = 0
NEXT i
loadtiledata maplumpname$(pt, "t"), map(), 3, wide, high
loadtiledata maplumpname$(pt, "p"), pass()
loadtiledata maplumpname$(pt, "e"), emap()
xbload maplumpname$(pt, "l"), npc(), "npclocation lump is missing!"
xbload maplumpname$(pt, "n"), npcstat(), "npcstat lump is missing!"
deserdoors game$ + ".dox", doors(), pt
deserdoorlinks maplumpname$(pt, "d"), link()

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
 printstr " TileMap " & map(0) & "*" & map(1) & " tiles", 0, j * 8, vpage: j = j + 1
 printstr " WallMap " & pass(0) & "*" & pass(1) & " tiles", 0, j * 8, vpage: j = j + 1
 printstr " FoeMap " & emap(0) & "*" & emap(1) & " tiles", 0, j * 8, vpage: j = j + 1
 j = j + 1
 printstr "What is the correct size?", 0, j * 8, vpage: j = j + 1
 DO
  textcolor 14, 240
  setkeys
  DO
   setwait timing(), 100
   setkeys
   IF keyval(1) > 1 OR keyval(28) > 1 THEN EXIT DO
   intgrabber wide, 0, 9999
   printstr "Width:" + XSTR$(wide) + "   ", 0, j * 8, vpage
   setvispage vpage
   dowait
  LOOP
  j = j + 1
  setkeys
  DO
   setwait timing(), 100
   setkeys
   IF keyval(1) > 1 OR keyval(28) > 1 THEN EXIT DO
   intgrabber high, 0, 9999
   printstr "Height:" + XSTR$(high) + "    ", 0, j * 8, vpage
   setvispage vpage
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
 setvispage vpage
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
GOSUB savemap   'other map tilemaps are loaded
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
 IF keyval(1) > 1 THEN
 	serdoorlinks(maplumpname$(pt, "d"), link())
 	RETRACE
 end if
 usemenu cur, ttop, 0, 199, 10
 IF keyval(28) > 1 OR keyval(57) > 1 THEN GOSUB seedoors
 FOR i = ttop TO ttop + 10
  textcolor 7, 0
  IF cur = i THEN textcolor 14 + tog, 0

  a$ = "Door " & link(i).source & " leads to door " & link(i).dest & " on map " & link(i).dest_map
  printstr a$, 0, 2 + (i - ttop) * 16, dpage

  IF link(i).tag1 = 0 AND link(i).tag2 = 0 THEN
   a$ = "  all the time"
  ELSE
   a$ = "  only if tag "
   IF link(i).tag1 <> 0 THEN
    a$ += ABS(link(i).tag1) & " = " & iif(link(i).tag1 > 0, 1, 0)
   END IF
   IF link(i).tag2 THEN
    IF link(i).tag1 THEN a$ += " and tag "
    a$ += ABS(link(i).tag2) & " = " & iif(link(i).tag2 > 0, 1, 0)
   END IF
  END IF
  printstr a$, 0, 10 + (i - ttop) * 16, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

seedoors:
menu$(-1) = "Go Back"
menu$(0) = "Entrance Door"
menu$(1) = "Exit Door"
menu$(2) = "Exit Map"
menu$(3) = "Require Tag"
menu$(4) = "Require Tag"
cur2 = -1
sdwait = 0
outmap$ = getmapname$(link(cur).dest_map)
DrawDoorPair pt, cur, map(), pass(), doors(), link(), gmap()
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF sdwait > 0 THEN
  sdwait = sdwait - 1
  IF sdwait = 0 THEN DrawDoorPair pt, cur, map(), pass(), doors(), link(), gmap()
 END IF
 IF keyval(1) > 1 THEN RETRACE
 usemenu cur2, 0, -1, 4, 24
 IF cur2 >= 0 THEN
  select case cur2
  	case 0
  		if intgrabber(link(cur).source, llim(cur2), ulim(cur2)) then sdwait = 3
  	case 1
  		if intgrabber(link(cur).dest, llim(cur2), ulim(cur2)) then sdwait = 3
  	case 2
  		if intgrabber(link(cur).dest_map, llim(cur2), ulim(cur2)) then sdwait = 3: outmap$ = getmapname$(link(cur).dest_map)
  	case 3
  		intgrabber(link(cur).tag1, llim(cur2), ulim(cur2))
  	case 4
  		intgrabber(link(cur).tag2, llim(cur2), ulim(cur2))
  	case else
  		'...
  end select
 ELSE
  IF keyval(28) > 1 OR keyval(57) > 1 THEN RETRACE
 END IF
 rectangle 0, 100, 320, 2, 1 + tog, dpage
 FOR i = -1 TO 4
  xtemp$ = ""
  select case i
	case 0
		xtemp$ = XSTR$(link(cur).source)
	case 1
		xtemp$ = XSTR$(link(cur).dest)
	case 2
		xtemp$ = XSTR$(link(cur).dest_map)
	case 3
		if link(cur).tag1 then
			xtemp$ = XSTR$(ABS(link(cur).tag1)) + " = " + onoroff$(link(cur).tag1) + " (" + lmnemonic$(ABS(link(cur).tag1)) + ")"
		else
			xtemp$ = " 0 [N/A]"
		end if
	case 4
		if link(cur).tag2 then
			xtemp$ = XSTR$(ABS(link(cur).tag2)) + " = " + onoroff$(link(cur).tag2) + " (" + lmnemonic$(ABS(link(cur).tag2)) + ")"
		else
			xtemp$ = " 0 [N/A]"
		end if
  end select

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

layermenu:

	gosub makelayermenu
	csr2 = 0
	
	DO 
		setwait timing(), 100
		setkeys
	 	tog = tog XOR 1

		IF keyval(1) > 1 THEN clearkey(1): EXIT DO
		usemenu(csr2, 0, 0, 3, 22)
		
		select case csr2
		case 0
			IF keyval(57) > 1 OR keyval(28) > 1 THEN
				clearkey(57) 'clear repeats
				clearkey(28)
    		EXIT DO
   		END IF
   	case 1 to 3
   		IF keyval(57) > 1 OR keyval(28) > 1 THEN
   			ToggleLayerEnabled(gmap(), csr2 - 1)
   			gosub makelayermenu
   		end if
   		IF layerisenabled(gmap(), csr2-1) AND (keyval(75) > 1 OR keyval(77) > 1) THEN
   			togglelayervisible(visible(), csr2-1)
   			gosub makelayermenu
   		end if

		end select
		
		FOR i = 0 TO 3
			if layerisenabled(gmap(), i - 1) then
				textcolor 7, 0
				if csr2 = i then textcolor 14 + tog, 0
			else
				textcolor 6, 0
		  	IF csr2 = i THEN textcolor 6 + tog, 0		 
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

for i = 0 to 2
	if layerisvisible(visible(),i) then
		menu$(i+1) = menu$(i+1) & "(Visible)"
	else
		menu$(i+1) = menu$(i+1) & "(Invisible)"
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

Function LayerIsVisible(vis() as integer, byval l as integer) as integer
	'debug "layer #" & l & " is: " & readbit(vis(), 0, l)
	return readbit(vis(), 0, l)
end function

Function LayerIsEnabled(gmap() as integer, byval l as integer) as integer
	if l <= 0 then return 1
	'debug "layer #" & l & " is: " & readbit(gmap(), 19, l-1)
	return readbit(gmap(), 19, l-1)
end function

Sub SetLayerVisible(vis() as integer, byval l as integer, byval v as integer)
	setbit(vis(), 0, l, v)
end sub

Sub SetLayerEnabled(gmap() as integer, byval l as integer, byval v as integer)
	if l <= 0 then exit sub
	setbit(gmap(), 19, l-1 ,v)
end sub

Sub ToggleLayerVisible(vis() as integer, byval l as integer)
	setbit(vis(), 0, l, readbit(vis(), 0, l) xor 1)
end sub

Sub ToggleLayerEnabled(gmap() as integer, byval l as integer)
	if l <= 0 then exit sub
	setbit(gmap(), 19, l - 1, readbit(gmap(), 19, l-1) xor 1)
end sub

SUB DrawDoorPair(curmap as integer, cur as integer, map(), pass(), doors() as door, link() as doorlink, gmap())
 DIM as integer dmx, dmy, i, tempw, temph
 DIM caption$
 DIM destdoor(99) as door
 DIM gmap2(dimbinsize(4)), anim(40)
 
 clearpage 2
 
 loadtanim gmap(0), anim()
 setanim anim(0), anim(20)
 setmapdata map(), pass(), 0, 101
 IF readbit(doors(link(cur).source).bits(),0,0) = 1 THEN
  dmx = doors(link(cur).source).x * 20 - 150
  dmy = doors(link(cur).source).y * 20 - 65
  dmx = small(large(dmx, 0), map(0) * 20 - 320)
  dmy = small(large(dmy, 0), map(1) * 20 - 100)
  FOR i = 0 to 2
   IF LayerIsEnabled(gmap(), i) THEN
     drawmap dmx, dmy, i, 0, 2, i <> 0
   END IF
  NEXT i
  rectangle doors(link(cur).source).x * 20 - dmx, doors(link(cur).source).y * 20 - dmy - 20, 20, 20, 240, 2
  rectangle 1 + doors(link(cur).source).x * 20 - dmx, 1 + doors(link(cur).source).y * 20 - dmy - 20, 18, 18, 7, 2
  textcolor 240, 0
  caption$ = XSTR$(link(cur).source)
  printstr caption$, doors(link(cur).source).x * 20 - dmx + 10 - (4 * LEN(caption$)), doors(link(cur).source).y * 20 - dmy - 14, 2
 END IF
 '-----------------EXIT DOOR
 destmap = link(cur).dest_map
 loadrecord gmap2(), game$ + ".map", dimbinsize(4), destmap
 deserdoors game$ + ".dox", destdoor(), destmap
 LoadTiledata maplumpname$(destmap, "t"), map(), 3, tempw, temph
 loadpage game$ + ".til", gmap2(0), 3
 
 loadtanim gmap2(0), anim()
 setanim anim(0), anim(20)
 setmapdata map(), pass(), 101, 0
 IF readbit(destdoor(link(cur).dest).bits(),0,0) = 1 THEN
  dmx = destdoor(link(cur).dest).x * 20 - 150
  dmy = destdoor(link(cur).dest).y * 20 - 65
  dmx = small(large(dmx, 0), map(0) * 20 - 320)
  dmy = small(large(dmy, 0), map(1) * 20 - 100)
  FOR i = 0 to 2
   IF LayerIsEnabled(gmap2(), i) THEN
     drawmap dmx, dmy - 100, i, 0, 2, i <> 0
   END IF
  NEXT i
  rectangle destdoor(link(cur).dest).x * 20 - dmx, destdoor(link(cur).dest).y * 20 - dmy + 80, 20, 20, 240, 2
  rectangle 1 + destdoor(link(cur).dest).x * 20 - dmx, 1 + destdoor(link(cur).dest).y * 20 - dmy + 80, 18, 18, 7, 2
  textcolor 240, 0
  caption$ = XSTR$(link(cur).dest)
  printstr caption$, destdoor(link(cur).dest).x * 20 - dmx + 10 - (4 * LEN(caption$)), destdoor(link(cur).dest).y * 20 - dmy + 86, 2
 END IF
 '-----------------RESET DATA
 loadpage game$ + ".til", gmap(0), 3
 LoadTiledata maplumpname$(curmap, "t"), map(), 3, tempw, temph
END SUB

SUB calculatepassblock(x AS INTEGER, y AS INTEGER, map() AS INTEGER, pass() AS INTEGER, defaults() AS INTEGER, tastuf() AS INTEGER)
 setmapdata map(), pass(), 0, 0
 n = 0
 FOR i = 0 TO 2
  tilenum = animadjust(readmapblock(x, y, i), tastuf())
  IF i = 0 OR tilenum > 0 THEN n = n OR defaults(tilenum)
 NEXT i
 setpassblock x, y, n
END SUB

SUB resizetiledata (array(), x_off, y_off, new_width, new_height, yout, page, layers)
 edgeprint "Resizing Map...", 0, yout * 10, 15, page
 yout += 1
 setvispage page
' debug "sizemar"
' debug "old_width = " & old_width & ", " & _
'       "old_height = " & old_height & ", " & _
'       "old_x = " & old_x & ", " & _
'       "old_y = " & old_y & ", " & _
'       "new_width = " & new_width & ", " & _
'       "new_height = " & new_height

 dim as integer tmp(ubound(array)), i, x, y

 memcpy (@tmp(0), @array(0), sizeof(integer) * (ubound(array) + 1))  'why doesn't sizeof work on arrays?!
 cleantiledata array(), new_width, new_height, layers
 
 for i = 0 to layers - 1
	for x = large(x_off, 0) to small(tmp(0), new_width + x_off) - 1
		for y = large(y_off, 0) to small(tmp(1), new_height + y_off) - 1
			'newarray(i * newsize + (x - tempx) * tempw + (y - tempy) + 2) = tmp(i * oldsize + x * wide + y + 2)
			'this'll be fixed when the tile data format is fixed, of course	
			setmapdata tmp(), tmp(), 20, 0
			j = readmapblock(x,y,i)
			setmapdata array(), array(), 20, 0
			setmapblock(x - x_off,y - y_off,i,j)
		next
	next
 next
END SUB

SUB resizemapmenu (map(), tastuf(), byref tempw, byref temph, byref tempx, byref tempy)
 'returns the new size and offset in passed args, or -1 width to cancel
REDIM minimap(0,0)
DIM menu$(6), tog, csr = 1, zoom = 0
wide = map(0)
high = map(1)
tempw = wide
temph = high
tempx = 0
tempy = 0
setmapdata map(), map(), 20, 0
GOSUB getmini
GOSUB buildmenu
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog xor 1
 IF keyval(1) > 1 THEN EXIT DO
 usemenu csr, 0, 0, 4, 10
 IF keyval(56) > 0 THEN incval = 8 ELSE incval = 1
 SELECT CASE csr
  CASE 0
   IF keyval(28) > 1 THEN EXIT DO
  CASE 1
   IF keyval(75) > 0 THEN tempw -= incval 
   IF keyval(77) > 0 THEN tempw += incval
   GOSUB correctw
  CASE 2
   IF keyval(75) > 0 THEN temph -= incval 
   IF keyval(77) > 0 THEN temph += incval
   GOSUB correcth
  CASE 3
   IF keyval(75) > 0 THEN tempx -= incval: tempw += incval
   IF keyval(77) > 0 THEN tempx += incval: tempw -= incval
   GOSUB correctw
  CASE 4
   IF keyval(75) > 0 THEN tempy -= incval: temph += incval
   IF keyval(77) > 0 THEN tempy += incval: temph -= incval
   GOSUB correcth
 END SELECT
 IF keyval(28) > 1 THEN EXIT SUB

 clearpage dpage
 drawoffx = large(0, -tempx * zoom)
 drawoffy = large(0, -tempy * zoom)
 FOR i = 0 TO UBOUND(minimap, 1) - 1
  FOR j = 0 TO UBOUND(minimap, 2) - 1
   putpixel drawoffx + i, drawoffy + j, minimap(i, j), dpage
  NEXT
 NEXT
 standardmenu menu$(), UBOUND(menu$), 28, csr, 0, 0, 140, dpage, 0
 drawbox drawoffx + zoom * tempx, drawoffy + zoom * tempy, zoom * tempw, zoom * temph, 14 + tog, dpage

 SWAP dpage, vpage
 setvispage vpage
 dowait
LOOP
'cancel
tempw = -1
temph = -1
EXIT SUB

correctw:
tempw = bound(tempw, 16, 32000)
tempx = bound(tempx, -tempw + 1, wide - 1)
WHILE temph * tempw > 32000 AND temph > 10
 temph -= 1
WEND
GOTO dimchange

correcth:
temph = bound(temph, 10, 32000)
tempy = bound(tempy, -temph + 1, high - 1)
WHILE temph * tempw > 32000 AND tempw > 16
 tempw -= 1
WEND
GOTO dimchange

dimchange:
WHILE temph * tempw > 32000
 temph = large(temph - 1, 10)
 tempw = large(tempw - 1, 16)
WEND
GOSUB getmini

buildmenu:
menu$(0) = "Cancel"
menu$(1) = "Width " & wide & CHR$(26) & tempw
menu$(2) = "Height " & high & CHR$(26) & temph
IF tempx > 0 THEN
 menu$(3) = "Left edge: trim " & tempx & " tiles"
ELSE
 menu$(3) = "Left edge: add " & -tempx & " tiles"
END IF
IF tempy > 0 THEN
 menu$(4) = "Top edge: trim " & tempy & " tiles"
ELSE
 menu$(4) = "Top edge: add " & -tempy & " tiles"
END IF
menu$(5) = "Area " & (wide * high) & CHR$(26) & (temph * tempw)
menu$(6) = zoom & "x zoom"
RETRACE

getmini:
lastzoom = zoom
tw = large(wide, tempx + tempw) 'right most point
IF tempx < 0 THEN tw -= tempx   'plus left most
th = large(high, tempy + temph)
IF tempy < 0 THEN th -= tempy
zoom = bound(small(320 \ tw, 200 \ th), 1, 20)
IF zoom <> lastzoom THEN
 createminimap minimap(), map(), tastuf(), 3, zoom
END IF
RETRACE

END SUB

SUB drawmini (high, wide, cursor(), page, tastuf())

clearpage vpage
FOR i = 0 TO high
 FOR o = 0 TO wide
  block = readmapblock(o, i, 0)
  IF block > 207 THEN block = (block - 207) + tastuf(20)
  IF block > 159 THEN block = (block - 159) + tastuf(0)
  mx = block - (INT(block / 16) * 16)
  my = INT(block / 16)
  loadsprite cursor(), 0, (INT(RND * 7) + 7) + (mx * 20), (INT(RND * 7) + 7) + (my * 20), 1, 1, 3
  stosprite cursor(), 0, o, i, page
 NEXT o
NEXT i

END SUB

SUB make_top_map_menu(maptop, topmenu$())
DIM i AS INTEGER
FOR i = 0 TO 24
 SELECT CASE maptop + i
  CASE 0
   topmenu$(i) = "Return to Main Menu"
  CASE 1 TO gen(genMaxMap) + 1
   topmenu$(i) = "Map " + filenum$((maptop + i) - 1) + ": " + getmapname$((maptop + i) - 1)
  CASE gen(genMaxMap) + 2
   topmenu$(i) = "Add a New Map"
  CASE ELSE
   topmenu$(i) = ""
 END SELECT
NEXT i
END SUB
